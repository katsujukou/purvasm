-- | Pure helpers for the `ulib` per-package manifest and dependency validation (ADR-0047): parse a
-- | `ulib.json`'s `dependencies`, parse a `spago.yaml`'s first `dependencies` block, parse the
-- | package set listing, classify a declared dependency by where it resolves, and detect dependency
-- | cycles. Kept free of effects so the resolution/validation logic is unit-testable.
module Purvasm.UlibTools.Manifest
  ( Resolution(..)
  , Fidelity(..)
  , TestSpec
  , parseDependencies
  , parseForeign
  , parseForeignSigs
  , renderForeignManifest
  , parseTest
  , parseBowerDependencies
  , parseRegistryVersion
  , repoSlug
  , parseSpagoDependencies
  , parsePackageSet
  , stripVersion
  , classifyDep
  , findCycle
  , inRepoClosure
  ) where

import Prelude

import Purvasm.Compiler.ForeignSig (ForeignShape, shapeFromJson, shapeToJson)

import Data.Argonaut.Core (fromObject, fromString, stringify, toArray, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- | Where a declared dependency resolves. `Resolved` = present in the staged `.spago/p`;
-- | `UnresolvedRegistry` = a known package-set member that is not resolved into `.spago/p`.
data Resolution = InRepo | Resolved | UnresolvedRegistry | Unknown

derive instance eqResolution :: Eq Resolution
instance showResolution :: Show Resolution where
  show = case _ of
    InRepo -> "InRepo"
    Resolved -> "Resolved"
    UnresolvedRegistry -> "UnresolvedRegistry"
    Unknown -> "Unknown"

-- | How faithfully a `ulib` package's behaviour must match its registry original, by representation
-- | seam (ADR-0040/0048). `JsFidelity` packages are representation-equivalent and run on node;
-- | `NativeFidelity`/`BespokeFidelity` need `purvm` and are deferred (ADR-0048 Phase 2).
data Fidelity = JsFidelity | NativeFidelity | BespokeFidelity

derive instance eqFidelity :: Eq Fidelity
instance showFidelity :: Show Fidelity where
  show = case _ of
    JsFidelity -> "js"
    NativeFidelity -> "native"
    BespokeFidelity -> "bespoke"

-- | A `ulib` package's upstream test suite, from the `test` object of its `ulib.json` (ADR-0048 §1).
-- | `repo`/`ref` pin the upstream source; `testMain` defaults to `Test.Main`; `testDeps` are the
-- | suite's extra dependencies; `fidelity` selects the run path; `xfail` documents intended
-- | divergences (Phase 2).
type TestSpec =
  { repo :: String
  , ref :: String
  , testMain :: String
  , testDeps :: Array String
  , fidelity :: Fidelity
  , xfail :: Array String
  }

-- | The `test` object of a `ulib.json` (`Nothing` if absent — the package is not yet tested).
parseTest :: String -> Either String (Maybe TestSpec)
parseTest src = do
  json <- jsonParser src
  obj <- note "ulib.json: top level is not an object" (toObject json)
  case Object.lookup "test" obj of
    Nothing -> Right Nothing
    Just t -> do
      to <- note "ulib.json: 'test' must be an object" (toObject t)
      repo <- reqStr to "repo"
      ref <- reqStr to "ref"
      fidStr <- reqStr to "fidelity"
      fidelity <- parseFidelity fidStr
      testDeps <- optStrArray to "testDeps"
      xfail <- optStrArray to "xfail"
      let testMain = fromMaybe "Test.Main" (Object.lookup "testMain" to >>= toString)
      pure (Just { repo, ref, testMain, testDeps, fidelity, xfail })
  where
  reqStr o k = note ("ulib.json: 'test." <> k <> "' must be a string") (Object.lookup k o >>= toString)
  optStrArray o k = case Object.lookup k o of
    Nothing -> Right []
    Just v -> do
      arr <- note ("ulib.json: 'test." <> k <> "' must be an array") (toArray v)
      traverse (note ("ulib.json: 'test." <> k <> "' must contain only strings") <<< toString) arr
  parseFidelity = case _ of
    "js" -> Right JsFidelity
    "native" -> Right NativeFidelity
    "bespoke" -> Right BespokeFidelity
    other -> Left ("ulib.json: 'test.fidelity' must be js|native|bespoke, got '" <> other <> "'")

-- | The dependency names of a `bower.json` (the build config the upstream `purescript-*` repos ship),
-- | with the conventional `purescript-` prefix stripped to the registry/package-set name
-- | (`purescript-foldable-traversable` -> `foldable-traversable`). Absent field ⇒ none.
parseBowerDependencies :: String -> Either String (Array String)
parseBowerDependencies src = do
  json <- jsonParser src
  obj <- note "bower.json: top level is not an object" (toObject json)
  case Object.lookup "dependencies" obj of
    Nothing -> Right []
    Just d -> do
      deps <- note "bower.json: 'dependencies' must be an object" (toObject d)
      Right (map stripPurescript (Object.keys deps))
  where
  stripPurescript name = fromMaybe name (String.stripPrefix (Pattern "purescript-") name)

-- | The package-set registry version from a workspace `spago.yaml` (`registry: 77.8.0`).
parseRegistryVersion :: String -> Maybe String
parseRegistryVersion src =
  Array.findMap registryOn (String.split (Pattern "\n") src)
  where
  registryOn line = String.trim <$> String.stripPrefix (Pattern "registry:") (String.trim line)

-- | The repository slug of a git URL — its last path segment without a trailing `.git`
-- | (`https://github.com/purescript/purescript-arrays` -> `purescript-arrays`). Used as a cache key.
repoSlug :: String -> String
repoSlug url =
  let
    segs = String.split (Pattern "/") url
    last = fromMaybe url (Array.last segs)
  in
    fromMaybe last (String.stripSuffix (Pattern ".git") last)

-- | The `dependencies` array of a `ulib.json` (absent field ⇒ no extra deps).
parseDependencies :: String -> Either String (Array String)
parseDependencies src = do
  json <- jsonParser src
  obj <- note "ulib.json: top level is not an object" (toObject json)
  case Object.lookup "dependencies" obj of
    Nothing -> Right []
    Just d -> do
      arr <- note "ulib.json: 'dependencies' must be an array" (toArray d)
      traverse (note "ulib.json: 'dependencies' must contain only strings" <<< toString) arr

-- | The `foreign` object of a `ulib.json` (ADR-0073 §3): each native foreign's qualified key → the `.c`
-- | source implementing it (a path relative to the package dir). Absent field ⇒ no native foreigns. A
-- | non-string value is a hard error (a malformed `source` mapping should not silently drop a foreign).
parseForeign :: String -> Either String (Array (Tuple String String))
parseForeign src = do
  json <- jsonParser src
  obj <- note "ulib.json: top level is not an object" (toObject json)
  case Object.lookup "foreign" obj of
    Nothing -> Right []
    Just f -> do
      fo <- note "ulib.json: 'foreign' must be an object" (toObject f)
      traverse pair (Object.toUnfoldable fo :: Array (Tuple String _))
  where
  pair (Tuple k v) =
    Tuple k <$> note ("ulib.json: 'foreign." <> k <> "' must be a string") (toString v)

-- | The `foreignSigs` object of a `ulib.json` (ADR-0080 §1): each foreign's qualified key → its
-- | calling shape `{ arity, vsat, retVsat }`, author-declared and validated by `ulib-tools build`
-- | against the source reconstruction. Mandatory for every foreign a ulib module's corefn retains
-- | (the consumer trusts this, never the ulib source). Absent field ⇒ no declared shapes.
parseForeignSigs :: String -> Either String (Array (Tuple String ForeignShape))
parseForeignSigs src = do
  json <- jsonParser src
  obj <- note "ulib.json: top level is not an object" (toObject json)
  case Object.lookup "foreignSigs" obj of
    Nothing -> Right []
    Just f -> do
      fo <- note "ulib.json: 'foreignSigs' must be an object" (toObject f)
      traverse pair (Object.toUnfoldable fo :: Array (Tuple String _))
  where
  pair (Tuple k v) = Tuple k <$> lmap (\e -> "ulib.json: 'foreignSigs." <> k <> "': " <> e)
    (shapeFromJson v)

-- | Render the aggregated manifest boot's native backend + the compiler driver read: a JSON object
-- | `{ "foreign": { <key>: <path>, … }, "foreignSigs": { <key>: <shape>, … } }`. The `foreign` map
-- | (paths relative to the staged `ulib` dir) is boot's native-link input (ADR-0073 §3); the
-- | `foreignSigs` map is the consumer's shape source for a ulib-overlaid module (ADR-0080 §1). The
-- | inverse of [parseForeign]/[parseForeignSigs] at the aggregation seam.
renderForeignManifest
  :: Array (Tuple String String) -> Array (Tuple String ForeignShape) -> String
renderForeignManifest foreignEntries sigEntries =
  stringify $ fromObject $ Object.fromFoldable
    [ Tuple "foreign" (fromObject foreignInner)
    , Tuple "foreignSigs" (fromObject sigsInner)
    ]
  where
  foreignInner = Object.fromFoldable (map (\(Tuple k p) -> Tuple k (fromString p)) foreignEntries)
  sigsInner = Object.fromFoldable (map (\(Tuple k s) -> Tuple k (shapeToJson s)) sigEntries)

-- | The package names of a `spago.yaml`'s first `dependencies:` block (its `package.dependencies`,
-- | which precedes any `test.dependencies`). Block list form only — the form the in-repo packages
-- | use; other forms yield `[]`.
parseSpagoDependencies :: String -> Array String
parseSpagoDependencies src =
  case Array.findIndex (\l -> String.trim l == "dependencies:") ls of
    Nothing -> []
    Just i -> Array.mapMaybe itemName (Array.takeWhile isItem (Array.drop (i + 1) ls))
  where
  ls = String.split (Pattern "\n") src
  isItem l = maybe false (const true) (String.stripPrefix (Pattern "- ") (String.trim l))
  itemName l = do
    rest <- String.stripPrefix (Pattern "- ") (String.trim l)
    Array.head (String.split (Pattern " ") (String.trim rest))

-- | The package names from `spago ls packages` table output (first column, excluding the header
-- | and separator rows).
parsePackageSet :: String -> Set String
parsePackageSet out = Set.fromFoldable (Array.mapMaybe row (String.split (Pattern "\n") out))
  where
  row line = do
    _ <- String.stripPrefix (Pattern "|") line
    let cols = map String.trim (String.split (Pattern "|") line)
    name <- Array.index cols 1
    if name == "" || name == "Package" then Nothing else Just name

-- | Drop a trailing `-<version>` from a resolved `.spago/p` directory name
-- | (`foldable-traversable-6.0.0` -> `foldable-traversable`); names without a version are kept.
stripVersion :: String -> String
stripVersion name =
  case Array.unsnoc (String.split (Pattern "-") name) of
    Just { init, last } | not (Array.null init) && startsWithDigit last ->
      String.joinWith "-" init
    _ -> name
  where
  startsWithDigit s = maybe false isDigit (SCU.charAt 0 s)
  isDigit c = c >= '0' && c <= '9'

-- | Classify a declared dependency by the resolution sets, in priority order.
classifyDep
  :: { inRepo :: Set String, resolved :: Set String, inSet :: Set String }
  -> String
  -> Resolution
classifyDep s n
  | Set.member n s.inRepo = InRepo
  | Set.member n s.resolved = Resolved
  | Set.member n s.inSet = UnresolvedRegistry
  | otherwise = Unknown

-- | Detect a cycle in a dependency graph (adjacency map; names absent as keys are leaves). Returns
-- | the cycle path (`a → b → a`) if one exists.
findCycle :: Map String (Array String) -> Maybe (Array String)
findCycle g = go (Array.fromFoldable (Map.keys g)) Set.empty
  where
  go nodes finished = case Array.uncons nodes of
    Nothing -> Nothing
    Just { head, tail } -> case dfs head [] finished of
      Left cyc -> Just cyc
      Right finished' -> go tail finished'

  dfs node path finished
    | Array.elem node path = Left (Array.snoc (Array.dropWhile (_ /= node) path) node)
    | Set.member node finished = Right finished
    | otherwise = case visit (Array.snoc path node) finished (depsOf node) of
        Left cyc -> Left cyc
        Right finished' -> Right (Set.insert node finished')

  visit path finished deps = case Array.uncons deps of
    Nothing -> Right finished
    Just { head, tail } -> case dfs head path finished of
      Left cyc -> Left cyc
      Right finished' -> visit path finished' tail

  depsOf node = fromMaybe [] (Map.lookup node g)

-- | The transitive closure of `seeds` over the in-repo dependency map, restricted to in-repo
-- | packages (registry deps are leaves). Used to know which `packages/<name>/src` trees to stage.
inRepoClosure :: Set String -> Map String (Array String) -> Array String -> Set String
inRepoClosure inRepoSet depMap = go Set.empty
  where
  go seen worklist = case Array.uncons worklist of
    Nothing -> seen
    Just { head, tail }
      | Set.member head seen -> go seen tail
      | otherwise ->
          let
            nexts = Array.filter (\d -> Set.member d inRepoSet) (fromMaybe [] (Map.lookup head depMap))
          in
            go (Set.insert head seen) (tail <> nexts)
