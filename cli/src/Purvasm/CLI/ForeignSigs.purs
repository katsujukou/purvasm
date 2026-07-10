-- | Per-module foreign-signature resolution (ADR-0080 §1). Foreign-signature reconstruction is
-- | purely syntactic and **module-local**, so — like separate compilation itself (ADR-0033) —
-- | it is done one module at a time, never by sweeping a loaded closure. A module's shapes come
-- | from its *provenance*:
-- |
-- |   * a **ulib-overlaid** module ships no source, so its shapes are read from the aggregated
-- |     `ulib.json` `foreignSigs` (author-declared, validated at `ulib-tools build` against the
-- |     patch source — the consumer trusts it);
-- |   * any other module (a `.spago` registry package, or the app's own `src/`) is reconstructed
-- |     from its source, located via spago's `cache-db.json`.
-- |
-- | The two static inputs (the aggregated `foreignSigs` map and `cache-db.json`) are read once
-- | into [Env] — they are compiled inputs (a manifest + the build cache), not a closure. A
-- | module with no foreigns is never touched.
module Purvasm.CLI.ForeignSigs
  ( ForeignSigMap
  , Env
  , loadEnv
  , moduleForeignSigs
  , moduleForeignSigsE
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (foldr, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Fmt as Fmt
import Foreign.Object as Object
import PureScript.CoreFn.Module (Module)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.ForeignSig (ForeignShape, reconstructModule, shapeFromJson)
import Run (Run)
import Run.Except (EXCEPT, runExcept, throw)
import Type.Row (type (+))

-- | Qualified key (`Module.Name.ident`) → calling shape (ADR-0080 §2).
type ForeignSigMap = Map String ForeignShape

-- | The static inputs a per-module resolution reads: the aggregated ulib `foreignSigs` (for
-- | overlaid modules) and spago's module → `.purs` cache-db (for reconstructed ones). Each is kept
-- | as a **deferred decode** (`Either` parse error / value): a malformed manifest is not a build-time
-- | throw but returned data, surfaced only when a module that actually consults that input reaches it —
-- | so an FSR input failure becomes a per-module `Left` (→ `ForeignSigFailed`, ADR-0090 §2), never a
-- | host-side abort before the driver runs.
type Env =
  { ulibDir :: FilePath
  , ulibSigs :: Either String ForeignSigMap
  , cacheDb :: Either String (Map String FilePath)
  }

-- | Read the two static inputs once. Non-fallible: absence ⇒ empty, and a decode failure is captured as
-- | a `Left` in [Env] (forced per-module by the provenance branch that needs it), not thrown here.
loadEnv
  :: forall r
   . { ulibDir :: FilePath, corefnDir :: FilePath }
  -> Run (FS + r) Env
loadEnv dirs = do
  ulibSigs <- loadUlibSigs dirs.ulibDir
  cacheDb <- loadCacheDb dirs.corefnDir
  pure { ulibDir: dirs.ulibDir, ulibSigs, cacheDb }

-- | One module's foreign shapes (ADR-0033 separate-compilation granularity): syntactic and
-- | module-local, no dependency on any other module. Empty when the module declares no foreigns.
moduleForeignSigs
  :: forall r
   . Env
  -> Module
  -> Run (FS + EXCEPT String + r) ForeignSigMap
moduleForeignSigs env m
  | Array.null m.foreignNames = pure Map.empty
  | otherwise = do
      let name = nameKey m.name
      overlaid <- isUlibOverlaid env.ulibDir name
      entries <-
        if overlaid then fromUlib env name m.foreignNames
        else fromSource env name m.foreignNames
      pure (Map.fromFoldable entries)

-- | `moduleForeignSigs` with its `EXCEPT String` discharged to a returned `Either` — the shape the
-- | build driver's `CompilerAction.foreignSigsOf` capability needs (ADR-0090 §2: FSR failure is
-- | returned data, not a throw, so the driver can halt with `ForeignSigFailed`).
moduleForeignSigsE
  :: forall r
   . Env
  -> Module
  -> Run (FS + r) (Either String ForeignSigMap)
moduleForeignSigsE env m = runExcept (moduleForeignSigs env m)

-- --- provenance branches --------------------------------------------------------------------

-- | A ulib-overlaid module's shapes come from the validated `foreignSigs`; every corefn foreign
-- | must be present there (the ulib build guaranteed it, so an absence means a stale overlay).
fromUlib
  :: forall r
   . Env
  -> String
  -> Array String
  -> Run (FS + EXCEPT String + r) (Array (String /\ ForeignShape))
fromUlib env name foreigns = do
  ulibSigs <- either throw pure env.ulibSigs
  for foreigns \fn -> do
    let key = name <> "." <> fn
    case Map.lookup key ulibSigs of
      Just shape -> pure (key /\ shape)
      Nothing ->
        throw $ Fmt.fmt
          @"foreign-sigs: {key}: ulib-overlaid but absent from ulib.json foreignSigs (restage the ulib)"
          { key }

-- | A non-ulib module is reconstructed from its source (`cache-db.json`-located). Every corefn
-- | foreign must be signed by the source; a module-name skew or missing source is a hard error.
fromSource
  :: forall r
   . Env
  -> String
  -> Array String
  -> Run (FS + EXCEPT String + r) (Array (String /\ ForeignShape))
fromSource env name foreigns = do
  cacheDb <- either throw pure env.cacheDb
  path <- case Map.lookup name cacheDb of
    Just p -> pure p
    Nothing ->
      throw $ Fmt.fmt
        @"foreign-sigs: {name}: not in cache-db.json (rebuild with spago)"
        { name }
  src <- FS.readText path >>= case _ of
    Just s -> pure s
    Nothing -> throw $ Fmt.fmt @"foreign-sigs: {name}: cannot read source {path}" { name, path }
  case reconstructModule src of
    Left issue -> throw $ Fmt.fmt @"foreign-sigs: {name}: {issue}" { name, issue: show issue }
    Right rec -> do
      when (rec.moduleName /= name)
        $ throw
        $ Fmt.fmt
            @"foreign-sigs: {name}: source declares module {other} (stale or misplaced source)"
            { name, other: rec.moduleName }
      let signed = map (\(ident /\ _) -> ident) rec.sigs
      for_ foreigns \fn ->
        when (not (Array.elem fn signed))
          $ throw
          $ Fmt.fmt
              @"foreign-sigs: {name}: corefn declares foreign {fn} but the source does not"
              { name, fn }
      pure (map (\(ident /\ shape) -> (name <> "." <> ident) /\ shape) rec.sigs)

-- --- static input loaders -------------------------------------------------------------------

isUlibOverlaid :: forall r. FilePath -> String -> Run (FS + r) Boolean
isUlibOverlaid ulibDir name =
  FS.joinPath [ ulibDir, name, "corefn.json" ] >>= FS.exists

-- | The aggregated `foreignSigs` of `<ulibDir>/ulib.json` (absent file / field ⇒ empty). A decode
-- | failure is returned as `Left` (deferred, ADR-0090 §2), not thrown.
loadUlibSigs :: forall r. FilePath -> Run (FS + r) (Either String ForeignSigMap)
loadUlibSigs ulibDir = do
  path <- FS.joinPath [ ulibDir, "ulib.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure (Right Map.empty)
    Just raw -> case jsonParser raw >>= parseSigs of
      Left err -> pure (Left (Fmt.fmt @"foreign-sigs: {path}: {err}" { path, err }))
      Right m -> pure (Right m)
  where
  parseSigs json = do
    top <- note "ulib.json: top level is not an object" (toObject json)
    case Object.lookup "foreignSigs" top of
      Nothing -> Right Map.empty
      Just f -> do
        fo <- note "ulib.json: 'foreignSigs' must be an object" (toObject f)
        pairs <- for (Object.toUnfoldable fo :: Array (String /\ Json)) \(k /\ v) ->
          (\s -> k /\ s) <$> shapeFromJson v
        Right (Map.fromFoldable pairs)
  note msg = case _ of
    Just a -> Right a
    Nothing -> Left msg

-- | Parse `<corefnDir>/cache-db.json` to module → `.purs` path (absent ⇒ empty). A decode failure is
-- | returned as `Left` (deferred, ADR-0090 §2), not thrown.
loadCacheDb :: forall r. FilePath -> Run (FS + r) (Either String (Map String FilePath))
loadCacheDb corefnDir = do
  path <- FS.joinPath [ corefnDir, "cache-db.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure (Right Map.empty)
    Just raw -> case jsonParser raw of
      Left err -> pure (Left (Fmt.fmt @"foreign-sigs: {path}: {err}" { path, err }))
      Right json -> pure (Right (projectCacheDb json))

projectCacheDb :: Json -> Map String FilePath
projectCacheDb json = case toObject json of
  Nothing -> Map.empty
  Just top -> foldr add Map.empty (Object.toUnfoldable top :: Array (String /\ Json))
  where
  add (name /\ entry) acc = case toObject entry of
    Nothing -> acc
    Just files -> case Array.head (filter isPursPath (Object.keys files)) of
      Just p -> Map.insert name p acc
      Nothing -> acc
  isPursPath p = isJust (String.stripSuffix (Pattern ".purs") p)
