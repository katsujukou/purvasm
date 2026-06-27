-- | Shared staging machinery for the `ulib` patches: assemble a `purs`-compilable source tree
-- | (resolved registry sources + `purvasm-base` + each patch's declared extra dependencies,
-- | optionally overlaid with the patches) and compile it to corefn. Used by `build` (extract the
-- | patched corefn), `verify` (diff the patched vs. registry surface), and `validate` (dependency
-- | checks). Path handling goes through the `Filesystem` effect (ADR-0043). Dependency declaration,
-- | resolution, and validation follow ADR-0047.
module Purvasm.UlibTools.Stage
  ( Patch
  , Resolution
  , modulePath
  , mkTemp
  , requireOk
  , copyTree
  , collectPatches
  , stageRegistry
  , overlayPatches
  , compileCorefn
  , validate
  , stageDeclaredDeps
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Fmt as Fmt
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.UlibTools.Manifest (Resolution(..))
import Purvasm.UlibTools.Manifest as Manifest
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

-- | A patched module: the package subdir it lives under and its dotted module name.
type Patch = { package :: String, modName :: String }

-- | Re-export so callers can pattern-match resolution outcomes without importing `Manifest`.
type Resolution = Manifest.Resolution

-- | A dotted module name to its slash path (`Data.Array.ST` -> `Data/Array/ST`).
modulePath :: String -> String
modulePath = String.replaceAll (Pattern ".") (String.Replacement "/")

-- | A fresh temp directory (`mktemp -d`), trimmed of the trailing newline.
mkTemp :: forall r. Run (PROC + EXCEPT String + r) FilePath
mkTemp = String.trim <$> (requireOk =<< Proc.execCapture "mktemp" [ "-d" ])

-- | Surface a `Proc`/tool failure through `EXCEPT`.
requireOk :: forall r a. Either String a -> Run (EXCEPT String + r) a
requireOk = either throw pure

-- | Recursively copy a directory's *contents* into `dst` (the shell's `cp -R src/. dst/`). The
-- | `/.` idiom is a shell-cp argument, not a logical path, so it is built by hand (a `joinPath`
-- | would normalise the `.` away).
copyTree :: forall r. FilePath -> FilePath -> Run (PROC + EXCEPT String + r) Unit
copyTree src dst = void $ requireOk =<< Proc.exec "cp" [ "-R", src <> "/.", dst <> "/" ]

-- | Copy the resolved package-set sources (registry `<pkg>/src`, git `<pkg>/<ref>/src`) plus the
-- | `purvasm-base` primitives into `srcDir` — the registry baseline both `build` and `verify` need.
stageRegistry :: forall r. FilePath -> FilePath -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
stageRegistry spagoPkgs baseDir srcDir = do
  pkgs <- listDir spagoPkgs
  for_ pkgs \pkg -> do
    pkgDir <- FS.joinPath [ spagoPkgs, pkg ]
    copyIfSrc pkgDir
    refs <- listDir pkgDir
    for_ refs \ref -> do
      refDir <- FS.joinPath [ pkgDir, ref ]
      copyIfSrc refDir
  copyTree baseDir srcDir
  where
  copyIfSrc dir = do
    src <- FS.joinPath [ dir, "src" ]
    whenM (FS.exists src) (copyTree src srcDir)

-- | Enumerate the patches under `ulibDir`: each `<package>/<Module>.purs` (mindepth 2). Non-`.purs`
-- | files (e.g. `ulib.json`) are ignored.
collectPatches :: forall r. FilePath -> Run (FS + r) (Array Patch)
collectPatches ulibDir = do
  pkgs <- listDir ulibDir
  map join $ traverse perPackage pkgs
  where
  perPackage pkg = do
    dir <- FS.joinPath [ ulibDir, pkg ]
    files <- listDir dir
    pure $ Array.mapMaybe (toPatch pkg) files
  toPatch pkg file = { package: pkg, modName: _ } <$> String.stripSuffix (Pattern ".purs") file

-- | Overlay the patches over the staged registry modules, dropping each sibling registry `.js` so a
-- | kept `foreign import` resolves to a boot intrinsic at link time (ADR-0038).
overlayPatches :: forall r. FilePath -> FilePath -> Array Patch -> Run (FS + EXCEPT String + r) Unit
overlayPatches ulibDir srcDir patches = for_ patches overlay
  where
  overlay { package, modName } = do
    let modPath = modulePath modName
    srcPath <- FS.joinPath [ ulibDir, package, modName <> ".purs" ]
    destPurs <- FS.joinPath [ srcDir, modPath <> ".purs" ]
    destJs <- FS.joinPath [ srcDir, modPath <> ".js" ]
    content <- FS.readText srcPath >>= maybe
      (throw $ Fmt.fmt @"ulib: cannot read patch {srcPath}" { srcPath })
      pure
    FS.dirname destPurs >>= FS.mkdirP
    FS.writeText destPurs content
    whenM (FS.exists destJs) (FS.unlink destJs)

-- | Compile a staged source tree to corefn (`purs` expands the `**` glob itself).
compileCorefn :: forall r. String -> FilePath -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
compileCorefn purs srcDir outputDir = do
  srcGlobs <- FS.joinPath [ srcDir, "**", "*.purs" ]
  requireOk =<< Proc.exec purs
    [ "compile", "--codegen", "corefn", "--output", outputDir, srcGlobs ]

-- ADR-0047: per-patch extra dependencies — declaration, validation, staging.

type Dirs r = { ulibDir :: FilePath, packagesDir :: FilePath | r }

-- | A `ulib` package's declared dependencies, from `ulib/<package>/ulib.json` (absent ⇒ none).
readManifestDeps :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Array String)
readManifestDeps ulibDir pkg = do
  path <- FS.joinPath [ ulibDir, pkg, "ulib.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure []
    Just src -> either
      (\e -> throw $ Fmt.fmt @"ulib: {pkg}/ulib.json: {e}" { pkg, e })
      pure
      (Manifest.parseDependencies src)

-- | The in-repo package names: subdirectories of `packagesDir` that have a `src/`.
inRepoPackages :: forall r. FilePath -> Run (FS + r) (Array String)
inRepoPackages packagesDir = do
  entries <- listDir packagesDir
  Array.filterA hasSrc entries
  where
  hasSrc name = FS.joinPath [ packagesDir, name, "src" ] >>= FS.exists

-- | An in-repo package's own `package.dependencies` (from its `spago.yaml`).
readSpagoDeps :: forall r. FilePath -> String -> Run (FS + r) (Array String)
readSpagoDeps packagesDir pkg = do
  path <- FS.joinPath [ packagesDir, pkg, "spago.yaml" ]
  FS.readText path >>= case _ of
    Nothing -> pure []
    Just src -> pure (Manifest.parseSpagoDependencies src)

-- | The package-set membership, via `spago ls packages`.
packageSetNames :: forall r. Run (PROC + EXCEPT String + r) (Set String)
packageSetNames =
  Manifest.parsePackageSet <$> (requireOk =<< Proc.execCapture "spago" [ "ls", "packages" ])

-- | The names resolved into `.spago/p` (version-stripped).
resolvedNames :: forall r. FilePath -> Run (FS + r) (Set String)
resolvedNames spagoPkgs = (Set.fromFoldable <<< map Manifest.stripVersion) <$> listDir spagoPkgs

-- | Validate the patches' declared dependencies (ADR-0047 §3): every declared dependency resolves
-- | (in-repo or registry) and the authored dependency graph has no cycle. Hard-errors otherwise.
validate :: forall r. Dirs (spagoPkgs :: FilePath) -> Array Patch -> Run (PROC + FS + EXCEPT String + r) Unit
validate { ulibDir, packagesDir, spagoPkgs } patches = do
  let ulibPkgs = Array.nub (map _.package patches)
  manifestPairs <- traverse (\p -> Tuple p <$> readManifestDeps ulibDir p) ulibPkgs

  inRepoArr <- inRepoPackages packagesDir
  let inRepoSet = Set.fromFoldable inRepoArr
  resolved <- resolvedNames spagoPkgs
  inSet <- packageSetNames
  let sets = { inRepo: inRepoSet, resolved, inSet }

  for_ manifestPairs \(Tuple pkg deps) -> for_ deps \dep ->
    case Manifest.classifyDep sets dep of
      InRepo -> pure unit
      Resolved -> pure unit
      UnresolvedRegistry -> throw $ Fmt.fmt
        @"ulib: {pkg} declares '{dep}', a registry package not resolved into .spago/p; add it to a workspace package's dependencies."
        { pkg, dep }
      Unknown -> throw $ Fmt.fmt
        @"ulib: {pkg} declares '{dep}', which is neither an in-repo package nor a known registry package."
        { pkg, dep }

  inRepoDepPairs <- traverse (\p -> Tuple p <$> readSpagoDeps packagesDir p) inRepoArr
  let adjacency = Map.union (Map.fromFoldable manifestPairs) (Map.fromFoldable inRepoDepPairs)
  case Manifest.findCycle adjacency of
    Nothing -> pure unit
    Just cyc -> throw $ Fmt.fmt @"ulib: dependency cycle: {path}"
      { path: String.joinWith " → " cyc }

-- | Stage the in-repo packages the patches declare as dependencies (ADR-0047 §2), transitively over
-- | their own in-repo dependencies. Registry dependencies need no staging — `.spago/p` is staged
-- | wholesale by `stageRegistry`.
stageDeclaredDeps :: forall r. Dirs () -> Array Patch -> FilePath -> Run (PROC + FS + EXCEPT String + r) Unit
stageDeclaredDeps { ulibDir, packagesDir } patches srcDir = do
  let ulibPkgs = Array.nub (map _.package patches)
  declared <- Array.concat <$> traverse (readManifestDeps ulibDir) ulibPkgs
  inRepoArr <- inRepoPackages packagesDir
  let inRepoSet = Set.fromFoldable inRepoArr
  inRepoDepPairs <- traverse (\p -> Tuple p <$> readSpagoDeps packagesDir p) inRepoArr
  let
    depMap = Map.fromFoldable inRepoDepPairs
    seeds = Array.filter (\d -> Set.member d inRepoSet) declared
    toStage = Manifest.inRepoClosure inRepoSet depMap seeds
  for_ (Set.toUnfoldable toStage :: Array String) \name -> do
    src <- FS.joinPath [ packagesDir, name, "src" ]
    copyTree src srcDir

-- | Directory entries via the FS effect (`Nothing`/absent → empty).
listDir :: forall r. FilePath -> Run (FS + r) (Array String)
listDir dir = fromMaybe [] <$> FS.readDir dir
