-- | `ulib-tools test`: run each `ulib` package's upstream behaviour suite at the fidelity its
-- | manifest declares (ADR-0048). Phase 1 — the `js`-fidelity path — is implemented here: clone the
-- | upstream `purescript-*` repo at the pinned ref, overlay the `ulib` patch over its `src/`, generate
-- | a standalone `spago.yaml` (the upstream repos ship `bower.json`, not a spago workspace), and
-- | delegate compile / dependency-resolution / run to `spago test` on node. `native`/`bespoke`
-- | packages need `purvm`, which cannot yet run a `Test.Assert` suite, so they are reported as
-- | **skipped (Phase 2)** rather than silently omitted.
module Purvasm.UlibTools.Test
  ( Options
  , options
  , cmd
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Fmt as Fmt
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.UlibTools.Manifest (Fidelity(..), TestSpec)
import Purvasm.UlibTools.Manifest as Manifest
import Purvasm.UlibTools.Stage as Stage
import Run (Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { package :: Maybe String
  , ulibDir :: FilePath
  , packagesDir :: FilePath
  , cacheDir :: FilePath
  , rootSpago :: FilePath
  }

options :: ArgParser Options
options = fromRecord
  { package:
      ArgParser.argument [ "--package" ]
        "Restrict to a single ulib package (default: all)."
        # ArgParser.optional
  , ulibDir:
      ArgParser.argument [ "--ulib" ]
        "ulib patch source dir. Defaults to 'ulib'."
        # ArgParser.default "ulib"
  , packagesDir:
      ArgParser.argument [ "--packages-dir" ]
        "In-repo packages dir for resolving declared deps. Defaults to 'packages'."
        # ArgParser.default "packages"
  , cacheDir:
      ArgParser.argument [ "--cache-dir" ]
        "Cache dir for cloned upstream suites (keyed repo@ref, so re-runs are offline).\n\
        \Defaults to '.ulib-test-cache'."
        # ArgParser.default ".ulib-test-cache"
  , rootSpago:
      ArgParser.argument [ "--root-spago" ]
        "Workspace spago.yaml to read the package-set registry version from. Defaults to 'spago.yaml'."
        # ArgParser.default "spago.yaml"
  }

-- | The outcome of one package's suite.
data Status = Passed | Failed | SkippedPhase2 Fidelity | NoTests

type Result = { package :: String, status :: Status }

cmd :: forall r. Options -> Run (PROC + FS + LOG + EXCEPT String + r) Unit
cmd opts = do
  patches <- Stage.collectPatches opts.ulibDir
  let
    allPkgs = Array.nub (map _.package patches)
    selected = case opts.package of
      Just p -> Array.filter (_ == p) allPkgs
      Nothing -> allPkgs
  when (Array.null selected) do
    throw "ulib-tools test: no packages found (check --ulib / --package)."

  registry <- readRegistry opts.rootSpago
  results <- traverse (runPackage opts registry) selected
  report results

-- | Run (or skip) one package's suite, dispatching on its declared fidelity.
runPackage
  :: forall r
   . Options
  -> String
  -> String
  -> Run (PROC + FS + LOG + EXCEPT String + r) Result
runPackage opts registry package = do
  readTestSpec opts.ulibDir package >>= case _ of
    Nothing -> do
      Log.info $ Fmt.fmt @"test: {package}: no test block (not yet tested)" { package }
      pure { package, status: NoTests }
    Just spec -> case spec.fidelity of
      JsFidelity -> do
        unless (Array.null spec.xfail) do
          Log.warn $ Fmt.fmt
            @"test: {package}: xfail is ignored on a js-fidelity suite (run whole-suite; ADR-0048)"
            { package }
        ok <- runJsSuite opts registry package spec
        pure { package, status: if ok then Passed else Failed }
      fidelity -> do
        Log.warn $ Fmt.fmt
          @"test: {package}: skipped — {fidelity}-fidelity needs purvm, deferred to Phase 2 (ADR-0048)"
          { package, fidelity: show fidelity }
        pure { package, status: SkippedPhase2 fidelity }

-- | Phase 1: prepare and run the upstream suite on node via `spago test`. Returns the pass/fail.
runJsSuite
  :: forall r
   . Options
  -> String
  -> String
  -> TestSpec
  -> Run (PROC + FS + LOG + EXCEPT String + r) Boolean
runJsSuite opts registry package spec = do
  let slug = Manifest.repoSlug spec.repo
  cloneDir <- FS.joinPath [ opts.cacheDir, slug <> "@" <> spec.ref ]
  ensureClone opts cloneDir package spec

  -- Overlay this package's patches over the clone's own sources (the clone *is* the registry
  -- package, so the patched modules replace their originals in place).
  srcDir <- FS.joinPath [ cloneDir, "src" ]
  patches <- Array.filter (\p -> p.package == package) <$> Stage.collectPatches opts.ulibDir
  Stage.overlayPatches opts.ulibDir srcDir patches

  yaml <- genSpagoYaml opts registry package spec cloneDir
  spagoPath <- FS.joinPath [ cloneDir, "spago.yaml" ]
  FS.writeText spagoPath yaml

  Log.info $ Fmt.fmt @"test: {package}: running suite (spago test)…" { package }
  res <- Proc.exec "sh" [ "-c", "cd " <> cloneDir <> " && spago test" ]
  pure (either (const false) (const true) res)

-- | Clone the upstream suite into the cache if not already present (keyed `repo@ref`); drop the
-- | `.git` directory so the cache is a plain source snapshot.
ensureClone
  :: forall r
   . Options
  -> FilePath
  -> String
  -> TestSpec
  -> Run (PROC + FS + LOG + EXCEPT String + r) Unit
ensureClone _ cloneDir package spec = do
  bowerPath <- FS.joinPath [ cloneDir, "bower.json" ]
  unlessM (FS.exists bowerPath) do
    Log.info $ Fmt.fmt @"test: {package}: cloning {repo}@{ref}" { package, repo: spec.repo, ref: spec.ref }
    _ <- Stage.requireOk =<< Proc.exec "git"
      [ "clone", "--depth", "1", "--branch", spec.ref, spec.repo, cloneDir ]
    gitDir <- FS.joinPath [ cloneDir, ".git" ]
    void $ Proc.exec "rm" [ "-rf", gitDir ]

-- | Generate the standalone `spago.yaml` for the prepared clone: the patched package's own deps
-- | (from its `bower.json`) plus `purvasm-base` and any in-repo declared deps as local
-- | `extraPackages`, the suite's `testDeps`, and the workspace package set.
genSpagoYaml
  :: forall r
   . Options
  -> String
  -> String
  -> TestSpec
  -> FilePath
  -> Run (FS + EXCEPT String + r) String
genSpagoYaml opts registry package spec cloneDir = do
  bowerDeps <- readBowerDeps cloneDir package
  manifestDeps <- readManifestDeps opts.ulibDir package
  inRepoSet <- inRepoPackages opts.packagesDir

  -- purvasm-base is always a local dependency; in-repo declared deps (e.g. purvasm-json) are too.
  let
    extraNames = Array.nub ([ "purvasm-base" ] <> Array.filter (\d -> Set.member d inRepoSet) manifestDeps)
    packageDeps = Array.nub (bowerDeps <> [ "purvasm-base" ] <> manifestDeps)
  extras <- traverse (\name -> { name, path: _ } <$> absPackagePath opts.packagesDir name) extraNames

  pure $ String.joinWith "\n"
    ( [ "package:"
      , "  name: " <> package
      , "  dependencies:"
      ]
        <> map (\d -> "    - " <> d) packageDeps
        <>
          [ "  test:"
          , "    main: " <> spec.testMain
          , "    dependencies:" <> if Array.null spec.testDeps then " []" else ""
          ]
        <> map (\d -> "      - " <> d) spec.testDeps
        <>
          [ "workspace:"
          , "  packageSet:"
          , "    registry: " <> registry
          , "  extraPackages:"
          ]
        <> Array.concatMap (\e -> [ "    " <> e.name <> ":", "      path: " <> e.path ]) extras
        <> [ "" ]
    )

-- | An in-repo package's absolute path (spago resolves `extraPackages` paths against the generated
-- | `spago.yaml`'s own location, not our cwd, so the path must be absolute).
absPackagePath :: forall r. FilePath -> String -> Run (FS + r) FilePath
absPackagePath packagesDir name = do
  rel <- FS.joinPath [ packagesDir, name ]
  FS.resolvePath [] rel

-- | The package-set registry version of the workspace `spago.yaml`.
readRegistry :: forall r. FilePath -> Run (FS + EXCEPT String + r) String
readRegistry rootSpago =
  FS.readText rootSpago >>= case _ of
    Nothing -> throw $ Fmt.fmt @"test: cannot read workspace spago.yaml at {rootSpago}" { rootSpago }
    Just src -> maybe
      (throw $ Fmt.fmt @"test: no package-set 'registry:' in {rootSpago}" { rootSpago })
      pure
      (Manifest.parseRegistryVersion src)

-- | The upstream package's own dependencies, from the clone's `bower.json`.
readBowerDeps :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Array String)
readBowerDeps cloneDir package = do
  path <- FS.joinPath [ cloneDir, "bower.json" ]
  FS.readText path >>= case _ of
    Nothing -> throw $ Fmt.fmt @"test: {package}: no bower.json in clone (cannot derive deps)" { package }
    Just src -> either
      (\e -> throw $ Fmt.fmt @"test: {package}: {e}" { package, e })
      pure
      (Manifest.parseBowerDependencies src)

-- | A `ulib` package's declared dependencies (`ulib.json`'s `dependencies`; absent ⇒ none).
readManifestDeps :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Array String)
readManifestDeps ulibDir package = do
  path <- FS.joinPath [ ulibDir, package, "ulib.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure []
    Just src -> either
      (\e -> throw $ Fmt.fmt @"test: {package}/ulib.json: {e}" { package, e })
      pure
      (Manifest.parseDependencies src)

-- | A `ulib` package's `test` spec (`Nothing` if it declares none).
readTestSpec :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Maybe TestSpec)
readTestSpec ulibDir package = do
  path <- FS.joinPath [ ulibDir, package, "ulib.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure Nothing
    Just src -> either
      (\e -> throw $ Fmt.fmt @"test: {package}/ulib.json: {e}" { package, e })
      pure
      (Manifest.parseTest src)

-- | The in-repo package names (subdirectories of `packagesDir` with a `src/`).
inRepoPackages :: forall r. FilePath -> Run (FS + r) (Set String)
inRepoPackages packagesDir = do
  entries <- fromMaybe [] <$> FS.readDir packagesDir
  Set.fromFoldable <$> Array.filterA hasSrc entries
  where
  hasSrc name = FS.joinPath [ packagesDir, name, "src" ] >>= FS.exists

report :: forall r. Array Result -> Run (LOG + EXCEPT String + r) Unit
report results = do
  for_ results \{ package, status } -> case status of
    Passed -> Log.info $ Fmt.fmt @"test: {package}: PASS" { package }
    Failed -> Log.error $ Fmt.fmt @"test: {package}: FAIL" { package }
    SkippedPhase2 f -> Log.warn $ Fmt.fmt @"test: {package}: skipped ({f}, Phase 2)" { package, f: show f }
    NoTests -> pure unit
  let
    failed = Array.filter (isFailed <<< _.status) results
    ran = Array.filter (isRan <<< _.status) results
  if Array.null failed then
    Log.info $ Fmt.fmt @"test: {n} suite(s) passed." { n: show (Array.length ran) }
  else
    throw $ Fmt.fmt @"test: {n} suite(s) failed." { n: show (Array.length failed) }
  where
  isFailed = case _ of
    Failed -> true
    _ -> false
  isRan = case _ of
    Passed -> true
    Failed -> true
    _ -> false
