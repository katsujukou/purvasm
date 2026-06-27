-- | `ulib-tools verify`: build-time patch-faithfulness (ADR-0043 §5). Compile both the registry
-- | baseline and the patched tree to corefn, then diff each patched module's exported-name surface
-- | (`exports` + `reExports`) against the registry module's. A patch that *narrows* the surface
-- | (drops an export the registry has — the `STArray(..)`-style drift) fails; one that *adds*
-- | exports warns. This is independent of boot.
-- |
-- | Note (ADR-0043 §5): the pinned `purs` (0.15.16) has no `docs --format json`, so the surface is
-- | the corefn export set (names), not full type signatures. Signature-level faithfulness needs
-- | externs/`purs publish` and is deferred.
module Purvasm.UlibTools.Verify
  ( Options
  , Surface
  , Drift
  , options
  , cmd
  , parseSurface
  , diffSurface
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Argonaut.Core (toArray, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either, either, note)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Fmt as Fmt
import Foreign.Object as Object
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.UlibTools.Stage (Patch)
import Purvasm.UlibTools.Stage as Stage
import Run (EFFECT, Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { package :: Maybe String
  , ulibDir :: FilePath
  , baseDir :: FilePath
  , packagesDir :: FilePath
  , purs :: String
  , spagoPkgs :: FilePath
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
  , baseDir:
      ArgParser.argument [ "--base" ]
        "purvasm-base src dir. Defaults to 'packages/purvasm-base/src'."
        # ArgParser.default "packages/purvasm-base/src"
  , packagesDir:
      ArgParser.argument [ "--packages-dir" ]
        "In-repo packages dir for resolving declared deps. Defaults to 'packages'."
        # ArgParser.default "packages"
  , purs:
      ArgParser.argument [ "--purs" ]
        "purs executable. Defaults to 'purs'."
        # ArgParser.default "purs"
  , spagoPkgs:
      ArgParser.argument [ "--spago-pkgs" ]
        "Resolved package-set sources dir. Defaults to '.spago/p'."
        # ArgParser.default ".spago/p"
  }

-- | A module's public surface: the set of exported (and re-exported) names.
type Surface = Set String

-- | The surface delta of a patched module against its registry original.
type Drift =
  { modName :: String
  , missing :: Array String -- in the registry, dropped by the patch (narrowing — a failure)
  , extra :: Array String -- added by the patch, absent upstream (a warning)
  }

-- | The verdict for one ulib module: either it patches an existing registry module (so its surface
-- | is compared), or it is a ulib-introduced module with no registry counterpart (e.g. an `*.Internal`
-- | helper of a reimplemented package, ADR-0044) — nothing upstream to narrow, faithful by construction.
data Outcome = Compared Drift | NewModule String

cmd :: forall r. Options -> Run (PROC + ENV + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  allPatches <- Stage.collectPatches opts.ulibDir
  let
    patches = case opts.package of
      Just p -> Array.filter (\x -> x.package == p) allPatches
      Nothing -> allPatches
  when (Array.null patches) do
    throw "ulib-tools verify: no patches found (check --ulib / --package)."

  Stage.validate { ulibDir: opts.ulibDir, packagesDir: opts.packagesDir, spagoPkgs: opts.spagoPkgs } allPatches

  Log.info "verify: compiling registry baseline…"
  baseline <- stage false
  Log.info "verify: compiling patched tree…"
  patched <- stage true

  drifts <- traverse (checkModule baseline.output patched.output) patches

  _ <- Proc.exec "rm" [ "-rf", baseline.work ]
  _ <- Proc.exec "rm" [ "-rf", patched.work ]

  report drifts
  where
  -- Stage the registry baseline, optionally overlaying every patch, and compile it to corefn.
  stage overlay = do
    work <- Stage.mkTemp
    srcDir <- FS.joinPath [ work, "src" ]
    output <- FS.joinPath [ work, "output" ]
    FS.mkdirP srcDir
    Stage.stageRegistry opts.spagoPkgs opts.baseDir srcDir
    when overlay do
      all <- Stage.collectPatches opts.ulibDir
      Stage.stageDeclaredDeps { ulibDir: opts.ulibDir, packagesDir: opts.packagesDir } all srcDir
      Stage.overlayPatches opts.ulibDir srcDir all
    Stage.compileCorefn opts.purs srcDir output
    pure { work, output }

checkModule
  :: forall r
   . FilePath
  -> FilePath
  -> Patch
  -> Run (FS + EXCEPT String + r) Outcome
checkModule baselineOut patchedOut { modName } = do
  mUp <- readSurface baselineOut modName
  mPa <- readSurface patchedOut modName
  case mPa of
    -- The patch was overlaid, so it must have compiled; a missing corefn here is a real failure.
    Nothing -> throw $ Fmt.fmt @"verify: patched corefn missing for {modName} (did it compile?)" { modName }
    Just pa -> case mUp of
      Nothing -> pure (NewModule modName)
      Just up -> pure (Compared (diffSurface modName up pa))

-- | A module's surface from its compiled corefn; `Nothing` if the tree produced no corefn for it
-- | (i.e. the module is absent from that tree).
readSurface :: forall r. FilePath -> String -> Run (FS + EXCEPT String + r) (Maybe Surface)
readSurface outputDir modName = do
  path <- FS.joinPath [ outputDir, modName, "corefn.json" ]
  FS.readText path >>= case _ of
    Nothing -> pure Nothing
    Just src -> Just <$> either throw pure (parseSurface src)

-- | The exported-name surface from a module's corefn: `exports` plus the flattened `reExports`.
parseSurface :: String -> Either String Surface
parseSurface src = do
  json <- jsonParser src
  obj <- note "corefn: top-level is not an object" (toObject json)
  pure $ Set.fromFoldable (namesAt "exports" obj <> reExportNames obj)
  where
  namesAt key o = fromMaybe [] do
    arr <- Object.lookup key o >>= toArray
    pure (Array.mapMaybe toString arr)
  reExportNames o = fromMaybe [] do
    ro <- Object.lookup "reExports" o >>= toObject
    pure $ Array.concatMap valNames (Object.values ro)
  valNames v = fromMaybe [] (Array.mapMaybe toString <$> toArray v)

diffSurface :: String -> Surface -> Surface -> Drift
diffSurface modName upstream patched =
  { modName
  , missing: Set.toUnfoldable (Set.difference upstream patched)
  , extra: Set.toUnfoldable (Set.difference patched upstream)
  }

report :: forall r. Array Outcome -> Run (LOG + EXCEPT String + r) Unit
report outcomes = do
  for_ outcomes case _ of
    NewModule m ->
      Log.info $ Fmt.fmt @"verify: {m}: new ulib module (no registry counterpart)" { m }
    Compared d ->
      if Array.null d.missing && Array.null d.extra then
        Log.info $ Fmt.fmt @"verify: {m}: ok" { m: d.modName }
      else do
        unless (Array.null d.missing) do
          Log.error $ Fmt.fmt @"verify: {m}: NARROWED — registry exports absent from patch: {xs}"
            { m: d.modName, xs: String.joinWith ", " d.missing }
        unless (Array.null d.extra) do
          Log.warn $ Fmt.fmt @"verify: {m}: extra exports not in registry: {xs}"
            { m: d.modName, xs: String.joinWith ", " d.extra }
  let narrowed = Array.filter isNarrowed outcomes
  if Array.null narrowed then
    Log.info $ Fmt.fmt @"verify: {n} module(s) interface-faithful (no narrowed exports)."
      { n: show (Array.length outcomes) }
  else
    throw $ Fmt.fmt @"verify: {n} module(s) narrowed the registry surface."
      { n: show (Array.length narrowed) }
  where
  isNarrowed = case _ of
    Compared d -> not (Array.null d.missing)
    NewModule _ -> false
