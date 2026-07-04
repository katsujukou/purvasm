-- | `ulib-tools build`: compile the `ulib` registry-package patches (ADR-0038) to corefn for
-- | boot's `--ulib` overlay. The typed port of `ulib-tools/install.sh` (ADR-0043 §1): overlay each
-- | `ulib/<package>/<Module>.purs` over the resolved registry sources plus `purvasm-base`, compile
-- | the lot to corefn with the pinned `purs`, and extract the patched modules into the flat lib
-- | layout `<out>/<Module>/corefn.json`.
module Purvasm.UlibTools.Build
  ( Options
  , options
  , cmd
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Fmt as Fmt
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
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
  { out :: Maybe FilePath
  , prepareRelease :: Boolean
  , ulibDir :: FilePath
  , baseDir :: FilePath
  , packagesDir :: FilePath
  , purs :: String
  , spagoPkgs :: FilePath
  }

options :: ArgParser Options
options = fromRecord
  { out:
      ArgParser.argument [ "--out" ]
        "Destination directory for the compiled corefn (writes <DIR>/<Module>/corefn.json per\n\
        \patched module). Use this for an ad-hoc build to a chosen path; to stage it for\n\
        \distribution use --prepare-release instead. One of --out / --prepare-release is required."
        # ArgParser.optional
  , prepareRelease:
      ArgParser.flag [ "--prepare-release" ]
        "Write the corefn to the default lib location for distribution with purvasm\n\
        \($PURVASM_LIB, else 'dist/ulib') — the path purvm reads by default. Alternative to --out."
        # ArgParser.boolean
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

cmd :: forall r. Options -> Run (PROC + ENV + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  out <- resolveOut opts
  patches <- Stage.collectPatches opts.ulibDir
  Stage.validate { ulibDir: opts.ulibDir, packagesDir: opts.packagesDir, spagoPkgs: opts.spagoPkgs } patches

  work <- Stage.mkTemp
  srcDir <- FS.joinPath [ work, "src" ]
  outputDir <- FS.joinPath [ work, "output" ]
  FS.mkdirP srcDir

  Stage.stageRegistry opts.spagoPkgs opts.baseDir srcDir
  Stage.stageDeclaredDeps { ulibDir: opts.ulibDir, packagesDir: opts.packagesDir } patches srcDir
  Stage.overlayPatches opts.ulibDir srcDir patches
  Stage.compileCorefn opts.purs srcDir outputDir

  FS.mkdirP out
  for_ patches (extractCorefn outputDir out)
  -- ADR-0073: stage any native `foreign` `.c` + the aggregated manifest beside the corefn (all ulib
  -- dirs, patch-bearing or `.c`-only).
  Stage.stageNativeForeign opts.ulibDir out

  _ <- Proc.exec "rm" [ "-rf", work ]
  Log.info $ Fmt.fmt @"ulib: wrote patched corefn to {out}" { out }

-- | `--out` wins; otherwise `--prepare-release` resolves the default location ($PURVASM_LIB, else
-- | 'dist/ulib'); with neither, fail loudly.
resolveOut :: forall r. Options -> Run (ENV + EXCEPT String + r) FilePath
resolveOut opts = case opts.out of
  Just o -> pure o
  Nothing
    | opts.prepareRelease -> fromMaybe "dist/ulib" <$> Env.lookupEnv "PURVASM_LIB"
    | otherwise -> throw "ulib-tools build: one of --out or --prepare-release is required."

-- | Copy one patched module's compiled corefn into the flat output layout.
extractCorefn :: forall r. FilePath -> FilePath -> Patch -> Run (FS + LOG + EXCEPT String + r) Unit
extractCorefn outputDir out { modName } = do
  from <- FS.joinPath [ outputDir, modName, "corefn.json" ]
  destDir <- FS.joinPath [ out, modName ]
  content <- FS.readText from >>= maybe
    (throw $ Fmt.fmt @"ulib: missing compiled corefn for {modName}" { modName })
    pure
  FS.mkdirP destDir
  destPath <- FS.joinPath [ destDir, "corefn.json" ]
  FS.writeText destPath content
  Log.info $ Fmt.fmt @"ulib: {modName}" { modName }
