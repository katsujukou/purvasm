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
  , install :: Boolean
  , ulibDir :: FilePath
  , baseDir :: FilePath
  , purs :: String
  , spagoPkgs :: FilePath
  }

options :: ArgParser Options
options = fromRecord
  { out:
      ArgParser.argument [ "--out" ]
        "Output dir for the extracted corefn (<dir>/<Module>/corefn.json)."
        # ArgParser.optional
  , install:
      ArgParser.flag [ "--install" ]
        "Write to the default install location ($PURVASM_ULIB, else 'dist/ulib')."
        # ArgParser.boolean
  , ulibDir:
      ArgParser.argument [ "--ulib" ]
        "ulib patch source dir. Defaults to 'ulib'."
        # ArgParser.default "ulib"
  , baseDir:
      ArgParser.argument [ "--base" ]
        "purvasm-base src dir. Defaults to 'packages/purvasm-base/src'."
        # ArgParser.default "packages/purvasm-base/src"
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
  work <- Stage.mkTemp
  srcDir <- FS.joinPath [ work, "src" ]
  outputDir <- FS.joinPath [ work, "output" ]
  FS.mkdirP srcDir

  Stage.stageRegistry opts.spagoPkgs opts.baseDir srcDir
  patches <- Stage.collectPatches opts.ulibDir
  Stage.overlayPatches opts.ulibDir srcDir patches
  Stage.compileCorefn opts.purs srcDir outputDir

  FS.mkdirP out
  for_ patches (extractCorefn outputDir out)

  _ <- Proc.exec "rm" [ "-rf", work ]
  Log.info $ Fmt.fmt @"ulib: wrote patched corefn to {out}" { out }

-- | `--out` wins; otherwise `--install` resolves the default location ($PURVASM_ULIB, else
-- | 'dist/ulib'); with neither, fail loudly.
resolveOut :: forall r. Options -> Run (ENV + EXCEPT String + r) FilePath
resolveOut opts = case opts.out of
  Just o -> pure o
  Nothing
    | opts.install -> fromMaybe "dist/ulib" <$> Env.lookupEnv "PURVASM_ULIB"
    | otherwise -> throw "ulib-tools build: one of --out or --install is required."

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
