-- | `ulib-tools verify-deps`: the dependency-validation pass (ADR-0047 §3) on its own — every
-- | declared dependency resolves (in-repo or registry) and the authored dependency graph is acyclic.
-- | `build` and `verify` run the same pass before staging; this exposes it standalone for CI.
module Purvasm.UlibTools.VerifyDeps
  ( Options
  , options
  , cmd
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Fmt as Fmt
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS, FilePath)
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.UlibTools.Stage as Stage
import Run (EFFECT, Run)
import Run.Except (EXCEPT, throw)
import Type.Row (type (+))

type Options =
  { ulibDir :: FilePath
  , packagesDir :: FilePath
  , spagoPkgs :: FilePath
  }

options :: ArgParser Options
options = fromRecord
  { ulibDir:
      ArgParser.argument [ "--ulib" ]
        "ulib patch source dir. Defaults to 'ulib'."
        # ArgParser.default "ulib"
  , packagesDir:
      ArgParser.argument [ "--packages-dir" ]
        "In-repo packages dir for resolving declared deps. Defaults to 'packages'."
        # ArgParser.default "packages"
  , spagoPkgs:
      ArgParser.argument [ "--spago-pkgs" ]
        "Resolved package-set sources dir. Defaults to '.spago/p'."
        # ArgParser.default ".spago/p"
  }

cmd :: forall r. Options -> Run (PROC + ENV + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd opts = do
  patches <- Stage.collectPatches opts.ulibDir
  when (Array.null patches) do
    throw "ulib-tools verify-deps: no patches found (check --ulib)."
  Stage.validate { ulibDir: opts.ulibDir, packagesDir: opts.packagesDir, spagoPkgs: opts.spagoPkgs } patches
  Log.info $ Fmt.fmt @"verify-deps: {n} patched module(s); dependencies resolve and the graph is acyclic."
    { n: show (Array.length patches) }
