-- | `ulib-tools test`: run each ulib module's behaviour test at the fidelity its manifest declares
-- | (ADR-0043 §4–§5): JS build for representation-equivalent modules, native (`purvm`) for the
-- | divergent seam, honouring per-module xfail. Not yet implemented; scaffolded so the CLI surface
-- | is complete.
module Purvasm.UlibTools.Test
  ( Options
  , options
  , cmd
  ) where

import Prelude

import ArgParse.Basic (ArgParser, fromRecord)
import ArgParse.Basic as ArgParser
import Data.Maybe (Maybe)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS)
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Run (EFFECT, Run)
import Run.Except (EXCEPT)
import Type.Row (type (+))

type Options = { package :: Maybe String }

options :: ArgParser Options
options = fromRecord
  { package:
      ArgParser.argument [ "--package" ]
        "Restrict to a single ulib package (default: all)."
        # ArgParser.optional
  }

cmd :: forall r. Options -> Run (PROC + ENV + FS + LOG + EXCEPT String + EFFECT + r) Unit
cmd _ = Log.warn "ulib-tools test: not yet implemented (ADR-0043 §4)."
