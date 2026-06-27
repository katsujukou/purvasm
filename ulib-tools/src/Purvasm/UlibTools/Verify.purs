-- | `ulib-tools verify`: build-time patch-faithfulness (ADR-0043 §5) — diff the patched module's
-- | public surface against the registry module's via `purs docs --format json`. Not yet
-- | implemented; scaffolded so the CLI surface is complete.
module Purvasm.UlibTools.Verify
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
cmd _ = Log.warn "ulib-tools verify: not yet implemented (ADR-0043 §5)."
