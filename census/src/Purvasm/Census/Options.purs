-- | The `census` command line. One command per measurement the tool can re-run.
module Purvasm.Census.Options
  ( Command(..)
  , command
  , dispatch
  , parse
  ) where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Filesystem (FS)
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.Census.ByNeed.Cmd as ByNeed
import Run (EFFECT, Run)
import Run.Except (EXCEPT)
import Type.Row (type (+))

data Command = ByNeed ByNeed.Options

command :: ArgParser Command
command =
  ArgParser.choose "COMMAND"
    [ ArgParser.command [ "byneed" ]
        "Census the ADR-0107 by-need demand sites of a program's compilation closure."
        ((ByNeed <$> ByNeed.options) <* ArgParser.flagHelp)
    ]
    <* ArgParser.flagHelp

dispatch :: forall r. Command -> Run (ENV + LOG + FS + PROC + EXCEPT String + EFFECT + r) Unit
dispatch = case _ of
  ByNeed opts -> ByNeed.cmd opts

parse :: Array String -> Either ArgParser.ArgError Command
parse =
  ArgParser.parseArgs "census"
    "Re-runnable compiler measurements (ADR-0107 and successors)."
    command
