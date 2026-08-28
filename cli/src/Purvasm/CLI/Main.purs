module Purvasm.CLI.Main where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Fmt as Fmt
import Node.Process as Process
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Run as Run
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.ForeignSigsCmd as ForeignSigsCmd
import Purvasm.CLI.Node as Node
import Purvasm.CLI.Options as Options

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err) *> Process.exit' 1
    Right cmd -> runNode case cmd of
      -- Every command answers with the status the process should end with. Only `run` has anything
      -- but 0 to say — it hands back what the program it launched reported — and routing that through
      -- the same place the other exits already happen keeps one exit path rather than two.
      Options.Compile opts -> 0 <$ Compile.cmd opts
      Options.Build opts -> 0 <$ Build.cmd opts
      Options.Run opts -> Run.cmd opts
      Options.ForeignSigs opts -> 0 <$ ForeignSigsCmd.cmd opts

  where
  runNode program = do
    res <- Node.runNode program
    case res of
      Right 0 -> pure unit
      Right code -> Process.exit' code
      Left err -> Console.error (Fmt.fmt @"purvasm: {err}" { err }) *> Process.exit' 1