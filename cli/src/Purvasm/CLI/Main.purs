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
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.Node as Node
import Purvasm.CLI.Options as Options

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err) *> Process.exit' 1
    Right cmd -> runNode case cmd of
      Options.Compile opts -> Compile.cmd opts
      Options.Build opts -> Build.cmd opts

  where
  runNode program = do
    res <- Node.runNode program
    case res of
      Right a -> pure a
      Left err -> Console.error (Fmt.fmt @"purvasm: {err}" { err }) *> Process.exit' 1