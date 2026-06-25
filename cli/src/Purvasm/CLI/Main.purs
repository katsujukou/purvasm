module Purvasm.CLI.Main where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Node.Process as Process
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.Option (Command(..), parse)

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err)
    Right cmd -> case cmd of
      Compile opts -> Compile.cmd opts