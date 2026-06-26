module Purvasm.CLI.Options where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.Version as Version

data Command
  = Compile Compile.Options
  | Build Build.Options

command :: ArgParser.ArgParser Command
command =
  ArgParser.choose "COMMAND"
    [ ArgParser.command [ "build" ]
        "Build a whole program and emit a single executable"
        ((Build <$> Build.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "compile" ]
        "Compile a single module and emit pmi/pmo files"
        ((Compile <$> Compile.options) <* ArgParser.flagHelp)
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show version" Version.versionString

parse :: Array String -> Either ArgParser.ArgError Command
parse =
  ArgParser.parseArgs "purvasm"
    "A bytecode compiler and interpreter for PureScript"
    command