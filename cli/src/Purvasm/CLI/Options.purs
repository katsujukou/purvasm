module Purvasm.CLI.Options where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Purvasm.CLI.Build as Build
import Purvasm.CLI.Compile as Compile
import Purvasm.CLI.ForeignSigsCmd as ForeignSigsCmd
import Purvasm.CLI.Run as Run
import Purvasm.CLI.Version as Version

data Command
  = Compile Compile.Options
  | Run Run.Options
  | Build Build.Options
  | ForeignSigs ForeignSigsCmd.Options

command :: ArgParser.ArgParser Command
command =
  ArgParser.choose "COMMAND"
    [ ArgParser.command [ "build" ]
        "Build a whole program and emit a single executable"
        ((Build <$> Build.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "compile" ]
        "Compile a single module and emit pmi/pmo files"
        ((Compile <$> Compile.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "run" ]
        "Build and execute program with purvasm bytecode compiler/interpreter"
        ((Run <$> Run.options) <* ArgParser.flagHelp)
    , ArgParser.command [ "foreign-sigs" ]
        "Dump the closure's reconstructed foreign signatures as JSON (ADR-0080). \
        \Transitional: the Level-2 half of the boot-vs-Lv2 consistency differential, \
        \removed once the native-codegen port retires boot."
        ((ForeignSigs <$> ForeignSigsCmd.options) <* ArgParser.flagHelp)
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show version" Version.versionString

parse :: Array String -> Either ArgParser.ArgError Command
parse =
  ArgParser.parseArgs "purvasm"
    "A bytecode and native code toolchain for PureScript"
    command