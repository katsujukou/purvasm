module Purvasm.CLI.Option where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Purvasm.CLI.Version as Version

type FilePath = String 

type CompileOptions =
  { corefnDir :: FilePath 
  , outDir :: FilePath
  , entryModule :: String
  }

compileOptionsParser :: ArgParser.ArgParser CompileOptions 
compileOptionsParser = ArgParser.fromRecord
  { corefnDir: 
    ArgParser.argument [ "--corefn-dir" ]
      "Path to PureScript compiler's output directory (the corefn.jsons are placed in)\n\
      \Defaults to './output'."
      # ArgParser.default "output"
  , outDir: 
      ArgParser.argument [ "-o", "--outdir" ]
      "Path to the directory the bundled executable is placed in\n\
      \Defaults to '/output-purvm"
      # ArgParser.default "output-purvm"
  , entryModule:
      ArgParser.argument ["--entry"]
      "The name of an entry module (which should contain `main` function)\n\
      \Defaults to `Main`"
      # ArgParser.default "Main" 
  }

data Command 
  = Compile CompileOptions

commandParser :: ArgParser Command
commandParser = ArgParser.choose "COMMAND"
  [ ArgParser.command [ "compile" ]
      "Compile a single module to its .pmi/.pmo images"
      (Compile <$> compileOptionsParser <* ArgParser.flagHelp)
  ]
  <* ArgParser.flagHelp
  <* ArgParser.flagInfo [ "-v", "--version" ] "Show version" Version.versionString

parse :: Array String -> Either ArgParser.ArgError Command
parse =
  ArgParser.parseArgs "purvmc"
    "Purvasm bytecode compiler"
    commandParser
