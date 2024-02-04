module Cmd.Compile where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Node.Path (FilePath)
import Options.Applicative (strArgument, switch)
import Options.Applicative as Opts
import Purvasm.Compiler (CompInput(..), compile)
import Purvasm.Compiler.Types (LogVerbosity)
import Purvasm.Types (parseModuleName)

type Options =
  { m :: Boolean
  , target :: String
  }

options :: Opts.Parser Options
options = ado
  m <- switch $ fold
    [ Opts.short 'm'
    , Opts.help "Tell to regard argument value as name of module to compile."
    ]
  target <- strArgument $ fold
    [ Opts.metavar "TARGET"
    , Opts.help
        "The name of target to compile. \
        \ If -m flag is specified, the value of this argument is \
        \ regarded as name of module name. Otherwise this should be \
        \ path to the directory containing source files of target modules."
    ]
  in { m, target }

cmd :: FilePath -> LogVerbosity -> Options -> Aff Unit
cmd dist verbosity = mkInput >=> compile { dist, verbosity }
  where
  mkInput :: Options -> Aff CompInput
  mkInput { m, target }
    | m = case parseModuleName target of
        Just moduleName -> pure (InpModule moduleName)
        _ -> throwError (error "Error: Invalid module name.")
    | otherwise = pure (InpFilePath target)