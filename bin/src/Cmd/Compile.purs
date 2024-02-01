module Cmd.Compile where

import Prelude

import Data.Array (fold)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Node.Path (FilePath)
import Options.Applicative as Opts
import Purvasm.Types (ModuleName(..))

type Options =
  { target :: ModuleName
  }

options :: Opts.Parser Options
options = ado
  target <- ModuleName <$> Opts.strArgument
    ( fold
        [ Opts.help "The name of module to compile"
        , Opts.metavar "TARGET"
        ]
    )
  in { target }

cmd :: FilePath -> Options -> Aff Unit
cmd dist opts = do
  Console.log "compile"
  Console.logShow dist
  Console.logShow opts
