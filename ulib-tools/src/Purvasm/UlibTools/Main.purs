module Purvasm.UlibTools.Main where

import Prelude

import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Fmt as Fmt
import Node.Process as Process
import Purvasm.CLI.Effect.Env (ENV)
import Purvasm.CLI.Effect.Env as Env
import Purvasm.CLI.Effect.Filesystem (FS)
import Purvasm.CLI.Effect.Filesystem as FS
import Purvasm.CLI.Effect.Log (LOG)
import Purvasm.CLI.Effect.Log as Log
import Purvasm.CLI.Effect.Process (PROC)
import Purvasm.CLI.Effect.Process as Proc
import Purvasm.CLI.Node (defaultLoggerConfig, nodeEnvHandler, nodeFsHandler, nodeProcHandler)
import Purvasm.UlibTools.Build as Build
import Purvasm.UlibTools.Options as Options
import Purvasm.UlibTools.Test as Test
import Purvasm.UlibTools.Verify as Verify
import Purvasm.UlibTools.VerifyDeps as VerifyDeps
import Run (EFFECT, Run, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err)
    Right cmd -> run case cmd of
      Options.Build opts -> Build.cmd opts
      Options.Verify opts -> Verify.cmd opts
      Options.VerifyDeps opts -> VerifyDeps.cmd opts
      Options.Test opts -> Test.cmd opts
  where
  run program = do
    res <- runNode program
    case res of
      Right _ -> pure unit
      Left err -> Console.error $ Fmt.fmt @"ulib-tools: {err}" { err }

-- | Discharge the full effect row against the synchronous Node backend (cli-lib handlers).
runNode
  :: forall a
   . Run (PROC + ENV + FS + LOG + EFFECT + EXCEPT String + ()) a
  -> Effect (Either String a)
runNode m = m
  # Env.interpret nodeEnvHandler
  # FS.interpret nodeFsHandler
  # Proc.interpret nodeProcHandler
  # Log.interpret (Log.terminalHandler defaultLoggerConfig)
  # Except.runExcept
  # runBaseEffect
