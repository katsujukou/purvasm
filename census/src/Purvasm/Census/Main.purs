-- | The `census` tool's entry point (ADR-0107 measurement infrastructure). A tool package, not part
-- | of the compiler or the `purvasm` CLI: it exists so a recorded census can be RE-RUN — the numbers
-- | an ADR closes on should be reproducible, not archaeological.
module Purvasm.Census.Main where

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
import Purvasm.Census.Options as Options
import Run (EFFECT, Run, runBaseEffect)
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case Options.parse cliArgs of
    Left err -> Console.error (ArgParser.printArgError err) *> Process.exit' 1
    Right cmd -> run (Options.dispatch cmd)
  where
  run program = do
    res <- runNode program
    case res of
      Right _ -> pure unit
      -- A census failure must exit non-zero: the reconciliation gate reads the exit code.
      Left err -> Console.error (Fmt.fmt @"census: {err}" { err }) *> Process.exit' 1

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
