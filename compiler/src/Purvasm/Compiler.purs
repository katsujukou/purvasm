module Purvasm.Compiler where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Graph as G
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Purvasm.Compiler.Effects.FS (FS, expandGlob)
import Purvasm.Compiler.Effects.FS as FS
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.Par (PAR)
import Purvasm.Compiler.Effects.Par as Par
import Purvasm.Compiler.Env (CompileEnv)
import Purvasm.Compiler.Metrics (Metrics)
import Purvasm.Compiler.PureScript (buildModuleGraph, sourceModuleName)
import Purvasm.Compiler.Types (LogVerbosity)
import Purvasm.DependencyGraph (ModuleGraph)
import Purvasm.Types (ModuleName)
import Run (AFF, Run, EFFECT)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

data CompInput
  = InpFilePath FilePath
  | InpModule ModuleName

derive instance Generic CompInput _
instance Show CompInput where
  show = genericShow

-- | Descriptor of each compilation step.
data CompileStep
  =
    -- | List all modules to compile.
    Initial CompInput
  -- | From the list of input modules, resolve all transitive dependencies and make build plan.
  | ModulesResolved ModuleGraph
  | EnvSetup CompileEnv
  | Finish Metrics

derive instance Generic CompileStep _
instance Show CompileStep where
  show = genericShow

-- type CompileSyncEffects r =
--   ( EXCEPT String
--       + FS
--       + LOG
--       + AFF
--       + EFFECT
--       + r
--   )

-- type CompileEffects r = (CompileSyncEffects + PAR (CompileSyncEffects r) + r)

-- | The compilation pass.
nextStep
  :: forall r
   . CompileStep
  -> Run (EXCEPT String + FS + LOG + (PAR (AFF + EFFECT + FS + ())) + AFF + EFFECT + r) (Step CompileStep Unit)
nextStep = case _ of
  -- From `CompInput`, resolve the path to input modules according to following scheme:
  --  - If `CompInput` is given with `InpFilePath`, all `*.purs` source files
  --    visible from directory in the specified path will be targeted.
  --  - Otherwise, the single module specified by the input is targeted.
  -- After all target modules are determined, the names and real paths of those modules
  -- will be passed to the next step.
  Initial inp ->
    do
      Log.debug "=============================="
      Log.debug "Entered step: Initial"
      modules <- case inp of
        InpModule modname -> do
          pure [ modname ]
        InpFilePath path -> do
          Log.info "Searching for modules to compile..."
          absPath <- Run.liftEffect
            if Path.isAbsolute path then pure path
            else Process.cwd <#> (_ `Array.cons` [ path ]) >>> Path.concat
          files <- expandGlob absPath "**/*.purs"
          Log.debug ("Target files are: \n" <> (Str.joinWith "\n" $ (" - " <> _) <$> files))
          -- For each purs source file, parse module header and read module name in parallel. 
          Par.all $ sourceModuleName <$> files
      Log.info "Resolving dependencies..."
      graph /\ size <- buildModuleGraph modules
      Log.info $ "Resolved. We have " <> show size <> " modules to build."
      Log.debug $ show (G.topsort graph)
      pure $ Loop $ ModulesResolved graph
  -- 
  ModulesResolved modules -> do
    Log.debug "=============================="
    Log.debug "Entered step: ModulesResolved"
    Log.debug "Make build env..."

    pure (Loop $ EnvSetup {})

  EnvSetup env -> do
    Log.debug "EnvSetup"
    Log.info $ show env
    pure (Loop $ Finish {})

  Finish {} -> do
    Log.debug "Finish"
    pure (Done unit)

type Options =
  { dist :: FilePath
  , verbosity :: LogVerbosity
  }

compile :: Options -> CompInput -> Aff Unit
compile opts inp = do
  let
    run = tailRecM nextStep (Initial inp)
    interpretPar r = r
      # Par.interpretAff
          ( FS.interpret FS.handleNode
              >>> Run.runBaseAff'
          )

  result <- run
    # Log.interpret (Log.handleTerminal opts.verbosity)
    # FS.interpret FS.handleNode
    # Except.runExcept
    # interpretPar
    # Run.runBaseAff'

  case result of
    Right _ -> pure unit
    Left err -> do
      Console.log $ "\x1b[31m[ERROR]\x1b[0m " <> err

