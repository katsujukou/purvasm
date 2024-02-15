module Purvasm.Compiler.Compile where

import Prelude

import Data.Traversable (for)
import PureScript.CoreFn as CF
import Purvasm.Backend.PmoFile (PmoFile(..))
import Purvasm.Compiler.Effects.FS (FS)
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.WEnv (WENV)
import Purvasm.Compiler.Effects.WEnv as WEnv
import Purvasm.Compiler.PureScript (openExternsCbor)
import Purvasm.ECore.Syntax (Ann(..), Expr, Module(..)) as ECore
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.MiddleEnd (translateModuleName, translateIdent)
import Purvasm.MiddleEnd as ME
import Purvasm.Types (ModuleName)
import Run (Run)
import Run.Except (EXCEPT)
import Safe.Coerce (coerce)
import Spago.Generated.BuildInfo (pursVersion)
import Type.Row (type (+))

type UnitaryCompileResult =
  { name :: ModuleName
  , desc :: UnitaryCompileResultDesc
  }

data UnitaryCompileResultDesc
  = UCompSkipped
  | UCompSucceeded PmoFile
  | UCompFailed String -- TODO: better error descriptor

data UnitaryCompileStep
  = Initial (CF.Module CF.Ann)
  | Translated (ECore.Module ECore.Ann)
  | Transformed ModuleName
  | Optimized ModuleName
  | Linearized ModuleName
  | Assembled PmoFile

type CompileEffects r = (LOG + FS + WENV GlobalEnv + EXCEPT String + r)

nextStep
  :: forall r
   . UnitaryCompileStep
  -> Run (CompileEffects r) UnitaryCompileStep
nextStep = case _ of
  Initial cfm@(CF.Module cfModule) -> do
    -- Apply externs declarations to global env. 
    genv <- do
      WEnv.update (Global.applyCorefnEnv cfm)
    -- translate CoreFn module and pass it in to next stage.
    Log.debug $ show genv
    pure $ Translated $ ME.translateCoreFn genv cfm

  Translated (ECore.Module ecModule) -> do
    Log.debug $ show ecModule
    pure $ Transformed ecModule.name

  Transformed mname -> pure $ Optimized mname
  Optimized mname -> pure $ Linearized mname
  Linearized mname -> pure (Assembled (emptyPmoFile mname))
  Assembled pmoFile -> pure $ Assembled pmoFile

compileModule
  :: forall r
   . CF.Module CF.Ann
  -> Run (CompileEffects r) UnitaryCompileResult
compileModule cfModule@(CF.Module { name }) = loop (Initial cfModule)
  where
  name' = translateModuleName name

  loop step = do
    nextStep step >>= case _ of
      Assembled pmoFile -> do
        pure $ { name: name', desc: UCompSucceeded pmoFile }
      step' -> loop step'

emptyPmoFile :: ModuleName -> PmoFile
emptyPmoFile name =
  PmoFile
    { head:
        { name
        , pursVersion
        , version: "0.1.0"
        }
    , symbols: []
    , datasec: []
    , textsec: []
    , refsec: []
    }
