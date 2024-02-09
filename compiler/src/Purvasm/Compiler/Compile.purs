module Purvasm.Compiler.Compile where

import Prelude

import PureScript.CoreFn as CF
import Purvasm.Backend.PmoFile (PmoFile(..))
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Global (GlobalEnv)
import Purvasm.MiddleEnd.ECF.Translate (translModuleName)
import Purvasm.Types (ModuleName)
import Run (AFF, Run)
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
  | Translated ModuleName
  | Transformed ModuleName
  | Optimized ModuleName
  | Linearized ModuleName
  | Assembled PmoFile

nextStep :: forall r. UnitaryCompileStep -> Run r UnitaryCompileStep
nextStep = case _ of
  Initial (CF.Module cfModule) -> do
    pure $ Translated (coerce cfModule.name)
  Translated mname -> pure $ Transformed mname
  Transformed mname -> pure $ Optimized mname
  Optimized mname -> pure $ Linearized mname
  Linearized mname -> pure (Assembled (emptyPmoFile mname))
  Assembled pmoFile -> pure $ Assembled pmoFile

compileModule
  :: forall r' r
   . ((GlobalEnv -> GlobalEnv) -> Run (AFF + r') Unit)
  -> CF.Module CF.Ann
  -> Run (LOG + r) UnitaryCompileResult
compileModule (extendGlobal) cfModule@(CF.Module { name }) = do
  -- Log.info $
  --   fmt @"{idx} Compiling {modname}"
  --     { idx: prettyPrintIndex idx
  --     , modname: "\x1b[1m" <> coerce name <> "\x1b[0m"
  --     }

  loop (Initial cfModule)
  where
  name' = translModuleName name
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
