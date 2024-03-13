module Purvasm.Compiler.Compile where

import Prelude

import PureScript.CoreFn as CF
import Purvasm.Backend as Backend
import Purvasm.Backend.PmoFile (PmoFile(..))
import Purvasm.Compiler.Effects.FS (FS)
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.WEnv (WENV)
import Purvasm.Compiler.Effects.WEnv as WEnv
import Purvasm.Compiler.PureScript (openExternsCbor)
import Purvasm.ECore.Syntax (Ann, Module) as ECF
import Purvasm.ECore.Translate (translateCoreFn)
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.LCore.Syntax (Module(..)) as LCF
import Purvasm.LCore.Translate (lowerModule) as LCF
import Purvasm.NCore.Syntax (Module(..)) as NCF
import Purvasm.NCore.Translate (translateModule) as NCF
import Purvasm.Types (ModuleName)
import Run (Run)
import Run.Except (EXCEPT)
import Safe.Coerce (coerce)
import Spago.Generated.BuildInfo (pursVersion)
import Spago.Generated.BuildInfo as Spago
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
  | Translated (ECF.Module ECF.Ann)
  | Transformed NCF.Module
  | Optimized NCF.Module
  | Lowered LCF.Module
  | Assembled PmoFile

type CompileEffects r = (LOG + FS + WENV GlobalEnv + EXCEPT String + r)

nextStep
  :: forall r
   . UnitaryCompileStep
  -> Run (CompileEffects r) UnitaryCompileStep
nextStep = case _ of
  Initial cfm@(CF.Module { name }) -> do
    -- Apply externs declarations to global env. 
    exts <- openExternsCbor "output" (coerce name)
    _ <- WEnv.update (Global.externsEnv exts)
    -- Apply corefn to global env
    genv <- WEnv.update (Global.applyCorefnEnv cfm)
    -- translate CoreFn module and pass it in to next stage.
    Log.debug $ show genv
    pure $ Translated $ translateCoreFn genv cfm

  Translated ecModule -> do
    Log.debug $ show ecModule
    genv <- WEnv.read
    let
      ncfModule = NCF.translateModule genv ecModule
    Log.info (show ncfModule)
    pure $ Transformed $ ncfModule

  Transformed ncfModule -> pure $ Optimized ncfModule

  Optimized ncfModule -> do
    genv <- WEnv.read
    let
      lcModule = LCF.lowerModule genv ncfModule
    Log.info $ show lcModule
    pure $ Lowered lcModule

  Lowered lcfModule@(LCF.Module { name }) -> do
    let
      header =
        { name
        , pursVersion: Spago.pursVersion
        , version: "0.1.0"
        }
      pmoFile = Backend.compileModule header lcfModule
    pure $ (Assembled pmoFile)
  Assembled pmoFile -> pure $ Assembled pmoFile

compileModule
  :: forall r
   . CF.Module CF.Ann
  -> Run (CompileEffects r) UnitaryCompileResult
compileModule cfModule@(CF.Module { name }) = loop (Initial cfModule)
  where
  name' = coerce name

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
