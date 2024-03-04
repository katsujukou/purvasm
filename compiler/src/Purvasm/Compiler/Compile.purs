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
import Purvasm.ECore.Syntax (Ann, Module) as ECore
import Purvasm.ECore.Transform (transformModule) as ECore
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.LCore.Syntax (Module(..)) as LCore
import Purvasm.MiddleEnd (translateModuleName)
import Purvasm.MiddleEnd as ME
import Purvasm.NCore.Syntax (Program(..)) as NCore
import Purvasm.NCore.Translate (translateProgram) as NCore
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
  | Translated (ECore.Module ECore.Ann)
  | Transformed (ECore.Module ECore.Ann)
  | Optimized NCore.Module
  | Lowered LCore.Module
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
    pure $ Translated $ ME.translateCoreFn genv cfm

  Translated ecModule -> do
    Log.debug $ show ecModule
    pure $ Transformed $
      ECore.transformModule ecModule

  Transformed ecModule -> do
    genv <- WEnv.read
    let
      lcProgram = NCore.translateProgram genv ecModule
    Log.info (show lcProgram)
    pure $ Lowered lcProgram
  Lowered lcProgram -> pure $ Optimized lcProgram
  Optimized lcProgram@(NCore.Program { name }) -> do
    let
      header =
        { name
        , pursVersion: Spago.pursVersion
        , version: "0.1.0"
        }
      pmoFile = Backend.compileProgram header lcProgram
    pure $ (Assembled pmoFile)
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
