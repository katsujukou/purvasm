module Purvasm.Compiler.Compile where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Tuple (snd)
import Effect.Console (log, logShow)
import Effect.Unsafe (unsafePerformEffect)
import PureScript.CoreFn as CF
import PureScript.ExternsFile (ExternsFile(..))
import Purvasm.Backend.PmoFile (PmoFile(..))
import Purvasm.Compiler.Effects.FS (FS)
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.WEnv (WENV)
import Purvasm.Compiler.Effects.WEnv as WEnv
import Purvasm.Compiler.PureScript (openExternsCbor)
import Purvasm.ECore.Syntax (Ann, Module(..), Binding(..)) as ECore
import Purvasm.ECore.Transform (transformModule) as ECore
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.LCore.Env (LocalSymbolTable(..))
import Purvasm.LCore.Translate as LCore
import Purvasm.MiddleEnd (translateModuleName)
import Purvasm.MiddleEnd as ME
import Purvasm.Types (ModuleName)
import Run (Run, EFFECT)
import Run as Run
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
  | Transformed (ECore.Module ECore.Ann)
  | Optimized ModuleName
  | Linearized ModuleName
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
    Log.debug $ show genv
    -- translate CoreFn module and pass it in to next stage.
    pure $ Translated $ ME.translateCoreFn genv cfm

  Translated ecModule -> do
    Log.debug $ show ecModule
    pure $ Transformed $
      ECore.transformModule ecModule

  Transformed (ECore.Module ecModule) -> do
    for_ ecModule.decls \(ECore.Binding id exp) -> do
      genv <- WEnv.read
      let
        _ = unsafePerformEffect do
          log $ "[" <> show id <> "]"
          logShow
            $ snd
            $ LCore.runTransl
                { moduleName: ecModule.name
                , locals: Tnull
                , globals: genv
                , static: []
                , fresh: 0
                , isToplevel: true
                }
            $
              LCore.translateExpr id exp
      pure unit
    pure $ Optimized ecModule.name
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
