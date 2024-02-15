module Purvasm.Compiler.BatchCompile
  ( BatchCompileEffects
  , BuildPlan
  , batchCompile
  , initBuildPlan
  ) where

import Prelude

import Data.Argonaut (printJsonDecodeError)
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lazy as L
import Data.Maybe (Maybe(..))
import Fmt (fmt)
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json (PartialModule(..), fullModule)
import PureScript.CoreFn.Json as CFJ
import Purvasm.Compiler.Compile (UnitaryCompileResult, UnitaryCompileResultDesc(..), CompileEffects, compileModule)
import Purvasm.Compiler.Effects.FS (FS)
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.Par (PAR)
import Purvasm.Compiler.Effects.Par as Par
import Purvasm.Compiler.ModuleImportMap (ModuleImportMap, filter, modules)
import Purvasm.Compiler.Types (prettyPrintIndex)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Safe.Coerce (coerce)
import Type.Row (type (+))

type BuildStage =
  { jobs :: Array (CFJ.PartialModule CF.Ann)
  }

newtype BuildPlan = BuildPlan
  { currentStage :: BuildStage
  , nextStage :: L.Lazy (BuildPlan)
  }

mkBuildPlan :: ModuleImportMap -> BuildPlan
mkBuildPlan importMap = do
  let
    targetModules = modules $ filter nullImports importMap
    currentStage = { jobs: targetModules }
    nextMap = foldl
      (\prevMap (CFJ.PartialModule { name }) -> (deleteImportModule name) <$> prevMap)
      (filter (not <<< nullImports) importMap)
      targetModules
  BuildPlan
    { currentStage
    , nextStage: L.defer \_ -> mkBuildPlan nextMap
    }
  where
  nullImports :: CFJ.PartialModule CF.Ann -> Boolean
  nullImports (CFJ.PartialModule { imports }) = Array.null imports

  deleteImportModule :: CF.ModuleName -> CFJ.PartialModule CF.Ann -> CFJ.PartialModule CF.Ann
  deleteImportModule module' (CFJ.PartialModule pm) = CFJ.PartialModule (pm { imports = Array.filter (_ `isn't` module') pm.imports })

  isn't :: CF.Import CF.Ann -> CF.ModuleName -> Boolean
  isn't (CF.Import _ name) modname = name /= coerce modname

type BatchCompileEffects r = LOG + FS + EXCEPT String + PAR (CompileEffects + AFF + EFFECT + ()) + EFFECT + AFF + r

initBuildPlan :: forall r. ModuleImportMap -> Run (AFF + r) BuildPlan
initBuildPlan importMap = pure $ mkBuildPlan importMap

batchCompile :: forall r. BuildPlan -> Run (BatchCompileEffects r) (Array UnitaryCompileResult)
batchCompile = loop 1
  where
  loop stg bp = do
    propagate stg bp >>= case _ of
      Left errs -> pure errs
      Right nextPlan
        | BuildPlan { currentStage } <- nextPlan
        , Array.length (currentStage.jobs) == 0 -> pure []
        | otherwise -> loop (stg + 1) nextPlan

  propagate stage (BuildPlan plan) = do
    let
      idxTpl =
        { stage
        , current: 1
        , total: Array.length plan.currentStage.jobs
        }
    -- Launch parallel compilation within current stage.
    results <- Par.all
      ( plan.currentStage.jobs <#*> \idx pm@(PartialModule { name }) -> do
          case fullModule pm of
            Left err -> do
              pure
                { name: coerce name
                , desc: UCompFailed $ printJsonDecodeError err
                }
            Right module_ -> do
              Log.info $
                fmt @"{idx} Compiling {modname}"
                  { idx: prettyPrintIndex (idxTpl { current = idx + 1 })
                  , modname: "\x1b[1;33m" <> coerce name <> "\x1b[0m"
                  }

              compileModule module_
      )

    case partitionResults results of
      { failed }
        | Array.length failed > 0 -> pure $ Left failed
        | otherwise -> do
            pure $ Right (L.force plan.nextStage)

  partitionResults = go [] [] []
    where
    go succeeded skipped failed = Array.uncons >>> case _ of
      Just { head: r@{ desc }, tail } -> case desc of
        UCompSucceeded _ -> go (Array.cons r succeeded) skipped failed tail
        UCompSkipped -> go succeeded (Array.cons r skipped) failed tail
        UCompFailed _ -> go succeeded skipped (Array.cons r failed) tail
      _ -> { succeeded, skipped, failed }

mapWithIndexFlipped :: forall a b. Array a -> (Int -> a -> b) -> Array b
mapWithIndexFlipped xs f = Array.mapWithIndex f xs

infixl 1 mapWithIndexFlipped as <#*>