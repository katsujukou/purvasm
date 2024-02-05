module Purvasm.Compiler.PureScript where

import Prelude

import Control.Monad.Rec.Class (tailRecM)
import Data.Argonaut (parseJson, printJsonDecodeError)
import Data.Array (elem, fold, foldl, foldr)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Graph (topsort)
import Data.Graph as G
import Data.HashGraph as HG
import Data.Set (Set)
import Data.Set as S
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Exception as Exn
import Node.Path (FilePath)
import Node.Path as Path
import PureScript.CST (PartialModule(..), RecoveredParserResult(..), parsePartialModule)
import PureScript.CST.Types (ModuleHeader(..))
import PureScript.CST.Types as CST
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json as CFJ
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (describeError, runDecoder)
import Purvasm.Compiler.Effects.FS (FS, readCborFile, readTextFile)
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.Par (PAR)
import Purvasm.Compiler.Effects.Par as Par
import Purvasm.DependencyGraph (ModuleGraph)
import Purvasm.Global (GlobalEnv)
import Purvasm.Global as Global
import Purvasm.Types (ModuleName(..))
import Run (EFFECT, Run, Step(..), AFF)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Safe.Coerce (coerce)
import Type.Row (type (+))

data ModuleArtifact = CorefnJson | ExternsCbor

artifactFileName :: ModuleArtifact -> String
artifactFileName = case _ of
  CorefnJson -> "corefn.json"
  ExternsCbor -> "externs.cbor"

moduleArtifactPath :: FilePath -> ModuleName -> ModuleArtifact -> FilePath
moduleArtifactPath outdir (ModuleName modname) art = Path.concat [ outdir, modname, artifactFileName art ]

-- | Read module name from source file by parsing module header using `purescript-language-cst-parser`.
sourceModuleName :: forall r. FilePath -> Run (EFFECT + FS + r) ModuleName
sourceModuleName file = do
  source <- readTextFile file
  case parsePartialModule source of
    ParseSucceeded (PartialModule { header }) ->
      let
        ModuleHeader
          { name: CST.Name
              { name: CST.ModuleName modname }
          } = header
      in
        pure (ModuleName modname)
    _ -> Run.liftEffect $ Exn.throw ("Failed to read module name from source file in " <> file <> ".")

buildModuleGraph :: forall r r'. Array ModuleName -> Run (LOG + PAR (EFFECT + AFF + FS + r) + FS + AFF + EFFECT + r') (ModuleGraph /\ Int)
buildModuleGraph rootModules = do
  avar <- Run.liftAff (AVar.new { graph: HG.empty, done: S.empty })
  cnt <- tailRecM go (avar /\ rootModules)
  graph <- Run.liftAff (AVar.take avar <#> _.graph)
  pure (graph /\ cnt)
  where
  go (avar /\ modules) = do
    next <- fold <$>
      ( Par.all $
          modules <#> \m -> do
            addDepsToGraphForSingleModule avar m
      )
    if Array.null next then Run.liftAff (AVar.read avar <#> _.done >>> Set.size) >>= Done >>> pure
    else pure $ Loop (avar /\ next)

  addDepsToGraphForSingleModule :: AVar _ -> ModuleName -> Run _ (Array ModuleName)
  addDepsToGraphForSingleModule avar m = do
    { done } <- Run.liftAff $ AVar.read avar
    if m `Set.member` done then pure []
    else do
      let corefnPath = moduleArtifactPath "output" m CorefnJson
      readTextFile corefnPath
        <#> (parseJson >=> CFJ.decodeModule)
        >>= case _ of
          Left e -> Run.liftEffect $ Exn.throw $
            ("Failed to decode corefn.json (" <> show corefnPath <> ").\n")
              <> "Error description: \n"
              <> printJsonDecodeError e
          Right cfm -> do
            let imports = listImportedModules cfm
            Run.liftAff do
              cur <- AVar.take avar
              let
                next = cur
                  { graph = cur.graph
                      # HG.addVertices imports
                      # HG.addVertexWithOutgoingEdges m imports
                  , done = Set.insert m cur.done
                  }
              AVar.put next avar
            pure imports

  listImportedModules :: forall a. CF.Module a -> Array ModuleName
  listImportedModules (CF.Module m) = m.imports
    <#> (CF.importName >>> coerce)
    -- exclude prim modules
    # Array.filter (not <<< isPrimModule)
    -- exclude the importing module itself
    # Array.filter (_ /= coerce m.name)

  isPrimModule :: ModuleName -> Boolean
  isPrimModule (ModuleName m) = m `elem`
    [ "Prim"
    , "Prim.Boolean"
    , "Prim.Coerce"
    , "Prim.Ordering"
    , "Prim.Row"
    , "Prim.RowList"
    , "Prim.Symbol"
    , "Prim.Int"
    , "Prim.TypeError"
    ]

openExternsCbor :: forall r. FilePath -> ModuleName -> Run (AFF + FS + EXCEPT String + LOG + r) ExternsFile
openExternsCbor outdir modname = do
  let
    externsFilePath = moduleArtifactPath outdir modname ExternsCbor
  f <- readCborFile externsFilePath
  case runDecoder (decoder @ExternsFile) f of
    Left err -> do
      Log.error "Failed to decode externs.cbor"
      Except.throw (describeError err)
    Right ext -> pure ext

makeExternsEnv :: forall r' r. Array (Set ModuleName) -> Run (AFF + PAR (FS + EXCEPT String + AFF + LOG + r) + r') GlobalEnv
makeExternsEnv sortedGraph = do
  aEnv <- Run.liftAff (AVar.new Global.emptyEnv)
  for_ sortedGraph \moduleSet -> do
    -- moduleSet` contains those modules of same depdendency level,
    -- so we can safely collect declarations from externs and add 
    -- them in env in paralle. 
    exts <- Par.all
      ( moduleSet # Array.fromFoldable <#> \mod -> do
          openExternsCbor "output" mod
      )
    Run.liftAff do
      env <- AVar.take aEnv
      AVar.put (foldr Global.applyExternsToEnv env exts) aEnv
  pure Global.emptyEnv