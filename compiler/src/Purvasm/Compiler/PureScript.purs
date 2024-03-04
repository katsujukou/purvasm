module Purvasm.Compiler.PureScript where

import Prelude

import Data.Argonaut (parseJson, printJsonDecodeError)
import Data.Array (elem, fold, nub)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.AVar as AVar
import Effect.Exception as Exn
import PureScript.CST (PartialModule(..), RecoveredParserResult(..), parsePartialModule)
import PureScript.CST.Types (ModuleHeader(..))
import PureScript.CST.Types as CST
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json as CFJ
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (describeError, runDecoder)
import Purvasm.Compiler.Effects.FS (FS, FilePath, concatPaths, readCborFile, readTextFile)
import Purvasm.Compiler.Effects.FS as FS
import Purvasm.Compiler.Effects.Log (LOG)
import Purvasm.Compiler.Effects.Log as Log
import Purvasm.Compiler.Effects.Par (PAR)
import Purvasm.Compiler.Effects.Par as Par
import Purvasm.Compiler.ModuleImportMap (ModuleImportMap)
import Purvasm.Types (ModuleName(..))
import Run (AFF, EFFECT, Run)
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

moduleArtifactPath :: forall r. FilePath -> ModuleName -> ModuleArtifact -> Run (FS + r) FilePath
moduleArtifactPath outdir (ModuleName modname) art = concatPaths [ outdir, modname, artifactFileName art ]

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

openPartialModule :: forall r. ModuleName -> Run (LOG + EXCEPT String + FS + r) (CFJ.PartialModule CF.Ann)
openPartialModule m = do
  filepath <- moduleArtifactPath "output" m CorefnJson
  json <- FS.readTextFile filepath
  case json # parseJson >>= CFJ.runJsonDecode CFJ.decodePartialModule of
    Left err -> do
      Log.error $ printJsonDecodeError err
      Except.throw $ "Failed to decode corefn.json of module " <> coerce m <> "."
    Right pm -> pure pm

mkModuleImportMap :: forall r r'. Array ModuleName -> Run (LOG + PAR (LOG + FS + EXCEPT String + AFF + r') + AFF + r) (ModuleImportMap /\ Int)
mkModuleImportMap roots = do
  avar <- Run.liftAff $ AVar.new HashMap.empty
  cnt <- go (avar /\ roots)
  importMap <- Run.liftAff (AVar.take avar)
  pure (importMap /\ cnt)
  where
  go (avar /\ []) = do
    Run.liftAff (AVar.read avar) >>= HashMap.size >>> pure

  go (avar /\ importers) = do
    next <- nub <<< fold <$> Par.all
      ( importers <#> \m -> do
          (CFJ.PartialModule pm') <- openPartialModule m
          let
            pm = pm'
              { imports = pm'.imports
                  # Array.filter
                      ( \(CF.Import _ name) ->
                          -- exclude prim modules
                          name `not <<< elem` primModules
                            -- exclude the importing module itself
                            && name /= pm'.name
                      )
              }
            pModule = CFJ.PartialModule pm
          Run.liftAff do
            importMap <- AVar.take avar
            avar # AVar.put
              (HashMap.insert m pModule importMap)
            pure $ listImportedModules pModule
      )
    go (avar /\ next)

  listImportedModules :: forall a. CFJ.PartialModule a -> Array ModuleName
  listImportedModules (CFJ.PartialModule m) = m.imports
    <#> (CF.importName >>> coerce)

openExternsCbor :: forall r. FilePath -> ModuleName -> Run (FS + EXCEPT String + LOG + r) ExternsFile
openExternsCbor outdir modname = do
  externsFilePath <- moduleArtifactPath outdir modname ExternsCbor
  f <- readCborFile externsFilePath
  case runDecoder (decoder @ExternsFile) f of
    Left err -> do
      Log.error "Failed to decode externs.cbor"
      Except.throw (describeError err)
    Right ext -> pure ext

primModules :: Array CF.ModuleName
primModules = CF.ModuleName <$>
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

