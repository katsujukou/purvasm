module Purvasm.CLI.Compile where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync (mkdir', readTextFile, writeTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Compile (compileModule)
import Purvasm.CLI.Option (CompileOptions)

-- | Compile a single module to its `.pmo`/`.pmi` objects (boot's `purvm compile`):
-- | read `<corefnDir>/<entryModule>/corefn.json`, decode it, run `compileModule`, and
-- | write the artifacts (byte-identical to boot) under `<outDir>/_build`.
cmd :: CompileOptions -> Effect Unit
cmd opts = do
  let corefnPath = opts.corefnDir <> "/" <> opts.entryModule <> "/corefn.json"
  src <- readTextFile UTF8 corefnPath
  case parseModule src of
    Left err -> Console.error ("purvmc: " <> err)
    Right m -> do
      let
        artifact = compileModule m
        buildDir = opts.outDir <> "/_build"
      mkdir' buildDir { recursive: true, mode: mkPerms all all all }
      writeTextFile UTF8 (buildDir <> "/" <> artifact.name <> ".pmo") (moduleToString artifact)
      writeTextFile UTF8 (buildDir <> "/" <> artifact.name <> ".pmi")
        (interfaceToString (interfaceOf artifact))
      Console.log ("compiled " <> artifact.name)

parseModule :: String -> Either String Module
parseModule src = do
  json <- jsonParser src
  lmap printJsonDecodeError (decodeModule json)
