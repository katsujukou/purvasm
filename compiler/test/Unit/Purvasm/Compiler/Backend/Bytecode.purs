-- | The bytecode backend end-to-end path (ADR-0088 (a)): drive `Purvasm.Compiler.build` with
-- | `bytecodeBackend` over the `Slice1` CoreFn fixture, assert it produces a linkable `.pmo`
-- | (`ModuleArtifact`), and that `Link.link` merges it into a non-empty runnable image — under both
-- | `--no-opt` (normalise-only) and `--opt` (the optimiser runs; the VM measurement field). This is a
-- | *behavioural* smoke test, not a boot byte-identity gate (that gate is released for the VM object).
module Test.Unit.Purvasm.Compiler.Backend.Bytecode where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler (LoadResult(..), build, defaultHooks)
import Purvasm.Compiler.Backend.Bytecode (bytecodeBackend)
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.Ffi as Ffi
import Purvasm.Compiler.Link (link)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseModule :: String -> Either String Module
parseModule src = jsonParser src >>= (lmap printJsonDecodeError <<< decodeModule)

-- Build the Slice1 closure through `bytecodeBackend`, returning the emitted `.pmo` artifacts.
buildArtifacts :: Boolean -> Module -> Effect (Array ModuleArtifact)
buildArtifacts opt mod = do
  modBuf <- Ref.new []
  let
    action =
      { workdir: "."
      , maxOptimizeIter: 5
      , loadModule: \name -> pure (if name == "Slice1" then Loaded { path: "Slice1", mod } else Missing)
      , emitFile: \artifact -> Ref.modify_ (\xs -> Array.snoc xs artifact.backendIR) modBuf $> "p.pmo"
      , emitEntry: \_ -> pure "e"
      , hooks: defaultHooks
      }
    opts = { entryModule: "Slice1", entryName: "identInt", isEffect: false, opt }
  _ <- build bytecodeBackend action opts
  Ref.read modBuf

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.Bytecode" do
  let
    run opt = do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err) $> []
        Right mod -> liftEffect (buildArtifacts opt mod)

  it "builds Slice1 to a linkable .pmo and links a non-empty image (--no-opt)" do
    arts <- run false
    Array.length arts `shouldEqual` 1
    case Array.head arts of
      Just a -> (Array.length a.groups > 0) `shouldEqual` true
      Nothing -> fail "no .pmo emitted"
    let image = link arts Ffi.resolver (TmVar "Slice1.identInt")
    (Array.length image.gdefs > 0) `shouldEqual` true

  it "builds and links under --opt (the optimiser runs, VM measurement field)" do
    arts <- run true
    Array.length arts `shouldEqual` 1
    let image = link arts Ffi.resolver (TmVar "Slice1.identInt")
    (Array.length image.gdefs > 0) `shouldEqual` true
