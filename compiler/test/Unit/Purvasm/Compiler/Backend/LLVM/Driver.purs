-- | The B2 native-build end-to-end gate: compile the `Slice1` module **from its CoreFn** (not a
-- | hand-built ANF) through the neutral `Purvasm.Compiler.build` driver + the LLVM `Backend` (`llvmBackend`),
-- | and assert the emitted module object + init/entry object are byte-identical to boot's `--no-opt` `.ll`
-- | (fixtures under `test/fixtures/slice1/`). The fixture module has a single, fully-reachable binding
-- | (`identInt`), so B2's "emit every module binding" and boot B1's DCE'd output coincide. This exercises
-- | the backend-private `DictElim` bridge (ADR-0086 Addendum) in `lowerModule`/`lowerEntry`, since the
-- | driver runs no `DictElim` under `--no-opt`.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Driver where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler (LoadResult(..), build, defaultHooks)
import Purvasm.Compiler.Backend.LLVM.Driver (llvmBackend)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseModule :: String -> Either String Module
parseModule src = jsonParser src >>= (lmap printJsonDecodeError <<< decodeModule)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Driver" do
  describe "llvmBackend via Purvasm.Compiler.build (B2 per-module, CoreFn → .ll)" do
    it "compiles Slice1 from CoreFn to byte-identical module + entry objects" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      expectedMod <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/mod_0.ll")
      expectedEntry <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/entry.ll")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          modBuf <- liftEffect (Ref.new [])
          entryBuf <- liftEffect (Ref.new Nothing)
          let
            backend = llvmBackend { isEffect: false, heapWords: 1048576, debug: false }
            -- `Slice1` imports `Prim`, which the table reports `Missing` (skipped), so the closure is
            -- exactly `[Slice1]`. The entry `Slice1.identInt` (a bare `--value` entry) is what `entryExprOf`
            -- builds; `isEffect: false` / `opt: false` matches boot's `--no-opt` reference.
            action =
              { workdir: "."
              , maxOptimizeIter: 1
              , loadModule: \name -> pure (if name == "Slice1" then Loaded { path: "Slice1", mod } else Missing)
              -- ADR-0090 stub: the fixture module declares no foreigns, so the driver never calls this.
              , foreignSigsOf: \_ -> pure (Right Map.empty)
              , emitFile: \artifact -> Ref.modify_ (\a -> Array.snoc a artifact.backendIR) modBuf $> "mod.ll"
              , emitEntry: \o -> Ref.write (Just o) entryBuf $> "entry.ll"
              , hooks: defaultHooks
              }
            opts = { entryModule: "Slice1", entryName: "identInt", isEffect: false, opt: false }
          _ <- liftEffect (build backend action opts)
          mods <- liftEffect (Ref.read modBuf)
          entry <- liftEffect (Ref.read entryBuf)
          mods `shouldEqual` [ expectedMod ]
          entry `shouldEqual` Just expectedEntry
