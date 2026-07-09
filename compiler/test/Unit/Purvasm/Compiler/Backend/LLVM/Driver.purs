-- | The B2 native-build end-to-end gate: compile the `Slice1` module **from its CoreFn** (not a
-- | hand-built ANF) through separate per-module compilation (`nativeSplit`), and assert the emitted
-- | module object + init/entry object are byte-identical to boot's `--no-opt` `.ll` (fixtures under
-- | `test/fixtures/slice1/`). The fixture module has a single, fully-reachable binding (`identInt`), so
-- | B2's "emit every module binding" and boot B1's DCE'd output coincide.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Driver where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler (LoadResult(..), build, defaultHooks)
import Purvasm.Compiler.Backend.LLVM.Driver (llvmBackend, nativeSplit)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseModule :: String -> Either String Module
parseModule src = jsonParser src >>= (lmap printJsonDecodeError <<< decodeModule)

-- The `--value -e identInt` entry: read the global binding (no application → no `CApp`, slice-1a).
slice1Entry :: Expr
slice1Entry = Ret (CAtom (AtomVar "Slice1.identInt"))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Driver" do
  describe "nativeSplit (B2 per-module, CoreFn → .ll)" do
    it "compiles Slice1 from CoreFn to byte-identical module + entry objects" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      expectedMod <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/mod_0.ll")
      expectedEntry <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/entry.ll")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right mod -> do
          let
            -- The surface (`identInt` is a public export of arity 1 → `Cfn 1`, so its `$d` is the
            -- exported `define tailcc`) is now derived from the module's own gdefs + exports.
            out = nativeSplit
              { isEffect: false
              , heapWords: 1048576
              , debug: false
              , opt: false
              }
              [ mod ]
              slice1Entry
          map snd out.modules `shouldEqual` [ expectedMod ]
          out.entry `shouldEqual` expectedEntry

  describe "llvmBackend via Purvasm.Compiler.build (ADR-0087 wiring)" do
    it "emits the same Slice1 module + entry objects as nativeSplit (byte-identity through the driver)" do
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
            -- exactly `[Slice1]` — the same single-module input `nativeSplit` gets above.
            action =
              { workdir: "."
              , maxOptimizeIter: 1
              , loadModule: \name -> pure (if name == "Slice1" then Loaded { path: "Slice1", mod } else Missing)
              , emitFile: \artifact -> Ref.modify_ (\a -> Array.snoc a artifact.backendIR) modBuf $> "mod.ll"
              , emitEntry: \o -> Ref.write (Just o) entryBuf $> "entry.ll"
              , hooks: defaultHooks
              }
            -- The entry `Ret (CAtom (AtomVar "Slice1.identInt"))` is what `entryExprOf` builds for a bare
            -- `--value` entry `Slice1.identInt`, matching `slice1Entry`.
            opts = { entryModule: "Slice1", entryName: "identInt", isEffect: false, opt: false }
          _ <- liftEffect (build backend action opts)
          mods <- liftEffect (Ref.read modBuf)
          entry <- liftEffect (Ref.read entryBuf)
          mods `shouldEqual` [ expectedMod ]
          entry `shouldEqual` Just expectedEntry
