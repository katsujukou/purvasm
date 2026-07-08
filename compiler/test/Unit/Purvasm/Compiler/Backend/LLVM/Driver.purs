-- | The B2 native-build end-to-end gate: compile the `Slice1` module **from its CoreFn** (not a
-- | hand-built ANF) through separate per-module compilation (`nativeSplit`), and assert the emitted
-- | module object + init/entry object are byte-identical to boot's `--no-opt` `.ll` (fixtures under
-- | `test/fixtures/slice1/`). The fixture module has a single, fully-reachable binding (`identInt`), so
-- | B2's "emit every module binding" and boot B1's DCE'd output coincide.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Driver where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Tuple (snd)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler.Backend.LLVM.Driver (nativeSplit)
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
              }
              [ mod ]
              slice1Entry
          map snd out.modules `shouldEqual` [ expectedMod ]
          out.entry `shouldEqual` expectedEntry
