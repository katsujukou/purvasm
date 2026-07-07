-- | The slice-1a end-to-end byte-identity gate (ADR-0082 §2/§3): `programSplit` on the linked ANF of
-- | the no-import module `Slice1` (`identInt x = x`, compiled `--value -e identInt`) must produce a
-- | module object and an init/entry object **byte-identical** to boot's `--no-opt` `.ll` (the fixtures
-- | under `test/fixtures/slice1/`, captured from boot). This exercises the whole backend: `Gfun` two-entry
-- | emission, `$init` + root globals, `pv_init_all`, and the `@main` entry stub reading the global.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Program where

import Prelude

import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Purvasm.Compiler.Backend.LLVM.Program (programSplit)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The linked ANF spine boot's front half produces for `Slice1` with entry `identInt` (`--value`): the
-- `identInt` binding followed by the bare-value entry (read the global closure).
slice1Anf :: Expr
slice1Anf =
  Let "Slice1.identInt" (CLam [ "x" ] (Ret (CAtom (AtomVar "x"))))
    (Ret (CAtom (AtomVar "Slice1.identInt")))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Program" do
  describe "programSplit (slice-1a end-to-end)" do
    let
      -- boot derives the surface from each module's `.pmi`: `identInt` is a public export of arity 1
      -- (`Efn 1` → `Cfn 1`), so its `$d` is the exported cross-module direct entry (`define tailcc`,
      -- no `internal`).
      out = programSplit
        { isEffect: false
        , heapWords: 1048576
        , surface: Map.fromFoldable [ Tuple "Slice1.identInt" (Cfn 1) ]
        , debug: false
        }
        slice1Anf

    it "partitions into exactly one Slice1 module object" do
      map fst out.modules `shouldEqual` [ "Slice1" ]

    it "emits the Slice1 module object byte-identical to boot --no-opt" do
      expected <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/mod_0.ll")
      map snd out.modules `shouldEqual` [ expected ]

    it "emits the init/entry object byte-identical to boot --no-opt" do
      expected <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/entry.ll")
      out.entry `shouldEqual` expected
