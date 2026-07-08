-- | The inline `Int`/`Boolean` primop IR must be byte-identical to boot's `inline_prim` — verified here
-- | against boot's exact emission shape (the `%t = trunc i64 %t' to i32` pattern where the trunc temp is
-- | numbered *before* the `ashr` payload temp, boot's right-to-left arg eval; cf. the `add i32` block in
-- | boot's `--no-opt` `.ll`). `primSym` maps each op to its runtime helper symbol.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Prim where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Map as Map
import Data.Set as Set
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, makeCx, renderBuffer, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Prim (inlinePrim, primSym)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Run an emitter from a fresh release-mode context, returning (result, rendered function body).
run :: forall a. Codegen a -> Tuple a String
run m =
  let
    Tuple a ctx = runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, inlineAbi: true }) m
  in
    Tuple a (renderBuffer ctx.fn)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Prim" do
  describe "primSym" do
    it "maps scalar Int/Bool ops to pure helpers and boxed ops to ctx-taking ones" do
      primSym AddInt `shouldEqual` Tuple "pv_prim_add_int" false
      primSym DivInt `shouldEqual` Tuple "pv_prim_div_int" false
      primSym EqBool `shouldEqual` Tuple "pv_prim_eq_bool" false
      primSym AddNumber `shouldEqual` Tuple "pv_prim_add_number" true
      primSym RecordGet `shouldEqual` Tuple "pv_prim_record_get" true

  describe "inlinePrim" do
    it "emits a 32-bit add (untag, add, re-tag), trunc temp before its ashr" do
      let Tuple r body = run (inlinePrim AddInt [ "%a", "%b" ])
      r `shouldEqual` Just "%t8"
      body `shouldEqual`
        ( "  %t2 = ashr i64 %a, 1\n"
            <> "  %t1 = trunc i64 %t2 to i32\n"
            <> "  %t4 = ashr i64 %b, 1\n"
            <> "  %t3 = trunc i64 %t4 to i32\n"
            <> "  %t5 = add i32 %t1, %t3\n"
            <> "  %t6 = sext i32 %t5 to i64\n"
            <> "  %t7 = shl i64 %t6, 1\n"
            <> "  %t8 = or i64 %t7, 1\n"
        )

    it "emits a signed less-than comparison re-tagged as a Boolean" do
      let Tuple r body = run (inlinePrim LtInt [ "%a", "%b" ])
      r `shouldEqual` Just "%t8"
      body `shouldEqual`
        ( "  %t2 = ashr i64 %a, 1\n"
            <> "  %t1 = trunc i64 %t2 to i32\n"
            <> "  %t4 = ashr i64 %b, 1\n"
            <> "  %t3 = trunc i64 %t4 to i32\n"
            <> "  %t5 = icmp slt i32 %t1, %t3\n"
            <> "  %t6 = zext i1 %t5 to i64\n"
            <> "  %t7 = shl i64 %t6, 1\n"
            <> "  %t8 = or i64 %t7, 1\n"
        )

    it "emits Boolean equality over the operands' truthiness" do
      let Tuple r body = run (inlinePrim EqBool [ "%a", "%b" ])
      r `shouldEqual` Just "%t8"
      body `shouldEqual`
        ( "  %t2 = ashr i64 %a, 1\n"
            <> "  %t1 = icmp ne i64 %t2, 0\n"
            <> "  %t4 = ashr i64 %b, 1\n"
            <> "  %t3 = icmp ne i64 %t4, 0\n"
            <> "  %t5 = icmp eq i1 %t1, %t3\n"
            <> "  %t6 = zext i1 %t5 to i64\n"
            <> "  %t7 = shl i64 %t6, 1\n"
            <> "  %t8 = or i64 %t7, 1\n"
        )

    it "emits Boolean negation" do
      let Tuple r body = run (inlinePrim NotBool [ "%a" ])
      r `shouldEqual` Just "%t6"
      body `shouldEqual`
        ( "  %t2 = ashr i64 %a, 1\n"
            <> "  %t1 = icmp ne i64 %t2, 0\n"
            <> "  %t3 = xor i1 %t1, true\n"
            <> "  %t4 = zext i1 %t3 to i64\n"
            <> "  %t5 = shl i64 %t4, 1\n"
            <> "  %t6 = or i64 %t5, 1\n"
        )

    it "returns Nothing for ops with no inline form (div, boxed)" do
      fst (run (inlinePrim DivInt [ "%a", "%b" ])) `shouldEqual` Nothing
      fst (run (inlinePrim AddNumber [ "%a", "%b" ])) `shouldEqual` Nothing
  where
  fst (Tuple a _) = a
