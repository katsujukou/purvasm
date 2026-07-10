-- | `emitFunction` is the whole ANF→`.ll` lowering for a function: frame open, param rooting, body, and
-- | the two-entry (`$d` + wrapper) shape (ADR-0076 §1). Run in isolation on the `identInt x = x` lifted
-- | and asserted against boot's `--no-opt` `.ll` block (labels here start at `rchk1` because no `$init`
-- | precedes it; the in-context exact offset is checked by the `Program` differential).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Emit where

import Prelude

import Data.Tuple (snd)
import Purvasm.Compiler.Backend.LLVM.Emit (emitFunction)
import Purvasm.Compiler.Backend.LLVM.Monad (makeCx, renderChunks, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Types (Lifted(..), LiftedBody(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The `identInt` top-level function's lifted record (name = mangled key, exported, no captures).
identIntLifted :: Lifted
identIntLifted = Lifted
  { name: "pv_g_Slice1_2eidentInt"
  , params: [ "x" ]
  , captures: []
  , body: LBody (Ret (CAtom (AtomVar "x")))
  , selfName: Nothing
  , captureFns: []
  , exported: true
  }

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Emit" do
  describe "emitFunction" do
    it "emits the tailcc $d entry and the generic wrapper for identInt" do
      let
        ctx = snd $ runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true })
          (emitFunction identIntLifted)
      renderChunks ctx.md `shouldEqual`
        ( "define tailcc i64 @pv_g_Slice1_2eidentInt$d(ptr %ctx, i64 %env, i64 %p0) {\n"
            <> "entry:\n"
            <> "  %t2 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  %t1 = load i64, ptr %t2\n"
            <> "  br label %rchk1\n"
            <> "rchk1:\n"
            <> "  %t3 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  %t4 = load i64, ptr %t3\n"
            <> "  %t6 = getelementptr i8, ptr %ctx, i64 16\n"
            <> "  %t5 = load i64, ptr %t6\n"
            <> "  %t7 = icmp eq i64 %t4, %t5\n"
            <> "  br i1 %t7, label %rslow3, label %rfast2\n"
            <> "rfast2:\n"
            <> "  %t8 = load ptr, ptr %ctx\n"
            <> "  %t9 = getelementptr i64, ptr %t8, i64 %t4\n"
            <> "  store i64 %p0, ptr %t9\n"
            <> "  %t10 = add i64 %t4, 1\n"
            <> "  store i64 %t10, ptr %t3\n"
            <> "  br label %rdone4\n"
            <> "rslow3:\n"
            <> "  %t11 = call i64 @pv_root(ptr %ctx, i64 %p0)\n"
            <> "  br label %rdone4\n"
            <> "rdone4:\n"
            <> "  %t12 = phi i64 [ %t4, %rfast2 ], [ %t11, %rslow3 ]\n"
            <> "  %t13 = load ptr, ptr %ctx\n"
            <> "  %t14 = getelementptr i64, ptr %t13, i64 %t12\n"
            <> "  %t15 = load i64, ptr %t14\n"
            <> "  %t16 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  store i64 %t1, ptr %t16\n"
            <> "  ret i64 %t15\n"
            <> "}\n"
            <> "\n"
            <> "define internal i64 @pv_g_Slice1_2eidentInt(ptr %ctx, i64 %clo, ptr %args, i64 %nargs) {\n"
            <> "entry:\n"
            <> "  %t1 = getelementptr i64, ptr %args, i64 0\n"
            <> "  %t2 = load i64, ptr %t1\n"
            <> "  %t3 = call tailcc i64 @pv_g_Slice1_2eidentInt$d(ptr %ctx, i64 1, i64 %t2)\n"
            <> "  ret i64 %t3\n"
            <> "}\n"
            <> "\n"
        )
