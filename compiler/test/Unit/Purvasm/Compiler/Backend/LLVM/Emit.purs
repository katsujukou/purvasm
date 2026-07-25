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
import Purvasm.Compiler.Literal (Literal(..))
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
      -- ADR-0105 slice 2 (re-baselined, §4 emission-class licence = the behavioural gate):
      -- `identInt = \x -> x` has NO safepoint and no crossing definition, so the plan elides the
      -- frame and the param root entirely — the direct entry is a bare return of the parameter.
      renderChunks ctx.md `shouldEqual`
        ( "define tailcc i64 @pv_g_Slice1_2eidentInt$d(ptr %ctx, i64 %env, i64 %p0) {\n"
            <> "entry:\n"
            <> "  ret i64 %p0\n"
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

    it "lowers a GER perform as the identical .ll to a one-argument unit call (ADR-0099)" do
      -- `CPerform t` delegates to the `CApp t [unit]` path, so its emitted `.ll` must be
      -- byte-identical to the explicit unit-argument call (reuses direct/musttail/pv_apply +
      -- tail position). Comparing the two emissions locks the delegation without hardcoding
      -- brittle `.ll` bytes.
      let
        emitLl body = renderChunks
          ( _.md $ snd $ runCodegen
              (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true })
              ( emitFunction
                  ( Lifted
                      { name: "pv_g_Test_2ef"
                      , params: [ "m" ]
                      , captures: []
                      , body: LBody body
                      , selfName: Nothing
                      , captureFns: []
                      , exported: true
                      }
                  )
              )
          )
      emitLl (Ret (CPerform (AtomVar "m")))
        `shouldEqual` emitLl (Ret (CApp (AtomVar "m") [ AtomLit (LInt 0) ]))
