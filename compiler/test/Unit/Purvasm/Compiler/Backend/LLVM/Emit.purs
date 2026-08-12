-- | `emitFunction` is the whole ANF→`.ll` lowering for a function: frame open, param rooting, body, and
-- | the two-entry (`$d` + wrapper) shape (ADR-0076 §1). Run in isolation on the `identInt x = x` lifted
-- | and asserted against boot's `--no-opt` `.ll` block (labels here start at `rchk1` because no `$init`
-- | precedes it; the in-context exact offset is checked by the `Program` differential).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Emit where

import Prelude

import Data.Tuple (snd)
import Purvasm.Compiler.Backend.LLVM.Emit (emitFunction, emitGcafInit)
import Purvasm.Compiler.Backend.LLVM.Monad (makeCx, renderChunks, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Types (Lifted(..), LiftedBody(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.Literal (Literal(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
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
        ctx = snd $ runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, profileApply: false, byNeed: true })
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
              (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, profileApply: false, byNeed: true })
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

  describe "emitGcafInit (ADR-0106 slice 2 — the plan-driven fixed-shape init)" do
    let
      gcafLl gkeys e = renderChunks
        ( _.md $ snd $ runCodegen
            (makeCx { gkeys: Set.fromFoldable gkeys, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true, defined: Set.empty, profileApply: false, byNeed: true })
            (emitGcafInit "TestK" e)
        )
    it "a frameless Gcaf: no frame open, no transient root, no pop — the permanent root still lands" do
      -- `x = 1`: no safepoint anywhere, the plan elides the frame entirely; only the
      -- permanent tier's rooting block remains, straight from the immediate.
      gcafLl [] (Ret (CAtom (AtomLit (LInt 1)))) `shouldEqual`
        ( "define void @pv_g_TestK$init(ptr %ctx) {\n"
            <> "entry:\n"
            <> "  br label %rchk1\n"
            <> "rchk1:\n"
            <> "  %t1 = getelementptr i8, ptr %ctx, i64 8\n"
            <> "  %t2 = load i64, ptr %t1\n"
            <> "  %t4 = getelementptr i8, ptr %ctx, i64 16\n"
            <> "  %t3 = load i64, ptr %t4\n"
            <> "  %t5 = icmp eq i64 %t2, %t3\n"
            <> "  br i1 %t5, label %rslow3, label %rfast2\n"
            <> "rfast2:\n"
            <> "  %t6 = load ptr, ptr %ctx\n"
            <> "  %t7 = getelementptr i64, ptr %t6, i64 %t2\n"
            <> "  store i64 3, ptr %t7\n"
            <> "  %t8 = add i64 %t2, 1\n"
            <> "  store i64 %t8, ptr %t1\n"
            <> "  br label %rdone4\n"
            <> "rslow3:\n"
            <> "  %t9 = call i64 @pv_root(ptr %ctx, i64 3)\n"
            <> "  br label %rdone4\n"
            <> "rdone4:\n"
            <> "  %t10 = phi i64 [ %t2, %rfast2 ], [ %t9, %rslow3 ]\n"
            <> "  store i64 %t10, ptr @pv_g_TestK$root\n"
            <> "  ret void\n"
            <> "}\n"
            <> "\n"
        )

    it "an activation-crossing Gcaf KEEPS its frame (open before the body, pop before the permanent tier)" do
      -- `x = let s = "ab" in (let t = "cd" in s)`: `s` crosses `t`'s allocation safepoint,
      -- so the plan roots it — the init must open a transient frame and pop it before the
      -- permanent root.
      let
        ll = gcafLl []
          ( Let "s" (CAtom (AtomLit (LString "ab")))
              ( Let "t" (CAtom (AtomLit (LString "cd")))
                  (Ret (CAtom (AtomVar "s")))
              )
          )
      -- structural assertions (full golden would be brittle): a frame open (roots_len load)
      -- precedes the body, and a pop (store back to roots_len) precedes the permanent store.
      (ll # String.contains (String.Pattern "@pv_new_str")) `shouldEqual` true
      (ll # String.contains (String.Pattern "store i64 %t1, ptr %t")) `shouldEqual` true
      (ll # String.contains (String.Pattern "ptr @pv_g_TestK$root")) `shouldEqual` true

    it "a GlobalSlot-aliasing Gcaf handle-copies frameless (reads another global, no root block)" do
      -- `x = someGlobal`: the body is a bare global read — no safepoint, frame elided,
      -- candidate arrives as a GlobalSlot token, the permanent tier copies its handle.
      gcafLl [ "Dep.someGlobal" ] (Ret (CAtom (AtomVar "Dep.someGlobal"))) `shouldEqual`
        ( "define void @pv_g_TestK$init(ptr %ctx) {\n"
            <> "entry:\n"
            <> "  %t1 = load i64, ptr @pv_g_Dep_2esomeGlobal$root\n"
            <> "  store i64 %t1, ptr @pv_g_TestK$root\n"
            <> "  ret void\n"
            <> "}\n"
            <> "\n"
        )
