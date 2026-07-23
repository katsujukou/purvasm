-- | The ADR-0105 §2 liveness analysis, pinned on the edge classes the ADR names: crossing vs
-- | consumed-at-op (the §1 use-at-call boundary), branch behaviour, closure-capture escape, the
-- | self-recursion `%env` word, case/dtree conservatism, frame elision, and the §2a
-- | default-stack scale fixtures. The §4 release/debug `RootPlan`-equality contract holds by
-- | construction — `activationPlan` takes no ABI-profile input — and the test documents that by
-- | asserting the plan is a pure function of (config, body) alone.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Liveness where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Purvasm.Compiler.Backend.LLVM.Liveness (ActivationConfig, activationPlan, cexprMayRootLocally, envPseudo, needsFrame, operandsMayRoot, primOpSafepoint)
import Purvasm.Compiler.Backend.LLVM.Mangle (sortRecordFields)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

cfg0 :: ActivationConfig
cfg0 = { params: [], captures: [], selfName: Nothing }

crossingOf :: ActivationConfig -> Expr -> Array String
crossingOf cfg body = Set.toUnfoldable (activationPlan cfg body).crossing

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Liveness" do
  describe "activationPlan (crossing)" do
    it "a value consumed as the next call's argument does not cross (sidenote 0011's fib case)" do
      -- let a = n - 1 in f a  — `a` is an operand of the call, never used after it.
      let
        body = Let "a" (CPrim SubInt [ var "n", int 1 ])
          (Ret (CApp (var "f") [ var "a" ]))
      crossingOf (cfg0 { params = [ "n" ] }) body `shouldEqual` []

    it "a value used after an intervening call crosses" do
      -- let x = g () in let y = h () in x + y — `x` is live across the `h` call.
      let
        body = Let "x" (CApp (var "g") [])
          ( Let "y" (CApp (var "h") [])
              (Ret (CPrim AddInt [ var "x", var "y" ]))
          )
      crossingOf cfg0 body `shouldEqual` [ "x" ]

    it "use-at-call vs live-after-call: the same operand crosses only when also used later" do
      let
        mk after = Let "x" (CApp (var "g") [])
          ( Let "r" (CApp (var "f") [ var "x" ])
              (Ret after)
          )
      -- x consumed by the f-call only → the g/f safepoints see live-after = {r}, not x
      crossingOf cfg0 (mk (CAtom (var "r"))) `shouldEqual` []
      -- x also used after the f-call → crosses it
      crossingOf cfg0 (mk (CPrim AddInt [ var "x", var "r" ])) `shouldEqual` [ "x" ]

    it "a param live at a branch entry crosses the condition force" do
      -- if c then f x else 1 — the cond force precedes both branches.
      let
        body = Ret
          ( CIf (var "c")
              (Ret (CApp (var "f") [ var "x" ]))
              (Ret (CAtom (int 1)))
          )
      crossingOf (cfg0 { params = [ "c", "x" ] }) body `shouldEqual` [ "x" ]

    it "no-safepoint straight-line code crosses nothing" do
      -- let a = x + 1 in a * 2 — int-literal second operands: no force-capable later operand,
      -- scalar ops are register-only.
      let
        body = Let "a" (CPrim AddInt [ var "x", int 1 ])
          (Ret (CPrim MulInt [ var "a", int 2 ]))
        plan = activationPlan (cfg0 { params = [ "x" ] }) body
      Set.toUnfoldable plan.crossing `shouldEqual` ([] :: Array String)
      plan.anySafepoint `shouldEqual` true -- the forces of `x`/`a` remain potential safepoints
      needsFrame plan `shouldEqual` false

    it "capture escape ends the caller's obligation at the closure construction" do
      -- let g = \p -> p + cap in g — cap is consumed by pv_make_closure, never used after.
      let
        body = Let "g" (CLam [ "p" ] (Ret (CPrim AddInt [ var "p", var "cap" ])))
          (Ret (CAtom (var "g")))
      crossingOf (cfg0 { params = [ "cap" ] }) body `shouldEqual` []

    it "the closure value itself crosses a later safepoint; its captures still do not" do
      -- let g = \p -> cap in let z = h () in g z
      let
        body = Let "g" (CLam [ "p" ] (Ret (CAtom (var "cap"))))
          ( Let "z" (CApp (var "h") [])
              (Ret (CApp (var "g") [ var "z" ]))
          )
      crossingOf (cfg0 { params = [ "cap" ] }) body `shouldEqual` [ "g" ]

    it "a nested lambda body's own crossings do NOT leak into this activation (closure opacity)" do
      -- \p -> let q = f () in let s = h () in q + s — everything crossing is inside the lambda.
      let
        lamBody = Let "q" (CApp (var "f") [])
          ( Let "s" (CApp (var "h") [])
              (Ret (CPrim AddInt [ var "q", var "s" ]))
          )
        body = Ret (CLam [ "p" ] lamBody)
      crossingOf cfg0 body `shouldEqual` []

    it "the self %env word crosses when a self-call sits after a safepoint" do
      -- if c then loop m else n — the cond force precedes the self-call, which reads %env.
      let
        body = Ret
          ( CIf (var "c")
              (Ret (CApp (var "loop") [ var "m" ]))
              (Ret (CAtom (var "n")))
          )
        plan = activationPlan { params: [ "c", "m", "n" ], captures: [], selfName: Just "loop" } body
      Set.member envPseudo plan.crossing `shouldEqual` true

    it "an immediate self tail-call does not root %env (consumed at the call)" do
      let
        body = Ret (CApp (var "loop") [ var "m" ])
        plan = activationPlan { params: [ "m" ], captures: [], selfName: Just "loop" } body
      Set.member envPseudo plan.crossing `shouldEqual` false

    it "guard clauses are sequential: a binder used only after a failing guard crosses its safepoints" do
      -- case s of b | g () -> 1 | true -> b — `b` is read only in clause 2, but clause 1's
      -- guard (a call) runs first and may fall through: b's live range crosses it.
      let
        body = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BVar "b" ]
                , result: Guarded
                    [ { guard: Ret (CApp (var "g") []), rhs: Ret (CAtom (int 1)) }
                    , { guard: Ret (CAtom (AtomLit (LBool true))), rhs: Ret (CAtom (var "b")) }
                    ]
                }
              ]
          )
        plan = activationPlan (cfg0 { params = [ "s" ] }) body
      Set.member "b" plan.crossing `shouldEqual` true

    it "the post-guard force is a safepoint: a variable guard with no internal safepoint still crosses" do
      -- case s of b | q -> 1 | true -> b — clause 1's guard expression (`Ret (CAtom q)`) has NO
      -- safepoint of its own, but the emitter forces the guard VALUE before testing it; `b`
      -- (used only in clause 2) crosses that force. The literal-true guard of clause 2 is the
      -- exemption contrast (its force cannot fire).
      let
        body = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BVar "b" ]
                , result: Guarded
                    [ { guard: Ret (CAtom (var "q")), rhs: Ret (CAtom (int 1)) }
                    , { guard: Ret (CAtom (AtomLit (LBool true))), rhs: Ret (CAtom (var "b")) }
                    ]
                }
              ]
          )
        plan = activationPlan (cfg0 { params = [ "s", "q" ] }) body
      -- (q, the force's own operand, still lands in `crossing` via the CCase level's
      -- everything-live-at-arm-entry conservatism — the arm-INTERNAL discrimination this test
      -- pins is b's.)
      Set.member "b" plan.crossing `shouldEqual` true

    it "a case-arm binder crosses a safepoint inside its arm; consumed-at-call it does not" do
      -- case s of Just b -> let z = h () in b + z — b is live across the h call.
      let
        arm rhs = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BCtor "Just" [ BVar "b" ] ], result: Uncond rhs } ]
          )
        crossing6 = arm
          ( Let "z" (CApp (var "h") [])
              (Ret (CPrim AddInt [ var "b", var "z" ]))
          )
        -- case s of Just b -> f b — b consumed by the call (§1 boundary): no crossing.
        consumed = arm (Ret (CApp (var "f") [ var "b" ]))
        planC = activationPlan (cfg0 { params = [ "s" ] }) crossing6
        planN = activationPlan (cfg0 { params = [ "s" ] }) consumed
      Set.member "b" planC.crossing `shouldEqual` true
      Set.member "b" planN.crossing `shouldEqual` false
      planC.loweringMayRoot `shouldEqual` true

    it "a LetRec group's captures are consumed at construction; later-live names cross it" do
      -- letrec go = \x -> go free in let z = h () in go z
      let
        body = LetRec [ { var: "go", rhs: Ret (CLam [ "x" ] (Ret (CApp (var "go") [ var "free" ]))) } ]
          ( Let "z" (CApp (var "h") [])
              (Ret (CApp (var "go") [ var "z" ]))
          )
        plan = activationPlan (cfg0 { params = [ "free" ] }) body
      -- `go` is live after the group's construction safepoints AND across the `h` call.
      Set.member "go" plan.crossing `shouldEqual` true
      plan.loweringMayRoot `shouldEqual` true

  describe "activationPlan (frame decision)" do
    it "a leaf body with no roots on either tier elides the frame" do
      let
        body = Let "a" (CPrim AddInt [ var "x", int 1 ])
          (Ret (CPrim MulInt [ var "a", int 2 ]))
      needsFrame (activationPlan (cfg0 { params = [ "x" ] }) body) `shouldEqual` false

    it "a lowering-local root (the CUpdate accumulator) forces a frame even with no crossings" do
      let
        body = Ret (CUpdate (var "r") [ { prop: "a", val: int 1 } ])
        plan = activationPlan (cfg0 { params = [ "r" ] }) body
      Set.toUnfoldable plan.crossing `shouldEqual` ([] :: Array String)
      needsFrame plan `shouldEqual` true

  describe "§4 ABI-profile independence" do
    it "the plan is a pure function of (config, body) — no ABI-profile input exists" do
      -- The release/debug RootPlan equality holds by construction: `activationPlan`'s signature
      -- admits no mode. Determinism is the residual property worth pinning.
      let
        body = Let "x" (CApp (var "g") [])
          ( Let "y" (CApp (var "h") [])
              (Ret (CPrim AddInt [ var "x", var "y" ]))
          )
        p1 = activationPlan cfg0 body
        p2 = activationPlan cfg0 body
      Set.toUnfoldable p1.crossing `shouldEqual` (Set.toUnfoldable p2.crossing :: Array String)
      needsFrame p1 `shouldEqual` needsFrame p2

  describe "operandsMayRoot (the evalAtoms suffix-scan mirror)" do
    it "CRecord analyses operands in the emitter's sorted-field order, not source order" do
      -- The emitter sorts fields by unsigned label id BEFORE evalAtoms (ADR-0069 §1); a source
      -- order placing the allocating operand first can invert the suffix scan. Construct the
      -- counterexample deterministically off `sortRecordFields` itself: the var on the
      -- sorted-FIRST label, the boxed string on the sorted-SECOND, fields given in the
      -- OPPOSITE source order (naive source-order analysis would say false).
      case map fst (sortRecordFields [ Tuple "a" unit, Tuple "value" unit ]) of
        [ l1, l2 ] -> do
          let fields = [ { prop: l2, val: AtomLit (LString "s") }, { prop: l1, val: var "x" } ]
          operandsMayRoot false (map _.val fields) `shouldEqual` false -- the naive source order
          cexprMayRootLocally (CRecord fields) `shouldEqual` true -- the canonical order
        other -> fail ("sortRecordFields returned " <> show (Array.length other) <> " labels")

    it "is linear: 20k non-immediate no-safepoint operands decide instantly" do
      -- unforced vars can never safepoint → false; the per-index suffix re-scan this pins
      -- against was O(n²) here (never exercised by all-immediate fixtures).
      operandsMayRoot false (Array.replicate 20000 (var "x")) `shouldEqual` false

    it "roots only when a later operand can safepoint" do
      -- [x, 1]: nothing after x can safepoint → no rooting; [x, y] forced: y forces → x rooted.
      operandsMayRoot true [ var "x", int 1 ] `shouldEqual` false
      operandsMayRoot true [ var "x", var "y" ] `shouldEqual` true
      -- unforced vars never safepoint as operands → no rooting even with two vars
      operandsMayRoot false [ var "x", var "y" ] `shouldEqual` false
      -- a later boxed literal allocates even unforced
      operandsMayRoot false [ var "x", AtomLit (LString "s") ] `shouldEqual` true
      -- immediates are never rooted
      operandsMayRoot true [ int 1, var "x" ] `shouldEqual` false

  describe "primOpSafepoint (the §1 table, prim rows)" do
    it "classifies the allocating vs non-allocating prim families" do
      primOpSafepoint Append `shouldEqual` true
      primOpSafepoint NewArray `shouldEqual` true
      primOpSafepoint AddNumber `shouldEqual` true
      primOpSafepoint RecordSet `shouldEqual` true
      primOpSafepoint SetArray `shouldEqual` false
      primOpSafepoint AddInt `shouldEqual` false
      primOpSafepoint EqString `shouldEqual` false
      primOpSafepoint RecordGet `shouldEqual` false
      primOpSafepoint NumberToInt `shouldEqual` false

  describe "cexprMayRootLocally (the lowering-tier declarations)" do
    it "declares the recipes that root temporaries" do
      cexprMayRootLocally (CUpdate (var "r") []) `shouldEqual` true
      cexprMayRootLocally (CCase [ var "s" ] []) `shouldEqual` true
      cexprMayRootLocally (CApp (var "f") []) `shouldEqual` true
      cexprMayRootLocally (CAtom (var "x")) `shouldEqual` false
      cexprMayRootLocally (CAccessor (var "d") "f") `shouldEqual` false
      -- ctor: saturated with no later-safepoint operands needs no operand roots
      cexprMayRootLocally (CCtor "T" 2 [ var "a", int 1 ]) `shouldEqual` false
      -- unsaturated with supplied fields roots the builder
      cexprMayRootLocally (CCtor "T" 2 [ var "a" ]) `shouldEqual` true

  describe "§2a scale (default stack)" do
    it "walks a 50k-binding Let spine on the default stack" do
      -- let a0 = g () in let a1 = a0 + 1 in … let aN = a(N-1) + 1 in aN
      -- (built innermost-out with a stack-safe fold — the TEST must not recurse either)
      let
        n = 50000
        spine = Array.foldl
          (\inner i -> Let ("a" <> show i) (CPrim AddInt [ var ("a" <> show (i - 1)), int 1 ]) inner)
          (Ret (CAtom (var ("a" <> show n))))
          (Array.reverse (Array.range 1 n))
        body = Let "a0" (CApp (var "g") []) spine
        plan = activationPlan cfg0 body
      -- a0 is consumed by a1's prim immediately; nothing crosses the initial call.
      Set.member "a0" plan.crossing `shouldEqual` false
      plan.anySafepoint `shouldEqual` true

    it "walks a 20k-operand array literal and a 5k-arm case on the default stack" do
      let
        wideArr = Ret (CArray (Array.replicate 20000 (int 1)))
        wideCase = Ret
          ( CCase [ var "s" ]
              ( Array.range 1 5000 <#> \i ->
                  { binders: [ BLit (LInt i) ]
                  , result: Uncond (Ret (CAtom (int i)))
                  }
              )
          )
      (activationPlan cfg0 wideArr).anySafepoint `shouldEqual` true
      (activationPlan (cfg0 { params = [ "s" ] }) wideCase).loweringMayRoot `shouldEqual` true
