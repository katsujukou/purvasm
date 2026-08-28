-- | The ADR-0105 §2 liveness analysis, pinned on the edge classes the ADR names under the
-- | slice-2b-1 crossing rule (§6.3: `liveAfter(N) ∪ preReadHazardOperands(N)` — an operand
-- | read with no earlier intra-node safepoint hands over safely on the §6.1 contract, and the
-- | §6.2 token net holds every claim against the real emission): branch behaviour,
-- | closure-capture escape, the self-recursion `%env` word, case/dtree conservatism, the
-- | per-recipe preRead fixture matrix, frame elision, and the §2a default-stack scale
-- | fixtures. The §4 release/debug `RootPlan`-equality contract holds by
-- | construction — `activationPlan` takes no ABI-profile input — and the test documents that by
-- | asserting the plan is a pure function of (config, body) alone.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Liveness where

import Prelude

import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignClosureMode(..))

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Purvasm.Compiler.Backend.LLVM.ByNeed (activationFacts, elidesForcedValue, noFacts)
import Purvasm.Compiler.Backend.LLVM.Liveness (ActivationConfig, activationPlan, activationPlanWith, cexprMayRootLocally, envPseudo, needsFrame, operandsMayRoot, primOpSafepoint)
import Purvasm.Compiler.Backend.LLVM.Mangle (sortRecordFields)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr, CExprF(..), Expr, ExprF(..), Rhs, RhsF(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

str :: String -> Atom
str = AtomLit <<< LString

cfg0 :: ActivationConfig
cfg0 = { params: [], captures: [], selfName: Nothing, foreignClosure: Hoisted }

crossingOf :: ActivationConfig -> Expr -> Array String
crossingOf cfg body = Set.toUnfoldable (activationPlan cfg body).crossing

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Liveness" do
  describe "activationPlan (crossing)" do
    it "the fib class stays direct (2b-1: no pre-read hazard, handed over safely)" do
      -- let a = n - 1 in f a — `a` is read at call position 1 with no earlier materialisation
      -- (§6.3 prefix), `n` at prim position 0: neither has a pre-read hazard nor a later use,
      -- so both stay direct — sidenote 0011's fib case, recovered on the §6.1 handover
      -- contract and held by the §6.2 token net.
      let
        body = Let "a" (CPrim SubInt [ var "n", int 1 ])
          (Ret (CApp unit (var "f") [ var "a" ]))
      crossingOf (cfg0 { params = [ "n" ] }) body `shouldEqual` []

    it "a value used after an intervening call crosses" do
      -- let x = g () in let y = h () in x + y — `x` crosses via liveAfter (live across the
      -- `h` call); `y` is hazarded by `x`'s earlier force in the prim's operand order (§6.3).
      let
        body = Let "x" (CApp unit (var "g") [])
          ( Let "y" (CApp unit (var "h") [])
              (Ret (CPrim AddInt [ var "x", var "y" ]))
          )
      crossingOf cfg0 body `shouldEqual` [ "x", "y" ]

    -- ADR-0109 slice A / §4: a foreign REFERENCE is a rooted read, not an allocation. Before the
    -- slice it built the leaf closure (`pv_make_closure`, a safepoint), so an earlier operand had to
    -- be rooted across it; now nothing between the two operands can move a value.
    --
    -- This is the seam counterfactual in test form: the arm reads `rootedReadSafepoint`, so flipping
    -- that row moves the plan, and the CONTRAST below shows the fixture is discriminating — the same
    -- shape with a still-allocating atom (a boxed string literal) DOES cross.
    it "a foreign reference no longer hazards an earlier operand; a boxed literal still does" do
      -- the §6.3 order: an ALLOCATING EARLIER operand hazards a later var.
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CApp unit (var "f") [ AtomForeign "M.leaf", var "x" ])) `shouldEqual` []
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CApp unit (var "f") [ str "s", var "x" ])) `shouldEqual` [ "x" ]

    -- …and under the §5.2 counterfactual the plan moves BACK, because the emitter does: `PerUse`
    -- builds the closure at the reference, so the same list roots `x` again. This is the
    -- plan-and-emitter co-switch in one assertion — a mode that reached only the emitter would
    -- leave this leg under-rooted, which is a GC bug, not a slower measurement.
    it "PerUse restores the hazard, because the reference allocates again (§5.2 co-switch)" do
      let perUse = cfg0 { params = [ "x" ], foreignClosure = PerUse }
      crossingOf perUse (Ret (CApp unit (var "f") [ AtomForeign "M.leaf", var "x" ])) `shouldEqual` [ "x" ]
      crossingOf perUse (Ret (CApp unit (var "f") [ var "x", AtomForeign "M.leaf" ])) `shouldEqual` []

    it "consumed-at-call is direct; a later use crosses via liveAfter (2b-1)" do
      let
        mk after = Let "x" (CApp unit (var "g") [])
          ( Let "r" (CApp unit (var "f") [ var "x" ])
              (Ret after)
          )
      -- x consumed by the f-call only: no pre-read hazard, no later use — direct (2b-1).
      crossingOf cfg0 (mk (CAtom (var "r"))) `shouldEqual` []
      -- x used after the f-call crosses via liveAfter; r is hazarded by x's earlier force in
      -- the prim's operand order (§6.3 prefix).
      crossingOf cfg0 (mk (CPrim AddInt [ var "x", var "r" ])) `shouldEqual` [ "r", "x" ]

    it "a param live at a branch entry crosses the condition force; the condition itself is direct (2b-1)" do
      -- if c then f x else 1 — the cond force precedes both branches; `c` itself is read by
      -- the safepointing condition node.
      let
        body = Ret
          ( CIf (var "c")
              (Ret (CApp unit (var "f") [ var "x" ]))
              (Ret (CAtom (int 1)))
          )
      -- the condition's own read precedes its force (§6.3): `c` no longer crosses.
      crossingOf (cfg0 { params = [ "c", "x" ] }) body `shouldEqual` [ "x" ]

    it "only names never read at (or live across) a safepoint stay direct" do
      -- let a = 1 + 2 in a — the all-immediate prim cannot safepoint, and the identity tail
      -- reads `a` at a non-safepoint node: nothing crosses, the frame is elided. This is the
      -- pure-leaf choreography 2a keeps (the `CAtom` identity class).
      let
        body = Let "a" (CPrim AddInt [ int 1, int 2 ])
          (Ret (CAtom (var "a")))
        plan = activationPlan cfg0 body
      Set.toUnfoldable plan.crossing `shouldEqual` ([] :: Array String)
      plan.anySafepoint `shouldEqual` false
      needsFrame plan `shouldEqual` false
      -- contrast: with TWO var operands, the second is hazarded by the first's force —
      -- position decides under §6.3, not mere presence at a safepoint node.
      let
        forced = Let "a" (CPrim AddInt [ var "x", var "y" ])
          (Ret (CAtom (var "a")))
        plan' = activationPlan (cfg0 { params = [ "x", "y" ] }) forced
      Set.toUnfoldable plan'.crossing `shouldEqual` [ "y" ]
      needsFrame plan' `shouldEqual` true

    it "closure captures are read before the first allocation and stay direct (2b-1)" do
      -- let g = \p -> p + cap in g — every capture read fills the arg buffer BEFORE
      -- pv_new_array/pv_make_closure allocate (§6.3), so cap has no pre-read hazard.
      let
        body = Let "g" (CLam unit [ "p" ] (Ret (CPrim AddInt [ var "p", var "cap" ])))
          (Ret (CAtom (var "g")))
      -- §6.3: every capture read precedes makeClosure's first allocation — no hazard.
      crossingOf (cfg0 { params = [ "cap" ] }) body `shouldEqual` []

    it "the closure value crosses a later safepoint; its captures stay direct (2b-1)" do
      -- let g = \p -> cap in let z = h () in g z
      let
        body = Let "g" (CLam unit [ "p" ] (Ret (CAtom (var "cap"))))
          ( Let "z" (CApp unit (var "h") [])
              (Ret (CApp unit (var "g") [ var "z" ]))
          )
      -- g lives across the h-call (liveAfter); cap and z have no hazard and no later use.
      crossingOf (cfg0 { params = [ "cap" ] }) body `shouldEqual` [ "g" ]

    it "a nested lambda body's own crossings do NOT leak into this activation (closure opacity)" do
      -- \p -> let q = f () in let s = h () in q + s — everything crossing is inside the lambda.
      let
        lamBody = Let "q" (CApp unit (var "f") [])
          ( Let "s" (CApp unit (var "h") [])
              (Ret (CPrim AddInt [ var "q", var "s" ]))
          )
        body = Ret (CLam unit [ "p" ] lamBody)
      crossingOf cfg0 body `shouldEqual` []

    it "the self %env word crosses when a self-call sits after a safepoint" do
      -- if c then loop m else n — the cond force precedes the self-call, which reads %env.
      let
        body = Ret
          ( CIf (var "c")
              (Ret (CApp unit (var "loop") [ var "m" ]))
              (Ret (CAtom (var "n")))
          )
        plan = activationPlan { params: [ "c", "m", "n" ], captures: [], selfName: Just "loop", foreignClosure: Hoisted } body
      Set.member envPseudo plan.crossing `shouldEqual` true

    it "%env crosses a self-call only when an argument's materialisation precedes its read (2b-1)" do
      -- The `SSelf` lowering evaluates the arguments (UNFORCED — only a materialisation can
      -- safepoint there) BEFORE reading the env word, so `%env` is hazarded exactly when some
      -- argument materialises (§6.3's SSelf row).
      let
        body = Ret (CApp unit (var "loop") [ var "m" ])
        plan = activationPlan { params: [ "m" ], captures: [], selfName: Just "loop", foreignClosure: Hoisted } body
      -- §6.3: a var argument cannot materialise, so the post-argument %env read has no
      -- pre-read hazard — recovered under 2b-1…
      Set.member envPseudo plan.crossing `shouldEqual` false
      -- …but an argument whose materialisation allocates (a string literal) hazards it.
      let
        body' = Ret (CApp unit (var "loop") [ str "s" ])
        plan' = activationPlan { params: [], captures: [], selfName: Just "loop", foreignClosure: Hoisted } body'
      Set.member envPseudo plan'.crossing `shouldEqual` true

    it "guard clauses are sequential: a binder used only after a failing guard crosses its safepoints" do
      -- case s of b | g () -> 1 | true -> b — `b` is read only in clause 2, but clause 1's
      -- guard (a call) runs first and may fall through: b's live range crosses it.
      let
        body = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BVar "b" ]
                , result: Guarded
                    [ { guard: Ret (CApp unit (var "g") []), rhs: Ret (CAtom (int 1)) }
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

    -- ADR-0107 §2: the guard-result force is classified through the SAME decision set the emitter
    -- elides with. A guard whose RESULT the lattice proves (`CPrim EqInt`) emits no chain, so it
    -- must not be a safepoint here either — a term-only classifier would keep rooting across a
    -- force that no longer exists (conservative, but a second derivation, and the accounting would
    -- charge the crossing to a chain that was never emitted).
    it "a PROVEN guard result is no longer a post-guard safepoint (the shared decision)" do
      let
        -- The sibling of the test above, with the guard's RESULT provable: `1 == 2` is a
        -- scalar-primitive result over immediates, so it has no internal safepoint of its own and
        -- the lattice proves the value the force would receive. `b` is the discriminator (an
        -- arm-bound name used only in clause 2 — a name live at CASE entry would cross anyway
        -- under the dtree's arm-entry conservatism, which is what makes `b` the right probe).
        guarded g = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BVar "b" ]
                , result: Guarded
                    [ { guard: g, rhs: Ret (CAtom (int 1)) }
                    , { guard: Ret (CAtom (AtomLit (LBool true))), rhs: Ret (CAtom (var "b")) }
                    ]
                }
              ]
          )
        provenBody = guarded (Ret (CPrim EqInt [ int 1, int 2 ]))
        opaqueBody = guarded (Ret (CAtom (var "q")))
        cfg = cfg0 { params = [ "s", "q" ] }
      -- with the lattice OFF the post-guard force is a safepoint, so `b` crosses it …
      Set.member "b" (activationPlanWith { byNeed: false } cfg provenBody).crossing `shouldEqual` true
      -- … with it ON the emitter emits no chain there, so the analysis must not root across one;
      -- the unprovable guard (a bare variable — `May`) still makes `b` cross.
      Set.member "b" (activationPlanWith { byNeed: true } cfg provenBody).crossing `shouldEqual` false
      Set.member "b" (activationPlanWith { byNeed: true } cfg opaqueBody).crossing `shouldEqual` true
      -- and the plan PUBLISHES that decision set, so the emitter reads the same one rather than
      -- computing its own (`FactMap` is opaque, so this is checked through the decision).
      let published = (activationPlanWith { byNeed: true } cfg provenBody).byNeed
      elidesForcedValue published (Ret (CPrim EqInt [ int 1, int 2 ])) `shouldEqual` true
      elidesForcedValue (activationFacts [ "s", "q" ] provenBody) (Ret (CPrim EqInt [ int 1, int 2 ]))
        `shouldEqual` true
      -- the OFF plan publishes the disabled set: nothing decides, so nothing elides.
      elidesForcedValue (activationPlanWith { byNeed: false } cfg provenBody).byNeed
        (Ret (CPrim EqInt [ int 1, int 2 ])) `shouldEqual` false

    it "a case-arm binder crosses a safepoint inside its arm; consumed-at-call stays direct (2b-1)" do
      -- case s of Just b -> let z = h () in b + z — b is live across the h call.
      let
        arm rhs = Ret
          ( CCase [ var "s" ]
              [ { binders: [ BCtor "Just" [ BVar "b" ] ], result: Uncond rhs } ]
          )
        crossing6 = arm
          ( Let "z" (CApp unit (var "h") [])
              (Ret (CPrim AddInt [ var "b", var "z" ]))
          )
        -- case s of Just b -> f b — b consumed at the call with no pre-read hazard: direct
        -- under 2b-1 (the handover leg is the §6.1 contract's).
        consumed = arm (Ret (CApp unit (var "f") [ var "b" ]))
        planC = activationPlan (cfg0 { params = [ "s" ] }) crossing6
        planN = activationPlan (cfg0 { params = [ "s" ] }) consumed
      Set.member "b" planC.crossing `shouldEqual` true
      Set.member "b" planN.crossing `shouldEqual` false
      planC.loweringMayRoot `shouldEqual` true

    it "a LetRec group's captures are consumed at construction; later-live names cross it" do
      -- letrec go = \x -> go free in let z = h () in go z
      let
        body = LetRec [ { var: "go", rhs: Ret (CLam unit [ "x" ] (Ret (CApp unit (var "go") [ var "free" ]))) } ]
          ( Let "z" (CApp unit (var "h") [])
              (Ret (CApp unit (var "go") [ var "z" ]))
          )
        plan = activationPlan (cfg0 { params = [ "free" ] }) body
      -- `go` is live after the group's construction safepoints AND across the `h` call.
      Set.member "go" plan.crossing `shouldEqual` true
      plan.loweringMayRoot `shouldEqual` true

  describe "the §6.3 preRead table (per-recipe fixture matrix, 2b-1)" do
    it "generic CApp: an allocating earlier operand hazards a later var; the reverse is direct" do
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CApp unit (var "f") [ str "s", var "x" ])) `shouldEqual` [ "x" ]
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CApp unit (var "f") [ var "x", str "s" ])) `shouldEqual` []

    it "saturated CCtor: the prefix rule in both directions" do
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CCtor "C" 2 [ str "s", var "x" ])) `shouldEqual` [ "x" ]
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CCtor "C" 2 [ var "x", str "s" ])) `shouldEqual` []

    it "CArray: the prefix rule in both directions" do
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CArray [ str "s", var "x" ])) `shouldEqual` [ "x" ]
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CArray [ var "x", str "s" ])) `shouldEqual` []

    it "unsaturated CCtor: the builder machinery hazards EVERY supplied operand" do
      crossingOf (cfg0 { params = [ "x" ] }) (Ret (CCtor "C" 2 [ var "x" ])) `shouldEqual` [ "x" ]

    it "CRecord hazards follow the CANONICAL sorted order, not source order" do
      let
        ls = map fst (sortRecordFields [ Tuple "a" unit, Tuple "value" unit ])
      case ls of
        [ l1, l2 ] -> do
          -- var on the sorted-SECOND label: the sorted-first string materialises before it.
          crossingOf (cfg0 { params = [ "x" ] })
            (Ret (CRecord [ { prop: l2, val: var "x" }, { prop: l1, val: str "s" } ])) `shouldEqual` [ "x" ]
          -- var on the sorted-FIRST label: read before the allocating field — direct.
          crossingOf (cfg0 { params = [ "x" ] })
            (Ret (CRecord [ { prop: l1, val: var "x" }, { prop: l2, val: str "s" } ])) `shouldEqual` []
        other -> fail ("sortRecordFields returned an unexpected shape: " <> show other)

    it "multiple CCase scrutinees: a later scrutinee is hazarded by an earlier force" do
      let
        plan = activationPlan (cfg0 { params = [ "s1", "s2" ] })
          ( Ret
              ( CCase [ var "s1", var "s2" ]
                  [ { binders: [ BNull, BNull ], result: Uncond (Ret (CAtom (int 1))) } ]
              )
          )
      Set.member "s2" plan.crossing `shouldEqual` true
      Set.member "s1" plan.crossing `shouldEqual` false

    it "CUpdate: a VARIABLE update value is hazarded (read after a record_set); the base is not" do
      let
        plan = activationPlan (cfg0 { params = [ "r", "v" ] })
          (Ret (CUpdate (var "r") [ { prop: "a", val: var "v" } ]))
      Set.member "v" plan.crossing `shouldEqual` true
      Set.member "r" plan.crossing `shouldEqual` false

    it "LetRec captures are read before the group's first allocation and stay direct" do
      let
        plan = activationPlan (cfg0 { params = [ "free" ] })
          ( LetRec [ { var: "go", rhs: Ret (CLam unit [ "x" ] (Ret (CAtom (var "free")))) } ]
              (Ret (CAtom (int 1)))
          )
      Set.member "free" plan.crossing `shouldEqual` false

  describe "activationPlan (frame decision)" do
    it "a leaf body with no roots on either tier elides the frame (the identity class)" do
      -- \x -> x — no safepoint node exists at all, so nothing can cross and no recipe roots:
      -- the pure-leaf choreography elision 2a keeps.
      let
        body = Ret (CAtom (var "x"))
        plan = activationPlan (cfg0 { params = [ "x" ] }) body
      Set.toUnfoldable plan.crossing `shouldEqual` ([] :: Array String)
      needsFrame plan `shouldEqual` false

    it "a lowering-local root forces a frame even with no crossings; CUpdate stays lowering-tier (2b-1)" do
      -- case 1 of 1 -> 2 — no tracked name is read anywhere, but the dtree recipe declares
      -- may-root: the frame exists for the lowering tier alone.
      let
        caseBody = Ret
          ( CCase [ int 1 ]
              [ { binders: [ BLit (LInt 1) ], result: Uncond (Ret (CAtom (int 2))) } ]
          )
        casePlan = activationPlan cfg0 caseBody
      Set.toUnfoldable casePlan.crossing `shouldEqual` ([] :: Array String)
      needsFrame casePlan `shouldEqual` true
      -- CUpdate: the recipe roots its accumulator (lowering tier); the base is read FIRST
      -- (§6.3 — no pre-read hazard) and has no later use, so the activation tier is empty
      -- while the frame stays for the lowering roots.
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
        body = Let "x" (CApp unit (var "g") [])
          ( Let "y" (CApp unit (var "h") [])
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
          operandsMayRoot Hoisted noFacts false (map _.val fields) `shouldEqual` false -- the naive source order
          cexprMayRootLocally Hoisted noFacts (CRecord fields) `shouldEqual` true -- the canonical order
        other -> fail ("sortRecordFields returned " <> show (Array.length other) <> " labels")

    it "is linear: 20k non-immediate no-safepoint operands decide instantly" do
      -- unforced vars can never safepoint → false; the per-index suffix re-scan this pins
      -- against was O(n²) here (never exercised by all-immediate fixtures).
      operandsMayRoot Hoisted noFacts false (Array.replicate 20000 (var "x")) `shouldEqual` false

    it "roots only when a later operand can safepoint" do
      -- [x, 1]: nothing after x can safepoint → no rooting; [x, y] forced: y forces → x rooted.
      operandsMayRoot Hoisted noFacts true [ var "x", int 1 ] `shouldEqual` false
      operandsMayRoot Hoisted noFacts true [ var "x", var "y" ] `shouldEqual` true
      -- unforced vars never safepoint as operands → no rooting even with two vars
      operandsMayRoot Hoisted noFacts false [ var "x", var "y" ] `shouldEqual` false
      -- a later boxed literal allocates even unforced
      operandsMayRoot Hoisted noFacts false [ var "x", AtomLit (LString "s") ] `shouldEqual` true
      -- immediates are never rooted
      operandsMayRoot Hoisted noFacts true [ int 1, var "x" ] `shouldEqual` false

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

  describe "cexprMayRootLocally Hoisted noFacts (the lowering-tier declarations)" do
    it "declares the recipes that root temporaries" do
      cexprMayRootLocally Hoisted noFacts (CUpdate (var "r") []) `shouldEqual` true
      cexprMayRootLocally Hoisted noFacts (CCase [ var "s" ] []) `shouldEqual` true
      cexprMayRootLocally Hoisted noFacts (CApp unit (var "f") []) `shouldEqual` true
      cexprMayRootLocally Hoisted noFacts (CAtom (var "x")) `shouldEqual` false
      cexprMayRootLocally Hoisted noFacts (CAccessor (var "d") "f") `shouldEqual` false
      -- ctor: saturated with no later-safepoint operands needs no operand roots
      cexprMayRootLocally Hoisted noFacts (CCtor "T" 2 [ var "a", int 1 ]) `shouldEqual` false
      -- unsaturated with supplied fields roots the builder
      cexprMayRootLocally Hoisted noFacts (CCtor "T" 2 [ var "a" ]) `shouldEqual` true

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
        body = Let "a0" (CApp unit (var "g") []) spine
        plan = activationPlan cfg0 body
      -- a0 is read at its consuming prim's FIRST position (no pre-read hazard, §6.3) and has
      -- no later use, so it stays direct. The pin here is the walk finishing on the default
      -- stack, not the crossing content.
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
