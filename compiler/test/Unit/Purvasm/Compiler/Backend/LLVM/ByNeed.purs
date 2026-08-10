-- | The ADR-0107 lattice's type-unenforceable invariants. Three are load-bearing for SOUNDNESS —
-- | a wrong `NeverByNeed` makes the emitter read a by-need cell as a value, which no type and no
-- | ADR-0105 token catches — and each is tested by construction rather than by example:
-- |
-- | * **`May` totality** — every `CExpr` constructor that is not an explicit `Never` producer must
-- |   read `May`. The test enumerates a representative of EVERY constructor, so a new ANF node
-- |   added without a lattice arm lands on the safe answer and this test says so.
-- | * **Shadow poisoning** — the decision set is keyed by NAME, so a name bound at more than one
-- |   occurrence in an activation must be `May` everywhere, whatever its right-hand sides. This is
-- |   what preserves ADR-0107 §2's occurrence-identity property under a name-keyed map.
-- | * **Activation boundaries** — a `CLam` body and a `LetRec` member are separate activations, so
-- |   an outer `Never` must not reach inside one (it would arrive through `%env` or a cell there).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.ByNeed where

import Prelude

import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.ByNeed (ByNeedFact(..), activationFacts, elidesForce, elidesForcedValue, factOfAtom, factOfExpr, meet, noFacts)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

int :: Int -> Atom
int = AtomLit <<< LInt

noParams :: { params :: Array String, captures :: Array String }
noParams = { params: [], captures: [] }

-- | The fact of `x` in the activation `body` (with parameters `ps`).
factIn :: Array String -> Expr -> String -> ByNeedFact
factIn ps body x = factOfAtom (activationFacts ps body) (var x)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.ByNeed" do
  describe "meet" do
    it "is Never only when both sides are" do
      meet NeverByNeed NeverByNeed `shouldEqual` NeverByNeed
      meet NeverByNeed MayBeByNeed `shouldEqual` MayBeByNeed
      meet MayBeByNeed NeverByNeed `shouldEqual` MayBeByNeed
      meet MayBeByNeed MayBeByNeed `shouldEqual` MayBeByNeed

    it "is commutative, associative and idempotent (a meet-semilattice)" do
      let facts = [ NeverByNeed, MayBeByNeed ]
      for_ facts \a -> do
        meet a a `shouldEqual` a
        for_ facts \b -> do
          meet a b `shouldEqual` meet b a
          for_ facts \c -> meet a (meet b c) `shouldEqual` meet (meet a b) c

  describe "the fact of a binding" do
    it "proves the §1 Never producers" do
      let
        producers =
          [ Tuple "literal" (CAtom (int 1))
          , Tuple "saturated ctor" (CCtor "Just" 1 [ var "p" ])
          , Tuple "array" (CArray [ var "p" ])
          , Tuple "record" (CRecord [ { prop: "a", val: var "p" } ])
          , Tuple "lambda" (CLam [ "u" ] (Ret (CAtom (var "u"))))
          , Tuple "scalar prim" (CPrim AddInt [ var "p", int 1 ])
          ]
      for_ producers \(Tuple label c) -> do
        let body = Let "x" c (Ret (CAtom (var "x")))
        { label, fact: factIn [ "p" ] body "x" } `shouldEqual` { label, fact: NeverByNeed }

    -- The catch-all is the safety property: everything that is not an explicit producer reads
    -- `May`, including values that look innocuous — a projection reads a container slot, and
    -- containers hold cells (ADR-0070's by-need dictionary members).
    it "falls to May over every non-producer constructor (totality)" do
      let
        nonProducers =
          [ Tuple "CApp" (CApp (var "f") [ var "p" ])
          , Tuple "CPerform" (CPerform (var "t"))
          , Tuple "CAccessor" (CAccessor (var "p") "field")
          , Tuple "CUpdate" (CUpdate (var "p") [ { prop: "a", val: int 1 } ])
          , Tuple "CPrim RecordGet" (CPrim RecordGet [ var "p", int 0 ])
          , Tuple "CPrim IndexArray" (CPrim IndexArray [ var "p", int 0 ])
          , Tuple "CAtom foreign" (CAtom (AtomForeign "M.leaf"))
          , Tuple "CAtom global" (CAtom (var "M.global"))
          , Tuple "CCase (no alts)" (CCase [ var "p" ] [])
          ]
      for_ nonProducers \(Tuple label c) -> do
        let body = Let "x" c (Ret (CAtom (var "x")))
        { label, fact: factIn [ "p" ] body "x" } `shouldEqual` { label, fact: MayBeByNeed }

    it "keeps params, captures, pattern binders and LetRec members May" do
      factIn [ "p" ] (Ret (CAtom (var "p"))) "p" `shouldEqual` MayBeByNeed
      factOfAtom (activationFacts [] (Ret (CAtom (var "c")))) (var "c")
        `shouldEqual` MayBeByNeed
      let
        alt = { binders: [ BVar "y" ], result: Uncond (Ret (CAtom (var "y"))) }
        cased = Ret (CCase [ var "p" ] [ alt ])
      factIn [ "p" ] cased "y" `shouldEqual` MayBeByNeed
      factIn [] (LetRec [ { var: "r", rhs: Ret (CAtom (int 1)) } ] (Ret (CAtom (var "r")))) "r"
        `shouldEqual` MayBeByNeed

    it "propagates through an alias" do
      let body = Let "a" (CAtom (int 1)) (Let "b" (CAtom (var "a")) (Ret (CAtom (var "b"))))
      factIn [] body "b" `shouldEqual` NeverByNeed

    it "meets branch results (a Never arm and a May arm give May)" do
      let
        bothNever = Let "x" (CIf (var "p") (Ret (CAtom (int 1))) (Ret (CAtom (int 2)))) (Ret (CAtom (var "x")))
        oneMay = Let "x" (CIf (var "p") (Ret (CAtom (int 1))) (Ret (CAtom (var "p")))) (Ret (CAtom (var "x")))
      factIn [ "p" ] bothNever "x" `shouldEqual` NeverByNeed
      factIn [ "p" ] oneMay "x" `shouldEqual` MayBeByNeed

    it "resolves a binding nested inside a branch before the binding that reads it" do
      -- let x = (if p then (let i = 1 in i) else 2) in x  ⇒ both arms Never ⇒ x is Never. This
      -- only holds if the inner binding is resolved first (the collection order is post-order).
      let
        body = Let "x"
          (CIf (var "p") (Let "i" (CAtom (int 1)) (Ret (CAtom (var "i")))) (Ret (CAtom (int 2))))
          (Ret (CAtom (var "x")))
      factIn [ "p" ] body "x" `shouldEqual` NeverByNeed

  describe "shadow poisoning (ADR-0107 §2 occurrence identity under a name-keyed map)" do
    it "poisons a name bound at more than one occurrence, both bindings" do
      -- let x = 1 in let y = (let x = <call> in x) in x — the OUTER x is provable in isolation,
      -- but the name is rebound, so BOTH occurrences read May and no site elides on `x`.
      let
        inner = Let "x" (CApp (var "f") [ int 0 ]) (Ret (CAtom (var "x")))
        body = Let "x" (CAtom (int 1)) (Let "y" (CIf (var "p") inner (Ret (CAtom (int 0)))) (Ret (CAtom (var "x"))))
      factIn [ "p" ] body "x" `shouldEqual` MayBeByNeed

    it "poisons a Let that shadows a parameter" do
      let body = Let "p" (CAtom (int 1)) (Ret (CAtom (var "p")))
      factIn [ "p" ] body "p" `shouldEqual` MayBeByNeed

    -- The soundness case that makes captures a NON-input: `c` occurs free (it is a capture, a
    -- global, or an enclosing local — the fact set cannot tell, and must not need to) AND is bound
    -- inside the body. A reference BEFORE the inner binding resolves to the free one, which a
    -- name-keyed map cannot distinguish, so the name is poisoned. Deriving this from the body's
    -- free names rather than from a capture LIST is also what lets an out-of-tree walk reproduce
    -- these facts exactly, with no lifting decision to guess at.
    it "poisons a name that occurs FREE and is also bound inside the body" do
      let
        -- if p then <read the free c> else (let c = 1 in <read the inner c>)
        body = Let "r"
          ( CIf (var "p")
              (Ret (CAtom (var "c")))
              (Let "c" (CAtom (int 1)) (Ret (CAtom (var "c"))))
          )
          (Ret (CAtom (var "r")))
      factIn [ "p" ] body "c" `shouldEqual` MayBeByNeed
      -- and the branch meet therefore cannot prove the binding that reads it either
      factIn [ "p" ] body "r" `shouldEqual` MayBeByNeed

    it "leaves a free name that is never re-bound alone (it is simply May, unbound)" do
      let body = Let "x" (CAtom (var "cap")) (Ret (CAtom (var "x")))
      factIn [] body "cap" `shouldEqual` MayBeByNeed
      factIn [] body "x" `shouldEqual` MayBeByNeed

    it "leaves distinct names alone" do
      let body = Let "x" (CAtom (int 1)) (Let "z" (CApp (var "f") [ int 0 ]) (Ret (CAtom (var "x"))))
      factIn [] body "x" `shouldEqual` NeverByNeed
      factIn [] body "z" `shouldEqual` MayBeByNeed

  describe "activation boundaries" do
    it "does not carry an outer Never into a lambda body" do
      -- the lambda's own activation binds only its params; `a` arrives through `%env` ⇒ May.
      let
        inner = Ret (CPrim AddInt [ var "a", var "u" ])
        outer = Let "a" (CAtom (int 1)) (Ret (CLam [ "u" ] inner))
      factIn [] outer "a" `shouldEqual` NeverByNeed
      factOfAtom (activationFacts [ "u" ] inner) (var "a")
        `shouldEqual` MayBeByNeed

    it "does not collect binders from inside a lambda body" do
      -- an inner binder of the same name is a different activation's, so it must not poison the
      -- outer one.
      let
        inner = Let "a" (CApp (var "f") [ int 0 ]) (Ret (CAtom (var "a")))
        outer = Let "a" (CAtom (int 1)) (Let "g" (CLam [ "u" ] inner) (Ret (CAtom (var "a"))))
      factIn [] outer "a" `shouldEqual` NeverByNeed

  describe "the decision" do
    it "elides a force only on a proven-Never variable" do
      let
        facts = activationFacts [ "p" ]
          (Let "a" (CAtom (int 1)) (Ret (CPrim AddInt [ var "a", var "p" ])))
      elidesForce facts (var "a") `shouldEqual` true
      elidesForce facts (var "p") `shouldEqual` false
      -- an unbound name is a global: `May` by pin
      elidesForce facts (var "M.dict") `shouldEqual` false

    it "never claims to elide a non-variable (no chain is emitted for one anyway)" do
      elidesForce noFacts (int 1) `shouldEqual` false
      elidesForce noFacts (AtomLit (LString "s")) `shouldEqual` false
      elidesForce noFacts (AtomForeign "M.leaf") `shouldEqual` false

    it "elides a guard-result force only when the guard's RESULT is proven" do
      let facts = activationFacts [ "p" ] (Ret (CAtom (var "p")))
      elidesForcedValue facts (Ret (CPrim EqInt [ var "p", int 1 ])) `shouldEqual` true
      elidesForcedValue facts (Ret (CAtom (var "p"))) `shouldEqual` false
      elidesForcedValue facts (Ret (CApp (var "f") [ var "p" ])) `shouldEqual` false

    -- The counterfactual is a state of the DECISION SET, not of either consumer: with the set
    -- disabled nothing elides, including the fact-independent producers (a scalar-primitive result
    -- is `Never` whatever the map holds — gating only on the map would leave those still eliding,
    -- which would have made the measurement counterfactual silently wrong).
    it "decides nothing at all under the disabled (counterfactual / un-planned) fact set" do
      elidesForce noFacts (var "anything") `shouldEqual` false
      elidesForcedValue noFacts (Ret (CPrim EqInt [ int 1, int 1 ])) `shouldEqual` false
      elidesForcedValue noFacts (Ret (CCtor "Just" 1 [ int 1 ])) `shouldEqual` false
      factOfExpr noFacts (Ret (CAtom (var "x"))) `shouldEqual` MayBeByNeed
