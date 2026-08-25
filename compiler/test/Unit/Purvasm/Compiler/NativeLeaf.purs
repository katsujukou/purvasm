-- | Native-leaf recognition ([ADR-0110](../../../../../docs/design-decisions/0110-owned-vm-purescript-native.md)
-- | §4(a)). Two backends now lower a leaf, and both ask this module the same three questions, so a
-- | wrong answer is wrong in the native binary AND in the bytecode image — while the *symptoms*
-- | differ enough (a link error there, a stuck run here) to look like unrelated bugs.
-- |
-- | The invariants worth pinning are the ones the types cannot: which keys count as leaves at all
-- | (a resolver rung decides, not a naming convention), and that a nullary `Effect` leaf's physical
-- | arity is 1 rather than its semantic 0.
module Test.Unit.Purvasm.Compiler.NativeLeaf (spec) where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.NativeLeaf (leafClosureArity, nativeLeafArities, resolveNativeForeigns)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | An FSR shape: `arity` arrows, `retVsat` when the result is a saturated `Effect`/`ST` action.
shape :: Int -> Boolean -> { arity :: Int, vsat :: Boolean, retVsat :: Boolean }
shape arity retVsat = { arity, vsat: false, retVsat }

spec :: Spec Unit
spec = describe "Purvasm.Compiler.NativeLeaf" do
  describe "leafClosureArity" do
    it "keeps a pure leaf's arrow count" do
      leafClosureArity (shape 2 false) `shouldEqual` 2

    it "gives a nullary Effect leaf arity 1, because it IS the effect thunk" do
      -- `argvImpl :: Effect (Array String)` reconstructs to `arity 0, retVsat`. Built at arity 0, the
      -- run's unit application over-applies an already-fired leaf onto its own result — the "not
      -- callable (kind Array)" fault. This is the single most load-bearing line in the module.
      leafClosureArity (shape 0 true) `shouldEqual` 1

    it "leaves an Effect leaf that takes arguments at its argument count" do
      -- Saturating it returns a *fresh* thunk, so the unit-run is applied to that, not to the leaf.
      leafClosureArity (shape 1 true) `shouldEqual` 1
      leafClosureArity (shape 3 true) `shouldEqual` 3

    it "answers 0 for a foreign constant, which is a leaf like any other" do
      leafClosureArity (shape 0 false) `shouldEqual` 0

  describe "nativeLeafArities" do
    it "keeps the keys no resolver rung answers" do
      nativeLeafArities (Map.fromFoldable [ "Data.Show.showIntImpl" /\ shape 1 false ])
        `shouldEqual` Map.fromFoldable [ "Data.Show.showIntImpl" /\ 1 ]

    it "drops an intrinsic, which is materialised as a definition instead" do
      -- `unsafeCoerce` is resolved by the compiler itself; lowering it to a host reference would ask
      -- the runtime for a `pvf_` symbol nothing provides.
      nativeLeafArities (Map.fromFoldable [ "Unsafe.Coerce.unsafeCoerce" /\ shape 1 false ])
        `shouldEqual` Map.empty

    it "drops a structural higher-order foreign for the same reason" do
      -- `Effect.bindE` is a guest term the FFI ladder substitutes — a definition, not a host leaf.
      nativeLeafArities (Map.fromFoldable [ "Effect.bindE" /\ shape 2 false ])
        `shouldEqual` Map.empty

    it "applies the physical arity, not the shape's raw one" do
      -- The correction has to survive the map, or every caller re-derives it (and one will forget).
      nativeLeafArities (Map.fromFoldable [ "Purvasm.System.Process.argvImpl" /\ shape 0 true ])
        `shouldEqual` Map.fromFoldable [ "Purvasm.System.Process.argvImpl" /\ 1 ]

  describe "resolveNativeForeigns" do
    it "rewrites a leaf reference to the foreign spelling" do
      resolveNativeForeigns (Map.fromFoldable [ "M.leaf" /\ 1 ]) (Ret (CAtom (AtomVar "M.leaf")))
        `shouldEqual` Ret (CAtom (AtomForeign "M.leaf"))

    it "leaves an ordinary global alone" do
      -- The map is the whole authority: a qualified name is not evidence of anything by itself.
      resolveNativeForeigns (Map.fromFoldable [ "M.leaf" /\ 1 ]) (Ret (CAtom (AtomVar "M.other")))
        `shouldEqual` Ret (CAtom (AtomVar "M.other"))

    it "reaches a reference nested inside a binding's right-hand side" do
      -- A leaf usually appears as a call operand, not as a whole body; a rewrite that only looked at
      -- the tail would leave those as unbound loads and fail only at run time.
      resolveNativeForeigns (Map.fromFoldable [ "M.leaf" /\ 1 ])
        (Let "x" (CApp (AtomVar "M.leaf") [ AtomVar "y" ]) (Ret (CAtom (AtomVar "x"))))
        `shouldEqual`
          Let "x" (CApp (AtomForeign "M.leaf") [ AtomVar "y" ]) (Ret (CAtom (AtomVar "x")))

    it "does not touch a local variable that shadows nothing" do
      resolveNativeForeigns Map.empty (Ret (CAtom (AtomVar "x")))
        `shouldEqual` Ret (CAtom (AtomVar "x"))
