-- | The free-variable analysis decides each lifted lambda's captures (ADR-0072 §4) and `cfExpr` the
-- | native foreign keys to compile/link (ADR-0073 §3); both must respect scope. Sets are compared as
-- | their sorted element arrays.
module Test.Unit.Purvasm.Compiler.MiddleEnd.ANF.FreeVars where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (binderVars, cfExpr, fvCexpr, fvExpr)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

elems :: Set String -> Array String
elems = Set.toUnfoldable

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.ANF.FreeVars" do
  describe "binderVars" do
    it "binds nothing for wildcards and literals" do
      elems (binderVars BNull) `shouldEqual` []
      elems (binderVars (BLit (LInt 0))) `shouldEqual` []

    it "collects variable, as-pattern, and nested constructor binders" do
      elems (binderVars (BVar "x")) `shouldEqual` [ "x" ]
      elems (binderVars (BNamed "y" (BVar "x"))) `shouldEqual` [ "x", "y" ]
      elems (binderVars (BCtor "Just" [ BVar "a", BCtor "Tuple" [ BVar "b", BNull ] ]))
        `shouldEqual` [ "a", "b" ]

  describe "fvExpr / fvCexpr" do
    it "reports a bare variable as free" do
      elems (fvExpr Set.empty (Ret (CAtom (AtomVar "x")))) `shouldEqual` [ "x" ]

    it "treats a lambda's parameters as bound in its body" do
      elems (fvCexpr Set.empty (CLam [ "x" ] (Ret (CAtom (AtomVar "x"))))) `shouldEqual` []

    it "removes let-bound names but keeps the genuinely free ones" do
      -- let a = 0 in f a b  →  free { f, b } (a is let-bound)
      let
        e = Let "a" (CAtom (AtomLit (LInt 0)))
          (Ret (CApp (AtomVar "f") [ AtomVar "a", AtomVar "b" ]))
      elems (fvExpr Set.empty e) `shouldEqual` [ "b", "f" ]

    it "unions the free variables of both if-branches" do
      let e = Ret (CIf (AtomVar "c") (Ret (CAtom (AtomVar "t"))) (Ret (CAtom (AtomVar "e"))))
      elems (fvExpr Set.empty e) `shouldEqual` [ "c", "e", "t" ]

  describe "cfExpr" do
    it "collects every referenced foreign key and ignores vars/literals" do
      let
        e = Ret (CApp (AtomForeign "Data.X.y") [ AtomVar "z", AtomForeign "Data.X.w" ])
      elems (cfExpr e) `shouldEqual` [ "Data.X.w", "Data.X.y" ]
