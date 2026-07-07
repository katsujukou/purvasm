-- | `Simplify` invariants (ADR-0028): the two rewrites must fire exactly where the callee's
-- | shape licenses them and nowhere else — copy-propagation only through atom bindings (chased
-- | transitively), saturated inlining only for a flat, parameter-closed body at a fully-applied
-- | call, and never across a shadowing binder. Semantics-preservation itself is the
-- | `--opt ≡ --no-opt` differential's job (ADR-0082 §2); these pin the structural rewrites.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Simplify where

import Prelude

import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Simplify (run)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Simplify" do
  describe "copy-propagation" do
    it "drops `let x = <atom>` and resolves x to the atom" do
      run (Let "x" (CAtom (AtomLit (LInt 7))) (Ret (CAtom (var "x"))))
        `shouldEqual` Ret (CAtom (AtomLit (LInt 7)))

    it "chases an alias chain to the underlying atom" do
      -- let x = y in let z = x in z + z  →  y + y
      run
        ( Let "x" (CAtom (var "y"))
            (Let "z" (CAtom (var "x")) (Ret (CPrim AddInt [ var "z", var "z" ])))
        )
        `shouldEqual` Ret (CPrim AddInt [ var "y", var "y" ])

    it "keeps a non-atom binding (no copy-propagation)" do
      let e = Let "x" (CPrim AddInt [ var "a", var "b" ]) (Ret (CAtom (var "x")))
      run e `shouldEqual` e

  describe "saturated inlining" do
    it "inlines a flat, parameter-closed callee at a fully-applied call" do
      -- let f = \a b -> a + b in f p q  →  (binding kept) p + q
      run
        ( Let "f" (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))
            (Ret (CApp (var "f") [ var "p", var "q" ]))
        )
        `shouldEqual`
          Let "f" (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))
            (Ret (CPrim AddInt [ var "p", var "q" ]))

    it "does not inline an under-applied (unsaturated) call" do
      let
        e =
          Let "f" (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ])))
            (Ret (CApp (var "f") [ var "p" ]))
      run e `shouldEqual` e

    it "does not inline a non-flat callee (nested if)" do
      let
        e =
          Let "f"
            (CLam [ "a" ] (Ret (CIf (var "a") (Ret (CAtom (AtomLit (LInt 1)))) (Ret (CAtom (AtomLit (LInt 0)))))))
            (Ret (CApp (var "f") [ var "p" ]))
      run e `shouldEqual` e

  describe "scope" do
    it "does not resolve through a shadowing case binder" do
      -- let x = y in case s of BVar x -> x  : the inner binder `x` shadows the alias.
      run
        ( Let "x" (CAtom (var "y"))
            ( Ret
                ( CCase [ var "s" ]
                    [ { binders: [ BVar "x" ], result: Uncond (Ret (CAtom (var "x"))) } ]
                )
            )
        )
        `shouldEqual`
          Ret
            ( CCase [ var "s" ]
                [ { binders: [ BVar "x" ], result: Uncond (Ret (CAtom (var "x"))) } ]
            )
