-- | `Simplify` invariants (ADR-0028): the three rewrites must fire exactly where the callee's
-- | shape licenses them and nowhere else — copy-propagation only through atom bindings (chased
-- | transitively), saturated inlining only for a flat, parameter-closed body at a fully-applied
-- | call, intrinsic-foreign saturation only at an exactly-saturated call of a known eta-primop
-- | key, and never across a shadowing binder. Semantics-preservation itself is the
-- | `--opt ≡ --no-opt` differential's job (ADR-0082 §2); these pin the structural rewrites.
module Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Simplify where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Simplify (IntrinsicLookup)
import Purvasm.Compiler.MiddleEnd.Optimizer.Simplify (moduleKnown, run) as Simplify
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

var :: String -> Atom
var = AtomVar

-- The structural rewrites are exercised with the intrinsic rung switched off and no module
-- facts…
run :: Expr -> Expr
run = Simplify.run (const Nothing) Map.empty

-- …and the saturation rewrite with a two-entry stub table (the real table is `Ffi.intrinsicPrim`,
-- pinned by the `Ffi` spec).
stubIntr :: IntrinsicLookup
stubIntr = case _ of
  "Data.Semiring.intAdd" -> Just { op: AddInt, arity: 2 }
  "Data.Int.Bits.complement" -> Just { op: ComplementInt, arity: 1 }
  _ -> Nothing

runIntr :: Expr -> Expr
runIntr = Simplify.run stubIntr Map.empty

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Optimizer.Simplify" do
  describe "intrinsic-foreign saturation" do
    it "collapses an exactly-saturated intrinsic-foreign call to its primop" do
      runIntr (Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])
      runIntr (Ret (CApp (AtomForeign "Data.Int.Bits.complement") [ var "p" ]))
        `shouldEqual` Ret (CPrim ComplementInt [ var "p" ])

    it "collapses through a copy-propagated alias of the foreign head" do
      -- let f = intAdd in f p q  →  Prim(AddInt, p, q)
      runIntr
        ( Let "f" (CAtom (AtomForeign "Data.Semiring.intAdd"))
            (Ret (CApp (var "f") [ var "p", var "q" ]))
        )
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "leaves an under- or over-applied intrinsic call to the link-time closure" do
      let under = Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p" ])
      let over = Ret (CApp (AtomForeign "Data.Semiring.intAdd") [ var "p", var "q", var "r" ])
      runIntr under `shouldEqual` under
      runIntr over `shouldEqual` over

    it "collapses the AtomVar spelling of an intrinsic key (a cross-module foreign reference)" do
      runIntr (Ret (CApp (var "Data.Semiring.intAdd") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "leaves a non-intrinsic foreign call untouched" do
      let e = Ret (CApp (AtomForeign "Effect.Console.log") [ var "p" ])
      runIntr e `shouldEqual` e

  describe "moduleKnown (the sibling-binding facts)" do
    it "collapses a call through a sibling top-level alias (the floated dictionary application)" do
      -- add1 = intAdd (a sibling decl); use = add1 p q  →  Prim(AddInt, p, q)
      let
        known = Simplify.moduleKnown
          [ { recursive: false
            , members: [ "M.add1" /\ Ret (CAtom (AtomForeign "Data.Semiring.intAdd")) ]
            }
          ]
      Simplify.run stubIntr known (Ret (CApp (var "M.add1") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "inlines a sibling top-level flat function at a saturated call" do
      let
        known = Simplify.moduleKnown
          [ { recursive: false
            , members: [ "M.f" /\ Ret (CLam [ "a", "b" ] (Ret (CPrim AddInt [ var "a", var "b" ]))) ]
            }
          ]
      Simplify.run (const Nothing) known (Ret (CApp (var "M.f") [ var "p", var "q" ]))
        `shouldEqual` Ret (CPrim AddInt [ var "p", var "q" ])

    it "never registers a recursive group member (by-need must not be short-circuited)" do
      let
        known = Simplify.moduleKnown
          [ { recursive: true
            , members: [ "M.r" /\ Ret (CAtom (AtomForeign "Data.Semiring.intAdd")) ]
            }
          ]
      let e = Ret (CApp (var "M.r") [ var "p", var "q" ])
      Simplify.run stubIntr known e `shouldEqual` e

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
