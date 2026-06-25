-- | Invariants of `normalize` (CESK AST → ANF) that the types cannot enforce: every
-- | argument position holds an *atom* (compound subexpressions are `let`-named, making
-- | evaluation order explicit), curried lambda/application spines are *uncurried*, and
-- | `if`/`case` branches stay `Expr`s (so an untaken branch is not evaluated). Ported
-- | from boot's `test_anf.ml` shape checks (boot's oracle round-trip is not yet runnable
-- | in PureScript, so these pin the structure directly).
module Test.Unit.Purvasm.Compiler.MiddleEnd.Normalize where

import Prelude

import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.AST as Cesk
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Normalize (collectApp, collectLam, normalize)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

num :: Int -> Term
num = TmLit <<< LInt

add :: Term -> Term -> Term
add a b = TmPrim AddInt [ a, b ]

mul :: Term -> Term -> Term
mul a b = TmPrim MulInt [ a, b ]

aInt :: Int -> Atom
aInt = AtomLit <<< LInt

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.Normalize" do
  describe "normalize" do
    it "leaves an already-atomic term as a tail atom" do
      normalize (num 42) `shouldEqual` Ret (CAtom (aInt 42))

    it "let-names a non-atomic argument so every operand is an atom: (1+2)*3" do
      normalize (mul (add (num 1) (num 2)) (num 3))
        `shouldEqual`
          Let "$a1" (CPrim AddInt [ aInt 1, aInt 2 ])
            (Ret (CPrim MulInt [ AtomVar "$a1", aInt 3 ]))

    it "names a nested call before its enclosing call: f (g x)" do
      normalize (TmApp (TmVar "f") (TmApp (TmVar "g") (TmVar "x")))
        `shouldEqual`
          Let "$a1" (CApp (AtomVar "g") [ AtomVar "x" ])
            (Ret (CApp (AtomVar "f") [ AtomVar "$a1" ]))

    it "uncurries a curried application spine into one call: f 1 2" do
      normalize (TmApp (TmApp (TmVar "f") (num 1)) (num 2))
        `shouldEqual` Ret (CApp (AtomVar "f") [ aInt 1, aInt 2 ])

    it "uncurries a curried lambda spine into one lambda" do
      normalize (TmLam "x" (TmLam "y" (add (TmVar "x") (TmVar "y"))))
        `shouldEqual`
          Ret (CLam [ "x", "y" ] (Ret (CPrim AddInt [ AtomVar "x", AtomVar "y" ])))

    it "keeps a constructor application as a CCtor node (not a generic call): Just 5" do
      normalize (TmApp (TmCtor "Just" 1) (num 5))
        `shouldEqual` Ret (CCtor "Just" 1 [ aInt 5 ])

    it "keeps if branches as Exprs so an untaken branch is not evaluated" do
      normalize (TmIf (TmPrim LtInt [ num 1, num 2 ]) (num 10) (num 20))
        `shouldEqual`
          Let "$a1" (CPrim LtInt [ aInt 1, aInt 2 ])
            (Ret (CIf (AtomVar "$a1") (Ret (CAtom (aInt 10))) (Ret (CAtom (aInt 20)))))

    it "atomises a case scrutinee and normalises each alternative's body" do
      normalize
        ( TmCase [ TmApp (TmCtor "Just" 1) (num 5) ]
            [ { binders: [ BCtor "Just" [ BVar "x" ] ]
              , result: Cesk.Unconditional (add (TmVar "x") (num 1))
              }
            , { binders: [ BCtor "Nothing" [] ]
              , result: Cesk.Unconditional (num 0)
              }
            ]
        )
        `shouldEqual`
          Let "$a1" (CCtor "Just" 1 [ aInt 5 ])
            ( Ret
                ( CCase [ AtomVar "$a1" ]
                    [ { binders: [ BCtor "Just" [ BVar "x" ] ]
                      , result: Uncond (Ret (CPrim AddInt [ AtomVar "x", aInt 1 ]))
                      }
                    , { binders: [ BCtor "Nothing" [] ]
                      , result: Uncond (Ret (CAtom (aInt 0)))
                      }
                    ]
                )
            )

  describe "collectLam" do
    it "collects the whole curried lambda spine in order" do
      collectLam (TmLam "x" (TmLam "y" (TmVar "x")))
        `shouldEqual` { params: [ "x", "y" ], body: TmVar "x" }

    it "collects no parameters from a non-lambda" do
      collectLam (TmVar "x") `shouldEqual` { params: [], body: TmVar "x" }

  describe "collectApp" do
    it "collects the whole application spine head and args in order" do
      collectApp (TmApp (TmApp (TmVar "f") (num 1)) (num 2))
        `shouldEqual` { head: TmVar "f", args: [ num 1, num 2 ] }

    it "collects no arguments from a non-application" do
      collectApp (TmVar "f") `shouldEqual` { head: TmVar "f", args: [] }
