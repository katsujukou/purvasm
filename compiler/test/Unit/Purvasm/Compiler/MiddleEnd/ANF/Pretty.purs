-- | `ANF.Pretty` rendering: pin the debug layout (uncurried calls, multi-line `let … in`, one
-- | alternative per line) so a regression in the printer is caught, and the expected shape is
-- | documented here.
module Test.Unit.Purvasm.Compiler.MiddleEnd.ANF.Pretty where

import Prelude

import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.Pretty (printExpr, printModuleAnf)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.MiddleEnd.ANF.Pretty" do
  describe "printExpr" do
    it "lays out a lambda over a let-bound primop as a flat column" do
      let
        term =
          Ret
            ( CLam [ "x" ]
                ( Let "y" (CPrim AddInt [ AtomVar "x", AtomLit (LInt 1) ])
                    (Ret (CApp (AtomVar "f") [ AtomVar "y" ]))
                )
            )
      printExpr term `shouldEqual`
        "\\x -> let\n  y = AddInt(x, 1)\nin f(y)"

    it "renders a case with one alternative per line and uncurried constructor binders" do
      let
        term =
          Ret
            ( CCase [ AtomVar "xs" ]
                [ { binders: [ BCtor "Nil" [] ], result: Uncond (Ret (CAtom (AtomLit (LInt 0)))) }
                , { binders: [ BCtor "Cons" [ BVar "h", BVar "t" ] ]
                  , result: Uncond (Ret (CApp (AtomVar "sum") [ AtomVar "t" ]))
                  }
                ]
            )
      printExpr term `shouldEqual`
        "case xs of\n  Nil -> 0\n  Cons(h, t) -> sum(t)"

  describe "printModuleAnf" do
    it "emits a module header and a `rec` group for a recursive binding" do
      let
        decls =
          [ { recursive: false
            , members: [ "M.k" /\ Ret (CAtom (AtomLit (LInt 42))) ]
            }
          , { recursive: true
            , members:
                [ "M.loop" /\ Ret (CLam [ "n" ] (Ret (CApp (AtomVar "M.loop") [ AtomVar "n" ]))) ]
            }
          ]
      printModuleAnf "M" decls `shouldEqual`
        ( "module M where\n\n"
            <> "M.k = 42\n\n"
            <> "rec\n  M.loop = \\n -> M.loop(n)\n"
        )
