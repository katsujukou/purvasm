-- | Invariants of `case` lowering (`Lower.Match`, ADR-0031) the types cannot enforce:
-- | the decision tree shares one discriminant switch across alternatives (each tag tested
-- | once), the naive matcher re-tests per alternative, both bind occurrences to fresh
-- | locals, resolve labels to correct relative offsets, and end in a `Fail` fall-through.
-- | Driven through the real `lowerAtom`/`lowerExpr` (the `Lowerers` `Lower` supplies).
module Test.Unit.Purvasm.Compiler.Bytecode.Lower.Match where

import Prelude

import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerAtom, lowerExpr)
import Purvasm.Compiler.Bytecode.Lower.Match (Lowerers, compileNaive, compileTree)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

lw :: Lowerers
lw = { atom: lowerAtom, body: lowerExpr }

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Lower.Match" do
  describe "compileNaive (per-alternative re-testing)" do
    it "binds the scrutinee, switches on the tag, projects the field, runs the body, and falls through to Fail" do
      compileNaive lw false [ AtomVar "x" ]
        [ { binders: [ BCtor "Just" [ BVar "y" ] ]
          , result: Uncond (Ret (CAtom (AtomVar "y")))
          }
        ]
        `shouldEqual`
          [ Load "x"
          , Bind "$nv1"
          , Load "$nv1"
          , SwitchCtor [ "Just" /\ 0 ] 7
          , Load "$nv1"
          , Proj 0
          , Bind "$nv2"
          , Load "$nv2"
          , Bind "y"
          , Load "y"
          , Jump 1
          , Fail "case: no matching alternative"
          ]

  describe "compileTree (Maranget decision tree)" do
    it "shares one tag switch across alternatives, dispatching each to its own branch" do
      compileTree lw false [ AtomVar "x" ]
        [ { binders: [ BCtor "Just" [ BVar "a" ] ]
          , result: Uncond (Ret (CAtom (AtomLit (LInt 1))))
          }
        , { binders: [ BCtor "Nothing" [] ]
          , result: Uncond (Ret (CAtom (AtomLit (LInt 2))))
          }
        ]
        `shouldEqual`
          [ Load "x"
          , Bind "$dt1"
          , Load "$dt1"
          , SwitchCtor [ "Just" /\ 0, "Nothing" /\ 7 ] 9
          , Load "$dt1"
          , Proj 0
          , Bind "$dt2"
          , Load "$dt2"
          , Bind "a"
          , PushInt 1
          , Jump 3
          , PushInt 2
          , Jump 1
          , Fail "case: no matching alternative"
          ]
