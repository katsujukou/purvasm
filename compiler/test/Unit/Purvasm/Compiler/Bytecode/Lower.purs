-- | Invariants of the core `lower` walk (ANF → bytecode `CodeBlock`) the types cannot
-- | enforce: postorder operand pushes (function before its arguments), `tail` becoming
-- | `TailCall` / `Return`, and `if` compiling to *relative* jumps. (`case` lowering lives
-- | in `Test.Unit.Purvasm.Compiler.Bytecode.Lower.Match`.)
module Test.Unit.Purvasm.Compiler.Bytecode.Lower where

import Prelude

import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerAtom, lowerCexpr, lowerExpr, lowerValue)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

aInt :: Int -> Atom
aInt = AtomLit <<< LInt

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Lower" do
  describe "lowerAtom" do
    it "loads a variable, pushes a literal, and refs a foreign by name" do
      lowerAtom (AtomVar "x") `shouldEqual` Load "x"
      lowerAtom (aInt 7) `shouldEqual` PushInt 7
      lowerAtom (AtomForeign "Effect.Console.log") `shouldEqual` ForeignRef "Effect.Console.log"

  describe "lowerCexpr / lowerValue" do
    it "pushes the function before its arguments, then Call with the arg count" do
      lowerCexpr false (CApp (AtomVar "f") [ aInt 1, aInt 2 ])
        `shouldEqual` [ Load "f", PushInt 1, PushInt 2, Call 2 ]

    it "uses TailCall for a call in tail position" do
      lowerCexpr true (CApp (AtomVar "f") [ aInt 1 ])
        `shouldEqual` [ Load "f", PushInt 1, TailCall 1 ]

    it "lowers a GER perform as a one-argument (unit) call, tail-aware (ADR-0099)" do
      -- perform f ≃ f unit; unit is the immediate 0 (ADR-0064).
      lowerCexpr false (CPerform (AtomVar "f"))
        `shouldEqual` [ Load "f", PushInt 0, Call 1 ]
      lowerCexpr true (CPerform (AtomVar "f"))
        `shouldEqual` [ Load "f", PushInt 0, TailCall 1 ]

    it "ends a tail value with Return, and a non-tail value without one" do
      lowerCexpr true (CAtom (aInt 5)) `shouldEqual` [ PushInt 5, Return ]
      lowerCexpr false (CAtom (aInt 5)) `shouldEqual` [ PushInt 5 ]

    it "carries the operand count on a primitive" do
      lowerValue (CPrim AddInt [ aInt 1, aInt 2 ])
        `shouldEqual` [ PushInt 1, PushInt 2, Prim AddInt 2 ]

    it "compiles a non-tail if to a then-branch with a join jump over the else" do
      lowerCexpr false (CIf (AtomVar "b") (Ret (CAtom (aInt 1))) (Ret (CAtom (aInt 2))))
        `shouldEqual` [ Load "b", JumpUnless 2, PushInt 1, Jump 1, PushInt 2 ]

  describe "lowerExpr" do
    it "binds a let's value before continuing" do
      lowerExpr false (Let "x" (CAtom (aInt 2)) (Ret (CAtom (AtomVar "x"))))
        `shouldEqual` [ PushInt 2, Bind "x", Load "x" ]

    it "emits a recursive group as MakeRec of compiled chunks" do
      lowerExpr false
        (LetRec [ { var: "f", rhs: Ret (CAtom (aInt 1)) } ] (Ret (CAtom (AtomVar "f"))))
        `shouldEqual` [ MakeRec [ "f" /\ [ PushInt 1, Return ] ], Load "f" ]
