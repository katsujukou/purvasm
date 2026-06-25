-- | End-to-end of the implemented pipeline as a black box: a CoreFn `Expr` in, the
-- | lowered bytecode `CodeBlock` out — `translExpr` (CoreFn → CESK AST) ≫ `normalize`
-- | (CESK AST → ANF) ≫ `lowerExpr` (ANF → bytecode). Asserting observable I/O, not
-- | intermediate structure. (Becomes compile-and-run once a bytecode VM lands; ADR-0037.)
module Test.E2E.Purvasm.Compiler where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Expr (Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerExpr)
import Purvasm.Compiler.CESK.Translate (translExpr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | A synthetic (zero-span, no-metadata) annotation for hand-built CoreFn nodes.
nullAnn :: Ann
nullAnn = { span: { start: z, end: z }, meta: Nothing }
  where
  z = { line: 0, column: 0 }

local :: String -> CF.Expr
local x = CF.Var nullAnn (CF.Qualified Nothing x)

int :: Int -> CF.Expr
int = CF.Literal nullAnn <<< CF.LitInt

-- | The whole front-to-back lowering of a top-level (tail-position) expression.
compile :: CF.Expr -> CodeBlock
compile = translExpr >>> normalize >>> lowerExpr true

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = describe "pipeline: CoreFn → CESK → ANF → bytecode" do
  it "lowers a curried application to one uncurried tail call" do
    compile (CF.App nullAnn (CF.App nullAnn (local "f") (int 1)) (int 2))
      `shouldEqual` [ Load "f", PushInt 1, PushInt 2, TailCall 2 ]

  it "lowers an identity abstraction to a returning closure" do
    compile (CF.Abs nullAnn "x" (local "x"))
      `shouldEqual` [ Closure [ "x" ] [ Load "x", Return ], Return ]
