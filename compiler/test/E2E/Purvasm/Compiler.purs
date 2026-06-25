-- | End-to-end of the implemented frontend: a CoreFn `Expr` through `translExpr`
-- | (CoreFn → CESK AST) then `normalize` (CESK AST → ANF). The full pipeline to runnable
-- | code awaits the `lower` (ANF → CodeBlock) backend; until then this exercises the two
-- | bridges together (ADR-0037 thin vertical slice).
module Test.E2E.Purvasm.Compiler where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Expr (Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.CESK.Translate (translExpr)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr)
import Purvasm.Compiler.MiddleEnd.ANF as ANF
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

compileExpr :: CF.Expr -> Expr
compileExpr = translExpr >>> normalize

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = describe "frontend pipeline: CoreFn → CESK (transl) → ANF (normalize)" do
  it "uncurries a curried application: f 1 2" do
    compileExpr (CF.App nullAnn (CF.App nullAnn (local "f") (int 1)) (int 2))
      `shouldEqual` ANF.Ret (CApp (AtomVar "f") [ AtomLit (LInt 1), AtomLit (LInt 2) ])

  it "lowers an identity abstraction" do
    compileExpr (CF.Abs nullAnn "x" (local "x"))
      `shouldEqual` ANF.Ret (CLam [ "x" ] (ANF.Ret (CAtom (AtomVar "x"))))
