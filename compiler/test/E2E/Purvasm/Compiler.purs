-- | End-to-end of the implemented pipeline as a black box: a CoreFn `Expr` in, the
-- | lowered bytecode `CodeBlock` out — `translExpr` (CoreFn → CESK AST) ≫ `normalize`
-- | (CESK AST → ANF) ≫ `lowerExpr` (ANF → bytecode). Asserting observable I/O, not
-- | intermediate structure. (Becomes compile-and-run once a bytecode VM lands; ADR-0037.)
module Test.E2E.Purvasm.Compiler where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Expr (Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerExpr)
import Purvasm.Compiler.CESK.Translate (translExpr)
import Purvasm.Compiler.Compile (compileModule)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
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

-- boot's `diamond/DiaA/corefn.json` verbatim (real `purs 0.15.16` output).
diaACorefn :: String
diaACorefn = """{"builtWith":"0.15.16","comments":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"end":[4,23],"start":[4,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[4,23],"start":[4,1]}},"constructorName":"Two","fieldNames":["value0","value1"],"type":"Constructor","typeName":"Two"},"identifier":"Two"},{"annotation":{"meta":null,"sourceSpan":{"end":[5,12],"start":[5,1]}},"bindType":"NonRec","expression":{"abstraction":{"abstraction":{"annotation":{"meta":{"constructorType":"ProductType","identifiers":["value0","value1"],"metaType":"IsConstructor"},"sourceSpan":{"end":[6,11],"start":[6,8]}},"type":"Var","value":{"identifier":"Two","moduleName":["DiaA"]}},"annotation":{"meta":null,"sourceSpan":{"end":[6,13],"start":[6,8]}},"argument":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[6,13],"start":[6,12]}},"type":"Var","value":{"identifier":"b","moduleName":["DiaB"]}},"type":"App"},"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[6,8]}},"argument":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[6,15],"start":[6,14]}},"type":"Var","value":{"identifier":"c","moduleName":["DiaC"]}},"type":"App"},"identifier":"both"}],"exports":["Two","both"],"foreign":[],"imports":[{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaA"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaB"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaC"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["Prim"]}],"moduleName":["DiaA"],"modulePath":"src/DiaA.purs","reExports":{},"sourceSpan":{"end":[6,15],"start":[1,1]}}"""

refPmoDiaA :: String
refPmoDiaA = """{"version":2,"name":"DiaA","imports":["DiaA","DiaB","DiaC","Prim"],"exports":["DiaA.Two","DiaA.both"],"groups":[{"keys":["DiaA.Two"],"deps":[],"members":[["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]]]},{"keys":["DiaA.both"],"deps":["DiaA.Two","DiaB.b","DiaC.c"],"members":[["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]]}]}"""

refPmiDiaA :: String
refPmiDiaA = """{"version":2,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce"}"""

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  describe "pipeline: CoreFn → CESK → ANF → bytecode" do
    it "lowers a curried application to one uncurried tail call" do
      compile (CF.App nullAnn (CF.App nullAnn (local "f") (int 1)) (int 2))
        `shouldEqual` [ Load "f", PushInt 1, PushInt 2, TailCall 2 ]

    it "lowers an identity abstraction to a returning closure" do
      compile (CF.Abs nullAnn "x" (local "x"))
        `shouldEqual` [ Closure [ "x" ] [ Load "x", Return ], Return ]

  describe "separate compilation: corefn.json → .pmo/.pmi (byte-identical to boot)" do
    it "compiles real purs corefn (diamond/DiaA) to the exact .pmo and .pmi bytes" do
      case jsonParser diaACorefn of
        Left e -> fail ("JSON parse failed: " <> e)
        Right json -> case decodeModule json of
          Left e -> fail ("decode failed: " <> show e)
          Right m -> do
            moduleToString (compileModule m) `shouldEqual` refPmoDiaA
            interfaceToString (interfaceOf (compileModule m)) `shouldEqual` refPmiDiaA
