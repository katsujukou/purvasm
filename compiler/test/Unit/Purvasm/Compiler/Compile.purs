-- | `compileModule` end-to-end: a hand-built CoreFn `Module` (boot's `diamond/DiaA`)
-- | through CoreFn → CESK → ANF → `Gdef` → groups must serialise to the exact `.pvmo`/
-- | `.pvmi` bytes boot's `purvm compile` emits — the whole separate-compilation pipeline
-- | bar `Decode`, byte-identical.
module Test.Unit.Purvasm.Compiler.Compile where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Expr (Bind(..), Expr(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Compile (compileModule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nullAnn :: Ann
nullAnn = { span: { start: z, end: z }, meta: Nothing }
  where
  z = { line: 0, column: 0 }

qvar :: Array String -> String -> CF.Expr
qvar mn id = CF.Var nullAnn (CF.Qualified (Just mn) id)

-- boot's diamond/DiaA:  data Two = Two _ _;  both = DiaA.Two DiaB.b DiaC.c
diaA :: CF.Module
diaA =
  { name: [ "DiaA" ]
  , path: ""
  , builtWith: ""
  , imports: [ imp [ "DiaA" ], imp [ "DiaB" ], imp [ "DiaC" ], imp [ "Prim" ] ]
  , exports: [ "Two", "both" ]
  , reExports: Object.empty
  , foreignNames: []
  , decls:
      [ CF.NonRec nullAnn "Two" (CF.Constructor nullAnn "Two" "Two" [ "a", "b" ])
      , CF.NonRec nullAnn "both"
          (CF.App nullAnn (CF.App nullAnn (qvar [ "DiaA" ] "Two") (qvar [ "DiaB" ] "b")) (qvar [ "DiaC" ] "c"))
      ]
  }
  where
  imp mn = { ann: nullAnn, moduleName: mn }

refPmo :: String
refPmo = """{"version":2,"name":"DiaA","imports":["DiaA","DiaB","DiaC","Prim"],"exports":["DiaA.Two","DiaA.both"],"groups":[{"keys":["DiaA.Two"],"deps":[],"members":[["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]]]},{"keys":["DiaA.both"],"deps":["DiaA.Two","DiaB.b","DiaC.c"],"members":[["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]]}]}"""

refPmi :: String
refPmi = """{"version":2,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce"}"""

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Compile" do
  it "compiles a CoreFn module to byte-identical .pmo (== boot's .pvmo)" do
    moduleToString (compileModule diaA) `shouldEqual` refPmo

  it "derives a byte-identical .pmi (== boot's .pvmi)" do
    interfaceToString (interfaceOf (compileModule diaA)) `shouldEqual` refPmi
