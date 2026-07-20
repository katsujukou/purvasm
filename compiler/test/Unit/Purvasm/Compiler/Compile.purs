-- | `compileModule` end-to-end: a hand-built CoreFn `Module` (the `diamond/DiaA` fixture)
-- | through CoreFn → CESK → ANF → `Gdef` → groups must serialise to the exact expected
-- | `.pmo`/`.pmi` bytes — the whole separate-compilation pipeline bar `Decode`. The
-- | expectations are **L2-owned composite goldens** (ADR-0104 §4; provenance: boot's
-- | `purvm compile`): the instruction/lowering *content* is emission-class (re-baselineable
-- | on an intentional VM-lowering change, behavioural gate green as the licence), while the
-- | *serialisation* — JSON schema, key order, `version`, hash — is format-class (a persistent
-- | ABI; changing those bytes is a format migration, never a routine re-baseline).
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
refPmo = """{"version":3,"name":"DiaA","imports":["DiaA","DiaB","DiaC","Prim"],"exports":["DiaA.Two","DiaA.both"],"groups":[{"keys":["DiaA.Two"],"deps":[],"recursive":false,"members":[["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]]]},{"keys":["DiaA.both"],"deps":["DiaA.Two","DiaB.b","DiaC.c"],"recursive":false,"members":[["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]]}]}"""

refPmi :: String
refPmi = """{"version":3,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce"}"""

-- A self-recursive lambda (`loop = \x -> loop x`): its group must carry `recursive:true`
-- and its interface must publish `["recfn", 1]`, not `["fn", 1]` (ADR-0077) — the fact a
-- native caller needs to pick the force-cell path over the sentinel path.
recMod :: CF.Module
recMod =
  { name: [ "RecMod" ]
  , path: ""
  , builtWith: ""
  , imports: [ { ann: nullAnn, moduleName: [ "Prim" ] } ]
  , exports: [ "loop" ]
  , reExports: Object.empty
  , foreignNames: []
  , decls:
      [ CF.Rec
          [ { ann: nullAnn
            , ident: "loop"
            , expr: CF.Abs nullAnn "x" (CF.App nullAnn (qvar [ "RecMod" ] "loop") (CF.Var nullAnn (CF.Qualified Nothing "x")))
            }
          ]
      ]
  }

refRecPmo :: String
refRecPmo = """{"version":3,"name":"RecMod","imports":["Prim"],"exports":["RecMod.loop"],"groups":[{"keys":["RecMod.loop"],"deps":["RecMod.loop"],"recursive":true,"members":[["RecMod.loop",["fn",["x"],[["ld","RecMod.loop"],["ld","x"],["tc",1]]]]]}]}"""

refRecPmi :: String
refRecPmi = """{"version":3,"name":"RecMod","exports":[["RecMod.loop",["recfn",1]]],"imports":["Prim"],"hash":"c98b98efe3f5ac8b6b5644a654c18ebb"}"""

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Compile" do
  it "compiles a CoreFn module to the exact golden .pmo (composite golden, ADR-0104 §4)" do
    moduleToString (compileModule diaA) `shouldEqual` refPmo

  it "derives the exact golden .pmi (composite golden, ADR-0104 §4)" do
    interfaceToString (interfaceOf (compileModule diaA)) `shouldEqual` refPmi

  it "marks a recursive lambda group recursive:true in the .pmo (ADR-0077)" do
    moduleToString (compileModule recMod) `shouldEqual` refRecPmo

  it "publishes a recursive lambda as [\"recfn\", n] in the .pmi (ADR-0077)" do
    interfaceToString (interfaceOf (compileModule recMod)) `shouldEqual` refRecPmi
