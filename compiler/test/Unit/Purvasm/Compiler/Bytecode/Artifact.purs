-- | The decisive byte-identity check: a hand-built `ModuleArtifact` must serialise to the
-- | exact `.pvmo`/`.pvmi` bytes boot emits for `DiaA` (`purvm compile`). This pins both the
-- | JSON shape/key-order and the Yojson-faithful serialiser + MD5 hash.
module Test.Unit.Purvasm.Compiler.Bytecode.Artifact where

import Prelude

import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact, interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The DiaA module of boot's `diamond` fixture (a diamond import graph), as compiled.
diaA :: ModuleArtifact
diaA =
  { name: "DiaA"
  , imports: [ "DiaA", "DiaB", "DiaC", "Prim" ]
  , exports: [ "DiaA.Two", "DiaA.both" ]
  , groups:
      [ { keys: [ "DiaA.Two" ]
        , deps: []
        , members: [ "DiaA.Two" /\ Gcaf [ Ctor "Two" 2 0, Return ] ]
        , recursive: false
        }
      , { keys: [ "DiaA.both" ]
        , deps: [ "DiaA.Two", "DiaB.b", "DiaC.c" ]
        , members:
            [ "DiaA.both" /\ Gcaf [ Load "DiaA.Two", Load "DiaB.b", Load "DiaC.c", TailCall 2 ] ]
        , recursive: false
        }
      ]
  }

refPmo :: String
refPmo = """{"version":3,"name":"DiaA","imports":["DiaA","DiaB","DiaC","Prim"],"exports":["DiaA.Two","DiaA.both"],"groups":[{"keys":["DiaA.Two"],"deps":[],"recursive":false,"members":[["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]]]},{"keys":["DiaA.both"],"deps":["DiaA.Two","DiaB.b","DiaC.c"],"recursive":false,"members":[["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]]}]}"""

refPmi :: String
refPmi = """{"version":3,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce"}"""

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Artifact" do
  it "serialises a module to byte-identical .pmo (== boot's .pvmo)" do
    moduleToString diaA `shouldEqual` refPmo

  it "serialises an interface to byte-identical .pmi (== boot's .pvmi, MD5 hash incl.)" do
    interfaceToString (interfaceOf diaA) `shouldEqual` refPmi
