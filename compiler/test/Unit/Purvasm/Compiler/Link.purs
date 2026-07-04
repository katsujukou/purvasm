-- | Invariants of `link` (artifacts → runnable `Image`) the types cannot enforce:
-- | modules come out in dependency order, only the entry-reachable definitions survive
-- | (reachability DCE), and the result is byte-identical to boot's `app.pvm`. Checked
-- | against boot's `diamond` image (`purvm build -m DiaA -e both --value`).
module Test.Unit.Purvasm.Compiler.Link where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact)
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Image (imageToString)
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.Link (link)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- boot's diamond fixture, as compiled artifacts (matches each module's `.pmo`).
diaD :: ModuleArtifact
diaD =
  { name: "DiaD"
  , imports: [ "Prim" ]
  , exports: [ "DiaD.d" ]
  , groups: [ { keys: [ "DiaD.d" ], deps: [], members: [ "DiaD.d" /\ Gcaf [ PushInt 7, Return ] ], recursive: false } ]
  }

diaB :: ModuleArtifact
diaB =
  { name: "DiaB"
  , imports: [ "DiaB", "DiaD", "Prim" ]
  , exports: [ "DiaB.b" ]
  , groups: [ { keys: [ "DiaB.b" ], deps: [ "DiaD.d" ], members: [ "DiaB.b" /\ Gcaf [ Load "DiaD.d", Return ] ], recursive: false } ]
  }

diaC :: ModuleArtifact
diaC =
  { name: "DiaC"
  , imports: [ "DiaC", "DiaD", "Prim" ]
  , exports: [ "DiaC.c" ]
  , groups: [ { keys: [ "DiaC.c" ], deps: [ "DiaD.d" ], members: [ "DiaC.c" /\ Gcaf [ Load "DiaD.d", Return ] ], recursive: false } ]
  }

diaA :: ModuleArtifact
diaA =
  { name: "DiaA"
  , imports: [ "DiaA", "DiaB", "DiaC", "Prim" ]
  , exports: [ "DiaA.Two", "DiaA.both" ]
  , groups:
      [ { keys: [ "DiaA.Two" ], deps: [], members: [ "DiaA.Two" /\ Gcaf [ Ctor "Two" 2 0, Return ] ], recursive: false }
      , { keys: [ "DiaA.both" ]
        , deps: [ "DiaA.Two", "DiaB.b", "DiaC.c" ]
        , members: [ "DiaA.both" /\ Gcaf [ Load "DiaA.Two", Load "DiaB.b", Load "DiaC.c", TailCall 2 ] ]
        , recursive: false
        }
      ]
  }

refAppPvm :: String
refAppPvm = """{"version":3,"gdefs":[["DiaD.d",["caf",[["pi",7],["rt"]]]],["DiaB.b",["caf",[["ld","DiaD.d"],["rt"]]]],["DiaC.c",["caf",[["ld","DiaD.d"],["rt"]]]],["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]],["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]],"main":[["ld","DiaA.both"],["rt"]],"effect":false}"""

-- Entry `DiaB.b` reaches only `DiaB.b → DiaD.d`; `DiaA.*` and `DiaC.c` are dropped.
refDce :: String
refDce = """{"version":3,"gdefs":[["DiaD.d",["caf",[["pi",7],["rt"]]]],["DiaB.b",["caf",[["ld","DiaD.d"],["rt"]]]]],"main":[["ld","DiaB.b"],["rt"]],"effect":false}"""

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Link" do
  it "links artifacts into a byte-identical app.pvm (dep order + reachability DCE)" do
    imageToString (link [ diaA, diaB, diaC, diaD ] (const Nothing) (TmVar "DiaA.both"))
      `shouldEqual` refAppPvm

  it "drops definitions unreachable from the entry (reachability DCE)" do
    imageToString (link [ diaA, diaB, diaC, diaD ] (const Nothing) (TmVar "DiaB.b"))
      `shouldEqual` refDce
