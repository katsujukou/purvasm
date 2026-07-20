-- | The decisive serialisation check: a hand-built `ModuleArtifact` must serialise to the
-- | exact expected `.pmo`/`.pmi` bytes (fixtures whose provenance is boot's `DiaA`,
-- | `purvm compile`). This pins the JSON shape/key-order, the Yojson-faithful serialiser,
-- | `version`, and the MD5 interface hash — a **format-class golden** (ADR-0104 §4): these
-- | bytes are a persistent on-disk ABI, so they are NOT freely re-baselineable — a behavioural
-- | green is not a licence here; changing them is a deliberate format migration (schema
-- | version bump + migration/rejection story for existing artifacts).
module Test.Unit.Purvasm.Compiler.Bytecode.Artifact where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Artifact (ModuleArtifact, Summary(..), interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Image (Json(..))
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

-- `refPmi` with the `--opt`-only summary appended after `hash` — the five-key core is byte-for-byte
-- identical (ADR-0084 §5).
refPmiWithSummary :: String
refPmiWithSummary = """{"version":3,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce","summary":{"v":1}}"""

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Artifact" do
  it "serialises a module to the exact golden .pmo (format-class, ADR-0104 §4)" do
    moduleToString diaA `shouldEqual` refPmo

  it "serialises an interface to the exact golden .pmi (format-class incl. MD5 hash)" do
    interfaceToString (interfaceOf diaA) `shouldEqual` refPmi

  -- ADR-0084 §5: the `--opt`-only summary field must be *entirely absent* when `Nothing` (the
  -- five-key core is the format-class ABI existing readers parse), and appended *after* `hash`
  -- when `Just`, leaving the core byte-for-byte unchanged.
  it "omits the summary field when Nothing (the five-key format-class core)" do
    interfaceToString ((interfaceOf diaA) { summary = Nothing }) `shouldEqual` refPmi

  it "appends the --opt summary after hash, core bytes unchanged" do
    interfaceToString ((interfaceOf diaA) { summary = Just (Summary (JObj [ "v" /\ JInt 1 ])) })
      `shouldEqual` refPmiWithSummary
