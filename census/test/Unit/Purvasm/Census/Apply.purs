-- | The apply census's own surface: the report. The classification itself is the compiler's (and is
-- | tested with it), and the per-object accounting identity is `tools/apply-census.sh`'s gate — what
-- | is left here is that the rendering does not lose or merge what the emitter recorded.
module Test.Unit.Purvasm.Census.Apply where

import Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Purvasm.Census.Apply.Report (renderEvents)
import Purvasm.Compiler.Backend.LLVM.CallClass (CallEvent(..), MissReason(..), callClassName, callClasses)
import Purvasm.Compiler.Backend.LLVM.Types (EnvSrc(..), FnInfo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

fn :: FnInfo
fn = { dsym: "M.f$d", arity: 1, src: SSentinel }

rowsOf :: Array CallEvent -> Array String
rowsOf = Array.filter (_ /= "") <<< String.split (Pattern "\n") <<< renderEvents "M"

spec :: Spec Unit
spec = describe "Purvasm.Census.Apply.Report" do
  it "emits a class row for EVERY column, including the ones at zero" do
    -- a column that stops occurring must read as an explicit zero: a missing row is indistinguishable
    -- from a row nobody looked at, and this report is what ranks the optimisation work.
    let rows = rowsOf [ WrapperEntry ]
    Array.length (Array.filter (String.contains (Pattern "\tclass\t")) rows)
      `shouldEqual` Array.length callClasses
    Array.filter (_ == "M\tclass\twrapper-entry\t1") rows `shouldEqual` [ "M\tclass\twrapper-entry\t1" ]
    Array.filter (_ == "M\tclass\tgeneric-apply\t0") rows `shouldEqual` [ "M\tclass\tgeneric-apply\t0" ]

  it "counts each class independently" do
    let
      rows = rowsOf
        [ DirectNonTail fn
        , DirectNonTail fn
        , DirectMusttail fn
        , StructuralApply
        , GenericApply MissCalleeNotVar
        , GenericTail MissLocalUnknownFn
        ]
    Array.filter (_ == "M\tclass\tdirect-nontail\t2") rows `shouldEqual` [ "M\tclass\tdirect-nontail\t2" ]
    Array.filter (_ == "M\tclass\tdirect-musttail\t1") rows `shouldEqual` [ "M\tclass\tdirect-musttail\t1" ]
    Array.filter (_ == "M\tclass\tstructural-apply\t1") rows `shouldEqual` [ "M\tclass\tstructural-apply\t1" ]

  -- A reason means a different thing — and points at a different lever — in a tail call than in a
  -- non-tail one, because the two emit different forms. Summing them would hide that.
  it "keys reasons by (class, reason) and never merges the two generic forms" do
    let
      rows = rowsOf [ GenericApply MissLocalUnknownFn, GenericTail MissLocalUnknownFn, GenericTail MissLocalUnknownFn ]
    Array.filter (_ == "M\treason\tgeneric-apply/local-unknown-fn\t1") rows
      `shouldEqual` [ "M\treason\tgeneric-apply/local-unknown-fn\t1" ]
    Array.filter (_ == "M\treason\tgeneric-tail/local-unknown-fn\t2") rows
      `shouldEqual` [ "M\treason\tgeneric-tail/local-unknown-fn\t2" ]

  it "reports no reason rows for the classes that have no reason" do
    let rows = rowsOf [ DirectNonTail fn, DirectMusttail fn, StructuralApply, WrapperEntry ]
    Array.filter (String.contains (Pattern "\treason\t")) rows `shouldEqual` []

  it "names classes through the compiler's own renderer (no second spelling)" do
    let rows = rowsOf []
    map (\c -> "M\tclass\t" <> callClassName c <> "\t0") callClasses `shouldEqual` rows
