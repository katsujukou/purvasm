-- | The apply census's own surface: the report. The classification itself is the compiler's (and is
-- | tested with it), and the per-object accounting identity is `tools/apply-census.sh`'s gate — what
-- | is left here is that the rendering does not lose or merge what the emitter recorded.
module Test.Unit.Purvasm.Census.Apply where

import Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Purvasm.Census.Apply.Report (RowKey(..), checkIdentities, parseRow, renderEvents)
import Purvasm.Compiler.Backend.LLVM.CallClass (CallEvent(..), MissReason(..), allMissReasons, callClassName, callClasses, missReasonName, profiledReasons)
import Purvasm.Compiler.Backend.LLVM.Types (CandidateKind(..), EnvSrc(..), FnInfo, candidateKinds)
import Test.Spec (Spec, describe, it)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec.Assertions (shouldEqual)
import Purvasm.Compiler.Backend.LLVM.Types (BindOrigin(..))

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
        , GenericApply MissCalleeForeign
        , GenericTail (MissLocalUnknownFn OParam)
        ]
    Array.filter (_ == "M\tclass\tdirect-nontail\t2") rows `shouldEqual` [ "M\tclass\tdirect-nontail\t2" ]
    Array.filter (_ == "M\tclass\tdirect-musttail\t1") rows `shouldEqual` [ "M\tclass\tdirect-musttail\t1" ]
    Array.filter (_ == "M\tclass\tstructural-apply\t1") rows `shouldEqual` [ "M\tclass\tstructural-apply\t1" ]

  -- A reason means a different thing — and points at a different lever — in a tail call than in a
  -- non-tail one, because the two emit different forms. Summing them would hide that.
  it "keys reasons by (class, reason) and never merges the two generic forms" do
    let
      rows = rowsOf [ GenericApply (MissLocalUnknownFn OParam), GenericTail (MissLocalUnknownFn OParam), GenericTail (MissLocalUnknownFn OParam) ]
    Array.filter (_ == "M\treason\tgeneric-apply/local-unknown-fn/param\t1") rows
      `shouldEqual` [ "M\treason\tgeneric-apply/local-unknown-fn/param\t1" ]
    Array.filter (_ == "M\treason\tgeneric-tail/local-unknown-fn/param\t2") rows
      `shouldEqual` [ "M\treason\tgeneric-tail/local-unknown-fn/param\t2" ]

  it "reports every reason row at ZERO for classes that have no reason" do
    -- ADR-0113 §3 changed this contract: the breakdowns are emitted in full, zeros included, so a
    -- row that stops being produced reads as a MISSING row rather than as a zero. What must hold
    -- is that none of them is non-zero.
    let rows = rowsOf [ DirectNonTail fn, DirectMusttail fn, StructuralApply, WrapperEntry ]
    Array.filter (\l -> String.contains (Pattern "\treason\t") l && not (String.contains (Pattern "\t0") l)) rows
      `shouldEqual` []
    Array.filter (\l -> String.contains (Pattern "\tkind\t") l && not (String.contains (Pattern "\t0") l)) rows
      `shouldEqual` []

  it "names classes through the compiler's own renderer (no second spelling)" do
    let rows = rowsOf []
    Array.take (Array.length callClasses) rows
      `shouldEqual` map (\c -> "M\tclass\t" <> callClassName c <> "\t0") callClasses

  it "emits the whole product of both breakdowns, so a missing row is detectable" do
    -- 2 generic forms x every MissReason, plus 2 candidate forms x every CandidateKind.
    let rows = rowsOf []
    Array.length (Array.filter (String.contains (Pattern "\treason\t")) rows)
      `shouldEqual` (2 * Array.length allMissReasons)
    Array.length (Array.filter (String.contains (Pattern "\tkind\t")) rows)
      `shouldEqual` (2 * Array.length candidateKinds)

-- --- ADR-0113 §3: the three identities, and the faults each one must refuse -------------------

-- | The report for a set of events, as the gate reads it.
gateOf :: Array CallEvent -> Array String
gateOf evs = (checkIdentities "M" (rowsOf evs)).failures

-- | Replace one row of a rendered report, so a fault can be injected into a report that was
-- | otherwise produced by the real renderer.
patch :: String -> String -> Array String -> Array String
patch from to = map (\l -> if l == from then to else l)

identitySpec :: Spec Unit
identitySpec = describe "Purvasm.Census.Apply.Report — the ADR-0113 §3 identities" do
  -- The CLEAN case is a row of the suite, not an assumption: a gate that only ever sees faults
  -- can be satisfied by refusing everything, and that is indistinguishable from working.
  it "accepts a report the renderer actually produced" do
    gateOf
      [ GenericApply (MissLocalUnknownFn OParam)
      , GenericTail (MissLocalUnknownFn OMatchBinder)
      , LocalDeferredApply Capture
      , LocalDeferredTail AliasGlobal
      , DirectNonTail fn
      ] `shouldEqual` []

  it "keeps the origin: local-unknown-fn/<origin> splits on the FIRST slash only" do
    parseRow "M\treason\tgeneric-apply/local-unknown-fn/match-binder\t3"
      `shouldEqual` Just { object: "M", key: ReasonRow "generic-apply" "local-unknown-fn/match-binder", count: 3 }

  it "keeps the object, so a mixed-object set cannot balance" do
    -- rows from another object are REFUSED rather than summed in: without this the identities hold
    -- per object only by convention, and one object's shortfall is covered by another's surplus.
    let rows = Array.snoc (rowsOf [ LocalDeferredApply Capture ]) "OTHER\tclass\tgeneric-apply\t99"
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 1

  it "refuses a row duplicated with the SAME value" do
    -- the hole `Map.fromFoldable` leaves: the last write wins, so an identical duplicate changes
    -- no sum and every identity below stays green. Occurrences are counted before the map is built.
    let
      base = rowsOf [ LocalDeferredApply Capture ]
      rows = Array.snoc base "M\tkind\tlocal-deferred-apply/capture\t1"
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 1

  it "(i) refuses a generic class that does not equal the sum of its reasons" do
    let
      rows = patch "M\treason\tgeneric-apply/local-unknown-fn/param\t1"
        "M\treason\tgeneric-apply/local-unknown-fn/param\t0"
        (rowsOf [ GenericApply (MissLocalUnknownFn OParam) ])
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 1

  it "(ii) refuses a local-deferred class that does not equal the sum of its kinds" do
    let
      rows = patch "M\tkind\tlocal-deferred-apply/capture\t1"
        "M\tkind\tlocal-deferred-apply/capture\t2"
        (rowsOf [ LocalDeferredApply Capture ])
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 1

  it "refuses a MISSING row rather than reading it as a zero" do
    let rows = Array.filter (_ /= "M\tkind\tlocal-deferred-tail/alias-local\t0") (rowsOf [ LocalDeferredApply Capture ])
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 1

  it "refuses a duplicated row whose value also disagrees" do
    let
      base = rowsOf [ LocalDeferredApply Capture ]
      rows = Array.snoc base "M\tkind\tlocal-deferred-apply/capture\t5"
    -- reported as a duplicate AND as a broken sum: neither diagnosis hides the other.
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 2

  it "refuses a count moved BETWEEN FORMS (apply <-> tail are never one pool)" do
    let
      rows = patch "M\tkind\tlocal-deferred-apply/capture\t1" "M\tkind\tlocal-deferred-apply/capture\t0"
        ( patch "M\tkind\tlocal-deferred-tail/capture\t0" "M\tkind\tlocal-deferred-tail/capture\t1"
            (rowsOf [ LocalDeferredApply Capture ])
        )
    -- both classes now disagree with their sums: apply is short, tail is over.
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 2

  it "refuses a count moved BETWEEN FAMILIES (a candidate is not a MissReason)" do
    -- the fault (i) and (ii) exist to catch: a dispatch leaves the candidate family and arrives in
    -- the opaque one. One identity stated over both would balance and report nothing.
    let
      rows = patch "M\tkind\tlocal-deferred-apply/capture\t1" "M\tkind\tlocal-deferred-apply/capture\t0"
        ( patch "M\treason\tgeneric-apply/local-unknown-fn/param\t0"
            "M\treason\tgeneric-apply/local-unknown-fn/param\t1"
            (rowsOf [ LocalDeferredApply Capture ])
        )
    Array.length (checkIdentities "M" rows).failures `shouldEqual` 2

  it "refuses an unparsable row instead of skipping it" do
    (Array.length (checkIdentities "M" [ "M\tclass\tnot-a-number\tx" ]).failures > 0) `shouldEqual` true

  it "pins each diagnostic row at zero individually" do
    let
      rows = patch "M\treason\tgeneric-apply/callee-literal\t0" "M\treason\tgeneric-apply/callee-literal\t1"
        (rowsOf [ GenericApply MissCalleeLiteral ])
    -- non-zero diagnostic AND the class sum now disagrees: both are reported, neither is absorbed.
    (Array.length (checkIdentities "M" rows).failures > 0) `shouldEqual` true

  describe "the MissReason enumerations are closed, and the two differ by exactly one" do
    -- Counting rows against `Array.length allMissReasons` cannot catch a reason DROPPED from the
    -- enumeration: both sides shrink together. The list is therefore pinned by name.
    it "allMissReasons is exactly the expected list, in order, without duplicates" do
      map missReasonName allMissReasons `shouldEqual`
        [ "callee-foreign"
        , "callee-literal"
        , "local-unknown-fn/param"
        , "local-unknown-fn/capture"
        , "local-unknown-fn/let-lambda"
        , "local-unknown-fn/let-value"
        , "local-unknown-fn/grec-lambda"
        , "local-unknown-fn/grec-value"
        , "local-unknown-fn/match-binder"
        , "arity-local"
        , "unknown-key"
        , "arity-own-module"
        , "own-object-not-fn"
        , "dep-no-direct-fact"
        , "arity-cross-module"
        ]
      Array.nub allMissReasons `shouldEqual` allMissReasons

    it "profiledReasons is allMissReasons minus MissUnknownKey, and nothing else" do
      -- the dynamic slot space omits exactly the reason that cannot execute a dispatch; any other
      -- divergence would mean the static census and the profile describe different partitions.
      profiledReasons `shouldEqual` Array.filter (_ /= MissUnknownKey) allMissReasons
