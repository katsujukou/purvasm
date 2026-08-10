-- | ADR-0107 slice-1 fixture: a REAL by-need cell reaching EVERY in-scope demand recipe, so a
-- | wrongly-elided force is caught as WRONG OUTPUT rather than as luck.
-- |
-- | Why this fixture is the load-bearing one. A wrong `NeverByNeed` makes the emitter read a
-- | `ByNeed` cell as if it were a value: the force AND its safepoint disappear together, so the
-- | ADR-0105 token/epoch net cannot catch it (nothing is stale — the value was never forced), and
-- | no type says otherwise. The only detector is running the code and reading the answer back.
-- |
-- | The cell: `dict` is a mutually-recursive top-level group (ADR-0070 §4 builds those members
-- | as by-need cells), and every value below reaches a demand site THROUGH it:
-- |
-- |   * `CAccessor` base — projecting a field off the cell-backed record;
-- |   * `CCase` scrutinee — matching on a value read out of the cell;
-- |   * `CIf` condition and guard result — branching on it;
-- |   * `CPrim` operands — arithmetic and comparison on it;
-- |   * `CUpdate` base — a functional update over it.
-- |
-- | Each arrives by a route the lattice must NOT prove: an alias chain, a branch MEET (one arm a
-- | provable literal, the other the cell — `Never ⊓ May = May`, and the selector is opaque so the
-- | fold cannot pick the arm statically), and a container round-trip (arrays and records store
-- | cells, so what comes out is `May` again).
-- |
-- | Every path prints its FULL readback, so an elided force that reads a cell header as a payload
-- | corrupts stdout instead of merely crashing.
module Gate.ByNeedCell where

import Prelude

import Data.Array (range, index)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

-- A mutually-recursive group: the members are by-need cells, and they stay cells because each
-- refers to the other (nothing can collapse the group to a plain value).
evenSum :: Int -> Int
evenSum n = if n <= 0 then 0 else n + oddSum (n - 1)

oddSum :: Int -> Int
oddSum n = if n <= 0 then 1 else n + evenSum (n - 1)

-- A cell-backed record: its fields are computed through the recursive group above, so the record
-- itself is a CAF whose value is built by-need.
cellRecord :: { tag :: Int, name :: String, items :: Array Int }
cellRecord =
  { tag: evenSum 6
  , name: "cell-" <> show (oddSum 5)
  , items: map (\i -> evenSum i) (range 1 6)
  }

data Shape = Leaf Int | Node String Int

-- Read out of the cell-backed record into a constructor: the `case` below scrutinises this.
shapeOf :: Int -> Shape
shapeOf n = if n > 20 then Node cellRecord.name n else Leaf n

main :: Effect Unit
main = do
  log "byneed-cell:start"

  -- (1) CAccessor base, straight off the cell.
  log ("accessor:" <> cellRecord.name <> ":" <> show cellRecord.tag)

  -- (2) an ALIAS chain into a prim operand and an if-condition.
  let
    aliasA = cellRecord
    aliasB = aliasA
  log ("alias-prim:" <> show (aliasB.tag + 1) <> ":" <> show (aliasB.tag * 2))
  log ("alias-if:" <> (if aliasB.tag > 3 then "gt" else "le"))

  -- (3) the branch MEET with an opaque selector: one arm is a provable literal record, the other
  -- is the cell. `Never ⊓ May = May`, so the force must survive — and the run SELECTS the cell
  -- arm, so an elided meet executes the unforced-cell path.
  sel <- Ref.new 1
  pick <- Ref.read sel
  let
    merged = if pick == 0 then { tag: 7, name: "literal", items: [ 1, 2, 3 ] } else cellRecord

  -- Churn AFTER the cell-derived values exist and BEFORE every readback below: under the gate's
  -- small heap this forces collections, so each value survives only by being rooted across them.
  -- Without it the fixture would allocate too little to collect and the gate would count it as
  -- vacuous coverage (`NO-GC(0)`).
  -- string concatenation in a fold: each step allocates a fresh string, so this churns the heap
  -- (thousands of transient allocations) rather than adding immediates.
  let churn = foldl (\acc j -> acc <> show (j * 3)) "" (range 1 2000)
  log ("churn:" <> show (SCU.length churn))
  log ("meet-accessor:" <> merged.name <> ":" <> show merged.tag)
  log ("meet-prim:" <> show (merged.tag - 2))

  -- (4) a CONTAINER round-trip: into an array, back out again.
  let roundTrip = index [ merged, cellRecord ] 1
  case roundTrip of
    Just r -> log ("container:" <> r.name <> ":" <> show (r.tag + foldl (+) 0 r.items))
    Nothing -> log "container:missing"

  -- (5) a CCase scrutinee and a GUARD result, both over cell-derived values.
  let scrut = shapeOf merged.tag
  log
    ( case scrut of
        Node nm k
          | k > merged.tag -> "case-node-gt:" <> nm
          | otherwise -> "case-node:" <> nm <> ":" <> show k
        Leaf k
          | k == merged.tag -> "case-leaf-eq:" <> show k
          | otherwise -> "case-leaf:" <> show k
    )

  -- (6) a CUpdate base over the cell, read back in full.
  let bumped = cellRecord { tag = cellRecord.tag + 100, name = cellRecord.name <> "!" }
  log ("update:" <> bumped.name <> ":" <> show bumped.tag <> ":" <> show (foldl (+) 0 bumped.items))

  log "byneed-cell:done"
