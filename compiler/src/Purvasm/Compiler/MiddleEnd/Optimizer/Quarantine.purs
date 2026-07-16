-- | The sticky backstop quarantine (ADR-0089 Addendum, 2026-07-16): module-fixpoint-scoped memory
-- | of round-growth rejections, so a binding whose NbE output tripped the backstop is not rebuilt
-- | and discarded again every round on an unchanged observation.
-- |
-- | Ownership split (the Addendum's API boundary): this module — the optimiser subsystem — owns the
-- | quarantine's *meaning*: what a record holds (`RelevantFacts`, the exact **pre-NbE** fact
-- | projections over the rejected body's reachable key set) and when a record licenses a skip
-- | (`stillRejected`: term-identical body *and* structurally equal re-projected facts). The build
-- | driver owns only the *lifetime*: it creates the empty quarantine per module fixpoint, threads
-- | it opaquely round to round, and drops it when the module converges — it never inspects one.
-- |
-- | Equality is structural, never a lossy hash: skipping NbE is *licensed* by this comparison, so a
-- | hash collision would silently suppress a valid retry. The bias is deliberate — a false retry
-- | merely re-spends CPU the memoryless backstop spent every round, while a false skip loses an
-- | optimisation — so anything unknown or changed retries.
module Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine
  ( FactLookups
  , Quarantine
  , RejectionEvent(..)
  , RelevantFacts
  , clearRejection
  , emptyQuarantine
  , recordRejection
  , relevantFactsOf
  , stillRejected
  ) where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (Lazy, force)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectFact)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)

-- | The per-round-variable facts NbE consults, as the seam projects them **before** NbE runs
-- | (ADR-0089 Addendum: `candidatesOf … early` ∪ dependency candidates, and the pre-NbE effect
-- | oracle — never the published `BuildSummary`, which derives from the later post-`Specialize`
-- | module). Entries for dependency keys may be present; they are constant through the module
-- | fixpoint, so they compare equal every round and never trigger a retry.
type FactLookups =
  { candidate :: String -> Maybe InlineCandidate
  , effect :: String -> Maybe EffectFact
  }

-- | Per reachable key, the exact lookup answers at rejection time. Plain structural data —
-- | `InlineCandidate` carries its body as `Expr` and `EffectFact` is a flat record, so `Eq` is the
-- | comparison (deliberately named `RelevantFacts`, not "fingerprint").
type RelevantFacts = Map String { candidate :: Maybe InlineCandidate, effect :: Maybe EffectFact }

-- | The rejection memory: binding key → the round input the backstop preserved plus the facts its
-- | NbE attempt could have consulted. Opaque outside the optimiser subsystem.
newtype Quarantine = Quarantine (Map String { input :: Expr, facts :: RelevantFacts })

-- | What the seam reports to the driver (dispatched to the `onOptimizerBackstop` hook, ADR-0087
-- | §3): each *newly recorded* rejection, and — driver-synthesised at fixpoint end — the summary.
-- | `rejectionAttempts` counts NbE executions that actually ran and tripped (a quarantined skip is
-- | not an attempt); `distinctBindings` counts unique keys that tripped at least once.
data RejectionEvent
  = BackstopFired { key :: String, inputSize :: Int, outputSize :: Int }
  | BackstopSummary { rejectionAttempts :: Int, distinctBindings :: Int }

derive instance eqRejectionEvent :: Eq RejectionEvent

instance showRejectionEvent :: Show RejectionEvent where
  show = case _ of
    BackstopFired r ->
      "(BackstopFired " <> r.key <> " " <> show r.inputSize <> " -> " <> show r.outputSize <> ")"
    BackstopSummary s ->
      "(BackstopSummary attempts=" <> show s.rejectionAttempts
        <> " distinct="
        <> show s.distinctBindings
        <> ")"

-- | The state a module's fixpoint starts from (created and owned by the driver, per fixpoint).
emptyQuarantine :: Quarantine
emptyQuarantine = Quarantine Map.empty

-- | Does a recorded rejection still hold for this round's body — i.e. is skipping the NbE attempt
-- | licensed? True iff the binding has a record, the body is term-identical to the recorded input,
-- | and the current round's facts, re-projected over exactly the recorded key set, are structurally
-- | equal. Re-projection over the *recorded* keys is sound: reachability is a function of the body
-- | and the reachable candidates' bodies, so if none of those changed, today's closure is the
-- | recorded one; if any did change, its own entry differs and the comparison fails (retry).
-- | `lookups` is `Lazy` so the healthy path (no record) never pays the fact projections.
stillRejected :: Lazy FactLookups -> String -> Expr -> Quarantine -> Boolean
stillRejected lookups key body (Quarantine m) = case Map.lookup key m of
  Nothing -> false
  Just r -> r.input == body && reproject (force lookups) r.facts == r.facts

-- | Record a fresh rejection (overwriting any previous record for the key).
recordRejection :: String -> { input :: Expr, facts :: RelevantFacts } -> Quarantine -> Quarantine
recordRejection key r (Quarantine m) = Quarantine (Map.insert key r m)

-- | Drop a binding's record — its NbE attempt succeeded (passed the cap).
clearRejection :: String -> Quarantine -> Quarantine
clearRejection key (Quarantine m) = Quarantine (Map.delete key m)

-- | The facts an NbE attempt on `seed` could consult: the keys reachable from the body's free
-- | variables (both atom spellings — plain vars and foreign refs), closed transitively through
-- | inline-candidate bodies, each mapped to the exact lookup answers. The visited set (the
-- | accumulator's keys) makes the worklist terminate on cyclic candidate graphs.
relevantFactsOf :: FactLookups -> Expr -> RelevantFacts
relevantFactsOf lookups seed = go Map.empty (List.fromFoldable (refsOf seed))
  where
  refsOf e = Set.union (fvExpr Set.empty e) (cfExpr e)

  go acc = case _ of
    Nil -> acc
    key : rest
      | Map.member key acc -> go acc rest
      | otherwise ->
          let
            entry = { candidate: lookups.candidate key, effect: lookups.effect key }
            frontier = case entry.candidate of
              Just c -> List.fromFoldable (refsOf c.body)
              Nothing -> Nil
          in
            go (Map.insert key entry acc) (frontier <> rest)

-- | Current answers over exactly the recorded key set (see `stillRejected`).
reproject :: FactLookups -> RelevantFacts -> RelevantFacts
reproject lookups = mapWithIndex \key _ -> { candidate: lookups.candidate key, effect: lookups.effect key }
