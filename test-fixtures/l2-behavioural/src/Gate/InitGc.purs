-- | ADR-0106 slice-2 fixture: GC INSIDE A `Gcaf` INIT, with a LOAD-BEARING order (the ADR
-- | Verification pin — a mere "a collection happened during init" would be vacuous):
-- |
-- |   1. `kept` — a heap value built FIRST inside the top-level CAF's init body;
-- |   2. SUBSEQUENT allocations (the churn fold) force collections (under the gate's small
-- |      heap, and at every allocation under `PURVASM_GC_STRESS=1`) — so `kept` survives
-- |      only by being ROOTED across them, never by allocation-order luck;
-- |   3. `kept`'s FULL CONTENT feeds the CAF's value, observed through the permanent root
-- |      (`@…$root`) after init returns — a value corrupted by a missing init-body root
-- |      changes stdout, not just crashes.
-- |
-- | Under the plan-driven `Gcaf` init (ADR-0106 slice 2) `kept` crosses the churn's
-- | safepoints, so the plan must keep the init framed and `kept` rooted — this fixture is
-- | the init-tier sibling of `Gate.RootStress`'s activation-tier readback.
module Gate.InitGc where

import Prelude

import Data.Array (range)
import Data.Foldable (foldl)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)

-- The Gcaf under test. `kept`'s contents are index-dependent (nothing constant-folds);
-- `churnLen` allocates hundreds of transient strings AFTER `kept` exists; the final value
-- reads back `kept`'s full content only after that churn.
keptReadback :: String
keptReadback =
  let
    kept = map (\i -> "k" <> show (i * 11) <> ";") (range 1 40)
    churnLen = SCU.length (foldl (\acc j -> acc <> show (j * 3)) "" (range 1 2000))
  in
    foldl (\acc s -> acc <> s) ("churn" <> show churnLen <> ":") kept

main :: Effect Unit
main = do
  log "init-gc:start"
  log keptReadback
  log "init-gc:done"
