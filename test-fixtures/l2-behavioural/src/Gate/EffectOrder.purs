-- | ADR-0104 §2 behavioural-gate fixture: `Effect` ORDER. The stdout line sequence pins where every
-- | effect fires — a re-ordered, duplicated, or skipped effect (the four ADR-0034 failure modes)
-- | diverges from the execution oracle (the boot-VM leg and the fixture-owned expected trace)
-- | even when final values agree.
module Gate.EffectOrder where

import Prelude

import Data.Array (range)
import Data.Foldable (foldM, foldl, for_)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

main :: Effect Unit
main = do
  log "start"
  r <- Ref.new 0
  for_ (range 1 5) \i -> do
    Ref.modify_ (_ + i) r
    v <- Ref.read r
    log ("acc:" <> show i <> ":" <> show v)
  total <- Ref.read r
  log ("total=" <> show total)
  -- Effectful churn: enough per-iteration transient allocation (a fresh array + folded string)
  -- that the small-heap harness forces collections MID-effect-sequence; the boundary logs pin
  -- that the order survives them. The string CONTENT depends on the loop variable and its length
  -- lands in the checksum, so the allocation is observably required — a constant-hoisting
  -- optimiser cannot delete it without changing the printed totals.
  for_ (range 1 400) \i -> do
    let line = foldl (\acc j -> acc <> show (i * 7 + j)) "" (range 1 10)
    Ref.modify_ (_ + SCU.length line) r
    when (i `mod` 100 == 0) do
      v <- Ref.read r
      log ("churn:" <> show i <> ":" <> show v)
  s <- foldM
    ( \acc i -> do
        log ("fold:" <> show i)
        pure (acc <> show i)
    )
    ""
    (range 1 3)
  log ("s=" <> s)
  log "end"
