-- | ADR-0105 §5 slice-0 fixture: ROOTING STRESS. Long-lived HEAP values whose live ranges cross
-- | the hundreds of safepoints the churn loop fires, interleaved with per-iteration transients
-- | that die before the next safepoint. Under `PURVASM_GC_STRESS=1` every allocation collects, so
-- | EVERY crossing window contains a relocation, and every stressed value's FULL content reaches
-- | stdout — a value corrupted by a missing root changes the output (the §5 observability
-- | contract: the stress knob exercises the window, the readback observes it).
-- |
-- | The two rooting paths are verified INDEPENDENTLY on two separate allocations with distinct
-- | contents (aliasing one array through both paths would let a correctly-rooted path keep it
-- | alive and mask the other path's missing root):
-- |
-- |   * `base` — reachable only as a direct local (the activation-root path); its full
-- |     concatenation is printed.
-- |   * `capBase` — reachable only through a closure capture riding a `Ref` (the capture path;
-- |     the `Ref` keeps the optimiser from inlining the capture away); the closure's result is
-- |     the full content-dependent concatenation, also printed.
module Gate.RootStress where

import Prelude

import Data.Array (range)
import Data.Foldable (foldl, for_)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

main :: Effect Unit
main = do
  log "start"
  -- Long-range HEAP values, defined BEFORE the churn: contents depend on the index so nothing
  -- can be constant-folded; each element must survive every collection the churn fires below.
  -- `base` is read back ONLY directly (never captured).
  let base = map (\i -> "v" <> show (i * 13) <> "#") (range 1 40)
  -- A SEPARATE allocation with distinct contents, reachable ONLY through the closure capture
  -- below — never read directly again — so the capture path is load-bearing on its own.
  let capBase = map (\i -> "c" <> show (i * 17) <> "!") (range 1 40)
  fRef <- Ref.new (\pre -> foldl (\acc s -> acc <> s) pre capBase)
  r <- Ref.new 0
  -- Churn: per-iteration transient allocation (dead before the iteration's last safepoint) with
  -- loop-variable-dependent content feeding a running checksum.
  for_ (range 1 150) \i -> do
    let line = foldl (\acc j -> acc <> show (i * 7 + j)) "" (range 1 6)
    Ref.modify_ (_ + SCU.length line) r
    when (i `mod` 50 == 0) do
      v <- Ref.read r
      log ("churn:" <> show i <> ":" <> show v)
  churn <- Ref.read r
  log ("churn=" <> show churn)
  -- Readback, FULL content, per path. Direct path: the entire concatenation of `base` is
  -- printed — every byte of every stressed element lands in stdout, so no element's corruption
  -- can hide behind a prefix or a length.
  let full = foldl (\acc s -> acc <> s) "" base
  log ("full-len=" <> show (SCU.length full))
  log ("full=" <> full)
  -- Capture path: the closure's result is the full concatenation of `capBase` — content-
  -- dependent, so a same-length corruption of any captured element changes this line too.
  f <- Ref.read fRef
  log ("cap=" <> f "&")
  log "end"
