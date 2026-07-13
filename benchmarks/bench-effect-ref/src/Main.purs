-- | The `Effect`/`Ref` hot-loop workload (ADR-0099 GER measurement instrument): `n` iterations of a
-- | `forE` loop whose body is an `Effect` do-block reading and writing a `Ref`. Unlike `count-state`
-- | (whose hot loop is `State`/`Identity` dictionary dispatch), this is the **`Effect`-thunk** cost
-- | centre — the `bind`/`discard`/`pure` glue GER collapses, plus the `Ref` reads/writes and the
-- | `forE` structural combinator. Only even `i` are added, so the printed accumulator is
-- | `m·(m-1)` where `m = ⌈n/2⌉` (the count of even `i` in `[0,n)`) — e.g. `249500` for `n = 1000` —
-- | the self-check.
module Bench.EffectRef.Main where

import Prelude

import Bench.Common (sizeArg)
import Effect (Effect, forE)
import Effect.Console (logShow)
import Effect.Ref as Ref

main :: Effect Unit
main = do
  n <- sizeArg
  acc <- Ref.new 0
  forE 0 n \i ->
    -- `when` (Applicative `pure`) + `void` (Functor `map`) + `Ref.modify` (an effectful foreign),
    -- so the loop body spans the whole dictionary surface GER lowers, not only `bind`/`discard`.
    when (i `mod` 2 == 0) do
      void (Ref.modify (_ + i) acc)
  total <- Ref.read acc
  logShow total
