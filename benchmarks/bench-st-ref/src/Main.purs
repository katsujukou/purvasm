-- | The `ST`/`STRef` hot-loop workload (ADR-0099 GER Slice 5 instrument): the `Control.Monad.ST`
-- | twin of `bench-effect-ref`. `n` iterations of an `ST.for` loop whose body reads-modifies-writes
-- | an `STRef` (`STRef.modify`, i.e. `modifyImpl` = an `IndexArray` read then a `SetArray` write on
-- | the cell), the whole computation discharged by `ST.run`. This is the behaviour gate for Slice 5's
-- | `ST` reference-combinator lowering: `--opt == --no-opt == oracle` here proves `new`/`read`/`modify`/
-- | `run` lower correctly end-to-end. (Ordering of the read-modify-write is *not* what is at risk — the
-- | cell primops `IndexArray`/`SetArray` are `mtouch = true` pins, so the ADR-0096 motion hazard already
-- | forbids reordering them; the region-local *relaxation* is the deferred track.) Only even `i` are
-- | added, so the printed accumulator is `m·(m-1)` where `m = ⌈n/2⌉` — `249500` for `n = 1000` — the
-- | self-check (identical oracle to `bench-effect-ref`).
module Bench.STRef.Main where

import Prelude

import Bench.Common (sizeArg)
import Control.Monad.ST (for, run) as ST
import Control.Monad.ST.Ref (modify, new, read) as STRef
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  n <- sizeArg
  logShow $ ST.run do
    acc <- STRef.new 0
    ST.for 0 n \i ->
      when (i `mod` 2 == 0) do
        void (STRef.modify (_ + i) acc)
    STRef.read acc
