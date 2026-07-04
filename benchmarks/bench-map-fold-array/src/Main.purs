-- | Array traversal workload (ADR-0075): build `1..n`, map, fold — the array-primop / iteration
-- | cost centre. The printed (wrapping) sum is the self-check: `Int` addition wraps identically
-- | on every backend (ADR-0006 32-bit semantics).
module Bench.MapFoldArray.Main where

import Prelude

import Bench.Common (sizeArg)
import Data.Array (range)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  n <- sizeArg
  logShow (foldl (+) 0 (map (_ * 2) (range 1 n)))
