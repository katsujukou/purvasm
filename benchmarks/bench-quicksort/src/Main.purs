-- | Allocation-heavy workload (ADR-0075): quicksort an `n`-element pseudo-random array by the
-- | filter/append formulation — the GC / transient-allocation cost centre. The LCG is fixed and
-- | pure, so the input (and the printed wrapping sum, invariant under sorting) is identical on
-- | every backend.
module Bench.Quicksort.Main where

import Prelude

import Bench.Common (sizeArg)
import Data.Array (filter, uncons)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Console (logShow)

-- A 30-bit LCG (positive, wraps identically everywhere).
lcg :: Int -> Int
lcg s = (s * 1103515245 + 12345) .&. 0x3FFFFFFF

gen :: Int -> Array Int
gen n = unfoldr step (Tuple n 42)
  where
  step (Tuple i s) =
    if i == 0 then Nothing
    else let s' = lcg s in Just (Tuple s' (Tuple (i - 1) s'))

qsort :: Array Int -> Array Int
qsort xs = case uncons xs of
  Nothing -> []
  Just { head: p, tail } ->
    qsort (filter (_ < p) tail)
      <> filter (_ == p) xs
      <> qsort (filter (_ > p) tail)

main :: Effect Unit
main = do
  n <- sizeArg
  let xs = gen n
  let sorted = qsort xs
  -- the sum is sort-invariant; printing it checks both the traversal and the sort's totality
  logShow (foldl (+) 0 sorted - foldl (+) 0 xs + Array.length sorted)
