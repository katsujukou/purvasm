-- | ADR-0104 §2 behavioural-gate fixture: transient-garbage churn. Every loop allocates
-- | per-iteration garbage (arrays, strings, boxed folds) while keeping the live set small, so under
-- | the harness's deliberately small `PURVASM_HEAP_WORDS` the run must survive many collections —
-- | any rooting/relocation emission bug shows up as a wrong checksum or a crash, and the harness
-- | additionally asserts (via `PURVASM_STATS`) that collections actually fired.
module Gate.GcChurn where

import Prelude

import Data.Array (range)
import Data.Foldable (foldl)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)

sumTo :: Int -> Int
sumTo n = go 0 0
  where
  go acc i = if i > n then acc else go (acc + i) (i + 1)

-- | Per-iteration array garbage: each step builds and folds a fresh 20-element array.
arrChurn :: Int -> Int
arrChurn n = go 0 0
  where
  go acc i =
    if i >= n then acc
    else go (acc + foldl (+) 0 (range 1 20)) (i + 1)

-- | Per-iteration string garbage with a bounded live prefix: append then re-take, so the live
-- | string stays 10 bytes while every iteration allocates (append + slice — the ADR-0103 paths).
strChurn :: Int -> Int
strChurn n = go "" 0
  where
  go s i =
    if i >= n then SCU.length s
    else go (SCU.take 10 (s <> "abcdefghij")) (i + 1)

main :: Effect Unit
main = do
  log ("sum=" <> show (sumTo 10000))
  log ("arr=" <> show (arrChurn 2000))
  log ("str=" <> show (strChurn 2000))
