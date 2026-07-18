-- | ADR-0104 §2 behavioural-gate fixture: the combination case — an `Effect` loop whose every
-- | iteration allocates record/array garbage (closures live across allocation points, so their
-- | captures must survive the collections the small-heap harness forces), plus ordinary
-- | records/filters as a value sanity check.
module Gate.Mixed where

import Prelude

import Data.Array (filter, range)
import Data.Foldable (for_, sum)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

type P = { x :: Int, y :: String }

mk :: Int -> P
mk i = { x: i * i, y: "p" <> show i }

main :: Effect Unit
main = do
  acc <- Ref.new 0
  -- the loop binder feeds the record contents, so every iteration's allocations are observably
  -- distinct — hoisting or reusing them would change the printed checksum.
  for_ (range 1 200) \i -> do
    let ps = map (\j -> mk (i + j)) (range 1 10)
    Ref.modify_ (_ + sum (map _.x ps)) acc
  v <- Ref.read acc
  log ("mixed=" <> show v)
  let evens = filter (\i -> i `mod` 2 == 0) (range 1 20)
  log ("evens=" <> show (sum evens))
