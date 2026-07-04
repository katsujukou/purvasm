-- | The State-monad counting workload (ADR-0075): `n` `get`/`put` steps through `tailRecM`, so
-- | every backend runs the same stack-safe shape — the dictionary-dispatch / bind-chain cost
-- | centre. The printed final state (= `n`) is the self-check.
module Bench.CountState.Main where

import Prelude

import Bench.Common (sizeArg)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, execState, get, put)
import Effect (Effect)
import Effect.Console (logShow)

count :: Int -> State Int Unit
count = tailRecM go
  where
  go i =
    if i == 0 then pure (Done unit)
    else do
      s <- get
      put (s + 1)
      pure (Loop (i - 1))

main :: Effect Unit
main = do
  n <- sizeArg
  logShow (execState (count n) 0)
