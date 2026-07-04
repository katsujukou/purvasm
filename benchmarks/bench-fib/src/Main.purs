-- | Call-heavy workload (ADR-0075): a tail loop of `n` iterations, each making a fixed burst of
-- | non-tail recursive calls — the generic-apply / TCE cost centre. Linear in `n`; the printed
-- | accumulator is the self-check.
module Bench.Fib.Main where

import Prelude

import Bench.Common (sizeArg)
import Effect (Effect)
import Effect.Console (logShow)

fib :: Int -> Int
fib k = if k < 2 then k else fib (k - 1) + fib (k - 2)

loop :: Int -> Int -> Int
loop i acc = if i == 0 then acc else loop (i - 1) (acc + fib 15)

main :: Effect Unit
main = do
  n <- sizeArg
  logShow (loop n 0)
