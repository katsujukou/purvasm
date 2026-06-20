module Fib where

import Data.Semiring ((+))
import Data.Ring ((-))
import Data.Ord ((<))

-- run n = nth Fibonacci number (exponential work in n)
run :: Int -> Int
run n = if n < 2 then n else run (n - 1) + run (n - 2)
