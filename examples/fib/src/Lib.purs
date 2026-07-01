module Example.Fib.Lib where

import Prelude

fib :: Int -> Int
fib = go 1 1
  where
  go a b n = if n <= 1 then a else go b (a + b) (n - 1)
