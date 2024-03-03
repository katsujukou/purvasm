module Sample.ForeignPrim where

import Data.Ord ((<))
import Data.Boolean (otherwise)
import Data.Semiring ((+))
import Data.Ring ((-))

fib :: Int -> Int
fib n
  | n < 2 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

f :: Int
f = fib 42
