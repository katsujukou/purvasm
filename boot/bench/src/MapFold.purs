module MapFold where

import Bench.List (range, map, foldr)
import Data.Semiring ((+), (*))

-- run n = sum (map (*2) [1..n]) via higher-order map/foldr
run :: Int -> Int
run n = foldr (\a b -> a + b) 0 (map (\x -> x * 2) (range 1 n))
