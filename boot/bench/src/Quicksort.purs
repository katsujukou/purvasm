module Quicksort where

import Bench.List (List(..), append, filter, down, sum)
import Data.Ord ((<), (>=))

qsort :: List Int -> List Int
qsort xs = case xs of
  Nil -> Nil
  Cons p rest ->
    append (qsort (filter (\x -> x < p) rest))
      (Cons p (qsort (filter (\x -> x >= p) rest)))

-- run n = sort a descending list [n..1] and sum it (worst-case-ish input)
run :: Int -> Int
run n = sum (qsort (down n))
