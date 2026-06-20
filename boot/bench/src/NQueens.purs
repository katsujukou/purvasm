module NQueens where

import Bench.List (List(..), range, length, foldr, append)
import Data.Eq ((==))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.HeytingAlgebra ((||))

safe :: Int -> Int -> List Int -> Boolean
safe d q qs = case qs of
  Nil -> true
  Cons x rest ->
    if (x == q) || (x - q == d) || (q - x == d) then false
    else safe (d + 1) q rest

extend :: Int -> List Int -> List (List Int)
extend n sol =
  foldr (\q acc -> if safe 1 q sol then Cons (Cons q sol) acc else acc) Nil (range 1 n)

solve :: Int -> Int -> List (List Int)
solve n k =
  if k == 0 then Cons Nil Nil
  else foldr (\sol acc -> append (extend n sol) acc) Nil (solve n (k - 1))

-- run n = number of solutions to the n-queens problem
run :: Int -> Int
run n = length (solve n n)
