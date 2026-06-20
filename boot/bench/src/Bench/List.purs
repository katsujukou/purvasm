module Bench.List where

import Data.Semiring ((+))
import Data.Ring ((-))
import Data.Ord ((<=))

data List a = Nil | Cons a (List a)

range :: Int -> Int -> List Int
range lo hi = if lo <= hi then Cons lo (range (lo + 1) hi) else Nil

down :: Int -> List Int
down n = if n <= 0 then Nil else Cons n (down (n - 1))

append :: forall a. List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons h t -> Cons h (append t ys)

map :: forall a b. (a -> b) -> List a -> List b
map f xs = case xs of
  Nil -> Nil
  Cons h t -> Cons (f h) (map f t)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p xs = case xs of
  Nil -> Nil
  Cons h t -> if p h then Cons h (filter p t) else filter p t

foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr f z xs = case xs of
  Nil -> z
  Cons h t -> f h (foldr f z t)

length :: forall a. List a -> Int
length xs = case xs of
  Nil -> 0
  Cons _ t -> 1 + length t

sum :: List Int -> Int
sum xs = foldr (\a b -> a + b) 0 xs
