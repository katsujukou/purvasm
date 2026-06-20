module BinTree where

import Bench.List (List(..), append)
import Data.Semiring ((+), (*))
import Data.Ring ((-))
import Data.Ord ((<=))

data Tree = Leaf | Node Tree Int Tree

build :: Int -> Int -> Tree
build depth label =
  if depth <= 0 then Leaf
  else Node (build (depth - 1) (label * 2)) label (build (depth - 1) (label * 2 + 1))

dfsSum :: Tree -> Int
dfsSum t = case t of
  Leaf -> 0
  Node l v r -> v + dfsSum l + dfsSum r

bfsSum :: Tree -> Int
bfsSum root = go (Cons root Nil) 0
  where
  go queue acc = case queue of
    Nil -> acc
    Cons t rest -> case t of
      Leaf -> go rest acc
      Node l v r -> go (append rest (Cons l (Cons r Nil))) (acc + v)

-- depth-first / breadth-first node-sum of a balanced tree of given depth
runDfs :: Int -> Int
runDfs depth = dfsSum (build depth 1)

runBfs :: Int -> Int
runBfs depth = bfsSum (build depth 1)
