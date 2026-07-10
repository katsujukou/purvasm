module RunCase where

data T = A Int | B Int

sel :: T -> Int
sel t = case t of
  A x -> x
  B y -> y

answer :: Int
answer = sel (B 55)
