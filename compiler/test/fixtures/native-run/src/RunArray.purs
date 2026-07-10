module RunArray where

pick :: Array Int -> Int
pick a = case a of
  [] -> 0
  [ x ] -> x
  [ _, _, z ] -> z
  _ -> 99

answer :: Int
answer = pick [ 7, 8, 9 ]
