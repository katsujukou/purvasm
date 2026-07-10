module RunGuard where

pick :: Boolean -> Int
pick b = case b of
  _ | b -> 100
  _ -> 200

answer :: Int
answer = pick false
