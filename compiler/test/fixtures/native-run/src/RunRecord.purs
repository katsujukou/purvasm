module RunRecord where

getA :: { a :: Int, b :: Int } -> Int
getA r = r.a

answer :: Int
answer =
  let
    base = { a: 10, b: 20 }
  in
    getA (base { a = 30 })
