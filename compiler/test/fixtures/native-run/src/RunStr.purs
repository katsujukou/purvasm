module RunStr where

classify :: String -> Int
classify s = case s of
  "foo" -> 1
  "bar" -> 2
  _ -> 0

answer :: Int
answer = classify "bar"
