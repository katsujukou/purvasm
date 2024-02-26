module Sample.RecordPatterns where

f :: { x :: Int, y :: Boolean, z :: String } → Int
f = case _ of
  { x: 1, y: true, z: "foo" } -> 0
  { x, y: false } -> x
  { y, z: "bar" } -> intOfBoolean y
  { x: 42 } -> 2
  _ -> 4

-- { y: true, z } -> intOfString z

intOfBoolean :: Boolean -> Int
intOfBoolean = if _ then 1 else 0

intOfString :: String -> Int
intOfString "" = 0
intOfString _ = 1