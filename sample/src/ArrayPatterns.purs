module Sample.ArrayPatterns where

f :: Array String -> String
f = case _ of
  [] -> "0"
  [ "x" ] -> "1"
  [ x, "y" ] -> x
  [ _, y ] -> y
  [ x ] -> x
  [ x, "a", _ ] -> "foo"
  [ _, y, "z" ] -> "bar"
  [ _, _, z ] -> "baz"
  xs -> "8"