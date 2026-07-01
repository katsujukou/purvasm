module Example.RecursiveValue.Main where

import Prelude

import Effect (Effect)
import Purvasm.Stdio (writeLine)

data Fib = Fib String (Int -> Int)

fibAnd :: Fib
fibAnd = Fib "fib" \n ->
  if n < 2 then n
  else (case fibAnd of Fib _ f -> f (n - 1)) + (case fibAnd of Fib _ f -> f (n - 2))

fib :: Int -> Int
fib n = case fibAnd of Fib _ f -> f n

main :: Effect Unit
main = do
  writeLine $ "fib(1) = " <> show (fib 1)
  writeLine $ "fib(2) = " <> show (fib 2)
  writeLine $ "fib(3) = " <> show (fib 3)
  writeLine $ "fib(4) = " <> show (fib 4)
  writeLine $ "fib(5) = " <> show (fib 5)
  writeLine $ "fib(6) = " <> show (fib 6)
  writeLine $ "fib(7) = " <> show (fib 7)
  writeLine $ "fib(8) = " <> show (fib 8)
  writeLine $ "fib(9) = " <> show (fib 9)
  writeLine $ "fib(10) = " <> show (fib 10)