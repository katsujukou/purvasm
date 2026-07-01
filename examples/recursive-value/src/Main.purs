module Example.RecursiveValue.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

data Fib = Fib String (Int -> Int)

fibAnd :: Fib
fibAnd = Fib "fib" \n ->
  if n < 2 then n
  else (case fibAnd of Fib _ f -> f (n - 1)) + (case fibAnd of Fib _ f -> f (n - 2))

fib :: Int -> Int
fib n = case fibAnd of Fib _ f -> f n

main :: Effect Unit
main = do
  log $ "fib(1) = " <> show (fib 1) 
  log $ "fib(2) = " <> show (fib 2) 
  log $ "fib(3) = " <> show (fib 3) 
  log $ "fib(4) = " <> show (fib 4) 
  log $ "fib(5) = " <> show (fib 5) 
  log $ "fib(6) = " <> show (fib 6) 
  log $ "fib(7) = " <> show (fib 7) 
  log $ "fib(8) = " <> show (fib 8) 
  log $ "fib(9) = " <> show (fib 9) 
  log $ "fib(10) = " <> show (fib 10) 