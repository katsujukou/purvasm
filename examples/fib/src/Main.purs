module Example.Fib.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Example.Fib.Lib (fib)

main :: Effect Unit
main = do
  logShow $ fib 1
  logShow $ fib 2
  logShow $ fib 3
  logShow $ fib 4
  logShow $ fib 5
  logShow $ fib 6
  logShow $ fib 7