module Bench.RunStateExcept.Main where

import Prelude

import Bench.Common (sizeArg)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console as Console
import Run (Run)
import Run as Run
import Run.Except (catch, throw)
import Run.State (STATE, get, modify, put, runState)
import Type.Row (type (+))

type S = { fizz :: Int, buzz :: Int }

data E = Fizz | Buzz | FizzBuzz

program :: Int -> Run (STATE S + ()) (List String)
program fin = loop Nil 0 <#> reverse
  where
  loop acc n = do
    if n > fin then pure acc
    else do
      s0 <- get
      c <- step n
        <#> show
        # catch case _ of
            Fizz -> modify (_ { fizz = s0.fizz + 1 }) $> "fizz"
            Buzz -> modify (_ { buzz = s0.buzz + 1 }) $> "buzz"
            FizzBuzz -> put { fizz: s0.fizz + 1, buzz: s0.buzz + 1 } $> "fizzbuzz"
      loop (c : acc) (n + 1)

  step n = do
    when (n `mod` 15 == 0) do
      throw FizzBuzz
    when (n `mod` 3 == 0) do
      throw Fizz
    when (n `mod` 5 == 0) do
      throw Buzz

    pure n

main :: Effect Unit
main = do
  n <- sizeArg
  let
    fi /\ acc =
      program n
        # runState { fizz: 0, buzz: 0 }
        # Run.extract
  Console.logShow { final: fi, steps: List.length acc }
