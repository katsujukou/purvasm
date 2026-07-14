-- | Exercises `Data.String.Common`'s `toLower` / `toUpper` / `localeCompare` as a registry-compiled
-- | consumer: the ulib shadow's pure-PS case mapping and byte-order compare run on every native
-- | backend (VM / OCaml-native / LLVM-native), unlike before when `toUpper` had no native provider
-- | at all and blocked linking. Every case here uses a genuine simple (1:1) Unicode
-- | mapping the real host `String.prototype` agrees with, so identical output on the JS column
-- | is a meaningful oracle too (the floor's documented divergences, e.g. `ß`, are deliberately
-- | out of scope for an example — see `ulib/strings/ulib.json`'s `test.xfail`).
module Example.StringCaseDemo.Main where

import Prelude

import Data.String.Common (localeCompare, toLower, toUpper)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ("toUpper hello       = " <> toUpper "hello")
  log ("toLower HELLO       = " <> toLower "HELLO")
  log ("toUpper café        = " <> toUpper "café")
  log ("toLower CAFÉ        = " <> toLower "CAFÉ")
  log ("localeCompare a b   = " <> show (localeCompare "a" "b"))
  log ("localeCompare b a   = " <> show (localeCompare "b" "a"))
  log ("localeCompare a a   = " <> show (localeCompare "a" "a"))
