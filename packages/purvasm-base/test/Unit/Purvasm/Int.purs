-- | `Purvasm.Int`'s division contract
-- | ([ADR-0112](../../../../../docs/design-decisions/0112-int-32-bit-invariant-and-division-layering.md)).
-- |
-- | Two families, four operations, and the two places where a plausible implementation goes wrong:
-- | the boundary of `Int`'s range, and the zero divisor — where there is no mathematical answer at
-- | all, so the values are pinned by that record rather than derived.
-- |
-- | These run on the JS target, which is where the range leak was: `bottom / (-1)` is `2^31`, and
-- | stock `purs` returns it unwrapped. The native path has always wrapped, so agreement here is
-- | agreement between purvasm's own targets.
module Test.Unit.Purvasm.Int (main) where

import Prelude

import Effect (Effect)
import Purvasm.Int as PI
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

bottom' :: Int
bottom' = -2147483648

top' :: Int
top' = 2147483647

spec :: Spec Unit
spec = describe "Purvasm.Int" do
  describe "Euclidean div/mod" do
    it "gives a non-negative remainder whichever way the signs fall" do
      PI.div 7 3 `shouldEqual` 2
      PI.mod 7 3 `shouldEqual` 1
      PI.div (-7) 3 `shouldEqual` (-3)
      PI.mod (-7) 3 `shouldEqual` 2
      PI.div 7 (-3) `shouldEqual` (-2)
      PI.mod 7 (-3) `shouldEqual` 1
      PI.div (-7) (-3) `shouldEqual` 3
      PI.mod (-7) (-3) `shouldEqual` 2

    it "stays inside Int at the extremes" do
      -- The quotient here is `2^31`, one past `top`. It wraps, as every other overflow does.
      PI.div bottom' (-1) `shouldEqual` bottom'
      PI.div bottom' 7 `shouldEqual` (-306783379)
      PI.mod bottom' 7 `shouldEqual` 5
      PI.mod 7 bottom' `shouldEqual` 7
      PI.div top' (-1) `shouldEqual` (-top')
      -- The remainder of the same pair, so the boundary is pinned as a pair rather than by its
      -- quotient alone.
      PI.mod bottom' (-1) `shouldEqual` 0

    it "is total on a zero divisor" do
      PI.div 7 0 `shouldEqual` 0
      PI.mod 7 0 `shouldEqual` 0
      PI.div 0 0 `shouldEqual` 0

  describe "truncating quot/rem" do
    it "rounds toward zero, so the remainder takes the dividend's sign" do
      PI.quot 7 3 `shouldEqual` 2
      PI.rem 7 3 `shouldEqual` 1
      PI.quot (-7) 3 `shouldEqual` (-2)
      PI.rem (-7) 3 `shouldEqual` (-1)
      PI.quot 7 (-3) `shouldEqual` (-2)
      PI.rem 7 (-3) `shouldEqual` 1
      PI.quot (-7) (-3) `shouldEqual` 2
      PI.rem (-7) (-3) `shouldEqual` (-1)

    it "stays inside Int at the extremes" do
      PI.quot bottom' (-1) `shouldEqual` bottom'
      PI.quot bottom' 7 `shouldEqual` (-306783378)
      PI.rem bottom' 7 `shouldEqual` (-2)
      PI.rem bottom' (-1) `shouldEqual` 0

    it "is total on a zero divisor, with rem preserving the division identity" do
      PI.quot 7 0 `shouldEqual` 0
      -- `x == y * quot x y + rem x y` holds at `y == 0` only because `rem x 0 == x`.
      PI.rem 7 0 `shouldEqual` 7
      PI.rem 0 0 `shouldEqual` 0

    it "satisfies the division identity away from zero too" do
      let check x y = (y * PI.quot x y + PI.rem x y) `shouldEqual` x
      check 7 3
      check (-7) 3
      check 7 (-3)
      check (-7) (-3)
      check bottom' 7

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
