-- | The primops' edge cases — the ones where the obvious spelling and the toolchain's semantics
-- | disagree, and where a divergence from boot would only show up as a wrong answer deep inside a
-- | benchmark: Euclidean division, the zero divisor, `Int`'s extremes, 32-bit wrapping, and
-- | `ToInt32`'s totality.
-- |
-- | These stay worth testing even though `Purvasm.VM.Prim` now delegates to the intrinsics rather
-- | than reimplementing them: they pin the *contract* the VM depends on, so a change on either side
-- | of that seam is caught here rather than in a benchmark's output.
module Test.Unit.Purvasm.VM.Prim (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (infinity, nan)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Purvasm.VM.Instruction (PrimOp(..))
import Purvasm.VM.Prim as VMPrim
import Purvasm.VM.Value (Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Apply a primop and read an `Int` back, so a test reads as arithmetic. `Nothing` means the
-- | operation did not produce an `Int` at all, which is itself a failure worth seeing.
evalInt :: PrimOp -> Array Value -> Aff (Maybe Int)
evalInt op args = liftEffect do
  result <- VMPrim.eval op args
  pure case result of
    VInt n -> Just n
    _ -> Nothing

spec :: Spec Unit
spec = describe "Purvasm.VM.Prim" do
  describe "integer division" do
    it "is Euclidean: the remainder is never negative" do
      evalInt ModInt [ VInt (-7), VInt 3 ] >>= shouldEqual (Just 2)
      evalInt ModInt [ VInt 7, VInt (-3) ] >>= shouldEqual (Just 1)
      evalInt DivInt [ VInt (-7), VInt 3 ] >>= shouldEqual (Just (-3))
      evalInt DivInt [ VInt 7, VInt (-3) ] >>= shouldEqual (Just (-2))

    it "makes a zero divisor total, not a fault" do
      evalInt DivInt [ VInt 7, VInt 0 ] >>= shouldEqual (Just 0)
      evalInt ModInt [ VInt 7, VInt 0 ] >>= shouldEqual (Just 0)

    it "satisfies a == b * div a b + mod a b" do
      let a = -7
      let b = 3
      d <- evalInt DivInt [ VInt a, VInt b ]
      m <- evalInt ModInt [ VInt a, VInt b ]
      ((+) <$> ((*) b <$> d) <*> m) `shouldEqual` Just a

    it "is correct at Int's minimum, where an intermediate would overflow" do
      -- Computing the quotient as `(a - mod a b) / b` wraps at `bottom` and answers 306783377.
      evalInt DivInt [ VInt (-2147483648), VInt 7 ] >>= shouldEqual (Just (-306783379))
      evalInt ModInt [ VInt (-2147483648), VInt 7 ] >>= shouldEqual (Just 5)
      -- `abs bottom` is `bottom`, so a remainder taken against `abs b` wraps here too.
      evalInt ModInt [ VInt 7, VInt (-2147483648) ] >>= shouldEqual (Just 7)

    it "wraps the one quotient that leaves Int's range" do
      -- `bottom / -1` is `2^31`, which is not an `Int`. ADR-0112 makes wrapping it the rule on every
      -- target, as a deliberate divergence from stock `purs` on JS; this case survives that record's
      -- implementation, which only moves *where* the normalisation happens.
      evalInt DivInt [ VInt (-2147483648), VInt (-1) ] >>= shouldEqual (Just (-2147483648))

  describe "bitwise operations" do
    it "masks the shift count to five bits" do
      evalInt ShlInt [ VInt 1, VInt 33 ] >>= shouldEqual (Just 2)
      evalInt ShrInt [ VInt 4, VInt 33 ] >>= shouldEqual (Just 2)

    it "keeps the zero-fill shift inside the signed 32-bit range" do
      -- JS `>>>` alone would answer 4294967295, which is not an `Int`; boot re-wraps, so we do too.
      evalInt ZshrInt [ VInt (-1), VInt 0 ] >>= shouldEqual (Just (-1))
      evalInt ZshrInt [ VInt (-1), VInt 1 ] >>= shouldEqual (Just 2147483647)

  describe "NumberToInt" do
    it "truncates toward zero" do
      evalInt NumberToInt [ VNumber 3.9 ] >>= shouldEqual (Just 3)
      evalInt NumberToInt [ VNumber (-3.9) ] >>= shouldEqual (Just (-3))

    it "is total on non-finite input" do
      evalInt NumberToInt [ VNumber nan ] >>= shouldEqual (Just 0)
      evalInt NumberToInt [ VNumber infinity ] >>= shouldEqual (Just 0)

    it "reduces modulo 2^32 into the signed range" do
      evalInt NumberToInt [ VNumber 4294967296.0 ] >>= shouldEqual (Just 0)
      evalInt NumberToInt [ VNumber 2147483648.0 ] >>= shouldEqual (Just (-2147483648))
