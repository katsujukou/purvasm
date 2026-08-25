-- | `Purvasm.Abi.Float64` is the inverse of the bit-exact `Number` serialisation
-- | (`Image.floatToJson` over `Int64Decimal.int64BitsDecimal`), so it is tested here, against the
-- | writer, rather than in isolation: what has to hold is that the pair is the identity on a
-- | `Number`, and either half can be right on its own while the pair is not.
-- |
-- | The interesting inputs are the ones where "close enough" and "exact" part company — negative
-- | zero, subnormals, both infinities, the 64-bit extremes — and the shapes a malformed image could
-- | present, which must be refused rather than approximated.
module Test.Unit.Purvasm.Abi.Float64 where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Abi.Float64 (bitsOfDecimal, numberOfBits)
import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)
import Purvasm.Number (floatBitsHi, floatBitsLo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | A `Number` through the whole serialisation: bits → decimal → bits → `Number`.
roundTrip :: Number -> Maybe Number
roundTrip f = numberOfBits <$> bitsOfDecimal (int64BitsDecimal { hi: floatBitsHi f, lo: floatBitsLo f })

-- | Round-trip identity, compared on the BITS: `0.0 == -0.0` is `true`, so comparing the values
-- | would let the one case most likely to break through.
survives :: Number -> Spec Unit
survives f = it ("round-trips " <> show f) case roundTrip f of
  Nothing -> fail ("refused its own serialisation: " <> show f)
  Just g -> do
    floatBitsHi g `shouldEqual` floatBitsHi f
    floatBitsLo g `shouldEqual` floatBitsLo f

spec :: Spec Unit
spec = describe "Purvasm.Abi.Float64" do
  describe "numberOfBits after int64BitsDecimal" do
    survives 0.0
    survives (-0.0)
    survives 1.0
    survives (-1.0)
    survives 0.5
    survives 2.5
    survives 0.1
    survives 1234567.891
    -- The exponent extremes, where the halving/doubling loop runs longest.
    survives 1.0e308
    survives (-1.0e308)
    -- The smallest normal and a subnormal: the subnormal path has no implicit leading 1, and taking
    -- the normal path for it would be off by a factor of two plus the missing bit.
    survives 2.2250738585072014e-308
    survives 1.0e-320

  describe "numberOfBits" do
    it "reads both infinities from the exponent-all-ones pattern" do
      -- 0x7FF0000000000000 and its sign-flipped twin.
      map (_ > 1.0e308) (numberOfBits <$> bitsOfDecimal "9218868437227405312") `shouldEqual` Just true
      map (_ < -1.0e308) (numberOfBits <$> bitsOfDecimal "-4503599627370496") `shouldEqual` Just true

    it "reads a NaN pattern as a NaN" do
      -- A NaN is not equal to itself, which is the only observable this can assert — and the only
      -- one a program can make either.
      map (\x -> x /= x) (numberOfBits <$> bitsOfDecimal "9221120237041090560") `shouldEqual` Just true

  describe "bitsOfDecimal" do
    it "reads the 64-bit extremes" do
      bitsOfDecimal "9223372036854775807" `shouldEqual` Just { hi: 2147483647, lo: -1 }
      bitsOfDecimal "-9223372036854775808" `shouldEqual` Just { hi: -2147483648, lo: 0 }

    it "reads a value spanning both halves" do
      bitsOfDecimal "10000000000" `shouldEqual` Just { hi: 2, lo: 1410065408 }

    it "refuses what is not a decimal integer, rather than reading a prefix" do
      -- A decimal *fraction* is the dangerous one: it is a plausible Number and an impossible
      -- encoding, and truncating at the dot would produce a completely different double.
      bitsOfDecimal "1.5" `shouldEqual` Nothing
      bitsOfDecimal "12abc" `shouldEqual` Nothing
      bitsOfDecimal "" `shouldEqual` Nothing
      bitsOfDecimal "-" `shouldEqual` Nothing
      bitsOfDecimal " 1" `shouldEqual` Nothing
      bitsOfDecimal "+1" `shouldEqual` Nothing
