-- | `int64BitsDecimal` must match OCaml's `Int64.to_string` on the raw halves — the invariant
-- | behind bit-exact `Number` serialisation (`Image.floatToJson`). The 16-bit-limb long division
-- | and the two's-complement negation are exactly the subtle-arithmetic spots unit tests exist
-- | for: zero, carry-across-halves negation, the unsigned reading of `lo`, both 64-bit extremes,
-- | and real IEEE-754 bit patterns checked against `Int64.to_string (Int64.bits_of_float f)`.
module Test.Unit.Purvasm.Compiler.Util.Int64Decimal where

import Prelude

import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Util.Int64Decimal" do
  it "renders zero and small values" do
    int64BitsDecimal { hi: 0, lo: 0 } `shouldEqual` "0"
    int64BitsDecimal { hi: 0, lo: 1 } `shouldEqual` "1"
    int64BitsDecimal { hi: 0, lo: 1000000 } `shouldEqual` "1000000"

  it "reads `lo` unsigned and carries across the halves" do
    -- lo = 0xFFFFFFFF as a raw pattern is the unsigned 4294967295, not -1
    int64BitsDecimal { hi: 0, lo: -1 } `shouldEqual` "4294967295"
    int64BitsDecimal { hi: 1, lo: 0 } `shouldEqual` "4294967296"
    -- 10^10 = 0x2_540B_E400 spans both halves
    int64BitsDecimal { hi: 2, lo: 1410065408 } `shouldEqual` "10000000000"

  it "renders negatives via two's complement (carry at lo = 0)" do
    int64BitsDecimal { hi: -1, lo: -1 } `shouldEqual` "-1"
    int64BitsDecimal { hi: -1, lo: 0 } `shouldEqual` "-4294967296"
    int64BitsDecimal { hi: -1, lo: 1 } `shouldEqual` "-4294967295"

  it "renders both 64-bit extremes" do
    int64BitsDecimal { hi: 2147483647, lo: -1 } `shouldEqual` "9223372036854775807"
    -- the minimum negates to itself; its halves then read as the magnitude 2^63
    int64BitsDecimal { hi: bottom, lo: 0 } `shouldEqual` "-9223372036854775808"

  it "matches Int64.to_string on real IEEE-754 bit patterns" do
    -- 1.5  = 0x3FF8000000000000
    int64BitsDecimal { hi: 1073217536, lo: 0 } `shouldEqual` "4609434218613702656"
    -- -1.0 = 0xBFF0000000000000 (a *negative* bits value)
    int64BitsDecimal { hi: -1074790400, lo: 0 } `shouldEqual` "-4616189618054758400"
    -- 0.1  = 0x3FB999999999999A (all limbs busy)
    int64BitsDecimal { hi: 1069128089, lo: -1717986918 } `shouldEqual` "4591870180066957722"
