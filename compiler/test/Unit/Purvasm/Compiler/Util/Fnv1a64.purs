-- | FNV-1a-64 must be byte-for-byte with the runtime's `record::fnv1a_64` and boot's `fnv1a_64` — the
-- | invariant behind cross-module record-label ids and constructor tags matching a runtime hash of the
-- | same name (ADR-0069 §2). The 16-bit-limb 64-bit multiply is exactly the subtle-arithmetic spot a
-- | unit test exists for, so it is pinned against the canonical FNV-1a-64 vectors plus real qualified
-- | names (reference u64/i64/tag computed with 64-bit `BigInt`).
module Test.Unit.Purvasm.Compiler.Util.Fnv1a64 where

import Prelude

import Purvasm.Compiler.Backend.LLVM.Mangle (ctorTag, labelId)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Util.Fnv1a64" do
  -- `labelId` renders the full 64-bit hash as a signed-i64 decimal (`int64BitsDecimal ∘ fnv1a64`).
  it "matches the canonical FNV-1a-64 vectors (signed-i64 decimal)" do
    labelId "" `shouldEqual` "-3750763034362895579"
    labelId "a" `shouldEqual` "-5808556873153909620"
    labelId "foobar" `shouldEqual` "-8821353812377114648"

  it "hashes real qualified names (both signed-decimal branches)" do
    labelId "Data.Maybe.Just" `shouldEqual` "1003180780154307117"
    labelId "Data.Maybe.Nothing" `shouldEqual` "-7001385846044247260"
    labelId "name" `shouldEqual` "-4270347329889690746"
    labelId "value" `shouldEqual` "8999596768310594794"

  -- `ctorTag` is the low 31 bits of the same hash (a positive `Int`).
  it "masks the tag to 31 bits" do
    ctorTag "" `shouldEqual` 69346085
    ctorTag "a" `shouldEqual` 100789388
    ctorTag "foobar" `shouldEqual` 2000250856
    ctorTag "Data.Maybe.Just" `shouldEqual` 1050198573
    ctorTag "Data.Maybe.Nothing" `shouldEqual` 2132130596
