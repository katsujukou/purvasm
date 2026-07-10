-- | FNV-1a-64 over a byte sequence, byte-for-byte with the runtime's `record::fnv1a_64` and boot's
-- | `codegen_llvm.ml` `fnv1a_64` (ADR-0069 §2): so a compile-time static record-label id / constructor
-- | tag equals a runtime hash of the same name. The 64-bit state and the `0x100000001b3` prime multiply
-- | are carried as a `{ hi, lo }` pair of 32-bit words (the same convention `Int64Decimal` renders), and
-- | the modular multiply is done on 16-bit limbs in exact `Number` arithmetic (products stay < 2^53).
-- |
-- | Pure and byte-source-agnostic (`fnv1a64Bytes` takes the UTF-8 bytes) so it has no dependency on the
-- | textual mangle layer — `Mangle` builds `labelId`/`ctorTag` on top of it.
module Purvasm.Compiler.Util.Fnv1a64
  ( I64
  , fnv1a64Bytes
  , mul64
  , unsignedCompareI64
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Int.Bits (and, shl, xor, zshr, (.|.))
import Data.Number (floor) as Num

-- | A 64-bit value as two 32-bit words (`hi`/`lo` hold the raw two's-complement bit patterns, the
-- | convention `Int64Decimal.int64BitsDecimal` consumes).
type I64 = { hi :: Int, lo :: Int }

-- | Assemble a 32-bit word from its high and low 16-bit halves (both `< 2^16`), so the FNV constants can
-- | be written without an out-of-range `Int` literal (`0xcbf29ce4 > 2^31`).
word32 :: Int -> Int -> Int
word32 hi16 lo16 = (hi16 `shl` 16) .|. lo16

-- | 64-bit modular multiply (`(a * b) mod 2^64`) on `{ hi, lo }` words. Each operand splits into four
-- | 16-bit limbs; the partial products (≤ `65535^2 ≈ 2^32`) and their column sums stay well under 2^53,
-- | so `Number` arithmetic is exact. Carries propagate limb-by-limb; the top carry out of limb 3 is
-- | dropped (mod 2^64).
mul64 :: I64 -> I64 -> I64
mul64 a b =
  let
    limb w s = toNumber ((w `zshr` s) `and` 0xffff)
    a0 = limb a.lo 0
    a1 = limb a.lo 16
    a2 = limb a.hi 0
    a3 = limb a.hi 16
    b0 = limb b.lo 0
    b1 = limb b.lo 16
    b2 = limb b.hi 0
    b3 = limb b.hi 16
    c0 = a0 * b0
    c1 = a0 * b1 + a1 * b0
    c2 = a0 * b2 + a1 * b1 + a2 * b0
    c3 = a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
    base = 65536.0
    carry x = Num.floor (x / base)
    keep x = x - base * carry x
    r0 = keep c0
    d1 = c1 + carry c0
    r1 = keep d1
    d2 = c2 + carry d1
    r2 = keep d2
    d3 = c3 + carry d2
    r3 = keep d3
  in
    { lo: round r0 .|. (round r1 `shl` 16)
    , hi: round r2 .|. (round r3 `shl` 16)
    }

-- | FNV-1a-64 over a byte array: `h = offset_basis; for each byte c: h = (h xor c) * prime`, the prime
-- | being `0x100000001b3`. The xor touches only the low byte of `lo`.
fnv1a64Bytes :: Array Int -> I64
fnv1a64Bytes = foldl step offsetBasis
  where
  offsetBasis = { hi: word32 0xcbf2 0x9ce4, lo: word32 0x8422 0x2325 }
  prime = { hi: 0x100, lo: 0x1b3 }
  step h c = mul64 (h { lo = h.lo `xor` c }) prime

-- | Unsigned comparison of two 64-bit values (the runtime `new_record` asserts strictly-ascending
-- | *unsigned* label ids, ADR-0069 §1). Compare `hi` unsigned, then `lo`; unsigned 32-bit compare is a
-- | signed compare after flipping the sign bit.
unsignedCompareI64 :: I64 -> I64 -> Ordering
unsignedCompareI64 a b = case cmpU a.hi b.hi of
  EQ -> cmpU a.lo b.lo
  o -> o
  where
  signBit = 1 `shl` 31
  cmpU x y = compare (x `xor` signBit) (y `xor` signBit)
