-- | Decimal rendering of a 64-bit two's-complement integer given as two 32-bit `Int` bit-pattern
-- | halves — boot's `Int64.to_string`, in ordinary PureScript. The consumer is the bytecode
-- | image's bit-exact `Number` serialisation (`Image.floatToJson` over
-- | `Purvasm.Number.floatBitsHi`/`Lo`, ADR-0038 §4): PureScript's `Int` cannot hold the 64-bit
-- | value, so the rendering long-divides four 16-bit limbs by 10 — every intermediate
-- | (`remainder * 2^16 + limb <= 9 * 65536 + 65535`) fits comfortably in a 32-bit `Int`.
module Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Int.Bits (complement, zshr, (.&.))

-- | The decimal spelling of the signed 64-bit integer whose raw bit halves are `hi`/`lo`.
int64BitsDecimal :: { hi :: Int, lo :: Int } -> String
int64BitsDecimal halves@{ hi }
  | hi < 0 = "-" <> unsignedDecimal (negate64 halves)
  | otherwise = unsignedDecimal halves

-- Two's-complement negation on raw halves: `~x + 1`, the `+1` carrying into `hi` exactly when
-- `lo` is zero. The minimum value negates to itself, which is correct here: its magnitude 2^63
-- is then read as the *unsigned* value of the unchanged halves.
negate64 :: { hi :: Int, lo :: Int } -> { hi :: Int, lo :: Int }
negate64 { hi, lo }
  | lo == 0 = { hi: complement hi + 1, lo: 0 }
  | otherwise = { hi: complement hi, lo: complement lo + 1 }

-- Decimal of the unsigned 64-bit value: repeated long division of four 16-bit limbs
-- (most-significant first) by 10; each pass's remainder is the next least-significant digit.
unsignedDecimal :: { hi :: Int, lo :: Int } -> String
unsignedDecimal { hi, lo } = go "" [ zshr hi 16, hi .&. 0xFFFF, zshr lo 16, lo .&. 0xFFFF ]
  where
  go acc limbs
    | all (_ == 0) limbs = if acc == "" then "0" else acc
    | otherwise =
        let
          step { q, r } limb =
            let
              cur = r * 0x10000 + limb
            in
              { q: Array.snoc q (cur / 10), r: cur `mod` 10 }
          res = foldl step { q: [], r: 0 } limbs
        in
          go (show res.r <> acc) res.q
