-- | Reading a `Number` back out of the bit-exact decimal a bytecode image writes.
-- |
-- | The writer's half is `Purvasm.Compiler.Util.Int64Decimal.int64BitsDecimal` over
-- | `Purvasm.Number.floatBitsHi`/`Lo` (ADR-0038 §4): a `Number` literal is serialised as the *signed
-- | 64-bit decimal spelling of its IEEE-754 bit pattern*, so that a `.pvm` carries the exact double
-- | and not a re-parsed approximation of it. This module is the inverse, and it lives beside
-- | `Mangle`/`Fnv1a64` for the reason those do: it is a fact about the artifact format, shared by
-- | whoever writes one and whoever reads one, and two derivations of it would drift.
-- |
-- | Everything here is ordinary arithmetic on `Int` halves — no `Math` leaf and no runtime API. The
-- | powers of two are reached by repeated halving/doubling rather than `pow`, which is exact for the
-- | same reason the loop terminates: every intermediate lies between the starting significand and the
-- | final value, so it is representable whenever the result is.
module Purvasm.Abi.Float64
  ( numberOfBits
  , bitsOfDecimal
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Int.Bits (complement, shl, zshr, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Array as Array
import Data.Char (toCharCode)

-- | The `Number` whose IEEE-754 bit pattern is `hi`/`lo` (the halves `floatBitsHi`/`Lo` produce).
-- |
-- | `NaN` is answered as *a* NaN rather than the exact payload: PureScript cannot construct a
-- | specific quiet-NaN bit pattern without a leaf, and no source program can observe the difference
-- | (`NaN /= NaN`, and `show` prints the same). Every other value — including both zeros, both
-- | infinities and every subnormal — comes back bit-identical.
numberOfBits :: { hi :: Int, lo :: Int } -> Number
numberOfBits { hi, lo } =
  let
    sign = if hi < 0 then -1.0 else 1.0
    biasedExponent = zshr hi 20 .&. 0x7FF
    -- The 52-bit fraction as an exact integer: the low 32 bits are a *bit pattern*, so they are read
    -- unsigned (`zshr` twice rather than a cast, since `Int` is signed).
    significand = toNumber (hi .&. 0xFFFFF) * 4294967296.0 + unsigned lo
  in
    if biasedExponent == 0x7FF then
      -- Overflow on purpose: with the exponent all ones, `scale` is asked for 2^972 on a significand
      -- of at least 2^52, which is past the largest double — so `huge` IS the infinity this pattern
      -- denotes, and `huge - huge` its NaN.
      --
      -- Both are computed from the decoded fields rather than written as constants, and that is not
      -- style: an expression the optimiser can fold to **NaN** never reaches a fixpoint (its literal
      -- compares unequal to itself), so `Nbe` exhausts its rewrite fuel on the binding. `v = 0.0 /
      -- 0.0` reproduces it on its own; ±Infinity folds fine. Deriving these from the arguments keeps
      -- the fold from having anything to reduce.
      let
        huge = scale (4503599627370496.0 + significand) (biasedExponent - 1075)
      in
        if significand == 0.0 then sign * huge else huge - huge
    else if biasedExponent == 0 then
      -- A subnormal has no implicit leading 1 and a fixed exponent of -1074.
      sign * scale significand (biasedExponent - 1074)
    else sign * scale (4503599627370496.0 + significand) (biasedExponent - 1075)

-- | `x * 2^n`, one factor of two at a time.
-- |
-- | Exact: each step multiplies or divides by two, and every intermediate lies between the starting
-- | significand and the result, so each is representable whenever the result is.
-- |
-- | Deliberately *not* exponentiation by squaring, and deliberately never called with a literal
-- | exponent. Both alternatives defeat the optimiser rather than the arithmetic: squaring binds a
-- | half and uses it twice, which the reducer duplicates at every level (2^depth, ADR-0097's
-- | multi-use small-deref clause), and a literal exponent lets it unfold this loop statically —
-- | 1074 times. Either one exhausts `Nbe`'s rewrite fuel on this binding, which is why the subnormal
-- | caller below subtracts from `biasedExponent` (known to be zero there, but not syntactically)
-- | instead of passing −1074.
scale :: Number -> Int -> Number
scale x n
  | n == 0 = x
  | n < 0 = scale (x / 2.0) (n + 1)
  | otherwise = scale (x * 2.0) (n - 1)

-- | A 32-bit bit pattern read as an unsigned value.
unsigned :: Int -> Number
unsigned n = toNumber (zshr n 16) * 65536.0 + toNumber (n .&. 0xFFFF)

-- | The 64-bit halves of a signed decimal, as `int64BitsDecimal` spells one. `Nothing` when the text
-- | is not such a spelling — a malformed image is refused, never approximated.
-- |
-- | The accumulation is mod 2^64, matching `Int64.of_string`: the writer only ever emits values in
-- | range, so wrapping is unreachable, but truncating silently would be the wrong failure and
-- | rejecting a legitimate `-9223372036854775808` would be another.
bitsOfDecimal :: String -> Maybe { hi :: Int, lo :: Int }
bitsOfDecimal text = case Array.uncons (toCharArray text) of
  Just { head: '-', tail } -> negate64 <$> digits tail
  _ -> digits (toCharArray text)
  where
  digits cs
    | Array.null cs = Nothing
    | otherwise = Array.foldM step { hi: 0, lo: 0 } cs

  step acc c =
    let
      d = toCharCode c - 48
    in
      if d < 0 || d > 9 then Nothing else Just (addSmall (times10 acc) d)

-- | `x * 10` on raw halves, through 16-bit limbs so no intermediate leaves `Int`'s range.
times10 :: { hi :: Int, lo :: Int } -> { hi :: Int, lo :: Int }
times10 { hi, lo } =
  let
    l0 = (lo .&. 0xFFFF) * 10
    l1 = zshr lo 16 * 10 + zshr l0 16
    l2 = (hi .&. 0xFFFF) * 10 + zshr l1 16
    l3 = zshr hi 16 * 10 + zshr l2 16
  in
    { hi: shl (l3 .&. 0xFFFF) 16 .|. (l2 .&. 0xFFFF)
    , lo: shl (l1 .&. 0xFFFF) 16 .|. (l0 .&. 0xFFFF)
    }

-- | `x + d` for a single digit, carrying into `hi`.
addSmall :: { hi :: Int, lo :: Int } -> Int -> { hi :: Int, lo :: Int }
addSmall { hi, lo } d =
  let
    l0 = (lo .&. 0xFFFF) + d
    l1 = zshr lo 16 + zshr l0 16
    carry = zshr l1 16
  in
    { hi: hi + carry, lo: shl (l1 .&. 0xFFFF) 16 .|. (l0 .&. 0xFFFF) }

-- | Two's-complement negation, the `+1` carrying into `hi` exactly when `lo` is zero — the mirror of
-- | `Int64Decimal.negate64`.
negate64 :: { hi :: Int, lo :: Int } -> { hi :: Int, lo :: Int }
negate64 { hi, lo }
  | lo == 0 = { hi: complement hi + 1, lo: 0 }
  | otherwise = { hi: complement hi, lo: complement lo + 1 }
