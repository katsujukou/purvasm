-- | ulib SHADOW of `integers`' `Data.Int` (ADR-0038 / ADR-0041), targeting integers 6.0.0.
-- |
-- | `Int` and `Number` are the same runtime value on JS, so the registry's `toNumber`/
-- | `fromNumberImpl` foreigns do not *convert* — `toNumber` is the identity and
-- | `fromNumberImpl` is a range/integrality test. On this backend the two are distinct
-- | representations (ADR-0008), so they are reimplemented in PureScript over the
-- | `Purvasm.Int` conversion intrinsics (`toNumber`/`fromNumber` = `ToInt32`, ADR-0041) — no
-- | native leaf. The other foreigns are likewise reimplemented in PureScript over the
-- | `Purvasm.*` primitive base — `quot`/`rem` as truncating division via `ToInt32`, `pow` by
-- | repeated squaring, and `toStringAs`/`fromStringAsImpl` as base-`b` digit conversions over the
-- | `Purvasm.String` byte primitives (digits are ASCII). Depending on the primitive base rather
-- | than `Data.String.*` keeps `Data.Int` low in the layering and avoids the module cycle that
-- | `Data.String.CodePoints` (which imports `Data.Int.toStringAs`) would otherwise create. No
-- | `Data.Int` foreign remains; the public interface is unchanged.
module Data.Int
  ( fromNumber
  , ceil
  , floor
  , trunc
  , round
  , toNumber
  , fromString
  , Radix
  , radix
  , binary
  , octal
  , decimal
  , hexadecimal
  , base36
  , fromStringAs
  , toStringAs
  , Parity(..)
  , parity
  , even
  , odd
  , quot
  , rem
  , pow
  ) where

import Prelude

import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isFinite)
import Data.Number as Number
import Purvasm.Int as PI
import Purvasm.Number as PN
import Purvasm.String as PS

-- | Creates an `Int` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Int
fromNumber = fromNumberImpl Just Nothing

-- ulib shadow (was a foreign): `n` round-trips through `ToInt32` (`Purvasm.Int.fromNumber`)
-- iff it is an integer in the 32-bit range, so `toNumber (fromNumber n) == n` reproduces the
-- registry's `(n | 0) === n` test exactly (IEEE `==` matches JS `===` on `Number`).
fromNumberImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Number
  -> Maybe Int
fromNumberImpl just nothing n =
  let
    i = PI.fromNumber n
  in
    if PN.eq (PI.toNumber i) n then just i else nothing

-- | Convert a `Number` to an `Int`, by taking the closest integer equal to or
-- | less than the argument. Values outside the `Int` range are clamped, `NaN`
-- | and `Infinity` values return 0.
floor :: Number -> Int
floor = unsafeClamp <<< Number.floor

-- | Convert a `Number` to an `Int`, by taking the closest integer equal to or
-- | greater than the argument. Values outside the `Int` range are clamped,
-- | `NaN` and `Infinity` values return 0.
ceil :: Number -> Int
ceil = unsafeClamp <<< Number.ceil

-- | Convert a `Number` to an `Int`, by dropping the decimal.
-- | Values outside the `Int` range are clamped, `NaN` and `Infinity`
-- | values return 0.
trunc :: Number -> Int
trunc = unsafeClamp <<< Number.trunc

-- | Convert a `Number` to an `Int`, by taking the nearest integer to the
-- | argument. Values outside the `Int` range are clamped, `NaN` and `Infinity`
-- | values return 0.
round :: Number -> Int
round = unsafeClamp <<< Number.round

-- | Convert an integral `Number` to an `Int`, by clamping to the `Int` range.
-- | This function will return 0 if the input is `NaN` or an `Infinity`.
unsafeClamp :: Number -> Int
unsafeClamp x
  | not (isFinite x) = 0
  | x >= toNumber top = top
  | x <= toNumber bottom = bottom
  | otherwise = fromMaybe 0 (fromNumber x)

-- ulib shadow (was a foreign): every `Int` is exactly representable as a `Number`; the widen
-- is the `Purvasm.Int.toNumber` intrinsic (ADR-0041).
toNumber :: Int -> Number
toNumber = PI.toNumber

-- | Reads an `Int` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Int
fromString = fromStringAs (Radix 10)

-- | A type for describing whether an integer is even or odd.
-- |
-- | The `Ord` instance considers `Even` to be less than `Odd`.
-- |
-- | The `Semiring` instance allows you to ask about the parity of the results
-- | of arithmetical operations, given only the parities of the inputs. For
-- | example, the sum of an odd number and an even number is odd, so
-- | `Odd + Even == Odd`. This also works for multiplication, eg. the product
-- | of two odd numbers is odd, and therefore `Odd * Odd == Odd`.
-- |
-- | More generally, we have that
-- |
-- | ```purescript
-- | parity x + parity y == parity (x + y)
-- | parity x * parity y == parity (x * y)
-- | ```
-- |
-- | for any integers `x`, `y`. (A mathematician would say that `parity` is a
-- | *ring homomorphism*.)
-- |
-- | After defining addition and multiplication on `Parity` in this way, the
-- | `Semiring` laws now force us to choose `zero = Even` and `one = Odd`.
-- | This `Semiring` instance actually turns out to be a `Field`.
data Parity = Even | Odd

derive instance eqParity :: Eq Parity
derive instance ordParity :: Ord Parity

instance showParity :: Show Parity where
  show Even = "Even"
  show Odd = "Odd"

instance boundedParity :: Bounded Parity where
  bottom = Even
  top = Odd

instance semiringParity :: Semiring Parity where
  zero = Even
  add x y = if x == y then Even else Odd
  one = Odd
  mul Odd Odd = Odd
  mul _ _ = Even

instance ringParity :: Ring Parity where
  sub = add

instance commutativeRingParity :: CommutativeRing Parity

instance euclideanRingParity :: EuclideanRing Parity where
  degree Even = 0
  degree Odd = 1
  div x _ = x
  mod _ _ = Even

instance divisionRingParity :: DivisionRing Parity where
  recip = identity

-- | Returns whether an `Int` is `Even` or `Odd`.
-- |
-- | ``` purescript
-- | parity 0 == Even
-- | parity 1 == Odd
-- | ```
parity :: Int -> Parity
parity n = if even n then Even else Odd

-- | Returns whether an `Int` is an even number.
-- |
-- | ``` purescript
-- | even 0 == true
-- | even 1 == false
-- | ```
even :: Int -> Boolean
even x = x .&. 1 == 0

-- | The negation of `even`.
-- |
-- | ``` purescript
-- | odd 0 == false
-- | odd 1 == true
-- | ```
odd :: Int -> Boolean
odd x = x .&. 1 /= 0

-- | The number of unique digits (including zero) used to represent integers in
-- | a specific base.
newtype Radix = Radix Int

-- | The base-2 system.
binary :: Radix
binary = Radix 2

-- | The base-8 system.
octal :: Radix
octal = Radix 8

-- | The base-10 system.
decimal :: Radix
decimal = Radix 10

-- | The base-16 system.
hexadecimal :: Radix
hexadecimal = Radix 16

-- | The base-36 system.
base36 :: Radix
base36 = Radix 36

-- | Create a `Radix` from a number between 2 and 36.
radix :: Int -> Maybe Radix
radix n
  | n >= 2 && n <= 36 = Just (Radix n)
  | otherwise = Nothing

-- | Like `fromString`, but the integer can be specified in a different base.
-- |
-- | Example:
-- | ``` purs
-- | fromStringAs binary      "100" == Just 4
-- | fromStringAs hexadecimal "ff"  == Just 255
-- | ```
fromStringAs :: Radix -> String -> Maybe Int
fromStringAs = fromStringAsImpl Just Nothing

-- | The `quot` function provides _truncating_ integer division (see the
-- | documentation for the `EuclideanRing` class). It is identical to `div` in
-- | the `EuclideanRing Int` instance if the dividend is positive, but will be
-- | slightly different if the dividend is negative. For example:
-- |
-- | ```purescript
-- | div 2 3 == 0
-- | quot 2 3 == 0
-- |
-- | div (-2) 3 == (-1)
-- | quot (-2) 3 == 0
-- |
-- | div 2 (-3) == 0
-- | quot 2 (-3) == 0
-- | ```
-- ulib shadow (was a foreign): truncating division, exactly JS `x / y | 0` — a `Number` divide
-- then `ToInt32` (`Purvasm.Int.fromNumber`), which truncates toward zero and yields `0` when
-- `y == 0` (the quotient is non-finite). (`EuclideanRing`'s `div` differs for negative dividends.)
quot :: Int -> Int -> Int
quot x y = PI.fromNumber (PN.div (PI.toNumber x) (PI.toNumber y))

-- | The `rem` function provides the remainder after _truncating_ integer
-- | division (see the documentation for the `EuclideanRing` class). It is
-- | identical to `mod` in the `EuclideanRing Int` instance if the dividend is
-- | positive, but will be slightly different if the dividend is negative. For
-- | example:
-- |
-- | ```purescript
-- | mod 2 3 == 2
-- | rem 2 3 == 2
-- |
-- | mod (-2) 3 == 1
-- | rem (-2) 3 == (-2)
-- |
-- | mod 2 (-3) == 2
-- | rem 2 (-3) == 2
-- | ```
-- ulib shadow (was a foreign): the matching truncating remainder, `x - (x `quot` y) * y`
-- (JS `x % y`, whose result takes the sign of the dividend).
rem :: Int -> Int -> Int
rem x y = x - quot x y * y

-- | Raise an Int to the power of another Int.
-- ulib shadow (was a foreign): `Math.pow(x, y) | 0`. For `y >= 0`, exponentiation by squaring
-- with the `Int` multiply (which wraps at 32 bits, matching the final `| 0`). For `y < 0` the
-- result is a fraction truncated to `0`, except for the bases `1` and `-1`.
pow :: Int -> Int -> Int
pow x y
  | y < 0 =
      if x == 1 then 1
      else if x == -1 then (if even y then 1 else -1)
      else 0
  | otherwise = go 1 x y
      where
      go acc b e =
        if e == 0 then acc
        else go (if odd e then acc * b else acc) (b * b) (e / 2)

-- ulib shadow (was a foreign): parse an optional sign and base-`b` digits, accumulating in
-- `Number` (so an out-of-`Int32`-range value is rejected by `fromNumber`, matching the
-- registry's `(i | 0) === i` check rather than silently wrapping). Any non-digit, an
-- out-of-range digit, or no digits at all fails.
fromStringAsImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Radix
  -> String
  -> Maybe Int
fromStringAsImpl just nothing (Radix b) s =
  if start >= n then nothing
  else case go start 0.0 of
    Just mag -> case fromNumber (PN.mul sign mag) of
      Just i -> just i
      Nothing -> nothing
    Nothing -> nothing
  where
  n = PS.byteLength s
  firstByte = if n > 0 then PS.byteAt s 0 else 0
  -- skip an optional leading '+' (0x2B) / '-' (0x2D); `start` is the first digit index
  sign = if firstByte == 0x2D then -1.0 else 1.0
  start = if firstByte == 0x2B || firstByte == 0x2D then 1 else 0
  bNum = PI.toNumber b

  -- Accumulate base-`b` digits in `Number` (self-tail recursive over the bytes).
  go i acc =
    if i >= n then Just acc
    else case digitValue (PS.byteAt s i) of
      Just d | d < b -> go (i + 1) (PN.add (PN.mul acc bNum) (PI.toNumber d))
      _ -> Nothing

-- ulib shadow (was a foreign): `i.toString(radix)` — the base-`b` digits of `|i|`, with a
-- leading '-' for negatives. As in JS the radix is trusted to be 2..36.
toStringAs :: Radix -> Int -> String
toStringAs (Radix b) i =
  if i == 0 then "0"
  else if i < 0 then "-" <> digitsOf (negate i)
  else digitsOf i
  where
  digitsOf n = go n ""
  go n acc =
    if n == 0 then acc
    else go (n / b) (digitChar (n `mod` b) <> acc)

-- The character for a digit value `0 <= d < 36`, as a one-byte ASCII `String` (`0-9`, then `a-z`).
digitChar :: Int -> String
digitChar d = byteString (if d < 10 then 0x30 + d else 0x61 + d - 10)

-- A one-byte `String` holding byte `b` (0-255). Safe: index 0 is in range of the length-1 buffer.
byteString :: Int -> String
byteString b = PS.unsafeSetByte (PS.unsafeNew 1) 0 b

-- The value of an ASCII digit byte (`0-9`, `a-z`, `A-Z` case-insensitive), or `Nothing`.
digitValue :: Int -> Maybe Int
digitValue c =
  if c >= 0x30 && c <= 0x39 then Just (c - 0x30)
  else if c >= 0x61 && c <= 0x7A then Just (c - 0x61 + 10)
  else if c >= 0x41 && c <= 0x5A then Just (c - 0x41 + 10)
  else Nothing
