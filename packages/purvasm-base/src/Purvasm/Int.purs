-- | The low-level `Int` API of the `purvasm-base` primitive layer (ADR-0038).
-- |
-- | Most members are **intrinsics**: the purvasm backend resolves them to machine primops, and the
-- | JS foreigns beside them are used only by stock `purs` / `purs-backend-es` builds. A few are
-- | **derived** — ordinary definitions built on those intrinsics, where the machine has no primop —
-- | and each member below says which it is.
-- |
-- | This module sits *below* `Prelude` and deliberately does not privilege one division family: the
-- | Euclidean `div`/`mod` and the truncating `quot`/`rem` are peers here, and it is `Prelude`'s
-- | `EuclideanRing Int` instance (and `Data.Int`) that choose between them (ADR-0112 §3).
-- |
-- | Every member that **returns an `Int`** preserves the type's range: its result is `ToInt32` of the
-- | mathematical answer, so nothing here can hand back a value that is not an `Int` (ADR-0112 §1).
-- | Where there is no mathematical answer — division by zero — the value is not derived but *pinned*
-- | by that record: `div`/`mod`/`quot` give `0`, and `rem x 0` gives `x`.
module Purvasm.Int
  ( add
  , sub
  , mul
  , eq
  , lt
  , div
  , mod
  , quot
  , rem
  , and
  , or
  , xor
  , shl
  , shr
  , zshr
  , complement
  , toNumber
  , fromNumber
  ) where

import Purvasm.Number as Number

foreign import add :: Int -> Int -> Int
foreign import sub :: Int -> Int -> Int
foreign import mul :: Int -> Int -> Int
foreign import eq :: Int -> Int -> Boolean

-- | Signed less-than. On purvasm: the `LtInt` intrinsic.
foreign import lt :: Int -> Int -> Boolean

-- | Euclidean integer division / remainder — non-negative remainder, `0` on a zero
-- | divisor — matching `Prelude`'s `EuclideanRing Int` (4.x+; truncating division is
-- | `quot`/`rem`, not these). On purvasm: the `DivInt` / `ModInt` intrinsics.
-- |
-- | `div bottom (-1)` is `bottom`: the mathematical quotient `2^31` is not an `Int`, so it wraps
-- | like every other overflow. Stock `purs` answers `2147483648` here; ADR-0112 records the
-- | divergence and why purvasm declines to reproduce it.
foreign import div :: Int -> Int -> Int
foreign import mod :: Int -> Int -> Int

-- | Truncating integer division / remainder — toward zero, `quot x 0 == 0` and `rem x 0 == x` — the
-- | peer of the Euclidean pair above, and what `Data.Int` exposes. **Derived**, not intrinsic: the
-- | `Number` round trip is what truncates, and `fromNumber` (`ToInt32`) is what both keeps the result
-- | an `Int` and makes the zero divisor total, since `x / 0` is non-finite and `ToInt32` of a
-- | non-finite is `0`. `rem` is then the remainder that satisfies `x == y * quot x y + rem x y`,
-- | including at `y == 0` (ADR-0112 §1).
quot :: Int -> Int -> Int
quot x y = fromNumber (Number.div (toNumber x) (toNumber y))

rem :: Int -> Int -> Int
rem x y = sub x (mul (quot x y) y)

-- | Bitwise operations on the signed 32-bit `Int` — the seam `Data.Int.Bits` is built over.
-- | On purvasm: the `AndInt` / `OrInt` / `XorInt` intrinsics.
foreign import and :: Int -> Int -> Int
foreign import or :: Int -> Int -> Int
foreign import xor :: Int -> Int -> Int

-- | Bitwise shifts. The shift count is taken mod 32. `shr` is arithmetic (sign-propagating)
-- | and `zshr` is logical (zero-fill). On purvasm: the `ShlInt` / `ShrInt` / `ZshrInt`
-- | intrinsics.
foreign import shl :: Int -> Int -> Int
foreign import shr :: Int -> Int -> Int
foreign import zshr :: Int -> Int -> Int

-- | Bitwise complement (one's complement). `complement n == -n - 1`, so `complement top`
-- | yields `bottom`. On purvasm: the `ComplementInt` intrinsic.
foreign import complement :: Int -> Int

-- | Widen an `Int` to a `Number`; every `Int` is representable exactly. On purvasm: the
-- | `IntToNumber` intrinsic (ADR-0041). (On JS `Int` and `Number` are one value, so the
-- | foreign is the identity.)
foreign import toNumber :: Int -> Number

-- | Narrow a `Number` to an `Int` by the ECMAScript `ToInt32` coercion (the JS `n | 0`):
-- | truncate toward zero, reduce mod 2^32, signed; `NaN`/`Infinity` give `0`. Total — it
-- | does NOT check integrality or range (that is the caller's job, e.g. `Data.Int.fromNumber`
-- | tests `toNumber (fromNumber n) == n`). On purvasm: the `NumberToInt` intrinsic (ADR-0041).
foreign import fromNumber :: Number -> Int
