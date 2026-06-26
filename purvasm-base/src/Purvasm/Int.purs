-- | `Int` primitives the purvasm backend recognises as *intrinsics* (resolved to the
-- | machine primops; the JS foreigns below are used only by stock `purs` /
-- | `purs-backend-es` builds). Part of the `purvasm-base` primitive layer (ADR-0038).
module Purvasm.Int
  ( add
  , sub
  , mul
  , eq
  , lt
  , div
  , mod
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

foreign import add :: Int -> Int -> Int
foreign import sub :: Int -> Int -> Int
foreign import mul :: Int -> Int -> Int
foreign import eq :: Int -> Int -> Boolean

-- | Signed less-than. On purvasm: the `LtInt` intrinsic.
foreign import lt :: Int -> Int -> Boolean

-- | Euclidean integer division / remainder — non-negative remainder, `0` on a zero
-- | divisor — matching `Prelude`'s `EuclideanRing Int` (4.x+; truncating division is
-- | `quot`/`rem`, not these). On purvasm: the `DivInt` / `ModInt` intrinsics.
foreign import div :: Int -> Int -> Int
foreign import mod :: Int -> Int -> Int

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
