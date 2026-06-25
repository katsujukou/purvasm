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
