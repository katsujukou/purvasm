-- | `Number` (double) primitives the purvasm backend recognises as *intrinsics*
-- | (resolved to the machine primops; the JS foreigns below are used only by stock
-- | `purs` / `purs-backend-es` builds). Part of the `purvasm-base` primitive layer
-- | (ADR-0038). The seam the `ulib` `Data.Eq`/`Data.Ord`/`Data.Semiring` Number
-- | reimplementations build on.
module Purvasm.Number
  ( add
  , sub
  , mul
  , div
  , eq
  , lt
  ) where

foreign import add :: Number -> Number -> Number
foreign import sub :: Number -> Number -> Number
foreign import mul :: Number -> Number -> Number

-- | IEEE division — total (`1.0/0.0 = Infinity`, `0.0/0.0 = NaN`). On purvasm: the
-- | `DivNumber` intrinsic.
foreign import div :: Number -> Number -> Number

-- | IEEE equality: `NaN /= NaN` and `-0.0 == 0.0`. On purvasm: the `EqNumber` intrinsic.
foreign import eq :: Number -> Number -> Boolean

-- | IEEE less-than. On purvasm: the `LtNumber` intrinsic.
foreign import lt :: Number -> Number -> Boolean
