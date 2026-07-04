-- | `Number` (double) primitives the purvasm backend recognises as *intrinsics*
-- | (resolved to the machine primops) or provides as representation-level native
-- | leaves (the float-bits reads below); the JS foreigns are used only by stock
-- | `purs` / `purs-backend-es` builds. Part of the `purvasm-base` primitive layer
-- | (ADR-0038). The seam the `ulib` `Data.Eq`/`Data.Ord`/`Data.Semiring` Number
-- | reimplementations build on.
module Purvasm.Number
  ( add
  , sub
  , mul
  , div
  , eq
  , lt
  , floatBitsHi
  , floatBitsLo
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

-- | The high 32 bits of the `Number`'s IEEE-754 representation, reinterpreted as a 32-bit
-- | `Int` bit pattern (ADR-0038 §4's float-bits read): the representation-level primitive a
-- | bit-exact `Number` serialisation builds on — any *rendering* of the 64-bit value (e.g.
-- | its decimal spelling) is ordinary PureScript over the `Hi`/`Lo` pair. A pure read:
-- | deterministic, no rounding.
foreign import floatBitsHi :: Number -> Int

-- | The low 32 bits of the `Number`'s IEEE-754 representation, as `floatBitsHi`.
foreign import floatBitsLo :: Number -> Int
