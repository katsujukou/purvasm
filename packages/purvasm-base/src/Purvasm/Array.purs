-- | The first-order array primitives the higher-order array combinators (in `ulib`) are
-- | built on. They resolve to the backend's array primops as intrinsics; the sibling
-- | `Array.js` provides them for stock `purs` / `purs-backend-es` (dual-target). Part of
-- | the `purvasm-base` primitive layer (ADR-0038).
module Purvasm.Array
  ( length
  , unsafeIndex
  , unsafeNew
  , unsafeSet
  ) where

-- | The element count. On purvasm: the `LengthArray` intrinsic.
foreign import length :: forall a. Array a -> Int

-- | The element at index `i`, **unchecked** (out of range is stuck). On purvasm:
-- | the `IndexArray` intrinsic.
foreign import unsafeIndex :: forall a. Array a -> Int -> a

-- | Allocate a length-`n` array (its slots are written before being read). On purvasm:
-- | the `NewArray` intrinsic.
foreign import unsafeNew :: forall a. Int -> Array a

-- | Write `v` at index `i`, **mutating the array in place**, and return that same array, so
-- | a builder loop threads it (keeping the write live and ordered by the data dependency,
-- | without an effect). Unchecked. On purvasm: the `SetArray` intrinsic.
foreign import unsafeSet :: forall a. Array a -> Int -> a -> Array a
