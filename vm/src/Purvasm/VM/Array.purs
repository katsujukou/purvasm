-- | Array operations over the shared cell every VM array is
-- | ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §3).
-- |
-- | purvasm arrays are mutable (`NewArray`/`SetArray`, ADR-0019) and `Ref` is a one-element array, so
-- | identity is observable and the storage lives behind a cell all aliases share. This module is the
-- | safe face of that: it bounds-checks, then reaches the storage through `Purvasm.Array`'s unchecked
-- | primitives — which are exactly the backend's array intrinsics, so the guest's `SetArray` costs one
-- | host `SetArray`.
-- |
-- | The `Promoted` case is unreachable until the FFI boundary lands (ADR-0111 §3): nothing else can
-- | construct it, and each operation says so by name rather than guessing a behaviour.
module Purvasm.VM.Array
  ( fromValues
  , index
  , length
  , new
  , toValues
  , write
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Purvasm.Array as PA
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Value (ArrayCell, ArrayStorage(..), Value(..))

-- | The cell for an already-built element vector (the `Array` instruction, a literal).
fromValues :: Array Value -> Effect ArrayCell
fromValues = Ref.new <<< Local

-- | A fresh length-`n` array. Its slots are filled with `VInt 0` rather than left undefined, matching
-- | boot — a guest that reads before writing then observes the same value on both interpreters
-- | instead of two different kinds of nonsense.
new :: Int -> Effect ArrayCell
new n
  | n < 0 = stuck ("array allocation with negative length: " <> show n)
  | otherwise = fromValues (fill 0 (PA.unsafeNew n))
      where
      fill i acc
        | i >= n = acc
        | otherwise = fill (i + 1) (PA.unsafeSet acc i (VInt 0))

length :: ArrayCell -> Effect Int
length cell = Ref.read cell >>= case _ of
  Local values -> pure (PA.length values)
  Promoted _ -> promotedUnreachable "length"

-- | The element at `i`, or `Nothing` when out of range — the caller decides whether that is a stuck
-- | program (`IndexArray`) or a non-match (a length test already established the shape).
index :: ArrayCell -> Int -> Effect (Maybe Value)
index cell i = Ref.read cell >>= case _ of
  Local values
    | i >= 0 && i < PA.length values -> pure (Just (PA.unsafeIndex values i))
    | otherwise -> pure Nothing
  Promoted _ -> promotedUnreachable "index"

-- | Write `v` at `i`, in place; `false` when out of range. The mutated array is written back to the
-- | cell so the update is ordered by a data dependency rather than by trusting the primitive's
-- | in-place behaviour.
write :: ArrayCell -> Int -> Value -> Effect Boolean
write cell i v = Ref.read cell >>= case _ of
  Local values
    | i >= 0 && i < PA.length values -> do
        Ref.write (Local (PA.unsafeSet values i v)) cell
        pure true
    | otherwise -> pure false
  Promoted _ -> promotedUnreachable "write"

-- | The current elements, for the operations that consume a whole array at once.
toValues :: ArrayCell -> Effect (Array Value)
toValues cell = Ref.read cell >>= case _ of
  Local values -> pure values
  Promoted _ -> promotedUnreachable "toValues"

promotedUnreachable :: forall a. String -> Effect a
promotedUnreachable op =
  stuck ("array " <> op <> ": a promoted array exists before the FFI boundary does (ADR-0111 §3)")
