-- | Array operations over the shared cell every VM array is
-- | ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §3).
-- |
-- | purvasm arrays are mutable (`NewArray`/`SetArray`, ADR-0019) and `Ref` is a one-element array, so
-- | identity is observable and the storage lives behind a cell all aliases share. This module is the
-- | safe face of that: it bounds-checks, then reaches the storage through `Purvasm.Array`'s unchecked
-- | primitives — which are exactly the backend's array intrinsics, so the guest's `SetArray` costs one
-- | host `SetArray`.
-- |
-- | Once an array has crossed to a leaf its cell holds a `Promoted` carrier, and every operation below
-- | routes to the runtime's accessors instead (ADR-0111 §3). That is the whole of the identity
-- | invariant in practice: after promotion the VM and the leaf are reading and writing ONE object, so
-- | a leaf's write is visible here and a `SetArray` here is visible to the leaf. Promotion is one-way
-- | and permanent, and every alias shares this cell, so no binding is left behind.
-- |
-- | An element read out of a promoted array comes back as a **carrier**: it is a runtime value, and
-- | decoding it would break the very invariant promotion protects (§3's "coming out"). Its origin
-- | says so, for a diagnostic that has to name something.
module Purvasm.VM.Array
  ( asCell
  , fromValues
  , index
  , length
  , new
  , toValues
  , write
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Purvasm.Array as PA
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Foreign as Foreign
import Purvasm.VM.Value (ArrayCell, ArrayStorage(..), Value(..))

-- | The cell to operate an array value through, whichever way it reached the VM (ADR-0111 §3).
-- |
-- | There are two entrances and one path. A `VArray` is already a cell — possibly promoted, possibly
-- | not. An array a **leaf returned** is a `VCarrier`: it never had a cell, because it was never
-- | VM-local. Wrapping it in a cell that is `Promoted` from birth costs no object and copies nothing
-- | — the cell forwards to the very array the leaf handed back — and it means `IndexArray`,
-- | `LengthArray` and, crucially, **`SetArray`** all reach it through the same accessors that a
-- | promoted VM array uses. The identity invariant is the same invariant either way: one object,
-- | every alias, in both directions.
-- |
-- | `Nothing` for anything else. A carrier that is not an array is NOT rejected here — the runtime's
-- | shape check catches it at the first accessor, which is where a demand belongs (§3).
asCell :: Value -> Effect (Maybe ArrayCell)
asCell = case _ of
  VArray cell -> pure (Just cell)
  VCarrier _ fv -> Just <$> Ref.new (Promoted fv)
  _ -> pure Nothing

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
  Promoted carrier -> pure (Foreign.arrayLength carrier)

-- | The element at `i`, or `Nothing` when out of range — the caller decides whether that is a stuck
-- | program (`IndexArray`) or a non-match (a length test already established the shape).
index :: ArrayCell -> Int -> Effect (Maybe Value)
index cell i = Ref.read cell >>= case _ of
  Local values
    | i >= 0 && i < PA.length values -> pure (Just (PA.unsafeIndex values i))
    | otherwise -> pure Nothing
  Promoted carrier
    | i >= 0 && i < Foreign.arrayLength carrier ->
        Just <<< VCarrier promotedOrigin <$> Foreign.readField carrier i
    | otherwise -> pure Nothing

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
  Promoted carrier
    | i >= 0 && i < Foreign.arrayLength carrier -> do
        -- The written value crosses the boundary too, by the same rules: a nested array is promoted
        -- rather than copied, so the structure stays one object all the way down.
        Foreign.writeField promotedOrigin carrier i v
        pure true
    | otherwise -> pure false

-- | The current elements, for the operations that consume a whole array at once. A promoted array's
-- | elements come back as carriers, one `pv_read_field` each — this is a snapshot of a mutable
-- | object, so it is only correct where the caller consumes it immediately.
toValues :: ArrayCell -> Effect (Array Value)
toValues cell = Ref.read cell >>= case _ of
  Local values -> pure values
  Promoted carrier -> do
    let n = Foreign.arrayLength carrier
    -- Stack-safe for the same reason the migration loop is: an `Effect` bind is a host call and the
    -- array is as long as the guest made it.
    tailRecM
      ( \acc ->
          if PA.length acc >= n then pure (Done acc)
          else do
            v <- Foreign.readField carrier (PA.length acc)
            pure (Loop (Array.snoc acc (VCarrier promotedOrigin v)))
      )
      []

-- | The origin reported for a value that came out of a promoted array. Promotion loses the key the
-- | array originally crossed at — the array is now shared by every alias and every later leaf — so
-- | this names the *mechanism* rather than pretending to name one call site.
promotedOrigin :: String
promotedOrigin = "a promoted array"
