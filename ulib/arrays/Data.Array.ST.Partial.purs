-- | ulib SHADOW of `arrays`' `Data.Array.ST.Partial` (ADR-0038 / ADR-0039 / ADR-0058), targeting
-- | arrays 7.3.x.
-- |
-- | The registry FFI indexes the `STArray` directly (`xs[i]` / `xs[i] = a`), which assumes the
-- | JS-array representation. The `ulib` reified `STArray` as an `STRef`-wrapped buffer (ADR-0039), so
-- | that FFI operates on the wrong shape. This shadow reimplements `peek`/`poke` over
-- | `Data.Array.ST`'s public, bounds-checked API instead of the reified internals: the `STArray`
-- | newtype stays abstract, so the representation does not leak across the module boundary.
-- |
-- | The module's `Partial` constraint discharges `fromJust`, so no `unsafePartial` is needed and the
-- | exported signatures match the registry's exactly. Out-of-bounds access is the caller's contract to
-- | avoid (as upstream): within the contract the behaviour is identical to the registry; out of it,
-- | the bounds-checked `poke` no-ops rather than extending the array.
module Data.Array.ST.Partial
  ( peek
  , poke
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Maybe (fromJust)

-- | Read the value at the specified index in a mutable array.
peek :: forall h a. Partial => Int -> STArray h a -> ST h a
peek i xs = fromJust <$> STA.peek i xs

-- | Change the value at the specified index in a mutable array.
poke :: forall h a. Partial => Int -> a -> STArray h a -> ST h Unit
poke i a xs = void (STA.poke i a xs)
