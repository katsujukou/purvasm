-- | ulib SHADOW of `foreign-object`'s `Foreign.Object.Unsafe` (ADR-0038 / ADR-0044), targeting
-- | foreign-object 4.1.0.
module Foreign.Object.Unsafe
  ( unsafeIndex
  ) where

import Data.Maybe (fromJust)
import Foreign.Object (Object, lookup)
import Partial.Unsafe (unsafePartial)

-- | Unsafely get the value for a key in an object. Does not check that the key is present
-- | (a missing key crashes via `fromJust`).
unsafeIndex :: forall a. Object a -> String -> a
unsafeIndex o k = unsafePartial (fromJust (lookup k o))
