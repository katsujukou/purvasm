-- | ulib SHADOW of `foreign-object`'s `Foreign.Object.ST.Unsafe` (ADR-0038 / ADR-0044), targeting
-- | foreign-object 4.1.0.
module Foreign.Object.ST.Unsafe
  ( unsafeFreeze
  ) where

import Control.Monad.ST (ST)
import Foreign.Object (Object, freezeST)
import Foreign.Object.ST (STObject)

-- | Get the immutable object out of `ST` by reading the ref's current `Map`. (Unlike the
-- | registry's zero-copy coercion, later mutations of the mutable object do not affect the
-- | returned value, since the `Map` is persistent — within the `unsafe` contract, ADR-0044.)
unsafeFreeze :: forall a r. STObject r a -> ST r (Object a)
unsafeFreeze = freezeST
