-- | ulib-internal representation for the mutable `STObject` (ADR-0044). An added helper module
-- | (permitted by ADR-0038): it holds the `STObject` newtype and its constructor so
-- | `Foreign.Object` (which converts `Object` ↔ `STObject` in `thawST`/`freezeST`/`runST`) and
-- | `Foreign.Object.ST` (which builds `STObject`) can share the representation while
-- | `Foreign.Object.ST` re-exports the type **abstractly**. Not for public use.
-- |
-- | `Object` itself is defined in `Foreign.Object` (so its type-class instances are not orphans).
module Foreign.Object.Internal
  ( STObject(..)
  ) where

import Control.Monad.ST (Region)
import Control.Monad.ST.Internal (STRef)
import Data.Map (Map)

-- | A mutable object: an `STRef` over the persistent `Map` (the `STArray` shape, ADR-0039).
newtype STObject :: Region -> Type -> Type
newtype STObject r a = STObject (STRef r (Map String a))
