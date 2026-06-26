-- | ulib SHADOW of `foreign-object`'s `Foreign.Object.ST` (ADR-0038 / ADR-0044), targeting
-- | foreign-object 4.1.0.
-- |
-- | A mutable object is an `STRef` over the persistent `Data.Map` (the `STArray` shape,
-- | ADR-0039): `poke`/`delete` write an updated `Map` back into the ref. `STObject` stays
-- | abstract; the public interface is unchanged.
module Foreign.Object.ST
  ( module ReExport
  , new
  , peek
  , poke
  , delete
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Internal as STI
import Data.Map as M
import Data.Maybe (Maybe)
import Foreign.Object.Internal (STObject(..))
import Foreign.Object.Internal (STObject) as ReExport

-- | Create a new, empty mutable object.
new :: forall a r. ST r (STObject r a)
new = STObject <$> STI.new M.empty

-- | Get the value for a key in a mutable object.
peek :: forall a r. String -> STObject r a -> ST r (Maybe a)
peek k (STObject ref) = M.lookup k <$> STI.read ref

-- | Update the value for a key in a mutable object.
poke :: forall a r. String -> a -> STObject r a -> ST r (STObject r a)
poke k v obj@(STObject ref) = STI.modify (M.insert k v) ref $> obj

-- | Remove a key and the corresponding value from a mutable object.
delete :: forall a r. String -> STObject r a -> ST r (STObject r a)
delete k obj@(STObject ref) = STI.modify (M.delete k) ref $> obj
