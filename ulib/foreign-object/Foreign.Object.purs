-- | ulib SHADOW of `foreign-object`'s `Foreign.Object` (ADR-0038 / ADR-0044), targeting
-- | foreign-object 4.1.0.
-- |
-- | On JS an `Object a` is a native JS object (the registry uses `unsafeCoerce` and in-place
-- | mutation). On this backend it is a `newtype` over the foreign-free `Data.Map String a`
-- | (ADR-0044), so every operation is ordinary PureScript over `Data.Map` — no native leaf, no
-- | primop, no new runtime value type. The higher-order operations are plain `App`s of their
-- | callbacks, so ADR-0020's "no native re-entrancy" holds without a leaf. Key order is
-- | `Ord String` (sorted), a documented divergence from JS enumeration order (ADR-0040). The
-- | public interface is unchanged and `Object` stays abstract.
module Foreign.Object
  ( Object
  , empty
  , isEmpty
  , size
  , singleton
  , insert
  , lookup
  , toUnfoldable
  , toAscUnfoldable
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , fromHomogeneous
  , delete
  , pop
  , member
  , alter
  , update
  , mapWithKey
  , filterWithKey
  , filterKeys
  , filter
  , keys
  , values
  , union
  , unionWith
  , unions
  , isSubmap
  , fold
  , foldMap
  , foldM
  , foldMaybe
  , all
  , thawST
  , freezeST
  , runST
  , toArrayWithKey
  ) where

import Prelude

import Control.Monad.ST (ST, run) as ST
import Control.Monad.ST.Internal as STI
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable)
import Foreign.Object.Internal (STObject(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

-- | `Object a` is a sorted `Data.Map` keyed by `String` (ADR-0044). Defined here (not in
-- | `Foreign.Object.Internal`) so its instances below are not orphans; the constructor is not
-- | exported, so the type stays abstract as upstream.
newtype Object a = Object (M.Map String a)

instance functorObject :: Functor Object where
  map f (Object m) = Object (map f m)

instance functorWithIndexObject :: FunctorWithIndex String Object where
  mapWithIndex = mapWithKey

-- | Fold the keys and values of an object.
fold :: forall a z. (z -> String -> a -> z) -> z -> Object a -> z
fold f z (Object m) = foldlWithIndex (\k acc v -> f acc k v) z m

-- | Fold the keys and values of an object, accumulating values using some `Monoid`.
foldMap :: forall a m. Monoid m => (String -> a -> m) -> Object a -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of an object, accumulating values and effects in some `Monad`.
foldM :: forall a m z. Monad m => (z -> String -> a -> m z) -> z -> Object a -> m z
foldM f z o = foldl (\macc (Tuple k v) -> macc >>= \acc -> f acc k v) (pure z) (toArrayWithKey Tuple o)

instance foldableObject :: Foldable Object where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance foldableWithIndexObject :: FoldableWithIndex String Object where
  foldlWithIndex f = fold (flip f)
  foldrWithIndex f z m = foldr (uncurry f) z (toArrayWithKey Tuple m)
  foldMapWithIndex = foldMap

instance traversableObject :: Traversable Object where
  traverse = traverseWithIndex <<< const
  sequence = traverse identity

instance traversableWithIndexObject :: TraversableWithIndex String Object where
  traverseWithIndex f ms =
    fold (\acc k v -> flip (insert k) <$> acc <*> f k v) (pure empty) ms

-- | Fold the keys and values of an object, allowing the folding function to terminate the fold
-- | early using `Maybe`.
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> Object a -> z
foldMaybe f z o = either identity identity (foldl step (Right z) (toArrayWithKey Tuple o))
  where
  step (Left a) _ = Left a
  step (Right a) (Tuple k v) = case f a k v of
    Just a' -> Right a'
    Nothing -> Left a

-- | Test whether all key/value pairs in an `Object` satisfy a predicate.
all :: forall a. (String -> a -> Boolean) -> Object a -> Boolean
all p o = fold (\acc k v -> acc && p k v) true o

instance eqObject :: Eq a => Eq (Object a) where
  eq m1 m2 = isSubmap m1 m2 && isSubmap m2 m1

instance eq1Object :: Eq1 Object where
  eq1 = eq

-- Internal
toAscArray :: forall v. Object v -> Array (Tuple String v)
toAscArray = toAscUnfoldable

instance ordObject :: Ord a => Ord (Object a) where
  compare m1 m2 = compare (toAscArray m1) (toAscArray m2)

instance showObject :: Show a => Show (Object a) where
  show m = "(fromFoldable " <> show (toArray m) <> ")"

-- | An empty object.
empty :: forall a. Object a
empty = Object M.empty

-- | Test whether one object contains all of the keys and values of another.
isSubmap :: forall a. Eq a => Object a -> Object a -> Boolean
isSubmap m1 m2 = all (\k v -> lookup k m2 == Just v) m1

-- | Test whether an object is empty.
isEmpty :: forall a. Object a -> Boolean
isEmpty (Object m) = M.isEmpty m

-- | Calculate the number of key/value pairs in an object.
size :: forall a. Object a -> Int
size (Object m) = M.size m

-- | Create an `Object a` with one key/value pair.
singleton :: forall a. String -> a -> Object a
singleton k v = Object (M.singleton k v)

-- | Look up the value for a key in an object.
lookup :: forall a. String -> Object a -> Maybe a
lookup k (Object m) = M.lookup k m

-- | Test whether a `String` appears as a key in an object.
member :: forall a. String -> Object a -> Boolean
member k (Object m) = M.member k m

-- | Insert or replace a key/value pair in an object.
insert :: forall a. String -> a -> Object a -> Object a
insert k v (Object m) = Object (M.insert k v m)

-- | Delete a key and value from an object.
delete :: forall a. String -> Object a -> Object a
delete k (Object m) = Object (M.delete k m)

-- | Delete a key and value from an object, returning the value as well as the subsequent object.
pop :: forall a. String -> Object a -> Maybe (Tuple a (Object a))
pop k m = lookup k m <#> \a -> Tuple a (delete k m)

-- | Insert, remove or update a value for a key in an object.
alter :: forall a. (Maybe a -> Maybe a) -> String -> Object a -> Object a
alter f k (Object m) = Object (M.alter f k m)

-- | Remove or update a value for a key in an object.
update :: forall a. (a -> Maybe a) -> String -> Object a -> Object a
update f k m = alter (maybe Nothing f) k m

-- | Create an `Object a` from a foldable collection of key/value pairs (last value wins).
fromFoldable :: forall f a. Foldable f => f (Tuple String a) -> Object a
fromFoldable l = Object (M.fromFoldable l)

-- | Create an `Object a` from a `String`-indexed foldable collection.
fromFoldableWithIndex :: forall f a. FoldableWithIndex String f => f a -> Object a
fromFoldableWithIndex l = Object (foldlWithIndex (\k m v -> M.insert k v m) M.empty l)

-- | Create an `Object a` from a foldable collection of key/value pairs, combining duplicate keys
-- | with the given function (`f newValue existingValue`, as upstream).
fromFoldableWith :: forall f a. Foldable f => (a -> a -> a) -> f (Tuple String a) -> Object a
fromFoldableWith f l = Object (foldl step M.empty l)
  where
  step m (Tuple k v) = M.alter (Just <<< maybe v (f v)) k m

-- | Create an `Object a` from a homogeneous record.
-- |
-- | NOTE (ADR-0044): on this backend a record (a row-typed field map) and an `Object` (a
-- | `Data.Map`) are *different* runtime representations, so the registry's `unsafeCoerce` is
-- | unsound here. It is kept verbatim only because the closure does not reach `fromHomogeneous`
-- | (it is dropped by link-time DCE); a `RowList`-driven conversion is the fix if it is ever used.
fromHomogeneous :: forall r a. Homogeneous r a => { | r } -> Object a
fromHomogeneous = unsafeCoerce

-- | Build an array by applying a function to each key/value pair, in ascending key order.
toArrayWithKey :: forall a b. (String -> a -> b) -> Object a -> Array b
toArrayWithKey f (Object m) = foldrWithIndex (\k v acc -> A.cons (f k v) acc) [] m

-- Internal
toArray :: forall a. Object a -> Array (Tuple String a)
toArray = toArrayWithKey Tuple

-- | Unfold an object into a collection of key/value pairs.
toUnfoldable :: forall f a. Unfoldable f => Object a -> f (Tuple String a)
toUnfoldable (Object m) = M.toUnfoldable m

-- | Unfold an object into a collection of key/value pairs, sorted by key.
toAscUnfoldable :: forall f a. Unfoldable f => Object a -> f (Tuple String a)
toAscUnfoldable (Object m) = M.toUnfoldable m

-- | Get an array of the keys in an object.
keys :: forall a. Object a -> Array String
keys (Object m) = A.fromFoldable (M.keys m)

-- | Get an array of the values in an object.
values :: forall a. Object a -> Array a
values (Object m) = A.fromFoldable (M.values m)

-- | Apply a function of two arguments to each key/value pair, producing a new object.
mapWithKey :: forall a b. (String -> a -> b) -> Object a -> Object b
mapWithKey f (Object m) = Object (M.mapMaybeWithKey (\k v -> Just (f k v)) m)

-- | Compute the union of two objects, preferring the first in the case of duplicate keys.
union :: forall a. Object a -> Object a -> Object a
union (Object m1) (Object m2) = Object (M.union m1 m2)

-- | Compute the union of two objects, combining duplicate keys with the given function
-- | (`f firstValue secondValue`, as upstream).
unionWith :: forall a. (a -> a -> a) -> Object a -> Object a -> Object a
unionWith f (Object m1) (Object m2) =
  Object (foldlWithIndex (\k acc v1 -> M.insert k (maybe v1 (f v1) (M.lookup k m2)) acc) m2 m1)

-- | Compute the union of a collection of objects.
unions :: forall f a. Foldable f => f (Object a) -> Object a
unions = foldl union empty

instance semigroupObject :: (Semigroup a) => Semigroup (Object a) where
  append = unionWith (<>)

instance monoidObject :: (Semigroup a) => Monoid (Object a) where
  mempty = empty

-- | Filter out those key/value pairs for which a predicate on the key and value fails to hold.
filterWithKey :: forall a. (String -> a -> Boolean) -> Object a -> Object a
filterWithKey predicate (Object m) = Object (M.filterWithKey predicate m)

-- | Filter out those key/value pairs for which a predicate on the key fails to hold.
filterKeys :: (String -> Boolean) -> Object ~> Object
filterKeys predicate (Object m) = Object (M.filterKeys predicate m)

-- | Filter out those key/value pairs for which a predicate on the value fails to hold.
filter :: forall a. (a -> Boolean) -> Object a -> Object a
filter predicate (Object m) = Object (M.filter predicate m)

-- | Convert an immutable object into a mutable object.
thawST :: forall a r. Object a -> ST.ST r (STObject r a)
thawST (Object m) = STObject <$> STI.new m

-- | Convert a mutable object into an immutable object.
freezeST :: forall a r. STObject r a -> ST.ST r (Object a)
freezeST (STObject ref) = Object <$> STI.read ref

-- | Freeze a mutable object, creating an immutable object. The rank-2 type prevents the mutable
-- | object from escaping the scope of `runST`.
runST :: forall a. (forall r. ST.ST r (STObject r a)) -> Object a
runST st = Object (ST.run (st >>= \(STObject ref) -> STI.read ref))
