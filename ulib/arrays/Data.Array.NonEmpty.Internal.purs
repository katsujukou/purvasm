-- | This module exports the `NonEmptyArray` constructor.
-- |
-- | It is **NOT** intended for public use and is **NOT** versioned.
-- |
-- | Its content may change **in any way**, **at any time** and
-- | **without notice**.
-- |
-- | ulib SHADOW (ADR-0038): interface unchanged (only `NonEmptyArray(..)` is exported). The three
-- | foreigns are reimplemented in PureScript over the `purvasm-base` array primitives. `traverse1Impl`
-- | keeps the registry's behaviour but, since its `apply`/`map` arguments are used polymorphically
-- | (rank-2) and so cannot pass through `mkFn3`, it is a plain curried higher-rank function and the
-- | `Traversable1` instance applies it directly rather than via `runFn3` (it is not exported, so the
-- | interface is unaffected). The JS version trampolines to stay stack-safe; the purvasm machine is
-- | already stack-safe (ADR-0030), so a direct right-fold suffices.
module Data.Array.NonEmpty.Internal (NonEmptyArray(..)) where

import Prelude

import Control.Alt (class Alt)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1DefaultL)
import Data.Semigroup.Traversable (class Traversable1, sequence1Default)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Unfoldable1 (class Unfoldable1)
import Purvasm.Array as PA

-- | An array that is known not to be empty.
-- |
-- | You can use the constructor to create a `NonEmptyArray` that isn't
-- | non-empty, breaking the guarantee behind this newtype. It is
-- | provided as an escape hatch mainly for the `Data.Array.NonEmpty`
-- | and `Data.Array` modules. Use this at your own risk when you know
-- | what you are doing.
newtype NonEmptyArray a = NonEmptyArray (Array a)

instance showNonEmptyArray :: Show a => Show (NonEmptyArray a) where
  show (NonEmptyArray xs) = "(NonEmptyArray " <> show xs <> ")"

derive newtype instance eqNonEmptyArray :: Eq a => Eq (NonEmptyArray a)
derive newtype instance eq1NonEmptyArray :: Eq1 NonEmptyArray

derive newtype instance ordNonEmptyArray :: Ord a => Ord (NonEmptyArray a)
derive newtype instance ord1NonEmptyArray :: Ord1 NonEmptyArray

derive newtype instance semigroupNonEmptyArray :: Semigroup (NonEmptyArray a)

derive newtype instance functorNonEmptyArray :: Functor NonEmptyArray
derive newtype instance functorWithIndexNonEmptyArray :: FunctorWithIndex Int NonEmptyArray

derive newtype instance foldableNonEmptyArray :: Foldable NonEmptyArray
derive newtype instance foldableWithIndexNonEmptyArray :: FoldableWithIndex Int NonEmptyArray

instance foldable1NonEmptyArray :: Foldable1 NonEmptyArray where
  foldMap1 = foldMap1DefaultL
  foldr1 = runFn2 foldr1Impl
  foldl1 = runFn2 foldl1Impl

derive newtype instance unfoldable1NonEmptyArray :: Unfoldable1 NonEmptyArray
derive newtype instance traversableNonEmptyArray :: Traversable NonEmptyArray
derive newtype instance traversableWithIndexNonEmptyArray :: TraversableWithIndex Int NonEmptyArray

instance traversable1NonEmptyArray :: Traversable1 NonEmptyArray where
  traverse1 f = traverse1Impl apply map f
  sequence1 = sequence1Default

derive newtype instance applyNonEmptyArray :: Apply NonEmptyArray

derive newtype instance applicativeNonEmptyArray :: Applicative NonEmptyArray

derive newtype instance bindNonEmptyArray :: Bind NonEmptyArray

derive newtype instance monadNonEmptyArray :: Monad NonEmptyArray

derive newtype instance altNonEmptyArray :: Alt NonEmptyArray

-- ulib shadow: a right fold over the non-empty array seeded with its last element (was a foreign).
foldr1Impl :: forall a. Fn2 (a -> a -> a) (NonEmptyArray a) a
foldr1Impl = mkFn2 \f (NonEmptyArray xs) ->
  let
    n = PA.length xs
    go i acc = if i < 0 then acc else go (i - 1) (f (PA.unsafeIndex xs i) acc)
  in
    go (n - 2) (PA.unsafeIndex xs (n - 1))

-- ulib shadow: a left fold over the non-empty array seeded with its first element (was a foreign).
foldl1Impl :: forall a. Fn2 (a -> a -> a) (NonEmptyArray a) a
foldl1Impl = mkFn2 \f (NonEmptyArray xs) ->
  let
    n = PA.length xs
    go i acc = if i >= n then acc else go (i + 1) (f acc (PA.unsafeIndex xs i))
  in
    go 1 (PA.unsafeIndex xs 0)

-- A private, in-order cons list — the accumulator `traverse1Impl` builds inside `m`.
data TList a = TNil | TCons a (TList a)

tListToArray :: forall a. TList a -> Array a
tListToArray l = fill l 0 (PA.unsafeNew (len l 0))
  where
  len ll acc = case ll of
    TNil -> acc
    TCons _ t -> len t (acc + 1)
  fill ll i out = case ll of
    TNil -> out
    TCons x t -> fill t (i + 1) (PA.unsafeSet out i x)

-- ulib shadow: applicative traverse of a non-empty array (was a foreign). Folds right-to-left,
-- threading `m (TList b)` through `apply`/`map`, then materialises the array. Higher-rank in
-- `apply`/`map` (used at several element/accumulator types); not an `Fn3` — see the module header.
traverse1Impl
  :: forall m a b
   . (forall a' b'. m (a' -> b') -> m a' -> m b')
  -> (forall a' b'. (a' -> b') -> m a' -> m b')
  -> (a -> m b)
  -> NonEmptyArray a
  -> m (NonEmptyArray b)
traverse1Impl ap mp f (NonEmptyArray xs) =
  mp (\l -> NonEmptyArray (tListToArray l)) (go (n - 2) start)
  where
  n = PA.length xs
  start = mp (\x -> TCons x TNil) (f (PA.unsafeIndex xs (n - 1)))
  go i acc =
    if i < 0 then acc
    else go (i - 1) (ap (mp TCons (f (PA.unsafeIndex xs i))) acc)
