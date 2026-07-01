-- | ulib SHADOW of `foldable-traversable`'s `Data.FunctorWithIndex` (ADR-0038), targeting 6.0.0.
-- |
-- | The upstream module's interface (exports, classes, instances) is unchanged, so this shadows the
-- | registry module at the user's resolved version. The one foreign, `mapWithIndexArray`, is
-- | reimplemented in PureScript over the `purvasm-base` array primitives (`Purvasm.Array`/
-- | `Purvasm.Int`) — so the mapped closure specializes (ADR-0027) instead of going through the
-- | opaque foreign, and no foreign remains.
module Data.FunctorWithIndex
  ( class FunctorWithIndex
  , mapWithIndex
  , mapDefault
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Tuple (Tuple, curry)
import Purvasm.Array as PA
import Purvasm.Int as PI

-- | A `Functor` with an additional index.
-- | Instances must satisfy a modified form of the `Functor` laws
-- | ```purescript
-- | mapWithIndex (\_ a -> a) = identity
-- | mapWithIndex f . mapWithIndex g = mapWithIndex (\i -> f i <<< g i)
-- | ```
-- | and be compatible with the `Functor` instance
-- | ```purescript
-- | map f = mapWithIndex (const f)
-- | ```
class Functor f <= FunctorWithIndex i f | f -> i where
  mapWithIndex :: forall a b. (i -> a -> b) -> f a -> f b

-- ulib shadow: `mapWithIndexArray` over the `purvasm-base` array primitives (mirrors the
-- `Data.Functor` `arrayMap` shadow). Builds the result in place and threads it, applying the
-- index-aware closure `f i xs[i]` per slot.
mapWithIndexArray :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndexArray f xs = go 0 (PA.unsafeNew n)
  where
  n = PA.length xs
  go i out =
    if PI.eq i n then out
    else go (PI.add i 1) (PA.unsafeSet out i (f i (PA.unsafeIndex xs i)))

instance functorWithIndexArray :: FunctorWithIndex Int Array where
  mapWithIndex = mapWithIndexArray

instance functorWithIndexMaybe :: FunctorWithIndex Unit Maybe where
  mapWithIndex f = map $ f unit

instance functorWithIndexFirst :: FunctorWithIndex Unit First where
  mapWithIndex f = map $ f unit

instance functorWithIndexLast :: FunctorWithIndex Unit Last where
  mapWithIndex f = map $ f unit

instance functorWithIndexAdditive :: FunctorWithIndex Unit Additive where
  mapWithIndex f = map $ f unit

instance functorWithIndexDual :: FunctorWithIndex Unit Dual where
  mapWithIndex f = map $ f unit

instance functorWithIndexConj :: FunctorWithIndex Unit Conj where
  mapWithIndex f = map $ f unit

instance functorWithIndexDisj :: FunctorWithIndex Unit Disj where
  mapWithIndex f = map $ f unit

instance functorWithIndexMultiplicative :: FunctorWithIndex Unit Multiplicative where
  mapWithIndex f = map $ f unit

instance functorWithIndexEither :: FunctorWithIndex Unit (Either a) where
  mapWithIndex f = map $ f unit

instance functorWithIndexTuple :: FunctorWithIndex Unit (Tuple a) where
  mapWithIndex f = map $ f unit

instance functorWithIndexIdentity :: FunctorWithIndex Unit Identity where
  mapWithIndex f (Identity a) = Identity (f unit a)

instance functorWithIndexConst :: FunctorWithIndex Void (Const a) where
  mapWithIndex _ (Const x) = Const x

instance functorWithIndexProduct :: (FunctorWithIndex a f, FunctorWithIndex b g) => FunctorWithIndex (Either a b) (Product f g) where
  mapWithIndex f (Product fga) = Product (bimap (mapWithIndex (f <<< Left)) (mapWithIndex (f <<< Right)) fga)

instance functorWithIndexCoproduct :: (FunctorWithIndex a f, FunctorWithIndex b g) => FunctorWithIndex (Either a b) (Coproduct f g) where
  mapWithIndex f (Coproduct e) = Coproduct (bimap (mapWithIndex (f <<< Left)) (mapWithIndex (f <<< Right)) e)

instance functorWithIndexCompose :: (FunctorWithIndex a f, FunctorWithIndex b g) => FunctorWithIndex (Tuple a b) (Compose f g) where
  mapWithIndex f (Compose fga) = Compose $ mapWithIndex (mapWithIndex <<< curry f) fga

instance functorWithIndexApp :: FunctorWithIndex a f => FunctorWithIndex a (App f) where
  mapWithIndex f (App x) = App $ mapWithIndex f x

-- | A default implementation of Functor's `map` in terms of `mapWithIndex`
mapDefault :: forall i f a b. FunctorWithIndex i f => (a -> b) -> f a -> f b
mapDefault f = mapWithIndex (const f)
