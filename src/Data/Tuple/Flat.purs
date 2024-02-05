module Data.Tuple.Flat where

import Prelude

import Data.Function (on)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))

foreign import data T2 :: Type -> Type -> Type

foreign import data T3 :: Type -> Type -> Type -> Type

foreign import mkT2 :: forall a b. a -> b -> T2 a b 

foreign import mkT3 :: forall a b c. a -> b -> c -> T3 a b c

foreign import toTuple2 :: forall a b. T2 a b -> Tuple a b

foreign import toTuple3 :: forall a b c. T3 a b c -> a /\ b /\ c

instance (Show a, Show b) => Show (T2 a b) where
  show = toTuple2 >>> case _ of
    a /\ b -> "(" <> show a <> ", " <> show b <> ")"

instance (Eq a, Eq b, Eq c) => Eq (T3 a b c) where
  eq = eq `on` toTuple3

instance (Ord a, Ord b, Ord c) => Ord (T3 a b c) where
  compare = compare `on` toTuple3

instance (Show a, Show b, Show c) => Show (T3 a b c) where
  show = toTuple3 >>> case _ of
    a /\ b /\ c -> "(" <> show a <> ", " <> show b <> ", " <> show c <> ")"