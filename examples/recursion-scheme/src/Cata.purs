module Example.RecursionScheme.Cata where

import Prelude

newtype Fix f = Fix (f (Fix f))

type Alg f a = f a -> a 

cata :: forall f a. Functor f => Alg f a -> Fix f -> a
cata alg = go
  where
  go (Fix f) = alg (map go f)