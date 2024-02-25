module Sample
  ( Maybe(..)
  , ap
  , apply
  , bind
  , class Applicative
  , class Apply
  , class Bind
  , class Functor
  , class Monad
  , class Traversable
  , liftM1
  , map
  , pure
  , traverse
  ) where

data Maybe a = Nothing | Just a

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

infixl 1 bind as >>=

class (Applicative m, Bind m) <= Monad m

ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)

liftM1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
liftM1 f fa = pure f <*> fa

instance Functor Maybe where
  map = liftM1

instance Apply Maybe where
  apply = case _, _ of
    Nothing, _ -> Nothing
    _, Nothing -> Nothing
    Just f, Just a -> Just (f a)

instance Applicative Maybe where
  pure = Just

instance Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a

instance Monad Maybe

class Functor t <= Traversable t where
  traverse :: forall m a b. Applicative m => (a -> m b) -> t a -> m (t b)

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = map Just (f a)