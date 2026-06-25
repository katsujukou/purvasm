-- | ulib SHADOW of `prelude`'s `Data.Functor` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | Byte-for-byte the upstream module EXCEPT `arrayMap`, which here is PureScript over the
-- | purvasm-base array primitives (`Purvasm.Array`) with prelude-free loop arithmetic (`Purvasm.Int`,
-- | since `Data.Functor` imports no `Prelude` arithmetic) — so the closure passed to `map`
-- | over an `Array` *specializes* (ADR-0027) instead of going through the opaque foreign
-- | `arrayMap`. The module's interface (exports, types, instances) is unchanged, so this can
-- | shadow the registry module at the user's resolved version.
module Data.Functor
  ( class Functor
  , map
  , (<$>)
  , mapFlipped
  , (<#>)
  , void
  , voidRight
  , (<$)
  , voidLeft
  , ($>)
  , flap
  , (<@>)
  ) where

import Data.Function (const, compose)
import Data.Unit (Unit, unit)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Int as PI

-- | A `Functor` is a type constructor which supports a mapping operation
-- | `map`.
-- |
-- | `map` can be used to turn functions `a -> b` into functions
-- | `f a -> f b` whose argument and return types use the type constructor `f`
-- | to represent some computational context.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Identity: `map identity = identity`
-- | - Composition: `map (f <<< g) = map f <<< map g`
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

-- | `mapFlipped` is `map` with its arguments reversed. For example:
-- |
-- | ```purescript
-- | [1, 2, 3] <#> \n -> n * n
-- | ```
mapFlipped :: forall f a b. Functor f => f a -> (a -> b) -> f b
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>

instance functorFn :: Functor ((->) r) where
  map = compose

instance functorArray :: Functor Array where
  map = arrayMap

instance functorProxy :: Functor Proxy where
  map _ _ = Proxy

-- ulib shadow: `arrayMap` over purvasm-base primitives, so the mapped closure specializes on purvasm
-- (vs prelude's opaque `foreign import arrayMap`). Builds the result in place and threads it.
arrayMap :: forall a b. (a -> b) -> Array a -> Array b
arrayMap f xs = go 0 (PA.unsafeNew n)
  where
  n = PA.length xs
  go i out = if PI.eq i n then out else go (PI.add i 1) (PA.unsafeSet out i (f (PA.unsafeIndex xs i)))

-- | The `void` function is used to ignore the type wrapped by a
-- | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
-- | information provided by the type constructor itself.
-- |
-- | `void` is often useful when using `do` notation to change the return type
-- | of a monadic computation:
-- |
-- | ```purescript
-- | main = forE 1 10 \n -> void do
-- |   print n
-- |   print (n * n)
-- | ```
void :: forall f a. Functor f => f a -> f Unit
void = map (const unit)

-- | Ignore the return value of a computation, using the specified return value
-- | instead.
voidRight :: forall f a b. Functor f => a -> f b -> f a
voidRight x = map (const x)

infixl 4 voidRight as <$

-- | A version of `voidRight` with its arguments flipped.
voidLeft :: forall f a b. Functor f => f a -> b -> f b
voidLeft f x = const x <$> f

infixl 4 voidLeft as $>

-- | Apply a value in a computational context to a value in no context.
-- |
-- | Generalizes `flip`.
-- |
-- | ```purescript
-- | longEnough :: String -> Bool
-- | hasSymbol :: String -> Bool
-- | hasDigit :: String -> Bool
-- | password :: String
-- |
-- | validate :: String -> Array Bool
-- | validate = flap [longEnough, hasSymbol, hasDigit]
-- | ```
-- |
-- | ```purescript
-- | flap (-) 3 4 == 1
-- | threeve <$> Just 1 <@> 'a' <*> Just true == Just (threeve 1 'a' true)
-- | ```
flap :: forall f a b. Functor f => f (a -> b) -> a -> f b
flap ff x = map (\f -> f x) ff

infixl 4 flap as <@>
