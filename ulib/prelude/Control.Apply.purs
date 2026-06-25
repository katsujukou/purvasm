-- | ulib SHADOW of `prelude`'s `Control.Apply` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | Byte-for-byte the upstream module EXCEPT `arrayApply`, which here is PureScript over the
-- | purvasm-base array primitives (`Purvasm.Array`) with prelude-free loop arithmetic (`Purvasm.Int`,
-- | since `Control.Apply` imports no `Prelude` arithmetic) — so the functions applied by `<*>`
-- | over an `Array` *specialize* (ADR-0027) instead of going through the opaque foreign
-- | `arrayApply`. The module's interface (exports, types, instances) is unchanged, so this can
-- | shadow the registry module at the user's resolved version.
module Control.Apply
  ( class Apply
  , apply
  , (<*>)
  , applyFirst
  , (<*)
  , applySecond
  , (*>)
  , lift2
  , lift3
  , lift4
  , lift5
  , module Data.Functor
  ) where

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Function (const)
import Control.Category (identity)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Int as PI

-- | The `Apply` class provides the `(<*>)` which is used to apply a function
-- | to an argument under a type constructor.
-- |
-- | `Apply` can be used to lift functions of two or more arguments to work on
-- | values wrapped with the type constructor `f`. It might also be understood
-- | in terms of the `lift2` function:
-- |
-- | ```purescript
-- | lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c
-- | lift2 f a b = f <$> a <*> b
-- | ```
-- |
-- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
-- | the function application operator `($)` to arguments wrapped with the
-- | type constructor `f`.
-- |
-- | Put differently...
-- | ```
-- | foo =
-- |   functionTakingNArguments <$> computationProducingArg1
-- |                            <*> computationProducingArg2
-- |                            <*> ...
-- |                            <*> computationProducingArgN
-- | ```
-- |
-- | Instances must satisfy the following law in addition to the `Functor`
-- | laws:
-- |
-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
-- |
-- | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

instance applyFn :: Apply ((->) r) where
  apply f g x = f x (g x)

instance applyArray :: Apply Array where
  apply = arrayApply

-- ulib shadow: `arrayApply` over purvasm-base primitives, so the functions applied by `<*>` specialize
-- on purvasm (vs prelude's opaque `foreign import arrayApply`). Nested loop: apply every function to
-- every element; the result length is `length fs * length xs`. Allocates once and threads the buffer.
arrayApply :: forall a b. Array (a -> b) -> Array a -> Array b
arrayApply fs xs = outer 0 0 (PA.unsafeNew (PI.mul l k))
  where
  l = PA.length fs
  k = PA.length xs
  outer i n out =
    if PI.eq i l then out
    else inner i 0 n out
  inner i j n out =
    if PI.eq j k then outer (PI.add i 1) n out
    else inner i (PI.add j 1) (PI.add n 1)
      (PA.unsafeSet out n ((PA.unsafeIndex fs i) (PA.unsafeIndex xs j)))

instance applyProxy :: Apply Proxy where
  apply _ _ = Proxy

-- | Combine two effectful actions, keeping only the result of the first.
applyFirst :: forall a b f. Apply f => f a -> f b -> f a
applyFirst a b = const <$> a <*> b

infixl 4 applyFirst as <*

-- | Combine two effectful actions, keeping only the result of the second.
applySecond :: forall a b f. Apply f => f a -> f b -> f b
applySecond a b = const identity <$> a <*> b

infixl 4 applySecond as *>

-- | Lift a function of two arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
-- |
-- | ```purescript
-- | lift2 add (Just 1) (Just 2) == Just 3
-- | lift2 add Nothing (Just 2) == Nothing
-- |```
-- |
lift2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b

-- | Lift a function of three arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f a b c = f <$> a <*> b <*> c

-- | Lift a function of four arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
lift4 :: forall a b c d e f. Apply f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 f a b c d = f <$> a <*> b <*> c <*> d

-- | Lift a function of five arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
lift5 :: forall a b c d e f g. Apply f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
lift5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
