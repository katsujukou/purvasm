-- | ulib SHADOW of `prelude`'s `Control.Bind` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | Byte-for-byte the upstream module EXCEPT `arrayBind`, which here is PureScript over the
-- | purvasm-base array primitives (`Purvasm.Array`) with prelude-free loop arithmetic (`Purvasm.Int`,
-- | since `Control.Bind` imports no `Prelude` arithmetic) — so the closure bound by `>>=` over
-- | an `Array` *specializes* (ADR-0027) instead of going through the opaque foreign `arrayBind`.
-- | The module's interface (exports, types, instances) is unchanged, so this can shadow the
-- | registry module at the user's resolved version.
module Control.Bind
  ( class Bind
  , bind
  , (>>=)
  , bindFlipped
  , (=<<)
  , class Discard
  , discard
  , join
  , composeKleisli
  , (>=>)
  , composeKleisliFlipped
  , (<=<)
  , ifM
  , module Data.Functor
  , module Control.Apply
  , module Control.Applicative
  ) where

import Control.Applicative (class Applicative, liftA1, pure, unless, when)
import Control.Apply (class Apply, apply, (*>), (<*), (<*>))
import Control.Category (identity)

import Data.Function (flip)
import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Unit (Unit)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Int as PI

-- | The `Bind` type class extends the [`Apply`](#apply) type class with a
-- | "bind" operation `(>>=)` which composes computations in sequence, using
-- | the return value of one computation to determine the next computation.
-- |
-- | The `>>=` operator can also be expressed using `do` notation, as follows:
-- |
-- | ```purescript
-- | x >>= f = do y <- x
-- |              f y
-- | ```
-- |
-- | where the function argument of `f` is given the name `y`.
-- |
-- | Instances must satisfy the following laws in addition to the `Apply`
-- | laws:
-- |
-- | - Associativity: `(x >>= f) >>= g = x >>= (\k -> f k >>= g)`
-- | - Apply Superclass: `apply f x = f >>= \f’ -> map f’ x`
-- |
-- | Associativity tells us that we can regroup operations which use `do`
-- | notation so that we can unambiguously write, for example:
-- |
-- | ```purescript
-- | do x <- m1
-- |    y <- m2 x
-- |    m3 x y
-- | ```
class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

infixl 1 bind as >>=

-- | `bindFlipped` is `bind` with its arguments reversed. For example:
-- |
-- | ```purescript
-- | print =<< random
-- | ```
bindFlipped :: forall m a b. Bind m => (a -> m b) -> m a -> m b
bindFlipped = flip bind

infixr 1 bindFlipped as =<<

instance bindFn :: Bind ((->) r) where
  bind m f x = f (m x) x

-- | The `bind`/`>>=` function for `Array` works by applying a function to
-- | each element in the array, and flattening the results into a single,
-- | new array.
-- |
-- | Array's `bind`/`>>=` works like a nested for loop. Each `bind` adds
-- | another level of nesting in the loop. For example:
-- | ```
-- | foo :: Array String
-- | foo =
-- |   ["a", "b"] >>= \eachElementInArray1 ->
-- |     ["c", "d"] >>= \eachElementInArray2
-- |       pure (eachElementInArray1 <> eachElementInArray2)
-- |
-- | -- In other words...
-- | foo
-- | -- ... is the same as...
-- | [ ("a" <> "c"), ("a" <> "d"), ("b" <> "c"), ("b" <> "d") ]
-- | -- which simplifies to...
-- | [ "ac", "ad", "bc", "bd" ]
-- | ```
instance bindArray :: Bind Array where
  bind = arrayBind

-- ulib shadow: `arrayBind` (concatMap) over purvasm-base primitives, so the bound closure specializes
-- on purvasm (vs prelude's opaque `foreign import arrayBind`). Two passes over precomputed sub-arrays:
-- materialise each `f x` once, sum their lengths, then copy them into a single result buffer.
arrayBind :: forall a b. Array a -> (a -> Array b) -> Array b
arrayBind xs f = fillOuter 0 0 (PA.unsafeNew total)
  where
  n = PA.length xs
  subs = mkSubs 0 (PA.unsafeNew n)
  mkSubs i acc =
    if PI.eq i n then acc
    else mkSubs (PI.add i 1) (PA.unsafeSet acc i (f (PA.unsafeIndex xs i)))
  total = sumLen 0 0
  sumLen i acc =
    if PI.eq i n then acc
    else sumLen (PI.add i 1) (PI.add acc (PA.length (PA.unsafeIndex subs i)))
  fillOuter i o out =
    if PI.eq i n then out
    else fillInner i 0 o out
  fillInner i j o out =
    if PI.eq j (PA.length (PA.unsafeIndex subs i)) then fillOuter (PI.add i 1) o out
    else fillInner i (PI.add j 1) (PI.add o 1)
      (PA.unsafeSet out o (PA.unsafeIndex (PA.unsafeIndex subs i) j))

instance bindProxy :: Bind Proxy where
  bind _ _ = Proxy

-- | A class for types whose values can safely be discarded
-- | in a `do` notation block.
-- |
-- | An example is the `Unit` type, since there is only one
-- | possible value which can be returned.
class Discard a where
  discard :: forall f b. Bind f => f a -> (a -> f b) -> f b

instance discardUnit :: Discard Unit where
  discard = bind

instance discardProxy :: Discard (Proxy a) where
  discard = bind

-- | Collapse two applications of a monadic type constructor into one.
join :: forall a m. Bind m => m (m a) -> m a
join m = m >>= identity

-- | Forwards Kleisli composition.
-- |
-- | For example:
-- |
-- | ```purescript
-- | import Data.Array (head, tail)
-- |
-- | third = tail >=> tail >=> head
-- | ```
composeKleisli :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
composeKleisli f g a = f a >>= g

infixr 1 composeKleisli as >=>

-- | Backwards Kleisli composition.
composeKleisliFlipped :: forall a b c m. Bind m => (b -> m c) -> (a -> m b) -> a -> m c
composeKleisliFlipped f g a = f =<< g a

infixr 1 composeKleisliFlipped as <=<

-- | Execute a monadic action if a condition holds.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = ifM ((< 0.5) <$> random)
-- |          (trace "Heads")
-- |          (trace "Tails")
-- | ```
ifM :: forall a m. Bind m => m Boolean -> m a -> m a -> m a
ifM cond t f = cond >>= \cond' -> if cond' then t else f
