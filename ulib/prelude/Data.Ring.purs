-- | ulib SHADOW of `prelude`'s `Data.Ring` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | The upstream module's interface (exports, types, instances) is unchanged, so this shadows the
-- | registry module at the user's resolved version. The scalar `Int`/`Number` subtraction, however,
-- | is reimplemented directly over the `purvasm-base` primitives (`Purvasm.Int`/`Purvasm.Number`)
-- | instead of the registry's `intSub`/`numSub` foreigns, so no opaque foreign remains.
module Data.Ring
  ( class Ring
  , sub
  , negate
  , (-)
  , module Data.Semiring
  , class RingRecord
  , subRecord
  ) where

import Data.Semiring (class Semiring, class SemiringRecord, add, mul, one, zero, (*), (+))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Purvasm.Int as PI
import Purvasm.Number as PN
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following laws in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = zero`
-- | - Compatibility of `sub` and `negate`: `a - b = a + (zero - b)`
class Semiring a <= Ring a where
  sub :: a -> a -> a

infixl 6 sub as -

-- ulib shadow: `Int`/`Number` subtraction over the `purvasm-base` primitives (was the registry's
-- `intSub`/`numSub` foreigns).
instance ringInt :: Ring Int where
  sub = PI.sub

instance ringNumber :: Ring Number where
  sub = PN.sub

instance ringUnit :: Ring Unit where
  sub _ _ = unit

instance ringFn :: Ring b => Ring (a -> b) where
  sub f g x = f x - g x

instance ringProxy :: Ring (Proxy a) where
  sub _ _ = Proxy

instance ringRecord :: (RL.RowToList row list, RingRecord list row row) => Ring (Record row) where
  sub = subRecord (Proxy :: Proxy list)

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. Ring a => a -> a
negate a = zero - a

-- | A class for records where all fields have `Ring` instances, used to
-- | implement the `Ring` instance for records.
class RingRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class SemiringRecord rowlist row subrow <= RingRecord rowlist row subrow | rowlist -> subrow where
  subRecord :: Proxy rowlist -> Record row -> Record row -> Record subrow

instance ringRecordNil :: RingRecord RL.Nil row () where
  subRecord _ _ _ = {}

instance ringRecordCons ::
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , RingRecord rowlistTail row subrowTail
  , Ring focus
  ) =>
  RingRecord (RL.Cons key focus rowlistTail) row subrow where
  subRecord _ ra rb = insert (get ra - get rb) tail
    where
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    tail = subRecord (Proxy :: Proxy rowlistTail) ra rb
