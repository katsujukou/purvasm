-- | ulib SHADOW of `prelude`'s `Data.Bounded` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | The upstream module's interface (exports, types, instances) is unchanged, so this shadows the
-- | registry module at the user's resolved version. The six foreign constants, however, are
-- | reimplemented in PureScript over the `purvasm-base` primitives so no opaque foreign remains:
-- | `bottomInt` is `complement topInt` (the only value that cannot be written as an `Int` literal,
-- | which is exactly why the registry routes it through a foreign), the `Char` bounds come from
-- | `Purvasm.Char.fromCodePoint`, and the `Number` infinities from a total `Purvasm.Number.div`.
module Data.Bounded
  ( class Bounded
  , bottom
  , top
  , module Data.Ord
  , class BoundedRecord
  , bottomRecord
  , topRecord
  ) where

import Data.Ord (class Ord, class OrdRecord, Ordering(..), compare, (<), (<=), (>), (>=))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Purvasm.Char as PC
import Purvasm.Int as PI
import Purvasm.Number as PN
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))

-- | The `Bounded` type class represents totally ordered types that have an
-- | upper and lower boundary.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Bounded: `bottom <= a <= top`
class Ord a <= Bounded a where
  top :: a
  bottom :: a

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

-- | The `Bounded` `Int` instance has `top :: Int` equal to 2^31 - 1,
-- | and `bottom :: Int` equal to -2^31, since these are the largest and smallest
-- | integers representable by twos-complement 32-bit integers, respectively.
instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

-- ulib shadow: `topInt` is the literal `2^31 - 1`; `bottomInt` (`-2^31`) cannot be an `Int`
-- literal (its magnitude exceeds `topInt`), so it is `complement topInt` — `~x == -x - 1`.
topInt :: Int
topInt = 2147483647

bottomInt :: Int
bottomInt = PI.complement topInt

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

-- ulib shadow: the `Char` bounds as code points (ADR-0006) — `U+FFFF` and `U+0000`, matching the
-- registry's `String.fromCharCode 65535` / `0`.
topChar :: Char
topChar = PC.fromCodePoint 65535

bottomChar :: Char
bottomChar = PC.fromCodePoint 0

instance boundedOrdering :: Bounded Ordering where
  top = GT
  bottom = LT

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

-- ulib shadow: the `Number` bounds are the IEEE infinities, since neither can be written as a
-- `Number` literal — `+inf` from the total `Purvasm.Number.div` (`1.0 / 0.0`), and `-inf` as
-- `0.0 - (+inf)` (avoiding a negative literal, which would need `Prelude`'s `negate`).
topNumber :: Number
topNumber = PN.div 1.0 0.0

bottomNumber :: Number
bottomNumber = PN.sub 0.0 topNumber

instance boundedNumber :: Bounded Number where
  top = topNumber
  bottom = bottomNumber

instance boundedProxy :: Bounded (Proxy a) where
  bottom = Proxy
  top = Proxy

class BoundedRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class OrdRecord rowlist row <= BoundedRecord rowlist row subrow | rowlist -> subrow where
  topRecord :: Proxy rowlist -> Proxy row -> Record subrow
  bottomRecord :: Proxy rowlist -> Proxy row -> Record subrow

instance boundedRecordNil :: BoundedRecord RL.Nil row () where
  topRecord _ _ = {}
  bottomRecord _ _ = {}

instance boundedRecordCons ::
  ( IsSymbol key
  , Bounded focus
  , Row.Cons key focus rowTail row
  , Row.Cons key focus subrowTail subrow
  , BoundedRecord rowlistTail row subrowTail
  ) =>
  BoundedRecord (RL.Cons key focus rowlistTail) row subrow where
  topRecord _ rowProxy = insert top tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = topRecord (Proxy :: Proxy rowlistTail) rowProxy

  bottomRecord _ rowProxy = insert bottom tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = bottomRecord (Proxy :: Proxy rowlistTail) rowProxy

instance boundedRecord ::
  ( RL.RowToList row list
  , BoundedRecord list row row
  ) =>
  Bounded (Record row) where
  top = topRecord (Proxy :: Proxy list) (Proxy :: Proxy row)
  bottom = bottomRecord (Proxy :: Proxy list) (Proxy :: Proxy row)
