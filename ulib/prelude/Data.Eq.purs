-- | ulib SHADOW of `prelude`'s `Data.Eq` (ADR-0038), targeting prelude 6.0.2.
-- |
-- | The upstream module's interface (exports, types, instances) is unchanged, so this shadows the
-- | registry module at the user's resolved version. The scalar and array equalities, however, are
-- | reimplemented in PureScript over the `purvasm-base` primitives
-- | (`Purvasm.Int`/`.Boolean`/`.Char`/`.Number`/`.Array`, with a byte-wise loop for `String`) — so
-- | the element comparator used by `Eq (Array a)` *specializes* (ADR-0027) instead of going through
-- | the opaque foreign `eqArrayImpl`, and no scalar foreign remains.
module Data.Eq
  ( class Eq
  , eq
  , (==)
  , notEq
  , (/=)
  , class Eq1
  , eq1
  , notEq1
  , class EqRecord
  , eqRecord
  ) where

import Data.HeytingAlgebra ((&&))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit)
import Data.Void (Void)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Boolean as PB
import Purvasm.Char as PC
import Purvasm.Int as PI
import Purvasm.Number as PN
import Purvasm.String as PS

-- | The `Eq` type class represents types which support decidable equality.
-- |
-- | `Eq` instances should satisfy the following laws:
-- |
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- |
-- | **Note:** The `Number` type is not an entirely law abiding member of this
-- | class due to the presence of `NaN`, since `NaN /= NaN`. Additionally,
-- | computing with `Number` can result in a loss of precision, so sometimes
-- | values that should be equivalent are not.
class Eq a where
  eq :: a -> a -> Boolean

infix 4 eq as ==

-- | `notEq` tests whether one value is _not equal_ to another. Shorthand for
-- | `not (eq x y)`.
notEq :: forall a. Eq a => a -> a -> Boolean
notEq x y = (x == y) == false

infix 4 notEq as /=

instance eqBoolean :: Eq Boolean where
  eq = eqBooleanImpl

instance eqInt :: Eq Int where
  eq = eqIntImpl

instance eqNumber :: Eq Number where
  eq = eqNumberImpl

instance eqChar :: Eq Char where
  eq = eqCharImpl

instance eqString :: Eq String where
  eq = eqStringImpl

instance eqUnit :: Eq Unit where
  eq _ _ = true

instance eqVoid :: Eq Void where
  eq _ _ = true

instance eqArray :: Eq a => Eq (Array a) where
  eq = eqArrayImpl eq

instance eqRec :: (RL.RowToList row list, EqRecord list row) => Eq (Record row) where
  eq = eqRecord (Proxy :: Proxy list)

instance eqProxy :: Eq (Proxy a) where
  eq _ _ = true

-- ulib shadow: scalar equality reimplemented over the `purvasm-base` primitives
-- (`Purvasm.Int`/`.Boolean`/`.Char`/`.Number`/`.String`), so `Data.Eq` carries no opaque scalar
-- foreign — the registry `eq*Impl` wat providers are gone. `String` goes through the ADR-0103
-- bulk comparison leaf: one leaf call instead of a `byteAt` apply per byte, which made `Eq String`
-- the dominant cost of string-keyed lookups (sidenote-0017).
eqBooleanImpl :: Boolean -> Boolean -> Boolean
eqBooleanImpl x y = if x then y else PB.not y

eqIntImpl :: Int -> Int -> Boolean
eqIntImpl = PI.eq

eqNumberImpl :: Number -> Number -> Boolean
eqNumberImpl = PN.eq

-- `Char` is its code point (ADR-0006); compare as `Int`.
eqCharImpl :: Char -> Char -> Boolean
eqCharImpl x y = PI.eq (PC.toCodePoint x) (PC.toCodePoint y)

eqStringImpl :: String -> String -> Boolean
eqStringImpl x y = PI.eq (PS.compareBytes x y) 0

-- ulib shadow: `eqArrayImpl` over the `Purvasm.Array` primitives, so the element comparator
-- specializes on purvasm (vs prelude's opaque `foreign import eqArrayImpl`). Lengths must match,
-- then compare elementwise, short-circuiting on the first inequality.
eqArrayImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
eqArrayImpl f xs ys =
  if PI.eq nx ny then go 0 else false
  where
  nx = PA.length xs
  ny = PA.length ys
  go i =
    if PI.eq i nx then true
    else if f (PA.unsafeIndex xs i) (PA.unsafeIndex ys i) then go (PI.add i 1)
    else false

-- | The `Eq1` type class represents type constructors with decidable equality.
class Eq1 f where
  eq1 :: forall a. Eq a => f a -> f a -> Boolean

instance eq1Array :: Eq1 Array where
  eq1 = eq

notEq1 :: forall f a. Eq1 f => Eq a => f a -> f a -> Boolean
notEq1 x y = (x `eq1` y) == false

-- | A class for records where all fields have `Eq` instances, used to implement
-- | the `Eq` instance for records.
class EqRecord :: RL.RowList Type -> Row Type -> Constraint
class EqRecord rowlist row where
  eqRecord :: Proxy rowlist -> Record row -> Record row -> Boolean

instance eqRowNil :: EqRecord RL.Nil row where
  eqRecord _ _ _ = true

instance eqRowCons ::
  ( EqRecord rowlistTail row
  , Row.Cons key focus rowTail row
  , IsSymbol key
  , Eq focus
  ) =>
  EqRecord (RL.Cons key focus rowlistTail) row where
  eqRecord _ ra rb = (get ra == get rb) && tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    tail = eqRecord (Proxy :: Proxy rowlistTail) ra rb
