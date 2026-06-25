-- | ulib patch of `Data.Semigroup` (ADR-0038): interface-compatible with the registry
-- | module — the class, instances, and `concatString` are verbatim — but the JS foreign
-- | `concatArray` is reimplemented in PureScript over `Purvasm.Array`/`Purvasm.Int`, so it
-- | has a purvasm provider, and `concatString` likewise over `Purvasm.String`'s byte ops, so
-- | each has its element/byte handling on the optimiser's turf — no opaque foreign remains.
module Data.Semigroup
  ( class Semigroup
  , append
  , (<>)
  , class SemigroupRecord
  , appendRecord
  ) where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Prim.Row as Row
import Prim.RowList as RL
import Purvasm.Array as PA
import Purvasm.Int as PI
import Purvasm.String as PS
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

instance semigroupString :: Semigroup String where
  append = concatString

instance semigroupUnit :: Semigroup Unit where
  append _ _ = unit

instance semigroupVoid :: Semigroup Void where
  append _ = absurd

instance semigroupFn :: Semigroup s' => Semigroup (s -> s') where
  append f g x = f x <> g x

instance semigroupArray :: Semigroup (Array a) where
  append = concatArray

instance semigroupProxy :: Semigroup (Proxy a) where
  append _ _ = Proxy

instance semigroupRecord :: (RL.RowToList row list, SemigroupRecord list row row) => Semigroup (Record row) where
  append = appendRecord (Proxy :: Proxy list)

-- ulib shadow: `concatString` reimplemented over `Purvasm.String`'s byte ops — allocate the
-- combined length, then blit `a` then `b` byte-for-byte — mirroring `concatArray`, so it carries
-- no opaque foreign (was the `Append` intrinsic).
concatString :: String -> String -> String
concatString a b = blitS b na (blitS a 0 (PS.unsafeNew (PI.add na nb)))
  where
  na = PS.byteLength a
  nb = PS.byteLength b
  blitS src off out = go 0 out
    where
    n = PS.byteLength src
    go i acc =
      if PI.lt i n then go (PI.add i 1) (PS.unsafeSetByte acc (PI.add off i) (PS.byteAt src i))
      else acc

-- | Allocate `length xs + length ys` and copy both in — a builder loop over the
-- | `Purvasm.Array` primitives (cf. the registry's JS `concatArray`).
concatArray :: forall a. Array a -> Array a -> Array a
concatArray xs ys =
  blit ys (PA.length xs) (blit xs 0 (PA.unsafeNew (PI.add (PA.length xs) (PA.length ys))))
  where
  blit src off out = go 0 out
    where
    n = PA.length src
    go i acc =
      if PI.lt i n then go (PI.add i 1) (PA.unsafeSet acc (PI.add off i) (PA.unsafeIndex src i))
      else acc

class SemigroupRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class SemigroupRecord rowlist row subrow | rowlist -> subrow where
  appendRecord :: Proxy rowlist -> Record row -> Record row -> Record subrow

instance semigroupRecordNil :: SemigroupRecord RL.Nil row () where
  appendRecord _ _ _ = {}

instance semigroupRecordCons ::
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , SemigroupRecord rowlistTail row subrowTail
  , Semigroup focus
  ) =>
  SemigroupRecord (RL.Cons key focus rowlistTail) row subrow where
  appendRecord _ ra rb = insert (get ra <> get rb) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = appendRecord (Proxy :: Proxy rowlistTail) ra rb
