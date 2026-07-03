-- | ulib SHADOW of `record-studio`'s `Record.Studio.SingletonRecord` (ADR-0038), targeting
-- | record-studio 1.0.4.
-- |
-- | Upstream's `unsafeGetFirstField :: forall r a. { | r } -> a` is a JS foreign that reads a
-- | record's only field *without its name* — unimplementable as written on the opaque id-keyed
-- | record representation (ADR-0069: ids are hashes, no string recovery). The name is never
-- | actually unknown, though: the `SingletonRecordFields` machinery recovers it from the
-- | type-level row, and its resolving instance already carries `IsSymbol key`.
-- |
-- | The one representation-imposed constraint on the rewrite is that **instance contexts are a
-- | frozen ABI**: a consumer's corefn (compiled against the *registry* module) applies each
-- | instance builder to exactly the registry context's dictionaries, and the ulib overlay swaps
-- | in these definitions under those call sites — so adding a constraint (e.g. `IsSymbol key`
-- | on the `SingletonRecord` instance) changes the builder's arity and leaves the dictionary a
-- | partial application. Classes, instance heads, contexts, and declaration order below are
-- | therefore kept verbatim from upstream; the key string instead travels through a **new
-- | `firstFieldKey` member** on `SingletonRecordFields` — the dictionary record is built by the
-- | instance builders and projected by the member accessors, both of which live in (and are
-- | overlaid with) this module, so widening the member set is ABI-invisible to consumers.
-- | (Residual: a hand-written downstream `SingletonRecordFields` instance would lack the new
-- | member — such an instance is already precluded in practice by the functional dependency and
-- | the `Fail` chain.) No foreign remains; the public interface only widens.
module Record.Studio.SingletonRecord
  ( class SingletonRecord
  , class SingletonRecordFields
  , key
  , value
  , singletonRecordFields
  , firstFieldKey
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Beside, Quote, QuoteLabel, Text)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class SingletonRecord :: forall k1 k2. k1 -> Type -> Row Type -> k2 -> Constraint
class SingletonRecord key value rec recRL | rec -> key value recRL where
  -- | Get the key of a record with only one field as a `Proxy`
  key :: Record rec -> Proxy key
  value :: Record rec -> value

instance
  ( RowToList rec recRL
  , SingletonRecordFields key a rec recRL
  ) =>
  SingletonRecord key a rec rl where
  key = singletonRecordFields (Proxy :: Proxy recRL)
  value r = unsafeGet (firstFieldKey (Proxy :: Proxy recRL) r) r
else instance (Fail (Text "The record must have exactly one field")) => SingletonRecord key a rec recRL where
  key _ = unsafeCoerce unit
  value _ = unsafeCoerce unit

class SingletonRecordFields :: forall k1 k2. k1 -> Type -> Row Type -> k2 -> Constraint
class
  SingletonRecordFields key value rec recRL
  | rec -> key value
  where
  singletonRecordFields :: Proxy recRL -> { | rec } -> Proxy key
  -- | The record's only field name, reflected from the type-level row (ulib addition — the
  -- | pure replacement for upstream's `unsafeGetFirstField` foreign).
  firstFieldKey :: Proxy recRL -> { | rec } -> String

instance
  ( RowToList rec (RL.Cons key a RL.Nil)
  , Row.Cons key a () rec
  , IsSymbol key
  ) =>
  SingletonRecordFields key a rec (RL.Cons key a RL.Nil) where
  singletonRecordFields _ _ = (Proxy :: Proxy key)
  firstFieldKey _ _ = reflectSymbol (Proxy :: Proxy key)

instance
  ( Fail (Beside ErrorMsg (Text "instead of {}"))
  ) =>
  SingletonRecordFields key a rec RL.Nil where
  singletonRecordFields _ _ = unsafeCoerce unit
  firstFieldKey _ _ = unsafeCoerce unit

else instance
  ( Fail
      ( ErrorMsg
          ++ ButReceivedStart
          ++ (QuoteLabel key ++ DblCol ++ Quote a)
          ++ Comma
          ++ (QuoteLabel key1 ++ DblCol ++ Quote a1)
          ++ ButReceivedEnd
      )
  ) =>
  SingletonRecordFields key a rec (RL.Cons key a (RL.Cons key1 a1 rl)) where
  singletonRecordFields _ _ = unsafeCoerce unit
  firstFieldKey _ _ = unsafeCoerce unit

type ErrorMsg = (Text "Must provide a record with exactly one field ")
type ButReceivedStart = (Text "instead of { ")
type ButReceivedEnd = (Text ", ... }")
type DblCol = (Text " :: ")
type Comma = (Text ", ")

infixl 3 type Beside as ++
