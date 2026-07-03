-- | ulib SHADOW of `record-studio`'s `Record.Studio.Shrink` (ADR-0038), targeting
-- | record-studio 1.0.4.
-- |
-- | Upstream's `shrinkImpl` is a JS foreign that copies the row's keys into a fresh object
-- | (`keys.reduce((acc, k) => { acc[k] = record[k]; return acc }, {})`). The key list comes
-- | from the type-level row at compile time (`shrink = shrinkImpl (keys (Proxy :: Proxy b))`),
-- | so the copy composes from the untyped record primitives: a fold of
-- | `Record.Unsafe.unsafeSet` (insert-or-replace on every backend) over `unsafeGet` into the
-- | empty record — no foreign remains, and the public interface is unchanged.
module Record.Studio.Shrink (shrink) where

import Data.Foldable (foldl)
import Prim.Row (class Union)
import Record.Studio.Keys (class Keys, keys)
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- An opaque stand-in for a copied field's type: `unsafeGet`'s result only flows into
-- `unsafeSet`, and the value representation is type-erased, so any nominal annotation works —
-- this one exists to name that fact rather than borrow an unrelated type like `Unit`.
data FieldValue

shrinkImpl :: forall r1 r2. Array String -> Record r1 -> Record r2
shrinkImpl ks r = foldl pick (unsafeCoerce {}) ks
  where
  pick :: Record r2 -> String -> Record r2
  pick acc k = unsafeSet k (unsafeGet k r :: FieldValue) acc

shrink :: forall a b r. Union b r a => Keys b => { | a } -> { | b }
shrink = shrinkImpl (keys (Proxy :: Proxy b))
