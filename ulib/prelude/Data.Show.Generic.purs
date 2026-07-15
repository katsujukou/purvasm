-- | ulib SHADOW of `prelude`'s `Data.Show.Generic` (ADR-0038 / ADR-0094), targeting prelude 6.0.2.
-- |
-- | The upstream `intercalate :: String -> Array String -> String` (JS `Array.prototype.join`) is a
-- | **JS-only** foreign, so on the native backend a reference to it is an unresolved leaf (`genericShow`
-- | of any multi-argument constructor reaches it). Here it is reimplemented in pure PureScript over the
-- | `Purvasm.Array` / `Purvasm.Int` purvasm-base primitives — the same primitive layer `Data.Show`'s
-- | shadow uses — so the module carries **no foreign** and stands alone on every backend. This adds **no
-- | dependency outside `prelude` + purvasm-base** (prelude is the root package): the primitives resolve as
-- | `LengthArray`/`IndexArray`/`LtInt`/`AddInt`/`EqInt` primops. Everything else is verbatim upstream — the
-- | public `GenericShow`/`GenericShowArgs` class + instance surface (a frozen ABI, [ulib-instance-context-abi])
-- | is unchanged, so this is drop-in.
module Data.Show.Generic
  ( class GenericShow
  , genericShow'
  , genericShow
  , class GenericShowArgs
  , genericShowArgs
  ) where

import Prelude (class Show, show, (<>))
import Data.Generic.Rep
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Int as PI

class GenericShow a where
  genericShow' :: a -> String

class GenericShowArgs a where
  genericShowArgs :: a -> Array String

instance genericShowNoConstructors :: GenericShow NoConstructors where
  genericShow' a = genericShow' a

instance genericShowArgsNoArguments :: GenericShowArgs NoArguments where
  genericShowArgs _ = []

instance genericShowSum :: (GenericShow a, GenericShow b) => GenericShow (Sum a b) where
  genericShow' (Inl a) = genericShow' a
  genericShow' (Inr b) = genericShow' b

instance genericShowArgsProduct ::
  ( GenericShowArgs a
  , GenericShowArgs b
  ) =>
  GenericShowArgs (Product a b) where
  genericShowArgs (Product a b) = genericShowArgs a <> genericShowArgs b

instance genericShowConstructor ::
  ( GenericShowArgs a
  , IsSymbol name
  ) =>
  GenericShow (Constructor name a) where
  genericShow' (Constructor a) =
    case genericShowArgs a of
      [] -> ctor
      args -> "(" <> intercalate " " ([ ctor ] <> args) <> ")"
    where
    ctor :: String
    ctor = reflectSymbol (Proxy :: Proxy name)

instance genericShowArgsArgument :: Show a => GenericShowArgs (Argument a) where
  genericShowArgs (Argument a) = [ show a ]

-- | A `Generic` implementation of the `show` member from the `Show` type class.
genericShow :: forall a rep. Generic a rep => GenericShow rep => a -> String
genericShow x = genericShow' (from x)

-- `intercalate sep xs` = `xs.join(sep)`: the elements joined with `sep` between them (no leading or
-- trailing separator; `[]` → `""`, `[a]` → `a`). Reimplemented over the array/int primops, in the same
-- accumulate-with-a-separator-after-the-first shape as `Data.Show`'s `showArrayImpl`.
intercalate :: String -> Array String -> String
intercalate sep xs = go 0 ""
  where
  m = PA.length xs
  go i acc =
    if PI.lt i m then go (PI.add i 1) (if PI.eq i 0 then PA.unsafeIndex xs i else acc <> sep <> PA.unsafeIndex xs i)
    else acc
