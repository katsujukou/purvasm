-- | The loader's *type-level* guarantees
-- | ([ADR-0111](../../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §6).
-- |
-- | Only the part that is target-independent lives here: `Arity`'s constructor, which is what lets
-- | `resolve` be pure and what stands between a negative `Int` and `pv_make_closure`'s `uint32_t`
-- | (where it becomes an enormous arity and the closure is then called with garbage).
-- |
-- | Everything else the boundary promises — that a path with an interior NUL fails by name, that a
-- | long path or symbol is never truncated into a *different* file or leaf, that `describe` gives a
-- | loaded provider's own path, and that `Nothing` means only "this provider does not define it" —
-- | is about `dlopen`, so it cannot be observed from a JS-hosted run at all: there are no `pvf_*`
-- | symbols to resolve. Those belong to the native fixture, and asserting them here against the
-- | stub JS provider would prove nothing while looking like coverage.
module Test.Unit.Purvasm.VM.Loader (spec) where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Purvasm.VM.Loader (arity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.VM.Loader" do
  describe "Arity" do
    it "refuses a negative arity" do
      isNothing (arity (-1)) `shouldEqual` true
      isNothing (arity (-2147483648)) `shouldEqual` true

    it "accepts zero" do
      -- A nullary leaf is ordinary: an `Effect a` with no data argument has physical arity 1, but a
      -- pure nullary one is 0, and `pv_make_closure` is happy with either.
      isJust (arity 0) `shouldEqual` true

    it "accepts the largest Int, since the bound is on the sign only" do
      isJust (arity 2147483647) `shouldEqual` true

    it "is the only way to make one" do
      -- `Arity` has no exported constructor, so this is the whole surface: an `Int` that survives
      -- `arity` is the only thing `resolve` can be handed.
      case arity 3 of
        Just _ -> pure unit
        Nothing -> shouldEqual "an arity" "nothing"
