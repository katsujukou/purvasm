-- | `ulib` shadow of `Effect.Unsafe` (ADR-0038, demanded by the `Data.Lazy` shadow's
-- | memoisation): under the purvasm `Effect` representation an effect value IS a one-argument
-- | closure performed by applying it to `unit` (the structural `Effect` providers' contract,
-- | ADR-0023), so running one "now" is exactly that application — exposed through
-- | `unsafeCoerce`, the same move the upstream JS implementation makes against ITS
-- | representation. Pure PureScript; no foreign, no leaf, all backends.
module Effect.Unsafe where

import Effect (Effect)
import Data.Unit (Unit, unit)
import Unsafe.Coerce (unsafeCoerce)

-- | Run an effectful computation.
-- |
-- | *Note*: use of this function can result in arbitrary side-effects.
unsafePerformEffect :: forall a. Effect a -> a
unsafePerformEffect eff = (unsafeCoerce eff :: Unit -> a) unit
