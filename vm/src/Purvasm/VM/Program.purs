-- | Loading a decoded image into a run environment
-- | ([ADR-0110](../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §6's slice 2).
-- |
-- | The three global shapes are not three spellings of one thing — they are three *evaluation
-- | strategies*, and the linker chose each one for a reason the loader has to honour (boot's
-- | `Vm.Codegen`):
-- |
-- |   * `Gfun` is a closure. Nothing runs at load time.
-- |   * `Gcaf` is **strict**: built once, at start-up, in the order the linker wrote it. That order is
-- |     a dependency order, which is why loading walks the array rather than building on demand.
-- |   * `Grec` is **by-need**, and that is what makes a cyclic group constructible at all: a member
-- |     that refers to its group resolves when forced, by which time the group is published.
-- |
-- | Collapsing `Gcaf` into `Grec` would look harmless and would change when effects happen; collapsing
-- | `Grec` into `Gcaf` would black-hole every cycle the compiler was careful to allow.
module Purvasm.VM.Program
  ( load
  ) where

import Prelude

import Data.Map as Map
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import Purvasm.VM.Image (Gdef(..), Image)
import Purvasm.VM.Machine (Env, defineGlobal, runBlock)
import Purvasm.VM.Value (Thunk(..), Value(..))

-- | Publish an image's globals, in the linker's order.
load :: Env -> Image -> Effect Unit
load env image = for_ image.gdefs \(name /\ definition) -> case definition of
  Gfun params body -> do
    -- A global function's captured environment is empty: its free names are other globals, and the
    -- machine falls through to the global table when a local misses.
    locals <- Ref.new Map.empty
    defineGlobal env name (VClosure { params, body, env: locals })
  Gcaf body -> do
    value <- runBlock env body Map.empty
    defineGlobal env name value
  Grec body -> do
    cell <- Ref.new (Unbuilt \_ -> runBlock env body Map.empty)
    defineGlobal env name (VThunk cell)
