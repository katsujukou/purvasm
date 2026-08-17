-- | The dynamic loader's trusted surface
-- | ([ADR-0111](../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §6).
-- |
-- | Three operations, drawn so that a code pointer is never a guest value: `hostRuntime` and `load`
-- | produce opaque handles, and `resolve` turns a foreign key into a **callable value** without the
-- | `dlsym` address ever leaving the sibling `Loader.c`. `dlsym : Handle -> String -> Effect Int`
-- | would have been the obvious shape and is the wrong one — it puts a function address where guest
-- | arithmetic can reach it, and `pv_make_closure` would believe whatever it was handed.
-- |
-- | `ModuleHandle` is a **table index**, not a `void *` behind a `foreign import data`: an opaque type
-- | is opaque to the type system, while the GC traces whatever the VM stores, so a raw handle in a
-- | purvasm field would be a non-value word wearing a tagged one's clothes. Its only introductions are
-- | `hostRuntime` and `load`, both effectful; nothing here can mint one from an arbitrary `Int`.
-- |
-- | A module is never unloaded. That is why an index cannot go stale, and it is required rather than
-- | convenient: a closure `resolve` built holds a code address into the module.
module Purvasm.VM.Loader
  ( Arity
  , ModuleHandle
  , arity
  , describe
  , hostRuntime
  , load
  , resolve
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Purvasm.Abi.Mangle (mangleForeign)
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Value (ForeignValue)

-- | A loaded provider. Opaque: the constructor is not exported, so the only handles in existence are
-- | the ones the loader itself made — which is also what lets `resolve` be pure (below).
newtype ModuleHandle = ModuleHandle Int

-- | A leaf's **physical** closure arity. A newtype with a checked constructor because the number
-- | reaches `pv_make_closure` as a `uint32_t`, where a negative value becomes an enormous arity and
-- | the closure is then called with garbage. The instruction stream is compiler-generated today, but
-- | `Instruction` admits any `Int`, so the guarantee belongs in a type rather than in a convention.
newtype Arity = Arity Int

-- | An arity, or `Nothing` when it is negative. The image reader validates here as well as the
-- | machine, so a malformed image is refused at decode rather than at the call.
arity :: Int -> Maybe Arity
arity n = if n >= 0 then Just (Arity n) else Nothing

foreign import hostRuntimeImpl :: Effect Int
foreign import loadImpl :: String -> Effect Int
foreign import loadErrorImpl :: Effect String
foreign import describeImpl :: Int -> String

-- | `Just`/`Nothing` are passed *in* so the provider never builds a data value, and so this module
-- | owes the C side no knowledge of any representation.
foreign import resolveImpl
  :: (ForeignValue -> Maybe ForeignValue)
  -> Maybe ForeignValue
  -> Int
  -> String
  -> Int
  -> Maybe ForeignValue

-- | The VM executable itself — the provider of every `pvf_*` the runtime staticlib defines
-- | (ADR-0111 §1.1). It is provider zero and needs no opt-in: it is the binary the user already chose
-- | to run. The resolver treats it as a handle like any other, which is what lets the exactly-one
-- | check name it as a colliding provider.
-- |
-- | Effectful, and deliberately so: establishing it can fail (`dlopen(NULL)`), and that failure must
-- | be reported where it happens. Folded into `resolve` it would come back later, and in the one
-- | disguise a resolver must never wear — "that symbol is absent".
hostRuntime :: Effect ModuleHandle
hostRuntime = do
  index <- hostRuntimeImpl
  if index >= 0 then pure (ModuleHandle index)
  else do
    detail <- loadErrorImpl
    stuck ("cannot open the host as a native provider: " <> detail)

-- | Open a provider at `path`. Loading executes arbitrary native code, so this is only ever reached
-- | from an explicit instruction to load that file (ADR-0111 §4) — never from discovery.
-- |
-- | A failure is stuck rather than a `Maybe`: the caller asked for a specific file by name, and the
-- | diagnostic (the platform's, via `dlerror`) says more than `Nothing` could. A path the boundary
-- | cannot represent — one containing an interior NUL, which `dlopen` would read as a terminator and
-- | so open a *different* file — is refused the same way.
load :: String -> Effect ModuleHandle
load path = do
  index <- loadImpl path
  if index >= 0 then pure (ModuleHandle index)
  else do
    detail <- loadErrorImpl
    stuck ("cannot load native provider " <> show path <> ": " <> detail)

-- | Resolve `key` in one provider to a callable value of the given physical arity, or `Nothing` when
-- | that provider does not define it. Asking each provider separately is what makes the exactly-one
-- | invariant (§4) an honest question rather than an observation of whichever definition won.
-- |
-- | Pure, though ADR-0111 §6 wrote it as `Effect`: every way it could fail *for a reason other than
-- | the provider not defining the key* has been moved into the constructors of its arguments — the
-- | table exists because a `ModuleHandle` exists, and the arity is non-negative because an `Arity`
-- | exists. What remains is a question with a stable answer, so `Nothing` means one thing only.
resolve :: ModuleHandle -> String -> Arity -> Maybe ForeignValue
resolve (ModuleHandle index) key (Arity n) = resolveImpl Just Nothing index (mangleForeign key) n

-- | A provider's name for diagnostics: the path it was loaded from, or `host-runtime`.
describe :: ModuleHandle -> String
describe (ModuleHandle index) = describeImpl index
