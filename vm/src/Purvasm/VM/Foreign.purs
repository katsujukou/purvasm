-- | The FFI boundary: the one place the VM knows the native representation
-- | ([ADR-0111](../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §3).
-- |
-- | Everything above this module works in `Value` and everything below it works in runtime words, so
-- | the bytecode format stays free of any native encoding (ADR-0110 §3's principle) and exactly one
-- | file has to be re-read when the ABI changes.
-- |
-- | **Going in is not a conversion for scalars and strings, and that is not a shortcut.** The VM is
-- | itself a purvasm program compiled by the same backend, so its `Int`, `Number`, `Boolean` and
-- | `String` *are already* runtime values of the very representation a leaf expects — one heap, one
-- | `pv_*` implementation (ADR-0111 fact 2). Rebuilding them through `pv_new_str` and friends would
-- | copy a value into itself. What the boundary owes is therefore not a re-encoding but a **type**:
-- | `unsafeAsForeign` states where a `Value`'s payload stops being the VM's and becomes a word the
-- | runtime owns, and it is not exported (the CLAUDE.md rule for an expression whose type the
-- | compiler cannot check).
-- |
-- | **Coming out is not a conversion either** — there is nothing to convert *into*. The ABI answers
-- | typed questions but never "what kind is this word?", so a returned value enters as an opaque
-- | carrier and is decoded at the site that eliminates it, which already knows the shape it demands.
-- | That is why this module has a `toPv` and no `ofPv`.
module Purvasm.VM.Foreign
  ( applyForeign
  , toPv
  ) where

import Prelude

import Effect (Effect)
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Value (ForeignValue, Value(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Reinterpret a VM scalar's payload as the runtime word it already is (see the module note). Only
-- | ever applied to the payload of a `VInt`/`VNumber`/`VBool`/`VString`, where the VM's compiled
-- | representation and the ABI's are the same thing — never to a `Value`, which is a VM-owned ADT
-- | the runtime knows nothing about.
unsafeAsForeign :: forall a. a -> ForeignValue
unsafeAsForeign = unsafeCoerce

-- | A named boundary error, in the shape boot's `Vm.Foreign` already used: what crossed, and which
-- | leaf it was crossing into. The key matters more than it looks — the value on the stack says
-- | nothing about which `foreign import` demanded it.
boundary :: forall a. String -> String -> Effect a
boundary key what = stuck ("foreign boundary: " <> what <> " crossed " <> key)

-- | Convert an argument for a native leaf. Total over what a first-order leaf can take today;
-- | every other shape is a named error rather than a silent coercion.
-- |
-- | The caller forces first: this takes an already-forced value, and a `VThunk` reaching here is the
-- | machine's bug, not the program's — so it says so rather than forcing behind the caller's back,
-- | which would hide a missing force at the one boundary where an unforced value would be handed to
-- | native code.
-- |
-- | The unsupported arms are the boundary's contract as much as the supported ones:
-- |
-- |   * **arrays** are identity-bearing and must be *promoted*, not copied (§3) — slice 3. Refusing
-- |     them here is what keeps the identity invariant from being violated even transiently;
-- |   * **records** cannot cross in either direction on either backend (§3, fact 4): label ids are
-- |     minted only by codegen, and no supported call hands one to a provider;
-- |   * **data values** need `pv_new_adt` with a tag derived from the constructor name — slice 5;
-- |   * **closures and partial constructors** are guest-level values the runtime cannot enter at all
-- |     (ADR-0110 §1.1): a VM closure is a code block plus an environment, not a code address.
toPv :: String -> Value -> Effect ForeignValue
toPv key = case _ of
  VInt n -> pure (unsafeAsForeign n)
  VNumber f -> pure (unsafeAsForeign f)
  -- The one supported arm with no native coverage yet: no runtime leaf takes a `Boolean`, so nothing
  -- reads this across the boundary until a loaded module can be called (slice 3).
  VBool b -> pure (unsafeAsForeign b)
  VString s -> pure (unsafeAsForeign s)
  -- A value that came from a leaf goes back unchanged: it never stopped being a runtime value, and
  -- decoding it was never possible (§3's opacity).
  VCarrier _ fv -> pure fv
  VArray _ -> boundary key "an array (promotion is ADR-0111 §3)"
  VRecord _ -> boundary key "a record (records do not cross on either backend, ADR-0111 §3)"
  VData tag _ -> boundary key ("a data value (" <> tag <> "; `pv_adt_tag` is ADR-0111 §3)")
  VCtor tag _ _ -> boundary key ("a partially applied constructor (" <> tag <> ")")
  VClosure _ -> boundary key "a VM closure (guest closures are not runtime closures, ADR-0110 §1.1)"
  VPap _ _ -> boundary key "a partially applied VM closure"
  VThunk _ -> stuck ("foreign boundary: an unforced value reached " <> key <> " (a VM defect)")

-- | `pv_apply`, via the sibling `Foreign.c`. The JS stub throws instead: there is no runtime there to
-- | apply anything with, and a target-availability flag would only move the same failure earlier.
foreign import applyImpl :: ForeignValue -> Array ForeignValue -> Effect ForeignValue

-- | Apply a carrier-held runtime closure to already-converted arguments — `pv_apply`, which is the
-- | calling convention for **carrier values only** (ADR-0111 §2). Arity dispatch, over- and
-- | under-application, and by-need forcing within that carrier are the runtime's, and behave exactly
-- | as they do in a compiled program because they *are* the compiled program's paths. The VM
-- | contributes the argument conversion above and nothing else.
-- |
-- | This is also how a guest `Effect` whose action is a leaf's thunk runs: the machine applies it to
-- | the run marker like any other call, and the result comes back as a carrier.
applyForeign :: ForeignValue -> Array ForeignValue -> Effect ForeignValue
applyForeign = applyImpl
