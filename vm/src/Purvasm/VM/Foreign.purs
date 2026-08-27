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
  ( adtField
  , adtTag
  , applyForeign
  , arrayLength
  , booleanOf
  , forceCarrier
  , intOf
  , numberOf
  , promote
  , readField
  , stringOf
  , toPv
  , writeField
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref as Ref
import Purvasm.Abi.Mangle (ctorTag)
import Purvasm.Array as PA
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Value (ArrayCell, ArrayStorage(..), ForeignValue, Value(..))
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
-- | **Data values DO cross**: built with `pv_new_adt` under a tag derived from the constructor name,
-- | or — for a nullary constructor — as the immediate whose payload is that tag. They are not on
-- | either list below for that reason.
-- |
-- | **Arrays do not convert at all — they are promoted** (§3), which is why they are not on either
-- | list below: an elementwise copy would be a correctness bug rather than a cost, since a leaf's
-- | write would land on the copy and two VM bindings holding the same array would stop agreeing.
-- | `promote` forwards the shared cell to a runtime object, once and permanently.
-- |
-- | The unsupported arms are the boundary's contract as much as the supported ones:
-- |
-- |   * **records** cannot cross in either direction on either backend (§3, fact 4): label ids are
-- |     minted only by codegen, and no supported call hands one to a provider;

-- |   * **closures and partial constructors** are guest-level values the runtime cannot enter at all
-- |     (ADR-0110 §1.1): a VM closure is a code block plus an environment, not a code address.
toPv :: String -> Value -> Effect ForeignValue
toPv key = case _ of
  VInt n -> pure (unsafeAsForeign n)
  VNumber f -> pure (unsafeAsForeign f)
  -- Read across the boundary by a loaded module's leaf (`Test.Loader.describeBoolImpl`), since no
  -- runtime leaf takes a `Boolean`.
  VBool b -> pure (unsafeAsForeign b)
  VString s -> pure (unsafeAsForeign s)
  -- A value that came from a leaf goes back unchanged: it never stopped being a runtime value, and
  -- decoding it was never possible (§3's opacity).
  VCarrier _ fv -> pure fv
  -- An array is PROMOTED, not converted (§3). An elementwise copy would be a correctness bug rather
  -- than a cost: a leaf's write would land on the copy, and two VM bindings holding the same array
  -- would stop agreeing.
  VArray cell -> promote cell
  VRecord _ -> boundary key "a record (records do not cross to native code on either backend)"
  -- A data value is built with the native backend's own tag derivation over the constructor NAME the
  -- bytecode carries (§3). Nothing stores a tag anywhere: `ctorTag` is a pure function of the name,
  -- shared with codegen from one definition, so the two sides agree by construction.
  VData tag fields
    -- A nullary constructor has NO heap object — it is an immediate — so it goes to its own ABI
    -- entry. The VM does not encode that itself: `pv_new_nullary_adt` does, which is what keeps the
    -- representation the runtime's (ADR-0069) rather than something the boundary reimplements.
    | Array.null fields -> pure (newNullaryAdtImpl (ctorTag tag))
    | otherwise -> traverse (toPv key) fields >>= newAdtImpl (ctorTag tag)
  VCtor tag _ _ -> boundary key ("a partially applied constructor (" <> tag <> ")")
  VClosure _ -> boundary key "a VM closure (a guest closure is not a native one)"
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

-- | `blankArrayImpl 0` is the canonical empty array, so there is no separate nullary import for it —
-- | and that is not tidiness. A `foreign import` of arity 0 does not reach the VM as a *value*: it
-- | reaches it as a closure of arity 0 (`leafClosureArity` over a non-function type), which a leaf
-- | then receives instead of the array. Measured, as `array_len on a non-Array object: Closure`.
foreign import blankArrayImpl :: Int -> Effect ForeignValue
foreign import arrayLengthImpl :: ForeignValue -> Int
foreign import readFieldImpl :: ForeignValue -> Int -> Effect ForeignValue
foreign import writeFieldImpl :: ForeignValue -> Int -> ForeignValue -> Effect Unit

-- | A promoted array's length. Unchanging — only its slots are mutable — so this needs no `Effect`.
arrayLength :: ForeignValue -> Int
arrayLength = arrayLengthImpl

-- | Read a promoted array's slot. Effectful because a leaf may have written it since the last read:
-- | that is the entire point of promoting rather than copying.
readField :: ForeignValue -> Int -> Effect ForeignValue
readField = readFieldImpl

-- | Write a promoted array's slot, converting the value at the boundary. The `key` is the origin
-- | reported if the written value cannot cross.
writeField :: String -> ForeignValue -> Int -> Value -> Effect Unit
writeField key carrier i v = toPv key v >>= writeFieldImpl carrier i

-- | Promote an array's cell in place, once and permanently (ADR-0111 §3).
-- |
-- | The step ORDER is the substance here, and it is the order a copying collector installs a
-- | forwarding pointer in:
-- |
-- |   1. `n = 0` → the canonical empty array; write `Promoted` and stop. (Two VM empty arrays
-- |      therefore promote to one object. Unobservable: an empty array has no slot to write, and
-- |      purvasm has no value-identity primitive.)
-- |   2. `n >= 1` → build the runtime array **blank**, because the ABI has no blank-array
-- |      constructor to build it any other way.
-- |   3. **Write `Promoted` into the cell NOW**, before any element is migrated. A cycle — an array
-- |      reachable from itself, directly or through a data value — then terminates on finding the
-- |      cell already promoted, instead of recurring forever.
-- |   4. Migrate the elements by the same boundary rules, which promotes nested arrays recursively.
-- |
-- | Rooting needs no shadow-stack work: step 3 puts the carrier in an ordinary PureScript field, so
-- | the GC traces AND updates it like any other value, and each element migration re-reads it from
-- | the cell. The VM never holds a runtime word outside a traced field — a property of being a
-- | purvasm program rather than a foreign leaf.
promote :: ArrayCell -> Effect ForeignValue
promote cell = Ref.read cell >>= case _ of
  -- Already promoted: this is both the idempotence of promotion and the cycle's base case.
  Promoted carrier -> pure carrier
  Local values -> do
    let n = PA.length values
    -- Step 1 and step 2 are one call: `blankArrayImpl` answers the canonical empty array for `n = 0`
    -- (the ABI's `pv_empty_array`) and a unit-filled array otherwise. Two VM empty arrays therefore
    -- promote to ONE object, which is unobservable — an empty array has no slot to write, and
    -- purvasm has no value-identity primitive.
    carrier <- blankArrayImpl n
    -- Step 3, and it happens BEFORE any element is migrated: a cycle terminates on finding the cell
    -- already promoted, exactly as a copying collector's forwarding pointer works.
    Ref.write (Promoted carrier) cell
    if n == 0 then pure carrier
    else do
      -- `tailRecM`, not a plain recursion or a `traverse_`: an `Effect` bind is a host call, and an
      -- array is as long as the guest made it. The migration must not be bounded by the host stack.
      tailRecM
        ( \i ->
            if i >= n then pure (Done unit)
            else do
              writeField "a promoted array" carrier i (PA.unsafeIndex values i)
              pure (Loop (i + 1))
        )
        0
      pure carrier

foreign import intOfImpl :: ForeignValue -> Int
foreign import numberOfImpl :: ForeignValue -> Number
foreign import booleanOfImpl :: ForeignValue -> Boolean
foreign import stringOfImpl :: ForeignValue -> String
foreign import forceCarrierImpl :: ForeignValue -> Effect ForeignValue

-- | Decode a carrier at a site that already knows the shape it demands (ADR-0111 §3).
-- |
-- | These are **demands, not questions**: the ABI answers no "what kind is this word?", so there is
-- | nothing to ask. The bytecode is generated from a well-typed program, so an `AddInt` demanding an
-- | `Int` is entitled to one, and the runtime's own shape check is what enforces the entitlement —
-- | a mis-shaped carrier aborts there, exactly as a mis-shaped leaf argument does. Nothing here
-- | branches on representation, so ADR-0069's opacity survives.
-- |
-- | Not `Effect`: reading a scalar mutates nothing and allocates nothing. A wrong shape is a fault,
-- | not a value, so there is no failure to sequence.
intOf :: ForeignValue -> Int
intOf = intOfImpl

numberOf :: ForeignValue -> Number
numberOf = numberOfImpl

booleanOf :: ForeignValue -> Boolean
booleanOf = booleanOfImpl

stringOf :: ForeignValue -> String
stringOf = stringOfImpl

-- | Force a carrier through the runtime's by-need discipline (`pv_force_if_byneed`, ADR-0070), which
-- | passes a non-cell through unchanged. The VM forces its OWN thunks at every site that inspects a
-- | value's shape; this is that same rule applied to what a leaf handed back, and it is effectful for
-- | the same reason — forcing a cell runs its suspension.
forceCarrier :: ForeignValue -> Effect ForeignValue
forceCarrier = forceCarrierImpl

foreign import newAdtImpl :: Int -> Array ForeignValue -> Effect ForeignValue
foreign import newNullaryAdtImpl :: Int -> ForeignValue
foreign import adtTagImpl :: ForeignValue -> Int

-- | The constructor tag of a data value a leaf returned, for `SwitchCtor` to dispatch on (ADR-0111
-- | §3). Compared against each arm's `ctorTag name` — the same derivation `toPv` uses going the other
-- | way, which is why the bytecode can keep carrying names and still meet a native ADT.
-- |
-- | This is the one accessor the foreign API did not have, and without it a leaf could not RETURN a
-- | data value at all: no `Maybe`, no `Either`. It answers for a nullary constructor as well, because
-- | a caller holding an opaque word cannot tell a nullary one from a field-carrying one — and asking
-- | is exactly what §3's opacity forbids.
-- |
-- | Its check is weaker than the scalar accessors', and the difference is worth stating: a heap value
-- | that is not an ADT aborts, but a nullary constructor is an immediate and so is indistinguishable
-- | from an `Int`, a `Boolean` or `Unit`. Nothing can close that — the representation does not
-- | distinguish them — so this rests on the caller being a site whose type already established the
-- | shape, which is what a compiler-emitted `SwitchCtor` is.
adtTag :: ForeignValue -> Int
adtTag = adtTagImpl

foreign import adtFieldImpl :: ForeignValue -> Int -> Effect ForeignValue

-- | Field `i` of a data value a leaf returned, or `Nothing` when `i` is negative — the same answer
-- | `Data.Array.index` gives for a VM-local data value, so a caller can report one diagnostic for
-- | both representations.
-- |
-- | Distinct from [readField] because an `Adt`'s payload carries its tag in word 0, so a field is one
-- | slot further along — one accessor per layout rather than one accessor and a convention to
-- | remember. That offset is also why the sign matters here and not merely as hygiene: `-1` would
-- | address slot 0 and hand back the raw TAG, which is not a value word at all.
-- |
-- | An index past the last field is NOT caught here: the runtime's own bounds check answers that,
-- | the same way it does for an array.
adtField :: ForeignValue -> Int -> Effect (Maybe ForeignValue)
adtField carrier i
  | i < 0 = pure Nothing
  | otherwise = Just <$> adtFieldImpl carrier i
