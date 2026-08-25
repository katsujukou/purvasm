-- | The FFI boundary's contract
-- | ([ADR-0111](../../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §3).
-- |
-- | What is testable here is the half that is target-independent: **which shapes may not cross, and
-- | what they say when they try**. Those errors are as much the boundary's interface as the
-- | conversions are — a shape that crossed silently would either corrupt an identity invariant
-- | (arrays, §3) or hand native code something it cannot enter (a VM closure, ADR-0110 §1.1).
-- |
-- | The supported arms cannot be *observed* from a JS-hosted run: their whole content is that a VM
-- | scalar already IS a runtime value, which is only true in a natively compiled VM. Asserting
-- | anything about them here would be asserting a coercion against itself. They are covered by
-- | `tools/vm-loader-e2e.sh`'s runtime-leaf leg, where a real leaf **reads** them — and the coverage
-- | there is uneven, so it is stated rather than implied:
-- |
-- |   * `VString` — `writeLineImpl` prints it, so a wrong representation prints rubbish;
-- |   * `VNumber` — `floatBitsHi 1.0` must yield 1072693248, which a wrong representation could not
-- |     produce by accident;
-- |   * `VInt` — `showIntImpl 42`, and every `CPerform` run marker;
-- |   * `VCarrier` — `showIntImpl`'s result is passed to `writeLineImpl` undecoded;
-- |   * `VBool` — a loaded-module fixture reads one (`Test.Loader.describeBoolImpl`), since no
-- |     runtime leaf takes a `Boolean` — the `loaded-provider` leg;
-- |   * `VArray` — **promoted**, not converted, so it is not a conversion arm at all: the cell is
-- |     forwarded to a runtime object every alias then shares (§3). Its gate is the aliasing leg of
-- |     `tools/vm-loader-e2e.sh`, where a leaf writes an element and the VM observes the write —
-- |     which is the only place the invariant is observable, since it is about identity rather than
-- |     about any one value;
-- |   * `VData` — built with `pv_new_adt` under a tag derived from the constructor NAME, or, for a
-- |     nullary constructor, as the immediate whose payload IS that tag (ADR-0064 §1). Both shapes,
-- |     and the dispatch back the other way, are the `data-leaves` leg. A JS-hosted assertion could
-- |     only observe that the tag arithmetic ran, which is what `Purvasm.Abi.Mangle`'s own tests are
-- |     for.
module Test.Unit.Purvasm.VM.Foreign (spec) where

import Prelude

import Data.Array as Array
import Data.Either (either)
import Data.List as List
import Data.Map as Map
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message, try)
import Effect.Ref as Ref
import Purvasm.VM.Foreign (toPv)
import Purvasm.VM.Value (Thunk(..), Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Attempt a crossing and give back the diagnostic (or a marker when it wrongly succeeded).
refused :: Value -> Aff String
refused value = liftEffect do
  result <- try (toPv "M.leafImpl" value)
  either (pure <<< message) (const (pure "unexpectedly crossed")) result

contains :: String -> String -> Boolean
contains needle = String.contains (String.Pattern needle)

spec :: Spec Unit
spec = describe "Purvasm.VM.Foreign" do
  describe "toPv (the unsupported arms)" do
    it "names the leaf every refusal happened at" do
      -- The value on the stack says nothing about which `foreign import` demanded it, so the key is
      -- the only thing that makes the error actionable.
      diagnostic <- refused (VRecord Map.empty)
      diagnostic `shouldSatisfy` contains "M.leafImpl"

    it "refuses a record, in both directions and on both backends" do
      diagnostic <- refused (VRecord (Map.singleton "a" (VInt 1)))
      diagnostic `shouldSatisfy` contains "a record"

    it "refuses a partially applied constructor" do
      diagnostic <- refused (VCtor "Tuple" 2 (List.singleton (VInt 1)))
      diagnostic `shouldSatisfy` contains "Tuple"

    it "refuses a VM closure, which is not a runtime closure at all" do
      envRef <- liftEffect (Ref.new Map.empty)
      diagnostic <- refused (VClosure { params: [ "x" ], body: [], env: envRef })
      diagnostic `shouldSatisfy` contains "closure"

    it "refuses a partially applied VM closure" do
      envRef <- liftEffect (Ref.new Map.empty)
      diagnostic <- refused (VPap { params: [ "x", "y" ], body: [], env: envRef } (List.singleton (VInt 1)))
      diagnostic `shouldSatisfy` contains "closure"

    it "treats an unforced cell as a VM defect, not a program error" do
      -- The caller forces; a thunk arriving here means a missing force at the one boundary where an
      -- unforced value would reach native code. Forcing it quietly would hide that.
      cell <- liftEffect (Ref.new (Built (VInt 1)))
      diagnostic <- refused (VThunk cell)
      diagnostic `shouldSatisfy` contains "VM defect"

    it "says `foreign boundary` in every refusal" do
      -- The shared prefix is what makes these greppable as one class, and it is boot's wording.
      envRef <- liftEffect (Ref.new Map.empty)
      diagnostics <- traverse refused
        [ VRecord Map.empty
        , VCtor "Y" 2 List.Nil
        , VClosure { params: [], body: [], env: envRef }
        ]
      Array.length (Array.filter (contains "foreign boundary") diagnostics) `shouldEqual` 3
