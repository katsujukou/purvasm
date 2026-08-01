-- | The classified emission seam's invariants (ADR-0105 §1/§3): every row's symbol must have a
-- | matching `declare` in `Abi.declarations` (a seam row that drifts from the declared ABI
-- | surface would link-fail at best and silently misclassify at worst), the safepoint
-- | classification pins (notably `pv_tailcall` NOT-a-safepoint and the ADR-0052 `SetArray`
-- | in-place store), and the rendered call shapes (ctx prefix, void, i32 operands, guest
-- | `tailcc`/`musttail`). Enumeration is MECHANICAL off the `Generic` rep of the closed sums
-- | (`RtOp` including every `RtPrim` payload), so a new constructor is swept automatically —
-- | it cannot be forgotten in a manual list.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Safepoint where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array as Array
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), to)
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Purvasm.Compiler.Backend.LLVM.Abi (declarations)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, makeCx, renderBuffer, runCodegen)
import Purvasm.Compiler.Backend.LLVM.Value (unsafeTestVal, vImm)
import Purvasm.Compiler.Backend.LLVM.Safepoint (RtArg(..), RtOp(..), emitPreparedMusttail, guestCallSafepoint, guestDirect, prepareMusttail, rtCall, rtCallVoid, rtSafepoint, rtSym)
import Purvasm.Compiler.Primitive (PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

emitted :: forall a. Codegen a -> String
emitted m = renderBuffer
  (snd (runCodegen (makeCx { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: true }) m)).fn

-- Force a pure emission inside the Effect runtime so its guard `unsafeCrashWith` surfaces as a
-- caught exception (evaluation deferred into the Effect closure — the `Monad`-test pattern).
expectCrash :: forall a. (Unit -> a) -> Aff Unit
expectCrash thunk = do
  r <- try (liftEffect (void (map (\_ -> thunk unit) (pure unit))))
  isLeft r `shouldEqual` true

-- Mechanical enumeration of a `Generic` sum whose constructors are nullary or carry ONE
-- generically-enumerable payload. Tied to the closed type: a new constructor changes the rep
-- and is enumerated automatically, so the sweeps below cannot silently miss it.
class GEnum rep where
  gEnum :: Array rep

instance gEnumSum :: (GEnum a, GEnum b) => GEnum (Sum a b) where
  gEnum = map Inl gEnum <> map Inr gEnum

instance gEnumCtorNoArgs :: GEnum (Constructor name NoArguments) where
  gEnum = [ Constructor NoArguments ]

instance gEnumCtorArg :: (Generic a repA, GEnum repA) => GEnum (Constructor name (Argument a)) where
  gEnum = map (Constructor <<< Argument <<< to) gEnum

allOf :: forall a rep. Generic a rep => GEnum rep => Array a
allOf = map to gEnum

-- Every RtOp — including one `RtPrim` per PrimOp constructor (enumerated off PrimOp's own rep).
allRtOps :: Array RtOp
allRtOps = allOf

allPrims :: Array PrimOp
allPrims = allOf

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Safepoint" do
  describe "rtSym" do
    it "every row's symbol has a matching declare in Abi.declarations (mechanical sweep)" do
      -- sanity-pin the mechanical enumeration itself: 29 non-prim rows + 38 prim rows.
      Array.length allRtOps `shouldEqual` (29 + Array.length allPrims)
      Array.length allPrims `shouldEqual` 38
      -- `pv_init_all` is the program's OWN symbol (defined by the entry object's emission, not
      -- part of the runtime's declared ABI surface), so it is the one row exempt from the sweep.
      let runtimeDeclared op = rtSym op /= "pv_init_all"
      for_ (Array.filter runtimeDeclared allRtOps) \op ->
        contains (Pattern ("@" <> rtSym op <> "(")) declarations `shouldEqual` true

  describe "rtSafepoint" do
    it "pins pv_tailcall as NOT a safepoint (ADR-0105 §1 stash-to-take)" do
      rtSafepoint RtTailcall `shouldEqual` false

    it "pins SetArray as non-allocating (ADR-0052 linear-builder in-place store)" do
      rtSafepoint (RtPrim SetArray) `shouldEqual` false

    it "pins pv_get as NOT a safepoint — the tie Monad's renderer-owned reload assumes (§6.4)" do
      -- Monad.emitReload emits the pv_get line itself (the one audited raw call outside the
      -- seam) and does NOT bump; if this row ever became sp = true, that reload would break
      -- verify-then-bump. The classification lives here; this test ties the two modules.
      rtSafepoint RtGet `shouldEqual` false

    it "classifies guest-running and allocating operations as safepoints" do
      for_ [ RtApply, RtSettle, RtForceIfByneed, RtMakeClosure, RtNewStr, RtNewNumber, RtNewArray, RtNewAdt, RtNewRecord, RtRecordSet, RtNewByneedPlaceholder, RtRunEffect ]
        \op -> rtSafepoint op `shouldEqual` true
      guestCallSafepoint `shouldEqual` true

    it "classifies read-only access, stores, aborts and shadow-stack machinery as safe" do
      for_ [ RtEmptyArray, RtRecordGet, RtReadField, RtWriteField, RtReadRaw, RtCaseFail, RtByneedSetSuspension, RtFrame, RtRoot, RtGet, RtPopFrame, RtDrainOutput, RtPrintInt ]
        \op -> rtSafepoint op `shouldEqual` false

    it "boxes/allocations in the prim family are safepoints, scalar/read-only prims are not" do
      -- the expected list IS the pin; `allPrims` (mechanical) is the swept domain.
      map rtSym (map RtPrim (Array.filter (rtSafepoint <<< RtPrim) allPrims)) `shouldEqual`
        map rtSym (map RtPrim [ AddNumber, SubNumber, MulNumber, DivNumber, IntToNumber, Append, NewArray, RecordSet, RecordDelete, RecordUnion ])

  describe "rtCall / rtCallVoid" do
    it "renders a ctx-taking value call" do
      emitted (rtCall RtApply [ V (unsafeTestVal "%f"), Ptr "%p", I64 "3" ]) `shouldEqual`
        "  %t1 = call i64 @pv_apply(ptr %ctx, i64 %f, ptr %p, i64 3)\n"

    it "renders a ctx-free value call with no operands" do
      emitted (rtCall RtEmptyArray []) `shouldEqual`
        "  %t1 = call i64 @pv_empty_array()\n"

    it "renders void calls, with and without ctx, including i32 operands" do
      emitted (rtCallVoid RtTailcall [ V (unsafeTestVal "%f"), Ptr "%p", I64 "2" ]) `shouldEqual`
        "  call void @pv_tailcall(ptr %ctx, i64 %f, ptr %p, i64 2)\n"
      emitted (rtCallVoid RtAbiCheck [ I32 "1" ]) `shouldEqual`
        "  call void @pv_abi_check(i32 1)\n"

    it "routes the pv_prim family through primSym's symbol/ctx" do
      emitted (rtCall (RtPrim AddNumber) [ V (unsafeTestVal "%a"), V (unsafeTestVal "%b") ]) `shouldEqual`
        "  %t1 = call i64 @pv_prim_add_number(ptr %ctx, i64 %a, i64 %b)\n"
      emitted (rtCall (RtPrim AddInt) [ V (unsafeTestVal "%a"), V (unsafeTestVal "%b") ]) `shouldEqual`
        "  %t1 = call i64 @pv_prim_add_int(i64 %a, i64 %b)\n"

  describe "the checked renderers (ADR-0105 §6.2 negative tests)" do
    it "a token staled by an sp row's bump crashes at the NEXT consumption (verify-then-bump)" do
      -- `v` mints at epoch 0 (RtEmptyArray is not a safepoint); the RtNewStr emission bumps;
      -- consuming `v` afterwards must fail — the read/use-across-safepoint class.
      expectCrash \_ -> emitted do
        v <- rtCall RtEmptyArray []
        _ <- rtCall RtNewStr [ Ptr "null", I64 "0" ]
        rtCall RtApply [ V v, Ptr "%p", I64 "1" ]

    it "the same-emission operands verify BEFORE the bump (a fresh operand passes into its own sp row)" do
      emitted
        ( do
            v <- rtCall RtEmptyArray []
            rtCall RtApply [ V v, Ptr "%p", I64 "1" ]
        ) `shouldEqual`
        ( "  %t1 = call i64 @pv_empty_array()\n"
            <> "  %t2 = call i64 @pv_apply(ptr %ctx, i64 %t1, ptr %p, i64 1)\n"
        )

    it "a guest value in a raw-metadata position is a schema violation" do
      expectCrash \_ -> emitted (rtCall RtApply [ I64 "%f", Ptr "%p", I64 "3" ])

    it "a raw word in a guest-value position is a schema violation" do
      expectCrash \_ -> emitted (rtCall RtApply [ V (unsafeTestVal "%f"), Ptr "%p", V (vImm "3") ])

    it "an operand-count mismatch against the row schema is a violation" do
      expectCrash \_ -> emitted (rtCall RtApply [ V (unsafeTestVal "%f") ])

  describe "guestDirect / the prepared musttail" do
    it "renders the tailcc direct entry call" do
      emitted (guestDirect { dsym: "foo$d", env: unsafeTestVal "%e", args: [ unsafeTestVal "%a", unsafeTestVal "%b" ] }) `shouldEqual`
        "  %t1 = call tailcc i64 @foo$d(ptr %ctx, i64 %e, i64 %a, i64 %b)\n"
      emitted (guestDirect { dsym: "foo$d", env: unsafeTestVal "%e", args: [] }) `shouldEqual`
        "  %t1 = call tailcc i64 @foo$d(ptr %ctx, i64 %e)\n"

    it "renders the musttail variant through the two-phase prepared form (§6.4)" do
      emitted (prepareMusttail { dsym: "foo$d", env: unsafeTestVal "%e", args: [ unsafeTestVal "%a" ] } >>= emitPreparedMusttail) `shouldEqual`
        "  %t1 = musttail call tailcc i64 @foo$d(ptr %ctx, i64 %e, i64 %a)\n"

    it "a safepoint between prepare and emission crashes fail-closed (§6.4)" do
      -- the HANDOVER happens at the call: an intervening allocation stales the sealed
      -- operands before the callee ever sees them.
      expectCrash \_ -> emitted do
        prepared <- prepareMusttail { dsym: "foo$d", env: unsafeTestVal "%e", args: [] }
        _ <- rtCall RtNewStr [ Ptr "null", I64 "0" ]
        emitPreparedMusttail prepared

    it "non-safepoint work between prepare and emission passes (the pop-shaped positive)" do
      emitted
        ( do
            prepared <- prepareMusttail { dsym: "foo$d", env: unsafeTestVal "%e", args: [] }
            _ <- rtCall RtEmptyArray []
            emitPreparedMusttail prepared
        ) `shouldEqual`
        ( "  %t1 = call i64 @pv_empty_array()\n"
            <> "  %t2 = musttail call tailcc i64 @foo$d(ptr %ctx, i64 %e)\n"
        )
