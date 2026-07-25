-- | The classified emission seam (ADR-0105 §1/§3): the single place where a runtime call's
-- | emitted text and its **safepoint classification** live together. Every `call` the lowering
-- | recipes produce goes through one of the emitters below, naming its [`RtOp`] row (or the
-- | guest-call emitters, whose classification is [`guestCallSafepoint`]); the liveness analysis
-- | (`Backend.LLVM.Liveness`) consults the SAME rows for its transfer functions. An analysis
-- | that drifts from the actual lowering sequence is exactly the failure class slice 2a exposed
-- | (a recipe reading a raw operand after an internal safepoint the analysis did not see) —
-- | after this seam, changing an operation's safepoint-ness or adding a new runtime call forces
-- | an edit HERE, visible to both sides at once, and the recipe-consistency unit tests hold the
-- | per-recipe declarations to the emitted roots.
-- |
-- | Classification ground truth is the runtime's ABI contract (ADR-0064 §4): a safepoint is an
-- | emitted operation that may **allocate on the guest heap or run guest code**. Host-side work
-- | (stdout writes, the shadow-stack machinery, aborts) moves no guest object. `pv_tailcall` is
-- | pinned NOT-a-safepoint (ADR-0105 §1: stash-to-take, no guest alloc in the calling
-- | convention); `pv_prim_set_array` is pinned non-allocating (ADR-0052 linear-builder store).
-- |
-- | Out of seam scope (the exact `tools/seam-audit.sh` allowlist): non-call IR
-- | (`getelementptr`/`load`/`store`/`icmp`/`br`/`phi`/`switch`/`alloca`/`ptrtoint`) — pure
-- | instructions that can never safepoint; the `%ctx = call ptr @pv_runtime_new` ctx-birth
-- | line (it produces the `%ctx` every renderer below emits, and returns `ptr`); and the
-- | per-gdef `@…$init` call lines assembled into `pv_init_all`'s module-skeleton body (the
-- | entry's own call TO `pv_init_all` goes through [`RtOp`]'s `RtInitAll`). `Monad.emit`
-- | rejects any other raw call text at emission time.
module Purvasm.Compiler.Backend.LLVM.Safepoint
  ( RtOp(..)
  , RtArg(..)
  , rtSafepoint
  , rtSym
  , rtCall
  , rtCallWith
  , rtCallVoid
  , guestDirect
  , guestMusttail
  , guestCallSafepoint
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, fresh, unsafeEmitRawCall)
import Purvasm.Compiler.Backend.LLVM.Prim (primSym)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | The closed set of runtime operations the lowering recipes emit calls to. One constructor per
-- | `pv_*` entry point (plus [`RtPrim`] for the whole `pv_prim_*` family via `Prim.primSym`);
-- | the row carries symbol, ctx-taking, return kind and safepoint class — see [`rtDesc`].
data RtOp
  -- generic application / calling convention
  = RtApply
  | RtTailcall
  | RtSettle
  | RtMakeClosure
  -- allocation
  | RtNewStr
  | RtNewNumber
  | RtNewArray
  | RtEmptyArray
  | RtNewAdt
  | RtNewRecord
  | RtRecordGet
  | RtRecordSet
  -- raw object access
  | RtReadField
  | RtWriteField
  | RtReadRaw
  | RtCaseFail
  -- by-need cells
  | RtForceIfByneed
  | RtNewByneedPlaceholder
  | RtByneedSetSuspension
  -- shadow-stack machinery (the `--debug` entry calls; `Backend.LLVM.Root`/`Abi` own their use)
  | RtFrame
  | RtRoot
  | RtGet
  | RtPopFrame
  -- entry-stub lifecycle (never inside a planned activation)
  | RtAbiCheck
  | RtInitAll
  | RtRunEffect
  | RtDrainOutput
  | RtPrintInt
  | RtRuntimeFree
  -- the `pv_prim_*` family (symbol/ctx from `Prim.primSym`)
  | RtPrim PrimOp

-- Generic is what ties the closed sum to MECHANICAL enumeration in the seam unit tests (a new
-- constructor appears in the rep, so the declaration-membership/classification sweeps cannot
-- silently miss it).
derive instance Generic RtOp _

-- | A rendered call operand with its LLVM type.
data RtArg
  = I64 String
  | I32 String
  | Ptr String

type RtDesc =
  { sym :: String
  , ctx :: Boolean -- ^ takes the leading `ptr %ctx`
  , void :: Boolean -- ^ `call void` (no result temp) vs `call i64`
  , sp :: Boolean -- ^ may allocate on the guest heap or run guest code (ADR-0064 §4)
  }

-- | The row table. Every emitter below renders from it and the classifiers read from it — the
-- | single source of truth the ADR-0105 seam pins.
rtDesc :: RtOp -> RtDesc
rtDesc = case _ of
  -- `pv_apply` may run arbitrary guest code (and allocates its result path).
  RtApply -> { sym: "pv_apply", ctx: true, void: false, sp: true }
  -- PINNED not-a-safepoint (ADR-0105 §1): stash-to-take; the trampoline takes the pending tail
  -- only after this frame returns, and the stash itself performs no guest allocation.
  RtTailcall -> { sym: "pv_tailcall", ctx: true, void: true, sp: false }
  -- settling a pending tail runs the stashed guest call.
  RtSettle -> { sym: "pv_settle", ctx: true, void: false, sp: true }
  RtMakeClosure -> { sym: "pv_make_closure", ctx: true, void: false, sp: true }
  RtNewStr -> { sym: "pv_new_str", ctx: true, void: false, sp: true }
  RtNewNumber -> { sym: "pv_new_number", ctx: true, void: false, sp: true }
  RtNewArray -> { sym: "pv_new_array", ctx: true, void: false, sp: true }
  -- ctx-free: returns the immediate empty-array sentinel, no heap access.
  RtEmptyArray -> { sym: "pv_empty_array", ctx: false, void: false, sp: false }
  RtNewAdt -> { sym: "pv_new_adt", ctx: true, void: false, sp: true }
  RtNewRecord -> { sym: "pv_new_record", ctx: true, void: false, sp: true }
  -- read-only projection (the by-need force that may precede it is a separate operation).
  RtRecordGet -> { sym: "pv_record_get", ctx: true, void: false, sp: false }
  -- functional update: allocates the new record.
  RtRecordSet -> { sym: "pv_record_set", ctx: true, void: false, sp: true }
  RtReadField -> { sym: "pv_read_field", ctx: true, void: false, sp: false }
  RtWriteField -> { sym: "pv_write_field", ctx: true, void: true, sp: false }
  RtReadRaw -> { sym: "pv_read_raw", ctx: true, void: false, sp: false }
  -- aborts; control never resumes, so nothing can observe a moved value.
  RtCaseFail -> { sym: "pv_case_fail", ctx: false, void: true, sp: false }
  -- the slow path runs a suspended guest thunk.
  RtForceIfByneed -> { sym: "pv_force_if_byneed", ctx: true, void: false, sp: true }
  RtNewByneedPlaceholder -> { sym: "pv_new_byneed_placeholder", ctx: true, void: false, sp: true }
  -- an in-place store into the already-allocated cell.
  RtByneedSetSuspension -> { sym: "pv_byneed_set_suspension", ctx: true, void: true, sp: false }
  -- shadow-stack machinery moves no guest object (`pv_root` may grow the HOST roots buffer).
  RtFrame -> { sym: "pv_frame", ctx: true, void: false, sp: false }
  RtRoot -> { sym: "pv_root", ctx: true, void: false, sp: false }
  RtGet -> { sym: "pv_get", ctx: true, void: false, sp: false }
  RtPopFrame -> { sym: "pv_pop_frame", ctx: true, void: true, sp: false }
  -- entry-stub lifecycle: host-side work except `pv_run_effect`, which runs the guest program.
  RtAbiCheck -> { sym: "pv_abi_check", ctx: false, void: true, sp: false }
  RtInitAll -> { sym: "pv_init_all", ctx: true, void: true, sp: true }
  RtRunEffect -> { sym: "pv_run_effect", ctx: true, void: false, sp: true }
  RtDrainOutput -> { sym: "pv_drain_output", ctx: true, void: true, sp: false }
  RtPrintInt -> { sym: "pv_print_int", ctx: false, void: true, sp: false }
  RtRuntimeFree -> { sym: "pv_runtime_free", ctx: true, void: true, sp: false }
  RtPrim op ->
    let
      Tuple sym ctx = primSym op
    in
      { sym, ctx, void: false, sp: primSafepoint op }

-- | The row's safepoint class — what `Liveness`'s transfer functions consult.
rtSafepoint :: RtOp -> Boolean
rtSafepoint = _.sp <<< rtDesc

-- | The row's runtime symbol (each must have a matching `declare` in `Abi.declarations`; the
-- | seam unit tests hold that).
rtSym :: RtOp -> String
rtSym = _.sym <<< rtDesc

-- | A direct/`musttail` guest call always runs guest code — the classification the `CApp`/
-- | `CPerform` transfer arms consume for [`guestDirect`]/[`guestMusttail`] (and the generic
-- | `RtApply` path).
guestCallSafepoint :: Boolean
guestCallSafepoint = true

renderArg :: RtArg -> String
renderArg = case _ of
  I64 v -> "i64 " <> v
  I32 v -> "i32 " <> v
  Ptr v -> "ptr " <> v

argList :: RtDesc -> Array RtArg -> String
argList desc args =
  joinWith ", " ((if desc.ctx then [ "ptr %ctx" ] else []) <> map renderArg args)

-- | Emit a value-returning runtime call into a caller-supplied result temp. The caller-side
-- | `fresh` exists for the boot-numbering sites where the result temp is numbered BEFORE the
-- | operand's own emission (OCaml right-to-left argument evaluation in `codegen_llvm.ml` — e.g.
-- | the `SForceCell` force, the dtree `extract` reads); everything else uses [`rtCall`].
rtCallWith :: String -> RtOp -> Array RtArg -> Codegen Unit
rtCallWith t op args =
  let
    desc = rtDesc op
  in
    if desc.void then unsafeCrashWith ("Safepoint.rtCallWith: void operation " <> desc.sym)
    else unsafeEmitRawCall ("  " <> t <> " = call i64 @" <> desc.sym <> "(" <> argList desc args <> ")")

-- | Emit a value-returning runtime call, returning its fresh result temp.
rtCall :: RtOp -> Array RtArg -> Codegen String
rtCall op args = do
  t <- fresh
  rtCallWith t op args
  pure t

-- | Emit a void runtime call.
rtCallVoid :: RtOp -> Array RtArg -> Codegen Unit
rtCallVoid op args =
  let
    desc = rtDesc op
  in
    if not desc.void then unsafeCrashWith ("Safepoint.rtCallVoid: value operation " <> desc.sym)
    else unsafeEmitRawCall ("  call void @" <> desc.sym <> "(" <> argList desc args <> ")")

-- | Emit a direct `tailcc` guest call (`@<name>$d`), returning its result temp. Always a
-- | safepoint ([`guestCallSafepoint`]).
guestDirect :: { dsym :: String, env :: String, args :: Array String } -> Codegen String
guestDirect c = do
  r <- fresh
  unsafeEmitRawCall ("  " <> r <> " = call tailcc i64 @" <> c.dsym <> "(ptr %ctx, i64 " <> c.env <> foldMap (\o -> ", i64 " <> o) c.args <> ")")
  pure r

-- | Emit a `musttail` direct guest call (the caller emits the mandatory `ret` of the result and
-- | pops its frame BEFORE this — ADR-0064 §4). Always a safepoint.
guestMusttail :: { dsym :: String, env :: String, args :: Array String } -> Codegen String
guestMusttail c = do
  r <- fresh
  unsafeEmitRawCall ("  " <> r <> " = musttail call tailcc i64 @" <> c.dsym <> "(ptr %ctx, i64 " <> c.env <> foldMap (\o -> ", i64 " <> o) c.args <> ")")
  pure r

-- | The safepoint classification of each primop's OWN runtime operation (operand forcing is a
-- | separate `RtForceIfByneed` concern, accounted by the `CPrim` transfer). Anything allocating
-- | (or boxing) is a safepoint; in-place/read-only/register-only ops are not. Moved here from
-- | `Liveness` so the row sits beside the emission that realises it (`RtPrim`'s `primSym`).
primSafepoint :: PrimOp -> Boolean
primSafepoint = case _ of
  -- boxed-Number arithmetic / widening box the result
  AddNumber -> true
  SubNumber -> true
  MulNumber -> true
  DivNumber -> true
  IntToNumber -> true
  -- string append and array construction allocate
  Append -> true
  NewArray -> true
  -- functional record updates allocate
  RecordSet -> true
  RecordDelete -> true
  RecordUnion -> true
  -- in-place store (ADR-0052 linear array-builder contract)
  SetArray -> false
  -- register-only scalar ops and comparisons; read-only accessors
  AddInt -> false
  SubInt -> false
  MulInt -> false
  DivInt -> false
  ModInt -> false
  AndInt -> false
  OrInt -> false
  XorInt -> false
  ShlInt -> false
  ShrInt -> false
  ZshrInt -> false
  ComplementInt -> false
  NumberToInt -> false
  EqInt -> false
  EqString -> false
  EqNumber -> false
  EqBool -> false
  LtInt -> false
  LtString -> false
  LtNumber -> false
  AndBool -> false
  OrBool -> false
  NotBool -> false
  IndexArray -> false
  LengthArray -> false
  RecordGet -> false
  RecordHas -> false
