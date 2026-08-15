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
  , rtCallVoid
  , machineryHandleCall
  , guestDirect
  , PreparedCall
  , prepareMusttail
  , emitPreparedMusttail
  , guestCallSafepoint
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, bumpEpoch, currentEpoch, forA, fresh, unsafeEmitRawCall, unsafeMintFresh, unsafeUseVal)
import Purvasm.Compiler.Backend.LLVM.Prim (primArity, primSym)
import Purvasm.Compiler.Backend.LLVM.Value (Val)
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
  -- ADR-0108 §3 measurement instrumentation (an OPT-IN build only; `sp = false` — a counter bump
  -- neither allocates nor runs guest code, so the activation plan is unchanged by it)
  | RtApplyProfileRegister
  | RtApplyProfileBump
  -- | ADR-0108 §4 drill: count against an emitted KEY string (instrumented builds only)
  | RtApplyProfileKey
  -- the `pv_prim_*` family (symbol/ctx from `Prim.primSym`)
  | RtPrim PrimOp

-- Generic is what ties the closed sum to MECHANICAL enumeration in the seam unit tests (a new
-- constructor appears in the rep, so the declaration-membership/classification sweeps cannot
-- silently miss it).
derive instance Generic RtOp _

-- | A call operand with its LLVM type and its ROLE (ADR-0105 §6.2): `V` is a guest value —
-- | an i64 word the GC may move, flowing as a verified token — while `I64`/`I32`/`Ptr` are
-- | RAW METADATA (counts, label ids, tags, buffer/constant pointers, root handles). Each row
-- | declares its argument schema and the renderer checks the supplied kinds against it, so a
-- | call site cannot bypass token verification by passing a guest value in a raw position.
data RtArg
  = V Val
  | I64 String
  | I32 String
  | Ptr String

-- | One row's expected operand roles. `Vals n` is the `pv_prim_*` family (every operand a
-- | guest value; the arity comes from `Prim.primArity`, so an operand-count drift is a
-- | schema violation, not a downstream LLVM error).
data ArgSchema
  = Fixed (Array Role)
  | Vals Int

data Role
  = RVal
  | RI64
  | RI32
  | RPtr

type RtDesc =
  { sym :: String
  , ctx :: Boolean -- ^ takes the leading `ptr %ctx`
  , void :: Boolean -- ^ `call void` (no result temp) vs `call i64`
  , sp :: Boolean -- ^ may allocate on the guest heap or run guest code (ADR-0064 §4)
  , schema :: ArgSchema -- ^ the operand roles (ADR-0105 §6.2 — guest value vs raw metadata, per position)
  }

-- | The row table. Every emitter below renders from it and the classifiers read from it — the
-- | single source of truth the ADR-0105 seam pins.
rtDesc :: RtOp -> RtDesc
rtDesc = case _ of
  -- `pv_apply` may run arbitrary guest code (and allocates its result path).
  RtApply -> { sym: "pv_apply", ctx: true, void: false, sp: true, schema: Fixed [ RVal, RPtr, RI64 ] }
  -- PINNED not-a-safepoint (ADR-0105 §1): stash-to-take; the trampoline takes the pending tail
  -- only after this frame returns, and the stash itself performs no guest allocation.
  RtTailcall -> { sym: "pv_tailcall", ctx: true, void: true, sp: false, schema: Fixed [ RVal, RPtr, RI64 ] }
  -- settling a pending tail runs the stashed guest call.
  RtSettle -> { sym: "pv_settle", ctx: true, void: false, sp: true, schema: Fixed [ RVal ] }
  RtMakeClosure -> { sym: "pv_make_closure", ctx: true, void: false, sp: true, schema: Fixed [ RI64, RI32, RVal ] }
  RtNewStr -> { sym: "pv_new_str", ctx: true, void: false, sp: true, schema: Fixed [ RPtr, RI64 ] }
  RtNewNumber -> { sym: "pv_new_number", ctx: true, void: false, sp: true, schema: Fixed [ RI64 ] }
  RtNewArray -> { sym: "pv_new_array", ctx: true, void: false, sp: true, schema: Fixed [ RPtr, RI64 ] }
  -- ctx-free: returns the immediate empty-array sentinel, no heap access.
  RtEmptyArray -> { sym: "pv_empty_array", ctx: false, void: false, sp: false, schema: Fixed [] }
  RtNewAdt -> { sym: "pv_new_adt", ctx: true, void: false, sp: true, schema: Fixed [ RI32, RPtr, RI64 ] }
  RtNewRecord -> { sym: "pv_new_record", ctx: true, void: false, sp: true, schema: Fixed [ RPtr, RPtr, RI64 ] }
  -- read-only projection (the by-need force that may precede it is a separate operation).
  RtRecordGet -> { sym: "pv_record_get", ctx: true, void: false, sp: false, schema: Fixed [ RVal, RI64 ] }
  -- functional update: allocates the new record.
  RtRecordSet -> { sym: "pv_record_set", ctx: true, void: false, sp: true, schema: Fixed [ RVal, RI64, RVal ] }
  RtReadField -> { sym: "pv_read_field", ctx: true, void: false, sp: false, schema: Fixed [ RVal, RI64 ] }
  RtWriteField -> { sym: "pv_write_field", ctx: true, void: true, sp: false, schema: Fixed [ RVal, RI64, RVal ] }
  RtReadRaw -> { sym: "pv_read_raw", ctx: true, void: false, sp: false, schema: Fixed [ RVal, RI64 ] }
  -- aborts; control never resumes, so nothing can observe a moved value.
  RtCaseFail -> { sym: "pv_case_fail", ctx: false, void: true, sp: false, schema: Fixed [] }
  -- the slow path runs a suspended guest thunk.
  RtForceIfByneed -> { sym: "pv_force_if_byneed", ctx: true, void: false, sp: true, schema: Fixed [ RVal ] }
  RtNewByneedPlaceholder -> { sym: "pv_new_byneed_placeholder", ctx: true, void: false, sp: true, schema: Fixed [] }
  -- an in-place store into the already-allocated cell.
  RtByneedSetSuspension -> { sym: "pv_byneed_set_suspension", ctx: true, void: true, sp: false, schema: Fixed [ RVal, RVal ] }
  -- shadow-stack machinery moves no guest object (`pv_root` may grow the HOST roots buffer).
  RtFrame -> { sym: "pv_frame", ctx: true, void: false, sp: false, schema: Fixed [] }
  RtRoot -> { sym: "pv_root", ctx: true, void: false, sp: false, schema: Fixed [ RVal ] }
  RtGet -> { sym: "pv_get", ctx: true, void: false, sp: false, schema: Fixed [ RI64 ] }
  RtPopFrame -> { sym: "pv_pop_frame", ctx: true, void: true, sp: false, schema: Fixed [ RI64 ] }
  -- entry-stub lifecycle: host-side work except `pv_run_effect`, which runs the guest program.
  RtAbiCheck -> { sym: "pv_abi_check", ctx: false, void: true, sp: false, schema: Fixed [ RI32 ] }
  RtInitAll -> { sym: "pv_init_all", ctx: true, void: true, sp: true, schema: Fixed [] }
  RtRunEffect -> { sym: "pv_run_effect", ctx: true, void: false, sp: true, schema: Fixed [ RVal ] }
  RtDrainOutput -> { sym: "pv_drain_output", ctx: true, void: true, sp: false, schema: Fixed [] }
  RtPrintInt -> { sym: "pv_print_int", ctx: false, void: true, sp: false, schema: Fixed [ RVal ] }
  RtRuntimeFree -> { sym: "pv_runtime_free", ctx: true, void: true, sp: false, schema: Fixed [] }
  RtApplyProfileRegister -> { sym: "pv_applyprofile_register", ctx: true, void: true, sp: false, schema: Fixed [ RPtr, RI64, RI64 ] }
  RtApplyProfileBump -> { sym: "pv_applyprofile_bump", ctx: true, void: true, sp: false, schema: Fixed [ RI64 ] }
  -- ADR-0108 §4: like the bump, `sp = false` — the drill must not move the activation plan, or the
  -- instrumented build would stop being a measurement of the shipped one.
  RtApplyProfileKey -> { sym: "pv_applyprofile_key", ctx: true, void: true, sp: false, schema: Fixed [ RPtr, RI64 ] }
  RtPrim op ->
    let
      Tuple sym ctx = primSym op
    in
      { sym, ctx, void: false, sp: primSafepoint op, schema: Vals (primArity op) }

-- | The row's safepoint class — what `Liveness`'s transfer functions consult.
rtSafepoint :: RtOp -> Boolean
rtSafepoint = _.sp <<< rtDesc

-- | The row's runtime symbol (each must have a matching `declare` in `Abi.declarations`; the
-- | seam unit tests hold that).
rtSym :: RtOp -> String
rtSym = _.sym <<< rtDesc

-- | A direct/`musttail` guest call always runs guest code — the classification the `CApp`/
-- | `CPerform` transfer arms consume for [`guestDirect`] and the [`prepareMusttail`]/
-- | [`emitPreparedMusttail`] pair (and the generic `RtApply` path).
guestCallSafepoint :: Boolean
guestCallSafepoint = true

-- | Verify one operand against its declared role and render it (ADR-0105 §6.2): a guest value
-- | (`V`) goes through the token check (`useVal`); a raw-metadata kind must match its declared
-- | position exactly — a kind/role mismatch is a structural error, not a rendering choice.
checkedArg :: String -> Role -> RtArg -> Codegen String
checkedArg sym role arg = case role, arg of
  RVal, V val -> unsafeUseVal val <#> \s -> "i64 " <> s
  RI64, I64 v -> pure ("i64 " <> v)
  RI32, I32 v -> pure ("i32 " <> v)
  RPtr, Ptr v -> pure ("ptr " <> v)
  _, _ -> unsafeCrashWith
    ("Safepoint.checkedArg: operand kind does not match the row's declared role (ADR-0105 §6.2): " <> sym)

-- | Verify the whole operand list against the row's schema (arity + per-position role) and
-- | render the call's argument text.
checkedArgs :: RtDesc -> Array RtArg -> Codegen String
checkedArgs desc args = do
  rendered <- case desc.schema of
    Fixed roles ->
      if Array.length roles /= Array.length args then
        unsafeCrashWith ("Safepoint.checkedArgs: operand count does not match the row's schema: " <> desc.sym)
      else forA (Array.zip roles args) (\(Tuple role arg) -> checkedArg desc.sym role arg)
    Vals n ->
      if n /= Array.length args then
        unsafeCrashWith ("Safepoint.checkedArgs: operand count does not match the prim arity: " <> desc.sym)
      else forA args (checkedArg desc.sym RVal)
  pure (joinWith ", " ((if desc.ctx then [ "ptr %ctx" ] else []) <> rendered))

-- | Emit a value-returning runtime call into a caller-supplied result temp — PRIVATE since
-- | 2b-2 phase 2: the boot-numbering sites that reserved the result temp before the operand
-- | emission (`SForceCell`, the dtree `extract` reads) are retired with the read-site reload
-- | choreography, so [`rtCall`] is the only public form. Verify-then-bump (ADR-0105 §6.4):
-- | operands verify against the PRE-call epoch (a rooted operand reload-on-miss emits here,
-- | before the call line); an `sp = true` row then bumps exactly once and the result token is
-- | minted post-bump.
rtCallWith :: String -> RtOp -> Array RtArg -> Codegen Val
rtCallWith t op args =
  let
    desc = rtDesc op
  in
    if desc.void then unsafeCrashWith ("Safepoint.rtCallWith: void operation " <> desc.sym)
    else do
      argText <- checkedArgs desc args
      unsafeEmitRawCall ("  " <> t <> " = call i64 @" <> desc.sym <> "(" <> argText <> ")")
      when desc.sp bumpEpoch
      unsafeMintFresh t

-- | Emit a value-returning runtime call, returning its fresh result token.
rtCall :: RtOp -> Array RtArg -> Codegen Val
rtCall op args = do
  t <- fresh
  rtCallWith t op args

-- | Emit a shadow-stack MACHINERY call whose result is a raw HOST index, not a guest value —
-- | `pv_frame` (a mark) and `pv_root` (a bare-index handle). Returning the raw text here is
-- | what lets `Val` stay fully opaque (2b-0 round 2): `Root` no longer needs a token-unwrap
-- | escape for its handles. Any other row is a structural error.
machineryHandleCall :: RtOp -> Array RtArg -> Codegen String
machineryHandleCall op args =
  let
    desc = rtDesc op
    allowed = case op of
      RtFrame -> true
      RtRoot -> true
      _ -> false
  in
    if not allowed then
      unsafeCrashWith ("Safepoint.machineryHandleCall: not a handle-returning machinery row: " <> desc.sym)
    else do
      argText <- checkedArgs desc args
      t <- fresh
      unsafeEmitRawCall ("  " <> t <> " = call i64 @" <> desc.sym <> "(" <> argText <> ")")
      pure t

-- | Emit a void runtime call (verify-then-bump, as [`rtCallWith`]).
rtCallVoid :: RtOp -> Array RtArg -> Codegen Unit
rtCallVoid op args =
  let
    desc = rtDesc op
  in
    if not desc.void then unsafeCrashWith ("Safepoint.rtCallVoid: value operation " <> desc.sym)
    else do
      argText <- checkedArgs desc args
      unsafeEmitRawCall ("  call void @" <> desc.sym <> "(" <> argText <> ")")
      when desc.sp bumpEpoch

-- | Emit a direct `tailcc` guest call (`@<name>$d`), returning its result token. Always a
-- | safepoint ([`guestCallSafepoint`]): operands verify, the epoch bumps once, the result is
-- | minted post-bump.
guestDirect :: { dsym :: String, env :: Val, args :: Array Val } -> Codegen Val
guestDirect c = do
  envS <- unsafeUseVal c.env
  argSs <- forA c.args unsafeUseVal
  r <- fresh
  unsafeEmitRawCall ("  " <> r <> " = call tailcc i64 @" <> c.dsym <> "(ptr %ctx, i64 " <> envS <> foldMap (\o -> ", i64 " <> o) argSs <> ")")
  bumpEpoch
  unsafeMintFresh r

-- | A `musttail` guest call whose operands were resolved and VERIFIED before the caller's
-- | frame pop (ADR-0105 §6.4, the 2b-2 blocker). The HANDOVER happens at the call, not at
-- | prepare — so the sealed text also carries its PREPARE-TIME EPOCH, and consumption
-- | fail-closed asserts the epoch is unchanged: nothing that can safepoint may sit between
-- | prepare and emit (the frame pop is a non-safepoint machinery row and passes). Opaque:
-- | the sealed text cannot be extracted or altered.
newtype PreparedCall = PreparedCall { dsym :: String, argText :: String, epoch :: Int }

-- | Resolve/verify a `musttail` call's operands BEFORE the frame pop and seal the rendered
-- | argument text with the prepare-time epoch. (The result temp is NOT reserved here — it
-- | must number after the pop's own temps.)
prepareMusttail :: { dsym :: String, env :: Val, args :: Array Val } -> Codegen PreparedCall
prepareMusttail c = do
  envS <- unsafeUseVal c.env
  argSs <- forA c.args unsafeUseVal
  e <- currentEpoch
  pure (PreparedCall { dsym: c.dsym, argText: "(ptr %ctx, i64 " <> envS <> foldMap (\o -> ", i64 " <> o) argSs <> ")", epoch: e })

-- | Emit a prepared `musttail` call AFTER the pop (the caller emits the mandatory `ret` of
-- | the result — ADR-0064 §4). Fail-closed: a safepoint between prepare and here would have
-- | staled the sealed operands before their handover, so an epoch drift crashes.
emitPreparedMusttail :: PreparedCall -> Codegen Val
emitPreparedMusttail (PreparedCall c) = do
  e <- currentEpoch
  if e /= c.epoch then
    unsafeCrashWith "Safepoint.emitPreparedMusttail: a safepoint intervened between prepare and emission (ADR-0105 §6.4 — the sealed operands are stale before their handover)"
  else do
    r <- fresh
    unsafeEmitRawCall ("  " <> r <> " = musttail call tailcc i64 @" <> c.dsym <> c.argText)
    bumpEpoch
    unsafeMintFresh r

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
