-- | The runtime C-ABI surface the emitted module declares, and the ctx-header inline fast paths
-- | (ADR-0079 §1/§2): `pv_get`, settle, and by-need force. A faithful transcription of boot's
-- | `codegen_llvm.ml` (`declarations`, `ctx_header_version`/offsets, `abi_stamp`, `header_field`,
-- | `abi_get`/`abi_settle`, `force_value`) — the ADR-0082 port; its boot byte-identity gate is
-- | retired (ADR-0104 §4) and emission is now L2-owned. The frame/rooting fast paths
-- | (`abi_frame_open`/`abi_pop_frame`/`abi_root`) live in `Backend.LLVM.Root` behind the
-- | ADR-0105 capability API; runtime entry calls route through the classified seam
-- | (`Backend.LLVM.Safepoint`).
-- |
-- | In release mode (`inlineAbi = true`) these emit the fast paths as inline IR against the
-- | `pv_ctx_header`; under `--debug` (`inlineAbi = false`) every operation is a single entry call (the
-- | pre-0079 IR). The SSA-temp emission order mirrors boot exactly — where boot writes
-- | `let x = fresh in emit "…(header_field …)" x (header_field …)` (OCaml right-to-left argument
-- | evaluation numbers `x` first but emits `header_field`'s line first), this sequences
-- | `x <- fresh; a <- headerField …; emit …` to reproduce both the numbering and the line order.
module Purvasm.Compiler.Backend.LLVM.Abi
  ( ctxHeaderVersion
  , offRootsLen
  , offRootsCap
  , offPendingTail
  , defaultHeapWords
  , declarations
  , abiStamp
  , headerField
  , abiGet
  , abiSettle
  , forceValue
  ) where

import Prelude

import Control.Monad.State.Class (gets)
import Data.String (joinWith)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, emit, armIncomingAt, armIncomingClosing, emitLowBitAnd, emitPhi, fresh, freshLabel, mintLoad)
import Purvasm.Compiler.Backend.LLVM.Safepoint (RtArg(..), RtOp(..), rtCall)
import Purvasm.Compiler.Backend.LLVM.Value (Val)

-- | The ctx-header ABI version stamped into each inline object (ADR-0079 §1).
ctxHeaderVersion :: Int
ctxHeaderVersion = 1

-- | `roots_base` sits at offset 0 (its load reads `ptr %ctx` directly); these are the other header
-- | field offsets in bytes.
offRootsLen :: Int
offRootsLen = 8

offRootsCap :: Int
offRootsCap = 16

offPendingTail :: Int
offPendingTail = 24

-- | Default heap size in words per semi-space for the entry runtime — `1 << 20` (ADR-0066 §4).
defaultHeapWords :: Int
defaultHeapWords = 1048576

-- | The fixed block of `declare` lines for every `pv_*` runtime symbol (order is load-bearing for
-- | byte-identity). Joined with `"\n"` and no trailing newline, exactly as boot's `String.concat "\n"`.
declarations :: String
declarations = joinWith "\n"
  [ "declare ptr @pv_runtime_new(i64)"
  , "declare void @pv_abi_check(i32)"
  , "declare void @pv_runtime_free(ptr)"
  , "declare i64 @pv_apply(ptr, i64, ptr, i64)"
  , "declare void @pv_tailcall(ptr, i64, ptr, i64)"
  , "declare i64 @pv_settle(ptr, i64)"
  , "declare i64 @pv_make_closure(ptr, i64, i32, i64)"
  , "declare i64 @pv_frame(ptr)"
  , "declare i64 @pv_root(ptr, i64)"
  , "declare i64 @pv_get(ptr, i64)"
  , "declare void @pv_pop_frame(ptr, i64)"
  , "declare i64 @pv_new_array(ptr, ptr, i64)"
  , "declare i64 @pv_new_adt(ptr, i32, ptr, i64)"
  , "declare i64 @pv_new_record(ptr, ptr, ptr, i64)"
  , "declare i64 @pv_new_str(ptr, ptr, i64)"
  , "declare i64 @pv_new_number(ptr, i64)"
  , "declare i64 @pv_record_get(ptr, i64, i64)"
  , "declare i64 @pv_record_set(ptr, i64, i64, i64)"
  , "declare i64 @pv_read_field(ptr, i64, i64)"
  , "declare void @pv_write_field(ptr, i64, i64, i64)"
  , "declare i64 @pv_read_raw(ptr, i64, i64)"
  , "declare void @pv_case_fail()"
  , "declare i64 @pv_run_effect(ptr, i64)"
  , "declare void @pv_drain_output(ptr)"
  , "declare void @pv_print_int(i64)"
  , "declare i64 @pv_prim_add_int(i64, i64)"
  , "declare i64 @pv_prim_sub_int(i64, i64)"
  , "declare i64 @pv_prim_mul_int(i64, i64)"
  , "declare i64 @pv_prim_div_int(i64, i64)"
  , "declare i64 @pv_prim_mod_int(i64, i64)"
  , "declare i64 @pv_prim_and_int(i64, i64)"
  , "declare i64 @pv_prim_or_int(i64, i64)"
  , "declare i64 @pv_prim_xor_int(i64, i64)"
  , "declare i64 @pv_prim_shl_int(i64, i64)"
  , "declare i64 @pv_prim_shr_int(i64, i64)"
  , "declare i64 @pv_prim_zshr_int(i64, i64)"
  , "declare i64 @pv_prim_complement_int(i64)"
  , "declare i64 @pv_prim_eq_int(i64, i64)"
  , "declare i64 @pv_prim_lt_int(i64, i64)"
  , "declare i64 @pv_prim_eq_bool(i64, i64)"
  , "declare i64 @pv_prim_and_bool(i64, i64)"
  , "declare i64 @pv_prim_or_bool(i64, i64)"
  , "declare i64 @pv_prim_not_bool(i64)"
  , "declare i64 @pv_prim_add_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_sub_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_mul_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_div_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_int_to_number(ptr, i64)"
  , "declare i64 @pv_prim_number_to_int(ptr, i64)"
  , "declare i64 @pv_prim_eq_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_lt_number(ptr, i64, i64)"
  , "declare i64 @pv_prim_eq_string(ptr, i64, i64)"
  , "declare i64 @pv_prim_lt_string(ptr, i64, i64)"
  , "declare i64 @pv_prim_append(ptr, i64, i64)"
  , "declare i64 @pv_prim_index_array(ptr, i64, i64)"
  , "declare i64 @pv_prim_length_array(ptr, i64)"
  , "declare i64 @pv_prim_new_array(ptr, i64)"
  , "declare i64 @pv_prim_set_array(ptr, i64, i64, i64)"
  , "declare i64 @pv_prim_record_get(ptr, i64, i64)"
  , "declare i64 @pv_prim_record_set(ptr, i64, i64, i64)"
  , "declare i64 @pv_prim_record_has(ptr, i64, i64)"
  , "declare i64 @pv_prim_record_delete(ptr, i64, i64)"
  , "declare i64 @pv_prim_record_union(ptr, i64, i64)"
  , "declare i64 @pv_empty_array()"
  , "declare i64 @pv_new_byneed_placeholder(ptr)"
  , "declare void @pv_byneed_set_suspension(ptr, i64, i64)"
  , "declare i64 @pv_force_if_byneed(ptr, i64)"
  ]

-- | The per-object link-time ABI stamp (ADR-0079 §1): an inline object carries a kept-alive reference
-- | to `pv_ctx_abi_v<N>` so a version/profile mismatch fails at link. Entry-call (`--debug`) objects
-- | carry nothing. Returns a trailing-newline-terminated block (or `""` under `--debug`), assembled
-- | into the module template.
abiStamp :: Boolean -> String
abiStamp inlineAbi
  | not inlineAbi = ""
  | otherwise =
      "@pv_ctx_abi_v" <> v <> " = external global i8\n"
        <> "@pv_abi_stamp = internal constant ptr @pv_ctx_abi_v"
        <> v
        <> "\n"
        <> "@llvm.used = appending global [1 x ptr] [ptr @pv_abi_stamp], section \"llvm.metadata\"\n"
      where
      v = show ctxHeaderVersion

-- | A header field's address, computed at the use site (the base moves on growth, so nothing is
-- | cached across calls).
headerField :: Int -> Codegen String
headerField off = do
  a <- fresh
  emit ("  " <> a <> " = getelementptr i8, ptr %ctx, i64 " <> show off)
  pure a

-- | Read a root handle's current value: inline loads `roots_base` then the slot, `--debug` calls
-- | `pv_get`. The handle is raw metadata (a stable index); the LOADED value is a fresh token —
-- | the reload event of ADR-0105 §6.2's `Rooted` arm.
abiGet :: String -> Codegen Val
abiGet handle = gets _.inlineAbi >>= case _ of
  false -> rtCall RtGet [ I64 handle ]
  true -> do
    base <- fresh
    emit ("  " <> base <> " = load ptr, ptr %ctx")
    slot <- fresh
    emit ("  " <> slot <> " = getelementptr i64, ptr " <> base <> ", i64 " <> handle)
    mintLoad slot

-- | Settle a returned value after a call (ADR-0079): if a tail is pending, `pv_settle` reifies it;
-- | otherwise the value passes through. Inline is a 3-block `schk`/`sslow`/`sdone` with a phi;
-- | `--debug` is a single `pv_settle`.
abiSettle :: Val -> Codegen Val
abiSettle r = gets _.inlineAbi >>= case _ of
  false -> rtCall RtSettle [ V r ]
  true -> do
    chk <- freshLabel "schk"
    slow <- freshLabel "sslow"
    done <- freshLabel "sdone"
    emit ("  br label %" <> chk)
    emit (chk <> ":")
    -- boot binds `pf` (the load result) *before* evaluating `header_field`, so the load-result temp is
    -- numbered before the getelementptr temp while the getelementptr line is emitted first (OCaml
    -- right-to-left arg eval) — matching the `%t9 = getelementptr; %t8 = load` order in boot's `.ll`.
    pf <- fresh
    addr <- headerField offPendingTail
    emit ("  " <> pf <> " = load i64, ptr " <> addr)
    has <- fresh
    emit ("  " <> has <> " = icmp ne i64 " <> pf <> ", 0")
    emit ("  br i1 " <> has <> ", label %" <> slow <> ", label %" <> done)
    -- each incoming's freeze is fused with the arm boundary it proves (§6.2 round 3): the
    -- fast arm seals as the slow block opens; the slow arm seals with its closing branch.
    rIn <- armIncomingAt { from: chk, startNext: slow } r
    rs <- rtCall RtSettle [ V r ]
    rsIn <- armIncomingClosing { from: slow, merge: done } rs
    t <- fresh
    emitPhi t [ rIn, rsIn ]

-- | Force a value if it is a by-need cell (ADR-0079): an immediate (low bit set) passes through; only a
-- | pointer word calls `pv_force_if_byneed`. Always a 3-block `fchk`/`fslow`/`fdone` with a phi (the
-- | slow path is the only safepoint), regardless of `inlineAbi`.
forceValue :: Val -> Codegen Val
forceValue v = do
  chk <- freshLabel "fchk"
  slow <- freshLabel "fslow"
  done <- freshLabel "fdone"
  emit ("  br label %" <> chk)
  emit (chk <> ":")
  bit <- fresh
  emitLowBitAnd bit v
  imm <- fresh
  emit ("  " <> imm <> " = icmp ne i64 " <> bit <> ", 0")
  emit ("  br i1 " <> imm <> ", label %" <> done <> ", label %" <> slow)
  -- each incoming's freeze is fused with the arm boundary it proves (§6.2 round 3).
  vIn <- armIncomingAt { from: chk, startNext: slow } v
  forced <- rtCall RtForceIfByneed [ V v ]
  forcedIn <- armIncomingClosing { from: slow, merge: done } forced
  r <- fresh
  emitPhi r [ vIn, forcedIn ]
