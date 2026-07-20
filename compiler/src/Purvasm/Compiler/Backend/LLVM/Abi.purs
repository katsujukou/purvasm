-- | The runtime C-ABI surface the emitted module declares, and the ctx-header inline fast paths
-- | (ADR-0079 §1/§2): rooting, frame open/pop, `pv_get`, settle, and by-need force. A faithful
-- | transcription of boot's `codegen_llvm.ml` (`declarations`, `ctx_header_version`/offsets,
-- | `abi_stamp`, `header_field`, `abi_frame_open`/`abi_pop_frame`/`abi_get`/`abi_root`/`abi_settle`,
-- | `force_value`) — the ADR-0082 port; its boot byte-identity gate is retired (ADR-0104 §4) and
-- | emission is now L2-owned.
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
  , abiFrameOpen
  , abiPopFrame
  , abiGet
  , abiRoot
  , abiSettle
  , forceValue
  ) where

import Prelude

import Control.Monad.State.Class (gets)
import Data.String (joinWith)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, emit, fresh, freshLabel)

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

-- | Open a shadow-stack frame, returning the mark operand: inline reads `roots_len`, `--debug` calls
-- | `pv_frame`.
abiFrameOpen :: Codegen String
abiFrameOpen = gets _.inlineAbi >>= case _ of
  false -> do
    m <- fresh
    emit ("  " <> m <> " = call i64 @pv_frame(ptr %ctx)")
    pure m
  true -> do
    m <- fresh
    lenp <- headerField offRootsLen
    emit ("  " <> m <> " = load i64, ptr " <> lenp)
    pure m

-- | Pop the frame back to `mark`: inline stores `roots_len`, `--debug` calls `pv_pop_frame`.
abiPopFrame :: String -> Codegen Unit
abiPopFrame mark = gets _.inlineAbi >>= case _ of
  false -> emit ("  call void @pv_pop_frame(ptr %ctx, i64 " <> mark <> ")")
  true -> do
    lenp <- headerField offRootsLen
    emit ("  store i64 " <> mark <> ", ptr " <> lenp)

-- | Read a root handle's current value: inline loads `roots_base` then the slot, `--debug` calls
-- | `pv_get`.
abiGet :: String -> Codegen String
abiGet handle = gets _.inlineAbi >>= case _ of
  false -> do
    t <- fresh
    emit ("  " <> t <> " = call i64 @pv_get(ptr %ctx, i64 " <> handle <> ")")
    pure t
  true -> do
    base <- fresh
    emit ("  " <> base <> " = load ptr, ptr %ctx")
    slot <- fresh
    emit ("  " <> slot <> " = getelementptr i64, ptr " <> base <> ", i64 " <> handle)
    t <- fresh
    emit ("  " <> t <> " = load i64, ptr " <> slot)
    pure t

-- | Root a freshly produced value and return its handle. Inline: the in-capacity store is the fast
-- | path (a 4-block `rchk`/`rfast`/`rslow`/`rdone` with a phi); `len == cap` falls to `pv_root`, which
-- | grows and returns the same bare-index handle. `--debug` is a single `pv_root`.
abiRoot :: String -> Codegen String
abiRoot v = gets _.inlineAbi >>= case _ of
  false -> do
    h <- fresh
    emit ("  " <> h <> " = call i64 @pv_root(ptr %ctx, i64 " <> v <> ")")
    pure h
  true -> do
    chk <- freshLabel "rchk"
    fast <- freshLabel "rfast"
    slow <- freshLabel "rslow"
    done <- freshLabel "rdone"
    emit ("  br label %" <> chk)
    emit (chk <> ":")
    lenp <- headerField offRootsLen
    len <- fresh
    emit ("  " <> len <> " = load i64, ptr " <> lenp)
    cap <- fresh
    capAddr <- headerField offRootsCap
    emit ("  " <> cap <> " = load i64, ptr " <> capAddr)
    full <- fresh
    emit ("  " <> full <> " = icmp eq i64 " <> len <> ", " <> cap)
    emit ("  br i1 " <> full <> ", label %" <> slow <> ", label %" <> fast)
    emit (fast <> ":")
    base <- fresh
    emit ("  " <> base <> " = load ptr, ptr %ctx")
    slot <- fresh
    emit ("  " <> slot <> " = getelementptr i64, ptr " <> base <> ", i64 " <> len)
    emit ("  store i64 " <> v <> ", ptr " <> slot)
    len1 <- fresh
    emit ("  " <> len1 <> " = add i64 " <> len <> ", 1")
    emit ("  store i64 " <> len1 <> ", ptr " <> lenp)
    emit ("  br label %" <> done)
    emit (slow <> ":")
    hs <- fresh
    emit ("  " <> hs <> " = call i64 @pv_root(ptr %ctx, i64 " <> v <> ")")
    emit ("  br label %" <> done)
    emit (done <> ":")
    h <- fresh
    emit ("  " <> h <> " = phi i64 [ " <> len <> ", %" <> fast <> " ], [ " <> hs <> ", %" <> slow <> " ]")
    pure h

-- | Settle a returned value after a call (ADR-0079): if a tail is pending, `pv_settle` reifies it;
-- | otherwise the value passes through. Inline is a 3-block `schk`/`sslow`/`sdone` with a phi;
-- | `--debug` is a single `pv_settle`.
abiSettle :: String -> Codegen String
abiSettle r = gets _.inlineAbi >>= case _ of
  false -> do
    t <- fresh
    emit ("  " <> t <> " = call i64 @pv_settle(ptr %ctx, i64 " <> r <> ")")
    pure t
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
    emit (slow <> ":")
    rs <- fresh
    emit ("  " <> rs <> " = call i64 @pv_settle(ptr %ctx, i64 " <> r <> ")")
    emit ("  br label %" <> done)
    emit (done <> ":")
    t <- fresh
    emit ("  " <> t <> " = phi i64 [ " <> r <> ", %" <> chk <> " ], [ " <> rs <> ", %" <> slow <> " ]")
    pure t

-- | Force a value if it is a by-need cell (ADR-0079): an immediate (low bit set) passes through; only a
-- | pointer word calls `pv_force_if_byneed`. Always a 3-block `fchk`/`fslow`/`fdone` with a phi (the
-- | slow path is the only safepoint), regardless of `inlineAbi`.
forceValue :: String -> Codegen String
forceValue v = do
  chk <- freshLabel "fchk"
  slow <- freshLabel "fslow"
  done <- freshLabel "fdone"
  emit ("  br label %" <> chk)
  emit (chk <> ":")
  bit <- fresh
  emit ("  " <> bit <> " = and i64 " <> v <> ", 1")
  imm <- fresh
  emit ("  " <> imm <> " = icmp ne i64 " <> bit <> ", 0")
  emit ("  br i1 " <> imm <> ", label %" <> done <> ", label %" <> slow)
  emit (slow <> ":")
  forced <- fresh
  emit ("  " <> forced <> " = call i64 @pv_force_if_byneed(ptr %ctx, i64 " <> v <> ")")
  emit ("  br label %" <> done)
  emit (done <> ":")
  r <- fresh
  emit ("  " <> r <> " = phi i64 [ " <> v <> ", %" <> chk <> " ], [ " <> forced <> ", %" <> slow <> " ]")
  pure r
