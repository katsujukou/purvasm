-- | Primitive-operation lowering for the LLVM backend (ADR-0071 §6): the runtime-helper symbol table
-- | (`primSym`) and the inline IR for the scalar `Int`/`Boolean` ops (`inlinePrim`). A faithful
-- | transcription of boot's `codegen_llvm.ml` (`prim_sym`, `inline_prim`), byte-identical to boot's
-- | `.ll` (ADR-0082 §2).
-- |
-- | Scalar `Int`/`Boolean` ops are **pure** (no ctx, no safepoint) and are emitted inline against the
-- | `(payload << 1) | 1` immediate representation; `Number`/`String`/`Array`/`Record` ops read or
-- | allocate a boxed value, so they take the ctx and go through the runtime call (`inlinePrim` returns
-- | `Nothing`, the caller falls back to `primSym`).
-- |
-- | `primSym` covers every `PrimOp` (the boxed `Record`/`Number`/`String`/`Array` ops go through their
-- | `pv_prim_*` runtime helper); `inlinePrim` handles only the scalar `Int`/`Boolean` ops inline.
module Purvasm.Compiler.Backend.LLVM.Prim
  ( primSym
  , inlinePrim
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, emit, fresh)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | A primop's runtime helper `(symbol, needs_ctx)`. `needs_ctx` is `true` for the boxed-value ops.
primSym :: PrimOp -> Tuple String Boolean
primSym = case _ of
  AddInt -> Tuple "pv_prim_add_int" false
  SubInt -> Tuple "pv_prim_sub_int" false
  MulInt -> Tuple "pv_prim_mul_int" false
  DivInt -> Tuple "pv_prim_div_int" false
  ModInt -> Tuple "pv_prim_mod_int" false
  AndInt -> Tuple "pv_prim_and_int" false
  OrInt -> Tuple "pv_prim_or_int" false
  XorInt -> Tuple "pv_prim_xor_int" false
  ShlInt -> Tuple "pv_prim_shl_int" false
  ShrInt -> Tuple "pv_prim_shr_int" false
  ZshrInt -> Tuple "pv_prim_zshr_int" false
  ComplementInt -> Tuple "pv_prim_complement_int" false
  EqInt -> Tuple "pv_prim_eq_int" false
  LtInt -> Tuple "pv_prim_lt_int" false
  EqBool -> Tuple "pv_prim_eq_bool" false
  AndBool -> Tuple "pv_prim_and_bool" false
  OrBool -> Tuple "pv_prim_or_bool" false
  NotBool -> Tuple "pv_prim_not_bool" false
  AddNumber -> Tuple "pv_prim_add_number" true
  SubNumber -> Tuple "pv_prim_sub_number" true
  MulNumber -> Tuple "pv_prim_mul_number" true
  DivNumber -> Tuple "pv_prim_div_number" true
  IntToNumber -> Tuple "pv_prim_int_to_number" true
  NumberToInt -> Tuple "pv_prim_number_to_int" true
  EqNumber -> Tuple "pv_prim_eq_number" true
  LtNumber -> Tuple "pv_prim_lt_number" true
  EqString -> Tuple "pv_prim_eq_string" true
  LtString -> Tuple "pv_prim_lt_string" true
  Append -> Tuple "pv_prim_append" true
  IndexArray -> Tuple "pv_prim_index_array" true
  LengthArray -> Tuple "pv_prim_length_array" true
  NewArray -> Tuple "pv_prim_new_array" true
  SetArray -> Tuple "pv_prim_set_array" true
  RecordGet -> Tuple "pv_prim_record_get" true
  RecordSet -> Tuple "pv_prim_record_set" true
  RecordHas -> Tuple "pv_prim_record_has" true
  RecordDelete -> Tuple "pv_prim_record_delete" true
  RecordUnion -> Tuple "pv_prim_record_union" true

-- The tagged-word payload `w >> 1` (arithmetic).
payload :: String -> Codegen String
payload w = do
  p <- fresh
  emit ("  " <> p <> " = ashr i64 " <> w <> ", 1")
  pure p

-- The payload as an i32. boot binds the result temp *before* evaluating `payload` (OCaml right-to-left
-- arg eval), so the trunc temp is numbered before the ashr temp while the ashr line is emitted first.
toI32 :: String -> Codegen String
toI32 w = do
  v <- fresh
  p <- payload w
  emit ("  " <> v <> " = trunc i64 " <> p <> " to i32")
  pure v

-- Re-tag an i32 result as the immediate `(sext << 1) | 1`.
ofI32 :: String -> Codegen String
ofI32 v = do
  s <- fresh
  emit ("  " <> s <> " = sext i32 " <> v <> " to i64")
  sh <- fresh
  emit ("  " <> sh <> " = shl i64 " <> s <> ", 1")
  t <- fresh
  emit ("  " <> t <> " = or i64 " <> sh <> ", 1")
  pure t

-- Re-tag an i1 result as the immediate `(zext << 1) | 1`.
ofI1 :: String -> Codegen String
ofI1 b = do
  z <- fresh
  emit ("  " <> z <> " = zext i1 " <> b <> " to i64")
  sh <- fresh
  emit ("  " <> sh <> " = shl i64 " <> z <> ", 1")
  t <- fresh
  emit ("  " <> t <> " = or i64 " <> sh <> ", 1")
  pure t

-- `payload w /= 0` as an i1 (the truthiness of a `Boolean`). `b` is bound before `payload` (as `toI32`).
truthy :: String -> Codegen String
truthy w = do
  b <- fresh
  p <- payload w
  emit ("  " <> b <> " = icmp ne i64 " <> p <> ", 0")
  pure b

-- A 32-bit binary arithmetic op on two tagged operands, re-tagged.
bin32 :: String -> String -> String -> Codegen String
bin32 mnem a b = do
  va <- toI32 a
  vb <- toI32 b
  r <- fresh
  emit ("  " <> r <> " = " <> mnem <> " i32 " <> va <> ", " <> vb)
  ofI32 r

-- A 32-bit shift on two tagged operands, re-tagged. The shift count is masked `& 31` (prim.rs
-- `wrapping_shl`/`shr` on a masked count), which also keeps the LLVM shift amount in range (an
-- unmasked >=32 would be poison).
shift32 :: String -> String -> String -> Codegen String
shift32 mnem a b = do
  va <- toI32 a
  vb <- toI32 b
  m <- fresh
  emit ("  " <> m <> " = and i32 " <> vb <> ", 31")
  r <- fresh
  emit ("  " <> r <> " = " <> mnem <> " i32 " <> va <> ", " <> m)
  ofI32 r

-- A 32-bit comparison, re-tagged as a `Boolean` immediate.
cmp32 :: String -> String -> String -> Codegen String
cmp32 cond a b = do
  va <- toI32 a
  vb <- toI32 b
  c <- fresh
  emit ("  " <> c <> " = icmp " <> cond <> " i32 " <> va <> ", " <> vb)
  ofI1 c

-- A binary `Boolean` op on two truthiness i1s, re-tagged.
boolBin :: String -> String -> String -> Codegen String
boolBin mnem a b = do
  ba <- truthy a
  bb <- truthy b
  r <- fresh
  emit ("  " <> r <> " = " <> mnem <> " i1 " <> ba <> ", " <> bb)
  ofI1 r

-- | Inline IR for a scalar `Int`/`Boolean` op (returns `Just` the result operand); `Nothing` for every
-- | other op (div/mod and all boxed ops), where the caller falls back to the `primSym` runtime call.
inlinePrim :: PrimOp -> Array String -> Codegen (Maybe String)
inlinePrim op ops = case op, ops of
  AddInt, [ a, b ] -> Just <$> bin32 "add" a b
  SubInt, [ a, b ] -> Just <$> bin32 "sub" a b
  MulInt, [ a, b ] -> Just <$> bin32 "mul" a b
  AndInt, [ a, b ] -> Just <$> bin32 "and" a b
  OrInt, [ a, b ] -> Just <$> bin32 "or" a b
  XorInt, [ a, b ] -> Just <$> bin32 "xor" a b
  ShlInt, [ a, b ] -> Just <$> shift32 "shl" a b
  ShrInt, [ a, b ] -> Just <$> shift32 "ashr" a b
  ZshrInt, [ a, b ] -> Just <$> shift32 "lshr" a b
  ComplementInt, [ a ] -> Just <$> do
    va <- toI32 a
    r <- fresh
    emit ("  " <> r <> " = xor i32 " <> va <> ", -1")
    ofI32 r
  EqInt, [ a, b ] -> Just <$> cmp32 "eq" a b
  LtInt, [ a, b ] -> Just <$> cmp32 "slt" a b
  EqBool, [ a, b ] -> Just <$> boolBin "icmp eq" a b
  AndBool, [ a, b ] -> Just <$> boolBin "and" a b
  OrBool, [ a, b ] -> Just <$> boolBin "or" a b
  NotBool, [ a ] -> Just <$> do
    ba <- truthy a
    r <- fresh
    emit ("  " <> r <> " = xor i1 " <> ba <> ", true")
    ofI1 r
  _, _ -> pure Nothing
