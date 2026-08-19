-- | The primitive operations, over VM values.
-- |
-- | These are the semantics the whole toolchain shares (ADR-0007 monomorphic primitives), and this
-- | module's obligation is to agree with boot's `Vm.Machine.eval_prim` on every input — it is the
-- | interpreter the owned VM is differentially gated against.
-- |
-- | It meets that obligation by **not reimplementing the arithmetic**. The VM is itself a purvasm
-- | program, so the guest's `Int` is the host's `Int`, and every operation whose semantics are subtle
-- | is delegated to `Purvasm.Int` (ADR-0038): Euclidean `div`/`mod` with a total zero divisor, shifts
-- | whose count is taken mod 32, `zshr` re-wrapped to signed 32, and `fromNumber` as the ECMAScript
-- | `ToInt32` coercion (ADR-0041) — total on `NaN`/`Infinity`. `Int`'s range is preserved by every
-- | one of them, including `div bottom (-1)` (ADR-0112 §1).
-- |
-- | A hand-written version of any of these is a second source of truth that can only drift: an
-- | earlier draft of this module wrote three of them and got `div` wrong at `Int`'s minimum, where
-- | computing `a - mod a b` first overflows.
-- |
-- | Arguments arrive already forced: a primop inspects a value's shape, and forcing is the machine's
-- | job at every such site.
module Purvasm.VM.Prim
  ( eval
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Purvasm.Int as Int
import Purvasm.VM.Array as VMArray
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Foreign as Foreign
import Purvasm.VM.Instruction (PrimOp(..))
import Purvasm.VM.Value (Value(..))

-- | Apply a primop to its (already forced) arguments.
-- |
-- | A value that came from a leaf arrives as an opaque carrier, and **this is one of the sites that
-- | decodes it** (ADR-0111 §3): a primop knows exactly what it wants, so it demands that rather than
-- | asking what it has. [demands] states the shape per operand and [decode] applies it; every arm
-- | below then sees ordinary VM values and is unchanged by the FFI existing.
eval :: PrimOp -> Array Value -> Effect Value
eval op args = evalDecoded op =<< traverseWithIndex (decode op) args

-- | What operand `i` of `op` must be for the arm to fire. `DAny` is not laxness — it is the operands
-- | that are *stored* rather than inspected (`SetArray`'s element, `RecordSet`'s value), where a
-- | carrier is a perfectly good value to keep carrying.
data Demand
  = DInt
  | DNumber
  | DBool
  | DString
  | DArray
  | DAny

demands :: PrimOp -> Array Demand
demands = case _ of
  AddInt -> [ DInt, DInt ]
  SubInt -> [ DInt, DInt ]
  MulInt -> [ DInt, DInt ]
  DivInt -> [ DInt, DInt ]
  ModInt -> [ DInt, DInt ]
  AndInt -> [ DInt, DInt ]
  OrInt -> [ DInt, DInt ]
  XorInt -> [ DInt, DInt ]
  ShlInt -> [ DInt, DInt ]
  ShrInt -> [ DInt, DInt ]
  ZshrInt -> [ DInt, DInt ]
  ComplementInt -> [ DInt ]
  AddNumber -> [ DNumber, DNumber ]
  SubNumber -> [ DNumber, DNumber ]
  MulNumber -> [ DNumber, DNumber ]
  DivNumber -> [ DNumber, DNumber ]
  IntToNumber -> [ DInt ]
  NumberToInt -> [ DNumber ]
  EqInt -> [ DInt, DInt ]
  EqString -> [ DString, DString ]
  EqNumber -> [ DNumber, DNumber ]
  EqBool -> [ DBool, DBool ]
  LtInt -> [ DInt, DInt ]
  LtString -> [ DString, DString ]
  LtNumber -> [ DNumber, DNumber ]
  AndBool -> [ DBool, DBool ]
  OrBool -> [ DBool, DBool ]
  NotBool -> [ DBool ]
  Append -> [ DString, DString ]
  IndexArray -> [ DArray, DInt ]
  LengthArray -> [ DArray ]
  NewArray -> [ DInt ]
  -- The written element is `DAny`: it is stored, not inspected, and `Purvasm.VM.Array.write` crosses
  -- it at the boundary if the array turns out to be promoted.
  SetArray -> [ DArray, DInt, DAny ]
  RecordGet -> [ DString, DAny ]
  RecordSet -> [ DString, DAny, DAny ]
  RecordHas -> [ DString, DAny ]
  RecordDelete -> [ DString, DAny ]
  RecordUnion -> [ DAny, DAny ]

-- | Decode operand `i` to the demanded shape. Only a carrier is ever touched: a VM value is already
-- | the shape it is, and an ill-typed one falls through to the arms below, which refuse it by name.
-- |
-- | An array is not decoded into anything — `Purvasm.VM.Array.asCell` gives the carrier a cell that
-- | forwards to it, so the array operations reach the leaf's own object rather than a copy.
decode :: PrimOp -> Int -> Value -> Effect Value
decode op i value = case value, Array.index (demands op) i of
  VCarrier _ fv, Just DInt -> pure (VInt (Foreign.intOf fv))
  VCarrier _ fv, Just DNumber -> pure (VNumber (Foreign.numberOf fv))
  VCarrier _ fv, Just DBool -> pure (VBool (Foreign.booleanOf fv))
  VCarrier _ fv, Just DString -> pure (VString (Foreign.stringOf fv))
  VCarrier _ _, Just DArray -> VMArray.asCell value >>= case _ of
    Just cell -> pure (VArray cell)
    Nothing -> pure value
  _, _ -> pure value

evalDecoded :: PrimOp -> Array Value -> Effect Value
evalDecoded op args = case op, args of
  AddInt, [ VInt a, VInt b ] -> pure (VInt (a + b))
  SubInt, [ VInt a, VInt b ] -> pure (VInt (a - b))
  MulInt, [ VInt a, VInt b ] -> pure (VInt (a * b))
  DivInt, [ VInt a, VInt b ] -> pure (VInt (Int.div a b))
  ModInt, [ VInt a, VInt b ] -> pure (VInt (Int.mod a b))
  AndInt, [ VInt a, VInt b ] -> pure (VInt (Int.and a b))
  OrInt, [ VInt a, VInt b ] -> pure (VInt (Int.or a b))
  XorInt, [ VInt a, VInt b ] -> pure (VInt (Int.xor a b))
  ShlInt, [ VInt a, VInt b ] -> pure (VInt (Int.shl a b))
  ShrInt, [ VInt a, VInt b ] -> pure (VInt (Int.shr a b))
  ZshrInt, [ VInt a, VInt b ] -> pure (VInt (Int.zshr a b))
  ComplementInt, [ VInt a ] -> pure (VInt (Int.complement a))
  AddNumber, [ VNumber a, VNumber b ] -> pure (VNumber (a + b))
  SubNumber, [ VNumber a, VNumber b ] -> pure (VNumber (a - b))
  MulNumber, [ VNumber a, VNumber b ] -> pure (VNumber (a * b))
  DivNumber, [ VNumber a, VNumber b ] -> pure (VNumber (a / b))
  IntToNumber, [ VInt a ] -> pure (VNumber (Int.toNumber a))
  NumberToInt, [ VNumber f ] -> pure (VInt (Int.fromNumber f))
  EqInt, [ VInt a, VInt b ] -> pure (VBool (a == b))
  EqString, [ VString a, VString b ] -> pure (VBool (a == b))
  EqNumber, [ VNumber a, VNumber b ] -> pure (VBool (a == b))
  EqBool, [ VBool a, VBool b ] -> pure (VBool (a == b))
  LtInt, [ VInt a, VInt b ] -> pure (VBool (a < b))
  LtString, [ VString a, VString b ] -> pure (VBool (a < b))
  LtNumber, [ VNumber a, VNumber b ] -> pure (VBool (a < b))
  AndBool, [ VBool a, VBool b ] -> pure (VBool (a && b))
  OrBool, [ VBool a, VBool b ] -> pure (VBool (a || b))
  NotBool, [ VBool a ] -> pure (VBool (not a))
  Append, [ VString a, VString b ] -> pure (VString (a <> b))
  IndexArray, [ VArray cell, VInt i ] -> VMArray.index cell i >>= case _ of
    Just v -> pure v
    Nothing -> stuck ("array index out of bounds: " <> show i)
  LengthArray, [ VArray cell ] -> VInt <$> VMArray.length cell
  NewArray, [ VInt n ] -> VArray <$> VMArray.new n
  -- `SetArray` returns the array it wrote, so a builder loop threads it (ADR-0019).
  SetArray, [ VArray cell, VInt i, v ] -> VMArray.write cell i v >>= case _ of
    true -> pure (VArray cell)
    false -> stuck ("array set out of bounds: " <> show i)
  RecordGet, [ VString label, VRecord m ] -> case Map.lookup label m of
    Just v -> pure v
    Nothing -> stuck ("record field absent: " <> label)
  RecordSet, [ VString label, v, VRecord m ] -> pure (VRecord (Map.insert label v m))
  RecordHas, [ VString label, VRecord m ] -> pure (VBool (Map.member label m))
  RecordDelete, [ VString label, VRecord m ] -> pure (VRecord (Map.delete label m))
  -- Left-biased merge (ADR-0069): the first record's fields win on a shared label.
  RecordUnion, [ VRecord m1, VRecord m2 ] -> pure (VRecord (Map.union m1 m2))
  _, _ -> stuck "primop: ill-typed arguments"
