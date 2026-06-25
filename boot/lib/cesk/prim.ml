open Ast

(* Primitive operations. Argument evaluation order and arity are enforced by the
   machine (see Cont.Prim_args); this module only defines the operations on
   fully-evaluated values. *)

(* PureScript `Int` is a signed 32-bit integer with wrapping arithmetic (JS `| 0`); the
   host `int` is 63-bit, so wrap every int result back to signed 32 bits. *)
let w32 (n : int) : int = Int32.to_int (Int32.of_int n)

(* Euclidean division / remainder — `Prelude`'s `EuclideanRing Int` (4.x+): a non-negative
   remainder `0 <= r < |b|`; `0` on a zero divisor. (Truncating is `quot`/`rem`.) *)
let emod a b = if b = 0 then 0 else (let m = abs b in let r = a mod m in if r < 0 then r + m else r)
let ediv a b = if b = 0 then 0 else (a - emod a b) / b

let eval (op : primop) (args : Value.t list) : Value.t =
  match op, args with
  | AddInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a + b))
  | SubInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a - b))
  | MulInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a * b))
  (* Euclidean division / remainder (PureScript `EuclideanRing Int`, 4.x+); zero divisor
     yields 0 (guarded, not raised — ADR-0017). *)
  | DivInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (ediv a b))
  | ModInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (emod a b))
  (* Bitwise ops on the signed 32-bit `Int` (`Data.Int.Bits`). Shift counts are taken
     mod 32 (JS `<<`/`>>`/`>>>` mask the count to its low 5 bits); `zshr` is the logical
     (zero-fill) right shift, computed on the unsigned 32-bit value then re-wrapped to
     signed via [w32], so e.g. `zshr (-1) 1 = 2147483647`. *)
  | AndInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a land b))
  | OrInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a lor b))
  | XorInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a lxor b))
  | ShlInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 (a lsl (b land 31)))
  | ShrInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a asr (b land 31))
  | ZshrInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (w32 ((a land 0xFFFFFFFF) lsr (b land 31)))
  | ComplementInt, [ Value.VInt a ] -> Value.VInt (w32 (lnot a))
  | AddNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VNumber (a +. b)
  | SubNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VNumber (a -. b)
  | MulNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VNumber (a *. b)
  (* Number division is total (1.0/.0.0 = inf, 0.0/.0.0 = nan); no exception. *)
  | DivNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VNumber (a /. b)
  | EqInt, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a = b)
  | EqString, [ Value.VString a; Value.VString b ] -> Value.VBool (String.equal a b)
  (* IEEE equality: the polymorphic = gives nan <> nan and -0.0 = 0.0 on floats,
     matching PureScript/JS. Deliberately NOT Float.equal/Float.compare, which
     impose a total order (nan = nan). See ADR-0008. *)
  | EqNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VBool (a = b)
  | EqBool, [ Value.VBool a; Value.VBool b ] -> Value.VBool (Bool.equal a b)
  | LtInt, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a < b)
  (* Byte-wise comparison; for UTF-8 this is code-point (natural) order. *)
  | LtString, [ Value.VString a; Value.VString b ] -> Value.VBool (String.compare a b < 0)
  (* IEEE ordering: polymorphic < is false for any comparison involving nan. *)
  | LtNumber, [ Value.VNumber a; Value.VNumber b ] -> Value.VBool (a < b)
  (* Boolean conj/disj/not. Both operands are already evaluated (the FFI receives
     evaluated arguments), so these are not short-circuiting — matching
     `boolConj`/`boolDisj`/`boolNot` (ADR-0017). *)
  | AndBool, [ Value.VBool a; Value.VBool b ] -> Value.VBool (a && b)
  | OrBool, [ Value.VBool a; Value.VBool b ] -> Value.VBool (a || b)
  | NotBool, [ Value.VBool a ] -> Value.VBool (not a)
  | Append, [ Value.VString a; Value.VString b ] -> Value.VString (a ^ b)
  (* Unsafe index (ADR-0009): out-of-bounds is stuck, not an OCaml exception.
     The safe `index` (returning Maybe) is library code on top of this + length. *)
  | IndexArray, [ Value.VArray a; Value.VInt i ] ->
    if i >= 0 && i < Array.length a
    then a.(i)
    else Errors.stuck ("array index out of bounds: " ^ string_of_int i)
  | LengthArray, [ Value.VArray a ] -> Value.VInt (Array.length a)
  (* Unsafe array builders (ADR-0019). NewArray allocates n slots holding a filler
     (0) that the caller must overwrite before reading; SetArray writes a slot
     in place and returns the same array so a builder loop can thread it. Confined
     to the first-order layer / structural-FFI guest code and used under the ST
     discipline (fresh array, fill, then use immutably). *)
  | NewArray, [ Value.VInt n ] ->
    if n < 0
    then Errors.stuck ("array allocation with negative length: " ^ string_of_int n)
    else Value.VArray (Array.make n (Value.VInt 0))
  | SetArray, [ Value.VArray a; Value.VInt i; v ] ->
    if i >= 0 && i < Array.length a
    then (
      a.(i) <- v;
      Value.VArray a)
    else Errors.stuck ("array set out of bounds: " ^ string_of_int i)
  | _ -> Errors.stuck ("primop " ^ primop_to_string op ^ ": ill-typed arguments")
