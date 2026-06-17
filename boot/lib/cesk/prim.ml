open Ast

(* Primitive operations. Argument evaluation order and arity are enforced by the
   machine (see Cont.Prim_args); this module only defines the operations on
   fully-evaluated values. *)

let eval (op : primop) (args : Value.t list) : Value.t =
  match op, args with
  | AddInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a + b)
  | SubInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a - b)
  | MulInt, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a * b)
  | EqInt, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a = b)
  | EqString, [ Value.VString a; Value.VString b ] -> Value.VBool (String.equal a b)
  | LtInt, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a < b)
  (* Byte-wise comparison; for UTF-8 this is code-point (natural) order. *)
  | LtString, [ Value.VString a; Value.VString b ] -> Value.VBool (String.compare a b < 0)
  | Append, [ Value.VString a; Value.VString b ] -> Value.VString (a ^ b)
  | _ -> Errors.stuck ("primop " ^ primop_to_string op ^ ": ill-typed arguments")
