open Ast

(* Primitive operations. Argument evaluation order and arity are enforced by the
   machine (see Cont.Prim_args); this module only defines the operations on
   fully-evaluated values. *)

let eval (op : primop) (args : Value.t list) : Value.t =
  match op, args with
  | Add, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a + b)
  | Sub, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a - b)
  | Mul, [ Value.VInt a; Value.VInt b ] -> Value.VInt (a * b)
  | Eq, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a = b)
  | Lt, [ Value.VInt a; Value.VInt b ] -> Value.VBool (a < b)
  | _ -> Errors.stuck ("primop " ^ primop_to_string op ^ ": ill-typed arguments")
;;
