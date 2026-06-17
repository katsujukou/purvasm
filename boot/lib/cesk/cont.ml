(* The K of CESK: the continuation, reified as an explicit stack of frames
   instead of relying on the host call stack. This is the keystone of the whole
   project: because the continuation is data, it can be saved, resumed, and
   scheduled — which is exactly what async, effect handlers, and lightweight
   fibers will need. The `attempts` stack of purescript-aff and the `conts`
   stack of Cats Effect 3 are this same idea. *)

type t =
  | Halt
  (* Evaluating the function position of an application; the argument term is
     still pending. *)
  | Fun of Ast.term * Env.t * t
  (* The function value is known; waiting on the argument's value. *)
  | Arg of Value.t * t
  (* Evaluating a let's bound expression; the body is pending. *)
  | Let_body of string * Ast.term * Env.t * t
  (* Evaluating an if's condition; both branches are pending. *)
  | If_branch of Ast.term * Ast.term * Env.t * t
  (* Evaluating a primitive's arguments left to right: values already computed
     (in reverse), then the terms still to evaluate. *)
  | Prim_args of Ast.primop * Value.t list * Ast.term list * Env.t * t
  (* Tying a recursive binding group's knot (ADR-0004/0005). Every address in
     the group is already reserved; the right-hand sides are evaluated left to
     right, each backpatched as its value arrives. Fields: the address being
     filled now, the (address, right-hand side) pairs still pending, the body,
     the recursive environment shared by every binding, and the rest. *)
  | Letrec_bind of Addr.t * (Addr.t * Ast.term) list * Ast.term * Env.t * t
  (* Building an array literal: elements already evaluated (in reverse), then the
     element terms still to evaluate — the same shape as Prim_args (ADR-0009). *)
  | Array_elems of Value.t list * Ast.term list * Env.t * t

let frame_name : t -> string = function
  | Halt -> "halt"
  | Fun _ -> "fun"
  | Arg _ -> "arg"
  | Let_body _ -> "let"
  | If_branch _ -> "if"
  | Prim_args _ -> "prim"
  | Letrec_bind _ -> "rec"
  | Array_elems _ -> "arr"
