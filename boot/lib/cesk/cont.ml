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
  (* Forcing a by-need recursive binding (ADR-0024). The binding's right-hand side
     is being evaluated; when its value arrives we memoize it at `addr` (ending the
     black-hole) and hand it on. *)
  | Force of Addr.t * t
  (* Building an array literal: elements already evaluated (in reverse), then the
     element terms still to evaluate — the same shape as Prim_args (ADR-0009). *)
  | Array_elems of Value.t list * Ast.term list * Env.t * t
  (* Building a record value field by field (ADR-0010): the field map so far, the
     label whose value is being evaluated now, and the (label, term) fields still
     to go. A record literal seeds the map empty; a record update seeds it with
     the base record (Update_rec evaluates that base first), so both share this
     frame. *)
  | Record_fields of Value.record * string * (string * Ast.term) list * Env.t * t
  (* Projecting a field: the record is being evaluated; once it is a value we
     read this label out of it. *)
  | Project of string * t
  (* A record *functional* update whose base record is being evaluated; once it is a record we
     start evaluating the update fields (see Record_fields). *)
  | Update_rec of (string * Ast.term) list * Env.t * t
  (* Evaluating a case's scrutinees left to right (ADR-0011): values already
     computed (in reverse), the scrutinee terms still to evaluate, the
     alternatives to try once all scrutinees are values, and the environment of
     the case. Same accumulate-in-reverse shape as Prim_args / Array_elems. *)
  | Case_scrut of Value.t list * Ast.term list * Ast.alternative list * Env.t * t
  (* Evaluating a guard of a matched alternative (ADR-0013). If the guard returns
     true we take `on_true`; if false we try the next guard in `rest_guards`, or —
     when those are exhausted — fall through to `rest_alts` (re-matching them
     against the already-evaluated `scrutinees` in `case_env`). `alt_env` is the
     environment extended with this alternative's pattern bindings, shared by the
     guards and their results. An inline record (not the tuple shape of the other
     frames) keeps these seven fields legible; it is still first-order data. *)
  | Guard_test of
      { on_true : Ast.term
      ; rest_guards : (Ast.term * Ast.term) list
      ; alt_env : Env.t
      ; rest_alts : Ast.alternative list
      ; scrutinees : Value.t list
      ; case_env : Env.t
      ; rest : t
      }

let frame_name : t -> string = function
  | Halt -> "halt"
  | Fun _ -> "fun"
  | Arg _ -> "arg"
  | Let_body _ -> "let"
  | If_branch _ -> "if"
  | Prim_args _ -> "prim"
  | Force _ -> "force"
  | Array_elems _ -> "arr"
  | Record_fields _ -> "fld"
  | Project _ -> "prj"
  | Update_rec _ -> "upd"
  | Case_scrut _ -> "case"
  | Guard_test _ -> "grd"
