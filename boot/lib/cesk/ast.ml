open Base

(* The minimal strict core the machine evaluates. Small by design — literals,
   variables, lambda, application, let, recursive let, if, and primitives — just
   enough to exercise every CESK transition. Data constructors, pattern
   matching, and a CoreFn frontend come next. *)

type lit =
  | LInt of int
  | LBool of bool
  | LString of string

type primop =
  | AddInt
  | SubInt
  | MulInt
  | EqInt
  | EqString
  | LtInt
  | LtString
  | Append

type term =
  | Lit of lit
  | Var of string
  | Lam of string * term
  | App of term * term
  | Let of string * term * term
  | Letrec of (string * term) list * term
  | If of term * term * term
  | Prim of primop * term list

let primop_to_string : primop -> string = function
  | AddInt -> "+i"
  | SubInt -> "-i"
  | MulInt -> "*i"
  | EqInt -> "==i"
  | EqString -> "==s"
  | LtInt -> "<i"
  | LtString -> "<s"
  | Append -> "<>"

let lit_to_string : lit -> string = function
  | LInt n -> Int.to_string n
  | LBool b -> Bool.to_string b
  | LString s -> "\"" ^ s ^ "\""

let rec to_string : term -> string = function
  | Lit l -> lit_to_string l
  | Var x -> x
  | Lam (x, body) -> "(\\" ^ x ^ " -> " ^ to_string body ^ ")"
  | App (f, a) -> "(" ^ to_string f ^ " " ^ to_string a ^ ")"
  | Let (x, e1, e2) -> "(let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ ")"
  | Letrec (binds, body) ->
    "(letrec "
    ^ String.concat
        ~sep:" and "
        (List.map binds ~f:(fun (x, e) -> x ^ " = " ^ to_string e))
    ^ " in "
    ^ to_string body
    ^ ")"
  | If (c, t, e) ->
    "(if " ^ to_string c ^ " then " ^ to_string t ^ " else " ^ to_string e ^ ")"
  | Prim (op, args) ->
    "("
    ^ primop_to_string op
    ^ " "
    ^ String.concat ~sep:" " (List.map args ~f:to_string)
    ^ ")"
