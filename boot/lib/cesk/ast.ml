open Base

(* The minimal strict core the machine evaluates. Small by design — literals,
   variables, lambda, application, let, recursive let, if, and primitives — just
   enough to exercise every CESK transition. Data constructors, pattern
   matching, and a CoreFn frontend come next. *)

type lit =
  | LInt of int
  | LBool of bool

type primop =
  | Add
  | Sub
  | Mul
  | Eq
  | Lt

type term =
  | Lit of lit
  | Var of string
  | Lam of string * term
  | App of term * term
  | Let of string * term * term
  | Letrec of string * term * term
  | If of term * term * term
  | Prim of primop * term list

let primop_to_string : primop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Eq -> "=="
  | Lt -> "<"
;;

let lit_to_string : lit -> string = function
  | LInt n -> Int.to_string n
  | LBool b -> Bool.to_string b
;;

let rec to_string : term -> string = function
  | Lit l -> lit_to_string l
  | Var x -> x
  | Lam (x, body) -> "(\\" ^ x ^ " -> " ^ to_string body ^ ")"
  | App (f, a) -> "(" ^ to_string f ^ " " ^ to_string a ^ ")"
  | Let (x, e1, e2) -> "(let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ ")"
  | Letrec (x, e1, e2) ->
    "(letrec " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ ")"
  | If (c, t, e) ->
    "(if " ^ to_string c ^ " then " ^ to_string t ^ " else " ^ to_string e ^ ")"
  | Prim (op, args) ->
    "("
    ^ primop_to_string op
    ^ " "
    ^ String.concat ~sep:" " (List.map args ~f:to_string)
    ^ ")"
;;
