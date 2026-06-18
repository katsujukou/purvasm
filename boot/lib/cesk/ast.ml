open Base

(* The minimal strict core the machine evaluates. Small by design — literals,
   variables, lambda, application, let, recursive let, if, and primitives — just
   enough to exercise every CESK transition. Data constructors, pattern
   matching, and a CoreFn frontend come next. *)

type lit =
  | LInt of int
  | LNumber of float
  | LBool of bool
  | LString of string

type primop =
  | AddInt
  | SubInt
  | MulInt
  | AddNumber
  | SubNumber
  | MulNumber
  | DivNumber
  | EqInt
  | EqString
  | EqNumber
  | LtInt
  | LtString
  | LtNumber
  | Append
  | IndexArray
  | LengthArray

(* A pattern, matched structurally against an already-evaluated value (ADR-0011,
   ADR-0012). Covers all of CoreFn's binders: wildcard, variable, scalar literal,
   (nested) constructor, array, record, and as-pattern. Guards live on the
   alternative, not the binder, and are a separate axis (ADR-0013). *)
type binder =
  (* `_` — matches anything, binds nothing. *)
  | BNull
  (* A variable — matches anything and binds it to the name. *)
  | BVar of string
  (* A scalar literal — matches only an equal value (Int/Number/Bool/String). *)
  | BLit of lit
  (* A constructor pattern — matches a `VData` with the same tag and arity, then
     matches each sub-binder against the corresponding field. *)
  | BCtor of string * binder list
  (* A fixed-length array pattern (ADR-0012) — matches a `VArray` of exactly this
     many elements, element-wise. A different length is a legitimate non-match
     (array length is not part of the type). *)
  | BArray of binder list
  (* A record pattern (ADR-0012) — matches a `VRecord` that has at least these
     labels (row-polymorphic subset), matching each named field's sub-binder. *)
  | BRecord of (string * binder) list
  (* An as-pattern `x@p` — binds the whole value to `x` and also matches `p`. *)
  | BNamed of string * binder

type term =
  | Lit of lit
  | Var of string
  | Lam of string * term
  | App of term * term
  | Let of string * term * term
  | Letrec of (string * term) list * term
  | If of term * term * term
  | Prim of primop * term list
  | Array of term list
  | Record of (string * term) list
  | Accessor of term * string
  | Update of term * (string * term) list
  (* A data constructor as a value: its tag and arity. Arity 0 is a nullary
     constructor (immediately a `VData`); higher arities yield a curried
     constructor function (a `VCtor`). *)
  | Ctor of string * int
  (* Case analysis over one or more scrutinees (ADR-0011). The scrutinees are
     evaluated left to right, then the first alternative whose binders all match
     is taken. *)
  | Case of term list * alternative list

(* One arm of a `Case`: a binder per scrutinee and the term to evaluate when they
   all match. Guards are deferred to a follow-on record. *)
and alternative =
  { binders : binder list
  ; result : term
  }

let primop_to_string : primop -> string = function
  | AddInt -> "+i"
  | SubInt -> "-i"
  | MulInt -> "*i"
  | AddNumber -> "+n"
  | SubNumber -> "-n"
  | MulNumber -> "*n"
  | DivNumber -> "/n"
  | EqInt -> "==i"
  | EqString -> "==s"
  | EqNumber -> "==n"
  | LtInt -> "<i"
  | LtString -> "<s"
  | LtNumber -> "<n"
  | Append -> "<>"
  | IndexArray -> "!!"
  | LengthArray -> "#"

let lit_to_string : lit -> string = function
  | LInt n -> Int.to_string n
  | LNumber f -> Float.to_string f
  | LBool b -> Bool.to_string b
  | LString s -> "\"" ^ s ^ "\""

let rec binder_to_string : binder -> string = function
  | BNull -> "_"
  | BVar x -> x
  | BLit l -> lit_to_string l
  | BCtor (tag, []) -> tag
  | BCtor (tag, subs) ->
    tag ^ "(" ^ String.concat ~sep:", " (List.map subs ~f:binder_to_string) ^ ")"
  | BArray subs -> "[" ^ String.concat ~sep:", " (List.map subs ~f:binder_to_string) ^ "]"
  | BRecord fields ->
    "{"
    ^ String.concat
        ~sep:", "
        (List.map fields ~f:(fun (l, b) -> l ^ ": " ^ binder_to_string b))
    ^ "}"
  | BNamed (x, b) -> x ^ "@" ^ binder_to_string b

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
  | Array elems -> "[" ^ String.concat ~sep:", " (List.map elems ~f:to_string) ^ "]"
  | Record fields ->
    "{"
    ^ String.concat ~sep:", " (List.map fields ~f:(fun (l, e) -> l ^ ": " ^ to_string e))
    ^ "}"
  | Accessor (e, l) -> to_string e ^ "." ^ l
  | Update (e, ups) ->
    to_string e
    ^ " { "
    ^ String.concat ~sep:", " (List.map ups ~f:(fun (l, u) -> l ^ " = " ^ to_string u))
    ^ " }"
  | Ctor (tag, arity) -> tag ^ "/" ^ Int.to_string arity
  | Case (scruts, alts) ->
    "(case "
    ^ String.concat ~sep:", " (List.map scruts ~f:to_string)
    ^ " of "
    ^ String.concat
        ~sep:"; "
        (List.map alts ~f:(fun { binders; result } ->
           String.concat ~sep:", " (List.map binders ~f:binder_to_string)
           ^ " -> "
           ^ to_string result))
    ^ ")"
