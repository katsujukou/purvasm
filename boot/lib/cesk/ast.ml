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
  | DivInt
  | ModInt
  | AndInt
  | OrInt
  | XorInt
  | ShlInt
  | ShrInt
  | ZshrInt
  | ComplementInt
  | AddNumber
  | SubNumber
  | MulNumber
  | DivNumber
  (* Cross-representation scalar conversions (ADR-0041). [IntToNumber] widens an `Int`
     to a `Number` (`float_of_int`); [NumberToInt] is the ECMAScript `ToInt32` coercion
     (the JS `n | 0`): truncate toward zero, reduce mod 2^32, signed; NaN/inf -> 0. *)
  | IntToNumber
  | NumberToInt
  | EqInt
  | EqString
  | EqNumber
  | EqBool
  | LtInt
  | LtString
  | LtNumber
  | AndBool
  | OrBool
  | NotBool
  | Append
  | IndexArray
  | LengthArray
  | NewArray
  | SetArray
  (* Dynamic record field access by a runtime `String` label (ADR-0010's record-as-field-map,
     completing `Record.Unsafe`): get / functional-set / membership / delete. The static-label
     form is `Accessor`/`Update`; these take the label as a value. *)
  | RecordGet
  | RecordSet
  | RecordHas
  | RecordDelete

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
  (* An opaque reference to a host-provided foreign function, by its qualified key
     (ADR-0022). It carries no implementation — the machine's host registry maps
     the name to a concrete arity and host function when this is evaluated — so the
     AST stays pure, printable data. The native rung of the FFI ladder for foreign
     leaves the guest cannot express (e.g. `Data.Show.showNumberImpl`). *)
  | Foreign of string
  (* Case analysis over one or more scrutinees (ADR-0011). The scrutinees are
     evaluated left to right, then the first alternative whose binders all match
     is taken. *)
  | Case of term list * alternative list

(* One arm of a `Case`: a binder per scrutinee and the right-hand side to take
   when they all match (ADR-0013). *)
and alternative =
  { binders : binder list
  ; result : rhs
  }

(* An alternative's right-hand side (ADR-0013): either an unconditional result,
   or a list of (guard, result) pairs tried in order — the first guard that
   evaluates to `true` wins, and if all fail control falls through to the next
   alternative. Mirrors CoreFn's `Either Expr [(Guard, Expr)]`; pattern and
   conjunctive guards are desugared to nested cases by the frontend, so a guard
   here is always a single boolean term. *)
and rhs =
  | Unconditional of term
  | Guarded of (term * term) list

let primop_to_string : primop -> string = function
  | AddInt -> "+i"
  | SubInt -> "-i"
  | MulInt -> "*i"
  | DivInt -> "/i"
  | ModInt -> "%i"
  | AndInt -> "&i"
  | OrInt -> "|i"
  | XorInt -> "^i"
  | ShlInt -> "<<i"
  | ShrInt -> ">>i"
  | ZshrInt -> ">>>i"
  | ComplementInt -> "~i"
  | AddNumber -> "+n"
  | SubNumber -> "-n"
  | MulNumber -> "*n"
  | DivNumber -> "/n"
  | IntToNumber -> "i->n"
  | NumberToInt -> "n->i"
  | EqInt -> "==i"
  | EqString -> "==s"
  | EqNumber -> "==n"
  | EqBool -> "==b"
  | LtInt -> "<i"
  | LtString -> "<s"
  | LtNumber -> "<n"
  | AndBool -> "&&"
  | OrBool -> "||"
  | NotBool -> "!"
  | Append -> "<>"
  | IndexArray -> "!!"
  | LengthArray -> "#"
  | NewArray -> "new[]"
  | SetArray -> "set[]"
  | RecordGet -> "rec.get"
  | RecordSet -> "rec.set"
  | RecordHas -> "rec.has"
  | RecordDelete -> "rec.del"

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
  | Foreign name -> "#" ^ name
  | Case (scruts, alts) ->
    "(case "
    ^ String.concat ~sep:", " (List.map scruts ~f:to_string)
    ^ " of "
    ^ String.concat
        ~sep:"; "
        (List.map alts ~f:(fun { binders; result } ->
           let bs = String.concat ~sep:", " (List.map binders ~f:binder_to_string) in
           match result with
           | Unconditional t -> bs ^ " -> " ^ to_string t
           | Guarded gs ->
             bs
             ^ String.concat
                 ~sep:""
                 (List.map gs ~f:(fun (g, e) ->
                    " | " ^ to_string g ^ " -> " ^ to_string e))))
    ^ ")"
