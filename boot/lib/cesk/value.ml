open Base

(* Runtime values. A closure captures its defining environment (the E), which
   is the whole point of lexical scoping: free variables resolve where the
   lambda was written, not where it is applied. *)

type t =
  | VInt of int
  | VNumber of float
  | VBool of bool
  | VString of string
  | VArray of t array
  | VRecord of record
  (* A fully-applied data constructor: its name (the tag we match on, ADR-0011)
     and its field values in declaration order. *)
  | VData of data
  (* A constructor still collecting its arguments. CoreFn's `Constructor` is a
     curried function of a fixed arity; this is the partial application that the
     `Arg` rule grows one field at a time until `arity` is reached, at which
     point it becomes a `VData`. `args` is accumulated in reverse. *)
  | VCtor of ctor
  | VClosure of closure
  (* A partially applied host-provided foreign function (ADR-0022). Like `VCtor`,
     it grows one argument at a time; on reaching `arity` the machine applies
     `call` to the collected arguments (in order) and returns the result. The
     implementation is first-order — it consumes evaluated values and produces a
     value, never applying a guest closure — so it needs no machine re-entrancy. *)
  | VForeign of foreign

and record = t Map.M(String).t

and data =
  { tag : string
  ; fields : t array
  }

and ctor =
  { tag : string
  ; arity : int
  ; args : t list
  }

and closure =
  { param : string
  ; body : Ast.term
  ; env : Env.t
  }

and foreign =
  { name : string
  ; arity : int
  ; args : t list (* collected in reverse, like [ctor] *)
  ; call : t list -> t
  }

let rec to_string : t -> string = function
  | VInt n -> Int.to_string n
  | VNumber f -> Float.to_string f
  | VBool b -> Bool.to_string b
  | VString s -> "\"" ^ s ^ "\""
  | VArray a ->
    "[" ^ String.concat ~sep:", " (List.map (Array.to_list a) ~f:to_string) ^ "]"
  | VRecord m ->
    "{"
    ^ String.concat
        ~sep:", "
        (List.map (Map.to_alist m) ~f:(fun (k, v) -> k ^ ": " ^ to_string v))
    ^ "}"
  | VData { tag; fields } ->
    if Array.is_empty fields
    then tag
    else
      tag
      ^ "("
      ^ String.concat ~sep:", " (List.map (Array.to_list fields) ~f:to_string)
      ^ ")"
  | VCtor { tag; arity; _ } -> "<ctor " ^ tag ^ "/" ^ Int.to_string arity ^ ">"
  | VClosure _ -> "<closure>"
  | VForeign { name; arity; _ } -> "<foreign " ^ name ^ "/" ^ Int.to_string arity ^ ">"
