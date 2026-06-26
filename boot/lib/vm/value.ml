(** Runtime values of the PURVASM bytecode VM (ADR-0030, slice 1). Scalars and
    constructor/record/array data mirror the CESK oracle's [Cesk.Value] so a result
    can be compared against it; a closure, however, is a *code chunk plus a captured
    environment* (not an [Ast.term]). eval/apply (ADR-0025) is realised with the
    partial forms: an under-applied closure is a [Vpap], an under-applied
    constructor a [Vctor], a saturated constructor a [Vdata]. *)

module SMap = Map.Make (String)

type t =
  | Vint of int
  | Vnumber of float
  | Vbool of bool
  | Vstring of string
  | Varray of t array
  | Vrecord of t SMap.t
  | Vdata of string * t array
  (* An under-applied constructor: tag, full arity, args collected so far. *)
  | Vctor of string * int * t list
  | Vclosure of closure
  (* An under-applied closure: the closure and the args collected so far. *)
  | Vpap of closure * t list
  (* A native foreign (ADR-0022/0032): name, arity, args collected so far (in order),
     and a first-order host implementation over VM values. Collects like [Vctor]; on
     reaching arity, [call] runs (performing IO for an effectful leaf). The [call] is
     the boundary-wrapped reuse of the oracle's host registry (see [Foreign]). *)
  | Vforeign of string * int * t list * (t list -> t)
  (* A by-need cell for the top-level recursive-binding group (ADR-0024 in the oracle;
     here the VM realisation, ADR-0030/0032). A CAF is published as [Vindirect] before
     it is built, so a cyclic instance-dictionary group can refer to siblings; forcing
     an [Unbuilt] cell builds it once and memoises ([Machine.force]). The cycle closes
     because the only *forced* edge (a curried dict projection) leads to a sibling
     whose construction merely *stores* the back-reference (a record field), not forces
     it; a genuinely self-forcing cycle hits [Building] — a black hole. *)
  | Vindirect of indirect ref

and indirect =
  | Unbuilt of (unit -> t)
  | Building
  | Built of t

(* A function value: its body chunk, its arity, and the environment captured where
   the lambda was built (lexical scope), held in a ref so a recursive group can tie
   the knot (`Make_rec`: build the members, then publish the group into the shared
   env). Free top-level references resolve through the VM's global table instead. *)
and closure =
  { params : string list (* parameter names; arity = List.length params *)
  ; body : Bytecode.chunk
  ; env : t SMap.t ref
  }

(* Printed form, byte-identical to [Cesk.Value.to_string], so the differential
   check against the oracle compares strings. *)
let rec to_string : t -> string = function
  | Vint n -> string_of_int n
  | Vnumber f -> Stdlib.string_of_float f
  | Vbool b -> string_of_bool b
  | Vstring s -> "\"" ^ s ^ "\""
  | Varray a -> "[" ^ String.concat ", " (List.map to_string (Array.to_list a)) ^ "]"
  | Vrecord m ->
    "{"
    ^ String.concat
        ", "
        (List.map (fun (k, v) -> k ^ ": " ^ to_string v) (SMap.bindings m))
    ^ "}"
  | Vdata (tag, fields) ->
    if Array.length fields = 0
    then tag
    else tag ^ "(" ^ String.concat ", " (List.map to_string (Array.to_list fields)) ^ ")"
  | Vctor (tag, arity, _) -> "<ctor " ^ tag ^ "/" ^ string_of_int arity ^ ">"
  | Vclosure _ | Vpap _ -> "<closure>"
  | Vforeign (name, arity, _, _) -> "<foreign " ^ name ^ "/" ^ string_of_int arity ^ ">"
  | Vindirect { contents = Built v } -> to_string v
  | Vindirect { contents = Unbuilt _ | Building } -> "<thunk>"
