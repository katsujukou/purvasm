(** The native foreign boundary (ADR-0032): reuse the oracle's host registry
    ([Cesk.Machine.host], the single source of truth, ADR-0022) from the VM by
    converting VM values to oracle values for a leaf's arguments and back for its
    result. The conversion is total over what first-order leaves touch — scalars,
    strings, arrays, constructor data — and re-wraps a returned foreign (e.g.
    `Console.log`'s `#perform` thunk) as a VM foreign carrying the same conversion. A
    function value never crosses a first-order leaf, so a closure on either side is
    stuck (it would indicate a non-first-order foreign, which is guest code, ADR-0020). *)

module V = Value
module O = Cesk.Value

let unsupported what = raise (Machine.Vm_error ("foreign boundary: " ^ what))

(* VM value → oracle value (a leaf's arguments). Force through any forwarding cell
   first (ADR-0032), so a knot-tied dictionary value crossing the boundary is its
   built form. *)
let rec to_oracle (v : V.t) : O.t =
  match Machine.force v with
  | V.Vint n -> O.VInt n
  | V.Vnumber f -> O.VNumber f
  | V.Vbool b -> O.VBool b
  | V.Vstring s -> O.VString s
  | V.Varray a -> O.VArray (Array.map to_oracle a)
  | V.Vdata (tag, fields) -> O.VData { tag; fields = Array.map to_oracle fields }
  | V.Vforeign (name, arity, args, call) ->
    (* Oracle [VForeign] keeps its collected args reversed; mirror that. *)
    O.VForeign
      { name
      ; arity
      ; args = List.rev_map to_oracle args
      ; call = (fun oargs -> to_oracle (call (List.map of_oracle oargs)))
      }
  | V.Vrecord _ -> unsupported "record crossed a first-order leaf"
  | V.Vctor _ -> unsupported "partial constructor crossed a leaf"
  | V.Vclosure _ | V.Vpap _ -> unsupported "closure crossed a first-order leaf"
  | V.Vindirect _ -> unsupported "black hole crossed a leaf" (* unreachable after force *)

(* Oracle value → VM value (a leaf's result). *)
and of_oracle (v : O.t) : V.t =
  match v with
  | O.VInt n -> V.Vint n
  | O.VNumber f -> V.Vnumber f
  | O.VBool b -> V.Vbool b
  | O.VString s -> V.Vstring s
  | O.VArray a -> V.Varray (Array.map of_oracle a)
  | O.VData { tag; fields } -> V.Vdata (tag, Array.map of_oracle fields)
  | O.VForeign { name; arity; args; call } ->
    (* Oracle args are reversed; restore order, and wrap the call back to VM values. *)
    V.Vforeign
      ( name
      , arity
      , List.rev_map of_oracle args
      , fun vmargs -> of_oracle (call (List.map to_oracle vmargs)) )
  | O.VRecord _ -> unsupported "record returned from a first-order leaf"
  | O.VCtor _ -> unsupported "partial constructor returned from a leaf"
  | O.VClosure _ -> unsupported "closure returned from a first-order leaf"

(** Materialise a VM foreign for [key] from the oracle host registry, with the
    boundary conversion wrapped around its call. [None] if the host has no such leaf. *)
let lookup (host : Cesk.Machine.host) (key : string) : V.t option =
  match host key with
  | None -> None
  | Some (arity, call) ->
    Some
      (V.Vforeign
         (key, arity, [], fun vmargs -> of_oracle (call (List.map to_oracle vmargs))))
