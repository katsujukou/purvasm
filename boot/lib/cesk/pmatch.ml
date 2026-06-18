open Base

(* Structural pattern matching (ADR-0011): attempt to match a binder against an
   already-evaluated value, accumulating `name -> value` bindings. This is a
   pure, total function over values — a binder only decomposes an existing value,
   it never evaluates a subterm or forces a black-hole — so the host recursion
   here hides no evaluation step (cf. ADR-0002), exactly like `Prim.eval` working
   on already-computed arguments. A binder either matches (yielding bindings) or
   fails; a failure is a normal "try the next alternative", not a stuck state. *)

let rec match_binder (b : Ast.binder) (v : Value.t) (acc : (string * Value.t) list)
  : (string * Value.t) list option
  =
  match b, v with
  | Ast.BNull, _ -> Some acc
  | Ast.BVar x, _ -> Some ((x, v) :: acc)
  (* As-pattern: bind the whole value, then keep matching the inner binder. *)
  | Ast.BNamed (x, inner), _ -> match_binder inner v ((x, v) :: acc)
  | Ast.BLit (Ast.LInt n), Value.VInt m -> if n = m then Some acc else None
  | Ast.BLit (Ast.LBool b'), Value.VBool m -> if Bool.equal b' m then Some acc else None
  | Ast.BLit (Ast.LString s), Value.VString m ->
    if String.equal s m then Some acc else None
  (* IEEE equality, as in `Prim.eval EqNumber` (ADR-0008): a NaN literal never
     matches, since polymorphic `=` on floats is false for NaN. *)
  | Ast.BLit (Ast.LNumber f), Value.VNumber m -> if Poly.(f = m) then Some acc else None
  | Ast.BCtor (tag, subs), Value.VData { tag = vtag; fields } ->
    (* A different tag is a legitimate match failure (take the next alternative),
       not an error. Arity always agrees for a well-typed program. *)
    if String.equal tag vtag && List.length subs = Array.length fields
    then match_binders subs (Array.to_list fields) acc
    else None
  (* Any other binder/value pairing cannot match. Well-typed CoreFn only reaches
     this for a literal/constructor pattern that legitimately does not match the
     value, so it falls through to the next alternative. *)
  | _ -> None

(* Match a list of binders against the same-length list of values, threading the
   accumulated bindings and short-circuiting on the first failure. *)
and match_binders
      (binders : Ast.binder list)
      (values : Value.t list)
      (acc : (string * Value.t) list)
  : (string * Value.t) list option
  =
  match List.zip binders values with
  | List.Or_unequal_lengths.Unequal_lengths -> None
  | List.Or_unequal_lengths.Ok pairs ->
    List.fold_until
      pairs
      ~init:acc
      ~f:(fun acc (b, v) ->
        match match_binder b v acc with
        | Some acc' -> Continue acc'
        | None -> Stop None)
      ~finish:(fun acc -> Some acc)

(* Select the first alternative whose binders all match the scrutinee values,
   returning its bindings and result term. *)
let select (alts : Ast.alternative list) (values : Value.t list)
  : ((string * Value.t) list * Ast.term) option
  =
  List.find_map alts ~f:(fun { Ast.binders; result } ->
    match match_binders binders values [] with
    | Some bindings -> Some (bindings, result)
    | None -> None)
