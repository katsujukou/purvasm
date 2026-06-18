open Base

(* Structural pattern matching (ADR-0011, ADR-0012): match a binder against an
   already-evaluated value, accumulating `name -> value` bindings. A binder only
   decomposes an existing value — it never evaluates a subterm or forces a
   black-hole — so the host recursion here hides no evaluation step (cf. ADR-0002),
   the same status as `Prim.eval` working on already-computed arguments.

   The result distinguishes the machine's two kinds of "does not match" (ADR-0012):
   - a *legitimate, well-typed value-level non-match* — a different constructor
     tag, an unequal same-typed scalar, a different array length — returns `None`,
     so `case` falls through to the next alternative;
   - a *type-impossible shape mismatch* — a binder against a value of the wrong
     kind, or a record pattern naming a label the value lacks — is `stuck`
     (reachable only via `unsafeCoerce` or a lowering bug), consistent with
     `Accessor` (ADR-0010) and ill-typed primitives (ADR-0007 §4). *)

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
    if String.equal tag vtag
    then
      (* Same constructor always has the same arity; an inequality here is
         type-impossible, not a discrimination. *)
      if List.length subs = Array.length fields
      then match_binders subs (Array.to_list fields) acc
      else Errors.stuck ("constructor pattern arity mismatch: " ^ tag)
    else (* a different tag is how a sum discriminates: fall through *)
      None
  | Ast.BArray subs, Value.VArray arr ->
    (* Array length is a runtime property, not part of the type, so a different
       length is a legitimate non-match. *)
    if List.length subs = Array.length arr
    then match_binders subs (Array.to_list arr) acc
    else None
  | Ast.BRecord fields, Value.VRecord m -> match_fields fields m acc
  (* A binder against a value of the wrong kind is type-impossible (ADR-0012). *)
  | (Ast.BLit _ | Ast.BCtor _ | Ast.BArray _ | Ast.BRecord _), _ ->
    Errors.stuck "pattern matched against a value of the wrong shape"

(* Positional matching of a list of binders against a list of values, threading
   the bindings and failing fast. Used for a `case`'s binders-against-scrutinees
   (one binder per scrutinee) and for constructor fields / array elements; callers
   ensure equal lengths, so an inequality is a malformed pattern, not a
   non-match. *)
and match_binders
      (binders : Ast.binder list)
      (values : Value.t list)
      (acc : (string * Value.t) list)
  : (string * Value.t) list option
  =
  match List.zip binders values with
  | List.Or_unequal_lengths.Unequal_lengths -> Errors.stuck "pattern arity mismatch"
  | List.Or_unequal_lengths.Ok pairs ->
    List.fold_until
      pairs
      ~init:acc
      ~f:(fun acc (b, v) ->
        match match_binder b v acc with
        | Some acc' -> Continue acc'
        | None -> Stop None)
      ~finish:(fun acc -> Some acc)

(* Record patterns name a row-polymorphic subset of fields. A named label absent
   from the value is type-impossible (presence is in the row type) -> stuck. *)
and match_fields
      (fields : (string * Ast.binder) list)
      (m : Value.record)
      (acc : (string * Value.t) list)
  : (string * Value.t) list option
  =
  match fields with
  | [] -> Some acc
  | (label, p) :: rest ->
    (match Map.find m label with
     | Some value ->
       (match match_binder p value acc with
        | Some acc' -> match_fields rest m acc'
        | None -> None)
     | None -> Errors.stuck ("record pattern: missing label " ^ label))
