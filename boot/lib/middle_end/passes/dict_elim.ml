(** DictElim (ADR-0027): collapse statically-known type-class dispatch.

    After lowering, a method call is [accessor dict args], where the method
    [accessor] is [\d -> case d of v -> v.φ] (the newtype dict binder erased,
    ADR-0018) and an instance [dict] is the newtype-identity-wrapped record
    [(\x -> x) { φ: impl, … }]. When both are statically known top-level bindings,
    this rewrites the saturated call to [impl args] — an atom swap, no substitution.

    Conservative by design: it fires only when the dictionary is a known instance
    (not a parameter — polymorphic code is left alone) and the resolved [impl] is
    an atom in scope everywhere (a top-level binding key, or a literal/foreign).
    Correctness is the ADR-0025 round-trip; effect is the ADR-0026 benchmark. *)

open Anf
module Ca = Cesk.Ast

(* The newtype dict wrapper lowers to the identity [\x -> x]; see through it. *)
let is_identity_lam : cexpr -> bool = function
  | CLam ([ x ], Ret (CAtom (AVar y))) -> String.equal x y
  | _ -> false

(* The program's top-level spine bindings, name -> defining computation. Only the
   outer `Let`/`LetRec` chain (where instances and accessors live); bindings inside
   lambda bodies are not collected. `Let` binds a `cexpr`; a `LetRec` member is
   recorded only when it is a single tail `cexpr` (computed members — e.g. the
   `Effect` dicts — do not classify anyway). *)
let collect_spine (e : expr) : (string, cexpr) Hashtbl.t =
  let tbl = Hashtbl.create 512 in
  let rec go = function
    | Ret _ -> ()
    | Let (x, c, rest) ->
      Hashtbl.replace tbl x c;
      go rest
    | LetRec (binds, rest) ->
      List.iter
        (fun (x, def) ->
           match def with
           | Ret c -> Hashtbl.replace tbl x c
           | _ -> ())
        binds;
      go rest
  in
  go e;
  tbl

(* Resolve a spine name to the record it denotes, seeing through aliases and the
   identity (newtype) wrapper. Returns the record's field -> atom association. *)
let rec resolve_record
          (tbl : (string, cexpr) Hashtbl.t)
          (seen : string list)
          (name : string)
  : (string * atom) list option
  =
  if List.mem name seen
  then None
  else (
    match Hashtbl.find_opt tbl name with
    | Some (CRecord fields) -> Some fields
    | Some (CAtom (AVar y)) -> resolve_record tbl (name :: seen) y
    | Some (CApp (AVar f, [ AVar a ]))
      when match Hashtbl.find_opt tbl f with
           | Some l -> is_identity_lam l
           | None -> false -> resolve_record tbl (name :: seen) a
    | _ -> None)

(* Recognise a method accessor [\d -> case d of v -> v.φ] and return [φ]. *)
let accessor_field : cexpr -> string option = function
  | CLam
      ( [ p ]
      , Ret
          (CCase
             ( [ AVar s ]
             , [ { binders = [ Ca.BVar v ]
                 ; result = Uncond (Ret (CAccessor (AVar w, field)))
                 }
               ] )) )
    when String.equal p s && String.equal v w -> Some field
  | _ -> None

(* Classify every spine binding once into accessors (name -> field) and instance
   dicts (name -> field -> impl atom). *)
let classify (tbl : (string, cexpr) Hashtbl.t)
  : (string, string) Hashtbl.t * (string, (string * atom) list) Hashtbl.t
  =
  let accs = Hashtbl.create 128 in
  let dicts = Hashtbl.create 128 in
  Hashtbl.iter
    (fun name c ->
       match accessor_field c with
       | Some field -> Hashtbl.replace accs name field
       | None -> ())
    tbl;
  Hashtbl.iter
    (fun name _ ->
       if not (Hashtbl.mem accs name)
       then (
         match resolve_record tbl [] name with
         | Some fields -> Hashtbl.replace dicts name fields
         | None -> ()))
    tbl;
  accs, dicts

(* An impl is safe to lift to a call site iff it is in scope everywhere: a literal
   or foreign, or a reference to a top-level spine binding. *)
let liftable (tbl : (string, cexpr) Hashtbl.t) : atom -> bool = function
  | ALit _ | AForeign _ -> true
  | AVar k -> Hashtbl.mem tbl k

let run (program : expr) : expr =
  let tbl = collect_spine program in
  let accs, dicts = classify tbl in
  let dispatch (acc : string) (d : string) (rest : atom list) : cexpr option =
    match Hashtbl.find_opt accs acc with
    | None -> None
    | Some field ->
      (match Hashtbl.find_opt dicts d with
       | None -> None
       | Some fields ->
         (match List.assoc_opt field fields with
          | Some impl when liftable tbl impl ->
            Some
              (match rest with
               | [] -> CAtom impl
               | _ -> CApp (impl, rest))
          | _ -> None))
  in
  let rec rw_expr (e : expr) : expr =
    match e with
    | Ret c -> Ret (rw_cexpr c)
    | Let (x, c, rest) -> Let (x, rw_cexpr c, rw_expr rest)
    | LetRec (binds, rest) ->
      LetRec (List.map (fun (x, d) -> x, rw_expr d) binds, rw_expr rest)
  and rw_cexpr (c : cexpr) : cexpr =
    match c with
    | CApp (AVar acc, AVar d :: rest) ->
      (match dispatch acc d rest with
       | Some c' -> c'
       | None -> c)
    | CLam (ps, body) -> CLam (ps, rw_expr body)
    | CIf (a, t, e) -> CIf (a, rw_expr t, rw_expr e)
    | CCase (ats, alts) -> CCase (ats, List.map rw_alt alts)
    | CAtom _
    | CApp _
    | CPrim _
    | CCtor _
    | CArray _
    | CRecord _
    | CAccessor _
    | CUpdate _ -> c
  and rw_alt (a : alt) : alt =
    { a with
      result =
        (match a.result with
         | Uncond e -> Uncond (rw_expr e)
         | Guarded gs -> Guarded (List.map (fun (g, e) -> rw_expr g, rw_expr e) gs))
    }
  in
  rw_expr program
