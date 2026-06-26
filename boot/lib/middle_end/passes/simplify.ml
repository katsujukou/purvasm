(** Copy-propagation and small-callee inlining (ADR-0028), run after DictElim.

    Two dependency-directed rewrites, applied with an environment threaded down the
    term (so it respects scope) to a bounded fixpoint:

    - copy-propagation — a [let x = <atom> in …] is dropped and [x] resolved to the
      atom (no work duplicated, no capture); this clears DictElim's aliases
      (`$x = intAdd`) and other trivial bindings;
    - saturated inlining — a call [f a₁…aₙ] whose binding is [\p₁…pₙ -> Ret c] with
      [c] *binder-free* (no nested if/case/lambda) and the call exactly saturated
      becomes [c] with each [pᵢ] replaced by [aᵢ]. The caller pulls in a callee's
      (a dependency's) body; the decision is the callee's own shape, never who
      calls it — so the pass stays modular ([[optimizer-modular-not-whole-program]]).

    Together with DictElim this turns a monomorphic `m a b` into `Prim(op, a, b)`.
    Correctness is the ADR-0025 round-trip; effect is the ADR-0026 benchmark. *)

open Anf
module M = Map.Make (String)

(* Drop [names] from the known-environment: a name being re-bound in an inner scope must not
   resolve to a (now-shadowed) outer alias/fun. Omitting this lets an inner binder that reuses a
   source name (e.g. a record field `step`) wrongly resolve to an outer entry — a scope bug that
   only surfaces once name reuse appears (large closures). *)
let remove_all (names : string list) (env : 'a M.t) : 'a M.t =
  List.fold_left (fun e n -> M.remove n e) env names

(* The variables an ANF/Cesk binder introduces. *)
let rec binder_vars : Cesk.Ast.binder -> string list = function
  | Cesk.Ast.BNull | Cesk.Ast.BLit _ -> []
  | Cesk.Ast.BVar x -> [ x ]
  | Cesk.Ast.BNamed (x, b) -> x :: binder_vars b
  | Cesk.Ast.BCtor (_, bs) | Cesk.Ast.BArray bs -> List.concat_map binder_vars bs
  | Cesk.Ast.BRecord fs -> List.concat_map (fun (_, b) -> binder_vars b) fs

(* A binder-free computation: small and with no nested scope. *)
let is_flat : cexpr -> bool = function
  | CLam _ | CIf _ | CCase _ -> false
  | _ -> true

(* The variables a flat computation refers to (atom positions only). *)
let avars (c : cexpr) : string list =
  let of_atom = function
    | AVar x -> [ x ]
    | _ -> []
  in
  let of_atoms = List.concat_map of_atom in
  match c with
  | CAtom a -> of_atom a
  | CApp (h, args) -> of_atom h @ of_atoms args
  | CPrim (_, args) | CArray args -> of_atoms args
  | CCtor (_, _, args) -> of_atoms args
  | CRecord fs -> of_atoms (List.map snd fs)
  | CAccessor (a, _) -> of_atom a
  | CUpdate (a, ups) -> of_atom a @ of_atoms (List.map snd ups)
  | CLam _ | CIf _ | CCase _ -> []

(* Inlinable iff binder-free and *closed but for its parameters* — so substituting
   it at any call site cannot be captured by a shadowing binder there. Conservative
   (a free top-level reference would also be safe), but it covers the eta-expanded
   primops, which is the v1 target; relaxing it needs alpha-renaming. *)
let inlinable (params : string list) (c : cexpr) : bool =
  is_flat c && List.for_all (fun v -> List.mem v params) (avars c)

(* What a name is known to denote in the current scope. *)
type known =
  | Alias of atom
  | Fun of string list * cexpr

let run (program : expr) : expr =
  let changed = ref false in
  (* Follow copy-propagation aliases to the underlying atom. *)
  let rec resolve_atom env (a : atom) : atom =
    match a with
    | AVar x ->
      (match M.find_opt x env with
       | Some (Alias b) -> resolve_atom env b
       | _ -> a)
    | _ -> a
  in
  (* Substitute parameter atoms into a flat inlined body (atom positions only). *)
  let subst (m : atom M.t) : cexpr -> cexpr =
    let s a =
      match a with
      | AVar x ->
        (match M.find_opt x m with
         | Some b -> b
         | None -> a)
      | _ -> a
    in
    function
    | CAtom a -> CAtom (s a)
    | CApp (h, args) -> CApp (s h, List.map s args)
    | CPrim (op, args) -> CPrim (op, List.map s args)
    | CCtor (t, n, args) -> CCtor (t, n, List.map s args)
    | CArray args -> CArray (List.map s args)
    | CRecord fs -> CRecord (List.map (fun (l, a) -> l, s a) fs)
    | CAccessor (a, l) -> CAccessor (s a, l)
    | CUpdate (a, ups) -> CUpdate (s a, List.map (fun (l, a) -> l, s a) ups)
    | (CLam _ | CIf _ | CCase _) as c -> c (* not flat: never an inlined body *)
  in
  let rec rw_expr env (e : expr) : expr =
    match e with
    | Ret c -> Ret (rw_cexpr env c)
    | Let (x, c, body) ->
      let c' = rw_cexpr env c in
      (match c' with
       | CAtom a ->
         (* copy-propagation: drop the binding, resolve x to the atom *)
         changed := true;
         rw_expr (M.add x (Alias (resolve_atom env a)) env) body
       | CLam (params, Ret cf) when inlinable params cf ->
         Let (x, c', rw_expr (M.add x (Fun (params, cf)) env) body)
       | _ ->
         (* [x] is re-bound to a non-inlinable computation: it must shadow any outer entry. *)
         Let (x, c', rw_expr (M.remove x env) body))
    | LetRec (binds, body) ->
      (* Conservatively do not register recursive members as inlinable; the bound names shadow
         any outer entries in both the members and the body. *)
      let env = remove_all (List.map fst binds) env in
      LetRec (List.map (fun (x, d) -> x, rw_expr env d) binds, rw_expr env body)
  and rw_cexpr env (c : cexpr) : cexpr =
    match c with
    | CAtom a -> CAtom (resolve_atom env a)
    | CApp (h, args) ->
      let args = List.map (resolve_atom env) args in
      (match resolve_atom env h with
       | AVar g ->
         (match M.find_opt g env with
          | Some (Fun (params, cf)) when List.length params = List.length args ->
            changed := true;
            let m = List.fold_left2 (fun acc p a -> M.add p a acc) M.empty params args in
            subst m cf
          | _ -> CApp (AVar g, args))
       | h' -> CApp (h', args))
    | CPrim (op, args) -> CPrim (op, List.map (resolve_atom env) args)
    | CCtor (t, n, args) -> CCtor (t, n, List.map (resolve_atom env) args)
    | CArray args -> CArray (List.map (resolve_atom env) args)
    | CRecord fs -> CRecord (List.map (fun (l, a) -> l, resolve_atom env a) fs)
    | CAccessor (a, l) -> CAccessor (resolve_atom env a, l)
    | CUpdate (a, ups) ->
      CUpdate (resolve_atom env a, List.map (fun (l, a) -> l, resolve_atom env a) ups)
    | CLam (ps, body) -> CLam (ps, rw_expr (remove_all ps env) body)
    | CIf (a, t, e) -> CIf (resolve_atom env a, rw_expr env t, rw_expr env e)
    | CCase (ats, alts) ->
      CCase (List.map (resolve_atom env) ats, List.map (rw_alt env) alts)
  and rw_alt env (a : alt) : alt =
    (* The alternative's binders shadow any outer entries in the result/guards. *)
    let env = remove_all (List.concat_map binder_vars a.binders) env in
    { a with
      result =
        (match a.result with
         | Uncond e -> Uncond (rw_expr env e)
         | Guarded gs ->
           Guarded (List.map (fun (g, e) -> rw_expr env g, rw_expr env e) gs))
    }
  in
  (* Bounded fixpoint: a flat body may expose a further inline; in practice one or
     two passes suffice (eta-primops are non-recursive). The cap bounds any
     pathological chain. *)
  let rec loop n e =
    if n = 0
    then e
    else (
      changed := false;
      let e' = rw_expr M.empty e in
      if !changed then loop (n - 1) e' else e')
  in
  loop 5 program
