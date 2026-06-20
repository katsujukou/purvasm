(** The two semantics-preserving bridges between the upper IR ([Cesk.Ast]) and the
    lower IR ([Anf]), ADR-0025: [transl] normalises and uncurries; [rev_transl]
    re-curries. Any optimiser pass [p : Anf.expr -> Anf.expr] is validated by the
    round-trip [eval (rev_transl (p (transl e))) = eval e] against the oracle — no
    separate evaluator or VM. Stdlib-only. *)

open Anf
module C = Cesk.Ast

(* --- transl: Cesk.Ast -> ANF (normalise + uncurry) ------------------------ *)

(* Collect a curried lambda spine `\a -> \b -> body` into ([a; b], body). *)
let rec collect_lam (t : C.term) : string list * C.term =
  match t with
  | C.Lam (p, body) ->
    let params, inner = collect_lam body in
    p :: params, inner
  | _ -> [], t

(* Collect an application spine `((f a) b) c` into (f, [a; b; c]). *)
let collect_app (t : C.term) : C.term * C.term list =
  let rec go t args =
    match t with
    | C.App (f, a) -> go f (a :: args)
    | _ -> t, args
  in
  go t []

(** Normalise a term to ANF (its tail computation is the result). *)
let transl (term : C.term) : expr =
  let counter = ref 0 in
  let fresh () =
    incr counter;
    "$a" ^ string_of_int !counter
  in
  (* [norm_ce t k]: normalise [t], handing its computation to [k], which builds the
     surrounding let-sequence. [norm_atom]/[norm_atoms] additionally let-bind a
     non-atomic result to a fresh name so it can sit in argument position. *)
  let rec norm_ce (t : C.term) (k : cexpr -> expr) : expr =
    match t with
    | C.Lit l -> k (CAtom (ALit l))
    | C.Var x -> k (CAtom (AVar x))
    | C.Foreign s -> k (CAtom (AForeign s))
    | C.Ctor (tag, arity) -> k (CCtor (tag, arity, []))
    | C.Lam _ ->
      let params, body = collect_lam t in
      k (CLam (params, anf_tail body))
    | C.App _ ->
      let head, args = collect_app t in
      norm_atoms args (fun arg_atoms ->
        match head with
        (* A constructor application stays a constructor node (keeps its arity). *)
        | C.Ctor (tag, arity) -> k (CCtor (tag, arity, arg_atoms))
        | _ -> norm_atom head (fun h -> k (CApp (h, arg_atoms))))
    | C.Prim (op, args) -> norm_atoms args (fun atoms -> k (CPrim (op, atoms)))
    | C.Array es -> norm_atoms es (fun atoms -> k (CArray atoms))
    | C.Record fields ->
      let labels = List.map fst fields in
      norm_atoms (List.map snd fields) (fun atoms ->
        k (CRecord (List.combine labels atoms)))
    | C.Accessor (e, label) -> norm_atom e (fun a -> k (CAccessor (a, label)))
    | C.Update (e, ups) ->
      norm_atom e (fun a ->
        let labels = List.map fst ups in
        norm_atoms (List.map snd ups) (fun atoms ->
          k (CUpdate (a, List.combine labels atoms))))
    | C.If (c, t1, t2) -> norm_atom c (fun ca -> k (CIf (ca, anf_tail t1, anf_tail t2)))
    | C.Let (x, e1, e2) -> norm_ce e1 (fun c1 -> Let (x, c1, norm_ce e2 k))
    | C.Letrec (binds, body) ->
      let binds = List.map (fun (x, rhs) -> x, anf_tail rhs) binds in
      LetRec (binds, norm_ce body k)
    | C.Case (scruts, alts) ->
      norm_atoms scruts (fun satoms -> k (CCase (satoms, List.map anf_alt alts)))
  and norm_atom (t : C.term) (k : atom -> expr) : expr =
    norm_ce t (fun ce ->
      match ce with
      | CAtom a -> k a
      | _ ->
        let x = fresh () in
        Let (x, ce, k (AVar x)))
  and norm_atoms (ts : C.term list) (k : atom list -> expr) : expr =
    match ts with
    | [] -> k []
    | t :: rest -> norm_atom t (fun a -> norm_atoms rest (fun atoms -> k (a :: atoms)))
  and anf_tail (t : C.term) : expr = norm_ce t (fun ce -> Ret ce)
  and anf_alt (a : C.alternative) : alt =
    { binders = a.binders
    ; result =
        (match a.result with
         | C.Unconditional e -> Uncond (anf_tail e)
         | C.Guarded gs -> Guarded (List.map (fun (g, e) -> anf_tail g, anf_tail e) gs))
    }
  in
  anf_tail term

(* --- rev_transl: ANF -> Cesk.Ast (re-curry) ------------------------------- *)

let atom_to_term : atom -> C.term = function
  | AVar x -> C.Var x
  | ALit l -> C.Lit l
  | AForeign s -> C.Foreign s

let rec cexpr_to_term (c : cexpr) : C.term =
  match c with
  | CAtom a -> atom_to_term a
  | CLam (params, body) ->
    List.fold_right (fun p acc -> C.Lam (p, acc)) params (expr_to_term body)
  | CApp (h, args) ->
    List.fold_left (fun f a -> C.App (f, atom_to_term a)) (atom_to_term h) args
  | CPrim (op, args) -> C.Prim (op, List.map atom_to_term args)
  | CCtor (tag, arity, args) ->
    List.fold_left (fun f a -> C.App (f, atom_to_term a)) (C.Ctor (tag, arity)) args
  | CArray atoms -> C.Array (List.map atom_to_term atoms)
  | CRecord fields -> C.Record (List.map (fun (l, a) -> l, atom_to_term a) fields)
  | CAccessor (a, label) -> C.Accessor (atom_to_term a, label)
  | CUpdate (a, ups) ->
    C.Update (atom_to_term a, List.map (fun (l, x) -> l, atom_to_term x) ups)
  | CIf (ca, t, e) -> C.If (atom_to_term ca, expr_to_term t, expr_to_term e)
  | CCase (satoms, alts) ->
    C.Case (List.map atom_to_term satoms, List.map alt_to_alternative alts)

and expr_to_term (e : expr) : C.term =
  match e with
  | Ret c -> cexpr_to_term c
  | Let (x, c, rest) -> C.Let (x, cexpr_to_term c, expr_to_term rest)
  | LetRec (binds, body) ->
    C.Letrec (List.map (fun (x, rhs) -> x, expr_to_term rhs) binds, expr_to_term body)

and alt_to_alternative (a : alt) : C.alternative =
  { C.binders = a.binders
  ; result =
      (match a.result with
       | Uncond e -> C.Unconditional (expr_to_term e)
       | Guarded gs ->
         C.Guarded (List.map (fun (g, e) -> expr_to_term g, expr_to_term e) gs))
  }

(** Re-curry ANF back to the upper IR, so a program (or an optimiser pass's output)
    can be evaluated on the CESK oracle (ADR-0025). Inverse of [transl] up to the
    fresh let-bindings normalisation introduces and the currying it removed —
    semantics-preserving, not syntactically identity. *)
let rev_transl (e : expr) : C.term = expr_to_term e
