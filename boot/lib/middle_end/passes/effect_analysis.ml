(** Structural effect analysis (ADR-0034): classify each binding by whether forcing
    it performs an observable effect, with no type information — only host leaf bits
    ([Ffi.effectful], passed in as [effectful_leaf]) and the term structure.

    The force/saturation model (ADR-0034 I1: construction ≠ execution) needs two
    properties per value, not one — *building* an `Effect` is pure; the effect fires
    only when the built thunk is itself saturated. Throughout, "saturate a value" means
    "apply that value's own [arity] of arguments to it". Because `Effect a` is the
    nullary thunk `Unit -> a`, an effectful call stacks *two* saturations on *two
    different values*, and the two properties name them:

    - [vsat] — does saturating *this* value (applying its own [arity] arguments)
      perform?
    - [ret_vsat] — is the value *produced* by that saturation itself effectful, i.e.
      does saturating *it* in turn perform?

    Worked example, [log : String -> Effect Unit] (arity 1):
    - [log "x"] saturates [log] — it only *builds* the `Effect Unit` value (the
      `#perform` thunk); no IO. So [log]'s [vsat] is *false*.
    - [(log "x") ()] saturates that *produced* value (the thunk's own unit argument) —
      this is the actual IO. So [log]'s [ret_vsat] is *true* (equivalently, the [vsat]
      of the [log "x"] value is true).

    So [log "x" ()] is *not* "saturating [log]"; it is saturating [log]'s result.

    [eperf e] ("does evaluating [e] perform") is the dead-binding-elimination
    predicate (drop a `let` only when [not (eperf rhs)] and the binding is unused —
    ADR-0034's partial-correctness DBE). It is pure for construction, including a
    *partial* application (a closure is built, its body not run); it is the saturated
    callee's [vsat], or — for the mutation primops [NewArray]/[SetArray] — [true].

    Conservative and sound by over-approximation: an unknown call (a parameter, a
    record field, dictionary-passing effect polymorphism) is [may-perform]. Precision
    returns once a specialiser concretises dictionaries (specialise → effect-analyse).

    A recursive (or mutually recursive) group is solved by a least fixpoint over the
    two booleans (monotone false → true), so a self-recursive effect like
    [go n = log n *> go (n-1)] is not misclassified pure. *)

open Anf
module M = Map.Make (String)

(** The summary of a *value*: how many more arguments saturate it, whether saturating
    performs, and whether the saturated result is itself an effect-thunk. *)
type vsum =
  { arity : int
  ; vsat : bool
  ; ret_vsat : bool
  }

(* A plain, fully-evaluated data value: nothing to force, nothing to perform. *)
let pure_value = { arity = 0; vsat = false; ret_vsat = false }

(* The conservative default for anything opaque (an unbound/free var, a parameter, a
   projected field): applying it may perform, and its result may itself be effectful. *)
let unknown = { arity = 0; vsat = true; ret_vsat = true }

let join a b =
  { arity = min a.arity b.arity (* fewer args to saturate: conservative *)
  ; vsat = a.vsat || b.vsat
  ; ret_vsat = a.ret_vsat || b.ret_vsat
  }

(* Local-name uniqueness invariant: PureScript's CoreFn renames shadowed locals to
   distinct names (e.g. `case Just x of Just x -> …` emits the inner binder as `x1`),
   and lowering/[Transl] keep source names verbatim (only ANF's own `$a…` are fresh).
   So a case binder or lambda parameter never collides with an outer in-scope binding,
   and a name not in [env] is genuinely free — [atom_sum]'s [unknown] default is
   therefore correct without explicitly inserting binders. (The other passes, e.g.
   [Simplify], rely on the same invariant.) *)

(* Does [x] occur in an atom position anywhere in [e]? The dead-binding eliminator
   keeps a binding that is still referenced. Shadowing is ignored — an occurrence under
   an inner binder of the same name is still counted, which can only *keep* a binding,
   never drop a live one (sound). *)
let occurs (x : string) (e : expr) : bool =
  let in_atom = function
    | AVar y -> String.equal x y
    | _ -> false
  in
  let any = List.exists in_atom in
  let rec ex = function
    | Ret c -> cx c
    | Let (_, c, body) -> cx c || ex body
    | LetRec (g, body) -> List.exists (fun (_, r) -> ex r) g || ex body
  and cx = function
    | CAtom a -> in_atom a
    | CApp (h, args) -> in_atom h || any args
    | CPrim (_, args) | CArray args -> any args
    | CCtor (_, _, args) -> any args
    | CRecord fs -> any (List.map snd fs)
    | CAccessor (a, _) -> in_atom a
    | CUpdate (a, ups) -> in_atom a || any (List.map snd ups)
    | CLam (_, body) -> ex body
    | CIf (a, t, e) -> in_atom a || ex t || ex e
    | CCase (ats, alts) -> any ats || List.exists alt alts
  and alt (a : alt) =
    match a.result with
    | Uncond e -> ex e
    | Guarded gs -> List.exists (fun (g, e) -> ex g || ex e) gs
  in
  ex e

(* The arity to *seed* a recursive-group member with before the fixpoint: its
   parameter count when it is a (possibly let-wrapped) lambda. A point-free binding
   (e.g. [inc = add 1], a partial application) has no syntactic lambda, so it seeds 0
   and the fixpoint recomputes the real residual arity via [vsum_c] (here 1). Seeding
   low is safe: an under-approximate arity only makes applications look like
   over-applications (conservatively may-perform). Non-recursive point-free bindings
   never use this — [collect] computes their arity directly from the head already in
   scope. *)
let rec rhs_arity (e : expr) : int =
  match e with
  | Ret (CLam (ps, _)) -> List.length ps
  | Let (_, _, rest) | LetRec (_, rest) -> rhs_arity rest
  | _ -> 0

(** The analysis API for a fixed leaf classification: [analyze] returns every
    top-level binding's [vsum]; [eperf] is the dead-binding-elimination predicate on a
    whole program (does evaluating it perform). *)
type t =
  { analyze : expr -> vsum M.t
  ; eperf : expr -> bool
  ; dbe : expr -> expr
    (* Dead-binding elimination (ADR-0034's partial-correctness DBE): drop a
           [let x = c in body] when [c] cannot perform and [x] is unused in [body]. *)
  }

(** Build the analysis for a given native-leaf classification: [effectful_leaf] (does
    the leaf's result perform when forced — [Ffi.effectful]) and [foreign_arity] (its
    declared arity — [Ffi.foreign_arity]). The arity is load-bearing: a multi-argument
    effectful leaf such as `runEffectFnN` (arity [N+1]) returns its `Effect` thunk only
    when *fully* applied — assuming arity 1 would call a partial application "saturated"
    and mis-place the effect. *)
let create ~(effectful_leaf : string -> bool) ~(foreign_arity : string -> int) : t =
  let foreign_sum k =
    (* Applying a leaf to its full arity *builds* a value (pure, so [vsat] false); that
       value is an effect-thunk iff the leaf is effectful (e.g. [log "x"], or
       [runEffectFn2 f a b], builds an `Effect` thunk that performs on force). *)
    { arity = foreign_arity k; vsat = false; ret_vsat = effectful_leaf k }
  in
  let atom_sum env = function
    | AVar x ->
      (match M.find_opt x env with
       | Some s -> s
       | None -> unknown)
    | AForeign k -> foreign_sum k
    | ALit _ -> pure_value
  in
  let rec vsum_expr env (e : expr) : vsum =
    match e with
    | Ret c -> vsum_c env c
    | Let (x, c, rest) -> vsum_expr (M.add x (vsum_c env c) env) rest
    | LetRec (g, rest) -> vsum_expr (fix_group env g) rest
  and vsum_c env (c : cexpr) : vsum =
    match c with
    | CAtom a -> atom_sum env a
    | CLam (ps, body) ->
      let env' = List.fold_left (fun e p -> M.add p unknown e) env ps in
      { arity = List.length ps
      ; vsat = eperf_expr env' body (* saturating the closure runs its body *)
      ; ret_vsat = (vsum_expr env' body).vsat (* its result, when forced, performs? *)
      }
    | CApp (f, args) ->
      let sf = atom_sum env f in
      let n = List.length args in
      if n < sf.arity
      then { arity = sf.arity - n; vsat = sf.vsat; ret_vsat = sf.ret_vsat } (* PAP *)
      else if n = sf.arity
      then
        { arity = 0; vsat = sf.ret_vsat; ret_vsat = sf.ret_vsat }
        (* saturated result: one level of [ret] info; [arity 0] forces any later
           application of it onto the over-application path (conservatively safe) *)
      else unknown (* over-application result is an opaque value (see [eperf_c]) *)
    | CPrim _ | CCtor _ | CArray _ | CRecord _ | CUpdate _ -> pure_value
    | CAccessor _ -> unknown (* a field may be any value, incl. an effectful function *)
    | CIf (_, t, e) -> join (vsum_expr env t) (vsum_expr env e)
    | CCase (_, alts) ->
      List.fold_left (fun acc a -> join acc (alt_vsum env a)) pure_value alts
  and alt_vsum env (a : alt) : vsum =
    match a.result with
    | Uncond e -> vsum_expr env e
    | Guarded gs ->
      List.fold_left (fun acc (_, e) -> join acc (vsum_expr env e)) pure_value gs
  and eperf_expr env (e : expr) : bool =
    match e with
    | Ret c -> eperf_c env c
    | Let (x, c, rest) -> eperf_c env c || eperf_expr (M.add x (vsum_c env c) env) rest
    | LetRec (g, rest) ->
      eperf_expr (fix_group env g) rest (* building closures is pure *)
  and eperf_c env (c : cexpr) : bool =
    match c with
    | CAtom _ | CLam _ | CCtor _ | CArray _ | CRecord _ | CAccessor _ | CUpdate _ ->
      false (* construction / projection is pure (I1) *)
    | CPrim (op, _) ->
      (match op with
       | Cesk.Ast.NewArray | Cesk.Ast.SetArray -> true
       | _ -> false)
    | CApp (f, args) ->
      let sf = atom_sum env f in
      let n = List.length args in
      if n < sf.arity
      then false (* partial application: builds a PAP, body not run *)
      else if n = sf.arity
      then
        sf.vsat
        (* exact saturation: runs only this callee; deeper levels of a
                      returned closure fire at their own application sites *)
      else true
      (* over-application (e.g. [(log s) u] flattened to one node, or applying a
         let-returned closure chain): the extra arguments force the produced value, and
         a [vsum] tracks only one level below — it cannot prove an effect buried ≥2
         levels deep absent, so over-application is conservatively may-perform. *)
    | CIf (_, t, e) -> eperf_expr env t || eperf_expr env e
    | CCase (_, alts) -> List.exists (alt_eperf env) alts
  and alt_eperf env (a : alt) : bool =
    match a.result with
    | Uncond e -> eperf_expr env e
    | Guarded gs -> List.exists (fun (g, e) -> eperf_expr env g || eperf_expr env e) gs
  (* Least fixpoint over a recursive group: start each member optimistically pure
     (with its known arity) and re-evaluate under the group's own bindings until the
     two booleans stop flipping (monotone, so it converges in a few rounds). *)
  and fix_group env (binds : (string * expr) list) : vsum M.t =
    let init =
      List.fold_left
        (fun e (x, rhs) ->
           M.add x { arity = rhs_arity rhs; vsat = false; ret_vsat = false } e)
        env
        binds
    in
    let step env_g =
      List.fold_left (fun e (x, rhs) -> M.add x (vsum_expr env_g rhs) e) env_g binds
    in
    let changed env_a env_b =
      List.exists
        (fun (x, _) ->
           let a = M.find x env_a
           and b = M.find x env_b in
           (* [arity] too: a point-free member's real arity is recovered over several
             hops (it propagates from the lambda it bottoms out at), and it is
             monotone-increasing and bounded, so iterating until it settles converges
             and makes recursive point-free bindings precise, not just sound-low. *)
           a.arity <> b.arity || a.vsat <> b.vsat || a.ret_vsat <> b.ret_vsat)
        binds
    in
    let rec loop env_g n =
      if n = 0
      then env_g
      else (
        let env' = step env_g in
        if changed env_g env' then loop env' (n - 1) else env')
    in
    loop init (List.length binds + 2)
  in
  let rec collect env (e : expr) : vsum M.t =
    match e with
    | Ret _ -> env
    | Let (x, c, rest) -> collect (M.add x (vsum_c env c) env) rest
    | LetRec (g, rest) -> collect (fix_group env g) rest
  in
  (* Rewrite, threading the same scope env the analysis uses, dropping dead pure lets.
     The body is rewritten first so a binding becomes droppable once the rewrite has
     removed its last use. *)
  let rec dbe env (e : expr) : expr =
    match e with
    | Ret c -> Ret (dbe_c env c)
    | Let (x, c, body) ->
      let pure = not (eperf_c env c) in
      let env' = M.add x (vsum_c env c) env in
      let body' = dbe env' body in
      if pure && not (occurs x body') then body' else Let (x, dbe_c env c, body')
    | LetRec (g, body) ->
      let env' = fix_group env g in
      let body' = dbe env' body in
      if
        List.for_all (fun (n, _) -> not (occurs n body')) g
        && not (List.exists (fun (_, rhs) -> eperf_expr env' rhs) g)
      then body' (* whole group unused and pure to build: drop it *)
      else LetRec (List.map (fun (n, rhs) -> n, dbe env' rhs) g, body')
  and dbe_c env (c : cexpr) : cexpr =
    match c with
    | CLam (ps, body) ->
      let env' = List.fold_left (fun e p -> M.add p unknown e) env ps in
      CLam (ps, dbe env' body)
    | CIf (a, t, e) -> CIf (a, dbe env t, dbe env e)
    | CCase (ats, alts) ->
      CCase
        ( ats
        , List.map
            (fun (al : alt) ->
               { al with
                 result =
                   (match al.result with
                    | Uncond e -> Uncond (dbe env e)
                    | Guarded gs ->
                      Guarded (List.map (fun (g, e) -> dbe env g, dbe env e) gs))
               })
            alts )
    | CAtom _
    | CApp _
    | CPrim _
    | CCtor _
    | CArray _
    | CRecord _
    | CAccessor _
    | CUpdate _ -> c
  in
  { analyze = (fun program -> collect M.empty program)
  ; eperf = (fun program -> eperf_expr M.empty program)
  ; dbe = (fun program -> dbe M.empty program)
  }
