(** Cross-module linking (ADR-0016): load an entry module's transitive import
    closure and link every module's lowered declarations into one [Cesk.Ast] term,
    ordered so a dependency initialises before its dependents. Foreign idents are
    resolved through an FFI resolver (empty until the FFI records); an unresolved
    foreign/external reference stays unbound, [stuck] only if forced. [load] is the
    IO surface; [link] is pure. Stdlib-only, like the rest of the frontend. *)

module C = Cesk.Ast
module M = Corefn.Module
module N = Corefn.Names
module E = Corefn.Expr
module SSet = Set.Make (String)

let name_key (n : N.module_name) : string = String.concat "." n

(* The names a binder binds (for free-variable analysis). *)
let rec binder_vars (b : C.binder) : string list =
  match b with
  | C.BNull | C.BLit _ -> []
  | C.BVar x -> [ x ]
  | C.BNamed (x, inner) -> x :: binder_vars inner
  | C.BCtor (_, subs) | C.BArray subs -> List.concat_map binder_vars subs
  | C.BRecord fields -> List.concat_map (fun (_, b) -> binder_vars b) fields

(* Free variables of a term: [Var] keys not bound by an enclosing
   Lam/Let/Letrec/Case binder. After linking, the chain binds every module
   declaration, so its free variables are exactly the program's external
   references — `foreign import`s and compiler builtins (e.g. `Prim.undefined`) —
   which the FFI resolver then binds. *)
let free_vars (t : C.term) : SSet.t =
  let rec fv (bound : SSet.t) (acc : SSet.t) (t : C.term) : SSet.t =
    match t with
    | C.Var x -> if SSet.mem x bound then acc else SSet.add x acc
    (* A `Foreign` reference is an opaque host leaf — no free variables. *)
    | C.Lit _ | C.Ctor _ | C.Foreign _ -> acc
    | C.Lam (p, body) -> fv (SSet.add p bound) acc body
    | C.App (f, a) -> fv bound (fv bound acc f) a
    | C.Let (x, e, body) -> fv (SSet.add x bound) (fv bound acc e) body
    | C.Letrec (binds, body) ->
      let bound = List.fold_left (fun s (n, _) -> SSet.add n s) bound binds in
      let acc = List.fold_left (fun a (_, e) -> fv bound a e) acc binds in
      fv bound acc body
    | C.If (c, t1, t2) -> fv bound (fv bound (fv bound acc c) t1) t2
    | C.Prim (_, args) -> List.fold_left (fv bound) acc args
    | C.Array es -> List.fold_left (fv bound) acc es
    | C.Record fs -> List.fold_left (fun a (_, e) -> fv bound a e) acc fs
    | C.Accessor (e, _) -> fv bound acc e
    | C.Update (e, ups) ->
      List.fold_left (fun a (_, e) -> fv bound a e) (fv bound acc e) ups
    | C.Case (scruts, alts) ->
      let acc = List.fold_left (fv bound) acc scruts in
      List.fold_left
        (fun acc (alt : C.alternative) ->
           let bound =
             List.fold_left
               (fun s n -> SSet.add n s)
               bound
               (List.concat_map binder_vars alt.binders)
           in
           match alt.result with
           | C.Unconditional e -> fv bound acc e
           | C.Guarded gs ->
             List.fold_left (fun a (g, e) -> fv bound (fv bound a g) e) acc gs)
        acc
        alts
  in
  fv SSet.empty SSet.empty t

(* The file `purs` emits for a module: <outdir>/<dotted name>/corefn.json. *)
let module_path (outdir : string) (n : N.module_name) : string =
  Filename.concat (Filename.concat outdir (name_key n)) "corefn.json"

(** Load the entry module and its transitive imports from a CoreFn output
    directory. A module with no [corefn.json] (Prim and other builtins) is
    skipped — it contributes no runtime bindings.

    [ulib_dir], when given, is a corefn directory of registry-package *patches*
    (ADR-0038): a module is taken from there iff it ships a [corefn.json] (a
    presence-driven, last-wins overlay over [outdir]). boot cannot read externs, so it
    *assumes* the patch is interface-compatible with the user's module. *)
let load ?ulib_dir ~(outdir : string) ~(entry_module : N.module_name) () : M.t list =
  let loaded : (string, M.t) Hashtbl.t = Hashtbl.create 32 in
  let resolve (n : N.module_name) : string =
    match ulib_dir with
    | Some ud ->
      let up = module_path ud n in
      if Sys.file_exists up then up else module_path outdir n
    | None -> module_path outdir n
  in
  let rec go (n : N.module_name) : unit =
    let k = name_key n in
    if not (Hashtbl.mem loaded k)
    then (
      let path = resolve n in
      if Sys.file_exists path
      then (
        let m = Corefn.Decode.module_of_file path in
        Hashtbl.replace loaded k m;
        List.iter (fun (imp : M.import) -> go imp.module_name) m.imports))
  in
  go entry_module;
  Hashtbl.fold (fun _ m acc -> m :: acc) loaded []

(* Order modules so a dependency precedes its dependents. The import graph is a
   DAG (PureScript forbids cyclic imports); the visited set also makes a
   self-import — or any stray cycle — terminate. *)
let topo_sort (modules : M.t list) : M.t list =
  let by_name : (string, M.t) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (m : M.t) -> Hashtbl.replace by_name (name_key m.name) m) modules;
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let order = ref [] in
  let rec visit (m : M.t) : unit =
    let k = name_key m.name in
    if not (Hashtbl.mem visited k)
    then (
      Hashtbl.replace visited k ();
      List.iter
        (fun (imp : M.import) ->
           match Hashtbl.find_opt by_name (name_key imp.module_name) with
           | Some dep -> visit dep
           | None -> ())
        m.imports;
      order := m :: !order)
  in
  List.iter visit modules;
  List.rev !order

(* A linkable binding group: the qualified keys it introduces, the free variables
   its right-hand sides reference (its dependencies), and how it wraps a body. A
   [NonRec] decl is a one-key [Let] group; a [Rec] decl is a multi-key [Letrec]
   group kept atomic — reaching any member links the whole group (ADR-0021). *)
type group =
  { keys : string list
  ; deps : SSet.t
  ; wrap : C.term -> C.term
  }

let group_of_bind (key : N.ident -> string) (b : E.bind) : group =
  match b with
  | E.NonRec (_, id, e) ->
    let k = key id in
    let t = Lower.expr e in
    { keys = [ k ]; deps = free_vars t; wrap = (fun body -> C.Let (k, t, body)) }
  | E.Rec rbs ->
    let pairs =
      List.map (fun (rb : E.rec_binding) -> key rb.ident, Lower.expr rb.expr) rbs
    in
    let deps =
      List.fold_left (fun acc (_, t) -> SSet.union acc (free_vars t)) SSet.empty pairs
    in
    { keys = List.map fst pairs; deps; wrap = (fun body -> C.Letrec (pairs, body)) }

(** Link a set of modules into a term that evaluates [entry] (in [entry_module]).
    Only the binding groups *reachable* from the entry are linked — the entry's
    transitive free-variable closure over module declarations and resolved foreign
    leaves (ADR-0021); declarations the import graph drags in but the entry never
    references are dropped, so they are never forced. [resolver] supplies a binding
    term for a resolvable foreign ident (keyed by its qualified key); an unresolved
    reached reference stays unbound (stuck only if forced). Pure — no IO. *)
let link
      ?(resolver : string -> C.term option = fun _ -> None)
      (modules : M.t list)
      ~(entry_module : N.module_name)
      ~(entry : N.ident)
  : C.term
  =
  let sorted = topo_sort modules in
  (* All module binding groups, outermost-first: in topo order a dependency module
     precedes its dependents, and within a module earlier decls precede later, so
     every cross-reference resolves to an already-introduced binding (ADR-0016). *)
  let module_groups =
    List.concat_map
      (fun (m : M.t) ->
         let key id = Lower.qualified_key m.name id in
         List.map (group_of_bind key) m.decls)
      sorted
  in
  (* Index each module-defined key to its group, for reachability lookup. *)
  let defs : (string, group) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun g -> List.iter (fun k -> Hashtbl.replace defs k g) g.keys) module_groups;
  (* Foreign groups discovered during reachability (resolved leaves), memoised by
     key. Foreign terms are closed (no module dependencies), so they can all sit
     outermost in any order. *)
  let foreign : (string, group) Hashtbl.t = Hashtbl.create 64 in
  (* Reachability from the entry: follow each reached group's free variables into
     the group that defines them (module decl or resolved foreign), to a fixpoint.
     A reached key with no defining group is an unresolved external — left unbound,
     stuck only if forced. A group is identified by its first key. *)
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 256 in
  let reached_gid : (string, unit) Hashtbl.t = Hashtbl.create 256 in
  let rec visit_key (k : string) : unit =
    if not (Hashtbl.mem seen k)
    then (
      Hashtbl.replace seen k ();
      let g_opt =
        match Hashtbl.find_opt defs k with
        | Some _ as g -> g
        | None ->
          (match Hashtbl.find_opt foreign k with
           | Some _ as g -> g
           | None ->
             Option.map
               (fun t ->
                  let g =
                    { keys = [ k ]
                    ; deps = free_vars t
                    ; wrap = (fun body -> C.Let (k, t, body))
                    }
                  in
                  Hashtbl.replace foreign k g;
                  g)
               (resolver k))
      in
      match g_opt with
      | None -> ()
      | Some g ->
        let gid = List.hd g.keys in
        if not (Hashtbl.mem reached_gid gid)
        then (
          Hashtbl.replace reached_gid gid ();
          List.iter (fun gk -> Hashtbl.replace seen gk ()) g.keys;
          SSet.iter visit_key g.deps))
  in
  let entry_key = Lower.qualified_key entry_module entry in
  visit_key entry_key;
  (* Emit only reached groups: foreign leaves outermost (closed, order-free), then
     the reached module groups in their original outermost-first order. *)
  let foreign_groups =
    Hashtbl.fold (fun _ g acc -> g :: acc) foreign []
    |> List.sort (fun a b -> String.compare (List.hd a.keys) (List.hd b.keys))
  in
  let module_reached =
    List.filter (fun g -> Hashtbl.mem reached_gid (List.hd g.keys)) module_groups
  in
  List.fold_right
    (fun g acc -> g.wrap acc)
    (foreign_groups @ module_reached)
    (C.Var entry_key)

(** Convenience: load then link from a CoreFn output directory. *)
let link_program
      ?(resolver : string -> C.term option = fun _ -> None)
      ?ulib_dir
      ~(outdir : string)
      ~(entry_module : N.module_name)
      ~(entry : N.ident)
      ()
  : C.term
  =
  link ~resolver (load ?ulib_dir ~outdir ~entry_module ()) ~entry_module ~entry
