(** Cross-module linking (ADR-0016): load an entry module's transitive import
    closure and link every module's lowered declarations into one [Cesk.Ast] term,
    ordered so a dependency initialises before its dependents. Foreign idents are
    resolved through an FFI resolver (empty until the FFI records); an unresolved
    foreign/external reference stays unbound, [stuck] only if forced. [load] is the
    IO surface; [link] is pure. Stdlib-only, like the rest of the frontend. *)

module C = Cesk.Ast
module M = Corefn.Module
module N = Corefn.Names

let name_key (n : N.module_name) : string = String.concat "." n

(* The file `purs` emits for a module: <outdir>/<dotted name>/corefn.json. *)
let module_path (outdir : string) (n : N.module_name) : string =
  Filename.concat (Filename.concat outdir (name_key n)) "corefn.json"

(** Load the entry module and its transitive imports from a CoreFn output
    directory. A module with no [corefn.json] (Prim and other builtins) is
    skipped — it contributes no runtime bindings. *)
let load ~(outdir : string) ~(entry_module : N.module_name) : M.t list =
  let loaded : (string, M.t) Hashtbl.t = Hashtbl.create 32 in
  let rec go (n : N.module_name) : unit =
    let k = name_key n in
    if not (Hashtbl.mem loaded k)
    then (
      let path = module_path outdir n in
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

(** Link a set of modules into a term that evaluates [entry] (in [entry_module])
    with every module's declarations in scope. [resolver] supplies a binding term
    for a resolvable foreign ident (keyed by its qualified key); unresolved
    foreign/external references stay unbound. Pure — no IO. *)
let link
      ?(resolver : string -> C.term option = fun _ -> None)
      (modules : M.t list)
      ~(entry_module : N.module_name)
      ~(entry : N.ident)
  : C.term
  =
  let sorted = topo_sort modules in
  let body = C.Var (Lower.qualified_key entry_module entry) in
  (* Each module's decls are bound under its own-module qualified keys; in topo
     order a dependency wraps outermost, so every cross-module reference resolves
     to an already-introduced binding (ADR-0016). *)
  let chain =
    List.fold_right
      (fun (m : M.t) acc -> Lower.lower_binds (Lower.qualified_key m.name) m.decls acc)
      sorted
      body
  in
  (* Foreign names the resolver can bind, prepended outermost so every module
     sees them. With the default empty resolver this is nothing. *)
  let ffi_bindings =
    List.concat_map
      (fun (m : M.t) ->
         List.filter_map
           (fun (f : N.ident) ->
              let key = Lower.qualified_key m.name f in
              Option.map (fun term -> key, term) (resolver key))
           m.foreign_names)
      modules
  in
  List.fold_right (fun (k, t) acc -> C.Let (k, t, acc)) ffi_bindings chain

(** Convenience: load then link from a CoreFn output directory. *)
let link_program
      ?(resolver : string -> C.term option = fun _ -> None)
      ~(outdir : string)
      ~(entry_module : N.module_name)
      ~(entry : N.ident)
      ()
  : C.term
  =
  link ~resolver (load ~outdir ~entry_module) ~entry_module ~entry
