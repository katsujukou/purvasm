(** Separate compilation (ADR-0033): compile one CoreFn module, in isolation, into a
    bytecode [Artifact.module_artifact] (+ its [Artifact.interface]). Cross-module and
    foreign references are left as qualified names — resolved at link (module / shared
    runtime) or at run (a native leaf, through the host fall-through in [Machine]). So
    the compiler needs no view of other modules, no resolver, and no host. *)

module M = Corefn.Module
module E = Corefn.Expr
module C = Cesk.Ast
module SSet = Set.Make (String)

(* One CoreFn binding → a linkable group: lower each right-hand side, record its free
   variables as dependencies, and classify its ANF into a [gdef] (ADR-0030/0032). A
   recursive group is kept atomic with by-need value members. *)
let group_of_bind (key : Corefn.Names.ident -> string) (b : E.bind) : Artifact.group =
  let gdef ~recursive (t : C.term) =
    Vm.Codegen.gdef_of_expr ~recursive (Middle_end.Transl.transl t)
  in
  match b with
  | E.NonRec (_, id, e) ->
    let k = key id in
    let t = Lower.expr e in
    { Artifact.keys = [ k ]
    ; deps = SSet.elements (Link.free_vars t)
    ; members = [ (k, gdef ~recursive:false t) ]
    }
  | E.Rec rbs ->
    let pairs =
      List.map (fun (rb : E.rec_binding) -> (key rb.ident, Lower.expr rb.expr)) rbs
    in
    let deps =
      List.fold_left (fun acc (_, t) -> SSet.union acc (Link.free_vars t)) SSet.empty pairs
    in
    { Artifact.keys = List.map fst pairs
    ; deps = SSet.elements deps
    ; members = List.map (fun (k, t) -> (k, gdef ~recursive:true t)) pairs
    }

(** Compile a module to its `.pvmo` artifact. The interface's [exports] are the
    module's *public* value exports (CoreFn `exports`), intersected with the keys it
    actually defines — so private, compiler-synthesised bindings (e.g. specialised
    typeclass methods) stay in the object for intra-module linking but are not part of
    the public surface (ADR-0033). *)
let compile_module (m : M.t) : Artifact.module_artifact =
  let key id = Lower.qualified_key m.name id in
  let groups = List.map (group_of_bind key) m.decls in
  let defined = SSet.of_list (List.concat_map (fun (g : Artifact.group) -> g.keys) groups) in
  { Artifact.name = Link.name_key m.name
  ; imports = List.map (fun (i : M.import) -> Link.name_key i.module_name) m.imports
  ; exports = List.filter (fun k -> SSet.mem k defined) (List.map key m.exports)
  ; groups
  }
