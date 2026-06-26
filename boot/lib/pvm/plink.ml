(** The bytecode linker (ADR-0033): merge per-module artifacts into a runnable
    [Image], keeping only the definitions reachable from the entry (reachability DCE,
    ADR-0021). Because globals are name-keyed, linking is a name-graph merge — no
    relocation (refining ADR-0016). An unresolved reached key that the [structural]
    resolver does not supply (a native leaf, or a genuine external) is left out and
    resolved at run through the host, stuck only if forced. *)

module C = Cesk.Ast
module I = Image
module A = Artifact
module SSet = Set.Make (String)

(* Order artifacts so a dependency precedes its dependents, then a module's own decls
   keep their order — so strict-CAF (`Gcaf`) build order is a valid dependency order. *)
let topo (arts : A.module_artifact list) : A.module_artifact list =
  let by_name : (string, A.module_artifact) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (a : A.module_artifact) -> Hashtbl.replace by_name a.name a) arts;
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let order = ref [] in
  let rec visit (a : A.module_artifact) =
    if not (Hashtbl.mem visited a.name)
    then (
      Hashtbl.replace visited a.name ();
      List.iter
        (fun imp ->
           match Hashtbl.find_opt by_name imp with
           | Some d -> visit d
           | None -> ())
        a.imports;
      order := a :: !order)
  in
  List.iter visit arts;
  List.rev !order

(** Link [artifacts] for a [main_term] (which references the entry by name) into an
    image. [resolver] supplies a binding term for a resolvable foreign (the FFI
    ladder, ADR-0017/0020): a *guest-term* result (intrinsic eta-primop or structural
    foreign) is compiled here into a shared runtime definition; a *native* result
    ([C.Foreign]) is left out and resolved at run through the host. *)
let link
      (artifacts : A.module_artifact list)
      ~(resolver : string -> C.term option)
      ~(main_term : C.term)
  : I.t
  =
  (* A foreign that resolves to a guest term becomes a runtime definition; a native
     leaf ([C.Foreign]) is host-resolved at run, so it contributes no definition. *)
  let structural k =
    match resolver k with
    | Some (C.Foreign _) | None -> None
    | Some t -> Some t
  in
  let module_groups =
    List.concat_map (fun (a : A.module_artifact) -> a.groups) (topo artifacts)
  in
  let defs : (string, A.group) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (g : A.group) -> List.iter (fun k -> Hashtbl.replace defs k g) g.keys)
    module_groups;
  (* Shared-runtime groups for reached structural foreigns, memoised by key. *)
  let runtime : (string, A.group) Hashtbl.t = Hashtbl.create 64 in
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
          (match Hashtbl.find_opt runtime k with
           | Some _ as g -> g
           | None ->
             Option.map
               (fun t ->
                  let gd =
                    Vm.Codegen.gdef_of_expr ~recursive:false (Middle_end.Transl.transl t)
                  in
                  let g =
                    { A.keys = [ k ]
                    ; deps = SSet.elements (Link.free_vars t)
                    ; members = [ k, gd ]
                    }
                  in
                  Hashtbl.replace runtime k g;
                  g)
               (structural k))
      in
      match g_opt with
      | None -> () (* native / external: host-resolved at run, or stuck if forced *)
      | Some g ->
        let gid = List.hd g.keys in
        if not (Hashtbl.mem reached_gid gid)
        then (
          Hashtbl.replace reached_gid gid ();
          List.iter (fun gk -> Hashtbl.replace seen gk ()) g.keys;
          List.iter visit_key g.deps))
  in
  SSet.iter visit_key (Link.free_vars main_term);
  (* Runtime (structural) definitions first — closed, order-free, sorted for
     determinism — then the reached module definitions in dependency order. *)
  let runtime_members =
    Hashtbl.fold (fun _ g acc -> g :: acc) runtime []
    |> List.sort (fun a b -> String.compare (List.hd a.A.keys) (List.hd b.A.keys))
    |> List.concat_map (fun (g : A.group) -> g.members)
  in
  let module_members =
    List.filter
      (fun (g : A.group) -> Hashtbl.mem reached_gid (List.hd g.keys))
      module_groups
    |> List.concat_map (fun (g : A.group) -> g.members)
  in
  let main_img = I.of_term main_term in
  { I.gdefs = runtime_members @ module_members @ main_img.I.gdefs
  ; main = main_img.I.main
  ; is_effect = false (* set by the caller (the toolchain) per the entry's run mode *)
  }
