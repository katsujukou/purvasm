(** Per-module separate-compilation artifacts (ADR-0033):

    - [.pvmo] (a [module_artifact]) — the compiled object: the module's binding groups
      as bytecode [gdef]s, each carrying the names it defines and its cross-binding
      dependencies, for link-time reachability DCE (ADR-0021).
    - [.pvmi] (an [interface]) — the module's linkable surface (exported keys) plus a
      content [hash], the unit of recompilation avoidance: if M's interface is
      unchanged, M's dependents need not be recompiled. It is extensible to carry the
      optimisation summary a future cross-module optimiser needs.

    Both reuse [Image]'s `gdef`/bytecode JSON encoding. *)

module G = Vm.Codegen
module I = Image
module J = Yojson.Safe

let err = I.err

(* A linkable binding group: the keys it defines, the keys it references (deps, for
   reachability), and its members as bytecode definitions. A [Rec] group is atomic —
   reaching any member links them all (ADR-0021). *)
type group =
  { keys : string list
  ; deps : string list
  ; members : (string * G.gdef) list
  }

type module_artifact =
  { name : string (* qualified module name, dotted *)
  ; imports : string list (* imported module names, dotted *)
  ; exports : string list (* the module's *public* value exports, qualified *)
  ; groups : group list
  }

(* An exported value's link/optimisation-relevant shape (ADR-0033's "name →
   arity/kind"): a function of an arity, a strict CAF, or a recursive-group value. *)
type export_kind =
  | Efn of int
  | Ecaf
  | Erec

type interface =
  { iface_name : string
  ; exports : (string * export_kind) list (* public value exports → kind (sorted) *)
  ; iface_imports : string list
  ; hash : string (* digest over the downstream-relevant surface (name + kind) *)
  }

(* --- the interface (.pvmi) ------------------------------------------------- *)

let kind_of_gdef : G.gdef -> export_kind = function
  | G.Gfun (ps, _) -> Efn (List.length ps)
  | G.Gcaf _ -> Ecaf
  | G.Grec _ -> Erec

let kind_to_tag = function
  | Efn n -> "fn" ^ string_of_int n
  | Ecaf -> "caf"
  | Erec -> "rec"

(** Compute the interface of a compiled module. Its surface is the module's *public*
    exports paired with each one's kind/arity (not its private, e.g.
    compiler-synthesised, bindings — those stay in the `.pvmo` for intra-module linking
    only). The hash covers name *and* kind, so a change a dependent could observe (e.g.
    an exported function's arity) moves it while a private edit does not — ADR-0033's
    hash-stability + interface-completeness. *)
let interface_of (a : module_artifact) : interface =
  let defs : (string, G.gdef) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun g -> List.iter (fun (k, gd) -> Hashtbl.replace defs k gd) g.members)
    a.groups;
  let exports =
    a.exports
    |> List.filter_map (fun k ->
         Option.map (fun gd -> (k, kind_of_gdef gd)) (Hashtbl.find_opt defs k))
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let surface = List.map (fun (k, kd) -> k ^ ":" ^ kind_to_tag kd) exports in
  { iface_name = a.name
  ; exports
  ; iface_imports = a.imports
  ; hash = Digest.to_hex (Digest.string (String.concat "\n" surface))
  }

(* --- serialization (.pvmo / .pvmi), via Image's gdef encoding ------------- *)

let group_to_json (g : group) : J.t =
  `Assoc
    [ "keys", I.strs g.keys
    ; "deps", I.strs g.deps
    ; ( "members"
      , `List
          (List.map (fun (n, gd) -> `List [ `String n; I.gdef_to_json gd ]) g.members) )
    ]

let group_of_json : J.t -> group = function
  | `Assoc fields ->
    let get k = match List.assoc_opt k fields with Some v -> v | None -> err ("group: missing " ^ k) in
    { keys = I.strs_of (get "keys")
    ; deps = I.strs_of (get "deps")
    ; members =
        (match get "members" with
         | `List xs ->
           List.map
             (function
               | `List [ `String n; gd ] -> (n, I.gdef_of_json gd)
               | _ -> err "bad group member")
             xs
         | _ -> err "members: expected array")
    }
  | _ -> err "group: expected object"

let module_to_json (a : module_artifact) : J.t =
  `Assoc
    [ "version", `Int I.format_version
    ; "name", `String a.name
    ; "imports", I.strs a.imports
    ; "exports", I.strs a.exports
    ; "groups", `List (List.map group_to_json a.groups)
    ]

let module_of_json : J.t -> module_artifact = function
  | `Assoc fields ->
    let get k = match List.assoc_opt k fields with Some v -> v | None -> err ("pvmo: missing " ^ k) in
    (match get "version" with `Int v when v = I.format_version -> () | _ -> err "pvmo: version");
    { name = I.str_of (get "name")
    ; imports = I.strs_of (get "imports")
    ; exports = (match List.assoc_opt "exports" fields with Some v -> I.strs_of v | None -> [])
    ; groups =
        (match get "groups" with
         | `List xs -> List.map group_of_json xs
         | _ -> err "groups: expected array")
    }
  | _ -> err "pvmo: expected object"

let kind_to_json : export_kind -> J.t = function
  | Efn n -> `List [ `String "fn"; `Int n ]
  | Ecaf -> `String "caf"
  | Erec -> `String "rec"

let kind_of_json : J.t -> export_kind = function
  | `List [ `String "fn"; `Int n ] -> Efn n
  | `String "caf" -> Ecaf
  | `String "rec" -> Erec
  | _ -> err "export kind"

let interface_to_json (i : interface) : J.t =
  `Assoc
    [ "version", `Int I.format_version
    ; "name", `String i.iface_name
    ; ( "exports"
      , `List (List.map (fun (k, kd) -> `List [ `String k; kind_to_json kd ]) i.exports) )
    ; "imports", I.strs i.iface_imports
    ; "hash", `String i.hash
    ]

let interface_of_json : J.t -> interface = function
  | `Assoc fields ->
    let get k = match List.assoc_opt k fields with Some v -> v | None -> err ("pvmi: missing " ^ k) in
    (match get "version" with `Int v when v = I.format_version -> () | _ -> err "pvmi: version");
    { iface_name = I.str_of (get "name")
    ; exports =
        (match get "exports" with
         | `List xs ->
           List.map
             (function
               | `List [ `String k; kd ] -> (k, kind_of_json kd)
               | _ -> err "pvmi: bad export")
             xs
         | _ -> err "pvmi: exports expected array")
    ; iface_imports = I.strs_of (get "imports")
    ; hash = I.str_of (get "hash")
    }
  | _ -> err "pvmi: expected object"

let module_to_string a = J.to_string (module_to_json a)
let module_of_string s = module_of_json (J.from_string s)
let interface_to_string i = J.to_string (interface_to_json i)
let interface_of_string s = interface_of_json (J.from_string s)
