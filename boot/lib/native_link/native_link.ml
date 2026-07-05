(** Resolve a `ulib`'s native `foreign` `.c` sources from its manifest (ADR-0073 §3).

    The native backend lowers each referenced foreign to a link-time `pvf_*` symbol; a foreign the runtime
    does not provide is shipped by a `ulib` as C over the `pv_*` C-ABI. The `ulib`'s staged corefn
    directory carries an aggregated [ulib.json] whose ["foreign"] object maps a qualified foreign key to
    the `.c` implementing it (a path relative to that directory). This module reads that mapping so the
    build can compile the reachable `.c` and link it; the linker then resolves each `pvf_*` symbol from
    exactly one provider (the compiled `.c` or the runtime staticlib). Kept free of shell/compile
    orchestration so the manifest logic is unit-testable. *)

(** Whether the host linker is Apple ld64 (macOS), detected once from [uname -s]. The LLVM link
    step's flags are linker-dialect-specific (below), and OCaml exposes no finer grain than
    [Sys.os_type = "Unix"], so the standard [uname] probe decides. *)
let host_is_macos : bool Lazy.t =
  lazy
    (try
       let ic = Unix.open_process_in "uname -s" in
       let line = In_channel.input_line ic in
       ignore (Unix.close_process_in ic);
       line = Some "Darwin"
     with
     | _ -> false)

(** The link flag that drops unreferenced symbols (ADR-0072 §3 tree-shaking — a *size*
    responsibility, not correctness): ld64 spells it [-dead_strip]; GNU ld / lld spell it
    [--gc-sections], which strips at *section* granularity and therefore only bites when
    [section_cflags] split the object into per-function sections. *)
let dead_strip_link_flag () : string =
  if Lazy.force host_is_macos then "-Wl,-dead_strip" else "-Wl,--gc-sections"

(** Compile flags pairing [dead_strip_link_flag]: GNU ld's [--gc-sections] needs each
    function/datum in its own section to strip anything; ld64 dead-strips per symbol and needs
    nothing. Included in every `clang -c` of the native pipeline so the tree-shaking property
    holds on both linkers. *)
let section_cflags () : string =
  if Lazy.force host_is_macos then "" else "-ffunction-sections -fdata-sections"

(** Optimisation flag for every `clang -c` of the native pipeline (generated `.ll` and ulib
    foreign `.c` alike). The emitted IR is deliberately naive — per-step virtual registers and
    conservative root/reload traffic (ADR-0072 §6) — so leaving it `-O0` (clang's default)
    prices that boilerplate at face value. `musttail` is a semantic guarantee, preserved under
    optimisation, and the standing differential is the gate for the whole pipeline. *)
let opt_cflags () : string = "-O2"

(** A packaged native-foreign provider — the tagged manifest `source` schema (ADR-0078 §5). A
    manifest value is either a bare string (shorthand for a `.c` — the pre-0078 form, kept
    back-compatible) or a tagged object `{ "kind": "c" | "rust-crate", "path": … }`. Paths are
    relative to the ulib directory.

    NB (ADR-0078 Policy/Correction): this manifest — the whole `"foreign"` map, schema included —
    is the **boot escape hatch only** (boot cannot parse PureScript source). The durable design is
    the purs-wasm model: arity/effect from the `foreign import` signature, the implementing
    artifact discovered by co-location convention. Level 2+ never reads this file; the plan and
    validation below are discovery-agnostic and port as-is. *)
type provider =
  | C_source of string (** a `.c` over the `pv_*` C-ABI (ADR-0073 §2) *)
  | Rust_crate of string (** a cargo package over `purvasm-sys` (ADR-0078 §3/§5) *)

(* A single manifest value → provider. An unrecognised shape (unknown kind, missing path,
   non-string) degrades to "no mapping" like every other manifest malformation below: the key then
   resolves from the runtime staticlib or surfaces as a link error naming the foreign. *)
let provider_of_json (v : Yojson.Safe.t) : provider option =
  match v with
  | `String p -> Some (C_source p)
  | `Assoc fields ->
    (match List.assoc_opt "kind" fields, List.assoc_opt "path" fields with
     | Some (`String "c"), Some (`String p) -> Some (C_source p)
     | Some (`String "rust-crate"), Some (`String p) -> Some (Rust_crate p)
     | _ -> None)
  | _ -> None

(** The ["foreign"] map of `<ulib_dir>/ulib.json`: [(qualified key, provider)]. An absent file or
    absent/ill-typed ["foreign"] field yields [[]] — those keys then resolve from the runtime
    staticlib (or a truly unbound one becomes a link error). Malformed JSON is tolerated as "no
    mapping" rather than a hard failure, matching the presence-driven overlay model (ADR-0038).
    Duplicate keys are preserved (Yojson keeps repeated object keys) so [foreign_plan] can reject
    them by name. *)
let manifest_providers ~(ulib_dir : string) : (string * provider) list =
  let mf = Filename.concat ulib_dir "ulib.json" in
  if not (Sys.file_exists mf)
  then []
  else (
    try
      match Yojson.Safe.from_file mf with
      | `Assoc top ->
        (match List.assoc_opt "foreign" top with
         | Some (`Assoc entries) ->
           List.filter_map
             (fun (k, v) -> Option.map (fun p -> k, p) (provider_of_json v))
             entries
         | _ -> [])
      | _ -> []
    with
    | _ -> [])

(** The `.c`-only view of [manifest_providers] — [(key, relative `.c` path)] — kept for the
    existing consumers and tests. *)
let manifest_foreign ~(ulib_dir : string) : (string * string) list =
  List.filter_map
    (fun (k, p) ->
       match p with
       | C_source rel -> Some (k, rel)
       | Rust_crate _ -> None)
    (manifest_providers ~ulib_dir)

(** What the native link must build for the referenced foreign [keys], by provider kind: the
    deduplicated, sorted absolute paths of `.c` sources to `clang -c`, and of Rust crate
    directories to fold into the bundle staticlib (ADR-0078 §5). *)
type plan =
  { c_files : string list
  ; rust_crates : string list
  }

(** Resolve the referenced [keys] against the manifest into a [plan], rejecting a key mapped by
    more than one packaged provider with an error message naming it (ADR-0078 §5: exactly-one is
    validated by the driver, not delegated to archive-semantics linking). Keys with no mapping are
    omitted (they resolve from the runtime staticlib, whose duplicate/missing detection is the
    `nm`-audit step's job at bundle time). *)
let foreign_plan ~(ulib_dir : string) ~(keys : string list) : (plan, string) result =
  let m = manifest_providers ~ulib_dir in
  let referenced = List.filter (fun (k, _) -> List.mem k keys) m in
  let dup =
    List.find_opt
      (fun (k, _) -> List.length (List.filter (fun (k', _) -> k' = k) referenced) > 1)
      referenced
  in
  match dup with
  | Some (k, _) -> Error (Printf.sprintf "duplicate native foreign provider for %s" k)
  | None ->
    let abs rel = Filename.concat ulib_dir rel in
    let c_files =
      List.filter_map
        (function
          | _, C_source rel -> Some (abs rel)
          | _, Rust_crate _ -> None)
        referenced
      |> List.sort_uniq Stdlib.compare
    in
    let rust_crates =
      List.filter_map
        (function
          | _, Rust_crate rel -> Some (abs rel)
          | _, C_source _ -> None)
        referenced
      |> List.sort_uniq Stdlib.compare
    in
    Ok { c_files; rust_crates }

(** The deduplicated absolute `.c` paths to compile for the referenced foreign [keys] (ADR-0073 §3):
    each referenced key the manifest maps, resolved against [ulib_dir]. Keys with no mapping are
    omitted (left to the linker). The result is sorted and unique, since one `.c` may implement
    several keys. *)
let foreign_c_files ~(ulib_dir : string) ~(keys : string list) : string list =
  let m = manifest_foreign ~ulib_dir in
  keys
  |> List.filter_map (fun k -> List.assoc_opt k m)
  |> List.map (fun rel -> Filename.concat ulib_dir rel)
  |> List.sort_uniq Stdlib.compare
