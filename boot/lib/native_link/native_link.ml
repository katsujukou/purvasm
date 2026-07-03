(** Resolve a `ulib`'s native `foreign` `.c` sources from its manifest (ADR-0073 §3).

    The native backend lowers each referenced foreign to a link-time `pvf_*` symbol; a foreign the runtime
    does not provide is shipped by a `ulib` as C over the `pv_*` C-ABI. The `ulib`'s staged corefn
    directory carries an aggregated [ulib.json] whose ["foreign"] object maps a qualified foreign key to
    the `.c` implementing it (a path relative to that directory). This module reads that mapping so the
    build can compile the reachable `.c` and link it; the linker then resolves each `pvf_*` symbol from
    exactly one provider (the compiled `.c` or the runtime staticlib). Kept free of shell/compile
    orchestration so the manifest logic is unit-testable. *)

(** The ["foreign"] map of `<ulib_dir>/ulib.json`: [(qualified key, relative `.c` path)]. An absent file
    or absent/ill-typed ["foreign"] field yields [[]] — those keys then resolve from the runtime staticlib
    (or a truly unbound one becomes a link error). Malformed JSON is tolerated as "no mapping" rather than
    a hard failure, matching the presence-driven overlay model (ADR-0038). *)
let manifest_foreign ~(ulib_dir : string) : (string * string) list =
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
             (fun (k, v) ->
                match v with
                | `String p -> Some (k, p)
                | _ -> None)
             entries
         | _ -> [])
      | _ -> []
    with
    | _ -> [])

(** The deduplicated absolute `.c` paths to compile for the referenced foreign [keys] (ADR-0073 §3): each
    referenced key the manifest maps, resolved against [ulib_dir]. Keys with no mapping are omitted (left
    to the linker). The result is sorted and unique, since one `.c` may implement several keys. *)
let foreign_c_files ~(ulib_dir : string) ~(keys : string list) : string list =
  let m = manifest_foreign ~ulib_dir in
  keys
  |> List.filter_map (fun k -> List.assoc_opt k m)
  |> List.map (fun rel -> Filename.concat ulib_dir rel)
  |> List.sort_uniq Stdlib.compare
