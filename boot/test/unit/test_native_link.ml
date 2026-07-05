(* Unit tests for [Native_link] (ADR-0073 §3): the `ulib` native-foreign manifest reader. Each test stages
   a fresh temp dir with a `ulib.json` (and, where relevant, dummy `.c`s), then checks the parse and the
   key → `.c` resolution invariants — including the tolerant "no mapping" degradation on absent/malformed
   input, which the presence-driven overlay model relies on (ADR-0038). *)

let write path s =
  let oc = open_out path in
  output_string oc s;
  close_out oc

(* A fresh empty directory (unique name via [temp_file], then swapped for a dir). *)
let fresh_dir () =
  let f = Filename.temp_file "nl_" "" in
  Sys.remove f;
  Sys.mkdir f 0o755;
  f

let with_ulib_json (contents : string) : string =
  let d = fresh_dir () in
  write (Filename.concat d "ulib.json") contents;
  d

let strings = Alcotest.(list string)

(* [manifest_foreign] returns every `(key, relpath)` pair of the `"foreign"` object. *)
let test_manifest_pairs () =
  let d =
    with_ulib_json {|{ "foreign": { "A.f": "A.foreign.c", "B.g": "sub/B.foreign.c" } }|}
  in
  let pairs = Native_link.manifest_foreign ~ulib_dir:d in
  Alcotest.(check strings)
    "keys"
    [ "A.f"; "B.g" ]
    (List.sort compare (List.map fst pairs));
  Alcotest.(check (option string))
    "A.f path"
    (Some "A.foreign.c")
    (List.assoc_opt "A.f" pairs)

(* [foreign_c_files] resolves referenced keys against the dir, omits unmapped keys, and dedups shared `.c`. *)
let test_foreign_c_files_resolves_and_filters () =
  let d =
    with_ulib_json {|{ "foreign": { "A.f": "A.foreign.c", "B.g": "B.foreign.c" } }|}
  in
  Alcotest.(check strings)
    "only referenced, mapped keys → full paths"
    [ Filename.concat d "A.foreign.c" ]
    (Native_link.foreign_c_files ~ulib_dir:d ~keys:[ "A.f"; "Z.unmapped" ])

let test_foreign_c_files_dedups_shared_source () =
  let d = with_ulib_json {|{ "foreign": { "A.f": "shared.c", "A.g": "shared.c" } }|} in
  Alcotest.(check strings)
    "one `.c` implementing two keys appears once"
    [ Filename.concat d "shared.c" ]
    (Native_link.foreign_c_files ~ulib_dir:d ~keys:[ "A.f"; "A.g" ])

(* Absent manifest, absent `"foreign"` field, and malformed JSON all degrade to "no mapping". *)
let test_absent_manifest_is_empty () =
  let d = fresh_dir () in
  Alcotest.(check strings)
    "no ulib.json"
    []
    (List.map fst (Native_link.manifest_foreign ~ulib_dir:d));
  Alcotest.(check strings)
    "no sources"
    []
    (Native_link.foreign_c_files ~ulib_dir:d ~keys:[ "A.f" ])

let test_no_foreign_field_is_empty () =
  let d = with_ulib_json {|{ "dependencies": ["purvasm-stdio"] }|} in
  Alcotest.(check strings)
    "no foreign field"
    []
    (List.map fst (Native_link.manifest_foreign ~ulib_dir:d))

let test_malformed_json_is_empty () =
  let d = with_ulib_json {|{ this is not json|} in
  Alcotest.(check strings)
    "malformed → tolerated as empty"
    []
    (List.map fst (Native_link.manifest_foreign ~ulib_dir:d))

(* A non-string mapping value is skipped rather than crashing the read. *)
let test_non_string_value_skipped () =
  let d = with_ulib_json {|{ "foreign": { "A.f": "A.foreign.c", "B.bad": 42 } }|} in
  Alcotest.(check strings)
    "only well-typed entries"
    [ "A.f" ]
    (List.sort compare (List.map fst (Native_link.manifest_foreign ~ulib_dir:d)))

(* --- the tagged provider schema + plan (ADR-0078 §5) ------------------------------------------- *)

(* Tagged `{kind, path}` values parse into their provider kinds; the bare-string shorthand stays C. *)
let test_tagged_schema_parses_both_kinds () =
  let d =
    with_ulib_json
      {|{ "foreign": {
            "A.f": "A.foreign.c",
            "B.g": { "kind": "c", "path": "B.foreign.c" },
            "C.h": { "kind": "rust-crate", "path": "c-crate" } } }|}
  in
  match Native_link.foreign_plan ~ulib_dir:d ~keys:[ "A.f"; "B.g"; "C.h" ] with
  | Error e -> Alcotest.fail e
  | Ok { c_files; c_keys; rust_crates; rust_keys } ->
    Alcotest.(check strings)
      "c sources (shorthand + tagged)"
      [ Filename.concat d "A.foreign.c"; Filename.concat d "B.foreign.c" ]
      c_files;
    Alcotest.(check strings) "c keys" [ "A.f"; "B.g" ] c_keys;
    Alcotest.(check strings) "rust crates" [ Filename.concat d "c-crate" ] rust_crates;
    Alcotest.(check strings)
      "rust keys (the nm-audit expectation set)"
      [ "C.h" ]
      rust_keys

(* Several keys naming one crate dedup to a single crate build (ADR-0078 §5). *)
let test_plan_dedups_shared_crate () =
  let d =
    with_ulib_json
      {|{ "foreign": {
            "A.f": { "kind": "rust-crate", "path": "shared" },
            "A.g": { "kind": "rust-crate", "path": "shared" } } }|}
  in
  match Native_link.foreign_plan ~ulib_dir:d ~keys:[ "A.f"; "A.g" ] with
  | Error e -> Alcotest.fail e
  | Ok { rust_crates; _ } ->
    Alcotest.(check strings) "one crate" [ Filename.concat d "shared" ] rust_crates

(* A key mapped twice is rejected by name, before any link (ADR-0078 §5 exactly-one). *)
let test_plan_rejects_duplicate_key () =
  let d =
    with_ulib_json
      {|{ "foreign": {
            "A.f": "one.c",
            "A.f": { "kind": "rust-crate", "path": "two" } } }|}
  in
  match Native_link.foreign_plan ~ulib_dir:d ~keys:[ "A.f" ] with
  | Ok _ -> Alcotest.fail "duplicate provider must be rejected"
  | Error e ->
    Alcotest.(check string) "names the key" "duplicate native foreign provider for A.f" e

(* An unreferenced duplicate does not block a build that never links it. *)
let test_plan_ignores_unreferenced_duplicate () =
  let d =
    with_ulib_json {|{ "foreign": { "A.f": "one.c", "A.f": "two.c", "B.g": "b.c" } }|}
  in
  match Native_link.foreign_plan ~ulib_dir:d ~keys:[ "B.g" ] with
  | Error e -> Alcotest.fail e
  | Ok { c_files; _ } ->
    Alcotest.(check strings) "only the referenced key" [ Filename.concat d "b.c" ] c_files

(* An unknown kind degrades to "no mapping" (the manifest-tolerance policy), never a crash. *)
let test_unknown_kind_is_skipped () =
  let d =
    with_ulib_json {|{ "foreign": { "A.f": { "kind": "zig", "path": "a.zig" } } }|}
  in
  match Native_link.foreign_plan ~ulib_dir:d ~keys:[ "A.f" ] with
  | Error e -> Alcotest.fail e
  | Ok { c_files; rust_crates; _ } ->
    Alcotest.(check strings) "no c" [] c_files;
    Alcotest.(check strings) "no rust" [] rust_crates

let suite =
  [ Alcotest.test_case "manifest_pairs" `Quick test_manifest_pairs
  ; Alcotest.test_case
      "foreign_c_files_resolves_and_filters"
      `Quick
      test_foreign_c_files_resolves_and_filters
  ; Alcotest.test_case
      "foreign_c_files_dedups_shared_source"
      `Quick
      test_foreign_c_files_dedups_shared_source
  ; Alcotest.test_case "absent_manifest_is_empty" `Quick test_absent_manifest_is_empty
  ; Alcotest.test_case "no_foreign_field_is_empty" `Quick test_no_foreign_field_is_empty
  ; Alcotest.test_case "malformed_json_is_empty" `Quick test_malformed_json_is_empty
  ; Alcotest.test_case "non_string_value_skipped" `Quick test_non_string_value_skipped
  ; Alcotest.test_case
      "tagged_schema_parses_both_kinds"
      `Quick
      test_tagged_schema_parses_both_kinds
  ; Alcotest.test_case "plan_dedups_shared_crate" `Quick test_plan_dedups_shared_crate
  ; Alcotest.test_case "plan_rejects_duplicate_key" `Quick test_plan_rejects_duplicate_key
  ; Alcotest.test_case
      "plan_ignores_unreferenced_duplicate"
      `Quick
      test_plan_ignores_unreferenced_duplicate
  ; Alcotest.test_case "unknown_kind_is_skipped" `Quick test_unknown_kind_is_skipped
  ]

let () = Alcotest.run "native_link" [ "native_link", suite ]
