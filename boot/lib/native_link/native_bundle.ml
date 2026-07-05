(** The Rust-foreign bundle orchestration (ADR-0078 §5): synthesize the bundle crate, drive the
    user's cargo, and audit archives with a symbol lister — shared by the CLI driver and the e2e
    harness so the tested code IS the shipping code. Failures are [Error] messages; the caller
    decides between [exit 1] (CLI) and a test failure (harness). *)

(* A TOML basic string of [s] — for the generated bundle `Cargo.toml`. [Filename.quote] is SHELL
   escaping and breaks as TOML on quotes/backslashes in a path. *)
let toml_string (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string b "\\\""
       | '\\' -> Buffer.add_string b "\\\\"
       | c when Char.code c < 0x20 ->
         Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
       | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(** The DEFINED global symbols of [archive], ld64's leading-underscore decoration stripped.
    `llvm-nm` when available — it reads Rust-produced archives cleanly, where Apple `nm` can warn
    and exit non-zero — else `nm`. A tool failure is an [Error], never an empty result (an empty
    enumeration would silently pass the duplicate checks). Parsed here rather than through a
    `grep` pipeline: a pipeline's exit status is its last command's, which would mask an `nm`
    failure with lucky partial output. Logs land in [dir] as [tag].nm.out/.err. *)
let nm_defined_symbols ~(dir : string) ~(tag : string) (archive : string)
  : (string list, string) result
  =
  let tool =
    if Sys.command "command -v llvm-nm >/dev/null 2>&1" = 0 then "llvm-nm" else "nm"
  in
  let out = Filename.concat dir (tag ^ ".nm.out") in
  let err = Filename.concat dir (tag ^ ".nm.err") in
  if
    Sys.command
      (Printf.sprintf
         "%s -g %s >%s 2>%s"
         tool
         (Filename.quote archive)
         (Filename.quote out)
         (Filename.quote err))
    <> 0
  then
    Error
      (Printf.sprintf
         "native foreign audit failed: %s could not read %s (see %s)"
         tool
         archive
         err)
  else (
    let ic = open_in out in
    let rec go acc =
      match In_channel.input_line ic with
      | None -> acc
      | Some line ->
        let toks = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
        (match List.rev toks with
         (* `nm -g` line = `[addr] TYPE name`; an undefined reference has TYPE `U` (a member
            merely *using* a symbol is fine). *)
         | name :: typ :: _ when typ <> "U" ->
           let name =
             if String.length name > 0 && name.[0] = '_'
             then String.sub name 1 (String.length name - 1)
             else name
           in
           go (name :: acc)
         | _ -> go acc)
    in
    let syms = go [] in
    close_in ic;
    Ok syms)

(** Exactly-one across ALL provider classes (ADR-0078 §5): reject any packaged `(key, symbol)`
    the runtime staticlib [rt_a] already defines — archive member selection would otherwise
    resolve the duplicate silently at the final link. *)
let check_intrinsic_collisions
      ~(dir : string)
      ~(rt_a : string)
      (packaged : (string * string) list)
  : (unit, string) result
  =
  match nm_defined_symbols ~dir ~tag:"rt" rt_a with
  | Error e -> Error e
  | Ok intrinsics ->
    (match List.find_opt (fun (_key, sym) -> List.mem sym intrinsics) packaged with
     | Some (key, sym) ->
       Error
         (Printf.sprintf
            "duplicate native foreign provider for %s: the runtime already provides %s"
            key
            sym)
     | None -> Ok ())

(* The crate's package name, read from its Cargo.toml (first `name = "…"` line — a full TOML
   parse buys nothing here); needed for the bundle's dependency renames. *)
let crate_name (dir : string) : (string, string) result =
  let mf = Filename.concat dir "Cargo.toml" in
  match open_in mf with
  | exception Sys_error e -> Error e
  | ic ->
    let rec scan () =
      match In_channel.input_line ic with
      | None ->
        close_in ic;
        Error (Printf.sprintf "no package name in %s" mf)
      | Some line ->
        (match String.index_opt line '=' with
         | Some i when String.trim (String.sub line 0 i) = "name" ->
           close_in ic;
           let s = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
           Ok (String.sub s 1 (String.length s - 2))
         | _ -> scan ())
    in
    scan ()

(** Synthesize the bundle crate under [dir]/bundle and build it with the user's cargo (ADR-0078
    §5): ONE staticlib folding the runtime rlib at [runtime_dir] and every foreign crate in
    [crates] (two Rust staticlibs cannot co-link — duplicate libstd). The cargo profile follows
    [debug] — the ADR-0079 `pv_ctx_abi_v<N>` stamp is profile-gated, so a mismatched pairing
    with the generated objects is a link error, not a silent skew. Returns the archive path. *)
let build_bundle
      ~(dir : string)
      ~(runtime_dir : string)
      ~(debug : bool)
      ~(crates : string list)
  : (string, string) result
  =
  let bundle = Filename.concat dir "bundle" in
  if Sys.command (Printf.sprintf "mkdir -p %s/src" (Filename.quote bundle)) <> 0
  then Error "cannot create the bundle crate directory"
  else (
    let dep_results = List.map crate_name crates in
    match List.find_opt Result.is_error dep_results with
    | Some (Error e) -> Error e
    | _ ->
      let deps =
        List.mapi
          (fun i (dir, name) ->
             Printf.sprintf
               "dep%d = { path = %s, package = %s }"
               i
               (toml_string dir)
               (toml_string name))
          (List.combine crates (List.map Result.get_ok dep_results))
      in
      write_file
        (Filename.concat bundle "Cargo.toml")
        (Printf.sprintf
           "[package]\n\
            name = \"purvasm-bundle\"\n\
            version = \"0.0.0\"\n\
            edition = \"2021\"\n\
            publish = false\n\n\
            [lib]\n\
            crate-type = [\"staticlib\"]\n\n\
            [dependencies]\n\
            purvasm-rt = { path = %s }\n\
            %s\n\n\
            [workspace]\n\n\
            [profile.release]\n\
            overflow-checks = true\n"
           (toml_string runtime_dir)
           (String.concat "\n" deps));
      write_file
        (Filename.concat bundle "src/lib.rs")
        (String.concat
           "\n"
           ("//! Generated by `purvasm` (ADR-0078 §5): co-links the runtime rlib and \
             every reachable Rust foreign crate into ONE staticlib.\n\
             pub use purvasm_rt as _rt;"
            :: List.mapi (fun i _ -> Printf.sprintf "pub use dep%d as _d%d;" i i) crates));
      let profile_flag, profile_dir =
        if debug then "", "debug" else "--release", "release"
      in
      let log = Filename.concat dir "bundle.cargo.log" in
      if
        Sys.command
          (Printf.sprintf
             "cargo build %s --manifest-path %s --target-dir %s 2>%s"
             profile_flag
             (Filename.quote (Filename.concat bundle "Cargo.toml"))
             (Filename.quote (Filename.concat dir "cargo"))
             (Filename.quote log))
        <> 0
      then Error (Printf.sprintf "cargo build of the foreign bundle failed (see %s)" log)
      else
        Ok
          (Filename.concat
             dir
             (Printf.sprintf "cargo/%s/libpurvasm_bundle.a" profile_dir)))

(** The `nm` count audit (ADR-0078 §5): every expected `(key, symbol)` must be defined by
    EXACTLY ONE bundle member — 0 = a crate failed to export it; > 1 = two members (two crates,
    or a crate and the runtime rlib) both define it, which archive member selection would
    silently resolve at the final link. *)
let audit_bundle ~(dir : string) ~(bundle_a : string) (expected : (string * string) list)
  : (unit, string) result
  =
  match nm_defined_symbols ~dir ~tag:"bundle" bundle_a with
  | Error e -> Error e
  | Ok syms ->
    let rec check = function
      | [] -> Ok ()
      | (key, sym) :: rest ->
        (match List.length (List.filter (String.equal sym) syms) with
         | 0 ->
           Error
             (Printf.sprintf
                "bundle staticlib is missing the native foreign symbol %s (for %s) — \
                 does its crate export it via #[pv_foreign]?"
                sym
                key)
         | 1 -> check rest
         | n ->
           Error
             (Printf.sprintf
                "duplicate native foreign provider for %s: %d bundle members define %s"
                key
                n
                sym))
    in
    check expected
