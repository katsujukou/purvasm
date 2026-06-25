open Cesk
open Cesk.Ast

(* Smart constructors keep the sample programs readable. There is no parser yet,
   so programs are built directly as core terms. *)
let num n = Lit (LInt n)
let add_int a b = Prim (AddInt, [ a; b ])
let sub_int a b = Prim (SubInt, [ a; b ])
let mul_int a b = Prim (MulInt, [ a; b ])
let lt_int a b = Prim (LtInt, [ a; b ])
let lt_string a b = Prim (LtString, [ a; b ])
let eq_int a b = Prim (EqInt, [ a; b ])
let str s = Lit (LString s)
let append a b = Prim (Append, [ a; b ])
let numf f = Lit (LNumber f)
let add_num a b = Prim (AddNumber, [ a; b ])
let div_num a b = Prim (DivNumber, [ a; b ])
let arr es = Array es
let index a i = Prim (IndexArray, [ a; i ])
let length a = Prim (LengthArray, [ a ])
let rcd fields = Record fields
let proj l e = Accessor (e, l)
let upd e ups = Update (e, ups)
let just x = App (Ctor ("Just", 1), x)
let nil = Ctor ("Nil", 0)
let cons h t = App (App (Ctor ("Cons", 2), h), t)
let alt binders result = { binders; result = Unconditional result }
let altg binders guards = { binders; result = Guarded guards }

let samples : (string * term) list =
  [ "arith: (2+3)*2", mul_int (add_int (num 2) (num 3)) (num 2)
  ; "let: let x=2 in x+1", Let ("x", num 2, add_int (Var "x") (num 1))
  ; "lambda: (\\x -> x+1) 4", App (Lam ("x", add_int (Var "x") (num 1)), num 4)
  ; ( "closure: let a=10 in (\\x -> x+a) 5"
    , Let ("a", num 10, App (Lam ("x", add_int (Var "x") (Var "a")), num 5)) )
  ; ( "higher-order: (\\f -> f (f 1)) (\\n -> n*2)"
    , App
        ( Lam ("f", App (Var "f", App (Var "f", num 1)))
        , Lam ("n", mul_int (Var "n") (num 2)) ) )
  ; "if: if 1<2 then 10 else 20", If (lt_int (num 1) (num 2), num 10, num 20)
  ; ( "letrec: fact 5"
    , Letrec
        ( [ ( "fact"
            , Lam
                ( "n"
                , If
                    ( lt_int (Var "n") (num 1)
                    , num 1
                    , mul_int (Var "n") (App (Var "fact", sub_int (Var "n") (num 1))) ) )
            )
          ]
        , App (Var "fact", num 5) ) )
  ; ( "mutual: even 10"
    , Letrec
        ( [ ( "even"
            , Lam
                ( "n"
                , If
                    ( eq_int (Var "n") (num 0)
                    , Lit (LBool true)
                    , App (Var "odd", sub_int (Var "n") (num 1)) ) ) )
          ; ( "odd"
            , Lam
                ( "n"
                , If
                    ( eq_int (Var "n") (num 0)
                    , Lit (LBool false)
                    , App (Var "even", sub_int (Var "n") (num 1)) ) ) )
          ]
        , App (Var "even", num 10) ) )
  ; "string: \"foo\" <> \"bar\"", append (str "foo") (str "bar")
  ; ( "string: if \"a\" < \"b\" then \"yes\" else \"no\""
    , If (lt_string (str "a") (str "b"), str "yes", str "no") )
  ; "number: 1.5 + 2.0", add_num (numf 1.5) (numf 2.0)
  ; "number: 10.0 / 4.0", div_num (numf 10.0) (numf 4.0)
  ; "array: [10,20,30] !! 1", index (arr [ num 10; num 20; num 30 ]) (num 1)
  ; "array: length [1,2,3]", length (arr [ num 1; num 2; num 3 ])
  ; "record: {x:1, y:2}.y", proj "y" (rcd [ "x", num 1; "y", num 2 ])
  ; ( "record: ({x:1,y:2} {x=9}).x"
    , proj "x" (upd (rcd [ "x", num 1; "y", num 2 ]) [ "x", num 9 ]) )
  ; ( "adt: case Just 5 of Just x -> x+1; Nothing -> 0"
    , Case
        ( [ just (num 5) ]
        , [ alt [ BCtor ("Just", [ BVar "x" ]) ] (add_int (Var "x") (num 1))
          ; alt [ BCtor ("Nothing", []) ] (num 0)
          ] ) )
  ; ( "adt: sum [1,2,3] over Cons/Nil"
    , Letrec
        ( [ ( "sum"
            , Lam
                ( "xs"
                , Case
                    ( [ Var "xs" ]
                    , [ alt [ BCtor ("Nil", []) ] (num 0)
                      ; alt
                          [ BCtor ("Cons", [ BVar "h"; BVar "t" ]) ]
                          (add_int (Var "h") (App (Var "sum", Var "t")))
                      ] ) ) )
          ]
        , App (Var "sum", cons (num 1) (cons (num 2) (cons (num 3) nil))) ) )
  ; ( "match: case [10,20] of [a,b] -> a+b"
    , Case
        ( [ arr [ num 10; num 20 ] ]
        , [ alt [ BArray [ BVar "a"; BVar "b" ] ] (add_int (Var "a") (Var "b")) ] ) )
  ; ( "match: case {x:1,y:2} of {x:a,y:b} -> a+b"
    , Case
        ( [ rcd [ "x", num 1; "y", num 2 ] ]
        , [ alt [ BRecord [ "x", BVar "a"; "y", BVar "b" ] ] (add_int (Var "a") (Var "b"))
          ] ) )
  ; ( "guard: case 5 of x | x<0 -> -1 | x==0 -> 0 | _ -> 1"
    , Case
        ( [ num 5 ]
        , [ altg
              [ BVar "x" ]
              [ lt_int (Var "x") (num 0), num (-1)
              ; eq_int (Var "x") (num 0), num 0
              ; Lit (LBool true), num 1
              ]
          ] ) )
  ]

let run_all (trace : bool) : unit =
  List.iter
    (fun (name, term) ->
       if trace then Stdlib.print_endline ("=== " ^ name ^ " ===");
       let v = Machine.eval ~trace term in
       Stdlib.Printf.printf "%-44s => %s\n" name (Value.to_string v))
    samples

let trace_arg =
  let open Cmdliner in
  Arg.(value & flag & info [ "t"; "trace" ] ~doc:"Print every CESK transition.")

(* --- the PURVASM AOT toolchain (ADR-0033): compile / link / run ------------ *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let split_module s = String.split_on_char '.' s
let mtime p = (Unix.stat p).Unix.st_mtime

(* Create a directory and any missing parents. *)
let rec mkdir_p dir =
  if not (Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

(* Layout (ADR-0033): the corefn dir holds `purs` output; the output dir holds the
   image at its root and intermediate `.pvmo`/`.pvmi` under `_build/`. *)
let build_dir output = Filename.concat output "_build"
let pvmo_path output name = Filename.concat (build_dir output) (name ^ ".pvmo")
let pvmi_path output name = Filename.concat (build_dir output) (name ^ ".pvmi")
let image_path output = Filename.concat output "app.pvm"

let emit_artifact output (a : Pvm.Artifact.module_artifact) : unit =
  write_file (pvmo_path output a.name) (Pvm.Artifact.module_to_string a);
  write_file
    (pvmi_path output a.name)
    (Pvm.Artifact.interface_to_string (Pvm.Artifact.interface_of a))

(* Compile one module, reusing a fresh `.pvmo` (recompilation avoidance, ADR-0033):
   skip the compile when the object is newer than the source AND still loads at the
   current artifact version. The version check makes a toolchain update that bumps
   [Image.format_version] (which must be bumped on any codegen/encoding change)
   invalidate stale objects rather than silently reuse them. The `.pvmi`-hash
   cross-module cascade activates once cross-module optimisation exists. *)
let compile_incremental ~corefn_dir ~output (m : Corefn.Module.t) : Pvm.Artifact.module_artifact =
  let name = Link.name_key m.name in
  let obj = pvmo_path output name in
  let pvmi = pvmi_path output name in
  let mtime_ok =
    Sys.file_exists obj
    && (try mtime obj >= mtime (Link.module_path corefn_dir m.name) with _ -> false)
  in
  (* Reuse only if the object both is newer than the source and parses at the current
     version (a version/format mismatch raises, dropping to recompile). *)
  let reused =
    if mtime_ok
    then (
      try
        let a = Pvm.Artifact.module_of_string (read_file obj) in
        if not (Sys.file_exists pvmi)
        then write_file pvmi (Pvm.Artifact.interface_to_string (Pvm.Artifact.interface_of a));
        Some a
      with _ -> None)
    else None
  in
  match reused with
  | Some a -> a
  | None ->
    let a = Pvm.Compile.compile_module m in
    emit_artifact output a;
    a

(* The entry's run mode: an `Effect` (default — apply to unit, perform); a swept
   `Int -> Int` ([--arg]); or a bare value ([--value]). *)
let entry_main ~value ~arg entry_key : term * bool =
  match value, arg with
  | true, _ -> (Var entry_key, false)
  | false, Some n -> (App (Var entry_key, Lit (LInt n)), false)
  | false, None -> (App (Var entry_key, Lit (LInt 0)), true)

let build_action corefn_dir output entry_module entry arg value =
  mkdir_p (build_dir output);
  let em = split_module entry_module in
  let modules = Link.load ~outdir:corefn_dir ~entry_module:em () in
  let artifacts = List.map (compile_incremental ~corefn_dir ~output) modules in
  let main_term, is_effect = entry_main ~value ~arg (Lower.qualified_key em entry) in
  let img =
    { (Pvm.Plink.link artifacts ~resolver:Ffi.resolver ~main_term) with
      Pvm.Image.is_effect = is_effect
    }
  in
  Pvm.Image.write_file (image_path output) img;
  Stdlib.Printf.printf
    "wrote %s (%d definitions)\n"
    (image_path output)
    (List.length img.Pvm.Image.gdefs)

let abspath p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

(* Compile a program to a *native* executable via the OCaml backend (ADR-0035/0036):
   link the CoreFn closure to one term, normalise to optimised ANF, emit OCaml source,
   and hand it to `ocamlopt`. The entry is treated as an `Effect` by default (the
   generated runner forces it to unit); `--arg N` applies an `Int -> Int` entry and
   `--value` takes a bare value. *)
let native_action corefn_dir output entry_module entry arg value ulib =
  mkdir_p (build_dir output);
  let em = split_module entry_module in
  let term0 =
    Link.link_program ~resolver:Ffi.resolver ?ulib_dir:ulib ~outdir:corefn_dir ~entry_module:em ~entry ()
  in
  let main_term, is_effect =
    match value, arg with
    | true, _ -> (term0, false)
    | false, Some n -> (App (term0, Lit (LInt n)), false)
    | false, None -> (term0, true)
  in
  let anf =
    Middle_end.Passes.Dbe.run ~effectful_leaf:Ffi.effectful ~foreign_arity:Ffi.foreign_arity
      (Middle_end.Passes.Simplify.run
         (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl main_term)))
  in
  let bdir = build_dir output in
  write_file (Filename.concat bdir "app.ml") (Ocaml_backend.Codegen_ml.program ~is_effect anf);
  let exe = abspath (Filename.concat output "app") in
  let rc =
    Sys.command
      (Stdlib.Printf.sprintf "cd %s && ocamlfind ocamlopt app.ml -o %s 2>ocamlopt.log"
         (Filename.quote (abspath bdir)) (Filename.quote exe))
  in
  if rc <> 0
  then (
    Stdlib.Printf.eprintf "ocamlopt failed; see %s\n" (Filename.concat bdir "ocamlopt.log");
    Stdlib.exit 1);
  Stdlib.Printf.printf "wrote %s\n" exe

let compile_action corefn output =
  mkdir_p (build_dir output);
  let m = Corefn.Decode.module_of_file corefn in
  emit_artifact output (Pvm.Compile.compile_module m);
  Stdlib.Printf.printf "compiled %s\n" (Link.name_key m.name)

(* Run the image. An `Effect` entry is performed for its effects and its `Unit`
   result is suppressed; a value entry's result is printed. *)
let run_action image =
  let img = Pvm.Image.read_file image in
  let v = Pvm.Image.run ~host:Ffi.host img in
  if not img.Pvm.Image.is_effect then Stdlib.print_endline (Vm.Value.to_string v)

(* --- command-line surface ------------------------------------------------- *)

let demo_cmd =
  let open Cmdliner in
  Cmd.v
    (Cmd.info "demo" ~doc:"Run built-in sample programs on the CESK machine.")
    Term.(const run_all $ trace_arg)

let corefn_dir_arg =
  let open Cmdliner in
  Arg.(value & opt string "./output" & info [ "corefn-dir" ] ~docv:"DIR"
       ~doc:"Directory of `purs` CoreFn output (per-module subdirs).")

let output_arg =
  let open Cmdliner in
  Arg.(value & opt string "output-purvm" & info [ "output"; "o" ] ~docv:"DIR"
       ~doc:"Output directory: the image at its root, intermediates under _build/.")

let build_cmd =
  let open Cmdliner in
  let em = Arg.(value & opt string "Main" & info [ "entry-module"; "m" ] ~docv:"MODULE") in
  let e = Arg.(value & opt string "main" & info [ "entry"; "e" ] ~docv:"NAME") in
  let arg = Arg.(value & opt (some int) None & info [ "arg" ] ~docv:"INT"
                 ~doc:"Apply the entry to this Int (an Int -> Int entry).") in
  let value = Arg.(value & flag & info [ "value" ]
                   ~doc:"Entry is a plain value (do not apply); default treats it as Effect.") in
  Cmd.v
    (Cmd.info "build" ~doc:"Compile a CoreFn dir's modules (incrementally) and link an image.")
    Term.(const build_action $ corefn_dir_arg $ output_arg $ em $ e $ arg $ value)

let native_cmd =
  let open Cmdliner in
  let em = Arg.(value & opt string "Main" & info [ "entry-module"; "m" ] ~docv:"MODULE") in
  let e = Arg.(value & opt string "main" & info [ "entry"; "e" ] ~docv:"NAME") in
  let arg = Arg.(value & opt (some int) None & info [ "arg" ] ~docv:"INT"
                 ~doc:"Apply the entry to this Int (an Int -> Int entry).") in
  let value = Arg.(value & flag & info [ "value" ]
                   ~doc:"Entry is a plain value (do not apply); default treats it as Effect.") in
  let ulib = Arg.(value & opt (some string) None & info [ "ulib" ] ~docv:"DIR"
                  ~doc:"A ulib corefn dir of registry-package patches, overlaid over --corefn-dir (ADR-0038).") in
  Cmd.v
    (Cmd.info "native" ~doc:"Compile a CoreFn dir to a native executable via the OCaml backend (ocamlopt).")
    Term.(const native_action $ corefn_dir_arg $ output_arg $ em $ e $ arg $ value $ ulib)

let compile_cmd =
  let open Cmdliner in
  Cmd.v
    (Cmd.info "compile" ~doc:"Compile one module's corefn.json to .pvmo + .pvmi.")
    Term.(
      const compile_action
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"COREFN_JSON")
      $ output_arg)

let run_cmd =
  let open Cmdliner in
  Cmd.v
    (Cmd.info "run" ~doc:"Run a linked .pvm image.")
    Term.(const run_action $ Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE"))

let cmd =
  let open Cmdliner in
  Cmd.group
    (Cmd.info "purvm" ~version:"0.1.0" ~doc:"The PURVASM bytecode toolchain.")
    [ build_cmd; native_cmd; compile_cmd; run_cmd; demo_cmd ]

let () = Stdlib.exit (Cmdliner.Cmd.eval cmd)
