(** Benchmark harness (ADR-0026): sweep each program over a range of input sizes
    and record deterministic oracle cost — [steps] (machine transitions to a value)
    and [allocs] (final store size; the store is monotonic pre-GC). Per (bench,
    size) the program is measured under each *variant* (currently the un-optimised
    [direct] link and the [anf] round-trip of ADR-0025); an optimiser pass adds one
    variant and one curve. Results are written as `.dat` files and plotted with
    gnuplot to `steps.png` / `allocs.png`.

    Not a test (it reports, it does not pass/fail). Run from the `boot` directory:
    [dune exec bench/bench.exe]; outputs land in `bench/out/` (override the input
    fixtures dir with argv.(1), the output dir with argv.(2)). *)

module C = Cesk.Ast
module V = Cesk.Value
module T = Middle_end.Transl

(* Drive the machine ourselves (ADR-0026: keep instrumentation out of the spec),
   counting transitions and reading the final store size. *)
let measure ?(host = Cesk.Machine.no_host) (t : C.term) : int * int =
  let rec go steps (s : Cesk.Machine.state) =
    match Cesk.Machine.step ~host s with
    | Cesk.Machine.Done _ -> steps, Cesk.Store.size s.store
    | Cesk.Machine.Step s' -> go (steps + 1) s'
  in
  go 0 (Cesk.Machine.inject t)

(* The VM (ADR-0030) is the real runtime, so it carries its own cost metrics: the
   deterministic count of instructions executed and wall-clock. The oracle's
   step/alloc are now the *proxy* this replaces. Each VM variant is an ANF transform
   (the VM runs the lower IR directly), so the optimiser passes' effect is visible in
   true VM-instruction terms — the decision the oracle proxy could not make. *)
let vm_variants : (string * (C.term -> Middle_end.Anf.expr)) list =
  [ ("anf", fun t -> T.transl t)
  ; ("dictelim", fun t -> Middle_end.Passes.Dict_elim.run (T.transl t))
  ; ( "opt"
    , fun t ->
        Middle_end.Passes.Simplify.run (Middle_end.Passes.Dict_elim.run (T.transl t)) )
  ]

(* Instructions executed (deterministic), wall-clock milliseconds, and the result
   value for one VM run. *)
let vm_measure (program : Middle_end.Anf.expr) : Vm.Value.t * int * float =
  let t0 = Unix.gettimeofday () in
  let v, instrs = Vm.eval_anf_counted program in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  (v, instrs, ms)

(* ADR-0030 requires the VM to agree with the oracle on every benchmark, not just
   the e2e fixtures. Tracked here and reported at the end of the sweep. *)
let mismatches = ref 0

(* Variants: each is a label and a term-to-term transform measured on the oracle.
   The baseline pair today; an optimiser pass appends one entry here (and a curve
   to every plot) — e.g. ["dictelim", fun t -> rev_transl (dict_elim (transl t))]. *)
let variants : (string * (C.term -> C.term)) list =
  [ ("direct", fun t -> t)
  ; ("anf", fun t -> Middle_end.Transl.rev_transl (Middle_end.Transl.transl t))
  ; ( "dictelim"
    , fun t ->
        Middle_end.Transl.rev_transl
          (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl t)) )
  ; ( "opt"
    , fun t ->
        Middle_end.Transl.rev_transl
          (Middle_end.Passes.Simplify.run
             (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl t))) )
  ]

(* label, entry module, entry (an `Int -> Int`), and the input sizes to sweep. *)
let benches =
  [ "fib", [ "Fib" ], "run", [ 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 22 ]
  ; "quicksort", [ "Quicksort" ], "run", [ 10; 20; 30; 40; 50; 60; 80; 100; 120 ]
  ; "n-queens", [ "NQueens" ], "run", [ 4; 5; 6; 7 ]
  ; "bintree-dfs", [ "BinTree" ], "runDfs", [ 4; 6; 8; 10; 12; 14 ]
  ; "bintree-bfs", [ "BinTree" ], "runBfs", [ 4; 5; 6; 7; 8; 9; 10 ]
  ; "map-fold", [ "MapFold" ], "run", [ 200; 400; 600; 800; 1000; 1500; 2000 ]
  ]

let write_file (path : string) (contents : string) : unit =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

(* The gnuplot script: for each metric a multiplot page, one panel per bench, one
   curve per variant. Column layout of `<bench>.dat`: size, then (steps, allocs)
   per variant in order — so variant i (1-based) is steps@col 2i, allocs@col 2i+1. *)
let gnuplot_script () : string =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  add "set datafile separator whitespace\n";
  add "set terminal pngcairo size 1280,800\n";
  add "set key left top\n";
  add "set grid\n";
  let page metric vars ~log col_of =
    add (if log then "set logscale y\n" else "unset logscale y\n");
    add (Printf.sprintf "set output '%s.png'\n" metric);
    add
      (Printf.sprintf
         "set multiplot layout 2,3 title 'purvasm benchmark — %s vs input size (ADR-0026/0030)'\n"
         metric);
    List.iter
      (fun (label, _, _, _) ->
         add (Printf.sprintf "set title '%s'\n" label);
         add "set xlabel 'input size'\n";
         add (Printf.sprintf "set ylabel '%s'\n" metric);
         let plots =
           List.mapi
             (fun i (vname, _) ->
                Printf.sprintf
                  "'%s.dat' using 1:%d with linespoints title '%s'"
                  label
                  (col_of i)
                  vname)
             vars
         in
         add ("plot " ^ String.concat ", " plots ^ "\n"))
      benches;
    add "unset multiplot\n"
  in
  (* Oracle proxy: variant i (0-based) has steps at col 2i+2, allocs at 2i+3. *)
  page "steps" variants ~log:true (fun i -> (2 * i) + 2);
  page "allocs" variants ~log:true (fun i -> (2 * i) + 3);
  (* VM metrics (ADR-0030): appended after the oracle columns. With [nv] oracle
     variants the VM block starts at col 2*nv+2; VM variant j has v_instrs there and
     v_ms one to the right. *)
  let vm_base = (2 * List.length variants) + 2 in
  page "vm_instrs" vm_variants ~log:true (fun j -> vm_base + (2 * j));
  page "vm_ms" vm_variants ~log:false (fun j -> vm_base + (2 * j) + 1);
  Buffer.contents buf

let () =
  let fixtures = if Array.length Sys.argv > 1 then Sys.argv.(1) else "bench/fixtures" in
  let outdir = if Array.length Sys.argv > 2 then Sys.argv.(2) else "bench/out" in
  (try Unix.mkdir outdir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.printf "purvasm benchmark sweep (ADR-0026) — deterministic oracle cost\n";
  Printf.printf "fixtures: %s   output: %s\n\n" fixtures outdir;
  let comparisons = ref [] (* (label, (opt tree instrs, opt naive instrs)) at max size *) in
  List.iter
    (fun (label, entry_module, entry, sizes) ->
       (* The entry is an `Int -> Int`; link it once and apply each swept size. *)
       let base =
         Link.link_program ~resolver:Ffi.resolver ~outdir:fixtures ~entry_module ~entry ()
       in
       let buf = Buffer.create 256 in
       let header =
         let oracle =
           List.fold_left
             (fun s (v, _) -> s ^ Printf.sprintf "  %s_steps  %s_allocs" v v)
             "# size"
             variants
         in
         let vm =
           List.fold_left
             (fun s (v, _) -> s ^ Printf.sprintf "  %s_vinstrs  %s_vms" v v)
             oracle
             vm_variants
         in
         vm ^ "  opt_naive_vinstrs"
       in
       Buffer.add_string buf (header ^ "\n");
       let last = ref (0, 0) in
       let last_cmp = ref (0, 0) (* (opt tree instrs, opt naive instrs) at last size *) in
       List.iter
         (fun n ->
            let term = C.App (base, C.Lit (C.LInt n)) in
            Buffer.add_string buf (string_of_int n);
            List.iter
              (fun (vname, transform) ->
                 let steps, allocs = measure ~host:Ffi.host (transform term) in
                 if String.equal vname "anf" then last := steps, allocs;
                 Buffer.add_string buf (Printf.sprintf "  %d  %d" steps allocs))
              variants;
            (* Differential check (ADR-0030): the VM result must match the oracle. *)
            let oracle = V.to_string (Cesk.Machine.eval ~host:Ffi.host term) in
            let opt_tree = ref 0 in
            List.iter
              (fun (vname, transform) ->
                 let v, instrs, ms = vm_measure (transform term) in
                 if String.equal vname "opt" then opt_tree := instrs;
                 if not (String.equal (Vm.Value.to_string v) oracle)
                 then (
                   incr mismatches;
                   Printf.printf
                     "  !! VM MISMATCH %s/%s n=%d: vm=%s oracle=%s\n"
                     label
                     vname
                     n
                     (Vm.Value.to_string v)
                     oracle);
                 Buffer.add_string buf (Printf.sprintf "  %d  %.3f" instrs ms))
              vm_variants;
            (* ADR-0031 measurement: recompile the opt pipeline with the naive
               explicit matcher and re-run, to quantify the decision tree's win. *)
            let opt_anf =
              Middle_end.Passes.Simplify.run
                (Middle_end.Passes.Dict_elim.run (T.transl term))
            in
            let nv, opt_naive = Vm.eval_anf_counted ~naive:true opt_anf in
            if not (String.equal (Vm.Value.to_string nv) oracle)
            then (
              incr mismatches;
              Printf.printf
                "  !! VM MISMATCH %s/opt-naive n=%d: vm=%s oracle=%s\n"
                label
                n
                (Vm.Value.to_string nv)
                oracle);
            last_cmp := (!opt_tree, opt_naive);
            Buffer.add_string buf (Printf.sprintf "  %d" opt_naive);
            Buffer.add_char buf '\n')
         sizes;
       comparisons := (label, !last_cmp) :: !comparisons;
       write_file (Filename.concat outdir (label ^ ".dat")) (Buffer.contents buf);
       let n = List.nth sizes (List.length sizes - 1) in
       let steps, allocs = !last in
       Printf.printf
         "%-12s n=%-5d  anf: steps=%-10d allocs=%-9d  -> %s.dat\n"
         label
         n
         steps
         allocs
         label)
    benches;
  write_file (Filename.concat outdir "plot.gp") (gnuplot_script ());
  let rc =
    Sys.command (Printf.sprintf "cd %s && gnuplot plot.gp" (Filename.quote outdir))
  in
  if !mismatches = 0
  then Printf.printf "\nVM differential: all benchmarks agree with the oracle\n"
  else Printf.printf "\nVM differential: %d MISMATCH(es) — see above\n" !mismatches;
  (* ADR-0031: decision tree vs naive explicit matcher, opt pipeline, at max size. *)
  Printf.printf "\ndecision tree vs naive matcher (opt, vm_instrs @ max size):\n";
  List.iter
    (fun (label, (tree, naive)) ->
       Printf.printf
         "  %-12s tree=%-10d naive=%-10d  tree/naive=%.3f\n"
         label
         tree
         naive
         (if naive = 0 then 1.0 else float_of_int tree /. float_of_int naive))
    (List.rev !comparisons);
  if rc = 0
  then
    Printf.printf
      "wrote %s/{steps,allocs,vm_instrs,vm_ms}.png\n"
      outdir
  else
    Printf.printf
      "\n\
       .dat and plot.gp written to %s; gnuplot exited %d (run `gnuplot plot.gp` there)\n"
      outdir
      rc
