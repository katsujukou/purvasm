(** PURVASM bytecode VM entry point (ADR-0030 slice 1, ADR-0032 slice 2): run a
    lower-IR program (or, via the oracle bridge, a [Cesk.Ast.term]) on the stack
    interpreter, with native foreign leaves resolved through the oracle host registry
    (ADR-0022) at the boundary [Foreign] wraps.

    Start-up follows ADR-0030's eager construction: install every top-level function
    as a global closure first (so any CAF may reference any function), then evaluate
    the CAFs in dependency (spine) order. Recursion is closed by the global table
    (top-level) and by [Make_rec] (local groups). *)

(* [Value] is re-exported so callers can name the result type ([Vm.Value.t]); the
   other submodules ([Bytecode]/[Codegen]/[Match_compile]/[Machine]/[Foreign]) are
   internal. *)
module Value = Value

(** Run a lower-IR (ANF) program to a value, resolving native foreigns through [host]
    (default: none, for the pure core). *)
let eval_anf ?(host = Cesk.Machine.no_host) (program : Middle_end.Anf.expr) : Value.t =
  let foreigns = Foreign.lookup host in
  let gdefs, main = Codegen.program program in
  let globals : (string, Value.t) Hashtbl.t = Hashtbl.create 64 in
  (* Pass 1: install every top-level function as a global closure (it resolves the
     rest of the table at call time, so order is irrelevant). *)
  List.iter
    (fun (name, gdef) ->
      match gdef with
      | Codegen.Gfun (ps, body) ->
        Hashtbl.replace
          globals
          name
          (Value.Vclosure { Value.params = ps; body; env = ref Value.SMap.empty })
      | Codegen.Gcaf _ | Codegen.Grec _ -> ())
    gdefs;
  (* Pass 2: publish a by-need cell for every recursive-group value, *before* building
     any, so a cyclic instance-dictionary group can refer to siblings not yet built
     (ADR-0024's by-need, realised in the VM, ADR-0032). A cell builds its value on
     first force and memoises; a genuine self-forcing cycle hits [Building]. *)
  List.iter
    (fun (name, gdef) ->
      match gdef with
      | Codegen.Grec chunk ->
        Hashtbl.replace
          globals
          name
          (Value.Vindirect
             (ref
                (Value.Unbuilt
                   (fun () ->
                     Machine.run_chunk ~foreigns globals chunk Value.SMap.empty))))
      | Codegen.Gfun _ | Codegen.Gcaf _ -> ())
    gdefs;
  (* Pass 3: build the non-recursive CAFs strictly, in dependency (spine) order — each
     reduces to a value, exactly as the oracle's strict `let`. *)
  List.iter
    (fun (name, gdef) ->
      match gdef with
      | Codegen.Gcaf chunk ->
        Hashtbl.replace
          globals
          name
          (Machine.run_chunk ~foreigns globals chunk Value.SMap.empty)
      | Codegen.Gfun _ | Codegen.Grec _ -> ())
    gdefs;
  Machine.run_chunk ~foreigns globals main Value.SMap.empty

(** Run a [Cesk.Ast.term] by translating it to the lower IR (ADR-0025) and executing
    it on the VM — the bridge used by the differential check against the oracle. *)
let eval ?(host = Cesk.Machine.no_host) (t : Cesk.Ast.term) : Value.t =
  eval_anf ~host (Middle_end.Transl.transl t)

(** Run a program's [main : Effect a] (ADR-0023/0032): force the `Effect` thunk by
    applying it to [unit] (the immediate `0`, ADR-0017), exactly as
    [Cesk.Machine.run_effect]; effects fire as native calls during evaluation. *)
let run_effect ?(host = Cesk.Machine.no_host) (main : Cesk.Ast.term) : Value.t =
  eval ~host (Cesk.Ast.App (main, Cesk.Ast.Lit (Cesk.Ast.LInt 0)))

(** Run an ANF program and also return the number of instructions executed — the
    deterministic VM cost metric (ADR-0030) reported by the benchmark harness.
    [naive] selects the naive explicit matcher over the decision tree (ADR-0031), so
    the harness can measure the tree's win without reaching into the matcher's
    internal flag; it is saved and restored around the run. *)
let eval_anf_counted ?(naive = false) (program : Middle_end.Anf.expr) : Value.t * int =
  let saved = !Match_compile.use_naive_matching in
  Match_compile.use_naive_matching := naive;
  Fun.protect
    ~finally:(fun () -> Match_compile.use_naive_matching := saved)
    (fun () ->
      Machine.executed := 0;
      let v = eval_anf program in
      (v, !Machine.executed))
