(** PURVASM bytecode VM entry point (ADR-0030 slice 1): run a lower-IR program (or,
    via the oracle bridge, a [Cesk.Ast.term]) on the stack interpreter.

    Start-up follows ADR-0030's eager construction: install every top-level function
    as a global closure first (so any CAF may reference any function), then evaluate
    the CAFs in dependency (spine) order. Recursion is closed by the global table
    (top-level) and by [Make_rec] (local groups). *)

module Value = Value
module Bytecode = Bytecode
module Codegen = Codegen
module Machine = Machine

(** Run a lower-IR (ANF) program to a value. *)
let eval_anf (program : Middle_end.Anf.expr) : Value.t =
  let gdefs, main = Codegen.program program in
  let globals : (string, Value.t) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (name, gdef) ->
      match gdef with
      | Codegen.Gfun (ps, body) ->
        Hashtbl.replace
          globals
          name
          (Value.Vclosure { Value.params = ps; body; env = ref Value.SMap.empty })
      | Codegen.Gcaf _ -> ())
    gdefs;
  List.iter
    (fun (name, gdef) ->
      match gdef with
      | Codegen.Gcaf chunk ->
        Hashtbl.replace globals name (Machine.run_chunk globals chunk Value.SMap.empty)
      | Codegen.Gfun _ -> ())
    gdefs;
  Machine.run_chunk globals main Value.SMap.empty

(** Run a [Cesk.Ast.term] by translating it to the lower IR (ADR-0025) and
    executing it on the VM — the bridge used by the differential check against the
    oracle. *)
let eval (t : Cesk.Ast.term) : Value.t = eval_anf (Middle_end.Transl.transl t)

(** Run an ANF program and also return the number of instructions executed — the
    deterministic VM cost metric (ADR-0030) reported by the benchmark harness. *)
let eval_anf_counted (program : Middle_end.Anf.expr) : Value.t * int =
  Machine.executed := 0;
  let v = eval_anf program in
  (v, !Machine.executed)
