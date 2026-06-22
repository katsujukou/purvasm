# 0030. PURVASM bytecode VM (slice 1): instruction set, ANF codegen, stack interpreter

- Status: Accepted
- Date: 2026-06-22

> **Progress (2026-06-22).** Implemented in `boot/lib/vm/` (`bytecode.ml`,
> `value.ml`, `codegen.ml`, `machine.ml`, `vm.ml`): postorder ANF→bytecode codegen
> with relative jumps; an explicit operand/frame-stack interpreter with eval/apply,
> distinct `Call`/`Tail_call` (TCE), over-application via an `Apply_more`
> continuation, and recursive groups by knot-tying (top-level through the global
> table, local through a shared env ref). Unconditional `case` uses a host-side
> `Match`; guarded `case` (ADR-0013) compiles to a `Test`/guard chain. Differential
> equivalence against the oracle holds for every pure fixture (e2e `vm` group, 18
> cases) and every benchmark at every swept size; the harness reports `vm_instrs`
> (deterministic) and `vm_ms` (wall-clock), with the optimiser's effect now visible
> in VM-instruction terms. Native foreign / `Effect` remain out of slice 1.

The lower IR is in place (ANF, ADR-0025) and the bytecode shape is decided (a
stack machine, ADR-0003). What is missing is the runtime itself: the bytecode, the
`ANF → bytecode` codegen, and an interpreter. Until it exists, optimiser passes
can only be judged by the CESK-oracle step/alloc *proxy* (ADR-0026) which — as the
decision-tree analysis showed — does not reflect the eventual runtime for codegen
optimisations. The VM is the first stage where that proxy is replaced by
ground truth, and where the deferred VM-stage optimisations (decision tree, TCE)
become measurable.

This ADR scopes the **first slice**: the **pure core** of the language. Native
foreigns and `Effect` (ADR-0022/0023) are a follow-up slice; all current
benchmarks are pure, so slice 1 already measures them.

## Decision

Build a stack-based bytecode VM for the pure ANF core, validated against the
oracle.

- **Codegen `ANF → bytecode` by postorder emit** (ADR-0003): walk the ANF tree
  operands-first, so each subexpression leaves exactly one value on the operand
  stack and the operand-stack height is statically known, empty at statement
  boundaries. Covers the pure ANF nodes: atoms, `let`, recursive groups,
  saturated application (eval/apply), `prim`, `if`, `case`, constructor / record
  / array construction, accessor, update.
- **Calling convention: eval/apply, uncurried (ADR-0025), with TCE.** Distinct
  instructions for a **tail call** (reuse the current frame) and a non-tail call
  (push a return frame); over-/under-application handled by the eval/apply
  protocol (saturate, apply-the-rest, or build a partial application).
- **Stack-safe by construction.** The interpreter is an explicit state machine
  over a **heap-allocated frame stack and operand stack** — it never uses the OCaml
  call stack for a guest call — so deep guest recursion cannot overflow the host,
  matching the oracle's heap-continuation stack safety (ADR-0002). Proper tail
  calls keep the frame stack bounded.
- **Recursive bindings: topological-sort eager construction, not by-need.** Under
  uncurried eval/apply the eager construction graph is a DAG (the cycle-closing
  references are superclass *thunks* — lazy edges — and an under-applied
  dictionary method is a partial application that does not run its body), so CAFs
  are initialised in dependency order; a genuine value cycle uses knot-tying
  (allocate-then-backpatch). This is the lazy-free scheme ADR-0024 named as the
  successor to its oracle-only by-need ("degrade to topological-sort plus
  knot-tying once the lower IR has uncurried calling"), and it is faithful to the
  native target (no per-CAF thunk/force in the cost model). (Genuine cyclic
  dictionaries appear with `Effect` in the follow-up slice; slice 1's instance
  graphs are acyclic chains.)
- **VM-specific value representation.** Scalars and constructor/record/array data
  mirror the oracle's; a closure is a *code pointer plus captured environment*
  (not an `Ast.term`). The oracle's `Cesk.Value` is left untouched; for the
  differential check a VM value is rendered to the same printed form as the
  oracle's result.
- **Verification and measurement.** Differential equivalence against the oracle:
  for every pure fixture and benchmark, the VM's result equals `Cesk.Machine.eval`.
  The benchmark harness gains a VM measurement — **executed instruction count
  (deterministic)** and **wall-clock** (the VM is the first stage where wall-clock
  is meaningful, ADR-0026) — reported alongside the oracle columns.

## Consequences

- The "real runtime" execution path begins; the CESK machine remains the spec and
  the differential reference, exactly as ADR-0025's round-trip intended.
- The deferred codegen-stage optimisations (decision tree, and re-measuring
  DictElim / inlining) become measurable in VM terms — unblocking the
  optimisation work that the oracle proxy could not evaluate.
- By-need is dropped *in the VM* (native-faithful); ADR-0024's by-need remains the
  oracle's mechanism. The two still agree on values (a pure CAF has the same value
  whether built eagerly or on demand).
- Effects/foreign are explicitly out of slice 1; the effect fixtures stay on the
  oracle until the follow-up slice adds the native rung to the VM.

## Alternatives considered

- **By-need in the VM** (mirror the oracle). Simplest path to parity, but it
  carries the oracle-only laziness into the runtime cost model, diverging from
  native; the uncurried VM gets the construction DAG for free, so lazy-free is both
  correct and faithful.
- **Reuse `Cesk.Value` for VM values.** Couples the spec's value type to VM
  closure representation; a VM-specific type keeps the oracle clean. (Sharing the
  value type to reuse the foreign `host` registry is a slice-2 concern.)
- **Interpret bytecode with host recursion.** Simpler dispatch, but deep guest
  recursion overflows the OCaml stack; an explicit heap stack + TCE is needed for
  the stack safety the oracle already guarantees.
- **Skip bytecode; add a second ANF tree-walker.** Would duplicate the oracle
  without advancing toward native; ADR-0003 committed to bytecode, and the bytecode
  VM is the path to phase-2 native (abstract stack simulation → SSA).
- **Register machine.** Settled against by ADR-0003 (stack machine); not
  re-litigated here.
