# 0032. PURVASM bytecode VM (slice 2): native foreign rung and Effect

- Status: Accepted
- Date: 2026-06-22

## Context

The bytecode VM (ADR-0030, ADR-0031) runs the **pure** core: every pure fixture and
benchmark agrees with the oracle. It explicitly deferred the **native foreign rung**
and **`Effect`** — in slice 1 a foreign reference (`AForeign`) compiles to a load
that is stuck if forced, so any program reaching a native leaf cannot run on the VM.

`Effect` is *not* an algebraic-effect system here. ADR-0023 settled its model: an
`Effect a` is a **thunk** (a function value), the monad combinators
(`pureE`/`bindE`/`untilE`/…) and `Effect.Ref` are **structural guest terms**
(ADR-0020; `Ref` is a one-cell mutable array, ADR-0019), and the only genuinely
effectful leaf is `Effect.Console.log`, a **native foreign** whose thunk, when
forced, performs the IO — *"the thunk's force is the Perform."* So nothing about
`Effect` needs new control machinery; what the VM is missing is the native rung that
ADR-0022 already gives the oracle. The native rung is **first-order** (higher-order
foreigns are structural guest code, ADR-0020), and `Ffi.host` is the single source of
truth for which names are native (`Cesk.Value.t list -> Cesk.Value.t`).

This slice adds that rung to the VM and brings `Effect` (and the pure `show` leaves)
onto it, without breaking the oracle's effect-soundness contract.

## Decision

Give the VM a native foreign rung mirroring the oracle's, reusing `Ffi.host`, and run
`Effect` exactly as the oracle does.

- **VM `Vforeign` value + eval/apply.** Add a value form `Vforeign { name; arity;
  args; call }` mirroring `Cesk.Value.VForeign`. It collects arguments like an
  under-applied constructor (ADR-0030's eval/apply); on reaching `arity` it invokes
  `call`. A foreign-reference instruction materialises a `Vforeign` for a `Foreign`
  leaf by looking the name up in the host registry passed to `Vm.eval ~host`
  (replacing slice 1's stuck `Load`).
- **Reuse `Ffi.host` via a boundary conversion.** The host stays the one source of
  truth. At a foreign call the VM converts its argument values to oracle values,
  calls the host `call`, and converts the result back. The conversion is total over
  what first-order leaves touch — scalars / strings / arrays / data; an oracle
  `VForeign` returned by a leaf (e.g. `log`'s `#perform` thunk) is re-wrapped as a VM
  `Vforeign` whose `call` carries the same conversion. A VM closure is never passed
  to, nor an oracle `VClosure` ever returned from, a first-order leaf — if one ever
  crosses, it is stuck.
- **`Effect` runs with no new machinery (ADR-0023).** The combinators and `Effect.Ref`
  are guest terms the VM already executes (Ref over the array builders the VM has,
  ADR-0019); `Effect.Console.log` is the lone native leaf. A `Vm.run_effect ~host`
  driver evaluates `main` and applies it to `unit` (the immediate `0`, ADR-0017),
  exactly as `Cesk.Machine.run_effect`; effects fire as native calls during that
  evaluation.
- **Effect-soundness preserved by construction.** Sequencing comes from strict
  evaluation of the *same* `bindE` guest term the oracle uses; the only effect site
  is the saturating application of a native leaf, performed exactly once when forced.
  So ADR-0023's contract — *effects in program order; each `Perform` runs exactly
  once, never eliminated, never duplicated* — holds by the identical argument as the
  oracle, and is checked differentially (value **and** stdout order).
- **Recursive `Effect` instance dictionaries: eager construction, knot-tying as
  fallback (ADR-0030).** Under uncurried eval/apply the cycle-closing references are
  superclass *thunks* and under-applied dictionary methods (PAPs that do not run
  their body), so the construction graph is a DAG and the existing global-table +
  `Make_rec` construction should build the `Functor↔Apply↔…↔Monad` group with no
  by-need. This is verified on the effect fixtures; if a genuine *forced* top-level
  value cycle appears, the fallback is **top-level knot-tying** (allocate a global
  cell, backpatch) — the global analogue of `Make_rec`, not the oracle's by-need
  (ADR-0024).

## Consequences

- The VM runs **every program the oracle does** (pure + `Effect`); slice 1's
  stuck-on-`AForeign` limitation is gone.
- The pure `Data.Show.*` leaves light up on the VM too (same rung), so the VM
  differential extends to the `show` fixtures.
- `Ffi.host` remains the single source of truth; the only new coupling is a thin VM↔
  oracle value conversion confined to the foreign call boundary (ADR-0030 otherwise
  keeps the two value types separate).
- The VM value type gains `Vforeign`; `run_effect` is the second VM entry point
  beside `eval`.
- The benchmark suite is pure (`Int → Int`) and unchanged; effects stay
  correctness-tested (differential, with stdout capture), not perf-swept.

## Alternatives considered

- **An algebraic-effects `Perform`/handler node.** Rejected: PureScript `Effect` is
  thunked IO, not algebraic effects (ADR-0023); a `Perform` node adds machinery the
  language never uses and diverges from the oracle. The native foreign call already
  *is* the `Perform`.
- **A VM-native host registry (host functions over VM values).** Rejected for now:
  it duplicates `Ffi.host`; boundary conversion reuses the one source of truth, and
  first-order leaves never need access to a guest closure. Revisit only if a leaf
  must manipulate VM closures (none do — higher-order foreigns are guest code).
- **Share `Cesk.Value` as the VM value type to avoid conversion.** Rejected:
  ADR-0030 keeps the VM value separate to keep the oracle clean; the conversion is
  small and confined to the foreign boundary.
- **By-need recursion in the VM for the `Effect` dictionaries.** Rejected: ADR-0030
  chose eager construction + knot-tying; the dictionary cycle is a construction DAG
  under uncurried eval/apply, and knot-tying is the fallback for a real value cycle —
  by-need stays the oracle's mechanism (ADR-0024).
