# 0001. Implement the phase-1 host in OCaml; reject a PureScript-on-V8 seed

- Status: Accepted
- Date: 2026-06-16

## Context

The project's goal is a production-grade abstract machine that executes
PureScript with true multicore-native parallel async. The agreed delivery is a
three-phase self-hosting bootstrap:

1. Implement the abstract machine and its bytecode in a host language.
2. Implement a bytecode→native compiler. PureScript then effectively has a
   native backend (CoreFn→bytecode→native).
3. Reimplement the machine in PureScript and compile it to native with the
   phase-2 compiler (self-host).

This record decides the **host language for phase 1**.

A native, self-sufficient PureScript machine does not exist yet — that is what
we are building — so the first version cannot host itself. Bootstrapping always
needs a pre-existing foreign *seed*. The truly impossible option is "pure
PureScript, no host at all"; every other option is on the table.

## Decision

Use **OCaml** as the phase-1 host language. The OCaml implementation lives in
`boot/`.

A consequence we adopt as a standing constraint: memory is **two-level**.

- *Host* memory (OCaml's own GC) manages the interpreter's own data structures.
- *Guest* memory (the running PureScript program's values) is **ours** to
  represent and collect.

Because phases 2 and 3 emit standalone native code that cannot depend on
OCaml's runtime, the **guest runtime — value representation/ABI, GC, scheduler,
effect handlers — must be a language-neutral artifact we own**, shared by the
OCaml interpreter (phase 1), the native codegen (phase 2), and the self-hosted
PureScript machine (phase 3). OCaml 5's domains/effects/GC may be used as a
*throwaway prototyping shortcut* for phase-1 semantics, but are not the
destination for guest values.

## Consequences

- OCaml is the canonical ML for writing compilers, and `ocamlopt`/`Bigarray`/C
  FFI/Unix threads let us build and **test native runtime concerns natively
  from day one**, rather than blind until a backend exists.
- OCaml 5 is a *faithful reference* for the target concurrency model (shared-
  memory domains + effect handlers + multicore GC), which is exactly what we
  are designing.
- The seed (OCaml) and the eventual self-host (PureScript) being **different
  languages** makes phase 3 a meaningful milestone and gives a differential-
  testing oracle: two independent implementations of one machine spec.
- Toolchain cost: the devShell must carry both OCaml (5.3 + dune + alcotest +
  cmdliner + base + js_of_ocaml/wasm_of_ocaml) and the PureScript tooling
  (needed for CoreFn input and phase 3). This is already provisioned.
- We must consciously avoid letting guest-value semantics lean on OCaml's
  runtime, or phase 2 would require a rewrite.

## Alternatives considered

- **PureScript-on-V8 (existing JS backend) as the seed.** *Not impossible* —
  the official PureScript→JS backend runs PureScript today, a bytecode→native
  compiler is pure data transformation that runs fine on V8, and the runtime
  bootstraps as more PureScript over a tiny native intrinsic layer. It is even
  *more elegant*: the machine is written once in PureScript and self-compiled,
  collapsing the phase-3 rewrite. Rejected on **engineering-risk** grounds, not
  feasibility: (a) the flagship feature — true shared-memory multicore + effect
  handlers — would be prototyped on V8, whose Workers are shared-nothing and
  whose `SharedArrayBuffer` shares only raw bytes, an *unfaithful* model of the
  native target, risking semantics that do not carry over; (b) a long "blind
  bring-up" where the native runtime cannot be exercised until the compiler
  works; (c) low-level runtime/codegen work is awkward on V8. Seeding on V8 also
  does not even achieve "purity" — it depends on V8, a foreign runtime.
- **Rust / Zig / C as the seed.** Equally valid native-capable seeds. OCaml was
  chosen for ML compiler-writing ergonomics and the OCaml 5 concurrency
  reference; this is a preference, not a forced choice.
- **Target an existing multicore runtime (BEAM / OCaml 5 / WASM-GC) for guest
  values.** Inherits a mature GC and scheduler (purerl is a precedent), but
  surrenders the control needed for a bespoke per-fiber-heap model and an owned,
  portable guest ABI; it also does not survive into phases 2–3.
