# 0037. Self-hosting: reimplement purvasm in PureScript on the boot native backend

- Status: Accepted
- Date: 2026-06-25

## Context

`boot` (the OCaml implementation) now compiles PureScript to a native executable
(ADR-0035 strategy, ADR-0036 codegen) and is held VM/oracle-differential. Per the
bootstrap plan ([[boot-then-selfhost-bootstrap]]), `boot` is a *means*: the next phase
is to **reimplement purvasm in PureScript**, compile it with `boot`, and let that become
the real artifact. From there the bespoke native runtime (ADR-0035) and the real
optimiser investment ([[optimizer-roadmap]]) happen in the PureScript codebase, not in
throwaway OCaml.

FFI is **demand-driven** by what the PureScript compiler actually needs. The first needs
are *not* `Data.Array` (ADR-0036's coverage finding) but **file I/O, argv/stdout, and
process spawn** (to read `corefn.json`, write `.ml`, and invoke `ocamlopt`); JSON
parsing is plain PureScript.

## Decision

### Bootstrap stages

0. **`boot` compiles arbitrary PureScript → native.** Done (ADR-0035/0036).
1. **Write `purvasm-ps`** — purvasm reimplemented in PureScript — and compile it with
   `boot` (`purvm native`) into a native compiler executable.
2. **Validate** `purvasm-ps` against `boot`: on every fixture/benchmark it must emit the
   same OCaml / produce the same program behaviour as `boot`'s `purvm native` (the
   oracle discipline extends to the self-host).
3. **Fixpoint** — `purvasm-ps` compiles *its own* source; the self-compiled compiler
   matches the `boot`-compiled one. True self-hosting; `boot` can then be retired.

### Port order

Start with a **thin end-to-end vertical slice** — decode → lower → ANF (`transl`) →
`Codegen_ml` for a *small* language subset — validated against `boot`, then grow the
subset. Each stage is differentially checkable against `boot` via the IRs as serialised
data (CoreFn is already JSON; ANF/`Cesk.Ast` get a serialisation if stage-by-stage
validation needs it). **Only the code is rewritten** — the *design* (CoreFn AST
ADR-0014, lowering ADR-0015, ANF ADR-0025, the force model ADR-0034, value rep ADR-0036,
the optimiser passes) ports directly from the existing ADRs.

### FFI plan (demand-driven, minimal)

Added to the resolver as the PureScript compiler reaches for them, cheapest layer first:

- **File I/O / argv / stdout / process spawn** — effectful native leaves
  (`Ffi.host` + the `Rt` prelude, kept in sync; the differential enforces parity). The
  `ocamlopt` invocation is a process spawn (or a thin shell wrapper around the emitted
  `.ml`).
- **JSON parsing** — plain PureScript over a file read as a `String` (no FFI).
- **`Data.*` library FFI** — only when the compiler actually uses a function, as a
  **structural guest term** (resolver-inlined, runs on every backend — ADR-0036), not an
  `Rt`-only leaf.

### Validation

Differential against `boot` at every stage (emitted OCaml byte-equal where practical, or
program behaviour equal), reusing the standing oracle/VM/native discipline. Unresolved
foreigns already surface as clear `unbound foreign: k` errors (ADR-0036), so each new FFI
need is legible.

## Consequences

- A concrete path to self-hosting: a thin slice proves the loop early, then the subset
  grows until `purvasm-ps` compiles itself.
- The FFI surface grows only as the compiler needs it — file I/O first, never the whole
  `Data.*` eagerly.
- `boot`'s OCaml is confirmed throwaway; its *design* (the ADRs) is the durable asset.
- The bespoke native runtime and the optimiser build-out move into `purvasm-ps`.

## Alternatives considered

- **Big-bang rewrite** (whole compiler in PureScript, validate at the end). High risk —
  no early signal; rejected for the thin-slice-then-grow approach.
- **Keep extending `boot` (OCaml) instead of self-hosting.** Defeats the bootstrap goal;
  the durable compiler must be in PureScript. `boot` only needs to be good enough to
  compile `purvasm-ps`.
- **JSON/file FFI as a big host module up front.** Against the minimal-FFI policy; add
  each leaf on demand.
