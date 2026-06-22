# 0033. Separate compilation: per-module artifacts (`.pvmo`/`.pvmi`), linker, and the `purvm` executable

- Status: Accepted
- Date: 2026-06-22

## Context

The VM runs every program the oracle does (ADR-0030/0031/0032), but only as an
*in-memory* pipeline: `Link.link_program` re-reads the entry's whole `corefn.json`
closure, lowers and links it into one `Cesk.Ast` spine, and the test/bench harness
evaluates it (ADR-0016). There is no reusable artifact, no separate compilation, and
no executable to ship — every run recompiles the world.

The goal is an ahead-of-time toolchain: compile each module **once** into a reusable
artifact, **link** the reachable artifacts into a single image, and **run** that
image with a `purvm` executable. Two enablers are already in place: the VM resolves
globals by **qualified name** (so cross-module linking is a name-keyed *merge*, no
relocation), and the optimiser passes are **module-local / dependency-directed**
([[optimizer-modular-not-whole-program]]) — a constraint adopted precisely for this.

Separate compilation is also where `purs-wasm` was bitten (sidenotes 0001/0003/0005):
a worker optimising a module against its **own stale interface** (self-pollution),
**over-export** keeping dead code alive for compilation, and **dead-code masking**
(a whole-program-dead pathology becoming live in a single over-exporting module). The
design bakes in those lessons.

## Decision

A three-stage toolchain over per-module artifacts, reusing all existing lowering,
codegen, and VM machinery.

- **Compile (`module → .pvmo + .pvmi`).** Lower a module's `corefn.json` (ADR-0015),
  ANF (ADR-0025), optionally run the **module-local** passes (ADR-0027/0028), codegen
  to bytecode (ADR-0030/0031). Emit two artifacts:
  - **`.pvmo`** — the compiled object: the module's top-level definitions as bytecode
    `gdef`s (`Gfun`/`Gcaf`/`Grec`) keyed by **qualified name**, with cross-module and
    foreign references left as names (resolved at link/run).
  - **`.pvmi`** — the *interface*: the module's exported symbols (name → arity / kind:
    function · CAF · recursive-CAF), its import list, and a **content hash**. It is
    the unit of recompilation avoidance and is **extensible** to carry the
    optimisation summary a future cross-module optimiser needs (small-body inline
    templates, purity/effect classification) — see Scope.
- **Link (`artifacts → image .pvm`).** From the entry, take the transitive closure of
  reachable `.pvmo`s, run **reachability DCE over the name graph** (ADR-0021), and
  **merge** the surviving `(name → gdef)` maps into a single image plus the entry
  descriptor. Because globals are name-keyed, linking is a merge with a duplicate /
  missing-symbol check — refining ADR-0016's whole-program link.
- **Run (`purvm image`).** `purvm` deserialises the image, builds the global table
  (install function closures; publish `Grec` by-need cells; build `Gcaf`s strictly in
  dependency order — ADR-0032), wires its **built-in host registry** (`Ffi.host`,
  ADR-0022) for native leaves, and runs the entry (`eval` or `run_effect`).
- **Structural foreigns** (`arrayMap`, the `Effect` combinators, …, ADR-0020) become a
  **shared runtime module** compiled once and referenced by name — not inlined per
  module — so they are not duplicated across artifacts. Native leaves stay by-name
  through the host.
- **Recompilation avoidance.** Track per module `(source-hash, consumed-`.pvmi`
  -hashes)`. A module is recompiled iff its source changed **or** an interface it
  consumed changed; the image is relinked iff any reachable `.pvmo` changed. The
  guarantee the user named: *if M changes but `M.pvmi` is unchanged, M's downstream
  dependents are not recompiled.* Three soundness rules from the `purs-wasm`
  experience are invariants here:
  1. **No self-pollution** — a module's compilation never consumes its own `.pvmi`
     (only its dependencies').
  2. **Interface completeness** — `.pvmi` must be a *superset* of everything a
     dependent's compilation consumes; otherwise a stable interface can hide a change
     that should have triggered a rebuild (stale, wrong output).
  3. **Hash stability** — `.pvmi`'s hash changes iff downstream-relevant content
     changes, so an edit that does not affect dependents does not cascade.
- **Serialization is explicit and versioned**, not OCaml `Marshal` (brittle and
  unsafe across compiler/struct versions); bytecode is first-order data, so an
  explicit reader/writer with a format-version tag is straightforward and stable.
- **Validation.** Differential equivalence: for every fixture and benchmark,
  `purvm`-running the linked image equals the current in-memory `Link.link_program` +
  `Vm.eval` / `run_effect` (value **and** stdout order). The image path must not
  change any observable result.

## Scope

- **In (slice 1):** the `.pvmo`/`.pvmi` formats, a per-module compile driver, the
  name-merge + DCE linker, the `purvm` runner, the shared runtime module, explicit
  serialization, and recompilation avoidance driven by `.pvmi` hashes (with the three
  soundness invariants). Optimisation, when run, is **module-local only**.
- **Deferred:** **cross-module, summary-driven optimisation** (the `.pvmi` carrying
  inline templates / effect classification that dependents inline against) — this is
  where the `purs-wasm` pitfalls concentrate, and it is the point of the deferred
  general inliner ([[general-inliner-study-first]]). User-defined FFI plugins (the
  host is built into `purvm` for now). Native (phase-2) codegen is unrelated
  (ADR-0003).

## Consequences

- purvasm becomes a real AOT toolchain (`compile` → `link` → `purvm`), not a
  test-driven in-memory evaluator; artifacts are reusable and rebuilds incremental.
- Refines ADR-0016 (link = artifact merge over name-keyed globals) and applies
  ADR-0021's DCE at the artifact/name-graph level; the modular-optimiser constraint
  ([[optimizer-modular-not-whole-program]]) pays off as true separate compilation.
- A new on-disk format (`.pvmo`/`.pvmi`/`.pvm`) must be versioned and maintained.
- Establishes the interface-file machinery so that, when cross-module optimisation
  lands, a stable `.pvmi` already short-circuits downstream recompilation — without
  re-importing `purs-wasm`'s self-pollution / over-export / masking bugs.

## Alternatives considered

- **Keep the whole-program in-memory link.** No reuse, no incrementality, no shippable
  artifact; recompiles the world every run — the very thing this removes.
- **ANF-granularity artifacts (link, then codegen the whole program).** Not true
  separate compilation — codegen stays whole-program; the name-keyed-globals merge at
  the *bytecode* level is cleaner and makes `.pvmo`s directly linkable.
- **`Marshal` for serialization.** Fast to write but brittle/unsafe across versions; a
  shippable artifact format needs an explicit, versioned reader/writer.
- **Inline structural foreigns per module.** Simpler than a shared runtime module but
  duplicates `arrayMap`/`Effect` code into every artifact; a shared runtime module is
  compiled once and referenced by name.
- **Cross-module summary optimisation in slice 1.** Premature: it concentrates the
  `purs-wasm` separate-compilation pitfalls and depends on the not-yet-designed
  general inliner; the `.pvmi` is structured to accept it later.
