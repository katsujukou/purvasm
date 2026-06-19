# 0016. Cross-module linking

- Status: Accepted
- Date: 2026-06-19

## Context

[0015](0015-corefn-lowering.md) (Accepted) lowers a **single** module to a
`Cesk.Ast.term`, evaluated at one entry, and leaves cross-module references as
`Var`s under their qualified key — unbound, so `stuck` if forced. To run a real
program — the milestone is a `Prelude`-only app — those references must resolve:
the entry module plus its transitive imports have to be loaded and **linked** into
one term where every binding is in scope.

This is the first of the three walls toward the milestone (linking; then the two
FFI slices). It is squarely on the already-built upper-IR / reference-interpreter
path: linking produces a `Cesk.Ast.term` and changes nothing below it.

Two facts make it tractable:

- [0015](0015-corefn-lowering.md)'s **flat qualified-key scheme** already gives
  every cross-module reference a well-formed key; linking only has to *bind* those
  keys, not change how references are produced.
- PureScript **forbids cyclic module imports**, so the module graph is a DAG and a
  topological order exists.

## Decision

### A program is an entry module plus its transitive import closure

A "program" is loaded by starting from an entry module's `corefn.json` and
following `imports` transitively. A module name maps to its file by the layout
`purs` emits — the dot-joined name as a directory: `<outdir>/<Module.Name>/corefn.json`.
A module with **no `corefn.json` contributes no bindings and is skipped** — this
covers `Prim` and its friends, which are type-level only (no runtime values).
(A stricter "missing because uncompiled vs builtin" check can come later;
skip-if-absent is enough for the slice.)

### Loading and linking are separated

- **Load** (filesystem) resolves the closure: `outdir`, entry module → the list
  of decoded `Corefn.Module.t` ([0014](0014-corefn-ingestion.md)).
- **Link** (pure) takes that module list and an entry, and returns a
  `Cesk.Ast.term`. Keeping link pure makes it testable without IO; load owns
  discovery.

Both live in the `lower` library (a `Link` module), composing
`Corefn.Decode` + `Lower` ([0015](0015-corefn-lowering.md)); no new dependency
direction.

> **Progress (2026-06-19):** implemented as a **separate `link` library**
> (depending on `lower` + `corefn` + `cesk`) rather than a module inside `lower`.
> Dune's wrapped-library rule makes the same-named main module (`Lower`) the sole
> entry point, so a sibling `Link` cannot depend on `Lower` within one library. A
> dedicated `link` library keeps the intended inward, acyclic dependency
> (`link → lower → {corefn, cesk}`) and the same load/link split; only the "same
> library" wording changes.

### Link = one binding chain in module-topological order

Each module is lowered to its declarations under its own-module qualified keys
([0015](0015-corefn-lowering.md)), and the modules' declaration groups are
concatenated into **one binding chain ordered by the module dependency DAG**
(a dependency before its dependents), preserving CoreFn's `Rec`/`NonRec` order
*within* each module. The body of the chain is `Var <entry's qualified key>`.

Because the graph is acyclic, in topological order **every cross-module reference
resolves to a binding already introduced earlier in the chain**, so strict
initialisation of top-level CAFs stays well-defined program-wide — the same
argument [0015](0015-corefn-lowering.md) made within a module, now across modules
via the topo sort. (No global recursive group is needed or wanted; see
*Alternatives*.)

### Foreign idents resolve through an (initially empty) FFI resolver

A `foreign import f` in module `M` is referenced as the key `M.f` but has **no
CoreFn binding** (its implementation is native). Linking consults an **FFI
resolver** (qualified ident → a binding) and binds the foreign names it can
resolve, prepending them to the chain; the rest stay unbound. **In this slice the
resolver is empty** — it is the seam the FFI records (ADR-0017+) will fill. So a
`Prelude` program *links and runs as far as its non-foreign parts go*, and forcing
a foreign leaf (e.g. `Data.Semiring.intAdd`) is `stuck` until FFI lands.

> **Progress (2026-06-19):** implemented by resolving the linked chain's **free
> variables** (referenced-but-unbound keys) through the resolver, rather than
> iterating each module's `foreign_names`. Free-variable resolution is the more
> general mechanism: it also binds **compiler builtins that are not `foreign
> import`s** — notably `Prim.undefined`, the throwaway argument applied to a
> nullary superclass thunk — which a `foreign_names`-only pass would miss. The
> resolver interface is unchanged.

### Scope of this slice

Loading + linking only. **Deferred:** populating the FFI resolver (the FFI
records), any optimisation, and import-cycle handling (cannot occur — the graph is
a DAG). Linking stays **total**: an unresolved foreign or external reference is the
unbound-`Var` / runtime-`stuck` case, never a link-time failure.

## Consequences

- **Pure cross-module programs run end to end** on the reference interpreter, and a
  `Prelude` program *links* (it only gets `stuck` when it forces a foreign leaf) —
  so linking is verifiable before any FFI exists.
- **The flat-key scheme pays off** ([0015](0015-corefn-lowering.md)): linking only
  *binds* keys; nothing about reference lowering changes.
- **The resolver + `foreign_names` are the exact hook for FFI.** The next slices
  populate the resolver (primitive leaves as eta-expanded primop terms; then
  higher-order native FFI), with no change to linking.
- **Program-wide strict init is correct** by topo order across modules × CoreFn
  order within — the cross-module generalisation of
  [0005](0005-mutual-recursion-binding-groups.md)/[0015](0015-corefn-lowering.md).
- **Tooling shift:** a test fixture becomes a *program directory* of `corefn.json`
  (entry + transitive imports), compiled by `purs`/`spago`; the E2E suite links a
  program and asserts on the result. The reference interpreter now runs
  multi-module PureScript — the differential oracle grows with it.
- **Load is the one IO surface**; link stays pure and unit-testable.

## Alternatives considered

- **One global `Letrec` over every module's decls.** Rejected: a non-function CAF
  reading a binding placed later in the group would black-hole
  ([0004](0004-recursion-letrec-fix.md)); topological order makes that impossible
  and is available because the module graph is acyclic, so the simpler ordered
  chain is also the correct one.
- **Caller supplies the module list** instead of discovering the closure from
  `imports`. Rejected as the default: a program is *defined* by its entry and what
  it imports; transitive discovery matches that and avoids a brittle hand-kept
  list. (The pure `link` still takes an explicit list, so a caller *may* supply one
  for tests.)
- **Hard-error at link time on an unresolved foreign/external.** Rejected: total
  linking lets a pre-FFI program link and run its self-contained parts, with the
  runtime `stuck` naming the missing key — consistent with
  [0015](0015-corefn-lowering.md). (An opt-in completeness check can come with the
  FFI/linking-complete slice.)
- **Eagerly load every `corefn.json` in the output directory.** Rejected: only the
  entry's transitive closure is needed; loading the whole compiled package is
  wasteful and would demand modules the program never uses.
- **Resolve foreign idents during per-module lowering.** Rejected: foreign
  resolution is program-global (one registry shared by all modules), so it belongs
  at link time, not inside a single module's lowering.
