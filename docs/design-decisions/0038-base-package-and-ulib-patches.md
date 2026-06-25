# 0038. A minimal native base (`Purvasm.*`) and a `ulib` of registry-package patches

- Status: Accepted
- Date: 2026-06-26

## Context

The self-host CLI (`compiler` + `cli`) reaches ~128 distinct `foreign import`s once its
whole closure is compiled (Data.Array, Data.Int.Bits, Data.Number, Data.String, the
`Uncurried` families, `Control.Monad.ST`, `Foreign.Object`, Node FS / process, Argonaut,
…). boot already compiles the *language* of that closure to native; the only gap is these
foreigns. Implementing all 128 as OCaml host leaves conflicts with the minimal-compiler
ethos (a backend should not bundle every package's FFI) and is high-maintenance.

purs-wasm faced the same problem and settled — after several iterations — on a two-part
model we take as the reference in its **latest** form (its ADRs 0026, 0039, 0041):

- **wasm-base** — a small *primitive* package (`Wasm.*`) the backend recognises as
  intrinsics, shipping JS foreigns too so the same code also builds on stock `purs`.
- **ulib** — *patches* on registry packages (not a from-scratch library): a foreign-impl
  completion and/or an interface-compatible PureScript reimplementation, shipped as
  **pre-built corefn** because the compiler core is **corefn-in and never runs `purs`**.

boot is already corefn-in (never runs `purs`), so that core invariant holds for free. This
record adapts the model to purvasm. It refines the existing FFI ladder — primitive FFI
(ADR-0017), structural/higher-order FFI as guest code (ADR-0020), the native rung
(ADR-0022) — and is a means to the self-host (ADR-0037).

## Decision

### 1. `Purvasm.*` — a small primitive base package

An in-repo spago package, namespace `Purvasm.*`, of the **true primitives** the backend
recognises as intrinsics (array `new`/`index`/`set`/`length`, int bit-ops, char/string
byte operations, float bits, …). It carries **JS foreigns** too, so a `Purvasm.*`-using
program still builds and tests on stock `purs` / `purs-backend-es` (dual-target dev/test);
on boot the `Purvasm.*` foreigns resolve to `Rt` leaves / primops (the JS is ignored).

The set of recognised `Purvasm.*` primitives **is the ABI contract**; base and backend
co-evolve. It stays in this repo for now (spago cannot publish a monorepo sub-package);
split to its own repo when tooling allows.

### 2. `ulib` — patches on registry packages

A ulib module is a **patch** on a registry package — one or both of:

- **foreign-impl completion** — keep the registry `.purs` verbatim and supply the foreigns
  that have no native implementation (a leaf, or `Purvasm.*`); the kept registry
  source compiles with its real imports.
- **interface-compatible reimplementation** — replace `.purs` with PureScript over
  `Purvasm.*`, so a higher-order foreign (e.g. `arrayMap`) comes onto the optimiser's
  turf and needs no native at all.

A patch may **add** (internal helper modules — e.g. a `ulib/strings`
`Data.String.Internal.Utf8`) but may never **remove or narrow** the registry's public
surface, and is never a from-scratch API. ulib stays a low-maintenance delta. It ships as
**pre-built corefn** (boot never runs `purs`).

### 3. Resolution — presence-driven overlay; compatibility assumed for now

A module is lib-sourced **iff ulib ships an *interface-compatible* corefn for it**: the
patch applies only when its interface matches the user's module — never by version. boot
then overlays ulib's corefn over the project's (last-wins) and compiles as usual. The
foreign ladder: intrinsic (`Purvasm.*` -> `Rt` leaf) -> native leaf (the few genuine ones)
-> stuck. ADR-0020's structural guest terms move into ulib PureScript.

The compatibility guard is part of the model, but **boot cannot read externs yet, so it
*assumes* the reached patch is interface-compatible** rather than checking. The PureScript
self-host will read externs and enforce the gate; version stays provenance only.

### 4. The native surface shrinks

Native OCaml leaves reduce to the `Purvasm.*` primitives plus a few genuinely-native ones:
IO (file / argv / console), the float-bits-decimal (`Int64.bits_of_float`), and a
Dragon4-class `showNumber`. Everything else becomes ulib PureScript, compiled by purvasm
itself (dogfooding) to `.pmo` and linked.

## Consequences

- The 128-leaf problem dissolves: most foreigns become pure PureScript (ulib); the native
  surface shrinks to ~20-30 primitives plus a handful of IO / format leaves.
- Dual-target: the self-host keeps building and testing on stock `purs` while it also
  targets boot-native, because `Purvasm.*` carries JS foreigns.
- ulib is an interface-compatible patch over upstream, not a fork — low maintenance.
- boot's corefn-in design (never runs `purs`) is exactly what the latest model assumes.

## Alternatives considered

- **Implement all ~128 foreigns as OCaml host leaves.** High maintenance, bundles every
  package's FFI into the compiler — against the minimal-compiler ethos. Rejected.
- **A from-scratch purvasm standard library (own API).** A fork we would have to keep
  behaviourally complete forever. Rejected — ulib stays a patch over registry packages.
- **Version-pinned patch application.** Brittle (purs-wasm 0039 retired exact-version
  gating). Use presence-driven application now; externs-based compatibility later.

## Deferred

- nix-style global content-addressed library cache (purs-wasm 0040): a separate design
  discussion; whether purvasm adopts it is open.
- Repo separation of the `Purvasm.*` base: when spago/registry supports sub-packages.
- externs-based compatibility gate: with the PureScript self-host (boot assumes it holds).
