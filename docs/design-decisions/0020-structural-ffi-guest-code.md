# 0020. Structural / higher-order FFI as guest code over first-order primitives

- Status: Accepted
- Date: 2026-06-19

## Context

Wall ③ of the `Prelude` milestone is the higher-order foreign leaves —
`Data.Functor.arrayMap`, `Data.Eq.eqArrayImpl`, `Data.Ord.ordArrayImpl`, … —
which must apply a guest closure (the mapper, the element comparator). An earlier
sketch reached for a native `VForeign` value plus a re-entrant `Machine.apply`
(host-recursive evaluation). How purs-wasm tackles the same problem points the
other way.

purs-wasm (its ADR 0026, *WasmBase*) found that **a `foreign` is an opaque barrier
to the optimiser** — higher-order specialisation, inlining, and dictionary
elimination cannot see through it — and that a *higher-order* foreign is the
pathological case: the closure it receives can never be fused, so it is applied
per element through an opaque boundary. Their resolution is structural: **a
higher-order function we want optimised must not be a `foreign` at all — it is
PureScript, built on *first-order* primitives** (array `get`/`set`/`length`, …),
which carry no closure and so cost nothing at the boundary. Genuine host effects
stay native; everything structural is PureScript over a tiny primitive layer.

The same conclusion holds for purvasm for an additional, immediate reason: if a
structural HOF is **guest code**, the callback is an ordinary `App`, evaluated by
the machine with the continuation **reified on `K`** ([0002](0002-cesk-execution-model.md))
— no host-stack re-entrancy, no `VForeign`, no `apply` injection, no store-cycle.
A native re-entrant `apply` would instead put the outer continuation on the host
stack (uncapturable) — acceptable for pure FFI but pointless here, and it would
*reintroduce* the optimiser barrier we are trying to avoid.

purvasm also differs from purs-wasm in two ways we exploit: we **own the runtime
ABI** (no fixed wasm-GC substrate), and we **do not target JS** — so the entire
JS-marshalling layer purs-wasm carries (eqref⇄JS, manifests, the eqref/externref
ambiguity, the closure-direction-2 gap, `Object a`) simply does not exist for us.

## Decision

### Structural / higher-order foreigns are guest code over first-order primitives

A structural or higher-order foreign operation is provided as **guest code**
(`Cesk.Ast`, ultimately PureScript) built on a **first-order primitive layer** —
*not* as native code and *not* as an opaque `foreign`. Its callback is an ordinary
`App`, so the machine drives it with `K` reified; **no native re-entrant `apply` /
`VForeign` is introduced.** This **supersedes** the earlier higher-order-native-FFI
sketch for structural FFI.

The **first-order primitive layer** is the irreducible set PureScript cannot
express here — array build/index/length ([0019](0019-mutable-array-building.md)),
string bytes, record/scalar ops. It is purvasm's analogue of purs-wasm's
WasmBase: first-order only (a higher-order primitive would push the barrier one
level down), representation-revealing, no type classes. For now it is referenced
through primops; a `purvasm-base`-style PureScript surface over it can come later.

### Native FFI (with the owned value ABI) is reserved for genuine host effects

Only *genuinely external* operations — IO, syscalls, host-library calls (the
`Effect`-shaped foreigns) — are native, speaking purvasm's owned value ABI
directly. These are designed with the effect / runtime slice and need the proper
`K`-based continuation mechanism (the flagship async), not the host-recursive
shortcut. **No JS interop, hence no marshalling layer** (the deliberate
simplification over purs-wasm). `Effect` is out of scope for the `Prelude`
milestone, so this rung is deferred.

### Staged authoring surface

The principle (guest code over first-order primitives) is fixed; the authoring
surface evolves:

- **Now (no optimiser):** the few structural foreigns the milestone needs
  (`arrayMap`, `eqArrayImpl`, …) are **hand-written `Cesk.Ast` guest terms** in the
  FFI bundle, over the first-order primitives. This already buys the
  re-entrancy-free, `K`-reified behaviour; optimiser transparency is moot with no
  optimiser.
- **Later (with an optimiser):** upgrade to **PureScript reimplementations that
  shadow the stock modules** (purs-wasm's ulib model), so the optimiser sees their
  bodies. Same principle, better surface.

### Deferred

The ulib / module-shadowing machinery, a published & version-locked low-level
package (and its capability check), the optimiser, and JS interop — none are
needed for the interpreter milestone. The FFI bundle stays a set of per-library
mappings ([0017](0017-primitive-ffi.md)'s ladder, composed by the driver); these
structural entries are guest terms rather than primop-eta-terms or native values.

## Consequences

- **`arrayMap` and friends run on the interpreter now** with no machine extension
  beyond [0019](0019-mutable-array-building.md)'s array builders — no `VForeign`,
  no re-entrant `apply`, the continuation stays reified.
- **Optimiser transparency is set up for free.** Structural code is guest code, not
  an opaque foreign, so when the optimiser lands it can specialise through it —
  exactly the property purs-wasm engineered WasmBase to obtain.
- **The native surface stays tiny** — only the first-order layer and (later)
  genuine-effect foreigns. That concentrates the owned-ABI coupling in one small
  place (purs-wasm's "axis 2" lesson), keeping the rest version- and
  ABI-decoupled guest code.
- **No JS-marshalling complexity** — we sidestep purs-wasm's largest subsystem and
  its open limitations, at the cost of JS-ecosystem reuse (a non-goal for a native
  runtime). A host-interop boundary can be designed later if ever wanted.
- **Maintenance is staged**: a handful of structural guest terms now; a curated
  PureScript reimplementation set (the ulib analogue, version-pinned) only when the
  optimiser makes it worthwhile.
- **The earlier `VForeign` + re-entrant-`apply` plan is dropped** for structural
  FFI; that machinery, if needed at all, belongs to the effect/async slice, which
  must capture `K` rather than recurse on the host stack.

## Alternatives considered

- **Native `VForeign` + re-entrant `Machine.apply` for structural FFI** (the earlier
  sketch). Rejected: unnecessary (guest code suffices), it reintroduces the
  optimiser barrier (opaque native), it puts the outer `K` on the host stack, and
  it duplicates effort with the future effect mechanism — which needs a real
  `K`-based capture anyway. purs-wasm's central FFI finding is exactly "HOFs must
  not be foreign."
- **Keep structural ops as opaque foreigns resolved to native primops.** Rejected:
  opaque to the future optimiser (the barrier), and the higher-order ones would
  still need re-entrancy to apply their callback.
- **Adopt JS interop / a marshalling tier** (purs-wasm's convenience path).
  Rejected for the native-runtime goal: it is purs-wasm's biggest complexity
  (marshalling glue, eqref/externref, closure-direction-2, `Object a`) bought for
  JS reuse we do not target.
- **Build the full WasmBase-style published package + ulib shadowing now.**
  Rejected as premature: its payoff is optimiser transparency, and there is no
  optimiser yet; the staged surface keeps this slice small.
- **A pure `snoc`-based `arrayMap` to avoid mutation.** Rejected with
  [0019](0019-mutable-array-building.md): O(n²) in the reference for no benefit
  once the first-order builders exist.
