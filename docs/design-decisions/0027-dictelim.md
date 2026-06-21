# 0027. DictElim: collapse statically-known type-class dispatch

- Status: Accepted
- Date: 2026-06-21

> **Progress (2026-06-21):** implemented as `Middle_end.Passes.Dict_elim`; sound
> (the ADR-0025 round-trip agrees with the oracle on every fixture) and no
> baseline regression. **Measured standalone effect is ~0%** (a constant −20…−40
> steps across the bench suite, independent of input size). The benchmark
> explained why: PureScript already *floats* a constant dictionary-member
> extraction (`add semiringInt`) to a shared binding, so the dispatch runs
> **once** (by-need memoised) and the hot loop just applies the extracted binary
> function — DictElim removes only that one-time dispatch. The per-call win it
> sets up (`add' = intAdd`, then `add' a b`) is unlocked by a follow-up
> copy-propagation-and-inlining pass that turns `intAdd a b` into
> `Prim(AddInt, a, b)`; DictElim is the prerequisite that makes the method resolve
> to `intAdd` for that inliner to see. Recommend that pass next.

## Context

Type classes are dictionary-passing (ADR-0007). After lowering, a method call
`m d a…` is `accessor d a…`, where — verified against the generated CoreFn:

- the method `accessor` is `\dict -> case dict of Cls$Dict v -> v.field`; with the
  newtype dict binder erased (ADR-0018) it is `\dict -> case dict of v -> v.field`;
- an instance `d` is the newtype-constructor application
  `Cls$Dict { field: impl, … }`, which erases to `(\x -> x) { … }`.

So every call re-runs: enter the accessor, beta, the one-arm `case`, and the
record projection — per call, in hot loops. By-need (ADR-0024) already makes an
*unused* dict cost nothing, but a *used* one pays this dispatch on every call.

When the dictionary at a site is statically known — a monomorphic use, e.g. `+`
or `==` on `Int` resolves to a fixed instance CAF — all of that is computable at
compile time.

## Decision

A whole-program ANF pass (ADR-0025) that collapses statically-known dispatch.

- **Classify** top-level bindings once:
  - *accessor* — the `\d -> case d of v -> v.φ` shape → record the field `φ`;
  - *instance dict* — a binding that, seen through the erased newtype identity
    application and trivial lets, is a record literal → record `φ ↦ impl` per
    field.
- **Rewrite** every *saturated* dispatch `accessor dict :: rest`, where `accessor`
  is a known accessor for `φ` and `dict` is a known instance whose `φ` is `impl`,
  to `impl :: rest` — an atom swap, no substitution, so no capture. Iterate to a
  fixpoint (superclass chains resolve in stages).
- **Safety guards** — fire only when the rewrite is certainly correct:
  - only when `dict` is a *statically known* instance binding; a dictionary
    received as a parameter (genuinely polymorphic code) is left untouched;
  - only when `impl` is in scope at the site — a top-level binding key, or a
    literal / foreign atom. A method whose field is a *computed* value (let-bound
    inside the instance, e.g. `compare = ordIntImpl LT EQ GT`) is left as
    dispatch; hoisting such fields is future work.
- **Dead bindings** — instances/accessors left unreferenced are not removed here;
  under by-need they are simply never forced, so they cost nothing at runtime. A
  later DCE pass can drop them from the code.

Verified by the ADR-0025 round-trip (same value, and same effects, on every
fixture) and measured by ADR-0026 (a new `dictelim` variant whose curve sits below
`anf`, with the `direct`/`anf` baseline columns unchanged — no regression).

## Consequences

- Monomorphic method calls in hot loops (e.g. `fib`'s `+`/`-`) lose their per-call
  dispatch; an expected step/alloc drop on the arithmetic-heavy benches.
- Polymorphic dispatch and computed-field methods are conservatively preserved —
  correctness over completeness for v1; both are addressable later (specialization;
  field hoisting).
- The classifier matches the specific shapes our lowering emits (newtype-erased
  accessor `case`, newtype-identity-wrapped instance record); documented so a
  later lowering change is noticed.
- This is the first optimiser pass on the ANF; it establishes the
  pass → round-trip → bench loop the rest will follow.

## Alternatives considered

- **A general inliner / partial evaluator** that subsumes DictElim. More powerful
  but much larger and riskier; DictElim is the high-value, bounded special case
  to do first.
- **Specialisation / monomorphisation** (clone polymorphic functions per instance)
  to also kill polymorphic dispatch. Heavier; revisit once DictElim and inlining
  exist.
- **Hoist computed instance fields to top level** so more methods qualify. A
  useful follow-up, deferred to keep v1 small.
- **Do nothing / rely on the VM.** Dispatch is a real per-call cost the IR can
  remove cheaply; worth doing at this layer.
