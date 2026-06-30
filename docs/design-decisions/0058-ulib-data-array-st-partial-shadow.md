# 0058. `ulib` `Data.Array.ST.Partial` shadow over the reified `STArray`

- Status: Accepted
- Date: 2026-06-30

> **Progress (2026-06-30):** implemented — `ulib/arrays/Data.Array.ST.Partial.purs` (the two-line
> shadow) plus the `arrays` `test` block. `verify` reports all four `arrays` modules interface-faithful
> (`Data.Array.ST.Partial: ok`), and `ulib-tools test` now shows `arrays` and `enums` both PASS.

## Context

[0039](0039-ulib-st-array-and-st-uncurried.md) reified the `ulib` `Data.Array.ST`'s `STArray` as an
`STRef`-wrapped growable buffer (`STArray h a = STArray (STRef h { buf :: Array a, len :: Int })`)
over the `Purvasm.Array` primitive, instead of a native JS mutable array. It did not address the
sibling registry module `Data.Array.ST.Partial`, which the `ulib` does not yet patch.

`Data.Array.ST.Partial` (arrays 7.3.0) is a two-function FFI module — `peek`/`poke` carrying a
`Partial` constraint — whose JavaScript implementation indexes the `STArray` **directly**:

```js
export const peekImpl = function (i, xs) { return xs[i]; };
export const pokeImpl = function (i, a, xs) { xs[i] = a; };
```

That assumes `STArray` *is* a JS array. Against the [0039](0039-ulib-st-array-and-st-uncurried.md)
representation it operates on the wrong shape (the `STRef`/record wrapper, not an array). The
[0048](0048-ulib-tools-test-upstream-suite-execution.md) `test` command surfaced exactly this: the
`arrays` upstream suite passes every group except `Test.Data.Array.ST.Partial`, which fails because
`peek`/`poke` read/write the wrapper rather than the buffer. So `arrays` cannot yet carry a `test`
block, and the patch set is representation-incomplete.

## Decision

Add a `ulib` shadow `ulib/arrays/Data.Array.ST.Partial.purs` that reimplements `peek`/`poke` over
`Data.Array.ST`'s **public, bounds-checked** API rather than over the reified internals:

```purescript
peek :: forall h a. Partial => Int -> STArray h a -> ST h a
peek i xs = fromJust <$> STA.peek i xs

poke :: forall h a. Partial => Int -> a -> STArray h a -> ST h Unit
poke i a xs = void (STA.poke i a xs)
```

- The module's `Partial` constraint discharges `Data.Maybe.fromJust`, so no `unsafePartial` is needed
  and the exported signatures match the registry's exactly (interface-faithful — the
  [0043](0043-ulib-tools-build-verify-test.md) `verify` surface is unchanged).
- It reuses `Data.Array.ST`'s existing reified `peek`/`poke`; the abstract `STArray` newtype stays
  abstract — the shadow needs **no** access to `{ buf, len }`, so the constructor is not exported and
  the representation does not leak across the module boundary.
- As a `ulib` shadow it has no FFI, so its sibling `.js` is dropped by the staging overlay
  ([0038](0038-base-package-and-ulib-patches.md)), the same as the other `arrays` patches.

With the shadow in place, give `ulib/arrays/ulib.json` a `test` block (`repo`
`purescript/purescript-arrays`, `ref` `v7.3.0`, `testDeps` `assert`/`console`/`const`, `fidelity`
`js`, no `xfail`). The full `arrays` suite then passes on the JS build (validated against the prepared
clone before this ADR), so `arrays` joins `enums` in the `prepare-release.sh` `test` gate.

## Consequences

- `arrays` becomes the second representation-equivalent package with real upstream-suite coverage, and
  the [0039](0039-ulib-st-array-and-st-uncurried.md) representation gains an end-to-end behavioural
  check it lacked.
- The partial `poke` diverges from the registry only in the out-of-bounds case the `Partial` contract
  forbids: upstream's `xs[i] = a` silently extends the JS array, whereas the bounds-checked `poke`
  no-ops (returns `false`, discarded). Within the contract (caller guarantees the index is live) the
  behaviour is identical; out of contract, neither result is meaningful.
- A small constant-factor cost over a hand-tuned `STFn`: `peek`/`poke` route through the public
  wrappers (which already use `runSTFn` internally) and allocate a transient `Maybe` for `peek`. This
  is within [0039](0039-ulib-st-array-and-st-uncurried.md)'s accepted-cost envelope for the bootstrap.

## Alternatives considered

- **Expose the `STArray` internals to a sibling `Data.Array.ST.Partial`** (export the constructor or a
  `Data.Array.ST.Internal`, and index `{ buf, len }` directly with `Purvasm.Array`). Rejected: it
  leaks the representation across a module boundary for a marginal constant-factor gain, when the
  public bounds-checked API already gives exactly the needed semantics.
- **Mark `Test.Data.Array.ST.Partial` `xfail`.** Rejected: `arrays` is representation-equivalent
  (`js` fidelity); a needed `xfail` would mean it is mis-categorised
  ([0048](0048-ulib-tools-test-upstream-suite-execution.md) §2). The failure is a real, fixable patch
  gap, not an intended divergence.
- **Leave `arrays` untested.** Rejected: it forgoes coverage of the highest-churn `ulib` array code
  for the sake of a two-line shadow.
