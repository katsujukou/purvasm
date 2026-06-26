# 0039. `ulib` `Data.Array.ST` over the fixed-length primitive; `ST.Uncurried` structural

- Status: Accepted
- Date: 2026-06-26

## Context

The PureScript self-host ([0037](0037-self-hosting-purescript.md)) is in the
demand-driven FFI/`ulib` build-out: foreigns are completed as the compiler's closure
reaches for them, cheapest layer first, under the `ulib`-first / minimal-native-surface
policy ([0038](0038-base-package-and-ulib-patches.md)). The `ulib` `Data.Array` keeps
upstream's ST-based implementations verbatim, so two more registry modules now have to be
provided:

1. **`Control.Monad.ST.Uncurried`** — `STFn1..10` + `mkSTFnN`/`runSTFnN` (~20 declarations),
   the exact analogue of `Effect.Uncurried`.
2. **`Data.Array.ST`** — `STArray` + ~15 operations. Unlike GHC's `array`-package
   `STArray` (a *fixed-size* `MutableArray#`), PureScript's `STArray` is a **growable**
   mutable array (JS `push`/`pop`/`splice`); it corresponds to `vector`'s `MVector`, not
   to `Data.Array`. boot's `Purvasm.Array` ([0019](0019-mutable-array-building.md)) is a
   **fixed-length** mutable buffer, so a growable representation must be decided.

[0019](0019-mutable-array-building.md) explicitly deferred this: "General mutable state —
`Ref`, `STArray` as a user feature — remains a later, effects-related slice." This record
is that slice for `STArray`.

## Decision

### 1. `Control.Monad.ST.Uncurried` — structural, mirroring `Effect.Uncurried`

boot is all-curried, so `STFnN` ≡ "a curried function that, when saturated, runs an `ST`",
and `mkSTFnN`/`runSTFnN` are the identity/saturated-application adapters between it and the
curried world. `Effect.Uncurried` is **already** handled this way (boot special compiler
support, structural — [0017](0017-primitive-ffi.md)/[0020](0020-structural-ffi-guest-code.md));
`ST.Uncurried` takes the same treatment. A `ulib` reimplementation is rejected because the
upstream `STFnN` are opaque `foreign import data` types: applying one from PureScript needs
a bridge anyway, and redefining them as type synonyms would change the public representation
(narrowing the interface, against [0038](0038-base-package-and-ulib-patches.md)).

### 2. `Data.Array.ST` — `ulib` growable buffer over the fixed-length primitive (Option 1)

`STArray` is reified in `ulib` PureScript as a single `STRef` (the `ST` ref primitive
already exists structurally) holding a fixed-length `Purvasm.Array` plus a logical length;
capacity doubles on overflow (the growable-buffer-over-`Purvasm.Array` pattern `ulib`
already uses in `Data.Unfoldable`/`Unfoldable1`):

```
STArray h a ≈ STRef h { buf :: Purvasm.Array a, len :: Int }   -- capacity = length buf
```

- `peek`/`poke` (index `< len`) → `Purvasm.Array.unsafeIndex`/`unsafeSet` (buffer identity
  unchanged; no ref write).
- `push`: `len < cap` → set at `len`, ref `len+1`; `len == cap` → allocate `2×` buffer, copy,
  set, ref the new buffer. Amortised O(1).
- `freeze`/`thaw` → copy to an exact-length array. `splice`/`shift`/`unshift`/`pop` → in-`len`
  shifts (O(n), as upstream JS already is).

**No backend primitive is added** — `Data.Array.ST` is pure `ulib` over `Purvasm.Array`
([0019](0019-mutable-array-building.md)) + the structural `ST` ref. The native/intrinsic
surface, hence the ABI contract base and backend co-evolve on
([0038](0038-base-package-and-ulib-patches.md)), is unchanged.

### Accepted cost

`unsafeFreeze`/`unsafeThaw` lose their O(1) zero-copy coercion: with doubling slack, freeze
must trim-copy to an exact-length array, and `unsafeThaw`'s "later mutations affect the
original" no longer holds across a resize. Both are within the documented `unsafe*` contract
("do not use the source afterward"), and the perf hit is a **constant factor, not asymptotic**
— the surrounding `Data.Array` build is already O(n), so an extra O(n) freeze is ~2×. Per
[0035](0035-native-backend-ocaml5-concurrency.md) (optimiser paused; correct-first,
perf-on-demand) this is acceptable for the bootstrap, whose `STArray` workload is build-then-
freeze-once with push-heavy growth — exactly what amortised-O(1) push + copy-on-freeze suits.

## Consequences

- `Data.Array` (and the rest of the ST-using closure) compiles with **zero new FFI / zero new
  primitives** — the `ulib`-first policy holds and the ABI stays fixed.
- One consistent growable strategy across `ulib` (`STArray`, `Unfoldable`, `Unfoldable1` all
  double over `Purvasm.Array`), rather than two.
- `unsafeFreeze`/`unsafeThaw` are O(n) copies here, not O(1) coercions — a confined,
  constant-factor regression, flagged for the on-demand perf pass.
- `ST.Uncurried` joins `Effect.Uncurried` as structural boot support; the two stay in lockstep.

## Alternatives considered

- **Option 2 — a growable-array primitive group in boot.** A real variable-length buffer with
  amortised-O(1) push and (claimed) near-zero-copy freeze, but it **permanently expands the ABI
  contract** ([0038](0038-base-package-and-ulib-patches.md)): the growable value must be
  implemented in `Cesk.Value`, the VM `Value.t`, the OCaml-codegen `Rt.value`, and the future
  bespoke native runtime. Growable-ness is not a *true* primitive (cf. array `new`/`index`/
  `set`/`length`) — it is a derived capability, exactly what 0038 pushes into `ulib`. **The
  prior-art check undercuts its main advantage:** GHC has no growable array primitive either —
  the RTS `MutableArray#` is fixed-size, `array`'s `STArray` is fixed-size, and the growable
  type (`vector`'s `MVector`) is *library* doubling (copy-on-grow) over it. `vector`'s O(1)
  `unsafeFreeze` works only because its **immutable** `Vector` is an `(offset, length, backing)`
  *slice view*. purvasm's immutable `Array` is **flat** ([0009](0009-array-immutable-host-backed.md),
  no view), so a slack buffer must trim-copy on freeze **regardless of whether the mutable side
  is `ulib` or a primitive** — i.e. Option 2 would *not* deliver near-zero-copy freeze without
  *also* changing the immutable `Array` to a view type, a far larger change. Rejected now;
  revisitable on-demand if freeze copies are a measured bottleneck — at which point the right
  lever is the immutable-`Array` slice-view, not a growable mutable primitive.
- **`ulib` reimplementation of `ST.Uncurried`.** Rejected — opaque `foreign import data`
  `STFnN` need a bridge regardless, and a type-synonym redefinition narrows the public
  interface. Structural-in-boot (as `Effect.Uncurried`) is the consistent, low-risk choice.
- **Exact-size (no-slack) growable buffer to keep `unsafeFreeze` zero-copy.** Push becomes O(n)
  each (no amortisation) — the opposite, worse tradeoff for the push-heavy build workload.
