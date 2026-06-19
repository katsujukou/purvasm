# 0019. Mutable array building primitives (revising ADR-0009)

- Status: Accepted
- Date: 2026-06-19

## Context

[0020](0020-structural-ffi-guest-code.md) (Proposed) decides that higher-order /
structural foreign operations (`Data.Functor.arrayMap`, `eqArrayImpl`, …) are
written as **guest code over first-order primitives**, not as native code — so a
callback is ordinary application and no native re-entrancy is needed. To write
`arrayMap` that way, guest code must **build an array incrementally** with
first-order operations.

[0009](0009-array-immutable-host-backed.md) made `Array` immutable and explicitly
deferred mutation: "although the host array has a mutable backing we never use
it." That backing is exactly what is needed now. This is the same move
purs-wasm's WasmBase makes — its `Wasm.Array` exposes `unsafeNew` / `unsafeSet`
over its mutable `$Vals`, "fill, then return as `Array`" (ADR 0026 there) — so
that `map`/`fold`/`filter` are PureScript over first-order array ops.

## Decision

Add two first-order, in-place **array-building** primitives, exposing the mutable
backing `VArray` already has:

- **`NewArray : Int -> Array a`** — allocate an array of `n` slots. Slots hold an
  arbitrary filler (the immediate `0`) and **must be written before they are read**
  — this is an unsafe builder primitive.
- **`SetArray : Array a -> Int -> a -> Array a`** — write slot `i` **in place** and
  return the same array, so a builder loop can thread it (matching purs-wasm's
  `unsafeSet :: … -> Array a`).

Reading and measuring reuse the existing primitives (`IndexArray`, `LengthArray`,
[0009](0009-array-immutable-host-backed.md)); literal construction is unchanged.

These are the **unsafe low-level builders**: aliasing a half-built array breaks
purity, so they are not a user-facing API. They are consumed only by the
first-order layer / structural-FFI guest code ([0020](0020-structural-ffi-guest-code.md)),
which uses them in the **ST discipline** — allocate a *fresh* array, fill it, then
use it immutably and never mutate a shared one — keeping the result observably
pure. (General mutable state — `Ref`, `STArray` as a user feature — remains a
later, effects-related slice; this record adds only local build-then-freeze.)

This **revises [0009](0009-array-immutable-host-backed.md)**: the `VArray` backing
is now mutated in place by these builders, where 0009 said it never would. The
immutable value, its literal form, and the read API are otherwise unchanged; a
dated Correction is recorded there.

## Consequences

- **Unblocks guest-code structural array functions** ([0020](0020-structural-ffi-guest-code.md))
  without native re-entrancy and at O(n) (vs the O(n²) a pure `snoc`-rebuild would
  cost).
- **First value-level in-place mutation.** Until now the only in-place write was
  the *store* backpatch of a recursive binding ([0004](0004-recursion-letrec-fix.md));
  this is the first mutation of a *value*'s contents. Forward-looking: a moving /
  generational collector will need a write barrier here, the value-level analogue
  of the store-backpatch site 0004 flagged.
- **Unsafety is confined.** The primitives are unsafe (uninitialised slots,
  aliasing), so per the `CLAUDE.md` convention they surface only through the
  low-level layer and are wrapped by safe builders; ordinary code never calls them.
- **`Store` is untouched** — `VArray` is still a host value, not a store cell
  ([0009](0009-array-immutable-host-backed.md)); only its element backing is
  written.
- **Bytecode impact (forward-looking).** `NewArray`/`SetArray` map to an
  allocate-then-indexed-store instruction pair — ordinary, consistent with
  [0003](0003-stack-based-bytecode.md).

## Alternatives considered

- **Pure `snoc :: Array a -> a -> Array a` (copy-on-append), no mutation.**
  Rejected (the interim option weighed in discussion): it keeps 0009 immutable but
  is O(n²) for a build, and still needs a builder loop. Since `VArray` already has
  a mutable backing, exposing in-place build is cheap and O(n); the interpreter is
  the spec, but needless O(n²) in the reference is worse than a confined unsafe
  primitive.
- **A separate `VMutableArray` / `STArray` value type with `freeze`.** Rejected for
  now: purs-wasm reuses one mutable array type (no `freeze` — fill then return as
  `Array`), and a second type plus freeze is more machinery than the build-then-use
  discipline needs. A distinct mutable type can come with the general `ST`/effects
  slice if wanted.
- **A higher-order `ArrayGenerate : Int -> (Int -> a) -> Array a` primitive.**
  Rejected: it takes a closure, so its implementation would have to apply guest
  code — exactly the native re-entrancy [0020](0020-structural-ffi-guest-code.md)
  avoids. Keeping the builders first-order pushes the iteration (and the callback)
  into guest code, where application is ordinary.
