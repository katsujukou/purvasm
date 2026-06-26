# 0041. `Int`↔`Number` conversion: a cross-representation primop pair in the `Purvasm.*` ABI

- Status: Accepted
- Date: 2026-06-26

## Context

The self-host's `ulib` build-out ([0037](0037-self-hosting-purescript.md),
[0038](0038-base-package-and-ulib-patches.md)) reaches `Data.Int`, whose foreigns include
`toNumber :: Int -> Number` and `fromNumberImpl` (the engine of `fromNumber :: Number -> Maybe
Int`). On the JS backend neither *converts* anything: a JS `Int` and `Number` are the **same
runtime value**, so `toNumber` is the identity and `fromNumberImpl` is just a range/integrality
test that returns the value unchanged —

```js
fromNumberImpl = just => nothing => n => (n | 0) === n ? just(n) : nothing;
toNumber = n => n;
```

On purvasm `Int` and `Number` are **distinct representations** (machine int vs IEEE double,
[0008](0008-number-ieee754-double.md)), so a genuine conversion is needed that JS never had to
express. Two facts frame the decision:

- **No conversion primitive exists.** boot's primop set (`cesk/prim.ml`: `AddInt`…`SetArray`),
  the self-host's `Purvasm.Compiler.Primitive`, and `purvasm-base` all lack any `Int`↔`Number`
  cast. It cannot be expressed over existing primitives — arithmetic never crosses the
  representation boundary.
- **purvasm `Int` is already 32-bit.** Every Int primop applies `w32` (`prim.ml`), so the
  machine faithfully models PureScript's 32-bit `Int`; the conversion's 32-bit semantics line up
  with the rest of the Int primops rather than introducing a new width.

The question is which **FFI ladder rung** ([0017](0017-primitive-ffi.md),
[0038](0038-base-package-and-ulib-patches.md)) this belongs to: a native host leaf, or a new
`Purvasm.*` intrinsic primop.

## Decision

### A cross-representation primop pair, at the intrinsic rung

Add two primops — pure scalar representation conversions the backend owns, in the same category
as the arithmetic intrinsics and the float-bits primitive ADR-0038 §1 already lists, **not**
native host leaves (they are not host *effects*; a leaf would be the wrong rung, per-backend,
and against the ladder):

- **`IntToNumber : Int -> Number`** — widening, `float_of_int`. Exposed as
  `Purvasm.Int.toNumber`.
- **`NumberToInt : Number -> Int`** — narrowing with **ECMAScript `ToInt32` semantics** (the
  `n | 0` coercion): truncate toward zero, reduce modulo 2³², interpret as signed 32-bit;
  `NaN`/`±Infinity` → `0`. Total and well-defined for every double (no `int_of_float`
  out-of-range UB). Exposed as `Purvasm.Int.fromNumber`.

`ToInt32` (not a naive truncation) is required so the pair reproduces the registry exactly: for
the `=== n` test below, `ToInt32 n == n` iff `n` is an integer in `[-2³¹, 2³¹)`.

### `Data.Int` becomes `ulib` over the pair

`Data.Int` is a **foreign-impl completion** (registry `.purs` verbatim; the foreign block
reimplemented in PureScript over `Purvasm.Int`):

```purescript
toNumber = PI.toNumber
fromNumberImpl just nothing n =
  let i = PI.fromNumber n
  in if PN.eq (PI.toNumber i) n then just i else nothing   -- ≡ (n | 0) === n ? just(n) : nothing
```

`fromString`/`toStringAs`/`quot`/`rem`/`pow` are separate foreigns handled on their own (string
radix work and a `pow`; out of scope here). `Data.Int.floor`/`ceil`/`round`/`trunc` build on
`toNumber`/`fromNumber` plus a `Number`-domain rounding step — a follow-on, not this record.

### Dual-target

`purvasm-base` carries JS foreigns so `Purvasm.Int`-using code still builds on stock `purs`:
`toNumber = n => n` (identity) and `fromNumber = n => n | 0` (`ToInt32`), matching the registry's
own JS exactly. On purvasm the JS is ignored and the names resolve to the two primops.

### Implementation surface (the ABI co-evolves)

This is the **first cross-representation primop** and an ABI extension ([0038](0038-base-package-and-ulib-patches.md)
§1: the recognised `Purvasm.*` primop set *is* the ABI contract), so it touches every backend in
lockstep, held by the standing differential discipline:

- **`purvasm-base`** — `Purvasm.Int.toNumber`/`fromNumber` (+ JS foreigns).
- **boot** — `IntToNumber`/`NumberToInt` in `cesk/prim.ml`, the VM, `ocaml_backend/codegen_ml`,
  and the `ffi` intrinsic table (name → primop).
- **self-host** — `Purvasm.Compiler.Primitive` and its lowering.

## Consequences

- `Data.Int`'s `toNumber`/`fromNumber` resolve with **no native leaf** — the native surface stays
  at IO + float-bits + `showNumber` (ADR-0038 §4).
- The ABI grows by exactly two primops; the precedent for *cross-representation* conversions is
  set (future `String`/`Char`/`Number` casts follow this rung, not a leaf).
- Per [0040](0040-ulib-testing-strategy.md) `Data.Int` is representation-divergent-adjacent (the
  Int/Number seam), so its behaviour is validated on the native build, and `purvasm-base`'s parity
  suite must cover `ToInt32` edge cases (non-integral, out-of-32-bit-range, `NaN`/`±Inf`, `-0`).
- `NumberToInt`'s `ToInt32` must be implemented as a real total coercion in each backend, not
  `int_of_float` (which is UB outside the host int range); this is the one subtle spot.

## Alternatives considered

- **A native host leaf for `fromNumberImpl`/`toNumber`** (the "host FFI" framing). Rejected: a
  representation cast is not a host *effect*; ADR-0038 §4 shrinks leaves to IO/float-bits/
  `showNumber`. A leaf is also per-backend (re-implemented in each foreign registry) and bundles
  library FFI into the backend, against the minimal-leaf ethos. The intrinsic rung is uniform and
  recognised by all backends.
- **Express it in `ulib` over existing primitives.** Impossible — no existing primop crosses the
  `Int`/`Number` representation boundary; arithmetic stays within one representation.
- **A naive truncation primop (`int_of_float`) + range check in `ulib`.** Rejected: `int_of_float`
  is undefined outside the host int range, and reproducing `(n | 0) === n` then needs the 32-bit
  reduction anyway. Folding `ToInt32` into the primop is total and matches the registry directly.
- **A single fused `numberToMaybeInt` primop.** Rejected: it bakes the `Maybe`/`just`/`nothing`
  plumbing into the backend; the thin `ToInt32` primop keeps the library logic (the `=== n` test)
  in `ulib` where it belongs, and `IntToNumber` is independently needed for `toNumber`.
