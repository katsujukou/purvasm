# 0042. `Data.Number` math family as JS-faithful native leaves (not primops, not `ulib`)

- Status: Accepted
- Date: 2026-06-26

## Context

The self-host closure reaches `Data.Number` math foreigns through the pure-PureScript MD5
(`Purvasm.Compiler.Util.MD5`, the `.pmi` interface hash): it imports `Data.Number (abs, floor,
sin)` — its `K` table is `abs (sin i)` — and `Data.Int (round)`, where `Data.Int.round =
unsafeClamp <<< Number.round` ([0041](0041-int-number-conversion-primops.md)). So the demand is
the **`Data.Number` math family** (`abs`, `floor`, `round`, `sin` now; `ceil`, `trunc`, `sqrt`,
`cos`, `tan`, `exp`, `log`, `pow`, `atan2`, `min`, `max`, `sign`, `remainder`, … on demand), not a
single `round`.

These are pure `Number -> Number` (or `Number -> Boolean`) operations with **no conversion
primitive analogue**: unlike `Int`↔`Number` ([0041](0041-int-number-conversion-primops.md)), they
cannot be expressed in `ulib` over existing primitives — `sin`/`sqrt`/`log` are transcendental, and
`floor`/`round` over the full double range cannot be built from the range-limited `ToInt32`
conversion. So the rung is a genuine choice: a new `Purvasm.*` primop family, or native leaves.

Two facts shape it:

- **The family is open-ended and libm-shaped.** `Data.Number` has ~25 foreigns, most of them libm
  calls (`sin`/`cos`/`sqrt`/`log`/`exp`/`pow`/`atan2`/…). This is a *host math library*, not the
  machine's core instruction set.
- **`Math.*` semantics are not OCaml `Float.*` semantics.** Notably `Math.round` rounds half
  **toward +∞** (`Math.round (-2.5) = -2`), whereas OCaml `Float.round` rounds half **away from
  zero** (`-3`). A naive `Float.round` leaf would be wrong. Faithful `round` is `floor (x + 0.5)`
  with the ECMAScript `NaN`/`±0`/`±∞` special cases.

## Decision

### The `Data.Number` math foreigns are native leaves (ADR-0022), demand-driven

Resolve them on the **native rung** of the FFI ladder, like `showNumber`
([0038](0038-base-package-and-ulib-patches.md) §4): a host implementation in `Ffi.host` (for the
CESK oracle and the VM) and a re-implementation in the `codegen_ml` `Rt` prelude over OCaml's
`Float.*` (for the native backend), the differential enforcing parity across the three. Leaves are
added **as the closure reaches them** (the minimal-FFI policy) — `abs`, `floor`, `round`, `sin`
first.

### `Data.Number` is used unpatched — no `ulib`, no `Purvasm.*` primop

The registry `Data.Number.purs` keeps its `foreign import`s, which resolve to the native leaves;
its own `Math.*` JS foreigns still serve stock `purs` / `purs-backend-es`, so **dual-target is
preserved with no `ulib` module and no `Purvasm.*` addition** — the lowest-maintenance option.

### Leaves must match ECMAScript `Math.*`, not OCaml `Float.*`

Each leaf reproduces the JS semantics the registry `.js` specifies, not OCaml's default:

- **`round`** = round-half-**toward +∞** (`floor (x +. 0.5)`), with `NaN`/`±∞` → themselves and the
  `-0`/`+0` cases per ECMAScript — *not* OCaml `Float.round` (half-away-from-zero).
- **`abs`/`floor`/`ceil`/`trunc`** = OCaml `Float.abs`/`floor`/`ceil`/`trunc` (these already agree
  with `Math.*`).
- **`sin`/`sqrt`/`log`/…** = the platform libm (OCaml and JS share it, so they agree).

The differential (oracle = VM = native) and, per [0040](0040-ulib-testing-strategy.md), the
native-side `Data.Number` tests guard the edges (`round` halves incl. negatives, `-0`, large
values, `NaN`/`±∞`).

### Rung rationale — why a leaf here but a primop in ADR-0041

`Int`↔`Number` is a **core scalar-representation conversion** the machine must own (it is the int
width / IEEE seam itself), so it is a primop in the ABI ([0041](0041-int-number-conversion-primops.md)).
The math family is a **host math library** — open-ended, libm-backed, with platform/JS-specific
semantics (`round`) — so it is a native leaf, the same call as `showNumber`. Putting libm into the
primop set would bloat the ABI contract ([0038](0038-base-package-and-ulib-patches.md) §1) with
operations that are not the machine's instruction set.

## Consequences

- The native-leaf surface grows by the `Data.Number` math family — but these *are* the
  "genuinely-native" leaves ADR-0038 §4 anticipates (libm + JS-faithful math), alongside IO /
  float-bits / `showNumber`.
- No `ulib` patch and no `Purvasm.*` primop for `Data.Number`: the registry module is used as-is,
  dual-target intact, maintenance minimal.
- `round`'s round-half-toward-+∞ (and `-0`) must be implemented deliberately; the differential and
  the parity suite ([0040](0040-ulib-testing-strategy.md)) are the guard.
- Precedent: the rung split is now explicit — *representation conversions* are primops (0041),
  *host math* is a native leaf (this record). Future `Number` math needs follow this rung.

## Alternatives considered

- **`Purvasm.Number` math primops + a `ulib` `Data.Number`** (the ADR-0041 shape). Rejected: it
  bakes an open-ended libm/transcendental family into the ABI primop set — far beyond the
  machine's core — and splits the family unnaturally. Conversions are core ABI; libm is not.
- **Express the rounding family in `ulib` over the `ToInt32` conversion** ([0041](0041-int-number-conversion-primops.md)).
  Impossible: `ToInt32` is range-limited (wraps outside ±2³¹) so it cannot implement full-range
  `floor`/`round`, and `sin`/`sqrt`/… are transcendental — no arithmetic expression exists.
- **A naive OCaml `Float.round` leaf.** Rejected: it rounds half away from zero, diverging from JS
  `Math.round` (half toward +∞) on negative halves; the leaf must be JS-faithful (`floor (x + 0.5)`).
