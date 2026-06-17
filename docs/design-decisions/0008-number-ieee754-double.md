# 0008. Number as an IEEE 754 double (host float)

- Status: Accepted
- Date: 2026-06-17

## Context

`Number` was missing from the value representation — the core had `Int`, `Bool`,
`String`, and closures, but not PureScript's `Number`. PureScript's `Number` is a
**double-precision IEEE 754 float** (the JavaScript `number`).

Unlike `String` ([0006](0006-string-utf8-char-int.md)), where we deliberately
rejected JavaScript's UTF-16 representation, there is nothing JS-specific to shed
here: **IEEE 754 is a language-neutral standard** that OCaml's `float` and JS's
`number` both implement identically. So `Number` maps directly onto the host
OCaml `float`, with no semantic divergence to design around. The one subtlety is
getting NaN/ordering semantics to match, which is an implementation note, not a
representation choice.

## Decision

1. **`Number` is the host OCaml `float` (IEEE 754 double).** Add
   `Value.VNumber of float` and `Ast.LNumber of float`. A number literal is the
   host float verbatim.

2. **Monomorphic, type-suffixed primitives** (per
   [0007](0007-monomorphic-primitives.md)): `AddNumber`, `SubNumber`,
   `MulNumber`, `DivNumber`, `EqNumber`, `LtNumber`. `Number` has a total `/`
   (no exception: `1.0 /. 0.0 = inf`, `0.0 /. 0.0 = nan`), so `DivNumber` is
   included as a base operation. `Int` and `Number` are **distinct** primitives;
   well-typed CoreFn never mixes them (Semiring Int vs Semiring Number are
   different instances → different FFI idents).

3. **Semantics are exactly host-float semantics, which coincide with
   PureScript/JS.** Implement `EqNumber` / `LtNumber` with the **IEEE float
   comparison operators** (`Float.equal` / float `<`), explicitly **not** OCaml's
   polymorphic `compare` or `Float.compare` (which impose a total order that
   makes `nan` orderable). This is what makes:
   - `EqNumber nan nan = false` (matches JS `NaN === NaN` → false),
   - `LtNumber nan _ = false` and `LtNumber _ nan = false`,
   - `±0.0`, `±inf` behave as IEEE specifies,

   so equality and ordering match PureScript's `Eq`/`Ord Number` (which are JS
   `===`/`<`, not a total order).

## Consequences

- **Zero divergence from PureScript** — IEEE 754 is shared ground, so the host
  `float` *is* the representation and the spec is just "host float semantics".
  This is the clean counterpart to [0006](0006-string-utf8-char-int.md), where we
  *did* diverge.
- **NaN handling falls out for free**, provided we use the IEEE operators and
  avoid polymorphic/`Float.compare`. The risk is using the wrong comparison by
  habit; the tests pin `nan` behavior so a regression is caught.
- **Literal/trace display:** `VNumber` prints via the host float formatter
  (round-trippable); trace-only, no semantic weight.
- **`Int` division stays absent.** Only `Number` gets `/` now (its base op);
  `Int`'s `div`/`mod` are added if and when CoreFn needs them, as separate
  `Int`-suffixed primitives.

## Alternatives considered

- **Arbitrary-precision / decimal `Number`.** Rejected: PureScript's `Number`
  *is* an IEEE 754 double; fidelity to the language requires the same
  representation, not a "better" numeric type.
- **Impose a total order on `Number`** (treat `nan` as orderable via
  `Float.compare`). Rejected: it diverges from PureScript's `Ord Number`
  (JS `<`), silently giving wrong answers for `nan`/sort behavior. Order must
  match the language, so use IEEE `<`.
- **Fold `Number` into `Int`, or build a numeric tower.** Rejected: they are
  distinct PureScript types with distinct type-class instances; keeping them as
  separate monomorphic primitives is exactly [0007](0007-monomorphic-primitives.md).
