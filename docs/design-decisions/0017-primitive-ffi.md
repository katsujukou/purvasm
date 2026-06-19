# 0017. Primitive FFI: foreign leaves as eta-expanded primops

- Status: Accepted
- Date: 2026-06-19

## Context

[0016](0016-cross-module-linking.md) (Accepted) links a program's modules but
leaves the **FFI resolver empty**, so any `foreign import` is unbound and `stuck`
if forced. `Prelude` bottoms out in such leaves: the primitive type-class methods
are `foreign import`s whose implementations are JS (`Data.Semiring.intAdd`,
`Data.Eq.eqIntImpl`, `Data.HeytingAlgebra.boolConj`, …). purvasm runs no JS, so it
must supply these natively. This is wall ② of the `Prelude`-only milestone.

The cheapest, highest-value subset is the **first-order scalar leaves** — the
arithmetic/comparison/boolean operations that *are* the machine's primitives.
[0007](0007-monomorphic-primitives.md) §3 deferred exactly this "qualified FFI
ident → primitive" table to the frontend; this record is it. Leaves that are
higher-order (`Data.Functor.arrayMap`) or need real native code belong to the
next slice (higher-order native FFI).

More broadly, foreign resolution is naturally **layered**: a name is an intrinsic,
or a system call, or a user-defined foreign, or nothing — tried in that priority.
So the resolver is built as an ordered **provider ladder** from the start, even
though this slice fills only its first rung.

A key payoff is already set up: once these leaves resolve, **type-class
dictionaries need no special machinery** — a dictionary is a plain `VRecord` of
closures ([0007](0007-monomorphic-primitives.md)/[0010](0010-record-row-polymorphic.md)),
so `add dictSemiringInt x y` reduces through ordinary record/closure evaluation to
`intAdd x y` and then to a primop.

## Decision

### Foreign resolution is a *provider ladder*

A foreign name is resolved by an **ordered chain of providers**, each consulted in
turn, the first match winning, falling through to `stuck` if none apply:

```text
{foreign name}
  ├─ intrinsic?     ─yes→ primitive             (e.g. Data.Semiring.intAdd)
  ├─ syscall?       ─yes→ system call           (e.g. filesystem IO)
  ├─ user foreign?  ─yes→ opaque native closure (e.g. Effect.Console.log)
  └─ otherwise      ────→ stuck
```

The implementation is this ladder from the start: the resolver
[0016](0016-cross-module-linking.md) consumes (`qualified key → Cesk.Ast.term
option`) is the **first-match fold over a list of providers**. **This slice
implements only the first rung — intrinsics** — and registers it as the sole
provider; the syscall and user-foreign rungs are later providers appended to the
same list, with linking unchanged.

An **intrinsic** maps a leaf's qualified ident to a core **primop**, returned as
an **eta-expanded curried lambda** wrapping it — an arity-`n` primop becomes
`\x₁ … \xₙ -> Prim(op, [x₁; …; xₙ])`. Because that is an ordinary closure, a leaf
used **partially or first-class** (`map intAdd`) works for free, and currying
falls out (CoreFn applies leaves curried).

The ladder lives in a **new `ffi` library** (depending on `cesk`); the driver
composes the providers and passes the resulting resolver to `Link.link`, so
linking stays FFI-agnostic ([0016](0016-cross-module-linking.md)'s resolver
parameter). The result type is `term option` for now; when the **syscall /
user-foreign** rungs land they return *native / opaque closures*, broadening the
provider result type and needing the higher-order-FFI machine extension
(`VForeign`, next slice) — the ladder's *shape* is unchanged by that.

### First batch of leaves

The scalar primitive classes — **taken from the `prelude` package's foreign
modules and pinned against them during implementation, not guessed**:

- **Semiring / Ring / EuclideanRing** for `Int` and `Number` — the four
  arithmetic operations plus integer `div`/`mod`.
- **Eq** for `Int` / `Number` / `String` / `Char` / `Boolean`.
- **HeytingAlgebra** for `Boolean` (conj / disj / not).
- **Semigroup** string concatenation.

`Char` reuses the `Int` primops (Char *is* Int, [0006](0006-string-utf8-char-int.md)),
so `eqCharImpl` maps to the same primop as `eqIntImpl`.

### New primops this batch needs

`DivInt`, `ModInt` (truncating, matching the JS `intDiv`/`intMod`; division or
modulo by zero yields `0` as the JS FFI does — noted because it differs from
OCaml's exception), `EqBool`, and boolean `AndBool` / `OrBool` / `NotBool`. The
existing primops cover the rest (`AddInt`/`SubInt`/`MulInt`, the `Number` ops,
`EqInt`/`EqNumber`/`EqString`, `Append`).

### Deferred to later batches / the higher-order slice

- **Ord** (`ord*Impl` picks among caller-supplied `LT`/`EQ`/`GT` values): not a
  single primop but a **composite guest term** (`\lt eq gt x y -> if … then …`),
  so a later batch — and a reminder that the resolver returns a *term*, not only a
  primop.
- **Show** (needs new string-producing primops and escaping).
- **`concatArray`** and all **higher-order** leaves (`arrayMap`, `eqArrayImpl`):
  these need native code that calls guest closures — the higher-order native FFI
  slice. The registry is extensible; they slot in without touching linking.

## Consequences

- **Real `Prelude` programs run end to end** for arithmetic / equality / boolean /
  string: their dictionaries build because the foreign leaves now resolve, and
  `add dict x y → intAdd x y → Prim(AddInt, …)`. The "dictionaries are free"
  prediction ([0007](0007-monomorphic-primitives.md)/[0010](0010-record-row-polymorphic.md))
  becomes concrete.
- **Minimal machine change** — only new primops; the FFI itself is *pure guest
  terms* (no new `Value` variant, no re-entrancy). This is the cheap slice, as the
  roadmap intended; the machine extension waits for higher-order FFI.
- **Eta-expansion unifies currying and partial application** with ordinary
  closures, so leaves behave like any other function.
- **The provider ladder is the extensibility seam.** Adding system calls or
  user-defined foreign is *appending a provider*; linking and the intrinsic rung
  are untouched. The result type broadens from `term` to include native closures
  when those rungs land (with the `VForeign` extension).
- **Testing becomes real PureScript:** a `Prelude` program compiled via `spago`
  (the program + its transitive `prelude` corefn) linked with this resolver, run,
  and asserted — the first genuinely real-PureScript E2E. The fixture is larger
  (many prelude modules) but it is the actual artifact.
- **Faithfulness pinned to the source:** the ident set and the `div`/`mod`-by-zero
  behaviour are matched to the `prelude` package's FFI, consistent with the
  project's "conform to a verified source, do not guess" stance.

## Alternatives considered

- **Bind leaves to native (`VForeign`) functions** instead of guest terms.
  Rejected for this batch: a scalar op *is* a primop, and a guest eta-expansion
  needs no machine change and gives currying / partial application for free.
  Native values are required only for higher-order leaves (next slice).
- **One polymorphic primop dispatching on value shape.** Rejected
  ([0007](0007-monomorphic-primitives.md)): leaves are per-type and monomorphic;
  the registry maps each ident to its monomorphic primop, with `Char` folding onto
  `Int`.
- **Put the table inside the lowering / linking library.** Rejected: keep it a
  separate `ffi` registry passed into `Link.link`, so linking stays FFI-agnostic
  and the higher-order native FFI joins the same ladder rather than a second
  mechanism.
- **A single flat table instead of an ordered provider ladder.** Rejected: foreign
  resolution is inherently layered (intrinsic, then syscall, then user foreign,
  with priority and fall-through); an ordered provider chain models that directly
  and lets each rung land as an independent slice, whereas a flat table conflates
  the kinds and has no fall-through story. The first-match fold also *is* a flat
  table when there is one provider, so the ladder costs nothing now.
- **Eliminate dictionaries by recognising `Data.Semiring.add dict …` at lowering**
  (inline the method). Rejected as the way in: that is the *dictionary-elimination
  optimisation* (needs to pattern-match the dictionary) and belongs to the
  optimisation axis ([0007](0007-monomorphic-primitives.md), the sketch);
  resolving the leaves runs unoptimised dictionaries *correctly* first, and the
  optimisation can come later validated against the interpreter.
