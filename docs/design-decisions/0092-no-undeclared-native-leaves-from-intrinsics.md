# 0092. Structural intrinsics introduce no undeclared native leaves

- Status: ~~Proposed~~ **Accepted** _(2026-07-11: accepted by the maintainer)_
- Date: 2026-07-11

## Context

ADR-0090 made a **`foreign import`'s reconstructed signature the single source of
truth** for a native leaf's shape (arity / effect / vsat): FSR reads the source
`foreign import`, the backend lowers `@pvf_<key>` with that arity, and a
`provider`/`.c`/Rust supplies the symbol. No hidden arity tables (we just deleted
the last `fromMaybe 1` fallback).

A compiler **structural intrinsic** (ADR-0038/0046) desugars a foreign the
compiler chooses to own into a **guest term** — a composition of `TmApp`/`TmLet`/
`TmIf`/`TmPrim` over other terms. That is legitimate. What is **not** legitimate,
under ADR-0090, is for such a term to reference a `TmForeign` **native leaf that
no `foreign import` declares**: FSR then has no signature to reconstruct, so the
leaf's arity is unknown and codegen crashes.

This is exactly what `Data.Number.fromStringImpl` does today
(`Ffi.purs:310`, ADR-0046):

    numberFromStringImpl = lams ["str","isFin","just","nothing"]
      ( TmLet "n" (TmApp (TmForeign "Data.Number.parseFloatImpl") (v "str")) … )

`Data.Number.parseFloatImpl` is **invented by the compiler** — numbers-9.0.1's
source has no such `foreign import` (it declares `fromStringImpl`, an `Fn4`). So:

- FSR never sees `parseFloatImpl` → its arity is absent from `foreignArity`;
- `synthForeignGdefs` emits the `fromStringImpl` gdef for **every** program that
  pulls in `Data.Number` (all of a module's foreigns are materialised, used or
  not) → the gdef body carries `AtomForeign "Data.Number.parseFloatImpl"` →
  `Backend.LLVM.Emit.atom: missing native foreign arity` crash.

The invented name also looks like a registry package's *internal* foreign, so the
compiler is minting — and permanently ABI-freezing — a `pvf_Data_2eNumber_…`
symbol in a namespace it does not own. This is a back door around the ADR-0090
invariant, and `parseFloatImpl` is the alarm that surfaced it.

## Decision

### The invariant (pinned)

> **Level2+ native backend introduces no undeclared native leaves from compiler
> structural intrinsics.** Any native leaf a structural intrinsic requires must be
> surfaced as a real `foreign import` in `Purvasm.*` / ulib source, so ADR-0090
> FSR supplies its shape. Structural intrinsics stay at guest-term composition and
> reference only *declared* foreigns; they never mint a fresh `TmForeign` key.

This generalises beyond `parseFloat` — the same rule decides the Number-math,
String-bytes, FS, and System frontiers: the compiler does not fabricate a floor;
the floor lives in `Purvasm.*`.

### The `Purvasm.*` ABI floor is an implicit FSR-visible dependency (pinned)

A structural intrinsic references `Purvasm.Number.parseFloat` even in a program
whose CoreFn never `import`s `Purvasm.Number`, so the ordinary
"a module's foreigns are FSR-visible iff it is in the source-import closure" rule
is **not** enough. We do not lean on `Purvasm.Number` incidentally entering the
closure (via, say, its `floatBitsHi/Lo` arithmetic leaves). Instead we pin the
project convention that already governs `Purvasm.*`:

> `ulib` overlays are registry-package implementations over the **purvasm-base
> floor**; therefore purvasm-base is an **implicit dependency of every
> `ulib`/provider context**. `purvasm-base` sits between `Prim` and `Prelude` and
> exposes only the low-level ABI surface (foreigns the compiler resolves to
> intrinsics / runtime `pv_*`, plus their JS stubs). A compiler structural
> intrinsic may reference a `Purvasm.*` leaf **only if that leaf is declared as a
> real `foreign import` in purvasm-base**, and the build seeds FSR with the
> source-derived signatures of the reachable `Purvasm.*` ABI modules. This is not
> a hidden arity table: the shape still comes from the `foreign import` signature
> in `packages/purvasm-base/src/Purvasm/…`; what is made explicit is only that the
> `Purvasm.*` ABI floor is always FSR-visible to `ulib`/structural-intrinsic code.

Implemented in `ForeignSigs.loadAbiFloorSigs`: the cache-db entries whose `.purs` lives under
`purvasm-base` are reconstructed from source once and **unioned into every module's `foreignSigs`**,
so a leaf's arity is present regardless of the loaded closure — it does not rely on the leaf's owning
module happening to be pulled in. Only genuine native leaves survive `nativeLeafArities` (the
`Purvasm.Number` arithmetic ops resolve to intrinsics and are filtered out), so the union does not
pollute `foreignArity`.

### Concretely for `parseFloat` (this ADR's migration)

1. **Declare** `foreign import parseFloat :: String -> Number` in
   `packages/purvasm-base/src/Purvasm/Number.purs` — a real leaf beside its
   siblings `floatBitsHi/Lo`, `add`, `sub`, …
2. **Provide** `pvf_Purvasm_2eNumber_2eparseFloat` from the **runtime**
   (`runtime/src/leaf.rs`), where the sibling `Purvasm.Number` leaves already
   live. The parse logic moves there from `ulib/numbers/Data.Number.c`.
3. **Repoint** the `numberFromStringImpl` structural intrinsic to
   `TmForeign "Purvasm.Number.parseFloat"`. It stays pure guest-term composition;
   only the referenced leaf changes from invented to declared.
4. **Retire** the invented leaf: drop `Data.Number.parseFloatImpl` from
   `ulib/numbers/ulib.json`'s `foreign` map and its impl from `Data.Number.c`
   (it never matched a numbers-9.0.1 source foreign anyway).

`parseFloat`'s arity comes from FSR reconstructing `Purvasm.Number.parseFloat`
(arity 1) from purvasm-base source, made available by the ABI-floor rule pinned
above — not from `Purvasm.Number` incidentally reaching the closure.

### `fromStringImpl` stays a compiler structural intrinsic

`Data.Number.fromStringImpl` is genuinely structural — it wires the caller's
`Fn4` args `isFinite`/`just`/`nothing` around the leaf so a NaN/non-finite parse
becomes `Nothing`. Once `parseFloat` is a declared purvasm-base leaf, keeping
`fromStringImpl` as the existing structural intrinsic (only its referenced leaf
changes from invented to declared) satisfies the invariant with the smallest diff
and no new module. We do **not** push it out to a `ulib` PS shadow of
`Data.Number`: that would make `Data.Number` a **ulib-overlaid** module, flipping
its FSR provenance from `fromSource` to `fromUlib` and so requiring
author-declared `foreignSigs` for **all** of `Data.Number`'s foreigns plus a
staged corefn overlay — cost with no benefit here, since the ABI-floor rule
already explains FSR visibility without a source `import`.

## Consequences

- The ADR-0090 invariant holds with no exceptions: every `@pvf_` the backend emits
  traces to a declared `foreign import`. No hidden compiler-side arity registry
  (A) and no shape-inference from untyped `TmForeign` (C) — both explicitly
  rejected.
- `Purvasm.Number.parseFloat` becomes the owned, stable ABI symbol; the
  `Data.Number`-namespaced invented symbol is retired before it ossifies.
- A regression fixture (a `Data.Number.fromString` program that builds, links
  against the runtime provider, and runs) guards the frontier.
- Actionable rule for future leaves (String bytes, FS, System): declare the floor
  in `Purvasm.*`, never mint it inside an intrinsic.

## Alternatives considered

- **(A) Compiler-side leaf registry** mapping invented leaves → arity, unioned
  into `foreignArity`. Rejected: reintroduces the hidden arity table ADR-0090
  removed; the shape's truth leaves the `foreign import`.
- **(C) Recover arity by scanning `TmForeign` in synthesised terms.** Rejected:
  inferring shape from an untyped internal term is the opposite of FSR; a
  `TmForeign` carries no signature, so the recovery source is again a side table.
- **Leave `parseFloatImpl` invented, patch only its arity.** Rejected: keeps the
  compiler minting a foreign in a namespace it does not own and permanently
  ABI-freezing it.
