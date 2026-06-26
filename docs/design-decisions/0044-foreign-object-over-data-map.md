# 0044. `Foreign.Object a` as a `ulib` newtype over `Data.Map String a`

- Status: Accepted
- Date: 2026-06-26

## Context

The self-host closure reaches `Foreign.Object` — its first unbound foreign at native startup is
`Foreign.Object.toArrayWithKey`. `Object` enters via **Argonaut**: `Data.Argonaut.Core` represents
a JSON object as `Foreign.Object Json`, and the CLI parses `corefn.json` through Argonaut. So
`Object a` needs a native representation, and *whatever produces an `Object Json`* must produce the
same one — `Object` and the JSON-object case are one representation question (this record covers
`Object`; it does **not** open a separate Argonaut ADR — Argonaut's object case simply *is* this
representation, and JSON parsing stays plain PureScript per [0037](0037-self-hosting-purescript.md)).

Three facts frame the representation:

- **`Foreign.Object`'s foreigns are mostly higher-order** — `toArrayWithKey`, `_foldM`,
  `_mapWithKey`, `_lookup` (a continuation `Fn4`), `all`, `_fmapObject`. Per
  [0020](0020-structural-ffi-guest-code.md) a higher-order foreign must be **guest code** (a native
  leaf cannot call back into the guest), so a native-leaf representation is ruled out.
- **There is an ST building path** — `Foreign.Object.ST` (`new`/`poke`/`peek`/`delete`) +
  `runST`/`_copyST` — the same mutable-build shape as `STArray` ([0039](0039-ulib-st-array-and-st-uncurried.md)).
- **Key order**: upstream iterates JS object enumeration order (`for (k in m)`, ~insertion order).
  purvasm has no insertion order, and `boot`'s records are `Map.Make(String)` (sorted). Insertion
  order is unreproducible off-JS; **sorted order is the deterministic, `boot`-consistent choice**.
- **`Data.Map` is foreign-free** on purvasm (`Data.Map.Internal` has zero `foreign import`s) — a
  pure balanced-tree ordered map already available as guest code.

## Decision

### `Object a` is a `ulib` newtype over `Data.Map String a`

Reimplement `Foreign.Object` as `ulib` PureScript over the foreign-free `Data.Map`:

```purescript
newtype Object a = Object (Data.Map.Map String a)   -- constructor NOT exported; Object stays abstract
```

All operations become **ordinary PureScript over `Data.Map`** — `lookup`/`insert`/`delete`/`size`/
`keys`/`values`/`fold*`/`union`/`fromFoldable`/… map directly onto `Data.Map`. This adds **no new
ABI primop, no native leaf, and no new runtime value type** — the [0038](0038-base-package-and-ulib-patches.md)/
[0039](0039-ulib-st-array-and-st-uncurried.md) line (prefer `ulib` over an existing pure structure;
do not grow the ABI). The higher-order operations are plain `App`s of their callbacks, so
[0020](0020-structural-ffi-guest-code.md)'s "no native re-entrancy" holds without any leaf.

### Scope: the whole `Object` `ulib` family is this ADR

All `Object`-related modules are interface-compatible reimplementations under this record:
`Foreign.Object`, `Foreign.Object.ST`, `Foreign.Object.Unsafe`, `Foreign.Object.ST.Unsafe`,
`Foreign.Object.Gen` — public surface unchanged, `Object`/`STObject` kept abstract.

### ST building mirrors `STArray`

`STObject r a` is an `STRef` holding the immutable `Map` (the [0039](0039-ulib-st-array-and-st-uncurried.md)
shape): `new` allocates a ref over `Map.empty`; `poke`/`delete` write an updated `Map` back into the
ref; `peek` reads-and-looks-up; `runST`/`_copyST` freeze/clone the `Map` out of the ref. Because a
`Map` is persistent, `freeze`/`copy` are O(1) sharing (no trim-copy cost — unlike `STArray`).

### Order is sorted — a documented divergence

`keys`/`toArrayWithKey`/`fold` iterate in `Ord String` (sorted) order, **not** JS enumeration order.
This is a deliberate representation divergence (like `String`/`Char` UTF-8, [0006](0006-string-utf8-char-int.md)):
deterministic, matches `boot`'s `SMap`, and the only reproducible choice off-JS. Per
[0040](0040-ulib-testing-strategy.md) `Foreign.Object` is representation-divergent → validated on the
native build, with key order an expected divergence in its `xfail` set. (No closure use is known to
depend on JS order; JSON decode looks keys up by name, and sorted order is the safer default for any
iteration-derived output determinism.)

## Consequences

- `Object` resolves with **zero new ABI / leaf / value type** — pure `ulib` over `Data.Map`.
- One ordered-map representation (`Data.Map`) is reused for both `Map` and `Object`; `Object`'s ST
  reuses the `STArray` STRef-over-immutable pattern, but with O(1) freeze (persistent map).
- The Argonaut entanglement dissolves: `Object Json` *is* this `ulib` representation, so the JSON
  object case and `Object` share it with no separate decision.
- A sizable but mechanical `ulib` package (`ulib/foreign-object/`, five modules); order is sorted,
  flagged for the native test + `xfail`.
- `Foreign.Object` joins the representation-divergent set ([0040](0040-ulib-testing-strategy.md)).

## Alternatives considered

- **Reuse the record value (`VRecord`/`SMap`) + new dynamic map primops.** Records are static
  (`Record`/`Accessor`/`Update` only); dynamic `insert`/`delete`/`lookup` would need new primops
  (ABI growth), the higher-order ops would still need guest code, and it conflates two types. Rejected.
- **Association array (`VArray` of pairs) + structural guest terms.** No new type, but hand-rolled
  sorted-array maintenance + binary search, O(n) insert, and easy to get subtly wrong. `Data.Map`
  already is the correct, tested structure. Rejected.
- **A new `VObject` runtime value type (hashmap/tree).** ABI growth for a library structure — the
  same call rejected for `STArray` ([0039](0039-ulib-st-array-and-st-uncurried.md)). Rejected.
- **Native leaves for the `Object` foreigns.** Impossible for the higher-order ones
  (`toArrayWithKey`/`_foldM`/…): a first-order leaf cannot call back into guest code
  ([0020](0020-structural-ffi-guest-code.md)).
- **Preserve JS insertion order** (e.g. a parallel insertion-index). Extra machinery for an order
  that is unreproducible off-JS and not needed; sorted is deterministic and `boot`-consistent.
