# 0046. JSON in pure PureScript: a backend-agnostic shared parse/print core, with `argonaut-core` as a `ulib` adapter over it

- Status: Accepted
- Date: 2026-06-27

## Context

The native self-host CLI ([[selfhost-native-bringup]]) runs arg-parse → dispatch → `readText`
(reads `corefn.json`) and stops at its next unbound foreign: `Data.Argonaut.Parser._jsonParser`.
The CLI parses compiler input through **Argonaut**, whose parsing seam is JavaScript-only:
`Data.Argonaut.Core` declares `foreign import data Json :: Type` (the value *is* a `JSON.parse`
result), and `Data.Argonaut.Parser.jsonParser` is `runFn3 _jsonParser` over a `JSON.parse`
foreign.

The grand goal is a **production-grade** machine that executes PureScript
([0001](0001-phase-1-host-language-ocaml.md)), not merely whatever the self-host's own closure
happens to touch today. The **demand-driven** discipline ([0037](0037-self-hosting-purescript.md))
governs *FFI/host-leaf growth* — do not bolt host leaves onto the backend speculatively — it is
**not** a licence to ship a JSON layer that only works for the one library the compiler currently
imports. A production JSON story must cover the ecosystem's real JSON libraries.

Two are in real-world use, with deliberately different representations:

- **`argonaut-core`** — `Json` is a raw JS value; objects are `Foreign.Object Json`, arrays are
  `Array Json`. Brings the `argonaut-codecs` `.:`/`decodeJson` ecosystem.
- **`json` (purescript-json)** — **backend-agnostic by design**; its README states it "does not
  use `Foreign.Object` as the representation for JSON objects, does not use `Array JSON`, and
  instead provides its own `JObject` and `JArray` types." Only argonaut is in the closure *today*
  (verified: `.spago/p` has only `argonaut*`; nothing imports the `JSON` module), but the
  backend-agnostic sibling is squarely in scope for a production-grade non-JS backend.

Facts that shape the fix:

- **The `ulib` overlay is corefn-only and drops the registry `.js`** ([0038](0038-base-package-and-ulib-patches.md)):
  an overlay that keeps a `foreign import` resolves it to a boot intrinsic / native leaf. So a
  `ulib` `Data.Argonaut.Core`/`Parser` must be **foreign-free** (or name only existing leaves).
- **`argonaut-codecs` has zero `foreign import`s** (verified across `argonaut-codecs-9.1.0/src`;
  no `.js`). Its decoders/encoders — including the `(.:)`/`(.:?)`/`decodeJson` the compiler's
  `PureScript.CoreFn.Decode` uses — are plain PureScript over Core's **public API**. Swapping only
  Core's *representation* leaves the whole codec stack and `Decode.purs` compiling unchanged.
- **The JSON object case is already `ulib`** ([0044](0044-foreign-object-over-data-map.md)):
  `Object Json` is `Foreign.Object` over `Data.Map String Json`, with [0037](0037-self-hosting-purescript.md)
  having already deferred "the Argonaut representation" to a later record. This is that record.
- **The parser and stringify are the same grammar walk for both libraries** — an identical
  lexer + recursive descent and an identical structural serialiser; the *only* difference is how a
  node is constructed (argonaut's `Foreign.Object`+`Array` vs `json`'s `JObject`+`JArray`) and the
  final value type. Writing them per-package — or writing argonaut's now and refactoring into a
  shared form when `json` arrives — means **touching and re-testing `argonaut-core` twice**. The
  shared core is cheap insurance only if it is built *with the first consumer*, not retrofitted.
- **`purvasm-base` is not a home for this.** Per [0038](0038-base-package-and-ulib-patches.md) §1,
  `Purvasm.*` is *the true primitives the backend recognises as intrinsics* and **is the ABI
  contract** (`dependencies: []`). A JSON parser is ordinary library code (needs Prelude/`Either`/
  `Tuple`/`Data.Number`), not a primitive — it cannot live there. And it is consumed by *two*
  registry overlays, so it is not a patch belonging to either. It needs a **third home**:
  purvasm-authored shared library PureScript, distinct from the `Purvasm.*` ABI base and the
  registry-patch `ulib`.

## Decision

### 1. A backend-agnostic shared JSON core — built now

Introduce purvasm-authored, foreign-free PureScript (in the `purvasm-json` package, §4) that
implements JSON parsing and printing once, parametrised over node construction/elimination so any
representation can ride it:

```purescript
-- parse: build nodes through a representation-neutral builder
type Builder j =
  { jnull    :: j
  , jboolean :: Boolean -> j
  , jnumber  :: Number -> j
  , jstring  :: String -> j
  , jarray   :: Array j -> j
  , jobject  :: Array (Tuple String j) -> j   -- neutral entries; adapter folds into its own type
  }
parse :: forall j. Builder j -> String -> Either String j

-- print: structural serialiser over a caseJson-style eliminator
print :: forall j. Eliminator j -> j -> String
```

- The lexer + recursive-descent control flow and the serialiser live **here, once**. Object and
  array representations are decoupled via the neutral `Array (Tuple String j)` / `Array j`; each
  adapter folds those into its own type.
- A numeric lexeme converts to `Number` via `Data.Number.fromString` — the **one demand-driven
  native leaf** this pulls in (`fromStringImpl`; not yet wired — boot has `float_of_string` but no
  binding), host-faithful `strtod` mirroring boot's own number handling
  ([0042](0042-data-number-math-native-leaves.md) family, [0008](0008-number-ieee754-double.md)).
  *(Maintainer-confirmed: keep this as a native leaf rather than hand-rolling float parsing.)*
- **String-escape handling (`\"\\\/\b\f\n\r\t` and `\uXXXX` incl. surrogate pairs) under
  [0006](0006-string-utf8-char-int.md) UTF-8 strings is the one fidelity-sensitive spot** and is
  the focus of the core's bespoke UTF-8-aware suite.
- No FFI beyond the `fromString` leaf, so it also builds on stock `purs` (the dual-target spirit
  of [0038](0038-base-package-and-ulib-patches.md)).

### 2. `argonaut-core` as a `ulib` adapter over the core — now

Overlay `Data.Argonaut.Core` and `Data.Argonaut.Parser`, foreign-free, as a thin adapter:

- `Json` becomes a pure-PS ADT, abstract (constructors not exported), so the public surface is
  unchanged:

  ```purescript
  data Json = JNull | JBoolean Boolean | JNumber Number | JString String
            | JArray (Array Json) | JObject (Object Json)   -- the [0044] Foreign.Object
  ```

- The whole exported surface (`caseJson`/`is*`/`to*`/`from*`/`json*` defaults/`Eq`/`Ord`) is
  ordinary guest code over the ADT — `caseJson` pattern-matches instead of `runFn7 _caseJson`,
  `Eq`/`Ord` become structural (faithful to upstream's tag-then-value order). `stringify`
  delegates to the core's `print` with an argonaut eliminator.
- `jsonParser = parse argonautBuilder`, where `argonautBuilder` constructs `JArray`/`JObject`
  (objects via `Foreign.Object.fromFoldable`).
- `argonaut-codecs` and `PureScript.CoreFn.Decode` are **untouched** (no FFI; ride Core's API).
  `Data.Argonaut.Gen` (unused) is not ported.

### 3. `purescript-json` is deferred — but core-ready

Not built now (nothing reaches it; speculative *packages* are avoided just as speculative *leaves*
are). When the closure reaches it, it is a **thin adapter** — a `JSON`-builder/eliminator over its
own `JObject`/`JArray` — and the shared core is reused **untouched**; `argonaut-core` is **not**
revisited. Avoiding that rework is precisely why §1 is built now rather than retrofitted.

### 4. Home of the shared core — a new in-repo `spago` package

A **new self-contained in-repo `spago` package, `packages/purvasm-json`** (namespace
`Json.Core.*`), a sibling of `purvasm-base` — a third source category beside the `Purvasm.*` ABI
base ([0038](0038-base-package-and-ulib-patches.md) §1) and the registry-patch `ulib`. In-repo
source packages are consolidated under a new **`packages/`** directory (`purvasm-base` moves to
`packages/purvasm-base`; `purvasm-json` is created beside it). The namespace is deliberately
**not** `Purvasm.*`, to avoid blurring that namespace's ABI-contract meaning
([0038](0038-base-package-and-ulib-patches.md) §1); the package is dual-target (builds on stock
`purs`) like `purvasm-base`, and carries **its own `spago` test suite** — the natural home for the
core's bespoke UTF-8 escape/Unicode suite. Both the `argonaut-core` overlay and the future `json`
overlay depend on it.

`ulib` is left **untouched** as "patches on registry packages, never a from-scratch API"
([0038](0038-base-package-and-ulib-patches.md) §2) — the shared core is original library code, not
a patch, so it does not belong there. `ulib-tools`/`install.sh`
([0043](0043-ulib-tools-build-verify-test.md)) is generalised from special-casing `purvasm-base`
to copying **all `packages/` source packages' `src`** (`packages/purvasm-base` + `packages/purvasm-json`)
into the overlay compile set — a natural generalisation, not a new mechanism.

### Scope

- **Now:** the shared core (§1) + the `argonaut-core` overlay (`Data.Argonaut.Core` + `…Parser`,
  §2). One new native leaf (`fromStringImpl`). Manifest entry per
  [0043](0043-ulib-tools-build-verify-test.md): `fidelity: native`, escape/Unicode handling in a
  bespoke UTF-8 suite, code-unit/order assumptions recorded as `xfail` ([0040](0040-ulib-testing-strategy.md)).
- **Deferred:** the `json` overlay (§3) — adapter only, when reached.

## Consequences

- The native CLI advances past `_jsonParser`: `jsonParser` + `decodeModule` run as guest code.
- **One JSON implementation, two (eventually) front-ends.** Parser/printer fidelity is fixed and
  tested once; adding `json` later is an adapter, not a re-implementation, and does not disturb
  `argonaut-core`.
- **Almost no ABI growth:** `Json` is an ordinary ADT, the core is guest code; the only host
  addition is the single `fromStringImpl` leaf — consistent with the "prefer `ulib`/guest code
  over growing the ABI" line ([0038](0038-base-package-and-ulib-patches.md)/[0039](0039-ulib-st-array-and-st-uncurried.md)/[0044](0044-foreign-object-over-data-map.md)).
- `argonaut-codecs` and `Decode.purs` are untouched — the representation swap is invisible above
  Core's public API.
- A **new structural element**: a third source home (purvasm-authored shared lib) and a small
  `ulib-tools`/`install.sh` change to include it in the overlay compile set.
- A documented representation divergence carries over: argonaut's object case iterates in sorted
  `Ord String` order (it *is* the [0044](0044-foreign-object-over-data-map.md) `Foreign.Object`),
  not JS enumeration order; decoding looks keys up by name, so the closure does not depend on it.

## Alternatives considered

- **Reimplement only `argonaut-core` now; factor a shared core later when `json` is reached.**
  The "easy now" path, but it forces re-opening, re-reviewing, and re-testing `argonaut-core` when
  the core is extracted — double work for the same result. Building the core *with its first
  consumer* costs little more now and nothing later. Rejected (the decisive objection).
- **Build both `argonaut-core` and `json` overlays now over a shared core.** Avoids duplication
  but ships an unused `json` package speculatively, against demand-driven (which restrains
  speculative *output*, even as it does not excuse an argonaut-only design). The shared core is the
  durable asset; the `json` *adapter* waits for a consumer. Rejected.
- **Duplicate the parser/printer in each package.** ~Two copies of identical grammar/serialiser
  to keep in fidelity-lockstep forever. Rejected.
- **Keep `Json` foreign; add native leaves for `_caseJson`/`_compare`/`from*`/`jsonNull`/
  `stringify`/`_jsonParser`.** A new native runtime value type for `Json` plus a cluster of host
  leaves — ABI/host growth for a library structure, the call rejected for `STArray`/`Object`
  ([0039](0039-ulib-st-array-and-st-uncurried.md)/[0044](0044-foreign-object-over-data-map.md)).
  Nothing higher-order is needed, so guest code is strictly cheaper. Rejected.
- **Hand-roll float parsing** instead of the `fromString` leaf. Re-derives `strtod`/IEEE
  round-trip in guest code and risks drift from the host `float_of_string` boot uses elsewhere.
  Rejected (maintainer-confirmed: native leaf).
- **Home the shared core in `purvasm-base`.** Conflicts with [0038](0038-base-package-and-ulib-patches.md)
  §1 (`Purvasm.*` = ABI primitives, `dependencies: []`); a JSON parser is library code, not a
  primitive. Rejected in favour of a distinct shared-lib package.
- **Relax `ulib` to also host non-patch, standalone packages** (put the core under `ulib/`).
  Mechanically easy (`install.sh` already extracts every `ulib/*/**.purs`), but it amends
  [0038](0038-base-package-and-ulib-patches.md) §2's invariant that `ulib` is "patches on registry
  packages, **never a from-scratch API**", diluting its single responsibility, and the future
  externs-based compatibility gate ([0038](0038-base-package-and-ulib-patches.md) §3) and testing
  strategy ([0040](0040-ulib-testing-strategy.md)) are patch-centric (they assume an upstream to
  check/oracle against), so an upstream-less module becomes a special case. Rejected in favour of a
  clean separate package (§4).
- **Migrate the compiler off argonaut onto `json`** (drop argonaut entirely). A larger change —
  rewrite `Decode.purs`/CLI call sites and forgo the `argonaut-codecs` `.:`/`decodeJson`
  ergonomics — and unnecessary: with a shared core, both libraries ride one implementation, so
  there is no representation cost to keeping argonaut. Out of scope here.
