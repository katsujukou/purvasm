# 0053. Association-array JSON objects on the CoreFn decode path — remove the `Foreign.Object`/`Data.Map` build that dominates `loadClosure`

- Status: Accepted
- Date: 2026-06-29

## Context

After ADR-0049/0051/0052 a cold native self-host build dropped to ~23 s, and profiling the residual
showed a single dominant phase: the pause between `Building from entry` and the first
`compiled …` is **`loadClosure` = ~21 s ≈ 91 % of the build**. `loadClosure` reads and decodes the
*entire* 227-module `corefn.json` closure up front (phase markers: `loaded closure` at 21.0 s, then
`depOrder` / `map compileModule` / first `compiled` all instant — confirming `map compileModule` is
lazy and the compile work streams in the `for_`).

Splitting the per-module decode (min-of-3 native, startup ~0.29 s):

| module | `jsonParser` (parse) | + `decodeModule` | decodeModule |
|--------|----------------------|------------------|--------------|
| Bytecode.Instruction 444 KB | 0.43 s | 0.46 s | **0.03 s** |
| PureScript.CoreFn.Expr 427 KB | 0.42 s | 0.45 s | **0.03 s** |
| Data.Map.Internal 601 KB | 0.60 s | 0.66 s | **0.06 s** |

So **the parser dominates and `decodeModule` (Json → CoreFn) is tiny.** Root cause: the parser
(`Json.Core.Parser` via the argonaut `Builder`) materialises **every JSON object as a
`Foreign.Object` = `Data.Map String Json`** (ADR-0044) through `fromFoldable`. `corefn.json` is
object-dense — every AST node is a small `{ "tag": …, … }` — so a module builds *thousands* of
balanced trees, and the per-object tree allocation + key comparisons are expensive on the native
runtime. (String building inside the parse is already linear after ADR-0052.)

Two things this is *not*: it is not `decodeModule` (cheap), and it is not solved by **streaming**
(ADR-0050) — streaming bounds peak memory but the total decode *work* is unchanged, so the 21 s
stays. The lever is to stop paying for `Data.Map` on the decode path.

## Decision

Decode `corefn.json` through a **cheap association-array object representation**, bypassing
`Foreign.Object`/`Data.Map` on the hot path, while leaving ADR-0044 (`Foreign.Object a =
Data.Map String a`) and argonaut untouched for every other user.

Concretely, the compiler's CoreFn ingestion uses **`Json.Core` directly** (the backend-agnostic
parse core, ADR-0046), with:

- a **`Builder` whose object case keeps the `Array (String /\ Json)` the parser has already
  accumulated** — object construction becomes ~O(1) (no tree build, no rebalancing, no key
  comparisons at build time), instead of `Foreign.Object.fromFoldable`; and
- a **`PureScript.CoreFn.Decode` that reads fields by key off that array** (a linear scan) — instead
  of `Data.Argonaut.Decode`'s `Foreign.Object.lookup` (`.:`). CoreFn objects are uniformly small `k`
  (an AST node is `{ tag, … }`; the unbounded shapes — `decls`, `imports`, an `Array` literal — are
  JSON *arrays*, not objects), and each field is read a fixed number of times, so the scan is O(k)
  with no O(k²) regression and stays within `decodeModule`'s already-negligible budget.

The expectation (from the parse/decode split above): object-build cost moves from "thousands of
`Data.Map` inserts" to "keep an array", collapsing the dominant term; `decodeModule` rises only
slightly (O(k) vs O(log k) lookups, k small). Net: `loadClosure` should fall from ~21 s toward the
parser's irreducible traversal + string cost.

**Staged validation — measure the parse-delta with the 1-line `Builder` change *before* the
bespoke decoder rework (the order matters).** The win ceiling is exactly the share of parse time
that `Obj.fromFoldable` (the `Data.Map` build) occupies; the *floor* that assoc-array cannot remove —
byte-scan, the `kvs` `List`/`reverse`, `JString` node allocation — may be 40–60 % of parse, not the
optimistic ~90 %. The cheap, low-risk way to settle this is to change **only** `jobject`
(≈ one line: keep the accumulated `kvs` as `JObj kvs` instead of `fromObject (Obj.fromFoldable kvs)`)
and re-time `jsonParser` / `loadClosure` — **no decoder change is needed to read this number**. The
result decides the path before the expensive, correctness-sensitive part:

  - if removing `fromFoldable` collapses parse → proceed with the assoc-array decoder (this record);
  - if a large floor remains (generic-`Json`-tree allocation is the residual) → the assoc-array
    `Builder` is insufficient and **parse-straight-to-CoreFn** (Alternatives) is the right target;
    revisit before touching the decoder.

The bespoke decoder is the high-risk part (it stops reusing `Data.Argonaut.Decode` combinators and
must stay schema-faithful), so it is gated behind this measurement. Output (`.pmo`/`.pmi`/`app.pvm`)
must stay byte-identical throughout.

## Scope

- **In:** the compiler's corefn parse+decode path — a `Json.Core` `Builder` with assoc-array
  objects and a `PureScript.CoreFn.Decode` that reads from it (replacing the argonaut-`Json`/
  `Foreign.Object` route in `Purvasm.CLI.Compile.parseModule` / `decodeModule`).
- **Out:**
  - `Foreign.Object`/`Data.Map` itself (ADR-0044) and argonaut-core (ADR-0046) for all non-decode
    users — unchanged.
  - The serializer (ADR-0051/0052) — already linear.
  - Streaming / incremental reuse (ADR-0050) — orthogonal (peak memory / warm rebuilds), does not
    reduce cold decode work.

## Validation

The acceptance gate, in the ADR-0049/0051/0052 shape (byte-identity + perf yardstick + headline),
with the staged parse-delta measurement (above) as the **first** gate:

- **(0) Path decision (gate before the decoder rework).** With only `jobject` changed to keep the
  assoc array, re-time `jsonParser` / `loadClosure`: report the parse-time drop and decide
  assoc-array vs. parse-straight-to-CoreFn. Only proceed to the decoder once this confirms
  assoc-array removes the bulk of parse time.
- **(a) Byte-identity (correctness).** Compiler unit (incl. a new `decodeModule` unit test over the
  assoc-array reader, per CLAUDE.md — the decoder no longer borrows argonaut's tested combinators)
  + E2E (`.pmo`/`.pmi`/`app.pvm` == boot) stay green; no `format_version` bump (output unchanged).
- **(b) Decode scaling (the fix).** `loadClosure` for the 227-module closure collapses from ~21 s
  (record the after-value); per-module parse drops on the heavy modules (444–601 KB).
- **(c) Headline — cold build.** The full cold build still completes and emits `app.pvm`, now well
  under the current ~23 s (record total and the `loadClosure` share after).

## Consequences

- Removes the last large term in the cold build: `loadClosure` (~91 % today) should drop sharply,
  taking the 227-module cold build well under the current ~23 s.
- Decouples the compiler's hot ingestion path from `Foreign.Object`/`Data.Map`, so the corefn
  reader's cost no longer tracks `Data.Map`'s native constant.
- A second consumer (besides argonaut) of `Json.Core`'s `Builder` abstraction — validating the
  ADR-0046 design that the parse core is representation-agnostic.
- Cost: a bespoke CoreFn decoder no longer reusing `Data.Argonaut.Decode` combinators; it must stay
  faithful to the CoreFn schema (the existing E2E byte-identity tests guard this).

## Alternatives considered

- **Change `Foreign.Object`'s backing globally (e.g. assoc array / hashing).** Far broader blast
  radius (every `Foreign.Object` user) for one path's benefit, and re-opens ADR-0044; rejected.
- **Parse straight into CoreFn (no intermediate JSON tree at all).** The largest possible win
  (skips both the object build *and* `decodeModule`), but a much bigger, schema-coupled parser; the
  assoc-array `Builder` keeps the clean parse/decode split and is the smaller step. Revisit if the
  prototype shows the generic-tree traversal/allocation (not the `Data.Map` build) is the residual
  cost.
- **Streaming the closure (ADR-0050).** Does not reduce total decode work; rejected as the remedy
  for this (it is a peak-memory / warm-rebuild concern).
- **Leave it.** 21 s of every cold build is the single largest remaining cost; not acceptable given
  the maintainer's cold-build priority.
