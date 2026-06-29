# 0054. Byte-oriented `Json.Core.Parser`: scan UTF-8 bytes directly, eliminating the code-point-array parse floor

- Status: Accepted
- Date: 2026-06-29

> **Progress (2026-06-29).** Implemented. `Json.Core.Parser` rewritten to scan UTF-8 bytes
> (`Purvasm.String.byteAt`/`byteLength`); a new `Json.Core.Utf8` helper (defined over the four
> `Purvasm.String` primitives, **not** ulib) provides `sliceBytes`, the canonical-UTF-8 `encodeCp`,
> and `bytesToString`; strings take the fast (`sliceBytes`) / slow (byte-wise escape decode, surrogate
> pairs) paths; error positions are byte offsets. `purvasm-json` gains a `purvasm-base` dep
> (`purvasm-base` stays `deps = []`, no cycle). `dune fmt`/`purs-tidy` clean.
>
> **Validation — all gates met.**
> - **(a) Correctness / byte-identity:** purvasm-json 25/25 (the non-ASCII `\u`/surrogate string tests
>   are now asserted as canonical UTF-8 **bytes** — representation-faithful on JS and native per
>   ADR-0040 — plus an escaped-key case and an `encodeCp` pin over 1–4-byte and boundary code points);
>   compiler unit 59/59 + E2E 3/3; and the decisive **native differential — 21/21 modules'
>   `.pmo` byte-identical to boot's `.pvmo`** (incl. string-heavy `Data.String.CodePoints`,
>   `Json.Core.Parser`, argonaut). No `format_version` bump.
> - **(b) Parse / loadClosure scaling:** the `Building from entry` → first `compiled` gap
>   (`loadClosure`) fell **~21 s → ~6 s (≈3.5×)** — in line with the ADR's "real parser ~4–5×, ~6×
>   ceiling".
> - **(c) Cold build:** the full 227-module cold build completes and emits a byte-identical `app.pvm`
>   (704,394 B) in **~11 s, down from ~23 s.**
>
> ADR-0053 stands Rejected (sidenote 0007); status/index edits are the maintainer's.

## Context

After ADR-0049/0051/0052 the Level-3 cold build is ~23 s, of which the `Building from entry …` →
first `compiled …` pause is **`loadClosure` ≈ 21 s (~91 %)** — decoding the whole `corefn.json`
closure up front, dominated by the *parser* (`decodeModule` is negligible).

The profiling case study [sidenote 0007](sidenotes/0007-loadclosure-parse-profiling.md) settled the
cause. ADR-0053's hypothesis — that building each JSON object as a `Foreign.Object` = `Data.Map`
(ADR-0044) is the cost — was **refuted**: three `Builder`s (argonaut/`Data.Map`, association-array,
and a no-op that allocates nothing) parse in the *same* time, so neither the object representation nor
the generic-`Json` tree is on the critical path. The cost is the parser's own floor: `Json.Core.Parser`
converts the whole input to a code-point `Int` array (`cps = map fromEnum (toCodePointArray s)`, ~30 %
of parse) and then rebuilds every token's string from code points (`slice cps` + `fromCodePointArray`,
~70 %). A prototype byte-indexed parser (scan UTF-8 bytes; extract via `sliceBytes`; no `cps` array)
measured **~6× on the floor** (0.25 s → 0.04 s per heavy module, head-to-head min-of-3), projecting
`loadClosure` ~21 s → ~3.5 s and the cold build ~23 s → ~5–6 s.

This record supersedes ADR-0053's *approach*. Since ADR-0053 was refuted by measurement before any
implementation, it should be marked **Rejected** (as ADR-0029 was), not "Superseded" — with pointers
to sidenote 0007 and this record. (Status/index edits are the maintainer's.)

## Decision

Rewrite `Json.Core.Parser` to scan the input **as UTF-8 bytes** instead of via a code-point `Int`
array. Only the *scanning layer* changes; the recursive-descent structure and the `Builder j` API are
unchanged, so every consumer — the `argonaut-core` `jsonParser` (the compiler's hot path) and
`Json.Core` itself — gets faster with no change of its own.

- **Index by byte offset.** `peek i = byteAt s i` for `i < byteLength s`, else EOF. The grammar's
  structural and lexical characters (`{ } [ ] : , "` `\`, digits, `t/f/n`, whitespace, control) are
  all ASCII = single bytes, and UTF-8 is self-synchronising (continuation bytes are ≥ 0x80, never
  equal to an ASCII byte), so byte-level scanning for them is exact. No `cps` array is built.
- **String extraction — two paths (this is where the speed and the risk both live).**
  - *Fast path:* scan bytes to the closing `"` (stopping at `\` or a control byte). With no escape,
    emit `sliceBytes s start cur` — a direct byte copy, O(token), capturing the raw UTF-8 verbatim
    (correct because the source is already UTF-8). This is the common case (identifiers, tags) and
    the source of the ~6×.
  - *Slow path (correctness-critical):* on the first `\`, switch to byte-accumulation and **decode
    escapes exactly**: `\" \\ \/ \b \f \n \r \t` → their byte(s); `\uXXXX` → the code point, UTF-8
    **encoded** into the output (`putCp`), combining a high+low surrogate pair into one supplementary
    code point (e.g. `😀` → 4 bytes), and **rejecting a lone low surrogate** — i.e. the
    exact semantics the current parser already implements, preserved byte-for-byte.
- **Numbers / literals.** Scan the numeric byte range, `sliceBytes`, hand to `Data.Number.fromString`;
  match `true`/`false`/`null` by bytes.
- **Byte primitives & dependency.** Byte access is the four `Purvasm.String` primitives
  (`byteAt`/`byteLength`/`unsafeNew`/`unsafeSetByte`, purvasm-base). The two helpers the parser needs —
  `sliceBytes` (a byte-range copy) and `putCp` (UTF-8-encode a code point into a buffer) — are
  **defined inside `purvasm-json` (a small local `Utf8` helper) over those primitives, NOT imported
  from the ulib `Data.String.Internal.Utf8`.** The ulib is an *overlay* applied only at native link
  time (`ulib-tools build`), not a `spago` package, so stock `purs` — which builds and tests
  `purvasm-json` (the portability this ADR's rationale (ii) relies on) — cannot resolve a ulib module
  at all; and depending on another subsystem's `Internal` module violates CLAUDE.md's layering rule.
  The helper is **not** pushed down into `purvasm-base` either: `purvasm-base` is the
  zero-dependency (`deps = []`) primitive layer and must stay so, so the right home is a
  `purvasm-json`-local definition. The result is a small duplication of the same `unsafeNew` +
  threaded `unsafeSetByte` logic the ulib `Data.String.Common.blit` / `Utf8.putCp` use. This adds a
  **purvasm-base dependency to purvasm-json** (only purvasm-base — purvasm-base itself stays
  `deps = []`, no cycle), coupling the parser to the purvasm byte ABI — a deliberate divergence from
  `Json.Core`'s backend-agnostic intent ([[purvasm-json-standalone-intent]]).
- **`putCp` must byte-match the ulib encoder.** Canonical UTF-8 has exactly one encoding per code
  point, so a correct `putCp` is byte-identical to ulib's by construction — but because a future path
  could route strings through the ulib encoder, the equivalence is *pinned* by test (see Validation),
  with later consolidation into a shared home left open.
  It is acceptable because (i) `Json.Core.Parser` runs on the native backend in production (on JS the
  compiler uses the registry `JSON.parse`), and (ii) `Purvasm.String` carries a JS FFI shim, so
  purvasm-json still builds and tests on stock `purs`. The standalone-publish plan, if pursued, must
  then bundle those four primitives as a small portability shim (a noted consequence, not a blocker).

**Invariant:** the parse *semantics* — the accept/reject set and every decoded value — are unchanged,
so `.pmo`/`.pmi`/`app.pvm` stay byte-identical and `Image.format_version` is not bumped.

## Validation

In the ADR-0049/0051/0052 shape — measured on the **real** parser (escape slow path included), not
the floor prototype:

- **(a) Correctness / byte-identity.** The existing `Json.Core` parser tests pass — in particular the
  escape and surrogate cases (`\uXXXX`, surrogate-pair combination, lone-low-surrogate rejection) —
  **plus new escape-heavy cases** (every simple escape; a supplementary-plane pair; an escaped key);
  and the compiler unit + E2E (`.pmo`/`.pmi`/`app.pvm` == boot) stay green. No `format_version` bump.
  A dedicated test **pins the local `putCp` to the canonical UTF-8 bytes** for representative code
  points (1-/2-/3-/4-byte, incl. a supplementary-plane one), guaranteeing byte-parity with the ulib
  encoder.
  **Caveat (per Consequences): error positions move from code-point index to byte offset**, so any
  test asserting a position *number* inside an error message is first relaxed to assert the message
  kind / `isLeft` / the parse result instead — the accept/reject set and decoded values are what must
  be unchanged, not the literal index in a message.
- **(b) Parse / loadClosure scaling.** Re-time the heavy modules and the full `loadClosure`
  (~21 s → record after). The headline ~6× is the escape-free *ceiling*; with the escape slow path the
  real parser may land ~4–5×, which is the number to report.
- **(c) Cold build.** The full cold build completes and emits `app.pvm`, ~23 s → record after.

## Consequences

- Removes the dominant remaining cold-build term: `loadClosure` should drop ~4–6×, taking the cold
  build from ~23 s to roughly single digits.
- One parser, faster for **all** its consumers (no second implementation to keep in sync), with the
  `Builder` output abstraction intact — only the input scanning is now byte-level.
- `purvasm-json` gains a `purvasm-base` dependency and is no longer backend-agnostic at the input
  layer; a future standalone publish needs the byte-primitive shim noted above.
- Error positions are now **byte offsets**, not code-point indices (the messages' numbers change):
  tests must not assert exact position numbers in messages (assert the message kind, or the parse
  result), and any human-facing "at index N" is understood as a byte offset.

## Alternatives considered

- **ADR-0053 (assoc-array objects; parse-straight-to-CoreFn).** Refuted by measurement
  (sidenote 0007): object representation and tree allocation are both off the critical path. Marked
  Rejected.
- **Parameterise the parser over a "source" abstraction** (portable code-point source + purvasm byte
  source), keeping `Json.Core` fully agnostic. Rejected for now: a per-character indirection through a
  dictionary risks eating the very constant factor we are removing, and the agnostic source is the
  slow `cps` path anyway. The `Builder` already abstracts *output*; abstracting *input* can be
  revisited if/when standalone publish is actually pursued.
- **A separate purvasm-specific byte parser** (e.g. in the `argonaut-core` ulib), leaving
  `Json.Core.Parser` agnostic. Rejected: two parsers with identical-by-contract semantics is a
  byte-identity hazard; the byte primitives' JS shim lets the single parser stay portable enough.
- **A native byte-scan / `memcpy` leaf for `sliceBytes`.** A further constant-factor win on top, but
  the PS byte parser already gets ~6×; defer as an optional follow-up (cf. ADR-0052's deferred
  `Bytes.blit`).
- **Streaming the closure (ADR-0050).** Orthogonal — bounds peak memory, does not reduce decode work.
