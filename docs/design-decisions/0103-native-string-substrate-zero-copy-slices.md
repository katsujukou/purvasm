# 0103. Native String substrate: bounded-retention slice views, linear cursor traversal, and bulk borrowed-byte string primitives

- Status: Accepted
- Date: 2026-07-16

## Context

[Sidenote 0017](sidenotes/0017-fsr-native-string-quadratic-measurement.md) measured a
complexity-class defect in the native string substrate. The CST lexer advances by `SCU.drop` on the
remaining input per token; on the native backend that is a whole-suffix copy built **one byte at a
time through foreign-leaf calls** (`Data.String.Internal.Utf8.sliceBytes` over
`Purvasm.String.byteAt`/`unsafeSetByte`), `SCU.length`/`charAt` are front-to-back walks, and the
runtime string primops (`pv_prim_eq_string`/`lt`/`append`) **copy guest bytes out into a Rust
`String`** before operating (and `append` copies back in). Net: lexing one source file is **O(n²)
with per-byte dynamic applies** — measured 19 s for a 5 KB file, 254 s for 20 KB, timeout beyond
50 KB on the L2 native compiler, versus flat sub-second on JS; a whole-closure `foreign-sigs` sweep
is 90–109× JS, dominated by FSR parsing a mere 43 KB of source.

This is distinct from the [sidenote 0015](sidenotes/0015-native-runtime-perf-investigation.md) /
[ADR-0102](0102-runtime-v1-1-performance-baseline-for-dynamic-apply.md) class (constant-factor
dynamic-apply cost). It is *worse asymptotics*, and it sits under every string-heavy native path:
FSR ([ADR-0080](0080-foreign-signature-reconstruction-cst.md)), CST lex/parse at self-host scale,
byte-oriented JSON scanning ([ADR-0054](0054-byte-oriented-json-parser.md)), and
`purvasm-regex` ([ADR-0081](0081-purvasm-regex-pure-ps-es-engine.md)). A cache in front of FSR would
mask one consumer and leave the class in place (rejected below); the substrate itself is fixed.

Inherited constraints:

- **`String` is packed UTF-8 bytes + length; `Char` is a code point**
  ([0006](0006-string-utf8-char-int.md), [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2).
  Observable semantics do not change — this record is representation/runtime only, invisible to the
  differential oracle.
- **The heap kind set is ABI** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2):
  adding a kind is a runtime/codegen contract extension and is recorded here.
- **boot is frozen** ([memory: boot-frozen-post-0079]): no boot *codegen* changes. New runtime
  entries must ride the existing foreign-leaf mechanisms (`pvf_*` link-time symbols /
  `foreign import` declarations in `purvasm-base`, [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md)),
  which both boot and Level-2 already lower without code changes. boot's *host FFI registry* needs
  matching leaves for VM/oracle parity — a leaf addition, i.e. a Level-2-blocking fix permitted
  under the freeze, flagged for the maintainer's confirmation.
- **The unsafe island discipline** ([0063](0063-runtime-implementation-language-rust.md)): no
  Rust reference into the moving heap survives an allocation; borrowed-byte operations must not
  allocate while borrowing (or must re-derive after rooting).
- **The in-place byte builder contract** ([0052](0052-native-unsafesetbyte-in-place.md)) is
  untouched: `unsafeSetByte`/`unsafeNew` remain the linear build primitives; this record adds bulk
  *read/derive* operations, it does not re-introduce per-byte writes elsewhere.

## Decision

Fix the substrate in five moves, runtime-first, with the ulib string shadows rewritten over the new
surface. No caching layer, no upstream parser patch.

### 1. Backing/view separation: a `StrSlice` kind under a bounded-retention build rule

Add a heap kind **`StrSlice` — discriminant `10`** (ABI, fixed here exactly as `RawIds = 9` was,
[0069](0069-v1-dynamic-record-operations.md) §1), layout

```text
[ base: value slot (→ a packed Str) ][ byte_off: raw ][ byte_len: raw ][ cp_len: raw ]
```

— a **borrowed view** into an existing `Str`'s bytes (`cp_len` per §2). The GC scans `base` as an
ordinary value slot (the view keeps the backing alive; no interior pointer is stored, so moving GC
is unaffected — readers re-derive `base + byte_off` per access and never hold the derived pointer
across a safepoint). A slice never nests: slicing a slice re-bases onto the underlying `Str`. Every
string **reader** in the runtime accepts `Str | StrSlice` through one internal normalisation
(`{ base_ptr, off, len }`); every string **constructor** keeps producing packed `Str`.

**The bounded-retention rule** (review round 2 — resolving the retention question the first draft
left open): **every slice builder applies one uniform rule.** With `B` = the normalised backing
`Str`'s byte length and `L` = the result's byte length:

- `L = 0` → the empty packed `Str`;
- the full range of the backing → the backing itself (no wrapper);
- **`B ≤ 4·L` → a `StrSlice`; otherwise → materialise a packed `Str`.**

Why this closes both failure modes at once: a live view retains at most **4×** its logical size —
a few-byte extraction from a 5 MB source auto-copies, so no policy is needed at any particular
consumer (regex captures, `split`, user `drop`s are all covered by construction) — while a lexer's
successive `drop`s stay views until the remainder falls below a quarter of the backing. The
complexity argument, in consistent units (review round 3 — `B`/`L` are *bytes*, a `drop`'s `k`
counts *code points*): when a `drop` trips the rule against an exactly-compacted backing, the
**remaining bytes are `< consumed bytes / 3`**, so each re-materialisation copies less than the
bytes consumed since the last one, and the geometric series keeps **the whole cursor sequence
O(n) bytes**; a single step is then O(consumed *code points*) because UTF-8 spends at most 4 bytes
per code point. The headline claim is therefore *view-or-materialise, amortised O(1) per consumed
code point, linear whole-traversal* — never "always a view". The explicit `materialize` leaf (§5)
remains for boundaries that must sever ownership below the threshold.

**Constructing a view is itself an allocation, with the standard rooting obligation** (review
round 3): the builder is a value-storing constructor in the [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)
§3 sense — it **roots the backing value before `alloc`, re-reads it after, and only then stores it
into the fresh `base` slot**. A `HeapPtr` normalised out of the input *before* the allocation must
never survive it (the collection the `alloc` may trigger moves the backing; storing the stale
pointer would be the exact use-after-move the self-rooting constructors exist to prevent).

**Validation tier:** the slice builders are safe public API and **release-validate** their inputs —
`byte_off`/`byte_len` within the base, no wraparound on the additions, both endpoints on UTF-8
boundaries (extending `new_str`'s valid-UTF-8 ownership, [0067](0067-v1-effect-execution-and-native-leaves.md)
§5, to every view). If the runtime needs an unvalidated internal variant it is named
`unsafe_slice_bytes` and is never exported. **`Purvasm.String.unsafeSetByte`
([0052](0052-native-unsafesetbyte-in-place.md)) rejects a `StrSlice`** — in-place byte writes stay
confined to freshly built packed `Str`s.

### 2. Logical length — storage, initialisation, and the sharing precondition

> **Review pins (2026-07-16, rounds 1–2):** the first draft left the `cp_len` home open; the
> second review corrected the sharing-safety argument and the derivation rule.

`byteLength` stays O(1) for both forms (`byte_len` on a slice). For the **code-point length**:

- **The base `Str` layout is unchanged** — no `cp_len` word is added to `Str`; a bare `Str`'s
  count remains a walk (O(bytes)), as today.
- **`StrSlice.cp_len` is derived only when the builder genuinely knows the result's count,
  memoised on demand otherwise.** The rule is per operation (review round 3 — "the consumed
  count" alone is *not* the result length of a `drop`, and the raw argument `k` is never
  recorded; clamping is always applied first):
  - `takeCodePoints k` → the **retained** count `c` (what was actually walked): `cp_len = c`;
  - `dropCodePoints k` → `cp_len = N − consumed` **only when the parent's count `N` is known**;
    `cp_len = 0` when the traversal reached the end of the input; otherwise (a bare-`Str` parent
    of unknown count, end not reached) the `unknown` sentinel;
  - `byteSlice` → known only if the builder actually counted the result's code points;
    otherwise `unknown`.

  An `unknown` slice's first `length` demand counts **the slice's own bytes** (never the whole
  base) and memoises into the raw word.
- The precise complexity claim (correcting the first draft's "O(1) length"): **O(1) when known or
  after the first memoised demand; the first demand on an `unknown` slice is O(byte_len of the
  slice); a bare `Str` count is a walk.**
- **The memo write is a v1-only exception to object immutability**, sound solely because v1 is
  single-capability ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §0).
  *Idempotence does not make racing writers safe in Rust — a non-atomic concurrent store is a
  data race even when both sides store the same value.* **Mandatory precondition before v2
  sharing / S1 promotion of any slice** (one of, decided then): the memo word becomes an
  `AtomicU64` with relaxed load/store; or the count is finalised eagerly before an object is
  promoted/shared; or memoisation is disabled on shared objects.

Net contract (what the lexer relies on): a cursor step is O(consumed) (§1, amortised); `length`
costs at most one count per slice and only if demanded; **the token loop performs no per-token
O(remaining) work**. A lexer whose suffixes never demand `length` (the common CST path —
`SCU.length` is called on small *match* strings, not suffixes) never pays any count at all.

### 3. O(1) head decode; cursor ops proportional to what they consume

`charAt 0` / `codePointAt 0` / `uncons` decode at `base + byte_off` directly; `drop k`/`take k`
advance by decoding exactly the clamped `k` code points — O(consumed), independent of the remaining
length, with §1's occasional compaction folded into the same bound (a compacting step copies less
than what was consumed since the last one). These are the lexer's cursor moves — after this record
they cost what the token costs, so a whole-file lex is **O(file)**.

### 4. Runtime string primops operate on borrowed guest bytes

`pv_prim_eq_string` / `pv_prim_lt_string` / hashing (`fnv1a_64` for record ids,
[0069](0069-v1-dynamic-record-operations.md)) / search compare **in place** over the guest bytes
(`&[u8]` views derived inside the island) — no copy-out into a Rust `String`. Allocating operations
(`pv_prim_append`, `materialize`) follow the island discipline: snapshot lengths, **root the
operands, allocate, re-derive, then copy** — never a borrow held across the allocation. Miri
([0063](0063-runtime-implementation-language-rust.md) §4) gates these paths.

### 5. The bulk surface: `pvf_*` foreign leaves, kept apart from the raw C accessor ABI

**Two namespaces, never conflated** (review round 2): the **raw C accessor ABI** —
`pv_str_len` / `pv_str_copy`, the existing FFI-provider read surface
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2, `runtime/src/abi.rs`) —
keeps its names and signatures unchanged and simply **accepts both kinds as input** (normalising
internally). The operations this record adds are **foreign leaves** under the `pvf_` link ABI
([0073]; `mangleForeign` = `pvf_` + the `_HH`-escaped qualified key) and are not referred to as
`pv_str_*` anywhere, in code or prose.

**ABI route, pinned** (review round 1: raw codegen C-ABI entries vs foreign leaves): the bulk
operations are **`pvf_*` foreign leaves** — `foreign import`s declared in `purvasm-base`, resolved
at link time and implemented in the runtime staticlib — **not** new `pv_prim_*` primops and
**not** a widened generated-code C ABI. Rationale:

- A primop (or any new codegen-emitted entry) requires a codegen change in **both** compilers —
  boot's codegen is frozen, and until [ADR-0104](0104-retire-boot-byte-identity-gate.md) lands the
  `--no-opt` byte-identity gate still stands. The `pvf_` route changes **no codegen logic**: both
  compilers already lower an `AForeign` reference the same way, so they keep emitting identical
  bytes for the identical (updated) `purvasm-base`/ulib sources — this record can land **before**
  0104 without touching the gate.
- The per-call cost of a leaf (a no-capture closure through `pv_apply`) is amortised by the bulk
  semantics — one call per *operation*, not per byte; that trade is exactly this record's point.
- Promoting the hottest leaves to primops / inline IR is a later, measurement-gated perf lever
  that becomes available once 0104 retires the gate; the leaf surface is designed so that
  promotion changes call sites' lowering only, not the operations' semantics.

**The pinned initial surface** — `foreign import`s in `purvasm-base`'s `Purvasm.String`
(qualified key `Purvasm.String.<ident>` → link symbol `pvf_Purvasm_2eString_2e<ident>`), one leaf
call per *operation*:

| identifier | PureScript type | notes |
|---|---|---|
| `byteSlice` | `Int -> Int -> String -> String` | `[from, to)` byte range; release-validated (§1); view-or-materialise per §1's rule |
| `dropCodePoints` | `Int -> String -> String` | clamped; result `cp_len` follows §2's known-parent/end/`unknown` rule |
| `takeCodePoints` | `Int -> String -> String` | clamped; a view records the retained count as its result `cp_len` (§2) |
| `codePointLength` | `String -> Int` | the §2 contract |
| `codePointAt` | `Int -> String -> Int` | the code point, `-1` when out of range |
| `byteIndexOf` | `String -> String -> Int -> Int` | hay → needle → from-byte → matched byte offset or `-1` |
| `byteLastIndexOf` | `String -> String -> Int -> Int` | as above, searching backwards |
| `compareBytes` | `String -> String -> Int` | byte-lexicographic; `-1`/`0`/`1` |
| `appendBulk` | `String -> String -> String` | borrowed-bytes concat (§4 root→alloc→re-derive) |
| `materialize` | `String -> String` | sever ownership below the §1 threshold; identity on a packed `Str` |

Extensions grow under the same naming, typing, and validation rules. **The 64-bit FNV-1a hash is
*not* exposed to PureScript** (it has no settled surface representation yet); it stays
runtime-internal to the record machinery ([0069](0069-v1-dynamic-record-operations.md) §2),
reading both kinds' bytes in place.

**Consumer rewrite scope** (review rounds 2–3 — this record removes the **hot
scan/slice/search/compare/append loops**, stated precisely rather than as "every per-byte loop"):
the ulib string shadows (`Data.String.Internal.Utf8`'s
`sliceBytes`/`byteIndexOf`/`byteOffsetOfCp`/…, `Data.String.CodeUnits`, `Data.String.CodePoints`,
`Data.String` proper), **`ulib/prelude`'s string instances** (`Data.Eq.eqStringImpl`,
`Data.Ord`'s string compare, `Data.Semigroup`'s `blitS` append), and the **package-local copies of
the byte loops** — `packages/purvasm-json`'s `Json.Core.Utf8` and `packages/purvasm-regex`'s
`Regex.Core.Utf8`. The JSON decoder and the regex engine are this record's headline
beneficiaries; leaving their private `sliceBytes` on the leaf-per-byte path would forfeit exactly
the claimed wins. All of these move onto the safe bulk surface (`AForeign` → `pvf_` — no
guest-term substitute, no cache in between).

**Named, deliberately remaining linear builders** (not in this record's ten-leaf surface): the
array conversions (`fromCharArray`/`toCharArray`), JSON's byte-materialisation
(`bytesToString`-class builders), and `Data.String.Common`'s `blit`-style table rebuilds — all
already O(n) single-pass over [0052](0052-native-unsafesetbyte-in-place.md)'s builder primitives,
so they carry constant-factor cost, not the complexity class this record closes. Bulk
conversion/copy leaves for them are the natural *extensions* under the same table rules, added
when a profile asks.

**The JS provider grows with the surface** (review round 3): the ten `foreign import`s live in
`packages/purvasm-base`'s `Purvasm.String`, whose **existing `String.js` must gain the matching
exports** — `purvasm-json` and `purvasm-regex` are *shared package sources*, so once they call the
bulk surface, stock `purs`/JS builds (their upstream test suites, `ulib-tools` verify, PS-CI)
resolve the same foreigns through the JS provider. What stays untouched is the **registry
`Data.String` and the JS codegen**, not the provider. Semantics are **provider-relative, as
already established for `Purvasm.String`**: on JS the "byte" offsets/lengths are UTF-16 code
units (the existing `byteLength`/`byteAt` contract, [0040](0040-ulib-testing-strategy.md)'s
representation seam), while `dropCodePoints`/`takeCodePoints`/`codePointLength`/`codePointAt`
count Unicode code points on **both** providers; the JS implementations are plain
`substring`/iterator code with no view machinery (V8 already owns that).

### Non-goals (explicit)

- **No FSR result cache / sidecar** — it would mask one consumer of a substrate defect and leave
  cold builds broken (and cold, sidecar-free execution is exactly what self-hosting exercises).
- **No patch to `language-cst-parser`** — upstream's `String`-cursor idiom is fine once the
  substrate honours string-slicing costs; shadowing a whole parser package to work around ulib
  strings would invert the dependency.
- **No interning/ropes/generic SSO** — out of scope until measurement asks for them.

### Implementation order (review round 2)

1. **Runtime + surface**: the `StrSlice` kind, reader normalisation, borrowed-byte primops,
   validation tiers, and the `purvasm-base` surface — **both providers**: the runtime `pvf_*`
   leaves and the matching `String.js` JS implementations (§5).
2. **boot host-registry parity leaves** — *before* any consumer moves, so the CESK/VM legs of the
   differential can run the moment a ulib shadow calls the new surface.
3. **Consumer rewrites**: `ulib/prelude` (Eq/Ord/Semigroup string instances), `ulib/strings`,
   `purvasm-json`, `purvasm-regex`.
4. **Gates and probes**: the full differential, Miri, and the §Verification performance probes.

## Verification (part of the decision)

Cold, sidecar-free runs, native vs JS, before/after — the
[sidenote 0017](sidenotes/0017-fsr-native-string-quadratic-measurement.md) method re-run on the fixed
substrate, plus the standing gates:

**The primary success gates are the scaling exponent (≈ 1.0) and the extinction of per-byte
`pv_apply` traffic** (`PURVASM_STATS` apply counters bound the per-operation call count) — wall
time is the secondary indicator, with "within an order of magnitude of JS" as the initial target
(the residual is dynamic-apply constant factor, ADR-0102's territory, not asymptotics).

- **raw scan/slice/search micro** — `drop`-loop over N KB, `indexOf` sweep: exponent + counters.
- **`ForeignSig.reconstructModule` probe** (the synthetic single-module closure): 50 KB completes;
  scaling ≈ linear.
- **representative CST lex/parse** and **CoreFn JSON parse/decode** legs, sizes ≥ 2 points each,
  exponent asserted.
- **whole-closure FSR sweep** (`foreign-sigs --entry Purvasm.CLI.Native`): minutes → seconds.
- **Correctness fixtures** (review round 2 additions included): every string reader over the
  **`Str` × `StrSlice` matrix**; `appendBulk`/`pv_prim_append` dispatch with a slice on **either
  operand**; builder **clamp and UTF-8-boundary validation faults** (off/len out of base, overlong
  `k`, mid-code-point offsets); **nested-slice re-base**; **`unsafeSetByte` rejecting a slice**;
  **known vs `unknown` `cp_len`** paths including the memo; the **bounded-retention rule** itself
  (threshold trip, full-range pass-through, empty result, and the cursor-sequence O(n) shape on a
  long `drop` chain).
- **Standing gates:** the full differential (string fixtures + **two distinct forced-GC
  fixtures**: a collection moving the base under an *already-live* `StrSlice`, and — review
  round 3 — a collection fired **by the slice construction itself**, moving the input base
  between normalisation and the `base`-slot store, which the §1 rooting obligation must survive),
  Miri on the borrowed-byte island paths, the ulib `strings` and `purvasm-regex` suites (on
  **both** providers — the JS legs now exercise the new `String.js` exports), the examples sweep,
  and the `--no-opt` self-compile byte-identity differential (codegen is untouched, so `.ll`
  byte-identity must hold bit-for-bit).

## Consequences

- The lexer/FSR class is closed at the root: cursor traversal becomes linear (amortised O(1) per
  consumed code point), string compare/hash/search stop paying copy-out, and every string consumer
  (FSR, CST at self-host scale, JSON, regex, future tooling) inherits the fix through the
  rewritten shared surface.
- The runtime gains one kind and a normalisation seam in its string readers: bounded complexity,
  concentrated in the island, Miri-gated. The kind set ABI grows by one (`StrSlice = 10`, §1).
- **Retention is bounded by construction**: a live view holds at most 4× its logical size (§1's
  rule), so no consumer-side discipline is required; `materialize` remains for boundaries that
  want ownership severed below the threshold.
- boot's host FFI needs the matching leaves for oracle/VM parity (leaf addition under the freeze —
  maintainer sign-off requested with this record's review).
- The residual native/JS gap on string workloads returns to the constant-factor dynamic-apply
  class, where ADR-0102's counters and the optimiser track own the roadmap.

## Alternatives considered

- **Cache FSR results (sidecar keyed by source hash).** Rejected (maintainer-directed): masks one
  consumer, leaves the complexity class in every other string path, and self-host cold builds are
  the case that matters.
- **Reconstruct signatures with a lexer-only / declaration-window scan instead of a full parse.**
  Cuts FSR constant factors but not the class (the substrate stays quadratic for the CST parser
  itself, which the self-host build also runs); worth revisiting *after* the substrate fix if FSR
  still shows in profiles.
- **Implement the CST lexer/parser as a native (Rust) foreign.** Fast, but forfeits the pure-PS
  self-host property for a core compiler component and duplicates a large surface behind the FFI;
  contradicts the minimal-FFI policy ([0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md) lineage).
- **Ulib-shadow `language-cst-parser` with a byte-cursor rewrite.** Huge patched surface pinned to
  an upstream version, benefits only that one consumer.
- **Mutable-cursor strings (a stateful reader object).** Fights `String`'s immutable value
  semantics and every existing consumer; the immutable view achieves the same complexity.

#### Progress (2026-07-16): implemented in the pinned order; gates green; quadratic gone

All four steps landed, in the §6 order:

1. **Runtime**: `Kind::StrSlice = 10`, `str_view` normalisation at every reader, bounded-retention
   `build_str_view`, borrowed-byte `eq`/`lt`/hash/search, `str_append2`
   (root → alloc → re-derive → copy), the 10 `pvf_Purvasm_2eString_2e*` leaves, memoised `cp_len`
   (slot-3 write, `CP_UNKNOWN` sentinel). 149 `cargo test` (all §5 correctness fixtures, incl. the
   two forced-GC fixtures and the slice-construction-fires-GC round 3 case), Miri clean on the
   island, clippy/fmt clean.
2. **Surface**: `Purvasm.String` foreign imports + the JS provider's matching exports
   (provider-relative units, as pinned).
3. **Parity leaves**: boot's host registry (`Ffi.host`) **and** the OCaml-backend copy
   (`codegen_ml.foreign` — the standalone-executable leg the ADR's "both providers" wording
   under-counted; three boot legs total, plus JS). `foreign_arity`/FSR pick the arities up without
   codegen changes, as designed.
4. **Consumers**: `Data.String.Internal.Utf8` slice/search/index↔offset → bulk leaves (per-code-point
   codec stays); `CodeUnits.length/charAt/take/drop` → direct `codePointLength`/`codePointAt`/
   `takeCodePoints`/`dropCodePoints`; prelude `eqStringImpl`/`compareStringImpl` → `compareBytes`,
   `concatString` → `appendBulk`; `Json.Core.Utf8.sliceBytes`/`Regex.Core.Utf8.sliceBytes` →
   `byteSlice` (ASCII-delimiter/width-advanced offsets satisfy the boundary contract).

Gates: boot 218 e2e; L2 403 unit + 11 e2e; ulib verify 44 + suites (7 native + json 25/25 +
regex 63/63 on JS); case-mapping E2E; examples sweep 10/10 across VM/ML/LLVM; boot bench oracle
agreement. Measurements re-run in
[sidenote 0017](sidenotes/0017-fsr-native-string-quadratic-measurement.md)'s post-fix section:
probe exponent **1.94 → 1.10**, 50 KB timeout → 4.4 s, `pv_apply_entries` ratio 1.97 (linear),
whole-closure sweep 181.5 s → 38.5 s (JS ratio 109× → ~20×; residual = the ADR-0102
constant-factor class, as §7 predicted).
