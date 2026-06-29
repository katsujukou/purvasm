# Case study: the `loadClosure` cold-build bottleneck — refuting ADR-0053 and measuring a byte-oriented parser

- Date: 2026-06-29
- Status: profiling evidence; refutes [ADR-0053](../0053-cheap-json-objects-corefn-decode.md) and
  motivates a new "byte-oriented `Json.Core.Parser`" ADR to supersede it
- Scope: native profiling of the `Building from entry …` → first `compiled …` pause in the
  Level-3 cold build, after ADR-0049/0051/0052 brought the cold build to ~23 s.
- Related: [ADR-0053](../0053-cheap-json-objects-corefn-decode.md) (the refuted hypothesis),
  [ADR-0044](../0044-foreign-object-over-data-map.md) (`Foreign.Object` = `Data.Map`),
  [ADR-0046](../0046-argonaut-core-pure-purescript-ulib.md) (`Json.Core` parser + `Builder`),
  [ADR-0006](../0006-string-utf8-char-int.md) (UTF-8 strings), ADR-0052 (in-place `unsafeSetByte`).

All numbers are native Level-3 (`purvm native -m Purvasm.CLI.Native --ulib dist/ulib`), single-module
`compile`, **min-of-3** wall-clock. The benchmark instrumentation (alternate `Builder`s, a prototype
byte parser) was reverted after measuring; only the numbers remain.

## 0. Where the cold build spends its time

After ADR-0049/0051/0052 a cold 227-module build is ~23 s. Phase markers in `Purvasm.CLI.Build.cmd`
show the pause between `Building from entry` and the first `compiled …` is **`loadClosure` ≈ 21 s
(~91 % of the build)** — reading and decoding the entire `corefn.json` closure up front (`depOrder` /
`map compileModule` / first `compiled` all fire instantly afterwards, so `map compileModule` is lazy
and the compile work streams in the `for_`).

Splitting one module's decode (`jsonParser` then `decodeModule`) showed the parser dominates and
`decodeModule` is negligible (~0.03–0.06 s). So the question was: what in the *parser* costs 21 s?

## 1. ADR-0053's hypothesis is refuted

ADR-0053 proposed that the cost is building every JSON object as a `Foreign.Object` = `Data.Map`
(ADR-0044) via `Obj.fromFoldable`, and that an association-array object representation would collapse
it. As the staged gate (0) of that ADR, the `Json.Core.Parser` was driven with three `Builder`s:

| module | A: argonaut (`Data.Map`) | B: assoc-array | C: discard (no tree) |
|--------|--------------------------|----------------|----------------------|
| Bytecode.Instruction (444 KB) | 0.24 | 0.23 | 0.23 |
| PureScript.CoreFn.Expr (427 KB) | 0.24 | 0.23 | 0.22 |
| Data.Map.Internal (601 KB) | 0.33 | 0.33 | 0.32 |

**A ≈ B ≈ C.** Removing the `Data.Map` build (A→B) changes nothing, and removing the generic `Json`
tree entirely (B→C, a `Builder` that allocates nothing) *also* changes nothing. Therefore:

- the assoc-array object representation (ADR-0053's decision) would **not** help; and
- parse-straight-to-CoreFn (ADR-0053's hedged alternative — skip the generic tree) would **not** help
  either, since C already skips the tree at no saving.

The staged gate did its job: it falsified the hypothesis with a one-line `Builder` change, before any
of the risky bespoke-decoder rework ADR-0053 scoped in.

## 2. The real floor: the code-point-array parser design

The entire parse cost is the parser's own floor. `Json.Core.Parser` first converts the whole input to
a code-point `Int` array — `cps = map fromEnum (toCodePointArray s)` — then indexes into it and rebuilds
each token's string from code points. Splitting the floor (C = full parse, no tree; D = only the `cps`
conversion):

| module | C (full parse) | D (only `cps` conversion) | remainder (per-token) |
|--------|----------------|---------------------------|-----------------------|
| Bytecode.Instruction | 0.23 | 0.07 | 0.16 |
| Data.Map.Internal | 0.32 | 0.10 | 0.22 |

- **~30 %** — the upfront `cps` conversion: `toCodePointArray` (UTF-8 → `Char` array) plus
  `map fromEnum` (→ `Int` array) — two O(n) array passes over the whole input.
- **~70 %** — per token: `slice cps` then `fromCodePointArray` rebuilds each string / key / number
  from code points.

Both are inherent to the "decode to an `Int` array, slice and rebuild" design, and independent of the
object representation — which is why §1 saw no movement.

## 3. Prototype: a byte-oriented parser is ~6× on the floor

A byte-indexed parser was prototyped: it scans the UTF-8 bytes directly (`Purvasm.String.byteAt` /
`byteLength`) and extracts each string / number with `sliceBytes` (a direct byte copy, O(token), no
`cps` array and no per-code-point rebuild). Values are discarded and it returns a count, to force the
realistic floor work; it parses the real corefn end-to-end (e.g. 41,511 values for Bytecode.Instruction).
Head-to-head, same session, min-of-3, against the current cps argonaut parse:

| module | cps argonaut parse | byte-oriented floor | speedup |
|--------|--------------------|---------------------|---------|
| Bytecode.Instruction | 0.25 | 0.04 | **6.2×** |
| PureScript.CoreFn.Expr | 0.25 | 0.04 | **6.2×** |
| Data.Map.Internal | 0.36 | 0.06 | **6.0×** |

The byte floor (~0.04 s) is below even the cps *conversion alone* (D = 0.07 s), because it never builds
the `Int` array. Projection: `loadClosure` ~21 s → ~3.5 s, and the cold build ~23 s → ~5–6 s.

## Conclusion

- ADR-0053 (assoc-array objects; parse-straight-to-CoreFn) is refuted by measurement — neither the
  `Data.Map` build nor the generic-`Json`-tree allocation is on the critical path.
- The bottleneck is the parser's code-point-array design: ~30 % upfront `cps` conversion + ~70 %
  per-token rebuild.
- A **byte-oriented `Json.Core.Parser`** (scan bytes; extract via `sliceBytes`; no code-point array)
  is prototype-confirmed at ~6× on the floor. The real implementation must decode escapes correctly
  (`\" \\ \/ \b \f \n \r \t` and `\uXXXX` → UTF-8, with surrogate pairs); the prototype scanned
  escapes for correct boundaries but sliced raw (sufficient for a floor measurement, since values
  were discarded).

Next step: a new ADR for the byte-oriented parser in `purvasm-json`, superseding ADR-0053.
