# Investigation: FSR on the native backend — the string-substrate quadratic, measured

- Date: 2026-07-16
- Status: measured investigation; establishes the problem [ADR-0103](../0103-native-string-substrate-zero-copy-slices.md)
  is proposed to fix.
- Related: [ADR-0080](../0080-foreign-signature-reconstruction-cst.md) (FSR by full CST parse — the
  *consumer* whose substrate this note measures), [ADR-0006](../0006-string-utf8-char-int.md) (String =
  packed UTF-8 bytes), [ADR-0052](../0052-native-unsafesetbyte-in-place.md) (the per-byte builder
  primitives the ulib string shadows are written over), [sidenote 0015](0015-native-runtime-perf-investigation.md)
  (the dynamic-apply baseline this cost sits on top of), [ADR-0102](../0102-runtime-v1-1-performance-baseline-for-dynamic-apply.md)
  (which made each apply cheaper — and thereby exposed this as a distinct, *super-linear* class).

## Question

The L3-compiler (namely, the PureScript implementation of compiler built with L1 (boot) native compiler)'s `purvasm build` (native) spends conspicuous time in before-per-module-compilation-loop phase foreign-signature reconstruction (FSR), and the
maintainer identified a candidate mechanism: the CST lexer advances its cursor by `SCU.drop` on the
remaining input per token, which on the native backend is a fresh whole-suffix copy built one byte at
a time. This note verifies the mechanism in code and quantifies it with cold, sidecar-free runs,
JS-compared, per the "measurements are the ground truth" instruction.

## Mechanism (code, verified)

- `language-cst-parser`'s lexer keeps the **remaining input as a `String`** and, per token, returns
  `SCU.drop n str` (`.spago/p/language-cst-parser-0.14.1/src/PureScript/CST/Lexer.purs` — the
  `regex`/`string`/`char` combinators).
- On the native backend `SCU.drop`/`take`/`slice` bottom out in the ulib shadow's `sliceBytes`
  (`ulib/strings/Data.String.Internal.Utf8.purs`), which **copies the whole suffix into a fresh
  string one byte at a time** via `Purvasm.String.unsafeSetByte`/`byteAt` — each byte a foreign-leaf
  call through the generic apply path.
- `SCU.length`/`charAt` walk the UTF-8 string from the front (`ulib/strings/Data.String.CodeUnits.purs`)
  — O(n) where the JS backend is O(1)/indexed.
- The runtime string primops **copy guest bytes out into a Rust `String`** before operating
  (`runtime/src/prim.rs`: `pv_prim_eq_string`/`pv_prim_lt_string` copy both operands; `pv_prim_append`
  copies both out, concatenates host-side, then copies back into the guest heap).

So per token the lexer pays O(remaining) byte-copies (each with dynamic-apply overhead), giving
**O(n²) per source file** — a complexity-class defect, not a constant-factor one. The JS backend
never sees it: there `SCU.drop` is V8 `substring`.

## Method

- Binaries: native = `output-purvm/app` (L2 compiler, built 2026-07-15, post-ADR-0102 runtime);
  JS = `node cli/index.node.js` over the same spago `output/`. Same `dist/ulib`, same `output/` inputs.
- Command: `foreign-sigs --corefn-dir <dir> --entry <M>` — loads the entry's closure (per-module
  CoreFn JSON decode) + runs FSR per module. Native runs used `PURVASM_HEAP_WORDS=134217728`;
  `PURVASM_STATS` unset.
- Isolation probe: a scratch corefn dir containing a **single synthetic module** (`Fsr.Probe`,
  a minimal corefn with one `foreign import`, no imports beyond `Prim`) whose `cache-db.json` points
  at a generated `.purs` of controlled size (`module … where` + one `foreign import` + filler
  top-level `Int` bindings). The closure is one module and its corefn decode is trivial, so the
  run time is FSR (`reconstructModule` = full CST parse) plus a measured ~0.03 s command floor.
- Single machine, single session, one quiet run each (not paired-interleaved; the effects measured
  are 40–700× and quadratic-vs-flat, far outside this machine's documented drift band —
  [sidenote 0011](0011-v1-gap-anatomy-post-0079.md)).

## Results

Whole-closure sweeps (decode + FSR):

| entry (closure size) | native | JS | ratio |
|---|---|---|---|
| `Purvasm.CLI.Native` (296 modules, 171 keys) | 181.5 s | 1.66 s | **109×** |
| `PureScript.CST.Parser` (118 modules) | 74.3 s | 0.83 s | **90×** |

Isolated FSR probe (one module, one source file; control = same closure with `foreign: []`, so FSR
never runs):

| source size | native | JS |
|---|---|---|
| control (no FSR) | 0.031 s | — |
| 5 KB | 19.1 s | 0.44 s |
| 10 KB | 73.4 s | — |
| 20 KB | 253.9 s | 0.35 s |
| 50 KB | **> 590 s (timeout)** | 0.40 s |
| 200 KB | — | 0.77 s |

Native scaling exponent between adjacent sizes: log₂(73.4/19.1) ≈ **1.94**, log₂(253.9/73.4) ≈
**1.79** — the predicted quadratic. JS is flat at process-startup cost (≈ linear in the lex itself).
The synthetic token mix costs ≈ 0.6–0.8 s per KB² on this machine.

## Attribution within a real sweep

In the `PureScript.CST.Parser` closure, the modules that take the `fromSource` (CST-parse) path —
foreign-declaring and **not** ulib-overlaid — are 19 files totalling only **43 KB** (largest 5.7 KB;
21 further foreign modules are overlaid and resolve via `ulib.json` with no parse), plus the 6
purvasm-base ABI-floor files (10 KB, parsed again by `loadEnv`). Σ size² ≈ 156 KB²; at the probe's
constant that predicts ~60–110 s of FSR — bracketing the observed 74 s total. Real-source token mix
is evidently somewhat cheaper per KB² than the synthetic, but the conclusion is robust: **FSR is the
dominant share of the sweep, and it burns minutes on kilobytes.** The remainder (CoreFn decode +
command floor) is bounded by the control and the JS leg.

Two consequences for the earlier session analysis, reconciled:

- The "time before the first module compiles" in `purvasm build` is still **not** per-module FSR
  (that runs inside the fold): it is closure decode + the driver's eager whole-program `context`
  (including a duplicated `declsOfModule` pass — a separate, real defect) + `loadEnv`'s ABI-floor
  parses (6 files ≈ 22 KB² ≈ seconds, not the bulk).
- The FSR cost itself is nonetheless a **complexity-class defect specific to the native string
  substrate**, felt per foreign module throughout the build — and the same substrate underlies every
  other string-heavy native path (CST lex/parse at self-host scale, JSON scanning, regex).

## Conclusion

The maintainer's mechanism is confirmed and quantified. This is not "the native runtime is N× slower
per operation" (the sidenote-0015 class); it is **worse asymptotics** introduced by the string
substrate: cursor movement that should be O(1) is O(remaining) with per-byte foreign calls, and
runtime string primops pay copy-out/copy-in. A cache in front of FSR would mask exactly one consumer
and leave the class in place; the substrate fix is [ADR-0103](../0103-native-string-substrate-zero-copy-slices.md).

## Post-fix re-measurement (2026-07-16, ADR-0103 implemented)

Same method, same machine, same fixtures; native = `output-purvm/app` rebuilt on the ADR-0103
substrate (StrSlice views + bulk leaves + rewritten ulib consumers). FSR output verified identical
between the two legs before timing.

Isolated FSR probe (single run each; command floor now negligible relative to every point):

| source size | native (was) | native (now) |
|---|---|---|
| 5 KB | 19.1 s | 0.31 s |
| 10 KB | 73.4 s | 0.62 s |
| 20 KB | 253.9 s | 1.87 s |
| 50 KB | > 590 s (timeout) | 4.38 s |
| 100 KB | — | 10.29 s |
| 200 KB | — | 18.21 s |

Overall scaling exponent 5 KB → 200 KB: log(18.21/0.31)/log(40) ≈ **1.10** (was 1.79–1.94) — the
quadratic is gone; the residual slope is the linear lex itself. `PURVASM_STATS` confirms the class:
`pv_apply_entries` goes 4.48 M (10 KB) → 8.83 M (20 KB), ratio **1.97 ≈ 2.0** — apply traffic is
linear in input size (the old per-byte suffix copies would double the ratio again).

Whole-closure sweep (`foreign-sigs --entry Purvasm.CLI.Native`): **181.5 s → 38.5 s** native
(4.7×), vs 1.96 s JS this session — the native/JS ratio drops 109× → ~20×. The remaining gap is
the ADR-0102 constant-factor dynamic-apply class (per-code-point `decodeAt`/`cpAt` applies in the
lexer and JSON scanner, CoreFn decode, the driver's eager context), not asymptotics: the probe's
exponent and the stats ratio above bound it.
