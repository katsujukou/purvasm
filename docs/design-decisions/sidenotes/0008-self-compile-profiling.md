# Case study: profiling the Level-3 self-compile — the README's ">10 min" is stale, and the floor is runtime representation, not missing passes

- Date: 2026-06-30
- Status: profiling evidence; motivates [ADR-0059](../0059-native-abi-value-representation.md) (native
  ABI / value representation), and corrects the project README's stale roadmap note.
- Scope: native Level-3 (`cli/bin/purvasm`, the boot-built `Purvasm.CLI.Native`) building its own
  242-module CoreFn closure (`output/`) to `app.pvm`, after
  [0049](../0049-eliminate-superlinear-bytecode-construction.md)/[0051](../0051-flatten-json-serialization.md)/[0052](../0052-native-unsafesetbyte-in-place.md)/[0054](../0054-byte-oriented-json-parser.md).
- Related: [ADR-0036](../0036-anf-to-ocaml-value-representation.md) (the OCaml-host value rep / hybrid
  calling convention being profiled), [sidenotes/0007](0007-loadclosure-parse-profiling.md) (the prior
  parse-floor profiling), [ADR-0050](../0050-build-streaming-incremental-reuse.md) (peak-residency /
  streaming, still Proposed).

Method: `PURVASM_LIB=cli/bin/ulib cli/bin/purvasm build --entry Purvasm.CLI.Native --corefn-dir output`.
Wall + max-RSS from `/usr/bin/time -l`; phase boundaries from per-line high-resolution timestamps
(`perl -MTime::HiRes`); CPU self-time from macOS `sample` (1 ms, ~8 s), bucketed by symbol.

## 0. The headline: the build is ~7 s, not ">10 min"

The README roadmap still says "compiling 227 modules takes more than ten minutes." That predates the
2026-06-28/29 performance ADRs. Measured now:

- **6.8 s wall**, **225 MB max RSS**, 242 modules, `app.pvm` written (857 KB), exit 0.

So the premise that "the Level-3 compiler is extremely slow → optimisation is urgent for *build speed*"
is **stale by roughly 60×**, and [0049](../0049-eliminate-superlinear-bytecode-construction.md)–[0054](../0054-byte-oriented-json-parser.md)
already closed it — and they were O(n²) *bug fixes*, not optimiser passes. (The README roadmap line
should be corrected to match.)

This does **not** mean optimisation is unneeded: Level-2 still performs **no** optimisation, so the code
it *emits* is unlikely to be competitive with `purs` (JS) / `purs-backend-es` (ES) — but that is a
separate, still-unmeasured question (the wall-4 3-way benchmark), distinct from build speed.

## 1. Phase split — parse+compile dominates, serialize is second

Per-line timestamps over `Purvasm.CLI.Build.cmd`. `map compileModule` is strict, so all compilation
finishes before the first `compiled …` line; the `for_` loop is serialise+write; the tail is link +
`imageToString`.

| phase | time | share |
|-------|------|-------|
| `loadClosure` parse + all `compileModule` | 3.95 s | 60 % |
| serialise + write (`.pmo`/`.pmi`) | 2.06 s | 31 % |
| link + `imageToString` (`app.pvm`) | 0.55 s | 8 % |

## 2. CPU profile — ~60 % is runtime-representation overhead, not compile algorithm

`sample` self-time (top of stack), 5 556 on-CPU samples, bucketed:

| bucket | share | what it is |
|--------|-------|------------|
| generic apply (`app_589`) | 16.8 % | [0036](../0036-anf-to-ocaml-value-representation.md)'s eval/apply fallback (indirect/curried calls, PAP building) |
| GC + allocation | 18.6 % | `do_some_marking`, `pool_sweep`, `oldify_*`, `caml_alloc*`, `caml_modify`, `caml_update_dummy` |
| scalar coercions | 11.9 % | `as_int` / `as_str` / `as_bool` — unboxing the uniform value rep |
| native foreign dispatch | 6.9 % | `foreign_*` |
| int primops | 5.5 % | `p_add_int` / `p_lt_int` / `p_eq_int` … |
| **runtime overhead subtotal** | **~60 %** | |
| everything else | 40 % | the actual compiler logic, spread across hundreds of generated functions — **no single algorithmic hotspot** |

The single heaviest named symbol is `app_589` (the generic apply), and `as_int`/`as_str` are pure
boxing tax. GC pressure is downstream of the allocation rate that boxing + PAP-building + transient
`Maybe`s create.

## 3. Why this refutes "port the optimiser to speed the build up"

Two compounding reasons the build will not get faster by porting `DictElim`/`Simplify`/`DBE` into
`compiler/`:

1. **Level-3 is built by `boot`**, whose `purvm native` path already applies those passes
   (`boot/bin/main.ml`'s `native_action`: `Dbe.run (Simplify.run (Dict_elim.run …))`). The passes are
   *already on*. The `.pmo` separate-compilation path runs none, but that path is not what produces the
   running Level-3 binary. Porting the same passes to `compiler/` only affects what `purvasm-ps` emits
   when it eventually compiles *itself* (Level 4), not this build.
2. The dominant cost is **representation overhead the existing passes do not remove** (`app`, boxing,
   GC). The pass that *would* cut the residual `app` dispatch is the deferred general inliner /
   caller-homed specialisation (ADR-0029's documented wall), not the cheap three.

## Conclusion

- The ">10 min" build-speed premise is stale (now ~6.8 s / 225 MB); the perf ADRs already fixed it.
- The native floor is **runtime representation** — generic apply + scalar boxing + the GC pressure they
  cause — i.e. exactly [ADR-0036](../0036-anf-to-ocaml-value-representation.md)'s deferred unboxing /
  calling-convention items. That is what the native ABI ([ADR-0059](../0059-native-abi-value-representation.md))
  is positioned to fix: `i31` immediates, module-local scalar unboxing, and a leaner calling
  convention, on an owned representation.
- Optimising Level-2's *emitted code* (vs `purs`/`purs-backend-es`) remains a real and separate goal,
  to be driven by the wall-4 3-way benchmark — not assumed.
