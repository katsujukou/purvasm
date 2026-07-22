# `tools/` — regression harnesses

Run-from-the-repo-root harnesses (inside `nix develop`). Except where noted they are
**not yet wired into CI** — run them before landing changes to the native backend /
link step / FFI. Wiring the rest into a CI job (one that has the Level-2 CLI built, the
runtime staticlib, staged ulib, `clang`/`llvm`, and `cargo`) is an open follow-up; see
the note at the bottom.

| Harness | Guards | Needs |
|---|---|---|
| `l2-native-behavioural.sh` | The ADR-0104 §2 **Level-2 native behavioural gate** (standing net; **CI-wired**: `l2-behavioural-ci.yaml`): behavioural fixtures (`test-fixtures/l2-behavioural`) compiled by Level-2 to native binaries, run under a small `PURVASM_HEAP_WORDS` in both `--opt`/`--no-opt`; stdout ≡ oracle (frozen boot VM + fixture-owned expected traces), stats schema-checked with `gc_collections >= 1` (forced-GC coverage cannot go vacuous). ADR-0105 slice 0 added the **GC-stress legs** (same binaries under `PURVASM_GC_STRESS=1` — every allocation collects — additionally asserting `gc_copied_words > 0`) and the **debug-ABI-profile leg** (`Gate.RootStress` as entry-call IR via `PURVASM_EMIT_DEBUG_ABI=1`, linked against the **debug** runtime staticlib, stamp-audited, run under stress). | boot, runtime `.a` (release **and** debug), staged ulib (`dist/ulib`), `clang`, `node`, `nm`, workspace `spago build` |
| `selfhost-fixpoint-diff.sh` | The ADR-0104 §2 **self-host fixpoint** (milestone gate): boot seeds `C2` (bytes excluded) → `C2` compiles the pinned closure → stage-3 artifacts + `C3` → `C3` compiles the same closure → stage-4; asserts stage-3 ≡ stage-4 byte-for-byte over every `.ll`, `entry.ll`, `.pmi` (inputs snapshotted; comparison set allowlist-validated). Profiles: `smoke` (`--no-opt`, default) / `milestone` (`--opt`, the required profile — currently **blocked + non-blocking under an explicit maintainer waiver**, ADR-0104 §2 amendment 2026-07-19: `smoke` is the operative gate meanwhile, and each release/ADR-landing checkpoint records a bounded `milestone` re-attempt + the waiver until the tracked perf blocker is resolved). | boot, runtime `.a` (release), staged ulib (`dist/ulib`), `clang`, `node`, workspace `spago build` |
| `native-run-diff.sh` | ADR-0082 native **execution** differential: `Prim`-only fixtures built to a native binary by **both** boot and Level-2, run, and asserted equal. Codegen → `clang` → link → run. | boot, runtime `.a`, `clang`, `purs`, `node` |
| `llvm-diff.sh` | **RETIRED from standing use** (ADR-0104 §4): boot-vs-Level-2 byte-identical `.ll` — fails by design after the first intentional divergence (§3 bridge removal). Kept as a bisection aid against pinned history; see its header. | boot, `purs`, `node` |
| `foreign-sigs-diff.sh` | FSR (ADR-0080) foreign-signature reconstruction vs boot's registry. | boot, `purs`, `node` |
| `ffi-e2e.sh` | ADR-0091 **user-defined native FFI**: app-C sibling `.c` (`PVF_EXPORT` + `-DPVF_MODULE`) and app-Rust `--rust-ffi` crate (`purvasm-bundle` cargo build + `nm` audit) built to a native binary, run, asserted `== 42`; plus two negative cases (a C sibling in a Rust workspace → C-xor-Rust ambiguity; `--runtime-lib` + `--rust-ffi` → rejected). The app-Rust *happy* leg is **skipped** (not failed) without `cargo`; the negatives always run (they error before any bundle build). | runtime `.a` + `purvasm.h`, staged ulib (`$PURVASM_LIB`), `clang`, `purs`, `node`; app-Rust also `cargo` + the runtime crate (`$PURVASM_RT_CRATE`, else `runtime/`) |

`native-run-diff.sh` covers only `Prim`-only pure values (no foreign providers), so
`ffi-e2e.sh` is the counterpart that exercises the whole native-FFI provider-map path.

## CI-wiring follow-up

`ffi-e2e.sh` and `native-run-diff.sh` are the two that assert Level-2 native execution;
neither runs in CI today. `.github/workflows/examples-ci.yaml` already crosses the full
toolchain (runtime build + ulib stage + `cargo`/`clang` + `spago build`, which builds the
Level-2 CLI) but is scoped by design to the boot-driven examples sweep ("Level 2
excluded"). Wiring these two in — either as a step there (`PURVASM_LIB=dist/ulib`,
`PURVASM_RT_A=runtime/target/release/libpurvasm_rt.a`) or as a small dedicated job — is a
CI decision for the maintainer, tracked here so the harnesses are a fixed target, not a
forgotten script.
