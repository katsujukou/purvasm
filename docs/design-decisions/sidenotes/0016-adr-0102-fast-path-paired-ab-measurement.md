# ADR-0102 §5 measurement: paired A/B, exact-saturated fast path vs. pre-fast-path baseline

- Date: 2026-07-15
- Status: measured result; closes ADR-0102 §5's paired-A/B requirement for the first v1.1 slice.
  Numbers below are from the **reviewed and hardened** harness (see "Harness review" below) — an
  initial pass had three latent correctness gaps in the measurement script itself (never in the
  runtime code), caught in review before this record was treated as final; the re-measured numbers
  are consistent with the original pass within noise.
- Related: [ADR-0102](../0102-runtime-v1-1-performance-baseline-for-dynamic-apply.md) (the fast path
  and its measurement requirement), [sidenote 0011](0011-v1-gap-anatomy-post-0079.md) (the "paired
  interleaved runs" measurement discipline this record follows), [sidenote
  0015](0015-native-runtime-perf-investigation.md) (the original profile that motivated the fast
  path), `benchmarks/paired-ab.sh` (the new tool this record's numbers came from — filling the
  "no checked-in paired-interleaved-runs script" gap sidenote 0011 flagged).

## Method

- **baseline** = commit `882f3e6` (§3 counters landed, §2 fast path not yet); **candidate** = `HEAD`
  `9796798` (§2 fast path + §4 heap-words landed). `git diff --stat 882f3e6 9796798 -- runtime/`
  confirms only `runtime/src/*.rs` differs — no `Cargo.toml`/`Cargo.lock` drift, so the two builds
  are directly comparable with no toolchain/dependency confound.
- The baseline runtime staticlib was built in a scratch `git worktree` at `882f3e6`; the candidate is
  the current tree's `runtime/target/release/libpurvasm_rt.a`. Both benchmark binaries were compiled
  from the **same** `purvm` (boot) binary, the **same** CoreFn/ulib inputs, and the **same**
  `--backend llvm --heap-words 33554432` flags, via the new `benchmarks/paired-ab.sh`'s
  `--runtime-lib` lever (`boot/bin/main.ml`'s `resolve_runtime_lib` override) — the runtime staticlib
  is the only variable between legs.
- **Workload**: `benchmarks/bench-run-state-except` (`Bench.RunStateExcept.Main`), ADR-0102 §5's
  designated primary paired-A/B gate — a `Run.State` + `Run.Except` interpreter loop, chosen because
  it stresses the dynamic-apply path directly rather than using self-host compilation as a proxy.
- **Self-check**: both legs' stdout compared before any timing was trusted (`paired-ab.sh` aborts
  loudly on divergence) — confirmed identical at both input sizes below.
- **Wall-clock**: interleaved (baseline, candidate, baseline, candidate, …) min-of-7,
  `PURVASM_STATS` unset, per sidenote 0011's discipline. **Counters**: a separate
  `PURVASM_STATS=1` run per leg — never timed together with the wall-clock runs.
- **Machine**: this session's single development machine, one quiet run — not the CI trend line
  (`.github/workflows/benchmarks.yaml`), which is explicitly documented there as a trend indicator,
  not an A/B referee.

## Harness review

A review of `paired-ab.sh` after the first measurement pass found three latent gaps in the script,
all fixed before these numbers were finalized:

- **Ambient `PURVASM_STATS`/`PURVASM_HEAP_WORDS` were not cleared.** A stray export in the caller's
  shell could silently override the "stats unset" / baked-in `--heap-words` the summary claims —
  especially relevant post-§4, where an ambient heap-size override would win over the value baked in
  at build time. Fixed: every self-check, timing, and stats-snapshot invocation now runs under an
  explicit `env -u PURVASM_STATS -u PURVASM_HEAP_WORDS` (plus `PURVASM_STATS=1` on top for the
  snapshot only).
- **Self-check and the stats snapshot ignored subprocess failure.** Two legs crashing to the same
  (e.g. empty) partial output would have passed the self-check's string comparison; a missing or
  malformed stats line would not have failed the script. Fixed: both now check the child's exit
  status explicitly, and the stats snapshot requires *exactly one* `purvasm-stats:v1` line per leg,
  failing closed otherwise.
- **`--label-a`/`--label-b` doubled as the internal build directory name**, so passing the same
  label for both legs made leg B's build silently overwrite leg A's — `bin_a`/`bin_b` would then
  point at the identical binary and the self-check would pass on a fake A/B comparison. Fixed: the
  internal directories are the fixed `leg-a`/`leg-b`; labels are display-only.

All three were verified by deliberately reproducing each failure mode against the fixed script
(ambient env vars set during a real run; `--label-a`/`--label-b` both `same`; an injected
always-fails stand-in binary) before re-running the measurement below.

## Results

### n = 1000

| leg | wall-clock (min of 7) |
|---|---|
| baseline | 0.0346s |
| candidate | 0.0251s |
| **ratio (baseline / candidate)** | **1.378x** |

Counters (baseline vs. candidate — every key is identical except `entry_exact_fast_hits`):

| counter | baseline | candidate |
|---|---|---|
| `pv_apply_entries` | 181021 | 181021 |
| `heap_apply_activations` | 226862 | 226862 |
| `closure_exact_dispatches` (all positive-arity) | 282659 | 282659 |
| **`entry_exact_fast_hits`** | **0** | **178931** |
| `under_apply` | 16931 | 16931 |
| `over_apply` | 7106 | 7106 |
| `pap_dispatch` | 42485 | 42485 |
| `byneed_dispatch` | 7147 | 7147 |
| `pv_tailcall_writes` | 103723 | 103723 |
| `pending_tail_apply_takes` | 72728 | 72728 |
| `pending_tail_settle_takes` / `pv_settle_slow` | 30995 | 30995 |
| `gc_*` | all 0 (no collection fired at this heap size/input) | all 0 |

63.3% of exact-saturated dispatches (178931 / 282659) took the new allocation-free path.

### n = 8000

| leg | wall-clock (min of 7) |
|---|---|
| baseline | 0.2136s |
| candidate | 0.1475s |
| **ratio (baseline / candidate)** | **1.448x** |

`entry_exact_fast_hits` = 1494098 / 2322544 total exact dispatches = **64.3%** — consistent with
n=1000's share, and every other counter is again bit-for-bit identical between legs (including
`gc_collections=0`: this workload does not force a collection at either size under a 32 Mi-word
heap, so this record says nothing about the fast path's effect under GC pressure).

## Reading

- **The counters are the load-bearing evidence, not the wall-clock number.** Every apply/GC counter
  except `entry_exact_fast_hits` is *exactly* equal between baseline and candidate at both sizes —
  confirming the §2 implementation's design intent (decided during that work): the fast path changes
  *which code path* serves an exact-saturated dispatch, not the event mix itself. This is a stronger,
  noise-free mechanism signal than the timing, and is exactly the kind of causal confirmation ADR-0102
  §3's counters exist to provide.
- **The wall-clock ratio (1.38–1.45x) is real but this is a single machine, single session
  snapshot** — exactly the caveat sidenote 0011 raised about this machine drifting between
  session-to-session absolute-time states. Treat it as a first data point, not a final number; the
  CI trend line (`benchmarks/run-benchmarks.sh` / `.github/workflows/benchmarks.yaml`'s
  `run-state-except` leg) will accumulate more of these over time.
- **The self-hosted-compiler completion gate from sidenote 0015 remains separate and unresolved by
  this measurement.** `Bench.STRef.Main --no-opt --emit-llvm` still needs its own completion/timeout
  check (ADR-0102 §5); `bench-run-state-except` was deliberately chosen instead because it is bounded
  and completes under both legs.
- **The residual ~36% of exact dispatches not on the fast path** are dispatches reached via
  `apply_loop` after a PAP-flatten or a tail bounce — the fast path only fires on the *initial*
  callee of an `apply()` call (ADR-0102 §2's own scoping), so any callee reached mid-trampoline still
  goes through the owned loop. This matches the ADR's framing of the residual: "PAPs, by-need
  forcing, tail bounces … remain" as the next thing the counters point at, not something this slice
  claimed to remove.

## Reproduction

```sh
git worktree add <scratch> 882f3e6   # or any other baseline commit
(cd <scratch>/runtime && cargo build --release)
spago build   # first time only, if Bench.RunStateExcept.Main's CoreFn is missing from output/
./benchmarks/paired-ab.sh --lib-a <scratch>/runtime/target/release/libpurvasm_rt.a \
    --lib-b runtime/target/release/libpurvasm_rt.a \
    --module Bench.RunStateExcept.Main --arg 1000 --heap-words 33554432 \
    --label-a baseline --label-b candidate --reps 7
git worktree remove <scratch>
```
