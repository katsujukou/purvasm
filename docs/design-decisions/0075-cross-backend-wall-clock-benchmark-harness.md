# 0075. Cross-backend wall-clock benchmarks

- Status: ~~Proposed~~ **Accepted** _(2026-07-04: accepted by the maintainer)_
- Date: 2026-07-04

## Abstract

Cross-backend wall-clock benchmarks (wall 4): input-size log sweeps, whole-process min-of-k timing, per-series scaling exponent + ratio-to-`js` baseline; `benchmarks/` PS corpus fed by argv (`purvm run` forwards guest argv); the [0026] steps/allocs harness stays the optimiser regression gate

## Context

The roadmap's wall 4 ([0059](0059-native-abi-value-representation.md) Context) is a **JS / ES /
Native benchmark that drives optimisation from there**. That wall is now reachable: the LLVM-native
Level-2 compiler runs on the owned runtime and self-compiles byte-identically to boot, and the v1
performance levers deliberately deferred by [0064](0064-v1-single-capability-native-abi-codegen-contract.md)
(direct known-arity fast path, `musttail`, inline primops, rooting minimisation, heap growth) all
need a measurement vehicle before any of them is pulled — the project's standing *profile before
designing* lesson.

The existing harness ([0026](0026-benchmark-harness.md)) measures **oracle steps/allocs**:
deterministic, noise-free, and exactly right as the *optimiser regression gate* — but it runs
embedded `Cesk.Ast` terms directly on the CESK machine, so it cannot measure a native binary, wall
time, or any cross-backend comparison. What the native era needs (maintainer requirement) is
**input-size versus execution-time sweep graphs** across implementations.

Two further constraints:

- purvasm has **no clock leaf**, and measurement is not a reason to add one — the guest ABI stays
  clean, so timing must be external to the process.
- The corpus must be **real PureScript programs** compiled by the real pipelines (a cross-backend
  wall-clock number for a hand-built OCaml term measures nothing anyone ships).
  [CLAUDE.md] already separates the roles: *examples* are demonstrations; *benchmarks* are solely
  for performance evaluation.

## Decision

### 1. Axes and series

For each benchmark program: **x = input size `n`** (a log sweep — doubling `2^k` steps),
**y = wall-clock time of the whole process** (spawn → exit), one series per implementation:

- `vm` — `purvm build` + `purvm run`;
- `ml` — `purvm native --backend ocaml`;
- `llvm` — `purvm native --backend llvm` (release staticlib — the default lookup);
- `js` — stock `purs` output on Node;
- `es` — `purs-backend-es` output on Node (wired when the driver grows the leg; the toolchain is
  already in the devshell).

Whole-process time includes init; that is intentional (init is real cost for compiler-shaped
workloads) and the sweep exposes the asymptote regardless.

### 2. Two derived numbers, not just curves

Each series additionally reports:

- **the scaling exponent** — the slope of a least-squares fit on the log-log points. This turns
  the [0049](0049-eliminate-superlinear-bytecode-construction.md)-class check ("the synthetic
  exponent fell ≈1.5 → 1.003") into a standing numeric output: an accidental O(n²) shows up as a
  slope shift, comparable across runs and in CI, not as a visual impression;
- **the ratio against a baseline series at the largest common `n`** (baseline: `js` — the wall-4
  competitiveness question in one number per backend).

### 3. Noise discipline

**min-of-k** per point (k = 3 by default) — the project's established convention
([sidenotes/0007](sidenotes/0007-loadclosure-parse-profiling.md)/[0008](sidenotes/0008-self-compile-profiling.md)
min-of-3); min is the standard noise-rejector for CPU-bound batch runs. A per-point **time budget**
bounds the sweep: once a series' point exceeds the budget, larger sizes for that series are
skipped (reported as such — no silent truncation).

### 4. Corpus: `benchmarks/` workspace packages, input via `argv`

Benchmark programs are ordinary spago workspace packages under **`benchmarks/`** (a sibling of
`examples/`, matching [CLAUDE.md]'s role split), each a `main :: Effect Unit` that reads its input
size from **argv** and prints a small self-check result (so a wrong answer is loud in every
backend). The `argv` channel is the one input mechanism that is uniform and sweep-friendly across
all five legs — `--arg N` is baked at build time (one binary per size), an environment variable is
uniform but gratuitous when argv exists. The established drop-one convention holds
([0045](0045-native-cli-run-interpreter-io-leaves.md)/[0056](0056-purvasm-system-host-leaves.md):
element 0 is the executable/script; consumers drop one element to get user arguments).

**`purvm run` forwards trailing arguments as the guest argv.** Today the VM-path `argvImpl` reads
`Sys.argv` — purvm's own argv, subcommand included. This record changes the `run` action to expose
`[image path] ++ trailing args` to the guest `argvImpl` leaf, so `purvm run app.pvm 100000` and
`./app 100000` present the same drop-one shape. (This is the only toolchain behaviour change in
this record; the boot native / LLVM / Node legs already behave this way.)

### 5. Driver and output

A checked-in driver (`benchmarks/run-benchmarks.sh`, the `examples/run-examples.sh` shape) builds
each program once per backend (cached), runs the sweeps, and writes:

- **CSV** per (program, backend): `benchmarks/out/<program>.<backend>.csv` with `n, seconds`
  rows (min-of-k already applied);
- **plots** via gnuplot (the [0026](0026-benchmark-harness.md) precedent) — one log-log chart per
  program, one series per backend;
- **a summary table** to stdout: per series the fitted exponent and the ratio to baseline at the
  largest common `n`.

Timing is external (`/usr/bin/time`-class / shell timing) — no guest clock, no runtime change.
LLVM legs may pass `--heap-words` per program (the v1 heap is fixed,
[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §4); the driver records the value used.

### 6. The oracle bench keeps its job

[0026](0026-benchmark-harness.md)'s steps/allocs harness is **unchanged**: deterministic guest
metrics remain the optimiser regression gate ([[bench-regression-check]] discipline). The two
harnesses share nothing but intent — this record adds the wall-clock/cross-backend instrument, it
does not replace the deterministic one.

### First corpus (initial, grows on demand)

Ports of the existing bench shapes as parameterised PS programs — `fib` (call-heavy),
`count-state` (the State-monad collapse / stack-safety workload), `map-fold-array` (array
traversal), `quicksort` (allocation + comparison), plus `json-parse` (a `purvasm-json` document of
size `n` — the real self-compile hotspot family). Each is small, self-checking, and exercises a
distinct cost centre ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)'s deferred
levers map onto them: apply/TCE → `fib`/`count-state`, primops/arrays → `map-fold-array`,
GC/allocation → `quicksort`/`json-parse`).

## Consequences

- Wall 4 turns on: the JS/ES/Native comparison exists as graphs plus two numbers per series, and
  each deferred v1 lever ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)) can be
  measured lever-by-lever against the same corpus.
- Scaling regressions become numeric and CI-comparable (the exponent), closing the gap where an
  O(n²) hid behind "it finished eventually" until a 200-second build exposed it
  ([sidenotes/0003](sidenotes/0003-wpo-to-smo-blowup-retrospective.md) lesson).
- `purvm run` gains guest-argv forwarding — a small, uniform interface change; all other legs are
  untouched.
- The guest ABI stays measurement-free (no clock leaf).
- Cost: a new `benchmarks/` package family to maintain, and wall-clock numbers are
  machine-relative (the exponent and the ratios are the portable outputs; absolute seconds are
  not).

## Alternatives considered

- **A guest clock leaf + in-program timing.** Excludes init and process noise, but pollutes the
  ABI for measurement's sake and forks timing semantics across five backends. Rejected — external
  whole-process timing with size sweeps answers the questions wall 4 actually asks.
- **Reuse the OCaml-term corpus of [0026](0026-benchmark-harness.md).** Not cross-backend (terms,
  not programs) and not representative of the compiled pipelines. Rejected; the steps/allocs
  harness keeps that corpus for its own job.
- **`hyperfine` as the runner.** A good tool, but an extra toolchain dependency for min-of-k
  timing a 20-line loop provides; the driver stays dependency-free. Declined for now (the CSV
  format would survive a later switch).
- **Environment-variable input instead of argv.** Uniform but redundant once `purvm run` forwards
  argv; argv matches how every other backend already receives input. Rejected.
