# 0026. Benchmark harness: deterministic step/alloc cost on the oracle

- Status: Accepted
- Date: 2026-06-20

> **Progress (2026-06-21):** the harness sweeps each program over a range of
> input sizes (the entry is an `Int -> Int`, applied to each size) and records
> steps and allocs per `(bench, size, variant)` to `bench/out/<bench>.dat`, then
> plots them with **gnuplot** to `bench/out/steps.png` and `bench/out/allocs.png`
> (a panel per bench, a curve per variant, log-scaled). Variants today are
> `direct` and the `anf` round-trip; each optimiser pass appends one variant and
> one curve. Run from `boot`: `dune exec bench/bench.exe`. Generated output is
> git-ignored.

## Context

The next work is optimiser passes (DictElim, decision tree, inlining, uncurry,
GER/impurification, DCE). Each is gated for *correctness* by the ADR-0025
round-trip; we also need to judge its *effect* by measurement, not by eye.

There is no bytecode VM yet. Wall-clock time would therefore measure the OCaml
small-step interpreter's own overhead (allocating states and continuations, `Map`
operations) — noisy, and not representative of the eventual runtime. It is the
wrong basis for judging an optimisation now.

What *is* deterministic and meaningful at the oracle stage is how much work a
program does on the machine: the number of **steps** (transitions to a value) and
the number of **allocations** (store growth). An optimisation that removes work
shows up directly and exactly — DictElim removes `Accessor`+`App` steps, inlining
and uncurrying remove application steps, decision trees remove repeated match
steps, GER/impurification turns `Effect` bind-chains into ordinary calls. These
are reproducible counts, diffable across commits.

## Decision

- A **benchmark harness** under `boot/bench/` — a runnable executable, *not* part
  of `dune runtest` — runs a suite of programs on the CESK machine and reports two
  deterministic metrics per program: **steps** (count of `Machine.step`
  transitions to a value) and **allocs** (final store size = total addresses
  handed out; the store is monotonic pre-GC). A static **node count** of the term
  may be printed too, as a cheap code-size proxy.
- Measurement reuses the ADR-0025 path. A pass's effect is the metric *delta*
  between the baseline `rev_transl (transl e)` and the optimised
  `rev_transl (pass (transl e))`, both run on the same oracle; the harness prints
  baseline and (as passes land) each variant side by side.
- Step/alloc on the oracle is a stage-appropriate **proxy** for runtime cost: it
  counts operations the program performs, which correlates with — but is not — the
  eventual bytecode VM's cost. When the VM exists it is benchmarked on the same
  suite for ground-truth performance (and wall-clock); until then step/alloc is
  the basis for judging optimisations, and any wall-clock printed is advisory.
- The harness drives the already-exposed `Machine.inject`/`Machine.step` itself,
  so the machine (the spec) stays free of instrumentation.
- The suite is a handful of programs sized to make deltas legible and chosen to
  exercise the upcoming passes: dictionary-heavy code (DictElim), recursion and
  loops (uncurry, tail calls, decision trees over `case`), and `Effect` loops
  (GER/impurification). It reuses the existing fixtures and adds a few dedicated,
  larger-input benches.

## Consequences

- Every optimiser ADR can quote concrete step/alloc deltas on the suite, so "did
  it help, and where" is answered by data, and regressions are visible.
- Benchmarks are separate from tests (per the project conventions): they report,
  they do not pass/fail. Correctness stays with the round-trip and the unit/e2e
  suites.
- Deterministic metrics are reproducible and diffable across commits, unlike
  wall-clock — suitable for tracking optimisation progress over time.
- Caveat (restated): a step/alloc win is a proxy; a pass that reduces oracle steps
  but would not help the VM (or vice versa) is possible. Re-measuring on the VM
  later closes that gap.

## Alternatives considered

- **Wall-clock only.** Noisy and dominated by interpreter overhead before the VM
  exists; not reproducible enough to judge small wins.
- **No benchmarks until the VM exists.** Then optimisation ADRs could not be
  justified by data and regressions would go unseen; the proxy now is worth its
  caveat.
- **Count steps/allocs inside `Machine.run`.** The harness drives the exposed
  `step`/`inject` instead, keeping instrumentation out of the spec machine.
- **A microbenchmark library (Core_bench etc.) for wall-clock distributions.**
  Overkill and still interpreter-bound now; revisit for the VM, where wall-clock
  becomes the metric of record.
