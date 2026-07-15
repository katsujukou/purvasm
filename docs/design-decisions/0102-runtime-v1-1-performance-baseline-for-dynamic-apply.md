# 0102. Runtime v1.1 performance baseline for dynamic apply

- Status: ~~Proposed~~ **Accepted** _(2026-07-15: accepted by the maintainer)_
- Date: 2026-07-15

## Abstract

Define "runtime v1.1" as a narrow performance increment over the accepted v1 native runtime: keep the
single-capability heap, current C ABI, shadow stack, Cheney semi-space collector, and trampoline
semantics, but make the unavoidable dynamic `apply` path cheap enough for self-hosted `Run`/`Free`
workloads to be a viable execution substrate. The first implementation slice is an allocation-free
exact-saturated closure fast path inside `Heap::apply`, paired with apply/GC counters and a fixed
self-host measurement protocol.

## Context

The native self-hosting compiler exposes a different performance problem from the earlier
`llvm/ml` micro-benchmark gap. The v1 boundary-cost arc already removed many static-call and rooting
crossings:

- [ADR-0076](0076-direct-known-arity-calls-musttail.md) added direct known-arity entries and `musttail`
  for statically resolved saturated calls.
- [ADR-0077](0077-cross-module-direct-calls-pmi-arity.md) extended direct calls across the `.pmi`
  surface.
- [ADR-0079](0079-ctx-header-abi-inline-rooting-fast-paths.md) made the hot root/get/frame/pop/settle
  choreography visible as inline IR.

Those levers help calls whose callee is statically known. The self-hosted compiler's hot path is
different: `Run`/`Free` interpreters, `Argonaut` CoreFn decode, traversals, callbacks, handlers, and
continuations generate a high density of genuinely dynamic calls. These cannot all be erased by the
direct-call path without changing the abstraction or relying on whole-program optimiser success.

The current native-L2/L3 investigation in [sidenote 0015](sidenotes/0015-native-runtime-perf-investigation.md)
shows the shape:

- `Bench.STRef.Main --no-opt --emit-llvm` finished through the JS L2 path in about 1.2 s, while the
  native L2 compiler remained unfinished after more than 6.5 min in the observed run.
- The default 8 MiB semi-space OOMs on the workload; a 1 GiB heap removes that OOM but does not remove
  the time cliff.
- `sample` puts `pv_apply` at the top, with `pv_settle`, `Heap::apply`, and system malloc/free also
  prominent.
- The code path explains the profile: `runtime/src/apply.rs::Heap::apply` copies the incoming argument
  slice into a `Vec` on entry, then copies the saturated prefix into another `Vec` before calling the
  closure body. For the common exact-saturated closure call with positive arity, this is two host
  allocations before the guest function body even starts.

This matters semantically, not only as a local benchmark issue. `Run` is one of the standard
PureScript application-architecture abstractions. If every dynamic apply in a `Run` interpreter pays
multiple host allocations, then programs built around `Run` can be correct but practically unusable
on the native backend. That would undercut the native backend's goal more deeply than an isolated
micro-benchmark gap.

At the same time, the fix must not reopen the v2 runtime design. [ADR-0061](0061-capability-local-shared-immutable-gc.md)
and [ADR-0062](0062-mn-work-stealing-scheduler-fibers.md) reserve multi-capability heaps,
shared-immutable regions, M:N work stealing, fibers, and `AVar` for the v2 line. The present need is a
v1-compatible performance baseline.

## Decision

### 1. Name the scope "runtime v1.1"

Runtime v1.1 is an implementation and measurement line for the existing v1 runtime. It preserves:

- the current generated-code/runtime C ABI, including the `pv_apply`/`pv_tailcall`/`pv_settle`
  protocol;
- one capability and one local moving heap;
- the shadow-stack rooting contract;
- the current trampoline semantics for generic tail calls and over-application;
- the current value representation and heap object layouts;
- the current correctness differential and Miri/runtime split.

It may change the implementation behind those contracts when doing so removes measured overhead from
dynamic apply. It is not a semantic redesign and is not the first slice of v2.

### 2. Add an allocation-free exact-saturated closure fast path inside `Heap::apply`

The first implementation slice front-loads the common case before materialising the working
`Vec<Value>`:

```text
given Heap::apply(f, args):
  if f is a live Closure and args.len == closure.arity:
    call the closure code with the original borrowed args slice
    if no pending tail was stashed:
      return the result
    otherwise:
      continue in the existing owned trampoline loop with the stashed tail
  otherwise:
    run the existing generic loop
```

This fast path must use the same release validation tier as the existing path: a stale, foreign,
forwarded, non-pointer, or non-callable callee is still rejected before any unchecked header or code
word access. The fast path is an implementation shortcut for a case the existing loop already accepts;
it is not a weaker entry.

The rooting contract is unchanged. `apply` does not root the callee's saturated arguments today; the
callee owns rooting any closure or argument it holds across its own safepoints. In the exact-saturated
case, `apply` does not need the input `args` after the code call unless the callee stashes a pending
tail, and that stashed tail already carries owned arguments through the existing protocol. Therefore
passing the original borrowed slice is compatible with the per-function rooting discipline.

The existing loop remains the authority for:

- under-application and PAP construction;
- PAP flattening;
- by-need callee forcing;
- over-application and continuation stacks;
- pending-tail bounces after the first exact call;
- all uncommon or diagnostically delicate cases.

This keeps the first slice deliberately small: remove the two common host `Vec` allocations without
rewriting the whole trampoline.

### 3. Add low-noise dynamic-apply counters with fixed event meanings

Runtime v1.1 adds opt-in counters for dynamic apply analysis. They should be disabled or inert by
default, and emitted only when requested, for example through a native-runtime environment flag such
as `PURVASM_STATS=1`.

The first counter set fixes the event boundaries so results can be compared across implementation
slices:

- **ABI `pv_apply` entries**: increments only in the exported `pv_apply` entry. This is not the same
  as a `Heap::apply` activation, because `pv_settle` slow path and runtime helpers/internal
  operations may enter `Heap::apply` directly.
- **`Heap::apply` activations**: increments once per call to `Heap::apply`, regardless of whether the
  caller was `pv_apply`, `pv_settle`, `run_effect`, or another runtime helper/internal operation.
- **Closure exact dispatch iterations**: increments every time the apply loop dispatches a
  `Kind::Closure` with `args.len == arity`, including exact dispatches reached after PAP flattening
  or pending-tail bounces.
- **Entry exact-fast-path hits**: increments only when the new pre-loop borrowed-slice fast path is
  taken at the beginning of a `Heap::apply` activation.
- **Zero-argument vs positive-argument exact dispatches**: split the closure-exact counter so the
  measured allocation opportunity is visible.
- **Fallback dispatch classes**: under-application, over-application, PAP dispatch, and by-need
  dispatch.
- **Trampoline events**: `pv_tailcall` writes, pending-tail takes by the apply loop, and pending-tail
  takes by `pv_settle`.
- **`pv_settle` cases**: fast path (`pending_tail == None`) vs slow path (`Some`, enters
  `Heap::apply`).
- **GC collections**: increments once per `Heap::gc` invocation.
- **Total GC time**: measures the whole `Heap::gc` invocation, including collection, semi-space swap,
  post-collection live-word accounting, and object-start rebuild. It does not include stats summary
  formatting.
- **Copied words**: accumulates the post-collection live words for each collection.
- **Max live words**: the maximum post-collection live words observed across collections.

Counter storage is per runtime context, uses unsigned saturating counters, and emits one summary to
`stderr` from `pv_runtime_free` when stats are enabled. A leaked context may lose its summary; that is
acceptable for the first diagnostic slice. The stats-enabled path may read clocks only around GC
collections, not on every apply event.

`PURVASM_STATS` parsing is strict: absent means disabled, exactly `1` means enabled, and any other
present value aborts runtime creation with a clear diagnostic. The emitted schema is a single
machine-readable line:

```text
purvasm-stats:v1 pv_apply_entries=<u64> heap_apply_activations=<u64> closure_exact_dispatches=<u64> closure_exact_zero=<u64> closure_exact_positive=<u64> entry_exact_fast_hits=<u64> under_apply=<u64> over_apply=<u64> pap_dispatch=<u64> byneed_dispatch=<u64> pv_tailcall_writes=<u64> pending_tail_apply_takes=<u64> pending_tail_settle_takes=<u64> pv_settle_fast=<u64> pv_settle_slow=<u64> gc_collections=<u64> gc_total_ns=<u64> gc_copied_words=<u64> gc_max_live_words=<u64>
```

Keys are append-only within `v1`; a breaking schema change must bump the prefix version. The line is
newline-terminated and written only once per context.

When stats are disabled, the hot path must perform no formatting, no clock reads, no atomics, and no
counter writes. The permitted disabled-path cost is a plain context-local flag check at each
instrumentation point. Wall-clock performance claims are measured with stats disabled; counter
snapshots are collected in separate stats-enabled runs.

Instrumentation lands before the fast path. The required sequence is:

1. Land counters with the existing behavior and capture the baseline event mix.
2. Land the exact-saturated fast path.
3. Re-capture the event mix and wall-clock measurements under the same workload set.

The purpose is not permanent observability infrastructure. It is to make the v1.1 performance work
causal: after the exact fast path lands, the project should know whether the remaining wall time is
still dynamic dispatch, tail bouncing, PAP/ByNeed traffic, guest allocation/GC, or something else.

### 4. Make heap-size experiments possible without relinking

The generated entry still supplies a compile-time default heap size, but the native runtime may
override that value at context creation from an explicit environment variable such as
`PURVASM_HEAP_WORDS`. This is operationally scoped to measurement and robustness: it removes the need
to rebuild or relink a native compiler just to test whether an OOM or GC cliff is heap-size-related.

This is not heap growth and not a GC design change. The override policy is fixed:

- if `PURVASM_HEAP_WORDS` is absent, use the codegen-provided default;
- if it is present, accept only a non-empty positive decimal `usize`;
- empty, zero, overflow, leading sign characters, separators, whitespace, or any non-decimal byte
  abort context creation with a clear diagnostic.

The parser is a pure helper tested directly. Process-level behavior is covered by subprocess tests so
environment mutation does not race parallel tests.

### 5. Verification and measurement are part of the decision

The fast path is accepted only with tests that exercise both the new branch and the fallback boundary:

- exact saturated closure call behavior is unchanged;
- a callee that allocates and roots its own heap-pointer argument across GC still works;
- a callee that stashes a pending tail from the exact fast path continues through the existing
  trampoline correctly;
- under-application, over-application, PAP, and by-need behavior remain covered by the existing
  runtime tests;
- `PURVASM_HEAP_WORDS` is verified for the absent case, a valid override, empty value, zero,
  malformed text, and overflow;
- `PURVASM_STATS` is verified by subprocess tests for the absent case, `1`, and any other present
  value;
- stats-disabled runs produce no stats line on `stderr`;
- stats-enabled runs emit exactly one newline-terminated `purvasm-stats:v1` line when the runtime
  context is freed;
- the emitted schema has every required key exactly once, with no missing or duplicate keys;
- deterministic stats fixtures exercise exact fast-path, fallback dispatch, tail bounce, and
  `pv_settle` slow/fast paths, and assert their stable counter values;
- a tiny-heap forced-GC fixture asserts that `gc_collections` increases, `gc_copied_words` is at
  least `gc_max_live_words`, `gc_max_live_words` is non-zero after a live collection, and
  `gc_total_ns` exists and is a non-negative integer. It must not assert an exact time value;
- native ABI behavior remains covered by the existing generated-code differentials.

The performance claim requires at least one bounded workload that completes under both the baseline
and candidate native runtimes. That bounded workload is the primary paired A/B gate and is fixed for
the first v1.1 slice as `Bench.RunStateExcept.Main` from `benchmarks/bench-run-state-except`: a
`Run` API workload combining `Run.State` and `Run.Except` intended to stress the dynamic
interpreter path directly instead of using self-host compilation as the only proxy.

The benchmark is part of the benchmark corpus and is registered in `benchmarks/run-benchmarks.sh` as
`run-state-except` with initial size `1000` and LLVM heap words `33554432`. It emits bounded
differential output (`final` state plus step count) rather than one line per interpreter step, so
stdout volume is not the measured hot path. The benchmark harness checks that the output is identical
across legs; it is not an internal closed-form oracle. This benchmark must remain harness-ready: it
must build through the ordinary Spago workspace, run to completion under the baseline and candidate
runtimes for the selected input size and heap size, and keep its selected input size small enough for
interleaved paired A/B runs but large enough that process startup noise does not dominate.

Additional bounded workloads may be added later, for example a small closure-count variant of the
self-host emit workload or a fixed module-prefix L3 emit workload whose baseline completes under the
selected heap size.

The full self-host native L2 compiler running `Bench.STRef.Main --no-opt --emit-llvm` remains a
separate completion/timeout gate, because the observed baseline did not finish in the investigation
window. It may report "candidate completes" or "timeout lower bound improved", but it cannot be the
sole source of a paired speed ratio until both sides complete.

Every measurement report must include:

- the apply/GC counters from §3 captured before and after;
- wall-clock paired A/B runs with stats disabled;
- separate stats-enabled runs for counter snapshots;
- interleaved runs for the bounded workload, with the same heap size and build flags, following the
  measurement discipline in [sidenote 0011](sidenotes/0011-v1-gap-anatomy-post-0079.md).

## Out of scope

Runtime v1.1 deliberately excludes:

- multi-capability heaps, shared-immutable heaps, work stealing, fibers, `AVar`, and the v2 scheduler;
- generational GC, large-object spaces, concurrent GC, or heap growth as a collector policy;
- public ABI widening for inline object field reads or inline allocation;
- unguarded public `pv_apply` entries or crate-wide `panic = "abort"` as a first performance lever;
- allocator replacement as the fix, though allocator A/B may be used as a diagnostic;
- higher-order specialization, PAP-CAF specialization, uncurrying, or any middle-end pass. Those may
  become follow-up ADRs once runtime v1.1 exposes the residual apply mix.

## Consequences

- The unavoidable dynamic call path becomes cheaper without changing PureScript semantics, object
  representation, or the generated-code ABI.
- `Run`/`Free` workloads get a runtime-level baseline before the optimiser is asked to erase their
  higher-order structure. This is important: the native backend should run well-typed dynamic
  abstractions acceptably even when specialisation cannot prove them away.
- The first slice has a high expected payoff because it removes host allocations from the common
  exact-saturated closure call while leaving the tricky trampoline cases alone.
- The improvement has a clear ceiling. PAPs, by-need forcing, tail bounces, over-application,
  `pv_apply`'s C boundary, and guest allocation remain. The counters are how the next bottleneck is
  chosen rather than guessed.
- The runtime implementation gains a second closure-dispatch path. The risk is bounded by making it a
  strict subset of the existing path and by falling back to the old loop for all non-exact cases.
- Heap-size override and GC stats make OOM/GC claims measurable, but they do not themselves solve
  guest allocation pressure.

## Alternatives considered

- **Start with higher-order specialization.** Rejected for this increment. HOS is likely valuable for
  recursive HOFs and `Run` residuals, but it reduces call count rather than making unavoidable dynamic
  calls cheap. Starting with runtime v1.1 gives HOS a clean baseline and avoids making correctness and
  blowup-prone optimiser work carry a runtime allocation bug.
- **Swap the global allocator.** Rejected as the durable fix. `mimalloc`/`jemalloc` A/B can help prove
  host allocation is causal, but the better fix is to remove the two temporary `Vec` allocations from
  the hot path.
- **Start with generational GC.** Rejected for this increment. The 8 MiB OOM proves heap pressure, but
  the 1 GiB run still shows a severe time cliff and the profile points first to mutator-side
  `pv_apply`/malloc. GC stats should precede GC design.
- **Make `pv_apply` unguarded or add a separate unchecked ABI entry.** Rejected for v1.1. Panic
  containment and the current release-fault posture are part of the v1 ABI safety story. If guard
  overhead remains material after allocation removal, it can be measured and designed separately.
- **Rewrite the entire apply trampoline around borrowed/small buffers now.** Deferred. That may be the
  right second slice if counters show tail/PAP/over-apply traffic dominates, but the exact-saturated
  closure case is smaller, safer, and directly targets the observed malloc profile.
- **Treat this as v2 runtime work.** Rejected. The self-host compiler needs a usable dynamic apply
  baseline before the multi-capability scheduler/heap design lands, and the proposed changes are
  intentionally compatible with the accepted v1 contracts.
