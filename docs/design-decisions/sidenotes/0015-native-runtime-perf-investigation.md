# Investigation: native L2 compiler runtime performance on the L3 build

- Date: 2026-07-14
- Status: measured investigation; establishes the runtime-v1.1 performance baseline problem later
  captured by [ADR-0102](../0102-runtime-v1-1-performance-baseline-for-dynamic-apply.md).
- Related: [sidenote 0011](0011-v1-gap-anatomy-post-0079.md) (the post-0079 v1 lever map),
  [ADR-0064](../0064-v1-single-capability-native-abi-codegen-contract.md) /
  [ADR-0066](../0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) /
  [ADR-0071](../0071-codegen-runtime-c-abi.md) (the v1 runtime contracts),
  [ADR-0076](../0076-direct-known-arity-calls-musttail.md) /
  [ADR-0079](../0079-ctx-header-abi-inline-rooting-fast-paths.md) (the earlier static-call and
  boundary levers), and [ADR-0099](../0099-generalized-effect-reflection-cperform.md) (ruled out as
  the cause).

## Terminology

This note uses three level names consistently:

- **L1 = boot**: the OCaml bootstrap compiler.
- **native L2 compiler**: the LLVM-native binary produced when L1 compiles the PureScript compiler
  itself, e.g. `output-purvm/app`.
- **L3 build**: using that native L2 compiler binary to build the next program. The runtime slowdown
  investigated here is the native L2 compiler's execution time during an L3 build.

## Executive summary

Two independent performance problems appeared together:

1. **The native L2 compiler binary is too slow when it runs.** The strongest current explanation is
   that the v1 runtime's dynamic apply path (`pv_apply` / `Heap::apply`) performs host `Vec`
   allocations on the common exact-saturated closure call. The compiler workload emits a very large
   number of dynamic applies through `Run`/`Free` interpretation and Argonaut CoreFn decoding, so
   apply dispatch plus system malloc/free dominate wall time.
2. **Producing the native L2 compiler is also slow.** The L1 → native-L2 build took about 23 minutes,
   of which roughly 18 minutes were child `clang` time compiling a huge `.ll`. That is a separate
   code-size / native-codegen pipeline problem, not the runtime problem this note analyses.

Neither issue is a GER / ADR-0099 regression. The dynamic-apply cost is a latent v1 runtime design
cost that became visible only once the self-hosted native compiler could run a real workload.

## Observed symptoms

The measurements below used the same target, `Bench.STRef.Main --no-opt --emit-llvm`, to exclude
`clang` and measure only purvasm's emit path.

| # | experiment | result | interpretation |
|---|---|---|---|
| 1 | JS L2 path vs native L2 compiler | JS completed in about 1.2 s; native remained unfinished after more than 6.5 min | The algorithmic work is not inherently huge; the native runtime is the likely cause. |
| 2 | native with the default 8 MiB semi-space | OOM after about 11.5 s: `live set exceed semi-space 1048576 words`; `--opt` showed the same class of failure | Not specific to `--no-opt`. |
| 3 | native with a 1 GiB semi-space (`--heap-words 134217728`) | OOM disappeared, but the run still remained unfinished after more than 6.5 min | Heap size alone does not explain the time cliff; GC may amplify but is not the whole story. |
| 4 | per-module elapsed time vs emitted `.ll` size | no useful correlation; one small `.ll` module took about 51 s while a much larger one was effectively instant | String output size is not the primary explanation. |
| 5 | macOS `sample` profile | `pv_apply`, `pv_settle`, `Heap::apply`, and system malloc/free dominate | Dynamic apply plus host allocation is the main suspect. |

The representative leaf-time profile:

```text
14150  pv_apply
 2377  fn_25 / 1682 fn_25$d
 1245  szone_malloc_should_clear
 1021  pv_settle
  976  free_tiny
  895  purvasm_rt::apply::Heap::apply
  786  tiny_malloc_should_clear
  769  Data.Traversable.traverseArrayImpl
  ...  free_tiny / tiny_malloc_* / tiny_free_* aggregate to about 6000+
  457  Data.Argonaut.Decode.Decoders.getField
```

The hot call graph reaches `Control.Monad.Free.toView` / `resume'` (the `Run` interpreter shape) and
CoreFn JSON decoding (`Data.Argonaut.Decode.Decoders.getField`, `Data.Traversable.traverseArrayImpl`).
Both are apply-dense workloads.

## Causal reading

There are two different allocation classes, and conflating them gives the wrong fix:

- **Host allocation**: Rust `Vec` / system heap allocation, visible as `szone_malloc` and `free_tiny`.
  This does not fill the guest semi-space. It is the apply-malloc problem.
- **Guest allocation**: closure, PAP, record, array, `CatList`, and similar objects in the moving
  semi-space. This drives GC and OOM for `Run`/`Free` workloads.

The first, directly observed runtime problem is in `runtime/src/apply.rs::Heap::apply`. On a common
exact-saturated closure call with positive arity, the current code copies the incoming argument slice
into an owned `Vec`, then copies the saturated prefix into another owned `Vec` before calling the
closure body:

```rust
pub fn apply(&mut self, f: Value, args: &[Value]) -> Value {
    let mut args: Vec<Value> = args.to_vec();
    ...
    let call_args: Vec<Value> = args[..arity].to_vec();
    ...
}
```

Zero-argument calls normally do not allocate through `Vec::to_vec()`, and `conts = Vec::new()` does
not allocate until a leftover group is pushed. The costly common case is therefore not literally
"every apply", but positive-arity exact-saturated closure apply. PAP, by-need, under-application, and
over-application paths allocate more, but the measured first target is the exact-saturated closure
case.

This explains why the JS path can remain quick while native falls off a cliff: V8 handles ordinary
function calls and short-lived allocations with a heavily optimised call stack and generational GC,
while the v1 native runtime currently turns many dynamic calls into system-heap `Vec` traffic before
the guest function body runs.

The conclusion still needs A/B confirmation. This note does not claim that apply-malloc is the only
factor: generic dispatch, the C ABI boundary, `pv_settle`, tail bouncing, and the sheer call density
of `Run`/`Free` remain possible residuals. It identifies the most strongly supported first lever.

## GC as an amplifier, not the first explanation

The v1 collector is a fixed-size, non-generational semi-space collector. Guest allocation from
`Run`/`Free` workloads can fill it quickly; the 8 MiB OOM is real. But the 1 GiB experiment still
showed the time cliff, and the profile points first at mutator-side `pv_apply` plus host
malloc/free.

Therefore GC should be instrumented before it is redesigned. The needed counters are collection
count, total GC time, copied/live words, and max live words. Without those numbers, claims about
multi-second GC pauses remain plausible but unproven.

## Why this is not a regression

- The v1 apply convention and the v1 collector were already part of the accepted runtime design.
- The runtime has no custom global allocator; on macOS it used the system allocator seen in the
  profile.
- The issue appears with both `--opt` and `--no-opt`, so it is not caused by Impurify/GER or by
  optimiser-only lowering.
- The native self-hosted compiler only recently reached the point where it could run this end-to-end
  workload, so an old v1 cost became newly measurable.

## Recommended response

The recommended order is:

1. **Add an allocation-free exact-saturated closure fast path.** At the `Heap::apply` entry, detect a
   live `Kind::Closure` with `args.len == arity`, call the code pointer with the original borrowed
   slice, return immediately if no pending tail was stashed, and fall back into the existing owned
   trampoline loop only if a pending tail must be driven. This preserves the current rooting contract:
   the callee still roots any inputs it holds across its own safepoints.
2. **Measure the fast path with paired A/B runs.** The original self-host emit workload did not finish
   under the native baseline, so a bounded `Run` benchmark is needed for the primary speed ratio; the
   full self-host workload can remain a completion/timeout gate.
3. **Use allocator A/B only as a diagnostic.** Swapping to mimalloc or jemalloc may confirm that host
   temporary allocation is causal, but it is not the durable fix if the `Vec`s can be removed.
4. **Add GC instrumentation.** Record collection count, total GC time, copied/live words, and max live
   words to decide whether a GC ADR is justified.
5. **Defer GC design until the counters justify it.** Generational GC, heap growth, and large-object
   handling may be needed later, but they should follow the instrumentation rather than precede it.

The compiler side may later reduce the number of dynamic applies through higher-order
specialization, PAP-CAF specialization, uncurrying, or dedicated interpreter shapes. That is a
separate optimiser track. The runtime first needs a cheap baseline for dynamic applies that cannot or
should not be specialised away.

## Key numbers

- Default heap: `defaultHeapWords = 1048576` words = 8 MiB per semi-space.
- L1 → native L2 binary production: about 1389 s total, with about 270 s CPU in the parent and about
  18 min in child `clang`.
- Native emit with a 1 GiB heap: about 88 s for 26 modules before the sampled window, with high
  per-module variance.
- JS emit for the same target: about 1.2 s for 106 modules.
