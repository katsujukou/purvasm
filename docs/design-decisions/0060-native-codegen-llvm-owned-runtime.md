# 0060. Native codegen via LLVM, with an owned runtime (GC, scheduler, fibers)

- Status: ~~Proposed~~ **Accepted** _(2026-06-30: accepted by the maintainer, with the review refinements folded in)_
- Date: 2026-06-30

## Context

[0059](0059-native-abi-value-representation.md) settled wall 1 — the target-neutral native ABI (value
representation, calling convention, and the GC *seam*). This ADR settles the next gate of **wall 2**: the
**codegen substrate** and the **codegen↔runtime interface**. The runtime's *policy* (the concrete GC
algorithm, the scheduler) is left to a further runtime ADR; this record fixes how native code is produced
and how it talks to the runtime it runs on.

`boot`'s current native path ([0035](0035-native-backend-ocaml5-concurrency.md),
[0036](0036-anf-to-ocaml-value-representation.md)) lowers CoreFn → link → ANF → OCaml source → `ocamlopt`,
leaning *entirely* on OCaml's runtime (GC, domains, effect handlers). That is the throwaway shortcut
([0001](0001-phase-1-host-language-ocaml.md), [0037](0037-self-hosting-purescript.md)). OCaml 5 is a
faithful concurrency *reference*, but its domains are bound 1:1 to OS threads — no floating capabilities,
and a blocking syscall freezes the whole domain ([0035](0035-native-backend-ocaml5-concurrency.md)'s
honest gaps) — so it cannot host the flagship parallel runtime
([sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md): M:N work-stealing, capability-local
+ shared-immutable heaps, fibers via continuations, reduction-count cooperative preemption, io_uring/epoll
Waker). The native backend therefore needs a substrate other than `ocamlopt`.

Two further facts frame the choice. The native floor is *representation overhead*, not algorithmic work
([sidenotes/0008](sidenotes/0008-self-compile-profiling.md)), so the substrate must produce efficient
direct-style code, a strong optimiser, and the unboxing [0059](0059-native-abi-value-representation.md)
enables. And [0059](0059-native-abi-value-representation.md) deliberately reserved the GC roots/stack-map
seam, the write-barrier hook, and the C-FFI boundary for precisely this runtime, and deferred the
tail-call mechanism to here.

## Decision

### 1. Codegen substrate = LLVM; runtime = owned (the hybrid)

"Target" decomposes into **two orthogonal choices**: the **codegen substrate** (instruction selection,
multi-platform, optimisation) and the **runtime** (GC, scheduler, fibers). Choose **LLVM** for the
substrate and keep the runtime **owned** ([0001](0001-phase-1-host-language-ocaml.md)). LLVM imposes no
runtime or collector; the owned runtime is a linked library that LLVM-compiled code calls. This is the
canonical design for a managed-language native backend — Swift, GHC's own LLVM backend, Julia,
Kotlin/Native, and Pony all pair LLVM codegen with a bespoke GC/runtime.

Why LLVM over the alternatives, given the runtime is owned either way:

- **Free multi-platform** (x86-64 / ARM64 / RISC-V / …) from one IR.
- **Guaranteed tail calls** (`musttail`) for the eval/apply TCE the machine assumes
  ([0030](0030-bytecode-vm-slice1.md)) — this resolves the tail-call mechanism
  [0059](0059-native-abi-value-representation.md) deferred.
- **Precise-GC-roots infrastructure** (`gc.statepoint`, stack maps) for a relocating collector.
- A **mature optimiser** that attacks the representation floor ([sidenotes/0008](sidenotes/0008-self-compile-profiling.md)).

C lacks a portable guaranteed tail call and native stack maps; a custom backend reimplements instruction
selection, register allocation, multi-platform, and optimisation (see *Alternatives*).

### 2. The codegen↔runtime interface

- **Allocation** — codegen emits calls to the runtime allocator (`gc_alloc(size, kind)`); header per
  [0059](0059-native-abi-value-representation.md) §2.
- **GC roots** — a precise relocating collector via LLVM **`gc.statepoint` / `gc.relocate`** with stack
  maps (the `.llvm_stackmaps` section the runtime parses), and **managed pointers in address space 1**
  (distinct from raw C-FFI pointers in addrspace 0). This is the *implementation* of
  [0059](0059-native-abi-value-representation.md)'s reserved roots/stack-map seam. A **shadow stack** is
  the fallback if statepoint integration proves too rough (simpler, at a per-call cost). The
  **statepoint-vs-shadow-stack** pick is the main engineering locus and is left to the runtime ADR.
- **Write barriers** — emitted at every mutating *pointer* store
  ([0059](0059-native-abi-value-representation.md) §4 scopes this to pointer stores — a non-pointer byte
  write needs pinning, not a barrier), inlined as IR for speed.
- **Tail calls** — `musttail`, multi-platform. `musttail` requires a matching signature/calling
  convention, so TCE applies on **direct→direct** edges (matching reps); a tail edge from the boxed
  generic entry into an unboxed direct entry cannot be `musttail` (signatures differ — consistent with
  [0059](0059-native-abi-value-representation.md) §3 confining unboxing to the direct entry), and falls
  back to an ordinary call.
- **Runtime services** — the scheduler, fiber operations, the io_uring/epoll driver, and `AVar` are
  reached across [0059](0059-native-abi-value-representation.md) §5's C-FFI boundary.

### 3. Fiber mechanism — heap continuations (core) + stack switching (add-on)

- **Core: heap continuation objects** (CPS / Cats-Effect-3 style) for the *suspending* `Aff` layer. This
  is target-agnostic and is `purvasm`'s native idiom: the machine already reifies the continuation `K` as
  a first-class heap value ([0002](0002-cesk-execution-model.md)), and `Effect` is a thunk
  ([0023](0023-effect-runtime-oracle.md)); a fiber is a saved continuation in the
  [0059](0059-native-abi-value-representation.md) GC representation, trivially stealable for
  work-stealing. No special LLVM support is needed.
- **Synchronous `Effect` stays direct-style.** Reifying a heap continuation at *every* `Effect` bind would
  rebuild exactly the allocation floor [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) attacks.
  Since `Effect` is a thunk ([0023](0023-effect-runtime-oracle.md)), synchronous effects run by direct
  thunk-force (native calls); **continuation reification is confined to the actual `Aff` suspension points**
  — `fork` / `await` / `AVar` / the async-I/O boundary — where the scheduler genuinely needs to save and
  reschedule.
- **Add-on: native stack switching** (ucontext / an assembly context switch in the runtime library), only
  where *pure direct-style* native code must block or be preempted mid-call. Optional and orthogonal to
  LLVM.

### 4. Lowering source and granularity

- **Per-module** native codegen — CoreFn module → CESK AST → ANF → LLVM IR module → object file, linked
  with the runtime — consistent with separate compilation ([0033](0033-separate-compilation.md)) and
  LLVM's per-module-`.o` + linker model. (`boot`'s whole-program `native_action` is throwaway.)
- **Lowering source = ANF**, mirroring `codegen_ml` (ANF → OCaml) and reusing the optimiser output
  directly — low friction, no SSA recovery. The cross-module boundary is boxed per
  [0059](0059-native-abi-value-representation.md) §3.
- The **`.pmo`-as-universal-object** direction — lowering native (and a possible wasm backend) *from*
  `.pmo` via [0003](0003-stack-based-bytecode.md)'s "lift bytecode to SSA by abstract stack simulation",
  growing `.pmo` into a PureScript alternative-backend platform — is **noted as a future direction, not
  this ADR's path**. The first native codegen lowers from ANF.

### 5. Runtime implementation language (direction)

The owned runtime — allocator, GC, scheduler, fiber primitives, io_uring/epoll driver — is a separately
built library that LLVM-compiled code links against; **what it is written in** is a substrate-level
decision this ADR must direct (the GC *algorithm* goes to the runtime ADR, but the implementation
substrate is fixed here). Criteria: direct syscall / `io_uring` access; clean LLVM and C-ABI interop
(it sits behind [0059](0059-native-abi-value-representation.md) §5's C-FFI boundary); **manual memory
control** (it *is* the allocator/GC, so it must not carry an imposed managed runtime of its own); and a
light addition to the bootstrap toolchain. Candidates:

- **C** — lowest friction, matches LLVM and `io_uring`/syscalls directly, no imposed runtime; but no
  memory-safety help for the scheduler/driver (where concurrency bugs are easy and costly).
- **Rust** — memory- and concurrency-safety for the non-GC machinery, a strong async/work-stealing
  ecosystem, `#![no_std]`-capable to avoid an imposed runtime; at the cost of a heavier toolchain and
  FFI ceremony.
- **Zig** — no hidden runtime, excellent C interop, `comptime`; a simpler middle ground than Rust,
  with a smaller ecosystem.

Lean: a memory-safe systems language (**Rust** or **Zig**) for the scheduler / driver where data races
are the real hazard, with **C** as the low-friction fallback; the final pick is made with the runtime
ADR, since the GC algorithm and the language interact. *Not* in scope: writing the GC/scheduler in
PureScript-on-`purvasm` — a bootstrap circularity for a later phase, if ever.

### Deferred (to the runtime ADR(s))

- The **GC algorithm** — capability-local copying + shared-immutable, and the **concurrent black-hole**
  that shared-CAF lazy forcing across capabilities needs (the multicore hazard
  [0036](0036-anf-to-ocaml-value-representation.md) flagged for OCaml `lazy`). Survey:
  [sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md).
- The **scheduler** (M:N work-stealing), **reduction-count preemption** details, the **io_uring/epoll
  Waker** driver, **`AVar`/fork/await** semantics, and the **`Aff` → continuation lowering**.
- The final **statepoint vs shadow-stack** decision (evaluated under the chosen GC).
- LLVM integration mechanics (C++ API vs emitted textual IR vs a binding) — an implementation detail.
- **Debug info (DWARF) and a symbolised profiling path** for LLVM-native — the
  [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) `sample`/per-phase methodology that drove the
  ABI design must carry over, and is a prerequisite for the wall-4 benchmark.
- **Cross-module rep publication** and **unboxed data fields** — gated as in
  [0059](0059-native-abi-value-representation.md).

## Consequences

- Free multi-platform native code, a strong optimiser against the
  [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) floor, and `musttail` TCE — while the runtime
  stays **owned** ([0001](0001-phase-1-host-language-ocaml.md)) and the ABI
  ([0059](0059-native-abi-value-representation.md)) slots in (statepoints are the *intended* realisation
  of its roots seam, with shadow stack a live alternative; addrspace 1 realises its managed/raw pointer
  split).
- **Risk calibration on the GC-roots path.** The precedents (Swift, GHC's LLVM backend, Julia,
  Kotlin/Native, Pony) strongly support the *hybrid* (LLVM codegen + a bespoke GC/runtime) but **not**
  statepoint-driven *moving* GC specifically: Swift is ARC (no relocation), GHC carries roots in its
  STG calling convention, and Julia/Kotlin/Pony each use bespoke rooting. So `gc.statepoint` is the
  documented LLVM mechanism but is under-exercised for relocating collectors, and there is a real chance
  the **shadow stack becomes the primary root strategy, not the fallback** — the engineering should be
  budgeted for that outcome, not assume statepoints land cleanly.
- The engineering concentrates in two places: the **LLVM GC-roots integration** (statepoints are LLVM's
  least-polished corner — see the calibration above) and **building the whole runtime** (GC, scheduler,
  fibers, io_uring) — LLVM gives codegen, *not* a runtime.
- A **heavy, version-churning LLVM dependency** enters the toolchain, and the bootstrap (`boot` builds
  Level-2 native) now targets LLVM rather than `ocamlopt`. Note that the *compiler's own execution
  runtime* and the *runtime its emitted code targets* are separable: a `boot`/`ocamlopt`-built Level-3
  compiler can already *emit* owned-ABI native code, so the cut-over to the owned runtime is a staged
  wall-3 concern (which artifact runs on which runtime), not a prerequisite of this codegen decision.
- The **differential-oracle discipline extends**: LLVM-native is another implementation held to value +
  `Effect`-order equality against the CESK oracle and the VM (sequentially). **Parallel scheduling needs a
  separate validation story**, deferred with the runtime.
- `.pmo` stays the bytecode-VM / separate-compilation artifact; the **universal-object vision is preserved
  as a future direction**, not foreclosed.

## Alternatives considered

- **C as the codegen substrate.** Portable and quick to stand up, with easy stack switching and io_uring
  FFI — but **no portable guaranteed tail call** (`musttail` is non-standard) and **no native stack maps**,
  so a precise relocating GC is forced onto a shadow stack (overhead) or conservative scanning
  (incompatible with moving). Multi-platform is per-arch toolchain work, not free. Rejected: LLVM gives
  `musttail`, statepoints, and multi-platform under the same owned-runtime model.
- **Custom native codegen.** Maximal control and the true endgame — emit stack maps, context switches, and
  tail calls exactly as designed — but it reimplements instruction selection, register allocation,
  multi-platform support, and optimisation, for no near-term benefit over LLVM. A plausible *later* move
  once the runtime is proven; not the wall-2 starting point.
- **Keep OCaml / `ocamlopt`.** A faithful concurrency reference, but domains are 1:1 with OS threads (no
  floating capabilities; blocking-syscall freeze — [0035](0035-native-backend-ocaml5-concurrency.md)), so
  it cannot host the flagship runtime, and it is the throwaway `boot` substrate by design
  ([0001](0001-phase-1-host-language-ocaml.md), [0037](0037-self-hosting-purescript.md)).
- **WASM(-GC) as the primary native target.** The engine GC is incompatible with the owned
  capability-local runtime that is wall 2's goal; retained only as a possible *secondary portability
  backend* (it would carry its own engine ABI, not [0059](0059-native-abi-value-representation.md)'s), not
  the primary.
- **Lower native from `.pmo` now (the universal object).** The appealing long-term platform vision, but it
  requires [0003](0003-stack-based-bytecode.md)'s bytecode→SSA recovery up front; ANF lowering is
  lower-friction and already proven by `codegen_ml`. Deferred, not foreclosed.
