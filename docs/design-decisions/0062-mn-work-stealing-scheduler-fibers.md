# 0062. M:N work-stealing scheduler, fibers, preemption, and async I/O

- Status: Accepted
- Date: 2026-06-30

> **Revision (2026-06-30):** tightened after a multi-round two-reviewer pass (recorded in
> [sidenotes/0009](sidenotes/0009-runtime-cross-capability-review.md)). The "fresh fiber allocated
> nothing" framing is replaced by an explicit **fork capture-class rule**
> (Ref-free → promote/place-anywhere; `Ref`-bearing → affinity-bind); `AVar`/`Waker` waiters are **shared wake
> tokens, not local continuation pointers**; **blocked fibers are owned and GC-rooted by their
> Capability**; primitive `Ref` mutation is safepoint-free; cancellation is made linearizable with
> wakeups; and the deque is clarified to carry **spawn descriptors**, not arbitrary running fibers. A
> **second cross-review pass** added **cross-Capability `join` result promotion** (a fourth send trigger)
> and **`kill`-as-a-message** to the owning Capability. A **third pass** defined cross-Capability `join`
> of a non-promotable `Ref` result (partition error), made `Ref.modify f` non-atomicity an explicit
> documented tradeoff (with a critical-section escape hatch), and renamed fiber/Capability *affinity*
> (was "pin") to reserve "pinning" for memory. A **fourth pass** distinguished a blocking-handoff
> argument as **immutable (promotable to S1) vs a mutable/raw buffer (needs pinned/non-moving or C-owned
> storage)**.

## Context

[0061](0061-capability-local-shared-immutable-gc.md) fixed the **GC** (per-capability copying local heaps;
a shared region split into an immutable value heap **S1** and synchronised runtime objects **S2**; the
partition invariant; cycle-aware promotion; shadow-stack roots that **include scheduler-owned blocked
fibers**; the concurrent black-hole). [0060](0060-native-codegen-llvm-owned-runtime.md) §3 fixed the
**fiber mechanism** (a fiber is a heap continuation; synchronous `Effect` runs direct-style; native
stack switching is an optional add-on) and `musttail` TCE. This ADR fixes the **scheduler / execution
side**, and — per review — the **cross-capability edges** (`fork` capture, `AVar` waiters, cancellation)
where the partition is preserved or broken.

Target: [sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md)'s matrix — **M:N work-stealing**
(one Capability per core), **heap-continuation fibers**, **reduction-count cooperative preemption**, an
**io_uring/epoll driver + Waker**. PureScript's surface is `Aff` on fibers, `Ref` the synchronous
intra-fiber cell, `AVar` the asynchronous inter-fiber cell
([0035](0035-native-backend-ocaml5-concurrency.md)). Because the runtime is **owned**, this ADR closes the
two gaps [0035](0035-native-backend-ocaml5-concurrency.md) recorded for OCaml domains — *no auto-yield*
and *a blocking syscall freezes the carrier*.

## Decision

### 1. Threading: M:N work-stealing, one Capability per core; the deque carries spawn descriptors

- **N Capabilities** (≈ cores), each an OS thread running a scheduler loop. A Capability **owns a local
  heap** ([0061](0061-capability-local-shared-immutable-gc.md) §1).
- Because a *started* fiber is bound to its Capability (§2), the **work-stealing deque does not hold
  arbitrary running fibers** — it holds **movable spawn descriptors** (not-yet-started fork work, and
  Ref-free promotable fibers). "Work-stealing" here means stealing *spawn work*, not migrating a runnable
  continuation; readers should not import the GHC/Go "any lightweight thread migrates" model.
- **Blocking handoff** (closes [0035](0035-native-backend-ocaml5-concurrency.md)'s blocking gap): when a
  Capability must make an unavoidably-blocking foreign call, a spare OS thread takes over the Capability
  (its heap, run-queue, and scheduler tables) so its other fibers keep running. First cut: Capability
  bound 1:1 to an OS thread (a blocking call simply freezes that Capability); handoff is the refinement.
  - **Blocking-call GC contract (deferred but explicit):** a detached blocking foreign call holds its
    local-pointer arguments for an unbounded time while another carrier may run the Capability's local
    moving GC and relocate them. So the handoff refinement requires the arguments be GC-safe:
    **immutable arguments may be promoted to S1, but mutable/raw buffers (e.g. `read(fd, buf)`'s `buf`)
    cannot be promoted and need pinned/non-moving or C-owned storage** (or the Capability cannot local-GC
    while the call is outstanding). Unneeded for the bound 1:1 first cut where
    blocking freezes the Capability; required for handoff
    ([0061](0061-capability-local-shared-immutable-gc.md) §8 coupling 5).

### 2. Fibers are bound to their Capability; `fork` placement is a capture-class rule

A fiber is a heap continuation ([0060](0060-native-codegen-llvm-owned-runtime.md) §3) plus its status,
allocating in its Capability's local heap. A *started* fiber's working data lives in that local heap, so
moving it elsewhere would create cross-local pointers and break the partition
([0061](0061-capability-local-shared-immutable-gc.md) §1) — hence **a started fiber stays on its
Capability**. The earlier "a fresh fiber has allocated nothing" is *not* sufficient, because the fiber's
**closure and captured environment already exist in the parent's local heap**. Placement is therefore a
**capture-class rule**:

- **Ref-free immutable capture** — the captured graph is promoted to S1 by the cycle-aware copier
  ([0061](0061-capability-local-shared-immutable-gc.md) §5), and the fiber may be placed on **any**
  Capability's deque (this *is* a promotion, paying one copy).
- **Capture contains `Ref`/`STRef` (local mutable identity)** — the fiber is **affinity-bound to the
  owning Capability** (copying a `Ref`'s contents would change `Effect.Ref` semantics: a fork's writes must
  stay visible to the parent). Since a `Ref` never leaves its Capability
  ([0061](0061-capability-local-shared-immutable-gc.md) §5), all captured `Ref`s are co-located on **one**
  Capability, so the affinity target is unique. ("Affinity-bound" is a *scheduler* binding to a Capability;
  it is distinct from memory *pinning* — reserved for non-moving allocation, which the first cut does not
  use, [0061](0061-capability-local-shared-immutable-gc.md) §8.)
- **Capture contains a local by-need cell** — promotion follows
  [0061](0061-capability-local-shared-immutable-gc.md) §5's state rules (forced → promote result;
  suspended → shared by-need or force-first; under-evaluation → block/fail).
- **Capture contains an in-build / unsafe-linear-build object** — not promotable until publishable; the
  fork is affinity-bound until then.

The runtime may implement this by a dynamic promotion walk that detects non-promotable nodes, or by a
compile-time capture summary plus runtime checks; the ADR fixes the **semantic rule**, not the
optimisation. **Consequence:** parallelism that shares a `Ref` cannot be distributed — a real,
intentional load-balancing constraint.

### 3. `Aff` → scheduler lowering

A **suspending** `Aff` step (`await`, a blocking `AVar` take/put, an async I/O op) reifies its
continuation (a heap continuation) and **yields to the scheduler**, which re-queues the fiber when the
event fires. **Synchronous `Effect` segments between suspension points run direct-style**
([0060](0060-native-codegen-llvm-owned-runtime.md) §3) — no continuation is reified, keeping the common
path off the allocation floor ([sidenotes/0008](sidenotes/0008-self-compile-profiling.md)). `fork`,
`join`/`await`, and `kill` are §6.

### 4. Preemption: reduction-count cooperative yield (= the GC safepoint)

The codegen inserts a **reduction-counter** decrement at call back-edges / loop headers; at zero the fiber
**yields**. This closes [0035](0035-native-backend-ocaml5-concurrency.md)'s auto-yield gap (Erlang/BEAM
reduction budget; Cats-Effect-3 auto-yield). **These poll sites are exactly
[0061](0061-capability-local-shared-immutable-gc.md) §7's GC safepoints** — one mechanism for preemption
and a valid root map. A corollary the primitives below rely on: **a poll is never inserted *inside* a
primitive operation**, so primitive reads/writes are atomic with respect to scheduling on a Capability.

### 5. Async I/O: an io_uring/epoll driver, waking by token

A driver (io_uring on Linux; epoll/kqueue elsewhere) is integrated into the scheduler. A fiber issuing
I/O **submits and suspends** (its continuation parked in the Capability's blocked-fiber table, §7); on
completion the driver **wakes by a stable token** — it enqueues a **wakeup message to the owning
Capability** rather than dereferencing a local continuation from shared space (the same token rule as
`AVar`, §6, so no S→local pointer is ever formed). A Capability thus **never truly blocks on I/O** — the
complement to §1's handoff for blocking *foreign* calls.

### 6. Concurrency primitives

- **`Ref`** — a synchronous, **intra-Capability** mutable cell. Fibers on one Capability interleave only
  at yields (§4) and a poll is never placed inside a primitive `Ref` read/write (§4), so a `Ref` needs
  **no lock**. It lives in the local heap and **does not cross Capabilities**
  ([0061](0061-capability-local-shared-immutable-gc.md) §5). **`modify f` semantics (an intentional
  tradeoff):** the primitive read and write are each atomic w.r.t. scheduling (§4 places no poll inside a
  primitive), so `modify f` is atomic **iff `f` contains no yield point** — the common case (a non-looping
  `f` has no back-edge between the read and the write). If `f` loops/recurses and hits a reduction poll,
  two fibers on the Capability can interleave and lose an update — a **deliberate divergence** from JS,
  where a synchronous `Effect` action never interleaves with `Aff` (auto-yield for long computations, §4,
  is preferred over JS's atomic-synchronous-`Effect`). Code needing atomicity *over* a non-trivial `f`
  uses an explicit **critical-section / CAS** primitive (provided for this case).
- **`AVar`** — the **asynchronous, cross-Capability** cell and the runtime's *one* synchronising
  primitive; an **S2 shared runtime object** ([0061](0061-capability-local-shared-immutable-gc.md) §1)
  accessed under atomics/lock. A value put into it **crosses by cycle-aware promotion**
  ([0061](0061-capability-local-shared-immutable-gc.md) §5). It records blocked takers/putters **only as
  shared wake tokens `{ capability_id, fiber_id, epoch }`** — never a pointer to a blocked fiber's
  local continuation; the continuation stays rooted in its owning Capability's blocked-fiber table (§7),
  and a `put` **wakes by enqueuing a message** to that Capability.
- **`fork` / `Fiber` / `join` / `kill`** — fiber lifecycle. A **same-Capability `join`** hands the local
  result directly. A **cross-Capability `join`** cannot hand the joinee's result over as a pointer into the
  joinee's local heap, so the result is **promoted to S1**
  ([0061](0061-capability-local-shared-immutable-gc.md) §5 — a send trigger alongside `AVar` put and
  fork-capture). If that result **contains a non-promotable `Ref`/`STRef`**, the first cut **fails with a
  clear partition error** (use an `AVar` for cross-Capability mutable state); a future refinement may
  auto-promote the escaping `Ref` to an S2 cell ([0061](0061-capability-local-shared-immutable-gc.md) §5). **`kill` of a fiber on another
  Capability is a message to that owning Capability** (which marks and unwinds its own fiber at its next
  safepoint), never a direct mutation of a foreign Capability's local fiber object. **Cancellation is
  cooperative** for running code (unwinds at the next safepoint, running `bracket` finalizers). For a
  **blocked** fiber, kill must be **linearizable with wakeups**: exactly one of — the waiter token is
  removed before any wakeup observes it; or the wakeup wins and the resumed fiber observes cancellation and
  runs finalizers; or an **epoch/state CAS** resolves the race — with **no lost wakeup and no double
  resume**. (Detailed CAS protocol deferred; the ADR fixes that cancellation is not merely a poll on
  running code.)

### 7. Blocked-fiber ownership and roots

A parked fiber (blocked on `AVar`/timer/I/O/`join`, or awaiting cancellation cleanup) has **no active
native stack**; its continuation is a heap object owned and **GC-rooted by its Capability's scheduler
tables** ([0061](0061-capability-local-shared-immutable-gc.md) §7). Wakeups address the Capability by
token (§5/§6) and are delivered to its inbox (itself a local root). This is what lets the partition hold:
the only references to a blocked local continuation are local (the owning Capability's tables), never
shared.

### Coupling to [0061](0061-capability-local-shared-immutable-gc.md)

1. **safepoint = yield point** — §4 inserts the poll; [0061](0061-capability-local-shared-immutable-gc.md) §7 owns the root map.
2. **per-Capability heap** — §1's Capability owns [0061](0061-capability-local-shared-immutable-gc.md) §1's local heap.
3. **promotion on cross-Capability send** — §6's `AVar` put, §2's fork-capture, or a cross-Capability `join` result trigger [0061](0061-capability-local-shared-immutable-gc.md) §5.
4. **fork capture-class** — §2 decides promote (Ref-free) vs affinity-bind (Ref-bearing); the promote half is [0061](0061-capability-local-shared-immutable-gc.md) §5.
5. **blocking-handoff GC contract** — §1's handoff requires [0061](0061-capability-local-shared-immutable-gc.md) §8 pinning/promotion of blocking-call arguments.

### Deferred

- **Floating-Capability / blocking-handoff** mechanism and its FFI pin/promote contract (§1) — start
  bound 1:1.
- **Migration of started fibers** (§2) — start with fork-time placement only.
- **io_uring driver** shape (§5); **fairness/priority**, timer wheels, `STM`, and the detailed
  cancellation-masking / CAS protocols (§6).

### Validation note

The sequential differential oracle validates a **single fiber**'s `Effect` order and values; **parallel
scheduling is non-deterministic and outside it**. A separate approach is needed: a single-Capability
deterministic mode for the oracle, plus property tests on **`AVar` wait-queue linearizability** and
**blocked-fiber cancellation** (no lost wakeup / no double resume), and deterministic-replay for
interleavings. Flagged here; beyond the wall-2 codegen surface.

## Consequences

- Realises the [sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md) matrix natively and the
  owned runtime **closes both [0035](0035-native-backend-ocaml5-concurrency.md) gaps** — auto-yield (§4)
  and never-truly-blocking via async I/O (§5) + Capability handoff (§1).
- **One safepoint mechanism** serves preemption and GC roots; primitive mutations are safepoint-free (§4).
- **Trades**: fibers bind to their Capability and the deque carries only spawn descriptors, so a
  `Ref`-sharing parallel computation cannot be distributed (§2) — coarser balance than free stealing, the
  partition's price; reduction-count polls add per-back-edge cost; every cross-Capability hop pays `AVar`
  synchronisation + cycle-aware promotion.
- **Partition held at the edges:** `AVar`/`Waker` carry wake **tokens** not local pointers (§5/§6), and
  blocked fibers stay **owned and rooted by their Capability** (§7) — the two rules that keep S2 from
  forming a shared→local pointer.
- **Parallel scheduling escapes the sequential oracle**, so validation needs a concurrency-aware
  complement (above).
- With [0061](0061-capability-local-shared-immutable-gc.md), this completes the wall-2 runtime design on
  [0060](0060-native-codegen-llvm-owned-runtime.md)'s LLVM substrate and the owned runtime library
  (implementation language per [0060](0060-native-codegen-llvm-owned-runtime.md) §5).

## Alternatives considered

- **Free fiber stealing (GHC-style) over a shared heap.** Better balance, but needs a shared,
  non-partitioned heap with a parallel collector — contradicting
  [0061](0061-capability-local-shared-immutable-gc.md)'s partition. Rejected; balance at fork (§2).
- **1:1 threading (an OS thread per fiber).** No M:N machinery, but does not scale to many fibers — what
  lightweight fibers exist to avoid.
- **N:1 (one OS thread, cooperative only).** No parallelism — defeats the flagship.
- **Stackful fibers (a native stack per fiber) as the core.** Simpler blocking, but per-fiber stack memory
  and migration cost; [0060](0060-native-codegen-llvm-owned-runtime.md) §3 chose stackless heap
  continuations as the core, stack switching only as an add-on. Kept.
- **Callback/CPS async without fibers.** Composes poorly; `Aff` on fibers chosen.
- **Keep OCaml 5 domains + effect handlers.** Rejected in
  [0035](0035-native-backend-ocaml5-concurrency.md)/[0060](0060-native-codegen-llvm-owned-runtime.md):
  domains are 1:1 with OS threads (no floating Capabilities, no auto-yield) — the gaps this runtime closes.
