# Design review: hardening the cross-capability boundary of ADR-0061 / ADR-0062

- Date: 2026-06-30
- Status: review record (fair copy). Backs [ADR-0061](../0061-capability-local-shared-immutable-gc.md)
  and [ADR-0062](../0062-mn-work-stealing-scheduler-fibers.md), both **Proposed**.
- Scope: the multi-round, two-reviewer design review of the wall-2 runtime ADRs (GC + scheduler), and the
  resolutions folded back into them. (Working notes were drafted bilingually off-tree; this is the
  English record of what was decided and why.)
- Related: [ADR-0059](../0059-native-abi-value-representation.md) (ABI seam),
  [ADR-0060](../0060-native-codegen-llvm-owned-runtime.md) (LLVM + owned runtime),
  [sidenotes/0006](0006-concurrent-runtime-case-study.md) (the concurrency-model survey).

## The converging conclusion

ADR-0061 (capability-local + shared-immutable GC) and ADR-0062 (M:N work-stealing scheduler) were drafted
together and reviewed by two independent reviewers over several rounds. Both converged, independently, on
one conclusion: **the local hot path was well-motivated, but the *cross-capability* boundary — `fork`
placement, promotion, `AVar`, `join`, shared allocation, shared collection, the runtime objects that hold
blocked fibers — is not a set of peripheral details; it is where the partition invariant is preserved or
silently broken.** Local copying GC is the easy part; the shared side is the flagship scalability risk and
had to become a central subdesign rather than an afterthought. All the changes below are facets of that.

## Resolutions folded into the ADRs

1. **The shared region is two classes, not one "immutable heap".** **S1** is a genuinely immutable value
   heap (promoted graphs, CAF constants, published interned strings). **S2** holds *synchronised mutable*
   runtime objects (`AVar`, wait queues, the intern table, CAF claim cells, work-stealing/wakeup
   structures). Precise partition: an S1 object points only to S1/`i31`; an S2 object may mutate but holds
   only S1 values or **wake tokens** — never a raw local-heap pointer; no pointer crosses into any local
   heap or between two local heaps. (Resolves the "shared heap is immutable" vs "`AVar` is a shared mutable
   cell" contradiction.)

2. **`fork` placement is a capture-class rule, not a freshness rule.** A forked fiber's closure and
   captured environment already exist in the parent's local heap, so a steal *promotes* them. Ref-free
   immutable captures are promoted to S1 and may be placed on any Capability; a capture carrying
   `Ref`/`STRef` is **affinity-bound** to the owning Capability (copying a `Ref`'s contents would change
   `Effect.Ref` semantics). A `Ref` never leaves its Capability, so the affinity target is unique. The
   deque therefore carries **movable spawn descriptors**, not arbitrary running fibers — "work-stealing"
   here is spawn-work stealing.

3. **Cross-capability transfer of mutable identity is a partition error in the first cut.** An `AVar` put
   or a cross-Capability `join` result that *contains* a non-promotable `Ref`/`STRef` cannot be delivered
   as a bare local cell; the first cut fails with a clear error (cross-Capability mutable state uses an
   `AVar`). A future refinement may auto-promote an escaping `Ref` to an S2 synchronised cell.

4. **`AVar`/`Waker` carry wake tokens, not local continuation pointers.** A blocked fiber's continuation
   stays rooted in its owning Capability's blocked-fiber table; an `AVar`/the I/O driver records only a
   shared token `{ capability_id, fiber_id, epoch }` and wakes by enqueuing a message to that Capability.
   This is what keeps S2 from ever forming a shared→local pointer.

5. **Scheduler tables are a first-class GC root category.** A blocked fiber has no active native stack;
   its heap continuation is kept alive only by a scheduler table. A Capability's local roots therefore
   include its ready queue, blocked-fiber table, joinable completed fibers, pending wakeup messages, and
   the running fiber's shadow stack — in addition to stack/register roots.

6. **Promotion is a cycle/sharing/by-need-state-aware copy.** Knot-tying (ADR-0059) yields cyclic
   immutable closures, so a naïve deep copy would not terminate (a correctness bug). Promotion uses a
   forwarding map. A suspended by-need cell promotes its **immutable thunk/env to S1** and represents its
   evaluation/claim/result state in an **S2 claim cell** (the same shape as shared-CAF forcing) — no
   mutable state lands in S1.

7. **Shared allocation and collection are specified.** Shared writes (promotion, CAF forcing, interning,
   `AVar`) are a bounded-but-nonzero contention point proportional to cross-capability communication, not
   "off the hot path"; the first cut uses per-capability shared buffers with segregated S1/S2 arenas. For
   collection, "no global STW" holds for *local* GC only; the first-cut shared collector is a
   global-safepoint mark-sweep in which **each Capability traces its own live local graph and reports
   every reachable S1 pointer** (a machine root often points only at a local object that indirectly holds
   an S1 pointer, so scanning root stacks alone would miss edges). A local→S1 remembered set is the
   *concurrent* refinement, not a first-cut requirement. Append-only shared is acceptable only for the
   bootstrap compiler running at single/low Capability (bounded promotion); real collection is required
   before long-running workloads.

8. **The first cut needs no memory pinning, and "pin" is reserved for memory.** A semi-space collector has
   no non-moving region, and the first cut does not need one: the in-place byte builder is safepoint-free
   (or re-loads its base after a grow), and a blocking foreign call freezes the Capability (no concurrent
   GC). A non-moving region is deferred with large-object handling and the blocking-handoff path. There,
   **immutable arguments may be promoted to S1, but mutable/raw buffers (e.g. `read(fd, buf)`'s `buf`)
   cannot be promoted and need non-moving or C-owned storage.** Scheduler *affinity* (fiber↔Capability,
   Capability↔OS-thread) is renamed "affinity-bound / bound" so "pinning" means only non-moving memory.

9. **Scheduling-semantics rules.** Reduction-count preemption poll sites *are* the GC safepoints (one
   mechanism). A poll is never placed inside a primitive operation, so a primitive `Ref` read/write is
   atomic w.r.t. scheduling — but `Ref.modify f` is atomic only when `f` has no yield point, an
   intentional, documented divergence from JS's atomic synchronous `Effect` (with an explicit
   critical-section / CAS primitive for the rare looping-`f` case). `kill` of a fiber on another Capability
   is a message to its owning Capability; cancellation of a *blocked* fiber must be linearizable with
   wakeups (no lost wakeup, no double resume).

## Outcome

The two independent reviews converged to a single accept-blocking set, and every item was either folded in
or explicitly deferred with a narrower first-cut semantics. The reviewers assessed the ADRs as
accept-ready in this state. Detailed sub-designs remain deferred and are marked in each ADR: the shared
collector algorithm, the `AVar`/cancellation CAS protocols, the runtime implementation language
(ADR-0060 §5), the `gc.statepoint` migration, and the non-moving pinned/large-object region.
