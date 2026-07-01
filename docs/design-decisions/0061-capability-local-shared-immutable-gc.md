# 0061. Capability-local + shared-immutable garbage collector

- Status: Accepted
- Date: 2026-06-30

> **Revision (2026-06-30):** tightened the cross-capability boundary after a multi-round two-reviewer pass
> (recorded in [sidenotes/0009](sidenotes/0009-runtime-cross-capability-review.md)). The local hot path
> was well-specified; the revision pins down the
> partition's real edges — the shared region is split into an *immutable value heap* and *synchronised
> runtime objects* (which hold only shared values or wake tokens, never local pointers); promotion is
> cycle/sharing/by-need-state-aware; `Ref` cannot promote; scheduler-owned blocked-fiber tables are a
> first-class root category; the shared allocator/collector and their cross-capability root protocol are
> specified; and the "rare/off-hot-path" claim for shared memory is recalibrated. A **second cross-review
> pass** then **dropped the first-cut pin bit** (a semi-space collector has no non-moving region; builds
> are safepoint-free / base-reload and blocking freezes the Capability, so none is needed — a non-moving
> region is deferred with large-object handling), added **cross-capability `join` as a promotion
> trigger**, refined append-only to a *bounded-promotion* (single/low-Capability bootstrap) condition,
> made CAFs **force locally and promote only the result**, and elevated the shared side to the **central
> scalability subdesign**. A **third pass** closed the cross-capability `join` of a non-promotable `Ref`
> result (first-cut partition error; future `Ref`→S2), specified the first-cut shared mark-sweep's
> indirect-S1-edge root protocol (each Capability traces its live local graph and reports reachable S1
> edges), and renamed scheduler *affinity* away from memory *pinning*. A **fourth pass** tightened
> wording: a promoted suspended by-need cell puts its **immutable thunk/env in S1 and its eval/claim/result
> state in an S2 claim cell** (no mutable S1), and a blocking-handoff argument distinguishes
> **immutable (promotable) from mutable/raw buffers (need pinned/non-moving or C-owned)**.

## Context

[0059](0059-native-abi-value-representation.md) reserved the **GC seam** — an inline header with
GC-colour bits and a forwarding `kind`, a precise-roots interface, a write-barrier hook on every mutating
*pointer* store, an ownership bit with the shared/local *partition invariant*, and moving-GC safety
(pinning, no interior pointers across a safepoint). [0060](0060-native-codegen-llvm-owned-runtime.md)
chose **LLVM codegen + an owned runtime** and deferred the **GC algorithm** and the
**statepoint-vs-shadow-stack** mechanism to here. [sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md)
set the target: **capability-local heaps + a shared region**, to avoid the parallel-GC contention a
single global heap imposes — the flagship's whole point.

This ADR fixes the **GC**; the scheduler/fibers are the sibling ADR-0062. The two meet at the coupling
points below. The load-bearing difficulty, per review, is that the *cross-capability* edges
(promotion, shared allocation/collection, the runtime objects that hold blocked fibers) are where the
partition is preserved or broken — so they are specified here, not left implicit. **The shared side — its
allocator, the runtime objects that hold blocked fibers and waiters, `join`/`kill` delivery, the intern
table and CAF claim cells, and shared-heap collection — is the central scalability subdesign of a
message-passing multicore runtime, not a deferred afterthought behind the success of local copying GC.**

Constraints: strict PureScript allocates many short-lived immutable values
([0002](0002-cesk-execution-model.md)); the runtime is **owned** and must avoid a *global* stop-the-world
([0001](0001-phase-1-host-language-ocaml.md)); the value rep is [0059](0059-native-abi-value-representation.md)'s
tagged word (nullary ctors / `Boolean` / `Char` are `i31`, never traced); by-need cells
([0024](0024-by-need-recursive-bindings.md)) and the in-place byte builder
([0052](0052-native-unsafesetbyte-in-place.md)) interact with relocation; and
[0059](0059-native-abi-value-representation.md)'s knot-tying produces **cyclic immutable closures**.

## Decision

### 1. Two regions, three object classes, one precise partition

- **Per-capability local heap** — where a Capability's fibers allocate; collected **independently,
  without stopping other capabilities** (no *global* STW). The scalability property the design exists for.
- **Shared region**, split (per review) into two classes that the term "shared-immutable heap" previously conflated:
  - **(S1) shared immutable value heap** — promoted immutable guest graphs, top-level CAF constants,
    interned strings after publication. Genuinely immutable.
  - **(S2) shared synchronised runtime objects** — `AVar`, wait queues, the intern table, CAF claim
    cells, work-stealing/wakeup structures. These **mutate** under synchronisation, but **may contain
    only shared (S1) values or wake tokens — never a raw local-heap pointer** (§6, ADR-0062 §6).

**Partition invariant (precise):** an S1 object's pointers target only S1 objects or `i31` immediates;
an S2 object's mutable fields target only S1 objects or wake tokens; **no pointer ever crosses into any
capability-local heap, nor between two different capabilities' local heaps.** A local collection's roots
are therefore confined to its own capability (stack/registers, scheduler tables §7, remembered set §6);
it never traces another heap and never scans the shared region for liveness.

### 2. Local heap: a copying collector that treats shared objects as non-moving externals

A **copying (semi-space / Cheney)** collector: bump-pointer allocation (the hot path) and survivor-copy +
compaction, suiting the high-allocation, mostly-short-lived profile. It **moves** objects, so it needs
precise roots (§7) and the [0059](0059-native-abi-value-representation.md) forwarding word. When it
follows a local→shared pointer it reads the [0059](0059-native-abi-value-representation.md) ownership bit
and treats the shared target as a **non-moving external leaf**: it keeps the local object's reference
valid but does **not** relocate the shared object nor scan it for local liveness. A **generational** local
split is a later refinement; the first cut is single-space copying. (Cost: semi-space reserves **2× the
live local size per capability — `2 × capability_count ×`** of address space; noted in Consequences.)

### 3. The shared region in detail

S1 holds immutable values that outlive a single fiber/capability: published interned strings, CAF
constants, and promotion targets (§5). **Interned strings** are *built locally then published*
as immutable S1; the intern table itself is an S2 synchronised object. Because S1 is immutable, mutators
never race on its objects.

### 4. Shared collection: first-cut stance and the cross-capability root protocol

"No global STW" is true for *local* GC (§1); it is **not automatic for shared GC**, because local heaps,
fiber continuations, run queues, blocked-fiber tables, and machine root stacks all hold local→S1
pointers — so a shared collector must **enumerate S1 roots from every capability** (and, if it moves
objects, update those roots and the local fields pointing at moved S1 objects). A machine root often
points only at a *local* object that *indirectly* holds an S1 pointer, so root enumeration cannot just
scan the root stacks: **the first-cut global-safepoint mark-sweep has each capability trace its own live
local graph (from its precise roots, §7) and report every S1 pointer it reaches** as a shared root —
shared collection piggybacks a local trace on each capability. (A **local→S1 remembered set** is the
*concurrent-refinement* optimisation that avoids re-tracing each shared GC; it is **not** needed for the
first cut, which therefore has no liveness hole.) First-cut stance, scoped honestly:

- **Bootstrap (batch compiler — `purvasm-ps` runs and exits):** the shared region may be **append-only
  (uncollected)** *when cross-capability promotion is bounded*. The real safety condition is not merely
  "short-lived" (a short run with heavy promotion can still OOM) but that the bootstrap compiler runs at
  **single / low capability** with no parallel module compilation — so there is no `AVar`/fork/`join`
  promotion and S1 holds only CAF constants + interned strings, which are bounded. (If self-compilation is
  later parallelised for speed, append-only must be re-evaluated, since every cross-capability send then
  promotes into append-only S1.)
- **Before any long-running / parallel-server workload (a committed follow-up, *not* optional polish):** a
  **non-moving mark-sweep shared collector at an infrequent global safepoint** — local heaps stay
  independently collectable; only the rare shared sweep takes a global pause, with each capability
  contributing its S1 roots. A concurrent shared collector (local→shared remembered sets) is a later
  refinement to remove even that pause.

The exact shared collector beyond this stance is a deferred sub-decision; what this ADR fixes is the
partition (§1), the root protocol (above), and that **shared collection is required for the flagship, not
the bootstrap**.

### 5. Promotion on escape — cycle/sharing/by-need-state aware; `Ref` does not promote

A value becomes shared only by an **explicit send** — an `AVar` put, a value captured by a fork placed on
another capability (ADR-0062 §2), or a **cross-capability `join` result** handed from the joinee's
capability to the joiner's (ADR-0062 §6). Promotion copies the value's reachable immutable graph into S1,
and
**must be a forwarding-map-based, cycle- and sharing-preserving copy**, not a naïve deep copy:
[0059](0059-native-abi-value-representation.md)'s knot-tying yields **cyclic** immutable closures, so a
naïve copy would *not terminate* (a correctness bug, not a cost) and would blow up DAG sharing. By-need
cells encountered during promotion are handled by state:

- **forced** → promote the result;
- **suspended** → promote the **immutable thunk/env graph to S1** and represent its evaluation/claim/
  result state in an **S2 claim cell** (the §9 shared-CAF model — force locally, publish the result), so
  no mutable state lands in S1; or force before promoting;
- **local black-hole / under-evaluation** → block, or fail with a clear invariant (do not silently copy a
  half-built value);
- **cycle through by-need cells** → preserved by the forwarding map.

**`Ref`/`STRef` (mutable identity) does not promote** — copying its contents would change PureScript
`Effect.Ref` semantics (a fork's writes must remain visible to the parent). A `Ref` is therefore confined
to its capability; a fork capturing one is **affinity-bound** to that capability (ADR-0062 §2). By the
partition, a `Ref` never leaves its capability, so the values a fiber captures are co-located on a
**single** owning capability — the affinity target is unique and well-defined.

A value that must cross — an `AVar` put or a cross-capability `join` *result* (ADR-0062 §6) — that
**contains** a non-promotable `Ref`/`STRef` cannot be delivered as a bare local cell. First cut: the
promotion walk hits the `Ref` and **fails with a clear partition error** (cross-capability mutable state
must use an `AVar`, the one S2 synchronised primitive). A **future refinement** may instead **auto-promote
an escaping `Ref` to a shared S2 synchronised cell** (preserving JS-like sharing at a synchronisation
cost); deferred.

### 6. Write barriers and remembered sets

The [0059](0059-native-abi-value-representation.md) barrier on every mutating *pointer* store does two
jobs: **(a) enforce the partition** — the only stores that could create an S→local or cross-local pointer
are the *send* paths (§5) and an S2 mutation (`AVar` put), so the barrier routes those through promotion /
token-only updates and rejects a raw shared→local store; **(b) feed the remembered set** — once the local
heap is generational, an old→young pointer is recorded as an extra young-collection root. Note that an S1
object takes **no ordinary user stores** (it is immutable), so the barrier's promotion path fires
specifically at sends, not at arbitrary guest writes.

### 7. Precise roots: shadow-stack first; scheduler-owned fiber roots are first-class

A moving collector needs precise roots. Per [0060](0060-native-codegen-llvm-owned-runtime.md)'s
calibration (statepoint-driven *moving* GC is LLVM's least-exercised corner): **start with a shadow
stack** (the codegen maintains a side list of live roots; the collector reads and **updates** the slots on
relocation; it distinguishes tagged-word roots from raw unboxed `i32`/`f64`), and **migrate to LLVM
`gc.statepoint`** once proven.

Crucially, the running fiber's shadow stack is **not** the only root set. A **blocked fiber has no active
native stack** — its continuation is a heap object kept alive solely by a scheduler table. So a
capability's local roots **must also include its scheduler-owned fiber roots** (a first-class category):

- ready-queue entries for fibers bound to the capability;
- blocked fibers (waiting on `AVar`, a timer, I/O, `join`, or cancellation cleanup);
- completed-but-still-joinable fibers;
- pending wakeup messages addressed to the capability;
- the running fiber's shadow stack.

ADR-0062 §7 names which structure owns a parked fiber.

### 8. Moving-GC safety (consuming [0059](0059-native-abi-value-representation.md))

The in-place byte builder ([0052](0052-native-unsafesetbyte-in-place.md)) allocates its buffer once then
writes only bytes, so its inner loop is **safepoint-free** (no allocation, no yield) and the codegen
places no GC poll inside it — the buffer cannot move mid-build; if the build grows (a `realloc`, which is
an allocation and therefore a safepoint) it simply **re-loads the base** afterward. **No raw interior
pointer is held across a safepoint** (`EnvField`, record/array elements, ADT fields are re-loaded from the
possibly-relocated base). **The first cut therefore needs no pinning** — and a semi-space collector (§2)
has no non-moving region to pin into anyway, so there is **no pin bit** (§10). The one place a non-moving
mechanism is genuinely required is the blocking-handoff path's *mutable* I/O buffer (`read(fd, buf)`'s
`buf` cannot be promoted to immutable S1), which is **deferred with large-object handling** (ADR-0062 §1).

### 9. Concurrent black-hole for shared-CAF forcing, with a disposal rule

A shared CAF that is by-need ([0024](0024-by-need-recursive-bindings.md)) lives in S1 and may be forced by
several capabilities at once (the multicore hazard [0036](0036-anf-to-ocaml-value-representation.md)
flagged for single-domain OCaml `lazy`). A **lock-free claim**: the first forcer CASes the CAF's S2 claim
cell to *under-evaluation*, **forces it in its own local heap, and promotes only the result to S1** — so
the evaluation's intermediate garbage stays local and collectable, and S1 grows by *results*, not by
evaluation cost. A later forcer either blocks for the result or — since CAFs are **pure** — is permitted
to **duplicate** the evaluation. **Disposal rule:** the CAS loser likewise forces locally and would
promote a *duplicate result* to S1; under append-only shared (§4) that result is permanent garbage, so
**large or unknown-size CAFs prefer blocking**, while small pure CAFs may duplicate and the duplicate S1
result is accepted in the cost model (reclaimed once shared collection lands).

### 10. Shared allocation, local allocation, and the header

Local allocation is bump-pointer in the capability-local heap; overflow triggers a local collection.
**Shared allocation** (promotion §5, CAF forcing §9, interning §3, `AVar` traffic) needs its own strategy
— the first cut: **per-capability shared allocation buffers** (each capability bump-allocates into a
slab it owns, refilled under a lock) with **segregated arenas for S1 immutable values vs S2 runtime
objects**; a global lock or lock-free bump are alternatives. The header is
[0059](0059-native-abi-value-representation.md)'s `{ size, GC colour, kind }` plus the ownership bit and a
forwarding word — **no pin bit in the first cut** (§8; a semi-space collector has nowhere to pin, and the
first cut needs no pinning).

### Coupling to ADR-0062 (the scheduler)

1. **safepoint = yield point** — this ADR owns the root map there; ADR-0062 §4 inserts the poll.
2. **per-capability heap = per-scheduler-Capability** — §1's local heap is owned by ADR-0062's Capability.
3. **promotion fires on cross-capability send** — §5 is triggered by ADR-0062's `AVar` put, fork-capture,
   or a cross-capability `join` result.
4. **fork-capture promotion + `Ref` placement** — a fork's capture class (ADR-0062 §2) decides promote
   (Ref-free) vs affinity-bind (Ref-bearing); §5 is the promote half.
5. **blocking-handoff GC contract** — before a handoff, a blocking foreign call's local-pointer arguments
   must be made GC-safe: **immutable arguments may be promoted to S1, but mutable/raw buffers (e.g.
   `read(fd, buf)`'s `buf`) cannot be promoted and require pinned/non-moving or C-owned storage**
   (ADR-0062 §1) — otherwise they can move under the new carrier's local GC.

### Deferred

- The **shared collector algorithm** beyond §4's stance; **generational** local heap; **`gc.statepoint`**
  migration; the detailed **promotion mechanism** and **shared allocator synchronisation**; a **non-moving
  pinned / large-object region** (needed only by large objects and the blocking-handoff mutable-I/O-buffer
  path, ADR-0062 §1 — bundled here, since a semi-space first cut has none and needs none); finalizers and
  weak references.

## Consequences

- **No *global* STW for local GC** — each capability collects independently; the scalable parallel GC the
  flagship needs. Shared GC takes an infrequent global safepoint (first cut, §4), required before
  long-running workloads.
- `i31` immediates and shared CAF constants keep the traced heap small; bump allocation + copying make the
  allocate-and-die path cheap. **But** semi-space costs **2× × capability_count** address-space reservation
  (§2).
- The partition's price is **on the cross-capability path, by design**: every send pays a cycle-aware
  promotion copy (§5); shared allocation is contention-bounded but **proportional to cross-capability
  communication and CAF forcing**, *not* off the hot path in a message-passing program (§10);
  shared-CAF forcing pays atomics and possibly duplicate S1 garbage (§9).
- It realises [0059](0059-native-abi-value-representation.md)'s seam concretely and settles
  [0060](0060-native-codegen-llvm-owned-runtime.md)'s deferred root pick (shadow stack first), with
  scheduler tables as first-class roots (§7).

## Alternatives considered

- **A single global heap with a parallel/concurrent collector** (OCaml 5, JVM G1/ZGC). No promotion or
  copy-on-send, but global GC coordination and cross-core contention at scale — the thing
  [sidenotes/0006](sidenotes/0006-concurrent-runtime-case-study.md) chose capability-local heaps to avoid,
  and the OCaml model [0035](0035-native-backend-ocaml5-concurrency.md)/[0060](0060-native-codegen-llvm-owned-runtime.md)
  rejected for the owned runtime.
- **Reference counting for the whole heap** (Swift). No tracing pauses, but pervasive atomic retain/release
  (contention) and a cycle collector; poor fit for the high-allocation local heap. Retained as a
  *candidate for the S1 heap only* (§4).
- **A non-moving local heap** (mark-region / Immix). Lets the stack be scanned conservatively (no precise
  roots for relocation), but loses bump allocation + compaction (fragmentation), and precise roots are
  needed anyway for the partition/remembered set and scheduler roots. Copying chosen; revisit if precise
  roots prove too costly.
- **`gc.statepoint` from the start.** Deferred per [0060](0060-native-codegen-llvm-owned-runtime.md)'s
  calibration; shadow stack de-risks bring-up.
- **No promotion — allow shared mutable across capabilities.** Requires a globally synchronised heap with
  locks/barriers on shared mutation — the contention the design avoids. Rejected for copy-on-send + `AVar`
  (S2) as the one synchronising primitive.
