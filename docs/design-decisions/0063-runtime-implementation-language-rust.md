# 0063. Runtime implementation language: Rust — an unsafe GC island under a safe runtime shell

- Status: Accepted
- Date: 2026-07-01

## Context

[0060](0060-native-codegen-llvm-owned-runtime.md) §5 left the runtime's **implementation language** an
open direction (candidates C / Rust / Zig; criteria: direct syscall / `io_uring` access, clean C-ABI
interop, **manual memory control with no imposed managed runtime**, a light toolchain addition). The
runtime is the owned library — the allocator and GC ([0061](0061-capability-local-shared-immutable-gc.md)),
and the M:N scheduler / fibers / `io_uring` driver ([0062](0062-mn-work-stealing-scheduler-fibers.md)) —
that is linked into the LLVM-compiled program and called from the generated code across the C ABI.

The runtime splits into two parts with **opposite** language fit. The **GC core** is inherently `unsafe`:
a moving collector, raw pointers, tagged words, object relocation (which invalidates references), and
lock-free structures — exactly the operations a safe language's model forbids. The **scheduler / driver /
FFI shell** is concurrent and higher-level, and is where `purvasm`'s real correctness risk lives (the
cross-capability protocol that four review rounds hardened —
[sidenotes/0009](sidenotes/0009-runtime-cross-capability-review.md)), so it benefits most from
compiler-enforced safety. The language choice must serve both.

## Decision

**Implement the runtime in Rust** — chosen not because Rust makes the GC *safe* (it cannot; the GC core
is an unsafe island), but because Rust **makes the *non-GC* runtime hard to break (`Send`/`Sync`
data-race safety, ownership) and makes the unsafe GC easy to *isolate*** behind a small typed API. This
matches [0060](0060-native-codegen-llvm-owned-runtime.md) §5's criteria: Rust carries **no imposed managed
runtime** (no GC, no green threads since pre-1.0; its `std` allocator manages Rust's own heap, disjoint
from the guest heap the GC owns), C-ABI interop is `extern "C"`, and `io_uring`/syscalls are directly
available.

### 1. Two layers: unsafe GC island, safe runtime shell

- **Unsafe GC island** — a *small* module that owns *all* raw-pointer / relocation / tagged-word logic.
  Centered on low-level types **`Value` / `TaggedWord` / `HeapPtr`**, *not* a `Gc<T>` that derefs to
  `&T`. Field access goes through **explicit APIs** — `read_field(value, index)`, `write_field(…)`, etc.
  — never a native Rust reference into the heap.
- **Safe runtime shell** — the scheduler, work-stealing, fibers, `io_uring` driver, and FFI marshaling,
  written in idiomatic **safe** Rust, with shared state expressed through ownership types and `Send`/`Sync`.

### 2. The load-bearing rule: GC memory is never a Rust object

A native Rust reference (`&T` / `&mut T`) into a **moving** heap is unsound: relocation invalidates it,
and it fights Rust's aliasing / provenance model. So `purvasm`'s runtime **never treats GC memory as a
Rust object**. The `Value` / `TaggedWord` / `HeapPtr` types and the explicit `read_field` /
`write_field` APIs *are* the interface to the guest heap; they realize
[0059](0059-native-abi-value-representation.md)'s tagged-word representation and encode
[0061](0061-capability-local-shared-immutable-gc.md) §8's discipline (re-derive `base + offset` after a
safepoint; hold no raw interior pointer across one) **as function calls, not references**.

### 3. Standing Rust rules (fixed from the start)

- **No long-lived `&mut T` into the GC heap.**
- **No raw interior pointer held across a safepoint** ([0061](0061-capability-local-shared-immutable-gc.md) §8).
- **FFI boundary is `extern "C"` + `#[repr(C)]`; a panic never crosses the boundary** (caught / aborted at
  the edge).
- **GC-header mutable state goes through `UnsafeCell` / atomics, explicitly** (never plain `&mut` aliasing).
- **Scheduler shared state is expressed with ownership in safe Rust types** (not raw shared pointers).
- **Keep `unsafe` modules small; document the upheld invariant on each `unsafe` block.**

### 4. Testing: Miri, with a calibrated expectation

**Miri** is an asset for **small unit tests of the unsafe island** — the GC pointer API — and for
**catching unsafe-wrapper contract violations early** (UB, aliasing, provenance). It is **not** a tool for
checking `io_uring` or a whole real-OS-thread scheduler. Whole-runtime concurrency validation stays
separate — the [0062](0062-mn-work-stealing-scheduler-fibers.md) validation note (single-Capability
deterministic mode, `AVar`/cancellation linearizability property tests, deterministic replay) — and Miri
does not replace it.

**Provenance mode — "Miri clean" means *exposed/permissive*, not *strict*.** The tagged-word ABI
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §1) deliberately round-trips a heap
pointer through an integer (`TaggedWord`) and rebuilds it (`HeapPtr::from_word`: `usize → *mut Header`).
That is an integer-to-pointer cast, which is legitimate under the **exposed-provenance** model (each such
recovery angelically re-acquires a previously exposed allocation) but forbidden under **strict**
provenance. So the runtime is run under Miri's *default* (permissive/exposed) provenance and emits one
expected **warning** ("integer-to-pointer cast") — not an error. `-Zmiri-strict-provenance` is **not**
used: it would report the ABI's intentional int↔pointer tagging as false failures. What Miri does enforce
here — and what "clean" asserts — is the substantive UB: out-of-bounds, use-after-free, invalid
transmutes, and aliasing (Stacked Borrows). CI runs exactly this default-provenance `cargo miri test`.

## Consequences

- The **non-GC runtime** (the flagship risk surface) gets Rust's data-race safety and ownership; the
  **unsafe GC** is a *bounded*, Miri-tested island hidden behind a low-level typed API. This is the
  concrete realization of [0060](0060-native-codegen-llvm-owned-runtime.md) §5's owned runtime plus the
  "encapsulate the unsafe core" idea — and the reason the choice fits §5's criteria.
- **Costs, accepted and confined:** borrow-checker / provenance friction *inside* the unsafe island
  (Rust's unsafe rules are stricter than C's — the price of keeping the island small and typed); FFI
  ceremony (`extern "C"` / `#[repr(C)]`, panic containment); a `rustc` / `cargo` toolchain alongside LLVM.
- The `Value` / `TaggedWord` / `HeapPtr` API becomes the **single seam** between the safe shell and the
  guest heap — so [0061](0061-capability-local-shared-immutable-gc.md) §8's safepoint / interior-pointer
  discipline is enforced *by construction* (there is no `&T` into the heap to misuse), not by convention.
- `Send`/`Sync` catch data races in the **runtime's own** state (scheduler tables, work-stealing deques,
  wakeup queues), which is exactly the cross-capability machinery the reviews flagged — but **not** in the
  guest heap, whose invariants (the partition) remain hand-upheld inside the island.

> **Note (2026-07-01, for v2 — recorded once the v1 single-capability scope was set,
> [0064](0064-v1-single-capability-native-abi-codegen-contract.md)):** the single opaque `HeapPtr`
> maximally isolates the unsafe island, but it *forgoes* using Rust's type system to enforce the
> **partition**. Because `HeapPtr` cannot distinguish a local from a shared reference, the
> four-round-hardened invariants (e.g. an `AVar` holds no local pointer) stay hand-upheld even in the safe
> shell. When v2's shell implements the runtime partition logic (promotion, `AVar` put) in Rust, splitting
> the pointer into distinct **`#[repr(transparent)]` newtypes over `HeapPtr` — `LocalRef<T>` /
> `SharedRef<T>` / `WakeToken`** — would let the type system plus `Send`/`Sync` reject "store a local into
> S2" **at compile time**. These stay *typed handles*, not `&T` into the moving heap, so §2's rule holds.
> Scope: it covers the runtime's **own** partition code, **not** the codegen-emitted stores (which use the
> raw ABI, §2); and it is unneeded for v1 (no partition). Recorded as a v2 shell-design option.

## Alternatives considered

- **C.** Idiomatic for GC/VM work and minimal friction on the unsafe core, but **no safety net in the
  concurrent scheduler**, which is where `purvasm`'s real risk is — the data-race discipline would be
  entirely hand-rolled. Rejected: the shell's safety is worth more than the island's marginal C
  convenience.
- **Zig.** Simpler `unsafe` than Rust and excellent C interop (`liburing`, stack-map parsing), no
  borrow-checker friction in the GC core — but a **smaller ecosystem** (the work-stealing / lock-free
  structures Rust gets from `crossbeam` would be hand-rolled) and less maturity. A reasonable middle;
  not chosen because the scheduler-safety win is the deciding factor.
- **A `Gc<T>` handle that derefs to `&T`** (a "safe GC reference" abstraction). Rejected outright: a
  native Rust reference into a *moving* heap is unsound under relocation and fights aliasing / provenance.
  Low-level `Value` / `HeapPtr` + explicit field APIs are used instead (§2).
- **Rely on Miri for whole-runtime safety.** Rejected: Miri covers the small unsafe island, not
  `io_uring` or real-thread scheduling; whole-runtime concurrency validation is a separate discipline
  ([0062](0062-mn-work-stealing-scheduler-fibers.md) validation note).
- **Keep the OCaml/`ocamlopt` runtime.** Already rejected in
  [0035](0035-native-backend-ocaml5-concurrency.md) / [0060](0060-native-codegen-llvm-owned-runtime.md)
  (domains 1:1 with OS threads; the throwaway seed).
