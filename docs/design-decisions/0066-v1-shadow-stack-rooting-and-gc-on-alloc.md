# 0066. v1 precise rooting: a shadow-stack root API and GC-on-allocation

- Status: ~~Proposed~~ **Accepted** _(2026-07-01: accepted by the maintainer)_
- Date: 2026-07-01

## Abstract

v1 precise rooting: a `Heap`-owned shadow-stack (`Vec<Value>`) root API with explicit reload-after-safepoint, per-function rooting (self-rooting constructors; `apply` roots over-apply leftovers; `CodeFn`s root their own live values), and `alloc` collects-and-retries (fixed heap, OOM on still-full); the pre-codegen realisation of ADR-0064 §4

## Context

The v1 runtime ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)) has a working
tagged-word value representation, header/kind layout, the eval/`apply` calling convention, and a
Cheney copying collector — but **the collector is never triggered automatically**. `Heap::alloc`
*panics* on semi-space overflow, and `Heap::collect` must be handed its roots explicitly by the
caller. The whole thing is Miri-clean ([0063](0063-runtime-implementation-language-rust.md) §4)
only because no collection can occur mid-`apply`: the in-flight argument `Value`s (and any
intermediate a `CodeFn` holds across an allocation) are **not rooted**, so a relocation would leave
them dangling.

To actually run allocation-heavy programs, `alloc` must **collect on overflow and retry**. The
moment it can, every live `Value` spanning an allocation site becomes a root the collector must find
and update — the [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 "root reload
after a safepoint" contract.

§4 pins that contract for the **codegen** future ("the codegen maintains the shadow stack
(push/pop live roots); the GC reads and updates the slots"). But there is **no LLVM codegen yet**:
v1 is a Rust-interpreter-style bring-up whose `apply` loop and (currently hand-written) `CodeFn`
bodies hold `Value`s in Rust locals. Those locals are invisible to the collector. So v1 needs a
concrete, pre-codegen realisation of the same contract: *a rooting API the hand-written runtime uses,
which the eventual codegen will target in the same shape.* That API — its representation, its
discipline, and the collect-on-overflow policy — is the decision this record fixes.

Constraints inherited from the accepted ADRs:

- **No `&T`/`&mut T` into the moving heap** ([0063](0063-runtime-implementation-language-rust.md)
  §2). A root is a `Value` (a `TaggedWord`, `Copy`) held in a slot the GC rewrites — never a Rust
  reference into the heap.
- **Single-capability, sequential** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
  §0): one root set, no cross-thread concerns.
- **Safepoints = allocation sites and calls that may allocate** (§4); a no-alloc/no-call loop stays
  safepoint-free.

## Decision

### 1. The shadow stack: a `Vec<Value>` of root slots owned by the `Heap`

The `Heap` gains a **shadow stack** `roots: Vec<Value>` — the single, authoritative root set for a
collection (v1 does no register/native-stack scanning). A root is a `Value` word; the collector
evacuates each pointer root and **rewrites the slot in place** with the survivor's new address, so a
value held across a safepoint is recovered by **re-reading its slot**, never from a stale local.

The `Vec` may reallocate as it grows, but a root **handle is an index**, not a pointer into the
`Vec`, so handles stay valid across growth; a collection changes slot *contents*, never the stack's
length. (Host-side `Vec` growth is ordinary Rust allocation — unrelated to the GC heap.)

### 2. The root API: a borrow-free frame mark, a handle, and explicit reload

Three operations, none of which retains a borrow of the `Heap`, so ordinary `heap.root(…)` /
`heap.alloc(…)` / `heap.apply(…)` calls remain legal while a frame is open:

- `root(&mut self, v: Value) -> Root` — push `v` onto the shadow stack; return a `Root` handle (its
  slot **index**, a `Copy` value — never a borrow or a pointer into the stack, so it survives `Vec`
  growth and does not lock the `Heap`).
- `get(&self, r: Root) -> Value` — read the **current** value of a root (post-relocation if a
  collection has fired). This *is* the "reload after safepoint" step, made explicit: after any call
  that may allocate, a caller re-reads its live roots with `get` instead of trusting the `Value` it
  passed to `root`.
- `frame(&self) -> RootFrame` / `pop_frame(&mut self, RootFrame)` — a **frame mark** is the
  shadow-stack length captured by value (`RootFrame(usize)`, `Copy`, borrows nothing); `pop_frame`
  truncates the stack back to that mark, releasing every root pushed since. A block roots its live
  values, does its work, then pops its frame.

**Why a mark, not an RAII `&mut Heap` guard.** A guard owning `&mut Heap` would block the very
`heap.root`/`heap.alloc`/`heap.apply` calls the scope needs (Rust's exclusive-borrow rule); a guard
that instead held a raw `*mut Heap` to run its `Drop` would reintroduce the aliasing/interior-mutability
hazards this island exists to avoid ([0063](0063-runtime-implementation-language-rust.md) §2), and
would make a stale `Root` easier to misuse. The borrow-free mark keeps the `Heap` freely callable and
is exactly the balanced push/pop shape codegen emits. Its cost is that unwinding does **not**
auto-pop; in v1 this is benign — an unwound `Root` merely over-retains until its `Heap` is dropped
(tests) and no collection reads a half-torn frame — and codegen emits statically balanced frames.

The discipline (the invariant codegen will also honour): **a `Value` that must survive a safepoint is
rooted before the safepoint and re-read via `get` after it.** A `Value` not held across any allocation
needs no rooting.

**Misuse boundary — the root API is `pub(crate)`, a trusted discipline, not a safe public contract.**
The borrow-free mark cannot make double-pop, non-LIFO pop, or a stale `Root` (used after its frame is
popped — an ABA where the slot index has been reused) *type* errors. Rather than bolt a lifetime scheme
onto it, v1 draws the boundary explicitly: `root`/`get`/`frame`/`pop_frame` are **`pub(crate)`** — used
only by `apply`, the constructors, and (later) codegen, all of which emit statically balanced,
stack-ordered frames — so misuse is a *runtime-internal invariant violation*, never a soundness hole a
*safe external* caller can reach. To catch that internal misuse during development, the **debug build
carries a per-slot generation stamp**: a `Root` records `{ index, generation }`, each push assigns a
fresh generation to its slot, `pop_frame` invalidates the popped slots' generations, and `get`
`debug_assert!`s the live slot's generation matches the handle's — so a stale/reused-index `Root` (which
the bare "index past top" check misses) is caught. `pop_frame` likewise `debug_assert!`s its mark does
not exceed the current top (double-pop to a stale-larger mark). Release builds drop the stamp and trust
the discipline (a bad `get` still cannot read out of bounds — the slot index is in range; it would read
the wrong live root, which the forced-GC differential (§6) catches). The generation is debug-only, so it
adds no release footprint to the hot `Value`-sized `Root`.

### 3. Rooting discipline: value-storing constructors, `apply`, and `CodeFn` bodies

The rule is **per-function**: each function roots the `Value`s *it* holds across a safepoint of its
own, reloading them after — there is no caller-roots-callee scheme. This is exactly the discipline
codegen emits per generated function. Three sites carry the burden:

- **Value-storing constructors are self-rooting.** `new_adt`, `new_array`, `new_ref`, `new_closure`,
  `new_pap`, and the low-level `new_closure_raw` take `Value` inputs (`fields`, `env`, `function`,
  `captured`, …) and store them *after* their internal `alloc` — a safepoint. If that `alloc` collects,
  the incoming words are stale, so the constructor could otherwise store a dangling pointer into a
  fresh object. The constructors therefore **root their `Value` inputs before `alloc` and re-read them
  (via `get`) when writing the fields**, and their safe signatures stay safe (any live `Value` may be
  passed). The new object pointer needs no rooting during its own field writes (writes do not
  allocate). `new_closure_raw` stays `unsafe` **only** for its raw `code` word — its `env` is
  self-rooted like every other input.
- **`apply`** roots only what *it* holds across a safepoint. Its one safepoint call in the loop that a
  value must outlive is `code(…)` on **over-application**: the **leftover args** (read after the call)
  are rooted across it and reloaded. `apply` does *not* root `f` (not read after `code` —
  over-application replaces it) nor the saturated call args (snapshotted from `args` with no allocation
  between the snapshot and `code`'s entry, so the callee sees current addresses). Under-application's
  `new_pap` is itself self-rooting, so `apply` roots nothing for it.
- **`CodeFn` bodies** own rooting *their* view: the `closure` and `args` are valid on entry, and a body
  that holds any of them across an allocation it performs roots and reloads it the same way; a body
  that touches its inputs only before it allocates (a leaf primop) needs no rooting. `apply` does not
  root the callee's inputs on its behalf — see the `CodeFn` contract in the runtime source.

### 4. GC on allocation: collect-and-retry, then OOM

`alloc` becomes: bump-allocate; on overflow, **collect once** (roots = the shadow stack), then
**retry** the bump in the freshly compacted space. If it still does not fit — the live set alone
exceeds a semi-space — that is a **fatal OOM** (a clear abort, not silent corruption). Heap
*growth* / resizing is deferred (a later increment / v2); v1 fixes each semi-space at `Heap::new`.

Mechanically, `alloc` feeds `self.roots` to the collector **without moving the stack out**. The
collection core is a function of the individual fields — the two space bases, `cap`, `&mut top`, and
`&mut roots` — so `alloc` calls it with *disjoint* `&mut` borrows of `self`'s fields rather than a
`&mut self` method that would alias `&mut self.roots`. This removes the earlier `mem::take` design and
with it the window it left: **there is no point at which `self.roots` is empty**, so an
invariant-`panic!` or OOM during a collection cannot leave the `Heap`'s root set silently torn out.

**Panic-during-collection policy.** A `panic!` raised inside a collection (a tripped memory-safety
invariant, or OOM) is a **fatal, unrecoverable runtime fault**, not a recoverable unwind. At the FFI
boundary the runtime aborts (the [0063](0063-runtime-implementation-language-rust.md) §3 "panic never
crosses FFI" rule); in Rust tests a collection-invariant panic (`#[should_panic]` / OOM) means that
`Heap` is **not reused** after the unwind — it is dropped, freeing both spaces. So no code observes a
post-panic half-collected heap. (The disjoint-field design above additionally guarantees the *root set*
stays structurally intact even under a caught unwind, so a debugger/test inspecting the dropped `Heap`
sees consistent roots.)

**The shadow stack is the only runtime collection root set; the explicit-slice form is demoted.**
`self.roots` (§1) is authoritative, so a collection must never move the heap while leaving those slots
un-rewritten. The existing `collect(&mut [Value])` entry
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §5) is therefore **demoted to a
`pub(crate)`/test-only** helper: it is *not* a public alternate root path. The single public runtime
trigger is the `alloc`-driven `gc()`, always rooted by (and rewriting) `self.roots`. The low-level
`collect(&mut [Value])` remains only for unit tests that exercise the collector in isolation, and its
contract is that **the caller-supplied slice is the complete root set for that call** — such tests keep
the shadow stack empty, so the two root sources never coexist and diverge. (An equivalent framing, if a
future caller needs both: every collection always additionally includes `self.roots`; v1 takes the
simpler "one source at a time" rule.)

### 5. The shared arena stays append-only — and v1 has no shared pointers yet

Unchanged from [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §5: S1
(CAF-constant graphs, interned strings) is append-only / not collected in v1. GC-on-alloc collects
the **local** semi-space only; the shared arena is not a root source and is not evacuated.

Reconciling this with [0061](0061-capability-local-shared-immutable-gc.md) (local GC treats a
`local → S1` edge as a **non-moving external leaf** — preserved unchanged, not scanned) requires the
collector to **classify pointer ownership**: evacuate a local pointer, leave an S1 pointer untouched.
v1 has **no S1 yet**, so that classifier is not built. The consequence is a hard invariant, not an
omission: **in v1 no shared pointer may appear in a root or a value slot** — every traced pointer is
local (from-space) and is evacuated. A non-local pointer reaching `evacuate` is a bug the collector
traps (the existing "pointer outside the from-space" guard, already noted in `gc.rs` as the placeholder
for the deferred evacuate-iff-local dispatch). The ownership classifier — and with it the ability to
hold S1 pointers — lands when S1 itself does (v2 / a later increment), not in this ADR.

**Write-barrier hook — placed now, no-op in v1.** v1 needs no write-barrier *logic* (no generational
old→young tracking, no cross-capability partition to enforce —
[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §5). But every pointer store into a
value slot (`write_field`, the closure knot-tie back-patch, a future `Ref` set / S2 mutation) routes
through a **single choke point carrying a `write_barrier(obj, slot, new)` hook that is a no-op in v1**.
Placing the *hook* now — not its logic — means the generational remembered-set barrier and the
`local → shared` partition check drop in at one site later, without re-touching every store call site.
(Initialising stores while a fresh object is still being filled do not conceptually need a barrier; the
firing condition — store into an already-live/old or shared object — is future logic behind the hook.)
This follows the [0061](0061-capability-local-shared-immutable-gc.md)/[0062](0062-mn-work-stealing-scheduler-fibers.md)
review's conclusion that the real unresolved risk is the shared/cross-capability side, so its seams are
kept cheap to fill — while the sequencing (M:N/cross-capability **before** generational GC) stays a
roadmap decision outside this ADR (see the deferred list in
[0064](0064-v1-single-capability-native-abi-codegen-contract.md)).

### 6. Validation

- **Miri** ([0063](0063-runtime-implementation-language-rust.md) §4) on tests that **force a
  collection mid-`apply`** (heaps sized so allocation overflows during evaluation) — the case the
  current "alloc panics, so no mid-apply GC" note explicitly could not exercise. Miri catches the
  **UB-shaped** misses: a missed root whose stale word dangles or lands outside any allocation surfaces
  as a use-after-move (dangling read) or a Stacked-Borrows / object-start rejection.
- **Forced-GC functional / differential** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
  §7) catches the **correctness** misses Miri does not guarantee: a missed root whose stale word
  happens to alias another live object is not UB — it is a *logic bug* (wrong value / wrong `Effect`
  order). Allocation-heavy fixtures that force collection are therefore held to boot's value +
  `Effect` parity, not left to whether a miss happens to be UB-shaped.

## Consequences

- `alloc` self-collects; allocation-heavy programs run without the artificial "panic on overflow"
  ceiling. The `collect`-mid-`apply` path is exercised and Miri-checked, closing the standing
  "in-flight args not rooted" gap.
- A single rooting shape serves both the hand-written v1 bring-up and the eventual codegen: codegen
  emits the same push/re-read pattern the interpreter uses, so §4's contract has one realisation, not
  two.
- Roots are `Value` slots the GC rewrites — no Rust reference into the moving heap, so the
  [0063](0063-runtime-implementation-language-rust.md) island discipline holds.
- Cost: rooting ceremony in the value-storing constructors, `apply`, and hand-written `CodeFn` bodies.
  Accepted — it is exactly what codegen will emit, and the borrow-free frame mark keeps it mechanical.
  The self-rooting constructors keep the safe `new_*` API safe under GC-on-alloc; code that holds no
  live value across a safepoint pays nothing.
- OOM is a hard abort in v1 (fixed semi-spaces). Programs whose live set exceeds a semi-space need the
  deferred heap-growth work; sizing at `Heap::new` is the v1 lever.

## Alternatives considered

- **Keep `alloc` panicking; never GC.** The status quo. Cannot run any program whose allocation
  exceeds one semi-space — defeats the purpose; only viable as the current pre-rooting stopgap.
- **Conservative stack scanning** (scan the native stack for word patterns that look like heap
  pointers). No rooting ceremony, but it is imprecise (cannot *move* an object a false-positive root
  "pins"), which a **copying** collector cannot tolerate, and it does not match the precise
  shadow-stack contract §4 commits codegen to. Rejected.
- **`gc.statepoint` / stack maps now.** The eventual precise-root mechanism under LLVM, but it is a
  codegen-era tool ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 defers
  `addrspace(1)`/statepoints); there is no codegen to attach maps to yet. The shadow stack is the
  v1 vehicle and stays valid as a fallback even after statepoints land.
- **Root every `Value` unconditionally (root-on-create).** Simpler rule, but it roots values that
  never cross a safepoint (most of them), bloating the shadow stack and every collection's root
  scan. The "root across a safepoint only" discipline is the standard precise-rooting trade and what
  codegen will emit.
- **Grow the heap on overflow instead of OOM.** Deferred, not rejected: a fixed heap keeps v1 simple
  and makes OOM observable; growth (and later generational collection) is a follow-up increment.
