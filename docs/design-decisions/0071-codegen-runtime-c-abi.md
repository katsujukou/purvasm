# 0071. The codegen↔runtime C-ABI: `extern "C"` surface, real-address code words, the trampoline tail-call protocol, and the value/rooting/panic boundary

- Status: ~~Proposed~~ **Accepted** _(2026-07-02: accepted by the maintainer)_
- Date: 2026-07-02

> **Revision (2026-07-02, pre-acceptance review round 1):** §4 rewritten — the tail-bounce now composes
> with over-application through an explicit **continuation stack**, and the `pv_tailcall` / `pv_pop_frame`
> ordering is pinned (a single pending-tail slot dropped over-application leftovers). §3 adds the
> **`u64 → extern "C" fn` reconstruction as a documented supported-target ABI assumption**. §5 pins the
> **`Root` / `RootFrame` C-ABI handle types** (both `u64`, ABI-invariant across debug/release). §6 adds
> **`pv_empty_array`** (the runtime `new_array` rejects empty, and `[]` occurs in real code).
>
> **Revision (2026-07-02, review round 2):** §4 — the **`conts` continuation stack is per-`pv_apply`
> activation-local, not context-shared** (a ctx-global `conts` let a callee's nested `pv_apply` pop the
> outer activation's leftovers); only the single pending-tail slot is context-shared, and the reentrancy
> argument is redone around that. §5 — the root handle is an **opaque `u64` that codegen round-trips
> verbatim**, into which the runtime **packs a debug generation** (a bare index cannot carry the
> generation the stale-reuse check compares).
>
> **Correction (2026-07-02, during implementation):** §6's *Effect + leaves* surface is synced to the
> implemented reality: **`Effect.Ref` / `ST` are structural guest terms, not leaves** (so there is **no
> `pv_ref_*` / `pv_stdio_write_line` symbol** — leaves resolve through `pv_foreign`, records through the
> `pv_prim_record_*` primops), and `pv_drain_output` is added. This averts a codegen author emitting
> non-existent symbols.
>
> **Correction (2026-07-03, during slice-5 implementation):** §6's *Apply / force* surface adds
> **`pv_force_if_byneed`** — the conditional force (a by-need cell chain, else pass-through) codegen emits
> at every value-demand site, alongside the unconditional `pv_force`. It was implemented (a directly
> depended-on C-ABI symbol) but not listed.

## Context

[0059](0059-native-abi-value-representation.md)–[0064](0064-v1-single-capability-native-abi-codegen-contract.md)
fixed the wall-2 *design*, and [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)–[0070](0070-v1-byneed-recursive-caf-force.md)
built the v1 Rust runtime's **semantic layer**: the tagged-word value model, the Cheney collector with
GC-on-alloc, the eval/`apply` calling convention, shadow-stack rooting, `Effect`/`Ref`/stdio leaves,
dynamic records, and by-need recursive CAFs. But that layer is a **Rust-interpreter-style bring-up**
([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) Context): its surface is Rust methods on
`&mut Heap`, `CodeFn` is a Rust `fn(&mut Heap, Value, &[Value]) -> Value`
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3), a closure's `code` word is a
**code-table index** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3 Correction),
and the rooting API is `pub(crate)` ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §2). Every
one of these records the same deferral: *"the C-ABI / native FFI boundary … land[s] in following
increments"* — with codegen.

Codegen now lands. It splits into two records (the [0059](0059-native-abi-value-representation.md) ABI /
[0060](0060-native-codegen-llvm-owned-runtime.md) codegen split, repeated): **this ADR fixes the
`extern "C"` boundary** between LLVM-generated code and the runtime — the surface, the calling protocol,
and the value/rooting/panic contract — and [0072](0072-anf-to-llvm-lowering.md) fixes the ANF→LLVM
*lowering* that consumes it. Generated code and the runtime must agree on this boundary exactly, so it is
pinned first.

Inherited constraints:

- **[0063](0063-runtime-implementation-language-rust.md) §2/§3** — GC memory is never a Rust reference;
  the boundary is `Value` / opaque handles as machine words; `extern "C"` + `#[repr(C)]`; **a panic
  never crosses the FFI boundary**.
- **[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3** — boundary values are tagged
  words (`Value`) or opaque handles, *never* a Rust reference into the heap nor an `addrspace(1)`
  pointer; the runtime C-ABI surface named there is `gc_alloc`, `apply`, `make_closure`, the primops,
  `run_effect`, the foreign leaves.
- **[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4** — v1 uses a **trampoline, not
  `musttail`**; safepoints are alloc/calls only; managed pointers use `addrspace(0)` (statepoints
  deferred).
- **[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)** — the shadow-stack rooting API is the
  precise-roots vehicle codegen targets *"in the same shape"* the interpreter uses.

## Decision

### 1. The runtime is a `staticlib`; the guest-heap word is the boundary type

The `purvasm-rt` crate builds as `crate-type = ["staticlib", "lib"]`: the **`staticlib`** exposes the
`extern "C"` surface below and is linked into the LLVM-codegen output
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4); the **`lib`** keeps the existing
Rust-unit-test + Miri surface ([0063](0063-runtime-implementation-language-rust.md) §4) unchanged — the
two are the same code behind different front doors.

Every guest value crosses the boundary as a **`u64`** — the raw [`TaggedWord`] bit pattern
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §1) — never a Rust reference nor an
`addrspace(1)` pointer ([0063](0063-runtime-implementation-language-rust.md) §2). This re-exposes
`TaggedWord::{from_bits,to_bits}` (today crate-internal, with a note reserving *"a dedicated `unsafe`
entry when the … C-ABI boundary lands"*) as the boundary's word decode/encode.

**The soundness shift this forces, and how it is contained.** Today only the runtime can mint a *pointer*
word (`from_addr`/`from_bits` are `pub(crate)`), so a safe caller cannot forge a pointer the collector
would deref as a header. Generated code breaks that monopoly: it stores, reloads, and hands back words in
its own frames. The boundary therefore **trusts codegen for a word's *tag* but validates its
*liveness/shape* on every dereference** — exactly the existing `apply` discipline (the [`checked_ptr`]
object-header-start check the lib.rs pointer-word invariant already documents). A forged, stale, or
interior pointer word from buggy codegen is a **release fault, not UB** — the same net as v1 has today.
No new trust is extended to the *shape* of an inbound pointer; only the *tag bit* is taken on faith,
which is unavoidable once codegen owns its own value slots.

### 2. The runtime context is an opaque `*mut Heap`

Every entry takes the runtime context first: `pv_runtime_new(local_words: usize) -> *mut Heap` /
`pv_runtime_free(*mut Heap)`, and the context threads into every other call as `ctx: *mut Heap` — the
C-ABI realisation of the `&mut Heap` every `CodeFn` already receives. The context owns the local
semi-spaces, the shadow stack ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §1), the code
address table (below), the pending-tail slot (§4), and the v1 output sink
([0067](0067-v1-effect-execution-and-native-leaves.md) §5). Single-capability
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §0), so one context, no cross-thread
concern.

### 3. The `CodeFn` C-ABI, and the closure `code` word as a **real function address**

A generated function has the signature

```
extern "C" fn(ctx: *mut Heap, closure: u64, args: *const u64, nargs: usize) -> u64
```

— the C-ABI form of the `CodeFn` shape ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
§3): the runtime, the closure (for its env / free variables), and exactly `arity` arguments as a
pointer+length, returning the result word.

**A closure's `code` word holds the function's real address, not a code-table index.** Each generated
function is an `extern "C"` symbol; `pv_make_closure(ctx, code_addr, arity, env)` stores `code_addr` in
the `code` raw word, and `pv_apply` calls it through the typed `extern "C" fn` pointer. This is the
native-ABI endgame form [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3 named
(*"under the native ABI the `code` word is a real code address the generated code calls directly"*). The
**code-table index remains the `lib`/Miri realisation only** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
§3 Correction): the two never mix in one heap. Miri never runs the compiled binary, so calling a real,
linker-provided function pointer is standard — *not* the provenance-free `usize→fn` transmute Miri
rejects for the hand-written `CodeFn` path. Concretely, `pv_apply` is a distinct entry that interprets
the `code` word as an address; `Heap::apply` (the `lib` tests) keeps interpreting it as a table index.
The closure representation ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2) gains no
new field — only the interpretation of the existing raw `code` word differs by front door.

**The `u64 → extern "C" fn` reconstruction is a documented supported-target ABI assumption.** On the
compiled path `pv_apply` rebuilds a callable `extern "C" fn` pointer from the stored `code` word. This is
sound because the word holds a **linker-provided symbol address** — a real code pointer with provenance in
the process image — so calling it is ordinary indirect dispatch, *not* the provenance-free `usize → fn`
transmute Miri rejects for the code-table stand-in
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3 Correction). It rests on the C-ABI
guarantees of the **supported 64-bit native targets** (x86-64 / ARM64 / RISC-V64,
[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §1) — a function pointer is one machine
word, and the `extern "C"` signature matches on both sides — which this ADR states as an **explicit ABI
assumption for supported targets**, not a Rust-abstract-machine guarantee (hence it lives on the
compiled, non-Miri front door only, never in the `lib`/Miri path).

### 4. The trampoline tail-call protocol — "bounce by value" via a pending-tail slot + continuation stack

All v1 calls route through `apply` ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3),
and v1's TCE is a **trampoline, not `musttail`** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
§4: *"a tail call bounces by value … a small register/stack-passed descriptor, with no per-call heap
thunk … growing neither the native nor the shadow stack"*). The subtlety a first draft missed is that a
tail bounce must **compose with over-application**: in `f a b` where `f` has arity 1 and its body
tail-calls `g x`, the leftover `b` is a *continuation* to apply to the value the whole `g x` chain
ultimately produces — the current `apply` loop applies a leftover to `code`'s immediate result
([apply.rs](../../runtime/src/apply.rs) over-apply case), which is wrong once a tail bounce sits between
the call and its value. So the pending slot alone is insufficient; the protocol carries a **continuation
stack**.

- **Only the pending-tail slot is context-shared.** The context carries **one** `{ f, args }` pending-tail
  slot + a **status flag** (a small reusable buffer, no per-call heap thunk —
  [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4). The **continuation stack `conts`
  is per-`pv_apply`-activation local state** — it lives on the native call stack of that `pv_apply`
  invocation (the reviewer's *"active apply frame"*), exactly as today's over-apply leftover is a Rust
  local to its `apply` call ([apply.rs](../../runtime/src/apply.rs)), and is **never shared through the
  context**. This is the fix for the reentrancy hazard a ctx-global `conts` would have: a callee's non-tail
  inner `pv_apply` runs with its *own* fresh `conts` and cannot pop the outer activation's leftovers.
- **Loop invariant:** each `pv_apply(ctx, f, args, nargs)` activation's local `conts` holds the leftover
  groups *that activation* still owes, innermost-last, once its current `(f, args)` chain yields a *real*
  value. The loop:
  - **Under-application** → build the PAP `p` (the current call's value); if `conts` is empty return `p`,
    else pop `L`, set `(f, args) = (p, L)`, continue.
  - **Saturate / over-apply** → split `call_args = args[..arity]`, `leftover = args[arity..]`; if
    `leftover` is non-empty **push it onto `conts`** (rooted); then call `code(f, call_args)`. After the
    call, inspect the status flag: **tail-bounce set** → take `(f', args')` from the pending slot and
    continue with it (the just-pushed leftover stays on `conts`, correctly deferred behind the new
    chain); **clear** (a real value `r`) → if `conts` is empty return `r`, else pop `L` and continue with
    `(r, L)`.
  - **PAP / `ByNeed` callee** → flatten / force and re-dispatch (unchanged).

  This generalises `apply`'s existing single-leftover step into a stack; **without any tail bounce it is
  behaviourally identical** (push-then-immediately-pop = the current apply-leftover-to-result), so the
  over-application tests are preserved. A tail-recursive loop never over-applies, so `conts` stays empty
  and the stack is flat; `conts` depth is bounded by outstanding *over-applications* (program structure),
  never by recursion depth.
- **Non-tail `CApp`** → the body calls `pv_apply` normally; the native stack grows with non-tail nesting,
  as in any language.
- Everything on a `conts` and the pending `(f', args')` is **live across the loop's next `code`
  safepoint**, so `pv_apply` keeps them rooted for their lifetime. The **roots** live on the ctx-owned
  shadow stack (§5) — that is shared, but reentrancy-safe by construction: each activation opens its own
  frame, so nested activations' roots stack LIFO and never alias, while the *`conts` handles* (the `Root`
  indices) stay activation-local. This extends the leftover rooting `apply` already does
  ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3).

**Generated-body ordering at a tail call — `pv_tailcall` *before* `pv_pop_frame`.** A body opens a frame
at entry (§5); at a tail `CApp` it: (1) `pv_get`-reloads the values it needs for `f'`/`args'`; (2) calls
`pv_tailcall(ctx, f', args', n)`, which **copies `f'` and the args into the runtime's own rooted pending
storage** — taking ownership *before* the body's roots die; (3) calls `pv_pop_frame(ctx, frame)` to
release the body's roots; (4) returns (its return word is ignored — the status flag drives the loop). The
`pv_tailcall`-then-`pv_pop_frame` order is the trampoline's realisation of the "pop caller roots before
the tail" discipline [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 named for
`musttail`: the runtime re-roots the bounce descriptor so the body can safely drop its frame.

**Reentrancy invariant (the single shared slot is safe).** Two facts keep the one context-shared
pending-tail slot from crossing activations. **(1)** It is written **only as a body's final action**
(`pv_tailcall` then return) and **read-and-cleared by the enclosing `pv_apply` immediately after `code`
returns, before that loop makes any further `code` call** — so between a write and its read there is no
other `code` call that could clobber it, and one reusable slot suffices. **(2)** A non-tail `pv_apply` a
body makes fully resolves its own tail chain (looping until *its* slot is clear *and its local `conts` is
empty*) before returning, so it leaves the slot clear and an outer loop never observes an inner bounce.
The `conts` stacks cannot cross activations at all — they are activation-local (above). The **status
flag**, not a reserved sentinel value, signals the bounce, so no real result word can collide with it.

### 5. The rooting API at the boundary

[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §2's `root` / `get` / `frame` / `pop_frame` gain
`extern "C"` entries `pv_root` / `pv_get` / `pv_frame` / `pv_pop_frame`, so codegen emits the
**per-function** shadow-stack discipline ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3) —
*when* it emits them is [0072](0072-anf-to-llvm-lowering.md) §6. This is the same push/`get`-reload/pop
shape the interpreter uses, now callable from IR —
[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)'s *"one realisation, not two"*. The shadow stack
stays per-`Heap` (single-capability).

**The handles are opaque `u64`s that codegen round-trips verbatim.** The internal `Root` / `RootFrame` are
`pub(crate)` Rust types, and `Root` additionally carries a **debug-only generation stamp**
([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §2 / [gc.rs](../../runtime/src/gc.rs)) — neither
can cross the C-ABI as a struct, and the stamp must **not** make the ABI vary by build profile (LLVM emits
one IR). So the boundary types are plain `u64`: `pv_root(ctx, v: u64) -> u64` and `pv_frame(ctx) -> u64`
return handles, `pv_get(ctx, root: u64) -> u64` and `pv_pop_frame(ctx, frame: u64)` take them back — and
**codegen treats the returned `u64` as opaque**, storing and passing it back without ever interpreting or
computing it.

Because codegen never inspects the handle, its *bit encoding* is a **runtime-internal choice invisible to
the IR**, and that is what lets the debug stale-reuse check survive the boundary: a bare slot index could
**not** catch a stale handle whose index was popped and reused (the handle would carry nothing to compare
against the slot's current generation — the flaw the reviewer flagged). So the runtime **packs the
generation into the `u64`** — e.g. the slot index in the low bits, the generation in the high bits — and
`pv_get` / `pv_pop_frame` `debug_assert!` the packed generation against the live slot's, exactly as the
Rust `Root` does today. Release builds may pack a zero generation and skip the check. Either way the
**emitted IR is identical** (codegen round-trips one opaque `u64`), so the codegen contract does not depend
on the build profile — only the runtime's interpretation of bits it minted itself does.

### 6. The allocator / field / effect / primop / leaf surface

The remaining families are `extern "C"` thin wrappers over the existing `Heap` methods (the exact
signatures live in the runtime source, kept isomorphic to the value model — not dumped here, per
[0064](0064-v1-single-capability-native-abi-codegen-contract.md)/the ADR-vs-source rule):

- **Allocation / constructors** — `pv_new_adt`, `pv_new_number`, `pv_new_ref`, `pv_new_array`,
  `pv_empty_array` (below), `pv_new_str`, `pv_new_record`, `pv_new_byneed`, `pv_make_closure`, plus the
  `Grec` **placeholder-cell + backpatch** entries ([0070](0070-v1-byneed-recursive-caf-force.md) §4). Each
  keeps its **self-rooting** contract ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §3), so the
  boundary inherits it.
- **The empty array is an immediate sentinel.** The runtime `new_array` **rejects a zero-length array**
  (the `size_words >= 1` header invariant; the heap empty-array singleton is deferred,
  [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2/§5), yet `[]` and `newArray 0` occur
  in ordinary code. v1 represents the empty array as an **immediate sentinel**, exactly the pattern
  [0069](0069-v1-dynamic-record-operations.md) §1 uses for the empty *record* (a `unit` sentinel in both
  slots), so no zero-length heap object is ever formed. `pv_empty_array()` returns it, and the **array
  primops recognise it**: `LengthArray` → `0`, `IndexArray` → out-of-bounds fault, `SetArray` →
  out-of-bounds fault, `Append` → identity on either side, `NewArray 0` → the sentinel. This un-defers the
  empty array *for the value ABI only* (the general heap empty-array singleton stays deferred); it is the
  small representation decision codegen needs, and [0072](0072-anf-to-llvm-lowering.md) §5 consumes it.
- **Field access** — `pv_read_field` / `pv_write_field` / `pv_read_raw` / `pv_write_raw`; the
  write-barrier no-op hook stays at the store choke point
  ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §5).
- **Apply / force** — `pv_apply`, `pv_tailcall` (§4), `pv_force`, and `pv_force_if_byneed(ctx, v) -> u64`
  ([0070](0070-v1-byneed-recursive-caf-force.md)) — the latter forces `v` **iff** it is a `ByNeed` cell
  (looping a cell chain) and passes any other value through, so codegen can emit it unconditionally at a
  **value-demand site** where a by-need cell may have arrived through an argument or data field, without
  static by-need tracking (`pv_force` is the unconditional force of a known cell).
- **Effect + leaves** — `pv_run_effect` and `pv_force`
  ([0067](0067-v1-effect-execution-and-native-leaves.md)/[0070](0070-v1-byneed-recursive-caf-force.md)),
  plus `pv_drain_output` (flush the capture sink to real `stdout` at exit, ADR-0067 §5). The **native
  leaves are not individual `extern` symbols**: they resolve through the **foreign-leaf resolver**
  `pv_foreign(ctx, key_ptr, key_len) -> u64`, which returns the leaf as a closure (mirroring
  `codegen_ml`'s `foreign` — `Data.Show.*`, `Purvasm.Stdio.writeLineImpl`, `Partial._crashWith`,
  `Purvasm.String.*`, `Data.Number.*`, `Purvasm.FS/System.*`; the set grows on demand). **`Effect.Ref`
  and `ST` are *not* leaves** — they are *structural* guest terms (`boot`'s `Ffi.structural`) that the
  linker substitutes to array primops before codegen ([0072](0072-anf-to-llvm-lowering.md) §9), so the
  runtime exposes **no `pv_ref_*` / `pv_stdio_write_line` symbol**. The dynamic record operations are the
  `pv_prim_record_{get,set,has,delete}` primops (below), with `str_label_id` internal to them.
  _(Corrected 2026-07-02 after the Effect.Ref-structural finding: the earlier draft listed `pv_ref_*` /
  `pv_stdio_write_line` / `pv_record_*` as direct symbols; the implemented surface routes leaves through
  `pv_foreign` and records through the primops.)_
- **Entry-print helper** — `pv_print_int(v)` writes a **pure `Int` entry**'s value to `stdout` (no
  trailing newline), matching the oracle's `Value.to_string` for `Int`. The codegen entry stub calls it
  for a pure `Int` program ([0072](0072-anf-to-llvm-lowering.md) §8); the native value is type-erased, so
  there is no generic runtime `to_string` — a per-entry-type printer is emitted per the codegen's known
  entry type, and the other-type printers are added with the slices that introduce them.
- **Primops** — one `pv_prim_*` per `Cesk.Ast.primop` (the ~40 in `codegen_ml`'s `prim_fn`). Making them
  **runtime helpers keeps one tested source of truth** for the tricky semantics — 32-bit wrapping,
  Euclidean div/mod, ECMAScript `ToInt32`/`Math.round` ([0041](0041-int-number-conversion-primops.md)/
  [0042](0042-number-math-primops.md)) — rather than re-deriving them in IR. Scalar primops don't
  allocate (no safepoint); the structural/allocating ones (`Append`, array, record) do. (Inlining the
  scalar-immediate ops in IR is a deferred perf lever — [0072](0072-anf-to-llvm-lowering.md) §7.)

### 7. Panic containment

Every `extern "C"` entry wraps its body so a Rust `panic!` (a tripped memory-safety invariant, or OOM —
[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §4) **aborts** rather than unwinding into
LLVM-generated frames (which would be UB): a `catch_unwind` → `abort` shim at the boundary
([0063](0063-runtime-implementation-language-rust.md) §3). It is **`catch_unwind`, not `panic = "abort"`
on the whole crate**, because the `lib`'s `#[should_panic]` / OOM tests
([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §4) need unwinding — the abort is confined to
the `staticlib`'s front door. The differential then observes a clean abort, never corruption.

### Deferred

- **The direct known-arity fast path + `musttail`** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
  §4/§Deferred) — v1 is all-`pv_apply` + the §4 trampoline; `musttail` needs matching signatures and the
  pop-caller-roots discipline, and lands with the fast path.
- **`addrspace(1)` + `gc.statepoint`** — the shadow stack is the v1 vehicle
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4,
  [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §7).
- **Cross-module rep publication** (unboxed cross-module entries via `.pmi` rep signatures) —
  [0059](0059-native-abi-value-representation.md) §3, gated on measurement; v1's cross-module boundary is
  boxed.
- **The v2 shell's typed pointer newtypes** (`LocalRef` / `SharedRef` / `WakeToken`) —
  [0063](0063-runtime-implementation-language-rust.md) note; v1 has no partition.
- **Real-`stdout` wiring vs the capture sink** — stays as
  [0067](0067-v1-effect-execution-and-native-leaves.md) §5 (production wires the sink to `stdout`; the
  differential reads captured lines).

## Consequences

- The runtime gains a **stable `extern "C"` front door** that LLVM-generated code links against, with the
  guest-heap word (`u64`) and opaque `*mut Heap` as the only boundary types
  ([0063](0063-runtime-implementation-language-rust.md) §2) — the same code the `lib`/Miri tests already
  exercise, now callable from IR.
- **Code words become real addresses on the compiled path**, retiring the code-table indirection from
  the hot `apply` loop for generated programs while keeping the Miri-clean index path for the `lib`
  tests — the two realisations [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3
  anticipated.
- The **trampoline protocol (§4) is now concrete**: a pending-tail slot + status flag gives flat-stack
  TCE for the all-`apply` v1 without `musttail`, at the cost 0064 §Consequences already priced ("the
  trampoline + all-`apply` path can widen the gap … until the direct fast path and `musttail` land").
- The boundary **takes an inbound pointer word's *tag* on trust but validates its *shape/liveness* on
  every deref** (§1), so a codegen bug is a release fault, not silent corruption — the property the
  forced-GC differential ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §6) leans on.
- Panic containment (§7) keeps a runtime invariant trip from becoming cross-language UB.

## Alternatives considered

- **Keep the code-table index on the compiled path too** (codegen registers each function, closures store
  indices). Miri would then cover the compiled call path — but it re-imposes a per-call table indirection
  in the hot loop and forgoes the native-ABI endgame
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3). Rejected: real addresses are the
  destination, and the compiled binary is validated by the differential, not Miri.
- **A reserved sentinel immediate for the tail bounce** (instead of a status flag). Rejected: any
  reserved value can collide with a real result (`Unit` / a small `Int` / a nullary ctor are all
  immediates); an out-of-band status flag (§4) is collision-free.
- **`panic = "abort"` for the whole crate.** Simpler than `catch_unwind`, but it breaks the `lib`'s
  `#[should_panic]` / OOM tests ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §4). Confining
  the abort to the `staticlib` boundary keeps both.
- **Emit primops inline in IR now** (no `pv_prim_*` calls). Faster, but it re-derives the 32-bit-wrap /
  Euclidean / `ToInt32` / `Math.round` semantics ([0041](0041-int-number-conversion-primops.md)/
  [0042](0042-number-math-primops.md)) off the one tested runtime implementation. Deferred to a perf
  slice ([0072](0072-anf-to-llvm-lowering.md) §7); correctness-first uses shared helpers.
- **A richer C-ABI that passes `Value` structs / uses `addrspace(1)` now.** Rejected for v1: a plain
  `u64` + `addrspace(0)` is what the shadow-stack (non-statepoint) path needs
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4), and structs over the C-ABI buy
  nothing for a one-word value.
