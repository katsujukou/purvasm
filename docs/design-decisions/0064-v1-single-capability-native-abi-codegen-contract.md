# 0064. v1 first cut: single-capability native runtime — concrete ABI, codegen contract, and boot-parity

- Status: Accepted
- Date: 2026-07-01

> **Revision (2026-07-01):** tightened the §3/§4 moving-GC × shadow-stack × LLVM contract after a
> two-reviewer pass. The under-specified edges that could cause *silent memory corruption or broken TCO*
> are now pinned: a **layout-directed** GC scan (the tag rule applies only within value slots), a concrete
> **forwarding** scheme + invariant, **arity** carried in the closure so `apply` can dispatch, a
> **root-reload-after-safepoint** contract, and — a simplification — **v1 uses a runtime trampoline (no
> `musttail`)** and places **safepoints only at allocation/calls (no back-edge polls)**, which removes the
> "all calls `musttail`" / "every back-edge safepoint" over-promises. v1 targets the **synchronous-`Effect`
> subset**.

## Context

[0059](0059-native-abi-value-representation.md)–[0063](0063-runtime-implementation-language-rust.md) fixed
the wall-2 *design*. This ADR pins the **concrete contract for the v1 first cut** so implementation can
start. Scope, agreed: **single-capability, sequential, aiming for complete semantic parity with `boot`** —
the standing differential oracle (CESK ⇔ VM ⇔ `boot`) extended to the LLVM-native binary as a fourth
implementation ([0036](0036-anf-to-ocaml-value-representation.md) already runs the native path as a
*behavioural* differential). The **entire cross-capability side** — the scheduler
([0062](0062-mn-work-stealing-scheduler-fibers.md)), promotion, `AVar`, wake tokens, `io_uring`,
cancellation, shared collection ([0061](0061-capability-local-shared-immutable-gc.md) §4) — is **deferred
to v2**.

This ADR records the concrete **decisions**; the exhaustive byte-level layout lives in the runtime code,
kept isomorphic to the VM value model ([0011](0011-adt-pattern-matching.md)/[0036](0036-anf-to-ocaml-value-representation.md))
and checked by the differential — not dumped here.

## Decision

### 0. v1 scope — single-capability, sequential, synchronous `Effect`

Single-capability, sequential. **v1 runs the synchronous-`Effect` subset only** — no `Aff`/fork/`AVar`
(those are v2). This is sufficient because the self-compile target (`purvasm-ps`, a batch compiler) is a
synchronous-`Effect` program. Success criterion: the native binary produces the **same values and the same
`Effect` order** as the CESK oracle / VM / `boot` on every fixture and benchmark.

### 1. Tagged word — 63-bit low-tagged immediates (OCaml-style)

A 64-bit word: **LSB = 1 → immediate** (payload is the arithmetic-shifted 63-bit value); **LSB = 0 → an
aligned heap pointer**.

**v1 targets 64-bit platforms only** (x86-64 / ARM64 / RISC-V64) — where the LLVM / `io_uring` /
multicore-parallel runtime lives. The unboxed-`Int` win below needs a 63-bit immediate; on a 32-bit word
the immediate is only 31 bits and `Int` cannot be one, which reverts to
[0059](0059-native-abi-value-representation.md)'s boxed-`Int` scheme. 0059's tagged-word convention is
word-size-agnostic, so a 32-bit port is *possible* (boxed `Int`, 31-bit immediates for
`Boolean`/`Char`/nullary), but it would carry a second representation; v1 declines it and commits to
64-bit. (OCaml's `int` is the same trade: 63-bit on 64-bit platforms, 31-bit on 32-bit.) The tag rule (LSB = 0 pointer / LSB = 1 immediate) is interpreted **only within a
*value slot*** — a field the object's kind marks as holding a `Value` (§2). Raw words (a code function
pointer, an `f64` payload, string bytes, label-id array elements, the header itself) are **never**
tag-interpreted; the collector skips them per the object's layout (§5).

**This refines [0059](0059-native-abi-value-representation.md)'s `Int` representation for the native
target.** 0059 boxed `Int` because a WASM 31-bit `i31ref` cannot hold a 32-bit `Int`; a native 63-bit
immediate holds it easily. So **`Int`, `Char`, `Boolean`, `Unit`, and nullary-constructor indices are all
immediates (unboxed), with no representation analysis.** `Int` keeps 32-bit wrapping
([0006](0006-string-utf8-char-int.md)) by masking on operations — the immediate payload is the
**sign-extended signed 32-bit value** (the canonical form `EqInt`/`OrdInt`/`show`/the FFI boundary read
directly; arithmetic wraps modulo 2³² then re-canonicalises). `Char` is a code point. This removes the
boxing/coercion cost [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) measured, for free.
`Number` (`f64`) does not fit 63 bits and stays **boxed** (NaN-boxing deferred). *(A `Progress`-note-worthy
refinement to [0059](0059-native-abi-value-representation.md), which was written against WASM's 31-bit
limit.)*

### 2. Heap object header, kinds, and forwarding

A field-carrying object carries one header word encoding `{ kind, size, GC-colour }`. **`kind` is the
layout descriptor**: it tells the collector which words are **value slots** (scan by the §1 tag rule) and
which are **raw** (code pointer, `f64`, bytes, ids — never scanned as pointers). Kinds, isomorphic to the
VM value model:

- **ADT** — ctor tag + N value-slot fields (nullary ctors are immediates, §1, so only field-carrying
  ctors are ADT objects).
- **Record** — a label-id array (raw 64-bit FNV-1a ids, [0059](0059-native-abi-value-representation.md)) +
  a parallel value-slot array. Type-class dictionaries use this kind.
- **Closure** — **`{ code fn ptr (raw), arity (raw), env (one *pointer* value slot → a shared
  value-slot block, the `Array` kind) }`**. Arity is carried inline so `apply` can dispatch (§3).
  _(Clarified 2026-07-01: env is a **pointer to a separate block, not inline** — the shared-env
  knot-tying of [0059](0059-native-abi-value-representation.md) §1 requires **one** block shared by a
  mutually-recursive group, which an inline array cannot express; a no-capture closure's env is an
  **immediate sentinel** (so the empty-array singleton stays deferred). The asymmetry with **PAP** —
  whose `captured_args` are inline below, since a PAP's captures are not shared — is intentional,
  despite both being "value-slot array" data.)_
- **PAP** — **`{ function (value slot), remaining_arity (raw), captured_args (inline value-slot
  array) }`** — captured args are *inline* (not shared, so no separate block; contrast Closure's env).
- **String** — length (raw) + packed UTF-8 bytes (raw).
- **NumberBox** — one raw `f64`. **Ref** — one mutable value slot. **ByNeedCell** — its state + a value
  slot.
- **Array** — a value-slot array `[slot; n]`: PureScript `Array`, a closure's shared env block, and a
  record's value array all use this kind. _(Added 2026-07-01 with the runtime — the kind discriminant is
  ABI, so it is listed here.)_ The **raw-id array** a `Record`'s `label_ids` points to is a separate
  raw-array kind, deferred with `Record` construction (the empty-`Array` singleton is likewise deferred).

**Forwarding (Cheney):** an object being evacuated is fully copied to to-space *first*, then its from-space
**header word** is marked **forwarded** (a GC-colour state) and its **first payload word** holds the
forwarding address (header word and payload word are distinct). Because the copy completes before the
overwrite, no live data is destroyed. **Invariant:** every movable object has an
unambiguous forwarding location the collector reads after a move; a from-space object is never read for its
fields once forwarded. (The exhaustive bit layout is in the runtime code; this fixes the contract.)

### 3. Calling convention — arity-aware `apply` + PAP; closure = (code, arity, env)

The generic entry is **`apply(f, args…)`**: it reads `f`'s arity, then
saturates exactly, over-applies (call, then `apply` the remainder), or builds a **PAP** on
under-application (arity is read from the closure/PAP representation, §2). **v1 routes *all* calls through
`apply`**; the direct known-arity fast path
([0059](0059-native-abi-value-representation.md) §3) is deferred. The runtime C-ABI surface called from
generated code (`extern "C"`): `gc_alloc`, `apply`, `make_closure`, the primops, `run_effect`, the foreign
leaves. **Boundary values are tagged words (`Value`) / opaque handles — never a Rust reference into the
heap nor an `addrspace(1)` pointer** ([0063](0063-runtime-implementation-language-rust.md) §2); the
generated code and the runtime agree on the tagged-word ABI.

### 4. Codegen — ANF → LLVM IR; safepoints, shadow stack, tail calls

Per-module **ANF → LLVM IR → object**, linked with the Rust runtime. Each ANF construct lowers to IR
calling the §3 runtime ABI; cross-module references are by qualified name
([0033](0033-separate-compilation.md)). The three moving-GC contracts:

- **Safepoints (v1 = GC points only).** v1 has no preemption, so the GC runs only where allocation can
  overflow the semi-space — i.e. at **allocation sites and calls that may allocate**. A safepoint is such a
  point; a **no-allocation, no-call loop body (e.g. the in-place byte builder,
  [0052](0052-native-unsafesetbyte-in-place.md)) gets no safepoint**, preserving its O(n). (Back-edge polls
  / reduction-count preemption are v2.)
- **Root reload after a safepoint (the key contract).** A live GC pointer that spans a safepoint is
  **slot-backed on the shadow stack and re-loaded from its slot after the safepoint** — it is *not* kept as
  an SSA/register value across a call, because the collector may have relocated it (use-after-move would be
  silent corruption the differential only catches if a GC actually fires). This is distinct from — and in
  addition to — [0061](0061-capability-local-shared-immutable-gc.md) §8's *interior*-pointer re-derivation
  (that re-loads `base + offset`; this re-loads the root itself). The codegen maintains the shadow stack
  (push/pop live roots); the GC reads and updates the slots.
- **Tail calls — v1 uses a runtime trampoline, not `musttail`.** A variable-shape `apply(f, args…)` is not
  a universal `musttail` target, and under a shadow stack `musttail` would also require popping the
  caller's roots first (else the shadow stack leaks on tail recursion). v1 sidesteps both: the eval/apply
  loop **trampolines** — a tail call **bounces by value** (a small register/stack-passed descriptor, with
  **no per-call heap thunk** — else tail recursion would allocate each iteration and offset
  [0052](0052-native-unsafesetbyte-in-place.md)), growing neither the native nor the shadow stack. `musttail` (with the pop-caller-roots-before-tail discipline) is adopted with the deferred direct
  known-arity fast path. (This refines [0060](0060-native-codegen-llvm-owned-runtime.md)'s `musttail`-TCE
  to "trampoline in v1, `musttail` with the fast path".)

**Managed pointers use address space 0** in v1: `addrspace(1)` is a `gc.statepoint`/stack-map-era tool that
does no work under a shadow stack, so it is deferred together with the statepoint migration.

### 5. Local GC — Cheney copying

Semi-space copying: bump allocation; on overflow, collect (a safepoint). The collection is
**layout-directed** — for each live object it reads the header `kind` to scan only its value slots (§2),
skipping raw words; a value slot with LSB = 0 is a pointer to evacuate, LSB = 1 an immediate to leave.
Forwarding per §2. Roots come from the shadow stack (§4). The **shared arena** (S1: CAF-constant graphs +
interned strings) is **append-only / not collected in v1** — bounded in a single-capability run
([0061](0061-capability-local-shared-immutable-gc.md) §4). **No write barrier** in v1: with no
cross-capability sends there is no partition-threatening store (the barrier + remembered set arrive with
generational collection in v2).

### 6. Effect and foreign leaves

`Effect` is a thunk ([0023](0023-effect-runtime-oracle.md)); `run_effect` applies `main` to unit;
synchronous `Effect` runs direct-style. `Purvasm.*` primitives lower to native operations or runtime
calls; native leaves (`Console.log`, `show`, `purvasm-system`/`purvasm-fs`) are `extern "C"` functions in
the Rust runtime ([0059](0059-native-abi-value-representation.md) §5), **kept at parity with `boot`'s
`Ffi.host` by the differential**.

### 7. Validation — the differential oracle, extended to native

The native binary is held to **value + `Effect`-order parity** with the CESK oracle / VM / `boot` on every
fixture and benchmark — the standing oracle discipline, now a fourth implementation. Because a use-after-move
(a §4 root-reload violation) is only observable when a GC actually fires, fixtures that force collection
(allocation-heavy) are part of the suite, not left to chance.

### Deferred (to v2 / later)

Everything cross-capability (scheduler, work-stealing, wake tokens, promotion, `AVar`, cancellation CAS,
`io_uring`, blocking handoff, shared collection); **reduction-count preemption + back-edge safepoints**;
**`musttail`** (with the pop-caller-roots discipline) and the **direct known-arity fast path**;
**`addrspace(1)` + `gc.statepoint`**; the **write barrier + generational** collection; representation
analysis / further unboxing (`Array Int`, data-field unboxing); a non-moving pinned / large-object region;
and `Number` NaN-boxing.

## Consequences

- A **tractable, implementable first cut**: LLVM codegen + local Cheney GC + `Effect`, single-threaded, at
  `boot` parity — without the hard shared side (which lands in v2 on this substrate).
- The moving-GC contract is now **explicit where it silently bites**: layout-directed scan, forwarding
  invariant, and root-reload-after-safepoint close the use-after-move / broken-TCO gaps that the
  differential would only catch by luck.
- The **63-bit-immediate choice unboxes `Int`/`Char`/`Boolean` for free**, removing the boxing cost
  [sidenotes/0008](sidenotes/0008-self-compile-profiling.md) measured, and refining
  [0059](0059-native-abi-value-representation.md)'s WASM-inherited "`Int` boxed".
- **Correctness parity, not performance parity, is the v1 goal:** immediates recover the boxing cost, but
  every call goes through the generic `apply` (the `app_589` cost, ~16.8% in
  [sidenotes/0008](sidenotes/0008-self-compile-profiling.md)) and TCO is a trampoline, so v1 may run
  **at or below `boot`** (the trampoline + all-`apply` path can widen the gap beyond "slightly") until the
  direct fast path and `musttail` land.
- The v1 ABI is **representation-transparent to the differential** (immediate-vs-boxed, trampoline-vs-`musttail`
  are observationally invisible), so this layout can evolve (unboxing, generational, `musttail`) without
  changing observable behaviour.

## Alternatives considered

- **31-bit immediates / boxed `Int`** (0059 as written). Rejected for native: a WASM artifact; 63-bit
  immediates unbox `Int` for free.
- **Multicapability v1.** Rejected: pulls the entire hard cross-capability side into the first cut;
  single-capability-first reaches a running, boot-parity binary far faster.
- **`musttail` for generic `apply` in v1** (a fixed `apply_n` family, or an args-array single-signature
  `apply`, to satisfy `musttail`). Deferred in favour of a **trampoline** — simplest correct TCE for v1,
  and it avoids both the `musttail` signature constraint and the shadow-stack-pop discipline; `musttail`
  returns with the known-arity fast path.
- **Blanket back-edge safepoints.** Rejected: v1 has no preemption, so a safepoint is only needed where GC
  can occur (alloc/calls); blanket back-edge polls would implicitly re-introduce the O(n) the byte builder
  ([0052](0052-native-unsafesetbyte-in-place.md)) removed.
- **Representation-analysis unboxing / direct known-arity calls in v1.** Deferred: 63-bit immediates
  already unbox the high-value scalars; the rest are performance levers to pull once correctness holds.
