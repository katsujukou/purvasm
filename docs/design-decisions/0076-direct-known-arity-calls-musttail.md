# 0076. Direct known-arity calls and `musttail` TCE on the LLVM backend

- Status: ~~Proposed~~ **Accepted** _(2026-07-05: accepted by the maintainer)_
- Date: 2026-07-04

## Context

The wall-4 baseline ([0075](0075-cross-backend-wall-clock-benchmark-harness.md)) quantifies the v1
calling-convention tax: at equal input sizes the LLVM-native leg runs **8.9–22.6× slower than the
OCaml-native leg** on the same optimised ANF (`llvm/ml`: fib 22.6×, quicksort 18.4×, count-state
16.9×, json-parse 14.6×, map-fold-array 8.9×) — largest where call density is highest, exactly the
distribution the v1 design predicted when it priced the gap in
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) Consequences: *"every call goes
through the generic `apply` … and TCO is a trampoline, so v1 may run at or below `boot`"*).

What one saturated, statically-resolvable call costs today: marshal the arguments into an
`alloca` buffer, cross the `extern "C"` boundary into `pv_apply` (a `catch_unwind` guard frame,
[0071](0071-codegen-runtime-c-abi.md) §7), validate the callee word, read its kind and arity from
the heap, dispatch saturate/under/over, rebuild the code pointer, and make the indirect call —
per call. A tail call additionally bounces through the [0071](0071-codegen-runtime-c-abi.md) §4
trampoline (`pv_tailcall` copy into the pending slot, frame pop, return, loop re-dispatch).

The remedy is already designed, deliberately deferred:

- [0059](0059-native-abi-value-representation.md) §3: each known-arity function presents **two
  entries** — the boxed generic apply-N entry, and a **direct known-arity entry** for statically
  resolved saturated calls.
- [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4: *"`musttail` (with the
  pop-caller-roots-before-tail discipline) is adopted with the deferred direct known-arity fast
  path."*
- [0072](0072-anf-to-llvm-lowering.md) §2 pins the constraint the fast path must not break: **no
  arity (or any metadata) crosses the module boundary** — a cross-module reference is a closure
  root-handle, applied generically.

This record turns those reservations into the concrete codegen/runtime contract.

## Decision

### 1. Every generated function gets a direct entry; the generic entry becomes a wrapper

For each lifted function (a hoisted `CLam` / `Gfun` body of arity `k`,
[0072](0072-anf-to-llvm-lowering.md) §4), codegen emits:

- **the direct entry** `@<sym>$d` — an ordinary LLVM function
  `i64 @<sym>$d(ptr %ctx, i64 %env, i64 %a0, …, i64 %a{k-1})`: the body as today, arguments as
  plain parameters (all still **boxed tagged words** — this record changes the *dispatch*, not
  the representation; rep-unboxing stays deferred per [0059](0059-native-abi-value-representation.md));
- **the generic entry** `@<sym>` — the `AbiCodeFn` ([0071](0071-codegen-runtime-c-abi.md) §3)
  the closure's `code` word keeps pointing at, now a thin wrapper: unpack `args[0..k-1]`, read
  the env slot from the closure, and `musttail`-shape-permitting **tail-call the direct entry**.

Closures, PAPs, `ByNeed` forcing, `pv_apply`, the trampoline, and every *dynamic* call path are
therefore **unchanged** — a function value is still its closure, and anything not statically
resolved keeps the v1 semantics bit for bit.

### 2. Which call sites lower to the direct entry

A `CApp` lowers to a direct call `@<sym>$d(ctx, env, a0…)` iff, **within the module being
compiled**, codegen statically knows the callee's binding and the call is **exactly saturated**:

- a **let-bound lambda** of the current function/module (the lifted symbol and arity are local
  facts), including recursive self/sibling calls in a lifted function group;
- a **same-module top-level `Gfun`** (its symbol, arity, and env shape — the no-capture sentinel
  or the group's shared env — are this module's own facts; the env operand is read from its
  root-handle global's closure, or passed as the sentinel for the no-capture case).

Everything else stays generic: **cross-module references** (the [0072](0072-anf-to-llvm-lowering.md)
§2 pin — publishing arity in `.pmi` is a separate, measurement-gated record per
[0059](0059-native-abi-value-representation.md) §3), under-/over-application, callee-position
variables whose binding is not statically a function (parameters, fields, foreigns, `ByNeed`
cells), and every call made *by the runtime* (`apply`, `force`, leaves).

### 3. Tail calls: `musttail` on direct→direct edges, trampoline elsewhere

- A **tail `CApp` that qualifies for §2** and whose callee has the **same direct-entry signature
  arity** as any other direct call is emitted as
  `musttail call i64 @<sym>$d(ptr %ctx, i64 %env, …)` + `ret` — after the caller **pops its
  shadow-stack frame** (`pv_pop_frame` *before* the tail call, the
  [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4 discipline: the callee opens
  its own frame; leaving the caller's frame open would leak roots on every iteration). LLVM
  guarantees the jump for matching signatures on the supported 64-bit targets.
  > **Correction (2026-07-05, from the 0077 review):** the "matching signatures" phrasing here
  > understates what shipped. `tailcc` is exempt from `musttail`'s prototype-matching
  > requirement (LLVM LangRef), so the implementation emits `musttail` on **every**
  > direct→direct tail edge — mismatched arities included — and the standing differential
  > exercises such edges. Matching arity is neither required nor checked.
  Arguments must be computed (and any needed `pv_get` reloads done) **before** the pop; between
  the pop and the `musttail` no safepoint occurs (argument words stay valid).
- A **tail call that does not qualify** — anything not statically direct per §2: a generic
  callee, or a cross-module reference (until the `.pmi`-publication record) — keeps the
  [0071](0071-codegen-runtime-c-abi.md) §4 trampoline unchanged.
- The generic wrapper's call into its own direct entry is a tail call by construction, so a
  dynamic call costs one extra jump, not a second frame.

### 4. Rooting and the differential are unchanged in contract

The direct entry opens/pops its own `pv_frame` exactly as generated bodies do today
([0072](0072-anf-to-llvm-lowering.md) §6); root-reload-after-safepoint is untouched. The change is
**representation-transparent to the differential**
([0064](0064-v1-single-capability-native-abi-codegen-contract.md) Consequences): same values, same
`Effect` order, same `.pmo`/`.pmi` (codegen-side only — no artifact change, no `format_version`
bump). Validation: the full boot e2e differential (including the forced-GC fixtures — a
fast-path-reached body under a mid-call collection is the new risk surface), the examples sweep
7/7, the 488/488 self-compile byte-identity, and a before/after
[0075](0075-cross-backend-wall-clock-benchmark-harness.md) run recorded in this record's Progress
note (the `llvm/ml` table is the success metric).

### Deferred (explicitly, unchanged from the parent records)

Cross-module direct calls via `.pmi` arity/rep publication
([0059](0059-native-abi-value-representation.md) §3 — activates the
[0033](0033-separate-compilation.md) cascade; measurement-gated); representation unboxing of the
direct entry's parameters; inline-IR scalar primops ([0072](0072-anf-to-llvm-lowering.md) §7 — the
next lever after this one); GER/impurification on native
([0067](0067-v1-effect-execution-and-native-leaves.md) §7).

## Consequences

- The dominant v1 tax — generic dispatch on calls that were statically resolved all along — is
  removed for module-local saturated calls, and tail recursion becomes a real jump. The
  [0075](0075-cross-backend-wall-clock-benchmark-harness.md) `llvm/ml` gap is the measured target;
  call-dense workloads (fib 22.6×, count-state 16.9×) should move the most.
- LLVM can finally optimise across call boundaries (inlining/const-prop into direct entries) —
  unavailable through an indirect `pv_apply`.
- Cross-module calls (the Prelude combinator traffic) remain generic until the `.pmi`-publication
  record lands — this increment's ceiling is real and is the datum that record's
  measurement gate asks for.
- Cost: two entries per lifted function (code size; the wrapper is tiny and the system linker
  dead-strips unreferenced ones), and a second call-lowering path in codegen to keep correct —
  covered by the standing differential rather than new test machinery.

## Alternatives considered

- **Cross-module direct calls now** (arity in `.pmi`, or exploiting the driver's whole-spine
  view). Rejected for this increment: baking neighbour arities into a module's `.o` silently
  breaks the separate-compilation contract ([0033](0033-separate-compilation.md) /
  [0072](0072-anf-to-llvm-lowering.md) §2) the moment per-module recompilation is real; and
  [0059](0059-native-abi-value-representation.md) §3 already gates that door on measurement this
  increment will produce.
- **`musttail` for the generic path too** (a fixed `apply_n` family). Already rejected in
  [0064](0064-v1-single-capability-native-abi-codegen-contract.md) Alternatives; the trampoline
  stays for dynamic calls.
- **One entry with a fast-path check inside `pv_apply`** (peek "saturated closure call" and jump).
  Saves codegen work but keeps the boundary crossing, the guard frame, and the argument buffer on
  every call — the very costs being removed; and LLVM still cannot see through it.
- **Skip the wrapper: keep `AbiCodeFn` bodies and add direct entries only where called.** Emitting
  the direct entry unconditionally with the generic wrapper on top is simpler, uniform, and the
  dead-strip removes unused wrappers; per-call-site duplication decisions buy nothing.
- **Direct-call the saturated *prefix* of an over-application** (`f a b c` with known arity 1 →
  `let r = @f$d(…, a) in pv_apply(r, [b, c])`). The remainder is irreducibly generic — `r`'s
  arity is a runtime fact in the erased world — so one `pv_apply` boundary survives regardless;
  the saving is only the prefix's dispatch, a second-order gain. Against it: the leftover
  arguments must now be rooted across the prefix call by *codegen* (today `pv_apply`'s
  continuation stack owns that, [0071](0071-codegen-runtime-c-abi.md) §4), and the tail-position
  case re-opens the trampoline/`conts` composition. Over-application is also rare in the
  uncurried eval/apply ANF ([0025](0025-lower-ir-anf.md)). Deferred as a purely additive
  extension if a post-landing profile shows it hot; under-application likewise stays generic
  (the PAP build *is* the work — there is nothing to call directly).

> **Progress (2026-07-05):** Implemented, with two findings folded in during bring-up:
>
> - **The trampoline stash needs a settle at direct call sites.** A directly-entered body's
>   generic tail call stashes `(f, args)` ([0071](0071-codegen-runtime-c-abi.md) §4) — and a
>   direct call has no enclosing `pv_apply` loop to take the stash (the tailapp differential
>   returned the dummy). Every non-`musttail` direct call site now emits **`pv_settle(ctx, r)`**
>   (a new codegen-internal runtime entry): a stashed tail is run flat to a real value via
>   `apply`, a real value passes through; `musttail` edges propagate to their caller's settle;
>   wrappers never settle (under the loop the stash belongs to the loop, as before).
> - **The self-call env word must be reloaded.** `%env` is an SSA parameter — stale after the
>   first safepoint in the body (the forced-GC differential caught it). The direct entry roots
>   its env word on entry when a self-call is possible; a self-call re-reads it via `pv_get`.
>
> Validation: boot e2e 213/213 (forced-GC fixtures included), examples sweep 7/7, self-compile
> **488/488 byte-identical**. Measured ([0075](0075-cross-backend-wall-clock-benchmark-harness.md),
> equal-`n` `llvm/ml`): fib 22.6× → **13.9×**, quicksort 18.4× → **10.8×**, count-state 16.9× →
> **10.9×**, json-parse 14.6× → **10.1×**, map-fold-array 8.9× → **5.7×** (≈1.4–1.7× per
> workload); full self-compile 2 m 44 s → **1 m 23 s** (1.97×). Scaling exponents unchanged
> (0.92–1.07). The remaining gap is the ceiling named above — cross-module calls (the Prelude
> combinator traffic) still generic pending the `.pmi`-publication record — plus the
> `pv_prim_*` boundaries ([0072](0072-anf-to-llvm-lowering.md) §7) and conservative rooting
> ([0072](0072-anf-to-llvm-lowering.md) §6), in that expected order.
