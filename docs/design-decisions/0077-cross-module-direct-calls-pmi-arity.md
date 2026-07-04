# 0077. Cross-module direct calls: consume the `.pmi` export surface, and distinguish recursive function members (`Erecfn`)

- Status: ~~Proposed~~ **Accepted** _(2026-07-05: accepted by the maintainer after review round 2)_
- Date: 2026-07-05

> **Revision (2026-07-05, maintainer review round 1):** Two P1 findings folded. (1) The record's
> premise was wrong: recursive syntactic-lambda members are classified `Gfun` (`gdef_of_expr
> ~recursive:true` still yields `Gfun`) and so publish as `Efn n` today — the hidden fact is
> **recursiveness**, not arity, and `Artifact.group` does not retain rec-membership (a singleton
> self-recursive function is indistinguishable), so §1 now pins `group.recursive : bool` carried
> from the bind site. The earlier "arity hash hole" claim was retracted as incorrect. (2) The
> cross-module `musttail` paragraph was widened from "nothing new" to the precise contract:
> `tailcc` exempts `musttail` from prototype matching (mismatched-arity edges already ship
> module-locally; re-verified against external declares with `llc`, arities 2→1 and 1→10), and
> the genuinely new obligation is declare accuracy, guarded by §4. P2: `.pvmi` JSON pinned as
> structured `["recfn", n]`.

## Context

[0076](0076-direct-known-arity-calls-musttail.md) removed the generic-dispatch tax for
module-local saturated calls and measured its own ceiling: `llvm/ml` at equal `n` is still
**5.7–13.9×**, and the named remainder is **cross-module calls — the Prelude combinator
traffic — still generic**. That is the measurement [0059](0059-native-abi-value-representation.md)
§3 gated its reserved follow-up on: *per-export signatures published in `.pmi`, callee-determined,
dependency-directed*. This record takes the **arity** slice of that reservation (representation
unboxing stays deferred) and supersedes the corresponding part of
[0072](0072-anf-to-llvm-lowering.md) §2's "no metadata crosses the boundary" pin.

Two facts found while scoping:

- **The `.pmi` already publishes most of what is needed.** ADR-[0033](0033-separate-compilation.md)'s
  interface carries per-export `Efn arity | Ecaf | Erec`, and its content hash covers name *and*
  kind — so for non-recursive top-level functions the arity is already published, and the
  recompilation cascade for an arity change is already armed.
- **The gap is *recursive* functions — and it is recursiveness, not arity, that is hidden.**
  Classification (`gdef_of_expr`, `boot/lib/vm/codegen.ml`) makes any syntactic-lambda RHS a
  `Gfun` *including recursive-group members* (in the VM a recursive function is closed by the
  global table; `Grec`'s by-need cell serves only value members). A top-level recursive
  function is therefore published as plain `Efn n` today — its arity is already in the hash —
  but its **recursiveness is invisible**, and that is exactly the fact a native cross-module
  caller needs: the LLVM backend gives recursive-group function members a group env behind a
  by-need cell ([0076](0076-direct-known-arity-calls-musttail.md)'s `SForceCell`), while a
  non-recursive `Gfun` is a no-capture closure called with the env sentinel. Consuming `Efn`
  blindly would pass the sentinel to a function that needs its group env — a miscompile, not a
  missed optimisation. Compounding it, `Artifact.group` does not retain whether it came from a
  recursive bind — a single self-recursive function is a one-member group indistinguishable
  from a plain one — so the fact must be **carried** to interface generation, not re-derived
  from the artifact.

Pipeline reality, stated honestly: the LLVM driver today lowers the **whole linked spine**
(`Link.link_program` → `program_split`) and never reads `.pmi` files; the bytecode path is what
writes them. The design below therefore fixes the **interface as the contract**: what a module's
codegen may know about a dependency is *exactly* that dependency's `.pmi` export surface — today
the driver derives it from the spine by the same classification; the future artifact-driven
driver (and the Level-2/3 self-host) reads it from the file, with no behavioural change.

## Decision

### 1. Interface: recursive function members publish as `Erecfn`, and a `format_version` bump

The centre of this record, stated precisely: **a recursive-group function member is published
as `Erecfn of int` instead of `Efn of int`**, so a native consumer can tell the force-cell
direct path from the sentinel direct path. `Efn n` now *means* "non-recursive top-level
function of arity `n`"; `Erec` remains for a group's value members.

Because rec-group membership is not derivable from the artifact (the singleton self-recursive
case above), it is carried explicitly:

- `group` gains **`recursive : bool`**, set where the bind's recursiveness is a local fact —
  boot threads the existing `gdef_of_expr ~recursive` argument into group construction; the
  Level-2 artifact writer mirrors it — and serialized in the `.pvmo` group object, so the
  interface stays derivable from the object alone.
- `kind_of_gdef` becomes group-aware: a `Gfun` member of a `recursive` group → `Erecfn n`.
- Encodings, pinned: `.pvmi` JSON **`["recfn", n]`** (mirroring the structured `["fn", n]`,
  not a fused string); hash tag **`recfn<N>`**.

Recursiveness thereby enters the interface hash. Today that omission is honest — the bytecode
linker treats a recursive `Gfun` like any other, so no dependent can observe the difference —
but §2 makes it observable, so it must move the hash from the same version.

This is an artifact-format change: **`format_version` 2 → 3**, per the
[0033](0033-separate-compilation.md) convention. Boot's `Pvm.Artifact` and the Level-2 compiler's
artifact writer change **in lockstep**, the Lv1↔Lv3 byte-identity fixtures are regenerated at the
new version, and stale objects are rejected (not silently reused) by the standing version check.

### 2. Codegen: a dependency's export surface feeds cross-module direct calls

Per dependency module, codegen receives the export surface `(key → Efn n | Erecfn n | …)` and
extends [0076](0076-direct-known-arity-calls-musttail.md) §2's resolution — same-module facts
still win first:

- **`Efn n`**, call saturated → direct call `@<mangle key>$d(ctx, sentinel, a0…)`. A top-level
  `Gfun` is a no-capture closure, so the env operand is the immediate sentinel and the callee's
  `$root` global is **not read at all** — a cross-module direct call to a plain function is a
  bare symbol call.
- **`Erecfn n`**, call saturated → read the callee's `$root` cell, `pv_force_if_byneed`, read the
  forced closure's env slot, then direct call ([0076](0076-direct-known-arity-calls-musttail.md)'s
  `SForceCell`, now cross-module). Initialisation order is unchanged (the reference is in the
  ANF, so reachability init covers the callee exactly as the generic path did).
- **`Ecaf` / `Erec` / non-exports / unsaturated** → generic, as today.

`musttail` on cross-module direct→direct tail edges — stated precisely rather than waved at:
the implemented [0076](0076-direct-known-arity-calls-musttail.md) §3 behaviour is `musttail` on
*every* direct→direct tail edge, **arity match not required**. That is legal because `tailcc`
is exempt from `musttail`'s prototype-matching requirement (LLVM LangRef — the exemption is why
[0076](0076-direct-known-arity-calls-musttail.md) chose `tailcc`), and mismatched-arity edges
already ship module-locally under the standing differential. Verified additionally against
**external declares** in both directions with the toolchain's `llc` (arity 2→1, and 1→10 — the
stack-argument-growth case). What *is* new cross-module is the **declare-accuracy obligation**:
the referencing module's `declare tailcc` must state the interface-exact signature — a stale
declare is an ABI break, and guarding it is precisely §4's job. Correctness never rests on
`musttail`: an edge that any future toolchain rejected would fall back to the generic
trampoline, losing only the guaranteed jump. The `pv_settle` obligation at non-`musttail`
direct sites applies unchanged.

### 3. Symbols: a top-level binding's direct entry becomes external

`@<mangle key>$d` for top-level bindings (`Gfun` code functions and pre-lifted `Grec` function
members) drops `internal` linkage; the injective mangle ([0072](0072-anf-to-llvm-lowering.md) §2)
already guarantees cross-`.o` uniqueness. A referencing module emits a per-signature
`declare tailcc i64 @<mangle key>$d(ptr, i64, …)`. Lambda-lifted internals (`fn_*`, `susp_*`,
local `recfn_*`) and all wrappers stay `internal`.

### 4. The staleness contract

A caller's `.o` now bakes in its dependencies' exported **call facts — the arity *and* the
`Efn`-vs-`Erecfn` kind**, i.e. whether the callee is entered with the sentinel or through the
force-cell path. The guard is exactly
[0033](0033-separate-compilation.md)'s dormant `.pmi`-hash cascade — armed for this metadata
because the hash covers `Efn`/`Erecfn` — plus, today, the fact that the whole-spine driver
rebuilds every module per invocation (no live hazard until incremental per-`.o` reuse lands; when
it does, the cascade is the mechanism this record presumes, not new machinery).

### Deferred

Representation (rep-signature) publication and cross-module unboxed entries —
[0059](0059-native-abi-value-representation.md) §3's larger half, still gated on measurement
after this record; incremental per-`.o` native builds (the artifact-driven driver that reads
`.pmi` from disk); saturated-prefix over-application ([0076](0076-direct-known-arity-calls-musttail.md)
Alternatives).

## Consequences

- The Prelude/combinator traffic — the measured remainder of the v1 call tax — becomes
  direct-callable; success is the before/after
  [0075](0075-cross-backend-wall-clock-benchmark-harness.md) `llvm/ml` table in this record's
  Progress note.
- The **interface becomes the honest contract** for cross-module knowledge (spine-derived today,
  file-read tomorrow), and recursiveness enters the hash in the same version that makes it
  downstream-observable — the interface-completeness invariant of
  [0033](0033-separate-compilation.md) is preserved, not patched after the fact.
- One-time cost of the `format_version` bump: lockstep boot + Level-2 artifact writers,
  regenerated byte-identity fixtures, invalidated stale objects (rejected cleanly by the version
  check).
- Cross-module `Gfun` calls skip even the `$root` load — cheaper than the generic path by the
  whole dispatch *and* the global read.

## Alternatives considered

- **Whole-spine call-fact table without touching `.pmi`.** Works for today's driver, but
  perpetuates the fiction [0072](0072-anf-to-llvm-lowering.md) §2 was guarding against
  (knowledge a real per-artifact build would not have), leaves the call-kind fact —
  recursiveness, i.e. sentinel vs force-cell path — outside the interface hash while dependents
  observably rely on it, and would have to be redone the moment the artifact-driven driver
  lands. Rejected: publish the contract now.
- **Consume `Efn` only (no format change).** Not merely a smaller increment — **unsound**:
  today's `Efn` conflates recursive function members (which need their group env) with
  no-capture functions (called with the sentinel), so blind consumption miscompiles exactly
  the `map`-/`foldl`-shaped callees this record is for. Any safe variant needs the
  recursiveness bit, i.e. the format change. Rejected.
- **Publish representation signatures now too.** [0059](0059-native-abi-value-representation.md)
  §3 gates that on measurement *after* boxed cross-module direct calls exist; arity is the cheap,
  self-contained slice. Deferred, unchanged.
- **`dlsym`-style runtime symbol resolution instead of interface metadata.** Replaces a
  compile-time fact with a runtime lookup and keeps the arity question open at every call.
  Rejected.

> **Progress (2026-07-05):** Implemented as revised, plus a post-acceptance review round:
> the driver now derives the surface **from the same code that writes the `.pmi`**
> (`Pvm.Compile.compile_module` → `Artifact.interface_of` → `Efn`/`Erecfn` → codegen
> `call_fact`), and codegen direct-calls a key only where the natively-lowered shape *agrees*
> with the published fact (same kind and arity) — a disagreement falls back to generic instead
> of baking a wrong ABI, and the exported-`$d` emission keys off the same intersection so every
> `declare` resolves. The Level-2 writer gained byte-identity regression tests for
> `recursive:true` / `["recfn", n]` against boot's output.
>
> Validation: boot e2e **216/216** (three new tests: two real multi-module differentials run
> with the surface supplied, and a structural pin that a neighbour's `$d` is declared
> `tailcc` and an exported `$d` is external); examples sweep 7/7; self-compile **488/488
> byte-identical** at `format_version` 3 against a fresh boot reference. Cross-module direct
> calls are live: the bench programs each declare **22–53** external `$d` symbols.
>
> **Measured result — honest and negative on wall-clock.** Paired, interleaved runs
> (alternating before/after binaries on the same machine, min of 5; before = the same
> toolchain with an empty surface): fib **1.02×**, quicksort **1.06×**, count-state **1.03×**,
> json-parse **1.04×**, map-fold-array **1.01×**; the full 244-module self-compile is **flat**
> (75.7 s vs 76.8 s / 76.5 s vs 77.4 s — if anything ~1% slower, within noise). Two
> measurement lessons are recorded with it: session-to-session comparisons (an earlier
> "1 m 23 s → 1 m 15 s" reading) and solo-vs-full-sweep runs (one-off ~1.6× readings) were
> machine-condition artifacts — only interleaved paired runs settled it; and the bench
> harness's per-leg cache silently reused executables of the *previous* toolchain until it was
> keyed on the `purvm` binary's mtime (fixed in `run-benchmarks.sh`).
>
> **Interpretation.** [0076](0076-direct-known-arity-calls-musttail.md)'s ceiling attribution
> — "the remainder is the cross-module Prelude combinator traffic" — is **refuted at v1's
> cost structure for this corpus**: the hot loops were already module-local direct after 0076,
> and the cross-module sites this record converts sit on cold paths. The residual `llvm/ml`
> gap is dominated by per-operation costs a call-path change cannot touch: the `pv_prim_*`
> extern boundaries ([0072](0072-anf-to-llvm-lowering.md) §7), conservative root/reload
> traffic ([0072](0072-anf-to-llvm-lowering.md) §6), and the fact that the generated `.ll` is
> compiled `-O0` today. Those are the next levers, now with a measurement pointing squarely at
> them. This record's standing value is the **contract**: recursiveness is interface-visible
> and hash-guarded, the external `$d` symbol ABI exists, and the interface is the proven
> carrier that representation publication ([0059](0059-native-abi-value-representation.md) §3)
> rides on.
