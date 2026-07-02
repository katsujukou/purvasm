# 0072. ANF → LLVM lowering: per-module textual IR, lambda-lifting, rooting emission, and the native differential

- Status: ~~Proposed~~ **Accepted** _(2026-07-02: accepted by the maintainer)_
- Date: 2026-07-02

> **Revision (2026-07-02, pre-acceptance review response):** §3 corrected — `Link.free_vars` returns a
> dependency *set*, not an order; CAF ordering now follows the **top-level ANF spine (declaration) order**
> with a `gdef`-style `Gfun`/`Gcaf`/`Grec` classification (mirroring the VM's `codegen.ml`), topo-sorting
> the **module** DAG for cross-module init. **Recursive CAF groups are intra-module only** — PureScript
> forbids cyclic imports ([0016](0016-cross-module-linking.md)), so the module graph is a DAG and there
> are no cross-module recursive value groups. §5 pins **`CArray []` → the empty-array sentinel**
> ([0071](0071-codegen-runtime-c-abi.md) §6).

## Context

[0071](0071-codegen-runtime-c-abi.md) fixed the `extern "C"` boundary between generated code and the v1
runtime — the surface, the real-address `code` word, the trampoline tail-call protocol, the exposed
rooting API, and the value/panic contract. This ADR fixes the **ANF → LLVM lowering** that emits code
against that boundary: what IR each ANF construct produces, how modules are compiled and linked, and how
the result is validated.

[0060](0060-native-codegen-llvm-owned-runtime.md) §4 set the direction — **per-module** CoreFn → CESK →
ANF → LLVM IR → object, linked with the runtime, consistent with separate compilation
([0033](0033-separate-compilation.md)) — and lowering **from ANF**, reusing the optimiser output as
`codegen_ml` does. The maintainer confirmed **per-module from the start** (not whole-program-first).

The structural reference is `boot/lib/ocaml_backend/codegen_ml.ml` (ANF → OCaml,
[0036](0036-anf-to-ocaml-value-representation.md)): the same ANF ([0025](0025-lower-ir-anf.md)) drives
both, and its node-by-node emission, its friendly-binder decision-tree vs CPS-cascade matcher, and its
`foreign` leaf set port almost directly. The load-bearing **differences** from `codegen_ml` — LLVM has no
nested functions (so lambda-lifting is required), the collector moves (so rooting must be *emitted*), and
compilation is per-module (so cross-module references and CAF init order become linker concerns) — are
what this ADR settles.

## Decision

### 1. Textual `.ll`, per module, linked with the runtime `staticlib`

Codegen emits **textual LLVM IR** (`.ll`), not via the LLVM C++ API or a binding: each module lowers
CoreFn → CESK → ANF (with the [0034](0034-effect-analysis-impurification.md)/`Simplify`/`Dbe` passes) →
one `.ll` → `llc`/`clang` → `.o`; the native linker (`lld`, in the flake) links all `.o` + the runtime
`staticlib` ([0071](0071-codegen-runtime-c-abi.md) §1) + a small entry stub (§8). Textual IR is chosen
for the **lowest toolchain coupling** (no LLVM-library link dependency in the compiler), because it
**mirrors `codegen_ml`'s text emission**, and because it is **portable into the Level-2/3 self-host**
([0037](0037-self-hosting-purescript.md)) — a PureScript compiler can emit IR text but cannot call a C++
API. This resolves [0060](0060-native-codegen-llvm-owned-runtime.md)'s deferred *"LLVM integration
mechanics (C++ API vs emitted textual IR vs a binding)"*.

Codegen lives in a boot OCaml module (`boot/lib/llvm_backend/codegen_llvm.ml`), a sibling of
`codegen_ml.ml`; the self-host port is later ([0037](0037-self-hosting-purescript.md)). The consumed
textual-IR dialect targets a **pinned LLVM version** (the flake pins `llvmPackages_NN` when a version is
chosen — a follow-up, [0060](0060-native-codegen-llvm-owned-runtime.md) §Deferred).

### 2. Top-level bindings = module-qualified global symbols; cross-module refs = `extern`

A module's top-level binding `M.x` emits a **global symbol** (its function symbol for a lambda RHS, its
CAF global otherwise — §3). A reference to another module's `N.y` is an **`external` symbol** the linker
resolves — the [0033](0033-separate-compilation.md) qualified-key scheme, faithful to `lower.ml`'s
`qualified_key`. Symbol names use a **total mangling** of the qualified key to LLVM's symbol charset
(mirroring `codegen_ml`'s `mangle`, retargeted to `.ll` global names). The cross-module calling
convention is **boxed** ([0059](0059-native-abi-value-representation.md) §3 / [0071](0071-codegen-runtime-c-abi.md)):
every cross-module value is a plain tagged word, so no `.pmi` rep signature is needed for v1.

### 3. CAF representation and cross-module initialisation order

Top-level CAFs are strict ([0070](0070-v1-byneed-recursive-caf-force.md): non-recursive CAFs are eager),
but a module's CAF may read another module's CAF, so initialisation must respect dependency order. This
mirrors the VM's global classification (`codegen.ml`'s `gdef_of_expr`): the top-level ANF spine
(`Let`/`LetRec`) *is* the module's **declaration order** — folded in CoreFn dependency order by
`lower.ml` — and each binding classifies by its RHS into a **`Gfun`** (a syntactic lambda → a function
symbol, §2/§4), a strict **`Gcaf`** (any other non-recursive top-level value, evaluated eagerly), or a
**`Grec`** member (a recursive-group value → a by-need `ByNeed` global, below).

- **Within a module**, an init function `pv_init_M(ctx)` evaluates the `Gcaf`s **in spine order** into
  their globals. (`Link.free_vars` supplies the dependency *edges* if a finer intra-module reorder is
  ever needed; it is a *set*, not itself an order — the spine order is the order.)
- **Across modules**, the entry stub (§8) calls the `pv_init_M`s in **topological module order**, reusing
  `Link.topo_sort` (already computed for the VM path). This is well-defined because **PureScript forbids
  cyclic module imports, so the module graph is a DAG** ([0016](0016-cross-module-linking.md)) — a
  cross-module reference always points "down" the DAG to an already-initialised module.

A **recursive CAF group is therefore intra-module only** (a single module's binding group = one
`A.LetRec`): the import DAG rules out cross-module value cycles, so v1 has **no cross-module `Grec`**. A
`Grec` member is a **by-need `ByNeed` global** ([0070](0070-v1-byneed-recursive-caf-force.md) §4), forced
on first reference — the knot tied by the runtime's Grec builder, no static order over the cycle required.
This closes the known *"gdef ordering vs boot on multi-module closures"* gap: **acyclic** (the common and,
cross-module, the *only*) case uses topological eager init; a **module-local** recursive group uses
by-need. (A `LetRec` node lowers to the Grec construction whether its members are local `let`-bindings or
a module's top-level group.)

### 4. Lambda-lifting is required (LLVM has no nested functions)

`codegen_ml` emits nested `VClos (fun … )` and rides OCaml's closures; LLVM has none. So a **closure
conversion / lambda-lift** step hoists every `CLam` to a **top-level `extern "C"` `CodeFn`**
([0071](0071-codegen-runtime-c-abi.md) §3) whose free variables are read positionally from a captured
**env `Array`** ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2 Closure shape). At
the `CLam` site codegen computes the free-var set (reusing `Link.free_vars`), builds the env with
`pv_new_array`, and emits `pv_make_closure(code_addr, arity, env)`. A no-capture lambda gets the immediate
env sentinel ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2). Mutually-recursive
function groups share **one** env block, knot-tied by back-patching
([0059](0059-native-abi-value-representation.md) §1) — the §3 Grec path for the value members.

### 5. Per-ANF-node lowering (against the [0071](0071-codegen-runtime-c-abi.md) surface)

Each ANF form ([0025](0025-lower-ir-anf.md)/`anf.ml`) lowers to IR calling the C-ABI, structurally
mirroring `codegen_ml`:

- **Atoms** — `ALit` `Int`/`Boolean`/`Char` → an **inline immediate constant** (tag bits in IR, no call —
  [0064](0064-v1-single-capability-native-abi-codegen-contract.md) §1); `ALit` `Number`/`String` →
  `pv_new_number` / `pv_new_str` (allocated once, hoisted to a CAF where possible); `AVar` → an SSA
  value, a global load (§2), or an env-field read (§4); `AForeign` → `pv_foreign` (§9).
- **`CApp`** — tail vs non-tail per the [0071](0071-codegen-runtime-c-abi.md) §4 trampoline: a tail call
  emits `pv_tailcall` + return; a non-tail call emits `pv_apply`.
- **`CPrim`** — `pv_prim_*` ([0071](0071-codegen-runtime-c-abi.md) §6); scalar ops don't allocate,
  structural ones (`Append`, array, record) do (→ safepoints, §6).
- **`CCtor`** — a nullary ctor → an immediate ([0064](0064-v1-single-capability-native-abi-codegen-contract.md)
  §1); a field-carrying ctor → `pv_new_adt(tag, fields)`; a still-partial ctor application accumulates
  (as `codegen_ml`'s `VCtor`) until saturated.
- **`CArray` / `CRecord` / `CAccessor` / `CUpdate`** — a **non-empty** `CArray` → `pv_new_array`; an
  **empty `CArray []` → `pv_empty_array`** (the runtime `new_array` rejects a zero-length array; the empty
  array is an immediate sentinel — [0071](0071-codegen-runtime-c-abi.md) §6), and the array primops
  (`LengthArray`/`IndexArray`/`SetArray`/`Append`) recognise the sentinel. `CRecord`/`CAccessor`/`CUpdate`
  → `pv_new_record` and the record ops ([0069](0069-v1-dynamic-record-operations.md)). Record labels are
  **hashed and sorted at compile time** (`fnv1a_64`, matching the runtime —
  [0069](0069-v1-dynamic-record-operations.md) §2), so a literal lowers to one `pv_new_record` with
  compiler-sorted ids and static field access uses the compiler-emitted id.
- **`CIf`** — branch on the unboxed `Boolean` immediate (no call).
- **`CCase`** — the `codegen_ml` matcher, ported: friendly binders → a **decision tree** (tag `switch` via
  the ADT tag / immediate compare / positional field reads / array-length check / record-id lookup);
  a record-bearing (unfriendly) case → the **CPS cascade** with a shared fail continuation. Nullary vs
  field-carrying ctor discrimination follows [0064](0064-v1-single-capability-native-abi-codegen-contract.md)
  §1 (immediate-vs-pointer first, then the header tag).
- **`Ret` / `Let` / `LetRec`** — straight-line SSA; `LetRec` → the §3/§4 Grec construction.

### 6. Shadow-stack rooting emission (consuming [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)/[0071](0071-codegen-runtime-c-abi.md) §5)

Per generated function, codegen emits `pv_frame` at entry and `pv_pop_frame` at every return, and — for
each value **live across a safepoint** (any `CApp` / `pv_force` / allocating primop / constructor) —
`pv_root` before the safepoint and a `pv_get` **reload** after ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)
§3: *"a `Value` that must survive a safepoint is rooted before it and re-read via `get` after it"*).
Because ANF is let-normal, *"live across a safepoint"* is a straightforward liveness read over the
let-sequence: a binding referenced after a later binding whose RHS may allocate. **v1 is conservative** —
it roots every such live binding (immediates may be rooted harmlessly; the collector passes them
through), correctness over minimality, refined later
([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) *Alternatives* warns against rooting *everything*
unconditionally, so the discipline is "across a safepoint", not "on create"). This is exactly the
push/`get`-reload/pop shape the runtime's own constructors and `apply` already emit — one realisation, not
two.

### 7. Primops as runtime helpers; inline IR deferred

Per [0071](0071-codegen-runtime-c-abi.md) §6, v1 emits `pv_prim_*` calls — **one tested source of truth**
for 32-bit wrapping, Euclidean div/mod, and ECMAScript `ToInt32`/`Math.round`
([0041](0041-int-number-conversion-primops.md)/[0042](0042-number-math-primops.md)). Inlining
scalar-immediate arithmetic directly in IR (unbox the tag, operate, re-tag with the wrap mask) is a
**deferred perf lever** — the self-compile profile puts the floor at generic `apply` and allocation, not
primop dispatch ([sidenotes/0008](sidenotes/0008-self-compile-profiling.md)).

### 8. Entry stub and `run_effect`

A generated entry stub (`main`) `pv_runtime_new`s the context, runs the module inits in topological order
(§3), evaluates the entry global, then: a **pure** entry prints via the `to_string`/`show` leaf; an
**`Effect`** entry runs `pv_run_effect` ([0067](0067-v1-effect-execution-and-native-leaves.md) §2),
performing its effects with the `Unit` result suppressed. Output routes through the sink to `stdout`
([0067](0067-v1-effect-execution-and-native-leaves.md) §5) — mirroring `native_action`'s `is_effect`
split (`boot/bin/main.ml`).

### 9. Foreign leaves

An `AForeign k` (or an unresolved qualified `Var` the resolver never bound) lowers to `pv_foreign(k)`
([0071](0071-codegen-runtime-c-abi.md) §6), whose set mirrors `codegen_ml`'s `foreign` — `show*`,
`Purvasm.Stdio.*`, the `Purvasm.String` byte primitives, the `Data.Number` math family,
`Purvasm.FS`/`Purvasm.System`, `Partial._crashWith` — grown on demand (the minimal-FFI policy;
prefer a `ulib` shadow over a native leaf where possible — [0068](0068-purvasm-stdio-and-effect-console-ulib-shadow.md)).

### 10. Validation — the native differential, extended, plus a slice plan

A new e2e `llvm_run` mirrors `ocaml_run` (`boot/test/e2e/test_e2e.ml`): emit the module `.ll`(s) →
`clang`/`lld` link with the runtime `staticlib` → run → diff stdout / printed value against the **CESK
oracle**, the **VM**, and **boot's native-OCaml** backend — the **fourth implementation** held to value +
`Effect`-order parity ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §7).
**Forced-GC fixtures** (heaps sized to collect mid-run) exercise the §6 rooting under real evaluation
([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §6).

Landed as **slices** (as `codegen_ml` was — [0036](0036-anf-to-ocaml-value-representation.md)), each held
to the differential, per-module machinery from slice 1 (first exercised on a single module, the
degenerate per-module case):

1. **Pure first-order** — arithmetic, `apply`/PAP, the §6 rooting, the §4 trampoline, one module.
2. **ADTs + `case`** — constructors, the §5 matcher, nullary-vs-boxed discrimination.
3. **Records** — literals, `get`/`set`/`insert`/`delete`/`modify` ([0069](0069-v1-dynamic-record-operations.md)).
4. **`Effect` + `Ref` + leaves** — `run_effect`, the `Ref` ops, `pv_foreign`, stdio
   ([0067](0067-v1-effect-execution-and-native-leaves.md)).
5. **`LetRec` + by-need + cross-module** — the Grec construction, `ByNeed` globals, multi-module linking
   and topological init (§3/§4).

### Deferred

- **Cross-module rep-unboxing** (`.pmi` rep publication, [0059](0059-native-abi-value-representation.md)
  §3) — gated on measurement; v1's cross-module boundary is boxed (§2).
- **Inline-IR primops** (§7); the **direct known-arity fast path + `musttail`**
  ([0071](0071-codegen-runtime-c-abi.md) §Deferred); **`Array Int`/data-field unboxing**
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §Deferred).
- **DWARF / a symbolised profiling path** ([0060](0060-native-codegen-llvm-owned-runtime.md) §Deferred) —
  a prerequisite for the wall-4 benchmark.
- **The Level-2/3 self-host port** of `codegen_llvm` ([0037](0037-self-hosting-purescript.md)); pinning
  the LLVM textual-IR version (§1).
- **A decision-tree matcher / match-compilation reuse** beyond the ported `codegen_ml` cascade — a perf
  slice.

## Consequences

- A **per-module native path** — CoreFn module → `.ll` → `.o`, linked with the runtime — running at
  **boot parity** as the fourth differential implementation, on the
  [0071](0071-codegen-runtime-c-abi.md) ABI. The examples and the self-compiler can then run on the owned
  runtime, and the wall-4 benchmark ([0059](0059-native-abi-value-representation.md) §4) turns on.
- **Lambda-lifting and rooting emission are the genuinely new codegen work** (`codegen_ml` needed
  neither); the rest is a structural port. §6's conservative rooting is correct-first and refines to
  minimal later without changing observable behaviour
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §Consequences: the ABI is
  representation-transparent to the differential).
- **Cross-module CAF init order is settled** (§3): topological eager init over the module DAG (cyclic
  imports are forbidden, [0016](0016-cross-module-linking.md)), with by-need `ByNeed` globals for the
  *module-local* recursive groups — closing the *"gdef ordering"* gap that whole-program lowering hid.
- Textual IR keeps the **toolchain light and the codegen self-host-portable**
  ([0037](0037-self-hosting-purescript.md)), at the cost of an `.ll` text format and `llc`/`clang`/`lld`
  process invocations per build.
- v1 is **correctness parity, not performance parity**
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §Consequences): all-`apply` +
  trampoline + `pv_prim_*` calls may run at or below boot until the deferred perf levers land.

## Alternatives considered

- **Whole-program lowering first** (one `.ll` for the linked program, as `codegen_ml`/`native_action` do
  today). Fastest to a first running binary, and it hides cross-module init order — but that hiding is
  exactly the *"gdef ordering"* gap, and the maintainer chose per-module from the start to exercise the
  separate-compilation ABI ([0033](0033-separate-compilation.md)/[0060](0060-native-codegen-llvm-owned-runtime.md)
  §4) immediately. Recorded as considered and declined for v1.
- **LLVM C++ API / a binding** instead of textual IR. Tighter integration and no text round-trip, but a
  heavy library-link dependency in the compiler and **not portable into the PureScript self-host**
  ([0037](0037-self-hosting-purescript.md)), which can emit text but not call C++. Rejected (§1).
- **Inline primops in IR from the start** (§7). Deferred: re-derives tricky numeric semantics off the one
  tested runtime.
- **All top-level CAFs by-need** (uniform, sidesteps init order entirely). Simpler than the §3 split, but
  it pays a force-check per top-level reference for the acyclic majority that eager topological init
  handles for free, and [0070](0070-v1-byneed-recursive-caf-force.md) keeps non-recursive CAFs strict.
  Rejected in favour of eager-acyclic + by-need-cyclic.
- **A stack-map / `gc.statepoint` rooting emission** instead of shadow-stack calls. The eventual precise
  mechanism, but deferred with the statepoint migration
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4,
  [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §7); the shadow stack is the v1 vehicle.
