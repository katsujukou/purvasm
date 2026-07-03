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
>
> **Revision (2026-07-02, post-acceptance — CAF-init strategy):** §3 replaces per-module `pv_init_M`
> functions with a **link-synthesised, reachability-pruned `pv_init_all`** (the purs-wasm model): the
> **purvasm linker** does whole-program reachability from the entry (reusing `Link`) and emits one
> `init.o` that initialises **only reachable bindings, in init-unit-level topological order**; the **system
> linker's dead-strip** (`ld -dead_strip` / `lld --gc-sections`) does the tree-shaking. This avoids dead
> init and orders at binding (not module) granularity, while keeping per-module code compilation
> incremental. §8's entry stub now calls `pv_init_all`. (Slices 1–4 use single self-contained terms — the
> degenerate one-module case — so this lands with slice 5's cross-module work.) **Three §2/§3 pins:**
> (1) a top-level binding exports a **root-handle global** `@<mangle(M.x)>$root` — an `i64` root handle, *not* a raw
> value, because a static global is **not a GC root** and a raw value would go stale after a moving
> collection (v1 has no non-moving S1 — [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §5);
> `pv_init_all` roots each value into a never-popped frame and stores the handle, references `pv_get` it.
> (2) A **`Gfun` is referenced as a closure value** (its `@M.f$root` handle), arity known in `M`, so **no
> arity crosses the boundary** (§2's "no `.pmi`" now covers arity, not just rep signatures). (3) The
> **init-unit topological order is built explicitly by the new link step from `free_vars` edges**, *not*
> reused from `plink`'s DFS visitation order.
>
> **Correction (2026-07-03, during slice-5 implementation):** §5's `CIf` is no longer "no call" — the
> condition is a **Boolean demand site** that forces a by-need cell via `pv_force_if_byneed`
> ([0071](0071-codegen-runtime-c-abi.md) §6) before reading the payload bit; a `case` **guard** is the
> same. §8's **pure entry forces its final result** before the printer (the entry value may be a by-need
> cell); the `Effect` entry needs no force (`pv_run_effect` → `pv_apply` auto-forces a by-need callee).
> Construction sites and `apply` head/args keep the raw cell (the knot-tie is preserved).
>
> **Correction (2026-07-03, B1 implementation):** §2's symbol mangling is pinned to an **injective link
> ABI** — a `pv_g_` prefix + `_HH` escaping of every non-alphanumeric byte (`_` included) — because a
> `codegen_ml`-style `.`→`_` map collides (`A.B`/`A_B`) and would double-define / clash `@…$root` across
> `.o`s. The constructor **tag is a deterministic name hash** (`fnv1a_64`, 31-bit-masked), not a per-module
> first-seen counter, so a ctor built in one `.o` matches its pattern in another. Lifted functions are
> emitted with **`internal` linkage** (referenced only by address within their own module).
>
> **Scope (2026-07-03, B2 — maintainer-directed minimal slice):** the first per-module slice lands
> **root-handle globals + real per-`.o` separate compilation** (each module → its own object, an
> `init`/entry object carrying `pv_init_all` + `@main`, linked by the mangle symbol ABI), but `pv_init_all`
> initialises **all** bindings in **linked-spine (dependency) order** — the explicit binding-level
> reachability graph, its pruning, and the system-linker dead-strip (`ld -dead_strip` / `lld
> --gc-sections`) of §3 are **deferred to a follow-up slice**. Spine order is a valid topological order
> ([Link] already spine-orders the import DAG), so init ordering is correct; only *dead-init elimination*
> and *tree-shaking* (size, not correctness) are deferred. Bindings with a bare (non-module-qualified) key
> — synthesised fresh-names — each form a singleton module object.
>
> **Update (2026-07-03, reachability slice):** the deferred pruning + tree-shaking of the previous note
> now lands. `pv_init_all` initialises **only the bindings reachable from the entry** — the transitive
> closure of the entry's global references over each binding's body free-vars (a `Grec` group reached as a
> unit). Modules still emit *all* their bindings (a module compiles without knowing the entry), so a
> pruned dead binding's code/globals become **unreferenced symbols the system linker's dead-strip removes**
> (`-Wl,-dead_strip` on ld64; `nm` confirms the symbol is gone). Init order remains **linked-spine order
> restricted to the reachable set** — still a valid topological order, so ordering stays correct without
> rebuilding the edge graph; the explicit edge-topo (§3 pin 3) is only needed once the driver consumes
> per-module *artifacts* rather than a single linked spine. Responsibility split per §3: purvasm decides
> *what* to initialise (reachability), the system linker removes the *dead code* (size).

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

A module's top-level binding `M.x` emits a **root-handle global** `@<mangle(M.x)>$root` — an `i64` holding
the value's shadow-stack root handle, *not* the raw value (a static global is not a GC root; §3). A
reference to another module's `N.y` is an **`external`** `@<mangle(N.y)>$root` the linker resolves — the
[0033](0033-separate-compilation.md) qualified-key scheme, faithful to `lower.ml`'s `qualified_key` — and
reads the current value with `pv_get` (§3). Because this is a **cross-`.o` link ABI**, the mangling must be
both a valid LLVM identifier and **injective**: an alphabetic prefix `pv_g_` (avoids a leading digit —
LLVM rejects `@1_foo` — and namespaces away from the runtime's `pv_*`) followed by each qualified-key byte,
alphanumerics verbatim and **every other byte, `_` included, escaped as `_HH` (hex)** so the escape is
unambiguous and distinct keys can never collide (`A.B` → `pv_g_A_2eB`, `A_B` → `pv_g_A_5fB`; a
`codegen_ml`-style `.`→`_` map is *not* injective and is unsafe as a link ABI). The
cross-module calling convention is **boxed** ([0059](0059-native-abi-value-representation.md) §3 /
[0071](0071-codegen-runtime-c-abi.md)): every cross-module value is a plain tagged word. **No `.pmi`
metadata is needed** — not a rep signature (the boundary is boxed) and **not even an arity** (a top-level
function crosses as its *closure* handle, which carries its own arity, so a referrer never needs the
callee's arity statically — §3).

### 3. CAF initialisation — a link-synthesised, reachability-pruned `pv_init_all`

Top-level bindings are strict ([0070](0070-v1-byneed-recursive-caf-force.md): non-recursive CAFs are
eager), but a binding may read another module's binding, so initialisation must respect dependency order.
Classification mirrors the VM's `gdef_of_expr` (`codegen.ml`): each is a **`Gfun`** (a syntactic lambda), a
strict **`Gcaf`** (any other non-recursive value), or a **`Grec`** member (a recursive-group value).

**Each top-level binding exports a *root-handle global*, not a raw value — because a static LLVM global is
not a GC root.** A raw `Value` stored in a global would go **stale after a moving collection** relocates
its heap object (v1 allocates *everything* in the local moving heap — there is no non-moving S1 arena yet,
[0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §5). So the exported global holds the value's
**shadow-stack root handle** (an `i64`), pinned for the program's lifetime: `pv_init_all` builds the value,
`pv_root(ctx, value)`s it into a **permanent root frame it never pops**, and stores the handle into the
global. **Every reference — intra- or cross-module — loads the handle and reads the *current* value via
`pv_get(ctx, handle)`** (relocation-correct), the global being an `external` symbol cross-module (§2). The
handle is opaque, so **no arity or rep metadata crosses the boundary** (a value's own arity rides inside
its closure object). Per class:

- **`Gfun` → a closure-root global `@<mangle(M.f)>$root`.** In the all-`apply` v1 even a saturated call goes
  through `pv_apply` ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §3), so a top-level
  function is referenced as a closure *value*. `M` emits its lifted code symbol `@<mangle(M.f)>` (§4); the
  init unit is `pv_root(ctx, pv_make_closure(@<mangle(M.f)>, arity, unit))` stored to `@<mangle(M.f)>$root`.
  **Arity is known where the closure is built (in `M`)**, so a referrer never needs it — it loads
  `@<mangle(M.f)>$root`, `pv_get`s the closure, and `pv_apply` reads the arity from the object. The
  no-capture closure has **no dependencies** → this init unit has no in-edges.
- **`Gcaf` → a value-root global `@<mangle(M.x)>$root`** — the init unit evaluates the CAF, roots the
  result, stores the handle.
- **`Grec` → a `ByNeed`-cell root global** per member — the init unit is the one-time Grec construction
  ([0070](0070-v1-byneed-recursive-caf-force.md) §4), rooting each cell; a dereference `pv_get`s the cell
  then `pv_force`s it.

(When the non-moving **S1** arena lands — [0061](0061-capability-local-shared-immutable-gc.md)/[0064](0064-v1-single-capability-native-abi-codegen-contract.md)
§5 — immortal CAF *constants* may instead live in S1 at a stable address and be **raw-value globals** with
no `pv_get` indirection; v1 has no S1, so every top-level value is a rooted local-heap object.)

**A binding is *not* initialised by a per-module `pv_init_M`.** A **link-time step synthesises one
whole-program `pv_init_all(ctx)`** (a small emitted `init.o`) that runs the init units **reachable from the
entry**, in **dependency-topological order**. Responsibility splits cleanly:

- **purvasm's linker computes the reachable set *and* explicitly builds the order.** `Link` already gives
  the *reachable binding set* from the entry's transitive `free_vars` over the import DAG (the VM's `plink`
  "visit reachable keys from `main`" — [0016](0016-cross-module-linking.md)/[0033](0033-separate-compilation.md)).
  But the **binding-level topological order is built by this new link step, not reused from a DFS
  visitation order**: `free_vars` yields *direct dependency edges* (a set, not an order), so the step
  **builds the reachable init-unit dependency graph from those edges and topologically sorts it** (a
  `Gfun`-closure unit, having no in-edges, naturally precedes the `Gcaf` that references it).
  `pv_init_all` then initialises **only reachable bindings** (no dead init), at binding granularity — the
  `purs-wasm` reachability-init model.
- **the system linker (`ld -dead_strip` / `lld --gc-sections`) does the tree-shaking** — a *size*
  responsibility (not correctness): any symbol not reachable from `main` / `pv_init_all` is dropped.
  purvasm decides *what to initialise and in what order*; the system linker *removes the dead code*.

Because **PureScript forbids cyclic module imports** ([0016](0016-cross-module-linking.md)), the module
graph is a DAG and a **recursive group is intra-module only** (a single `A.LetRec`) — there is no
cross-module `Grec`, so the reachable init-unit graph the step topo-sorts is acyclic *at binding
granularity*: the only cycles are inside a single `Grec`, which is by-need (its init unit is one atomic
Grec construction, **never a static order over the cycle**). This closes the *"gdef ordering vs boot
on multi-module closures"* gap.

**Per-module `.o` compilation of *code* is preserved** (recompile one module → one `.o`, relink); only the
tiny `init.o` is re-synthesised at link, which is cheap. (The new link step orders the **init units** from
the `free_vars` dependency edges — a *set*, not itself an order; `Link.topo_sort` on the module DAG is a
coarser input, not the init order.)

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
- **`CIf`** — branch on the unboxed `Boolean` immediate. The condition is a **Boolean demand site**, so a
  by-need cell reaching it (a `Grec` member, or one arrived through an argument/field) is forced first via
  `pv_force_if_byneed` ([0071](0071-codegen-runtime-c-abi.md) §6) — mirroring the VM's `Jump_unless` force
  — then the branch reads the payload bit with no further call. A `case` **guard** is the same demand site
  (its Boolean result is forced before its bit is read).
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

A generated entry stub (`main`) `pv_runtime_new`s the context, calls the link-synthesised `pv_init_all`
(§3), evaluates the entry global, then: a **pure** entry prints via a type-directed print (e.g.
`pv_print_int` for an `Int` entry — the native rep is type-erased, so there is no generic runtime
`to_string`; the codegen emits the printer its known entry type dictates); an
**`Effect`** entry runs `pv_run_effect` ([0067](0067-v1-effect-execution-and-native-leaves.md) §2),
performing its effects with the `Unit` result suppressed. The **pure entry's final result is itself a
demand site** — it is `pv_force_if_byneed`d before the printer reads it, since the entry value may be a
by-need cell (`letrec x = 7 in x`, or a call returning one); the `Effect` entry needs no such force, as
`pv_run_effect` → `pv_apply` already auto-forces a by-need callee. Output routes through the sink to `stdout`
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
- **Initialisation is a link-synthesised, reachability-pruned `pv_init_all`** (§3), *not* a chain of
  per-module inits: purvasm's linker does the whole-program reachability + init-unit-level ordering (only
  reachable bindings, no dead init), and the system linker's dead-strip removes dead code — a clean
  responsibility split. Top-level bindings are **root-handle globals** (moving-GC-safe, §3); by-need
  `ByNeed` globals cover the *module-local* recursive groups; this closes the *"gdef ordering"* gap while
  keeping per-module code compilation incremental.
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
- **Per-module `pv_init_M` inits called in module topological order** (each module eagerly initialises
  all its own bindings; the entry calls every module's init). The §3 draft's first form. Rejected: it
  initialises **dead bindings** (a module's unused exports still run) and orders only at *module*
  granularity. The link-synthesised, reachability-pruned `pv_init_all` (§3) initialises only reachable
  bindings at init-unit granularity — strictly better, and the `Link` reachability it needs already
  exists.
- **All top-level CAFs by-need** (uniform, sidesteps init order entirely). Simpler than the §3 split, but
  it pays a force-check per top-level reference for the acyclic majority the reachability-ordered eager
  init handles for free, and [0070](0070-v1-byneed-recursive-caf-force.md) keeps non-recursive CAFs
  strict. Rejected in favour of eager-acyclic + by-need-cyclic.
- **A stack-map / `gc.statepoint` rooting emission** instead of shadow-stack calls. The eventual precise
  mechanism, but deferred with the statepoint migration
  ([0064](0064-v1-single-capability-native-abi-codegen-contract.md) §4,
  [0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md) §7); the shadow stack is the v1 vehicle.
