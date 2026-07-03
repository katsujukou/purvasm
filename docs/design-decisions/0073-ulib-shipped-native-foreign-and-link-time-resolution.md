# 0073. ulib-shipped native foreign for the native backend: `.c` over the `pv_*` C-ABI, resolved by link-time `pvf_*` symbols

- Status: Accepted
- Date: 2026-07-03

## Context

[0038](0038-base-package-and-ulib-patches.md) fixed the FFI model: the recognised `Purvasm.*` primitives are
the intrinsic floor, and a **ulib** patches a registry package either by *reimplementing* it in PureScript
over `Purvasm.*` (so a would-be foreign lands on the optimiser's turf and needs no native) or by
*foreign-impl completion* — keeping the registry `.purs` and supplying the foreigns it declares. §2 lists the
completion sources as "a leaf, or `Purvasm.*`", and §4 keeps a few **genuinely-native** first-order leaves in
the host (IO, float-bits, a Dragon4-class `showNumber`).

The boot OCaml backend implements those native leaves in its host registry ([0022](0022-native-foreign-rung.md)/
[0056](0056-purvasm-system-host-leaves.md)). The **native/LLVM backend** ([0071](0071-codegen-runtime-c-abi.md)/
[0072](0072-anf-to-llvm-lowering.md)) instead resolves a foreign at run time: codegen lowers `AForeign key` to
`pv_foreign(ctx, "key")`, and the runtime's `leaf.rs` **hard-codes a string `match`** returning the leaf as a
closure. That works for the tiny intrinsic set, but it does not scale and — crucially — **a ulib or a user
package cannot ship a native foreign** for this backend: a residual first-order function (`showNumberImpl`) or
a user-package foreign (`Record.Studio.*`) would have to be added to the runtime's hard-coded table, which the
runtime has no business knowing about (a user package is not the runtime's concern).

The desired shape (agreed): **foreigns that can be PureScript *are* PureScript** (ADR-0038 reimplementation,
over `Purvasm.*` — e.g. `Record.Studio.shrinkImpl` is `foldl … unsafeSet (unsafeGet …) {}`), and the
**residual first-order foreigns that genuinely need native code** (`showNumberImpl`) are **shipped by the ulib
alongside its PureScript**, exactly as purs-wasm's ulib ships pure-PS reimplementations plus an optional
`foreign.wat`. purvasm's ulib carries only PureScript today solely because there was no channel for a
ulib-supplied native foreign on the native backend. This ADR opens that channel.

## Decision

### 1. PureScript first; native foreign only for the residual first-order floor

A ulib foreign is a PureScript reimplementation over `Purvasm.*` **whenever it composes** (ADR-0038 §2). A
**native foreign** is written only for a genuinely-native first-order function — one that cannot be expressed
over the primitives (`showNumberImpl`'s shortest-round-trip `Number → String`, and the like). This keeps the
native surface minimal (ADR-0038 §4): the runtime owns only the true intrinsics; everything above is ulib.

### 2. Form — `.c` source over the `pv_*` C-ABI (portable), compiled at build; prebuilt object optional

A ulib native foreign is shipped as **C source over the `pv_*` C-ABI** ([0071](0071-codegen-runtime-c-abi.md)):
the ulib package carries a `foreign.c` (beside its corefn) that `#include`s a stable `purvasm.h` declaring the
`pv_*` surface, and the build compiles it with the same `clang` that compiles the program's `.ll`, then links
it. Rationale:

- **Portable / retargetable** — C source retargets through `clang` per platform, unlike a prebuilt object;
  this is the purs-wasm-`foreign.wat` property (a portable source the target toolchain compiles), for the
  native target.
- **Ergonomic** — human-writable, unlike hand-authored textual `.ll`.
- **Opaque values + an explicit rooting contract.** The foreign works with **opaque `PVWord`s** (typedef'd
  `uint64_t`) and calls `pv_new_str` / `pv_record_get` / `pv_apply` / … (ADR-0071): it never inspects a tag,
  distinguishes pointer-vs-immediate, or dereferences a word — the [0069](0069-v1-dynamic-record-operations.md)
  representation-opacity boundary applies to FFI authors. The `pv_*` surface **validates each object's shape**
  and each value-constructor **self-roots its own arguments across its own allocation** (ADR-0066 §3). But the
  representation *moves* (ADR-0064): a `PVWord` a foreign holds **live across a *later* allocating `pv_*` call**
  goes stale unless rooted. So `purvasm.h`'s contract is: **root a value that must survive a subsequent
  allocation** via `pv_frame` → `pv_root` → (allocate) → `pv_get`-reload → `pv_pop_frame` — the same
  shadow-stack discipline codegen emits (ADR-0066 §3 / ADR-0071 §5). A single-allocation leaf (e.g.
  `showNumberImpl` building one `Str`) needs none; a multi-allocation one does. Rooting is the FFI author's
  responsibility; representation/GC/panic-containment stay the runtime's (ADR-0071 §7).
- **Opaque scalar accessors — required, not optional, and `ctx`-taking.** A residual leaf must *read* its
  scalar arguments: `showNumberImpl` reads the `Number`'s IEEE-754 value to format it. It must do so
  **without** knowing the encoding (no `pv_read_raw(n, 0)`, which would break opacity and pin the layout). So
  `purvasm.h` **must expose opaque accessors** paired with the constructors it already has (`pv_new_number` /
  `pv_new_str` / …): at minimum `pv_number_bits(PVContext*, PVWord) -> uint64_t` (the IEEE-754 bit pattern),
  `pv_int_payload(PVContext*, PVWord) -> int32_t`, and `pv_bool_payload(PVContext*, PVWord) -> int` — the read
  side of ADR-0064's scalar reps, behind the ABI so the encoding stays the runtime's. **A `Number` is boxed**
  (ADR-0064 §1), so `pv_number_bits` needs `ctx` to reach — and shape-validate — the heap object; a C leaf can
  never dereference the word itself without breaking opacity. `pv_int_payload` / `pv_bool_payload` read an
  *immediate* and could be `ctx`-free, but **every accessor takes `PVContext*` for ABI uniformity** (the whole
  `pv_*` surface is `ctx`-first; an immediate accessor ignores it). `showNumberImpl` is then writable as
  `pv_number_bits(ctx, n)` → format in C → `pv_new_str(ctx, …)`, touching no representation. The exact
  accessor set grows on demand, but the ADR pins that this **read surface exists** and is `ctx`-taking — the
  representative example is otherwise unwritable.
- **One pipeline** — reuses the existing `.ll → clang → link` flow ([0072](0072-anf-to-llvm-lowering.md) §1).

A **prebuilt `.o`** is an optional build *cache* (skip recompiling an unchanged foreign), never the primary
distribution form — an object is target-specific and a registry package must stay portable. (Whether the cache
is per-package or content-addressed, and how cross-compilation selects it, is deferred.)

### 3. The `pvf_*` symbol ABI + the arity/effect metadata that resolves it

Foreign resolution moves from the runtime's hard-coded `pv_foreign(string)` dispatch to **link-time symbol
resolution**, but the symbol alone is not enough — codegen must build a *closure* to apply, and the
[0034](0034-effect-analysis-impurification.md) effect analysis must know whether a foreign is effectful. Both
are **compile-time metadata**, so this ADR pins two things together: the symbol shape and where its metadata
comes from.

**Symbol shape.** `pvf_<mangle(key)>` (the injective mangling of [0072](0072-anf-to-llvm-lowering.md) §2) is an
**`AbiCodeFn`** ([0071](0071-codegen-runtime-c-abi.md) §3) — the leaf's code, not a closure. Codegen lowers
`AForeign key` to `pv_make_closure(@pvf_<mangle(key)>, arity, unit)` (a no-capture closure over the leaf code),
then applies it as any closure — so **the leaf's arity is fixed at the reference, from metadata** (it does not
travel with the symbol). An *effectful* leaf follows the ADR-0067 shape (return a thunk); the analysis, not the
ABI, marks it. The runtime's own intrinsics are re-exposed under this `pvf_*` naming.

**Metadata — where `arity` / `effectful` / `source` come from.** Codegen needs three facts per foreign, and
they have **two different homes**:

- **`arity` and `effectful` are properties of the foreign's `foreign import` *type signature*** — the
  principled source (an `a -> b -> Effect c` foreign is arity-2 and effectful). So **Level 2+ obtains them by
  reading the module's externs, or by reconstructing the signature from the PureScript source** — never from a
  hand-written table. This is essential: a **non-ulib, project-source `foreign import`** has *no* ulib manifest
  to declare its arity in, so a manifest cannot be the general answer. **boot is the exception**: it parses no
  PureScript and reads no externs, so it obtains a foreign's arity/effectfulness from its `Ffi` registry (the
  `Ffi.foreign_arity` / `Ffi.effectful` it uses today), extended by a **manifest entry from any
  *manifest-carrying overlay provider*** — a ulib package *or* an overlaid user package ([0038](0038-base-package-and-ulib-patches.md)
  §3 treats both as presence-driven overlays). So boot's native-foreign support is **exactly the packaged
  (overlay) providers**: their manifest supplies arity/effectful. A **bare project-source `foreign import`**
  (a loose foreign in the app's own module, with no package manifest) has **no arity source in boot** and is
  therefore a **Level 2+ feature** (its arity comes from externs / signature reconstruction). This
  manifest-declared arity/effectful is a **boot-only expedient** that dissolves once the self-host reads
  externs — at which point even packaged foreigns take their arity/effectful from the signature and the
  manifest keeps only `source`.
- **`source` (which `.c`/object implements the foreign) is genuine build metadata, not derivable from any
  type** — so it lives in the **ulib package manifest** ([0047](0047-ulib-package-manifest-extra-dependencies.md))
  for a ulib foreign, and in the project's build config for a project-source foreign (its own `foreign.c`,
  the JS-backend `foreign.js` analogue). This mapping is the manifest's *permanent* role, at every level.

All of this is **compiler-side only** — none of it reaches the runtime, which sees just the resolved code
symbol. The intrinsic `Purvasm.*` foreigns keep their existing compiler-known arities (the ABI contract,
ADR-0038 §1).

**Resolution = exactly one provider per key.** Each `pvf_*` symbol is defined by **exactly one** object — the
runtime for an intrinsic, or one ulib package's compiled `foreign.c` for a package foreign — and the system
linker resolves each reference against them, dead-stripping the unused ones (the reachability/tree-shaking
model already in place, [0072](0072-anf-to-llvm-lowering.md) §3). This is **not** a linker "first-match
ladder": a **missing** provider is a **link error naming the foreign** (clearer than a runtime `pv_foreign`
panic), and a **duplicate** strong definition is a **link error** (catching an accidental double-provide).
**v1 defines no override** — no weak/strong priority, no ulib-shadows-intrinsic; a foreign has one home. (A
priority policy for overriding an intrinsic is a deferred extension, and would be the *only* reason to
introduce weak symbols.) The runtime-side `pv_foreign(string)` may coexist during migration but is superseded
— the string table stops being the extension point.

### 4. The runtime keeps only the intrinsic floor

With §3 in place, even a *stdlib* residual (`showNumberImpl`) is a **ulib-shipped** `foreign.c`, not a runtime
`match` arm. The runtime retains only the genuine intrinsics — the `Purvasm.*` primitives, value conversion,
the `apply`/`force` machinery, and the FFI-call plumbing — matching ADR-0038 §4's "the native surface shrinks".
The boot OCaml backend keeps its host-registry leaves ([0056](0056-purvasm-system-host-leaves.md)); this ADR
governs the **native/LLVM backend** only.

## Consequences

- A **packaged provider** — a ulib package, or a user package overlaid the same way (ADR-0038 §3) — can ship a
  native foreign for the native backend, the FFI channel that was missing. Most foreigns stay pure PureScript;
  the native ones are a small, portable `.c`. On **boot** this covers exactly the packaged (manifest-carrying)
  providers; a bare project-source `foreign import` (no package manifest) is Level 2+ (§3).
- Resolution failures are link errors that *name the foreign*, not runtime `pv_foreign` panics.
- The runtime's foreign surface stops growing per stdlib/user need; it holds only intrinsics.
- The `.c`-over-`pv_*` foreign is retargetable, so a ulib package stays portable across native targets.
- New published artifacts / build steps: a stable **`purvasm.h`** (the `pv_*` C-ABI as a C header, including
  the `PVWord` typedef and the **rooting contract** of §2); a **`source` mapping** — in the **ulib manifest**
  for a ulib foreign, in the **project build config** for a project-source foreign — recording which `.c`
  implements each native foreign (the mapping's permanent role); and a "compile the reachable foreigns" step
  before the link. **`arity`/`effectful` are *not* the manifest's job in general** — they come
  from the `foreign import` signature (externs / reconstructed) at Level 2+, and only boot leans on its `Ffi`
  registry plus a manifest expedient (§3). A wrong arity/effectful mis-compiles the caller (as a wrong
  `Ffi.foreign_arity` would today), which the externs path eliminates by construction.
- Rooting stays the FFI author's responsibility across multi-allocation leaves (§2); a single-allocation leaf
  needs none, so the common case (`showNumberImpl`) is barrier-free.

## Alternatives considered

- **Keep the runtime's hard-coded `pv_foreign(string)` and add arms per foreign.** Does not scale, and a user
  package cannot extend the runtime — the very problem. Rejected.
- **Ship prebuilt `.o` as the primary form.** Target-specific; breaks a registry package's portability.
  Retained only as an optional cache (§2).
- **Ship textual `.ll` as the foreign.** Matches codegen output and is portable, but hand-authoring `.ll` is
  far less ergonomic than `.c` over the same `pv_*` surface; a generated-`.ll` path can be added later without
  changing the resolution model (§3).
- **A purvasm-specific FFI bytecode/format.** More machinery than `.c`-over-a-C-header buys, and it would not
  be more portable. Rejected for now.
