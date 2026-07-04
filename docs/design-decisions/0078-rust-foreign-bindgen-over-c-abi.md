# 0078. Idiomatic Rust native foreigns as a bindgen-style layer over the `pv_*` C ABI

- Status: Accepted
- Date: 2026-07-05

## Context

[0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) opened the native-foreign channel:
a packaged provider ships a residual first-order leaf as **`.c` over the `pv_*` C ABI** (`purvasm.h`),
exported as a link-time `pvf_<mangle(key)>` symbol ([0072](0072-anf-to-llvm-lowering.md) §2 mangling),
with exactly one provider per key. The channel works end-to-end (`showNumberImpl`), and C is the right
*floor*: zero-dependency, retargetable through the same `clang` that lowers the `.ll`.

But C as the *only* authoring language has a DX and safety ceiling, precisely at the points the C ABI
made the FFI author's responsibility:

- **The rooting contract is a comment-level discipline.** `purvasm.h` documents it — root a value held
  live across a later allocating `pv_*` call, reload via `pv_get` after the safepoint — but nothing
  enforces it. A missed root or a stale (unreloaded) word is at best a release-mode shape fault and at
  worst a silently wrong value read from a moved object's old address. This is the exact class of bug
  the runtime itself eliminated internally with typed rooting ([0066](0066-v1-shadow-stack-rooting-and-gc-on-alloc.md)).
- **Marshalling is manual**: every leaf hand-writes accessor calls, buffer sizing (`pv_str_len` /
  `pv_str_copy`), and the argument-unpacking prologue of the `AbiCodeFn` shape.
- **Panic/UB containment is convention**: nothing stops a C leaf from dereferencing a `PVWord`.

Meanwhile the runtime is Rust ([0063](0063-runtime-implementation-language-rust.md)), so a Rust
toolchain already exists in the build. The obvious ask — *"let me write a foreign in idiomatic Rust"*
— has a well-known good shape: **wasm-bindgen**, which does not add an ABI to Wasm but generates the
glue that adapts idiomatic user code to the fixed low-level boundary. This record proposes the same
move for purvasm: Rust foreigns as a **DX layer over the existing C ABI**, not a second ABI.

## Decision

### 1. Positioning: a wrapper over the `pv_*` C ABI — link-indistinguishable from a C foreign

A Rust foreign compiles to **the same artifact a C foreign does**: one `extern "C"` `AbiCodeFn` per
leaf, exported under the same `pvf_<mangle(key)>` symbol, calling only the `pv_*` functions that
`purvasm.h` declares. Everything [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md)
fixed is **unchanged**: the one-provider-per-key link resolution (§3), the metadata model
(`arity`/`effectful` from the `foreign import` signature at Level 2+, boot's manifest expedient;
`source` in the manifest/build config), the runtime surface (no new `pv_*` symbol, no runtime code
change), and C authoring (which remains fully supported — the zero-dependency floor; Rust is a
per-foreign additive choice).

### 2. The load-bearing restriction: the layer speaks `purvasm.h`, never `purvasm-rt` internals

Although both the runtime and the foreign are Rust, a Rust foreign **must not** depend on
`purvasm-rt`'s crate internals (`Heap`, `Value`, `TaggedWord`, …). It sees only a 1:1 `extern "C"`
declaration of the `purvasm.h` surface, resolved at the final link exactly as a C foreign's calls are.
Rationale: linking against runtime internals would silently freeze them into a public ABI — the
representation-opacity boundary ([0069](0069-v1-dynamic-record-operations.md) /
[0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2) exists so the runtime can
change layout without breaking any foreign, and that property must not depend on which language the
foreign happens to be written in. A Rust foreign is a *client* of the runtime, not a sibling.

### 3. Crate shape: raw `sys` floor, safe typed layer, attribute macro on top

Three layers, each usable without the one above it (the project's `unsafe`-floor / safe-surface
convention applied to the FFI):

- **`purvasm-sys`** — the `unsafe` floor: hand-maintained `extern "C"` declarations mirroring
  `purvasm.h` 1:1 (the header is small and deliberately stable, so hand-maintenance beats a
  build-time `rust-bindgen` step and its toolchain cost). Nothing else.
- **`purvasm-foreign`** (name bikesheddable at acceptance) — the safe typed layer:
  - a `Ctx` handle wrapping `*mut PVContext`, through which every operation flows;
  - **conversion traits** (`FromPv` / `IntoPv`) for the scalar reps: `i32` ↔ `Int`
    (`pv_int_payload` / `pv_int`), `bool` ↔ `Boolean`, `f64` ↔ `Number`
    (`pv_number_bits` / `pv_new_number`), `String`/`&str` ↔ `Str` — the string conversion is an
    owned **copy-out/copy-in**, which is exactly the two-call shape (`pv_str_len` + `pv_str_copy`)
    the header already mandates so no interior pointer into the moving heap ever exists. In
    particular, a `&str` parameter is a borrow of a **wrapper-owned Rust buffer** (the copy the
    generated prologue made via `pv_str_len`/`pv_str_copy`), valid exactly for the user fn's call —
    never a borrow into the guest heap and never derived from a `PvValue`, so string borrows are
    entirely ordinary Rust lifetimes with no GC interaction;
  - an opaque **rooted handle type** (`PvValue`) for guest values passed through untyped: it stores
    the `pv_root` handle, **every read goes through `pv_get`** — reload-after-safepoint becomes
    automatic instead of a discipline. Constructors (`ctx.new_array(…)`, `ctx.apply(f, args)`, …)
    take and return these. A raw unrooted `PVWord` is never exposed by the safe layer;
  - **frame-lifetime branding, and no `Send`/`Sync` — pinned, not optional**: the context and
    handle types are lifetime-branded to the shadow-stack frame the macro opens —
    `Ctx<'frame>` / `PvValue<'frame>` — so a handle cannot outlive the `pv_pop_frame` that
    invalidates its root slot (escaping one from the leaf is a **compile error**, not a stale-handle
    fault at run time), and both types are `!Send`/`!Sync`, so a guest value cannot cross a thread
    from safe code (the single-capability contract,
    [0064](0064-v1-single-capability-native-abi-codegen-contract.md), becomes unbreakable by
    construction rather than by convention). This is the layer's strongest win over C: safe Rust
    must not be able to construct a dangling root handle or a cross-thread guest reference.
- **`#[pv_foreign(module = "Data.Show", name = "showNumberImpl")]`** — the attribute macro (the
  bindgen analogue, in a companion proc-macro crate): applied to an idiomatic
  `fn(f64) -> String`-shaped Rust function it generates the `AbiCodeFn` wrapper — computes the
  [0072](0072-anf-to-llvm-lowering.md) §2 mangled symbol at expansion time (`#[export_name =
  "pvf_…"]`, so the injective mangle is reproduced in one place, not by hand), opens/closes the
  `pv_frame`, converts `args[0..n]` via `FromPv`, calls the user fn, converts the result via
  `IntoPv`, and installs the panic guard (§4). An `effect` marker generates the
  [0067](0067-v1-effect-execution-and-native-leaves.md)-shaped pair (outer leaf returning the thunk
  closure) that an effectful C leaf writes by hand today.

The rooting contract thus moves from a header comment to a structure: the macro owns the frame, the
handle type owns the reload, owned conversions copy out, and the lifetime brand rules out frame
escape. What remains unproven is only what `unsafe` opts into (a leaf can still hold a stale copied
word via the `sys` floor); the paved path makes the contract the default instead of the author's
memory.

> **Progress (2026-07-05):** Accepted, and the maintainer fixed the crate homes ahead of
> implementation. The three layer crates live under a new top-level `crates/` directory —
> `crates/purvasm-sys/`, `crates/purvasm-foreign/`, `crates/purvasm-foreign-macros/` (the §3 names
> stand). The runtime **stays at `runtime/` for now**: moving it to `crates/purvasm-rt/` is the
> desired end state but is deferred until the CI wiring can move with it. All crates remain
> monorepo-internal with `publish = false` at least until, post-v1.0: external users are writing
> purvasm foreign crates, an ABI-compatibility policy can be stated, and a crates.io publishing
> process is actually needed.

### 4. Panic containment at the `pvf_*` boundary

A Rust panic must never unwind across the `extern "C"` boundary into the runtime's `apply` (UB, and
[0071](0071-codegen-runtime-c-abi.md) §7 already pins the runtime side: `catch_unwind` → abort). The
generated wrapper mirrors it from the foreign side: the user fn runs under `catch_unwind`, and a
panic becomes a fatal runtime fault (stderr, then abort) — the same observable class as a C leaf's
shape-validation fault, never a recoverable guest value ([0074](0074-effect-exception-throw-only-ulib-shadow.md)'s
no-catchable-exception premise is untouched).

**Panic strategy of the bundle**: the guard above presupposes `panic = "unwind"`, which is the
supported profile — it is the default, and [0071](0071-codegen-runtime-c-abi.md) §7 already builds
the runtime staticlib on the same assumption (its `catch_unwind`-not-`panic=abort` choice keeps the
`lib` test surface). A `panic = "abort"` profile would remain *sound* (the process aborts before any
unwind can reach the boundary) but forfeits the named diagnostic — the wrapper's stderr report
becomes best-effort (Rust's own abort message only). So: **`unwind` is the pinned default; `abort`
is tolerated with degraded diagnostics, never required**.

### 5. Build and link: one bundled staticlib; the plain runtime staticlib when no Rust foreign exists

A Rust foreign is shipped as **crate source** (a small cargo package, exactly parallel to a `.c` —
prebuilt objects stay an optional cache, per
[0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2's portability rejection of
prebuilt-as-primary). The manifest's `source` field grows a **tagged schema** so the driver can tell
the two forms apart — `source = { kind = "c", path } | { kind = "rust-crate", path }`, with today's
bare-string `.c` form kept as shorthand for `kind = "c"` — and several keys may name the same crate
(one crate, many `#[pv_foreign]` exports): the driver **dedups to a single rlib build** per crate.

Linking is where Rust differs from C, and it needs two pinned decisions:

- **One bundled staticlib.** Two Rust staticlibs cannot be linked into one binary (each embeds
  libstd — duplicate symbols), so the build does not produce a per-foreign staticlib. When at least
  one Rust foreign is reachable, the driver synthesizes a **bundle crate**:
  `crate-type = ["staticlib"]`, depending on `purvasm-rt` (its existing `lib`/rlib front door,
  [0071](0071-codegen-runtime-c-abi.md) §1) and on each reachable foreign crate (rlib). One rustc
  link produces one staticlib that **replaces** the plain runtime staticlib in the existing `clang`
  link; C foreign objects and `.ll` objects link against it unchanged. The foreign crates still
  depend only on `purvasm-sys` (§2) — the bundle merely co-links them with the runtime that defines
  those symbols; one libstd, one panic runtime. **Symbol survival is a pinned contract, not an
  assumption**: the macro-generated wrapper is `pub extern "C"` carrying the
  `#[export_name = "pvf_…"]`, the bundle crate guarantees every provider's exports reach the archive
  (`#[used]`-anchored or explicitly referenced — the implementation picks the mechanism), and the
  build **audits the bundled staticlib with an `nm`-class check** that every `pvf_*` the provider
  map (below) expects from Rust providers is actually present — before the final link, so a dropped
  symbol is a named build error, not a late undefined-reference.
- **Exactly-one-provider is validated by the driver, not delegated to the linker.** Archive
  semantics make the linker an unreliable judge here: members of a `.a` are pulled only when
  referenced, so a duplicate `pvf_*` between two bundled Rust providers — or between the bundle and
  a C object — can go unseen at the final link. So the driver builds a **provider map** over the
  reachable foreign keys, covering **runtime intrinsics, C `source`s, and Rust crate exports
  together**, and **validates exactly one provider per key before linking** — failing with
  `duplicate native foreign provider for <key>` (or the missing-provider analogue naming the
  foreign). This refines the [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3
  enforcement point: the invariant is unchanged, but its realisation moves from link-time symbol
  collision to a driver-side check — which also upgrades the diagnostic from a raw linker message
  to one naming the key, for C providers too. The linker's duplicate-symbol error remains only a
  backstop.

When no Rust foreign is reachable, the build uses the prebuilt plain runtime staticlib exactly as
today — the cargo/rustc dependency and the bundle step cost nothing unless a Rust foreign is present.

> **Progress (2026-07-05):** build-orchestration clarifications agreed with the maintainer ahead of
> implementation — `purvasm build` owns the whole flow first-class:
>
> 1. provider map over the reachable keys (the §5 exactly-one validation, C/Rust/intrinsics
>    together, crate-level dedup);
> 2. C providers compile via `clang -c` exactly as today;
> 3. only when a Rust provider is present, the driver synthesizes the bundle crate in the build
>    directory and drives the **user's** cargo
>    (`cargo build --release --target-dir <bdir>/cargo`), then runs the `nm`-class audit on the
>    produced archive;
> 4. the final `clang` link is unchanged except that the bundle `.a` stands in for the plain
>    runtime staticlib.
>
> Two consequences made explicit:
>
> - **A Rust-foreign build recompiles the runtime with the user's toolchain.** rlibs are not
>   stable across rustc versions, so the bundle cannot link a prebuilt `purvasm-rt` rlib: the
>   distribution ships the `purvasm-rt` / `purvasm-sys` / `purvasm-foreign` *sources*, and the
>   bundle builds them all with the user's rustc. The prebuilt runtime staticlib serves only the
>   no-Rust path. Cargo's target-dir cache confines this to a first-build cost.
> - **Cargo invocation defaults**: `--locked --offline` (a foreign crate depends only on the local
>   path crates by default; third-party dependencies are an explicit opt-in flag), an MSRV floor
>   pinned via `rust-version` in lockstep with the runtime crate, and cargo failures surfaced with
>   a log path exactly as the existing `clang` logs are.
> - **The driver checks `purvasm-sys` ↔ `purvasm.h` version agreement at bundle-build time.** The
>   Consequences already pin lockstep versioning of the layer crates with the header; this makes
>   the check active rather than a convention: before invoking cargo, the driver verifies that the
>   `purvasm-sys` version the bundle resolves matches the header/runtime-staticlib version the rest
>   of the build uses, and fails with an error naming both versions on mismatch. This is the
>   wasm-bindgen lesson (its library↔CLI version-match failures are a recurring operational trap)
>   applied preemptively, as a natural extension of the provider-map validation step.

### Non-goals (unchanged invariants)

No new `pv_*` symbol and no runtime behaviour change; no override/priority between providers
([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3); leaves stay first-order in
spirit (the safe layer exposes `apply` with the same contract the C header does, nothing more); no
threads/async inside a leaf (single-capability v1, [0064](0064-v1-single-capability-native-abi-codegen-contract.md)
— and type-enforced by §3's `!Send`/`!Sync` branding rather than left to convention);
JS dual-target foreigns and the boot OCaml host registry are untouched. This record is
future-extension design only — implementation begins on acceptance, after the
[0076](0076-direct-known-arity-calls-musttail.md) increment.

## Consequences

- A foreign author gets idiomatic Rust with the rooting contract structurally enforced and
  marshalling generated — the two places a C leaf is easiest to get subtly wrong — at zero cost to
  the ABI, the runtime, or existing C foreigns.
- **Rust foreigns become Miri-testable**: a foreign crate plus `purvasm-rt`'s rlib front door form a
  pure-Rust test graph, so a leaf can run under the island's Miri discipline
  ([0063](0063-runtime-implementation-language-rust.md)) — a validation class a C foreign can never
  join. (The `extern` declarations resolve against the runtime's `#[no_mangle]` definitions in-graph.)
- New moving parts to own: the `sys`/safe/macro crates (versioned in lockstep with `purvasm.h` — the
  header remains the single source of truth for the surface), and three driver-side steps — the
  bundle link, the provider-map exactly-one validation, and the `nm`-class symbol audit (§5).
- The provider-map check improves diagnostics for **all** providers, C included: a duplicate or
  missing native foreign becomes a named driver error (`duplicate native foreign provider for
  <key>`) instead of a raw linker message — and duplicates masked by archive member selection are
  caught at all.
- The toolchain gains a conditional cargo/rustc requirement — only when a package ships a Rust
  foreign; the C floor keeps the no-cargo build working.
- Per-access cost of the safe layer (a `pv_get` per rooted read, copy-out strings) is the same class
  of cost the C contract already implies for a *correct* leaf; leaves are small by policy
  ([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §1), so this is noise.
- Two authoring paths must stay documented and equivalent; the acceptance demo should be a worked
  foreign behind the layer (a scratch-consumer package, or a Rust `showNumberImpl` — noting the
  latter must keep byte-parity with the libc-deferring formatting, e.g. via the `libc` crate, since
  the differential gate is byte-identity with boot) passing the standing differential and the
  examples sweep.

## Alternatives considered

- **Let Rust foreigns link against `purvasm-rt` internals** (use `Heap`/`Value` directly — "it's all
  Rust anyway"). Rejected: freezes runtime internals into a de-facto public ABI, breaks
  representation opacity, and couples every foreign to the runtime's crate version. The C-ABI
  boundary is the contract precisely so the runtime can move.
- **A second, Rust-idiomatic runtime ABI** exported beside the C one. Rejected: two ABIs to keep in
  lockstep forever, for the same expressive power the wrapper gets over one.
- **`cdylib`/`dlopen` plugin foreigns.** Rejected: contradicts the static single-binary link model
  and forfeits the link-time missing/duplicate-provider errors ([0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §3).
- **Prebuilt per-platform staticlib as the distribution form.** Same rejection as prebuilt `.o` in
  [0073](0073-ulib-shipped-native-foreign-and-link-time-resolution.md) §2: target-specific, breaks
  package portability. Source crates retarget via `--target` exactly as `.c` does via `clang`.
- **Safe crate only, no macro** (author hand-writes the `AbiCodeFn` wrapper and `#[export_name]`).
  Kept as a supported escape hatch by layering (§3), but rejected as the paved path: hand-reproducing
  the injective mangle and the frame/panic prologue per leaf is exactly the boilerplate-with-sharp-edges
  a bindgen layer exists to delete.
- **Generate the `sys` layer from `purvasm.h` with `rust-bindgen` at build time.** Deferred as an
  implementation option; the header is tiny and stability-pinned, so a hand-maintained 1:1 crate is
  simpler than adding a bindgen toolchain step, and drift is caught by the differential the first
  time a signature disagrees.
