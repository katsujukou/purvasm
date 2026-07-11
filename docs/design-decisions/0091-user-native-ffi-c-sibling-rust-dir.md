# 0091. User-defined native FFI: C siblings vs. one Rust dir

- Status: ~~Proposed~~ **Accepted** _(2026-07-11: accepted by the maintainer)_
- Date: 2026-07-11

## Context

With FSR threaded through the build env (ADR-0090) and the LLVM backend emitting
`@pvf_<mangle(key)>` for every native leaf (a `foreign import` the compiler does
not resolve to an intrinsic — `resolver k = Nothing`, ADR-0073 §3), two of the
three pieces of user-defined native FFI are already in place:

- **shape** (arity / effect) — FSR reconstructs it from the app's own source, so
  a `foreign import foo :: T1 -> T2` in an app module needs no manifest to be
  lowered correctly; and
- **codegen** — the leaf lowers to `@pvf_<mangle("M.foo")>`, an undefined
  external symbol the linker must satisfy.

The remaining gap is the **native provider link plan for the app's own
foreigns**: telling the link step *which native object exports
`pvf_<mangle("M.foo")>`*. Library foreigns are already handled — the ulib ships
its providers as `.c` and declares them in `ulib.json`'s `foreign` map, which
`NativeLink` reads and compiles on demand (the referenced-only `.c` link plan,
re-added 2026-07-11). But an **application** has no `ulib.json`.

boot bridged this gap by letting an app project *borrow* the `ulib.json`
mechanism — a project-local manifest that could name both `.c` files and
`rust-crate` providers, freely **mixed within one project**. We want the
Level-2+ story to be deliberately different and simpler:

- a project-borrowed manifest is boilerplate the user must hand-author and keep
  in sync with the module/ident names; and
- mixing C and Rust providers in one project widens the CLI surface (per-provider
  `kind`/`path`) for a case almost nobody wants — someone who writes Rust writes
  all of it in Rust; someone who wants C writes all of it in C.

## Decision

Frame the whole link step as ADR-0078's **provider-map problem**, not a set of
independent link inputs: *discover* candidate providers, *build* the ones a key
actually needs, *audit* their exported `pvf_*` symbols, then *enforce* exactly one
provider per referenced native key. The C-sibling / one-Rust-crate UX below is how
the **app** classes plug into that map.

### 1. The provider map and the exactly-one-provider invariant

After codegen, the **referenced native keys** are the `@pvf_<mangle(M.ident)>`
symbols that appear in the emitted `.ll`. Each is satisfied by a provider drawn from
four classes:

- **runtime** — a `pvf_*` exported by `libpurvasm_rt.a` (e.g.
  `Purvasm.Number.parseFloat`, ADR-0092);
- **ulib** — a `.c` named in a dependency's `ulib.json` `foreign` map (ADR-0073),
  compiled on demand;
- **app-C** — a workspace-source module's sibling `.c` (§2);
- **app-Rust** — the single `--rust-ffi` crate (§3).

**Where the exactly-one-provider check runs — and why it is scoped.** A `.ll`
*textually* references a `@pvf_` for every native leaf whose eta it lowers,
including registry/runtime leaves that end up **dead** and are removed by
`--gc-sections`/`-dead_strip`. So the referenced-key set over-approximates what the
link truly needs, and a blanket zero-provider check over it would false-positive on
dead etas (e.g. an unused `Control.Extend.arrayExtend`). The build therefore
enforces the exactly-one-provider invariant **as a pre-link named diagnostic over
the referenced keys in a workspace-source namespace** (§2) — the classes the user
authors (app-C, later app-Rust), where a missing or mis-exported `.c` is the likely
error and where a workspace leaf is referenced *iff* it is used. For such a key the
build collects every class that provides it (`runtime`/`ulib` from their known
symbol sets, `app-C` from the §4 audit) and **rejects zero providers (`no native
provider for M.foo`, with a `PVF_EXPORT` hint) or more than one (naming the
conflicting classes)** — this is ADR-0078's exactly-one invariant, and correctness
does **not** rest on class key-sets being namespace-disjoint.

Non-workspace (registry/runtime) referenced leaves are **delegated to the linker +
dead-strip**: a dead eta is stripped, and a genuinely-missing *live* runtime/dep
symbol — a compiler or packaging bug, not user FFI — still surfaces, as the linker's
own undefined-symbol error. A fully liveness-aware all-class check would need the
linker's reachability result; scoping to workspace keys buys the user-facing named
diagnostics without it.

**App FFI is C xor Rust.** At most one app class is active: `--rust-ffi` selects
app-Rust and suppresses app-C discovery; its absence selects app-C. The runtime
and ulib classes are always present and orthogonal to the app's choice — a Rust
app still links the ulib's `Data.Show.c` and the runtime's `parseFloat`.

### 2. App-C discovery: local workspace source modules

App-C sibling discovery applies to **local workspace source modules** — modules the
user compiles from source in the workspace, as opposed to **registry dependencies**
resolved under `.spago`. The build must **not** adopt a registry package's adjacent
`.c` as an app provider: a `.spago/…/somepkg/…/Foo.purs` with a neighbouring `Foo.c`
is that package's concern, surfaced through **packaged-provider metadata** (today a
dependency's `ulib.json`; later, per-package native metadata), never app FFI. The
candidate set is the non-registry source modules, identified by source location
(the cache-db path is not under `.spago`), not by inferring names from arbitrary
paths.

This scope is **local workspace source**, not strictly the entry package: a local
*library* package in the same workspace (e.g. `packages/…`) is included. That is
intended — such a package is source the user controls and may legitimately carry a
sibling `.c`. In practice a local library that backs its leaves through the runtime
or a ulib (like `purvasm-base`, whose leaves are Rust `pvf_` in the runtime) simply
has **no** sibling `.c`, so it is never compiled as app-C, and its runtime/ulib
providers satisfy the §1 check with no diagnostic. A tighter *entry-package-only*
predicate would need spago's local-package metadata and is a possible future
refinement; it is unnecessary while local libraries provide their native leaves
through packaged classes.

For a project module `M`, its sibling provider is the `.c` at `M`'s **own source
path with the extension swapped** (`…/M.purs` → `…/M.c`). The module identity `M`
is the **CoreFn / FSR module key** of the loaded module — the authoritative
qualified name — **not** a name reconstructed from the file path: a PureScript
module's qualified name is not recoverable from its path, and the sibling
convention only asserts "this `.c` implements this `.purs`". The build derives the
sibling path from that module's known source path and compiles it with
`-DPVF_MODULE=<escapeIdent(M)>`, where `M` is the CoreFn module key (not the
filename). This mirrors PureScript's own `.purs`/`.js` pairing; C files *can* sit
beside the `.purs` (Rust crates cannot), so the common C case stays flag-free.

**Symbol naming.** The provider must export the global symbol
`pvf_<escapeIdent("M.foo")>` (ADR-0073 §3 ABI; `escapeIdent`: alphanumerics pass
through, every other byte → `_<hex>`, e.g. `.`→`_2e`, `_`→`_5f`). To spare users
from hand-mangling, `-DPVF_MODULE=<escapeIdent(M)>` (a valid C token, e.g.
`Data.Show` → `Data_2eShow`) plus a `purvasm.h` macro let the user name the
function by its bare ident:

    #define PVF_EXPORT(ident)  PVF_CAT(PVF_CAT(pvf_, PVF_MODULE), PVF_CAT(_2e, ident))

so `PVF_EXPORT(foo)` becomes `pvf_Data_2eShow_2efoo`. The macro is **recommended**
and documented; a user who does not use it writes the correctly-mangled symbol by
hand — and §4's audit catches a mismatch by name either way.

*Caveat (documented):* token-pasting is correct only when `ident` is purely
alphanumeric (the overwhelming common case). An ident containing `_` or other
bytes (e.g. `_localeCompare` → `_5flocaleCompare`) does not survive the paste; such
a leaf must be exported under its full hand-mangled symbol.

### 3. App-Rust: `--rust-ffi <dir>`, one crate

`--rust-ffi <dir>` names a directory holding a single `purvasm-foreign` crate
(ADR-0078) that provides **all** app foreigns. The user writes an ordinary Rust
crate against `purvasm-foreign`; the crate exports each `pvf_<mangle(key)>` (the DX
layer's macro handles the mangling Rust-side). One crate, one directory — no
per-module wiring. The crate is a single app-Rust provider entry in the map,
audited (§4) exactly like the app-C objects.

### 4. Build, audit, enforce

- **Mode.** `--rust-ffi` present ⇒ app-Rust; a workspace `.c` sibling found
  alongside `--rust-ffi` is an ambiguous-language error (§Addendum). Absent ⇒ app-C.
- **Symbol-level audit of the app-provider classes, not file presence.** The
  audit targets the classes the **user authors** — app-C (and app-Rust later) —
  because those are untrusted and error-prone: discovering a candidate (a sibling
  `.c`, or the crate) is not proof it satisfies a key, since the `.c` may compile yet
  not export `pvf_<mangle(M.foo)>`. So the build compiles/builds the referenced app
  candidates and **audits their exported symbols** with `nm` (parallel to ADR-0078's
  Rust-crate audit); a `.c` that exports the wrong symbol fails with the key named
  (`no native provider for M.foo`), never a raw linker `undefined symbol`. The
  **packaged** classes are trusted from their declarations — ulib membership from the
  `ulib.json` `foreign` manifest (packaged and tested, ADR-0073), runtime from its
  `nm` export set — not re-audited per build.
- **Enforce.** Over the workspace-namespace keys (§1's scope), require exactly one
  provider across the collected `{runtime, ulib, app-C}` sets — a zero or a duplicate
  is the key-named diagnostic above.

### 5. Wiring

`Build` supplies `NativeLink` with the local workspace source modules (their CoreFn
keys and source paths, §2 — the non-`.spago` cache-db entries) and the app mode (C
siblings vs. the `--rust-ffi` dir), alongside the existing `ulibDir`. `NativeLink`
keeps its **referenced-only** discipline — a candidate `.c` (or the crate) is built
only when some emitted `.ll` references one of its `@pvf_` symbols — then runs the §4
audit and §1 provider-map enforcement before invoking the linker.

## Consequences

- The app author's happy path is zero-config for C (drop `M.c` next to `M.purs`,
  use `PVF_EXPORT`) and one flag for Rust (`--rust-ffi rs`, write a normal
  crate). No hand-authored per-project foreign manifest.
- The CLI grows exactly one flag (`--rust-ffi`); C adds none.
- Losing in-project C+Rust mixing is an accepted, deliberate constraint.
- The ADR-0078 exactly-one-provider invariant is enforced as a **pre-link named
  diagnostic over workspace-namespace keys** (§1), collecting the runtime/ulib/app-C
  classes; a missing or duplicate provider fails by key, not by a raw linker error,
  and correctness never rests on class key-sets being disjoint. Non-workspace
  (registry/runtime) leaves are delegated to the linker + dead-strip — their dead
  etas are stripped, and a live-missing one is a compiler/packaging bug the linker
  still reports.
- Symbol-level auditing (not file presence) of the **user-authored** classes (app-C,
  later app-Rust) means a `.c` that compiles but exports the wrong symbol fails by
  key name, parallel to ADR-0078's Rust `nm` audit; the **packaged** classes (ulib
  manifest, runtime `nm` set) are trusted, not re-audited.
- App-C discovery is scoped to local workspace source modules (non-`.spago`); a
  registry package's `.c` is never treated as app FFI — dependency native providers
  stay packaged-provider work (ulib today, package metadata later). A local *library*
  package is in scope but normally backs its leaves through the runtime/ulib (no
  sibling `.c`), so it is not compiled as app-C.
- ulib-C + app-Rust must co-link: ulib `.c` objects link beside the one Rust
  bundle that folds the app crate with the runtime (ADR-0078). This is the only
  cross-layer link interaction and is already how a Rust foreign links.
- `purvasm.h` gains `PVF_EXPORT` / the `PVF_CAT` helpers and a documented
  `PVF_MODULE` contract; the build injects `-DPVF_MODULE=<escapeIdent(M)>` from the
  **CoreFn module key**, and derives the sibling `.c` path from that module's
  source path — never reconstructing the module name from the file path.

## Alternatives considered

- **`--c-ffi <dir>` symmetric with `--rust-ffi`.** A dir of `.c` named after
  their modules, mirroring the Rust flag. Rejected: C files are siblable, so a
  directory is needless indirection; the sibling convention is PS-idiomatic,
  keeps the provider next to the module it implements (visibility/discovery), and
  costs one fewer flag. The name-mangling burden is identical either way (the
  global `pvf_` symbol), so the dir buys nothing there.
- **Keep boot's borrowed project manifest (mixed C+Rust).** Rejected: manifest
  boilerplate and a wider CLI/`kind` surface for a case users don't want.
- **Per-module Rust crates.** Rejected: one crate for the whole app is simpler to
  declare, build, and fold into the runtime bundle.

## Addendum (2026-07-11): app-Rust implementation

- Status: Proposed _(the app-C path of this ADR is implemented; this addendum is the
  concrete plan for the deferred app-Rust class, for review before implementation)_

§3 named `--rust-ffi <dir>` and the top of §1 listed **app-Rust** as the fourth
provider class; the implementation so far errors on the flag. This addendum pins
*how* app-Rust slots into the provider map, reusing ADR-0078's already-built machinery
rather than inventing anything.

### What already exists (ADR-0078)

The Rust foreign **crate layer is implemented** under `crates/`: `purvasm-sys` (raw
`pv_*`), `purvasm-foreign` (`Ctx<'f>`/`PvValue<'f>` rooted, frame-lifetime-branded,
`!Send`/`!Sync`; `FromPv`/`IntoPv`), `purvasm-foreign-macros` (`#[pv_foreign]`, pure +
`effect`, mangle pinned to the compiler's vectors). A user's `--rust-ffi` dir **is** an
ordinary crate depending on `purvasm-foreign`, whose `#[pv_foreign]` fns export the same
`pvf_<mangle(key)>` symbols a C sibling would. boot already drives the full flow in
`boot/lib/native_link/native_bundle.ml` (ADR-0078 §5) — the **reference to port**.

### The one real difference from app-C: a bundle staticlib, not a sibling `.o`

app-C links a `.o` **beside** the prebuilt runtime staticlib. app-Rust cannot just add a
second Rust staticlib — two Rust staticlibs cannot co-link (duplicate `libstd`). So, per
ADR-0078 §5, the build **synthesizes a `purvasm-bundle` staticlib** (a generated
`crate-type = ["staticlib"]` crate) that folds the **runtime rlib** and the user's
foreign crate into one archive, and links **that in place of** the plain
`libpurvasm_rt.a`. The cargo profile follows `--debug`/release (ADR-0079 stamp). This is
the only wiring change: app-Rust mode swaps the runtime-staticlib link for a
bundle-staticlib build+link; ulib `.c` objects still link alongside it unchanged.

Porting `native_bundle.ml` to `NativeLink` (PureScript, over the `PROC`/`FS` effects):
its `build_bundle` (synthesize the bundle crate, `cargo build` it) and `nm_defined_symbols`
(prefer **`llvm-nm`** — it reads Rust archives cleanly where Apple `nm` warns and exits
non-zero — else `nm`; a tool failure is an error, never an empty result).

### Slotting into the §1 provider map — discovery-agnostic

ADR-0078's own note: "the provider map, exactly-one validation, bundle link, and `nm`
audit are discovery-agnostic and survive unchanged; only where the providers are *found*
differs per level." So app-Rust reuses §1/§4 verbatim:

- **Discovery.** `--rust-ffi <dir>` selects app-Rust: the one crate provides **all** app
  foreigns, and local workspace `.c` siblings are **not compiled**. They are still *scanned*
  only to enforce C-xor-Rust — a workspace module with a sibling `.c` present alongside
  `--rust-ffi` is the §4 ambiguous-language error, not a silently-ignored file.
- **Audit substrate ≠ provider set.** The bundle folds the runtime rlib **and** the app
  crate, so its full `pvf_*` set is **not** the app-Rust provider set — reading it as such
  would conflate runtime and app providers. The bundle `nm` is the *audit substrate*:
  - the **app-Rust provider set is the expected app keys confirmed in the bundle** — a
    defined-symbol **count == 1** per expected key (ADR-0078 §5 / `native_bundle.ml`); a
    referenced app key the bundle does not export fails by key name (a `#[pv_foreign]`
    `module`/`name` typo), never a raw linker `undefined symbol`;
  - the **runtime provider set stays the runtime staticlib's own export set** (its `nm`),
    kept separate — never inferred from the bundle.
- **Enforce.** The §1 exactly-one check over workspace-namespace keys runs against
  `{runtime, ulib, app-Rust}` (app-C absent under `--rust-ffi`). The **cross-class collision**
  — an expected app key whose symbol the **runtime already defines** — is checked *before*
  bundling, against the runtime export set (boot's `check_intrinsic_collisions`), so a Rust
  foreign shadowing an intrinsic is a named duplicate, not a silent archive-member pick.
  `nmDefinedPvf` is the shared substrate for all of this, upgraded to prefer `llvm-nm`.

### Scope / non-goals

- **User/third-party only, opt-in.** app-Rust imposes the cargo/rustc requirement only on a
  user who passes `--rust-ffi`; the default distribution and the C path need neither
  (ADR-0078's "the runtime that *needs* no Rust foreign remains free of one").
- The **entry-package predicate** (§2) and the **`$PURVASM_LIB`-relative runtime
  resolution** (a separate future want) are orthogonal and out of scope here.
- Acceptance mirrors ADR-0078's `rust_foreign_bundle` e2e: a scratch crate's
  `#[pv_foreign]` leaf, bundled, audited, linked, and called through ordinary `pvf_*`
  resolution — now driven by the Level-2 `NativeLink` rather than boot.
