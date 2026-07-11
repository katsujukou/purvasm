# 0091. User-defined native FFI: C siblings vs. one Rust dir

- Status: Proposed
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

### 1. App-level native FFI is **C xor Rust**, and orthogonal to the ulib

The "one language" rule applies **only to the application's own foreigns**. The
ulib is **C-only by construction** and is always linked regardless of the app's
choice (its providers come from `ulib.json`, ADR-0073). So a Rust app that calls
`show` on a `Number` still links the ulib's `Data.Show.c` — the two provider
layers are independent:

    link plan  =  ulib.json `foreign` (library, C, always)
               ∪  app providers        (C siblings  XOR  one Rust crate)

Their key sets are disjoint by module namespace (ulib/dep modules vs. the app's
own modules), so there is never a provider collision to arbitrate.

### 2. C (app): sibling `.c`, auto-discovered, **no flag**

An app module `M` at `…/M.purs` whose foreigns are C-implemented places the
provider at the **sibling** `…/M.c`. The build already knows each own-module's
source path (the module→source map FSR walks), so the sibling is discovered by
swapping the extension — no CLI flag, no manifest. This mirrors PureScript's own
`.purs`/`.js` FFI pairing.

C files *can* sit beside the `.purs` (Rust crates cannot), so the common C case
stays flag-free; the asymmetry with `--rust-ffi` is a property of the two
ecosystems (file vs. crate), not an inconsistency — PureScript itself pairs `.js`
by convention while native crates need a directory.

**Symbol naming.** The provider must export the global symbol
`pvf_<escapeIdent("M.foo")>` (ADR-0073 §3 ABI; `escapeIdent`: alphanumerics pass
through, every other byte → `_<hex>`, e.g. `.`→`_2e`, `_`→`_5f`). To spare users
from hand-mangling, the build compiles each `M.c` with
`-DPVF_MODULE=<escapeIdent(M)>` (the module name from the sibling filename,
pre-escaped to a valid C token, e.g. `Data.Show` → `Data_2eShow`), and
`purvasm.h` provides:

    #define PVF_EXPORT(ident)  PVF_CAT(PVF_CAT(pvf_, PVF_MODULE), PVF_CAT(_2e, ident))

so the user writes `PVF_EXPORT(foo)` as the function name and gets
`pvf_Data_2eShow_2efoo`. The macro is **recommended** and documented; a user who
does not use it is responsible for writing the correctly-mangled symbol by hand.

*Caveat (documented):* token-pasting is correct only when `ident` is purely
alphanumeric (the overwhelming common case — `showNumberImpl`, `parseFloatImpl`).
An ident containing `_` or other bytes (e.g. `_localeCompare` →
`_5flocaleCompare`) does not survive the paste; such a leaf must be exported under
its full hand-mangled symbol.

### 3. Rust (app): `--rust-ffi <dir>`, one crate

`--rust-ffi <dir>` names a directory holding a single `purvasm-foreign` crate
(ADR-0078) that provides **all** app foreigns. It is exactly equivalent to boot
seeing, for every app foreign key `M.foo`:

    { "foreign": { "M.foo": { "kind": "rust-crate", "path": "<dir>" } } }

The user writes an ordinary Rust crate against `purvasm-foreign`; the crate
exports each `pvf_<mangle(key)>` (the DX layer's macro handles the mangling on
the Rust side). One crate, one directory — no per-module wiring.

### 4. Enforcement and validation

- `--rust-ffi` present ⇒ Rust mode: the app's C siblings are **not** consulted.
  If both a `--rust-ffi` dir and any app-module `.c` sibling exist, the build
  **errors** (ambiguous provider language) rather than silently preferring one.
- Absent ⇒ C mode: app foreigns are resolved from siblings.
- In either mode the build can **pre-check** that every app native leaf has a
  provider (a sibling `.c`, or the Rust crate is present) and fail with the
  offending `M.foo` named, instead of surfacing a raw `undefined symbol pvf_…`
  from the linker.

### 5. Wiring

`Build` resolves the app providers — the set of sibling `.c` paths (C mode) or
the single crate dir (Rust mode) — from the own-module source map it already
holds, and passes them to `NativeLink` alongside the existing `ulibDir`.
`NativeLink` keeps its referenced-only discipline: an app `.c` (or the crate) is
compiled/built only when some emitted `.ll` actually references one of its
`@pvf_` symbols.

## Consequences

- The app author's happy path is zero-config for C (drop `M.c` next to `M.purs`,
  use `PVF_EXPORT`) and one flag for Rust (`--rust-ffi rs`, write a normal
  crate). No hand-authored per-project foreign manifest.
- The CLI grows exactly one flag (`--rust-ffi`); C adds none.
- Losing in-project C+Rust mixing is an accepted, deliberate constraint.
- ulib-C + app-Rust must co-link: ulib `.c` objects link beside the one Rust
  bundle that folds the app crate with the runtime (ADR-0078). This is the only
  cross-layer link interaction and is already how a Rust foreign links.
- `purvasm.h` gains `PVF_EXPORT` / the `PVF_CAT` helpers and a documented
  `PVF_MODULE` contract; the build gains the `-DPVF_MODULE=` injection.

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
