# 0056. Two host-capability packages — `purvasm-system` (env, process) and `purvasm-fs` (file IO) — to retire every `Purvasm.CLI.Native.*` leaf name from the backend

- Status: ~~Proposed~~ **Accepted** _(2026-06-29: accepted by the maintainer)_
- Date: 2026-06-29

## Context

ADR-0045 introduced the native IO leaves the CLI's `runPurvasmNative` interpreter discharges to,
naming them `Purvasm.CLI.Native.{readText,exists,writeText,mkdirRec,argv}Impl`; ADR-0055 added
`getenvImpl` to the same family. boot recognises these exact strings in three places —
`Ffi.host` (the VM/oracle native rung), `Ocaml_backend.Codegen_ml` (the native-binary rung), and
the effect-analysis `effectful` list (ADR-0034) — and the CLI declares them as `foreign import`s in
`Purvasm.CLI.Native`.

The names are wrong, and not only for `getenv`: `argv`, `readText`, `writeText`, `mkdirRec`, and
`file_exists` are all general **host-system** operations — OCaml's `Sys` / `In_channel` /
`Out_channel`, or Node's `process` / `fs` — that ADR-0045 leaked into the backend under a
CLI-specific name. Baking a `Purvasm.CLI.*` key into the backend's host registry:

- **misplaces general capabilities behind one consumer** — a non-CLI native program cannot read the
  environment or a file without importing a module named after the CLI;
- **inverts the layering** — the backend's foreign registry should expose *general* host leaves, and
  the CLI should *consume* them, not the reverse;
- leaves the native foreign rung **with no package home**: these leaves are declared ad-hoc in
  `Purvasm.CLI.Native` rather than in a library, unlike the pure ABI which lives in `purvasm-base`.

The FFI provider ladder already names the two rungs that matter here (ADR-0017/0022):

- **Intrinsic rung** — pure, first-order primitives resolved at link to bytecode primops. These
  live in `purvasm-base` (`Purvasm.*`), recognised by the backend as intrinsics (ADR-0038).
- **Native foreign rung** — opaque, effectful host functions resolved at run through the host
  registry (ADR-0022). The env/argv/file leaves are precisely this rung — and have no package.

So `purvasm-base` is the package for the intrinsic rung, and the effectful rung is currently
homeless and named after the CLI. It also spans **two distinct responsibilities** — *process /
environment* (`Sys` / `process`) and *file IO* (`fs`) — that neither host runtime conflates.

## Decision

Re-home the effectful host leaves into **two** in-repo spago packages, split by responsibility —
the effectful counterparts to the pure-ABI `purvasm-base`, each a slice of the native foreign rung
(ADR-0022). Both wrap first-order host leaves in safe `Effect` APIs; both are dual-target.

- **`packages/purvasm-system`** — process & environment info (the OCaml `Sys` / Node `process`
  role; deliberately **not** file IO):
  - `Purvasm.System.Env` — leaf `getenvImpl :: String -> Effect String` ("" = unset), wrapped as the
    safe `lookupEnv :: String -> Effect (Maybe String)` (the empty/unset fold lives in PureScript).
  - `Purvasm.System.Process` — leaf `argvImpl :: Effect (Array String)`, wrapped as `argv`. Room for
    `exit` / `cwd` later.

- **`packages/purvasm-fs`** — file IO (the Node `fs` / OCaml `In_channel`/`Out_channel` role):
  - `Purvasm.FS` — leaves `readTextImpl` / `writeTextImpl` / `mkdirRecImpl` / `existsImpl`, wrapped as
    `readTextFile :: FilePath -> Effect (Maybe String)`, `writeTextFile`, `mkdirp`, `exists`.
    `Maybe`/error composition stays in PureScript (the leaf boundary is first-order, ADR-0020/0036).

Common to both:

- **The leaf is `unsafe`-shaped, the API is safe.** Per the project convention, the raw `*Impl`
  foreigns are not the public surface; the safe wrappers are. (A first-order leaf cannot carry
  `Maybe`, so the wrapper composes it from `exists` + read, as the CLI already does today.)
- **Dual-target (ADR-0038).** Each module ships a `.js` (`node:process` / `node:fs`) so it builds and
  runs on stock `purs`/Node; on purvasm, boot ignores the `.js` and binds each name to a host leaf.
- **boot recognises the general names.** `Ffi.host`, `Codegen_ml`, and the effect-analysis
  `effectful` list key off `Purvasm.System.*` / `Purvasm.FS.*` instead of `Purvasm.CLI.Native.*`. The
  leaf bodies are unchanged (same `Sys.getenv_opt`, `Sys.argv`, `read_file`, `write_file`, `mkdir_p`,
  `Sys.file_exists`); only the recognised key strings move.
- **The CLI consumes the packages.** `runPurvasmNative` discharges the cli-lib `ENV` `Run` effect to
  `Purvasm.System.Env` and the `FS` `Run` effect to `Purvasm.FS`; `Purvasm.CLI.Native` keeps only the
  entry/interpreter wiring and declares **no** `foreign import`s. The Node interpreter (`runNode`,
  over the `node-*` packages, ADR-0045) is untouched.

This **refines ADR-0045 and ADR-0055**: the native leaves move from `Purvasm.CLI.Native.*` into
`purvasm-system` / `purvasm-fs` (a rename + re-home, no behavioural change). The getenv *capability*
(ADR-0055) and the interpreter-swap design (ADR-0045) stand.

## Consequences

- The backend's host registry exposes **general** system and file leaves; any native purvasm program
  can use them, not just the CLI.
- A clear capability taxonomy, each package a single rung/responsibility: `purvasm-base` (pure
  intrinsics, link-resolved) · `purvasm-system` (process/env effects, run-resolved) · `purvasm-fs`
  (file IO effects, run-resolved). The `system`/`fs` split mirrors the host runtimes (neither `Sys`
  nor `process` does file read/write) and keeps each package's responsibility narrow (CLAUDE.md).
- Bytecode codegen is unchanged, so `Image.format_version` does not bump. But the **host-leaf key
  strings change**, so a program that uses these leaves must be built with a native binary and
  `Purvasm.System`/`Purvasm.FS` corefn that agree on the new names (such a program's `.pmo`/`.pmi`
  differ from the old build only in the foreign-name strings).
- Demand-driven growth (ADR-0037) still governs each surface: add `exit` / `cwd` / `command` (system)
  or `readDir` / `unlink` / binary-file ops (fs) only as a phase reaches them.

## Alternatives considered

- **Keep `Purvasm.CLI.Native.*` (status quo).** The smell this record fixes: general capabilities
  named after, and homed in, one consumer; unreusable by non-CLI native programs; layering inverted.
- **One `purvasm-system` package owning file IO too** (the first draft of this record). Rejected:
  `system` then spans two responsibilities the host runtimes themselves keep apart (`process` vs
  `fs`), making the package a grab-bag; the two-package split is the narrower, more faithful design.
- **Put the leaves in `purvasm-base`.** Conflates the pure intrinsic rung (link-resolved primops, no
  effects) with effectful host leaves (run-resolved native rung); the two have different resolution
  mechanisms and a different soundness story (ADR-0034). Separate packages keep the rung distinction
  crisp.
- **A single flat `Purvasm.System` module (no submodules).** `Env`/`Process` submodules keep
  single-responsibility and mirror the namespaced host surface; preferred over one grab-bag module.
- **A first-order `getenv` that returns a richer value (e.g. tagged present/absent).** The leaf stays
  the simplest first-order shape (`String -> Effect String`, "" = unset) and the `Maybe` is composed
  in PureScript, exactly as ADR-0055 specifies and as the file leaves do with `exists`.
