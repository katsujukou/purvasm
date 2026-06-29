# 0055. Resolve `ulib` from `PURVASM_LIB` in the environment (set by the launcher) — no user flag, no binary self-location; overlay the patched corefn in `build`/`compile`

- Status: Accepted
- Date: 2026-06-29

## Context

The Level-1 `purvm` overlays the `ulib` patches over `--corefn-dir` via `Link.load ?ulib_dir`
(`boot/bin/main.ml`), so its `build`/`native` read the **patched** modules (e.g.
`Data.Argonaut.Parser` over `Json.Core`, not the stock registry module that imports the JS foreign
`_jsonParser`). The Level-2/3 CLI (`Purvasm.CLI.Build`) has **no overlay**: `loadClosure` reads only
`--corefn-dir` (default `output/`, the **stock** corefn). So `app build --entry …`:

- compiles against stock modules — its `.pmo`/`.pmi` are **not** what `purvm --ulib` produces (not a
  faithful self-host / Level-4 build); and
- links an `app.pvm` that references unresolved foreigns (stock argonaut's `_jsonParser`) and is
  **stuck at run**.

Two tempting fixes are both wrong:

- **A `--ulib` flag (copy boot).** `ulib` is the toolchain's *own* patched standard library — an
  implementation detail. Making users name it (and point at the right staged directory) leaks that
  detail and is a footgun.
- **The binary locates its own `ulib` (e.g. OCaml `Sys.executable_name`).** The OCaml backend is a
  **bootstrap only — it is not distributed.** Depending on its runtime's executable-path resolution
  bakes a dependency on a layer that goes away: the eventual self-hosted native backend will not
  provide `Sys.executable_name`, so this is a runtime leak / tech debt, not a foundation.

The distribution target is **npm**: a thin JavaScript launcher (the package `bin`) that spawns the
native purvasm bundle. JavaScript has a rock-solid, runtime-agnostic way to locate *itself* —
`import.meta.url` — so the launcher, not the native binary, is the right place to resolve "where am I
installed", and it can hand the answer to purvasm.

## Decision

The **launcher resolves `ulib` and passes it to purvasm through the subprocess environment variable
`PURVASM_LIB`.** No `--ulib` user flag; no binary self-location.

- **Launcher owns location.** The npm `bin` (Node) computes its own directory from `import.meta.url`,
  resolves the bundled `ulib` directory, and **spawns the native binary with `PURVASM_LIB` set in its
  environment** (`spawn(bin, args, { stdio: 'inherit', env: { ...process.env, PURVASM_LIB: ulibDir } })`),
  forwarding argv, stdio, and the exit code. This keeps location resolution in the one place that can
  do it portably, independent of the (bootstrap) native runtime.
- **purvasm reads `PURVASM_LIB`; it never self-locates.** The CLI's `ulib` directory is:
  `PURVASM_LIB` if set (the launcher sets it for npm; a developer sets it for a direct run), else a
  **cwd-relative dev default** (`purvasm_lib`, else `dist/ulib`), else **none** — in which case the
  build proceeds with no overlay and `log`s that it is un-overlaid (a bare-checkout dev run still
  works). No `Sys.executable_name`, no new path-resolution leaf.
- **A minimal environment read.** This re-introduces one environment capability the native
  interpreter dropped as unused (ADR-0045): a `getenv`-style leaf (`Sys.getenv_opt` in the bootstrap;
  `getenv` is a universal libc syscall, trivially provided by any future native backend — unlike
  executable-path resolution, which is platform-specific and runtime-bound). The `build`/`compile`
  commands read `PURVASM_LIB` through this capability.
- **Overlay in `build`/`compile`.** `loadClosure` (and single-module `compile`) resolve each module's
  corefn from the `ulib` directory **first** (the patched module wins), falling back to
  `--corefn-dir` — exactly boot's `Link.load ?ulib_dir`. `--corefn-dir` stays a user flag (the
  *project's* output); `ulib` does not appear in the CLI surface at all.
- **Deployment layout.** Native binaries are shipped per-platform (the npm `optionalDependencies`
  pattern); `ulib` is **platform-independent corefn**, so it ships **once** in the main package, and
  the launcher resolves it (and the platform binary) from `import.meta.url`. `ulib-tools`'
  `--prepare-release` (`$PURVASM_LIB` else `dist/ulib`) stages it into that layout.

## Validation

- **Faithful, runnable build with no flag.** Via the launcher (so `PURVASM_LIB` is set), or with
  `PURVASM_LIB` set by hand, `app build --entry Purvasm.CLI.Native` produces `.pmo`/`.pmi`/`app.pvm`
  **byte-identical to `purvm build --ulib …`** for the same closure (the patched argonaut is used; no
  `format_version` bump), and the produced `app.pvm` **runs** (no `_jsonParser`-stuck) — closing the
  gap this record opens.
- **Degrade, don't crash.** With `PURVASM_LIB` unset and no dev default present, the build runs
  un-overlaid with a clear log rather than failing.
- **No runtime-backend coupling.** Nothing in purvasm resolves its own path; the only environment
  dependency is `getenv`, which any native backend provides.
- **No regression.** Existing `compile`/`build` and the Node entry stay green (unit/E2E).

## Consequences

- Users never see `ulib`: `app build --entry Main` "just works" with the toolchain's patched stdlib,
  matching `purvm`'s behaviour without `purvm`'s flag.
- purvasm does **not** depend on its own executable path, so the design survives the eventual move off
  the OCaml bootstrap to a self-hosted native backend — the launcher (`import.meta.url`) remains the
  locator regardless of how the binary is produced.
- Refines ADR-0045: the native interpreter regains a minimal `getenv` capability (one leaf), used only
  to read `PURVASM_LIB`.
- Establishes the npm layout contract — launcher + single platform-independent `ulib` in the main
  package, per-platform binaries as optional deps; packaging must honour it.

## Alternatives considered

- **A `--ulib` user flag (copy boot).** Leaks an implementation detail and is a footgun (users must
  know the flag and the staged path).
- **Binary self-location via `Sys.executable_name` / `argv[0]`.** Couples to the bootstrap OCaml
  runtime, which is not distributed and whose path-resolution the self-hosted native backend will not
  provide; a leak/tech-debt. The launcher's `import.meta.url` is the portable locator instead.
- **Pass the `ulib` path as an internal argv argument** rather than via the environment. Works, but
  adds hidden surface to the argv namespace (collision / misuse risk) and touches the option parser;
  the environment variable is invisible to argv parsing and is already the override, so it is cleaner.
- **Resolve relative to the current working directory only.** Breaks the moment the binary runs from
  elsewhere; kept only as the *developer* fallback when `PURVASM_LIB` is unset.
- **Embed the `ulib` corefn in the binary.** Heavier, and rebuilds the binary on every `ulib` change;
  a directory resolved by the launcher keeps `ulib` independently rebuildable by `ulib-tools`.
- **Keep reading stock `output/` (status quo).** Produces non-faithful, non-runnable artifacts — the
  problem this record fixes.
