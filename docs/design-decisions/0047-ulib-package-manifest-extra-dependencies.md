# 0047. `ulib` package manifest: per-patch extra dependencies (in-repo or registry), with validation

- Status: Accepted
- Date: 2026-06-27

## Context

A `ulib` patch ([0038](0038-base-package-and-ulib-patches.md)) is an interface-compatible patch
over a registry package, written in PureScript over `Purvasm.*` (`purvasm-base`). Its dependency
surface was assumed to be **the registry package's own deps plus `purvasm-base`** — so `ulib-tools`
stages the whole resolved `.spago/p` (every registry package the workspace pins) plus
`packages/purvasm-base/src`, then overlays the patches and compiles ([0043](0043-ulib-tools-build-verify-test.md)).

[0046](0046-argonaut-core-pure-purescript-ulib.md) breaks that assumption. Its `argonaut-core`
patch reimplements `Json` over a **new in-repo package**, `packages/purvasm-json`
(`Json.Core.{Types,Parser,Printer}`) — a dependency the registry `argonaut-core` never had.
`ulib-tools` does not stage it, so `build`/`verify`/`install.sh` all fail:

```
in module Data.Argonaut.Parser
  Module Json.Core.Types was not found.
ulib-tools: Command failed: purs compile --codegen corefn …
```

So a `ulib` patch needs a way to declare dependencies **beyond** its registry baseline. Two
requirements shape the mechanism:

- **Source-agnostic declarations.** `purvasm-base` and `purvasm-json` are in-repo today but will be
  published as independent registry packages in time. A declaration must therefore name a package
  *without* committing to where it comes from: the same `dependencies: ["purvasm-json"]` should
  resolve from `packages/` today and from the registry once published, with no manifest change. So a
  patch must be able to declare **both in-repo and registry** packages, uniformly.
- **Validated.** A declared dependency that resolves to nothing (typo, or a package that is neither
  in-repo nor on the registry), or that introduces a dependency **cycle**, must be a loud, early
  error — not an opaque `purs` "Module not found" deep in the staged compile.

## Decision

### 1. An optional per-package manifest, co-located with the patch

Each `ulib` package directory may carry `ulib/<package>/ulib.json`. It realizes (and relocates) the
per-package descriptor [0043](0043-ulib-tools-build-verify-test.md) §4 sketched; the testing fields
(`testMain`, `xfail`, per-module `fidelity`, pinned upstream tag) will join it when `test` is built.
For now one field:

```json
{ "dependencies": ["purvasm-json"] }
```

`dependencies` lists package names the patch needs in addition to the registry baseline and
`purvasm-base`. The names are **source-agnostic** — the manifest does not say in-repo vs. registry;
resolution decides.

### 2. Resolution: in-repo first, then registry

For each declared name, `ulib-tools` resolves in order:

- **In-repo** — `packages/<name>/` exists ⇒ stage `packages/<name>/src`, and **recurse** into that
  package's own `dependencies` (from `packages/<name>/spago.yaml`) so transitive in-repo deps are
  staged too. (Its registry deps are already staged via `.spago/p`.)
- **Registry** — otherwise the name must be a registry package. Its source is already in the staged
  `.spago/p`, so no extra staging is needed; resolution only confirms it is present.

A `--packages-dir` (default `packages`) locates in-repo packages; `purvasm-base` remains the
always-staged base ABI.

### 3. Validation, before staging — hard errors

> **Correction (2026-06-28):** the standalone command is **`ulib-tools verify-deps`** (renamed from
> `validate`).

`build`/`verify` run a validation pass first (also available on its own as `ulib-tools verify-deps`)
that aborts on:

- **Unresolvable dependency** — a declared name that is **neither** an in-repo package
  (`packages/<name>`) **nor** a registry package (present in the package set, read via
  `spago ls packages`). Reported as a hard error naming the offending package and the declaring
  patch. (Diagnostic refinement: a name that *is* in the package set but is not resolved into
  `.spago/p` is reported distinctly — "registry package X is not resolved; add it to the workspace's
  dependencies" — rather than as unknown.)
- **Circular dependency** — the dependency graph over the **authored** packages contains a cycle.
  Nodes are the `ulib`-patched packages (edges from their manifest `dependencies`) and the in-repo
  packages (edges from their `spago.yaml` `dependencies`); registry packages are external leaves and
  terminate traversal. The reported error names the cycle path (e.g. `argonaut-core → purvasm-json →
  argonaut-core`).

### 4. Declaring registry deps is supported and recommended, though not required for availability

Because the whole resolved `.spago/p` is staged, a registry package is *available* without being
declared. Declaring it anyway is supported and encouraged: it documents the patch's true surface,
lets validation guard it, and — crucially — makes the declaration forward-compatible, so an in-repo
package that later moves to the registry needs no manifest edit. Manifests are optional; absent ⇒ no
extra deps (the prior behaviour).

## Consequences

- `ulib/argonaut-core/ulib.json` declares `{ "dependencies": ["purvasm-json"] }`; it resolves
  in-repo today and from the registry unchanged once `purvasm-json` is published.
- A patch's beyond-baseline dependency surface becomes **explicit, source-agnostic, and validated**.
  Typos, unknown packages, unresolved registry packages, and dependency cycles fail fast with
  actionable messages instead of an opaque `purs` "Module not found".
- Cycle detection catches authoring mistakes (a patched package and an in-repo package depending on
  each other) before `purs` does, and names the path.
- The manifest is the same descriptor [0043](0043-ulib-tools-build-verify-test.md) §4's testing
  metadata will use — one file per package.
- Cost: `ulib-tools` reads N manifests plus each in-repo dependency's `spago.yaml`, and queries the
  package set once for validation; it stages only the declared in-repo packages (the registry set and
  `purvasm-base` are staged as before).

## Alternatives considered

- **In-repo-only declarations** (this ADR's first draft). Rejected by the maintainer: `purvasm-base`
  and `purvasm-json` will be published, so a declaration must be source-agnostic and registry
  packages must be declarable; special-casing in-repo would force a manifest rewrite at publish time.
- **Stage every `packages/*/src` unconditionally.** Rejected: it compiles every in-repo package into
  every `ulib` build, so a WIP/broken local package breaks unrelated patches, and it hides each
  patch's true dependency surface. The maintainer asked for a *declared* mechanism, not ambient.
- **Skip validation, let `purs` fail.** Rejected: `purs` reports "Module X not found" without
  distinguishing a typo from an unresolved-registry package, and cannot name a dependency cycle
  clearly. An explicit validate gives actionable errors and is cheap.
- **Keep the manifest in `ulib-tools/manifests/<package>.json`** ([0043](0043-ulib-tools-build-verify-test.md)
  §4's stated location). Chose co-location at `ulib/<package>/ulib.json` instead: a patch's deps are
  intrinsic to the patch, so patch + descriptor belong together. Refines §4's location.
- **A real `spago.yaml` per `ulib` package.** Rejected: `ulib` packages are *staged and compiled with
  raw `purs`*, not built by `spago`; a full `spago` package implies a build lifecycle `ulib` does not
  have, and most of its fields would be inert.
- **Infer the extra deps from the patch's `import`s.** Rejected: mapping an import to its providing
  package is fragile and implicit; an explicit declaration is the contract a reviewer can check.
