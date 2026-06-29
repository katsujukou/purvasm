# 0048. `ulib-tools test`: per-package upstream-suite execution, JS-fidelity first; native/bespoke deferred

- Status: ~~Proposed~~ **Accepted** _(2026-06-29: promoted — implemented in `ulib-tools` `test` (Phase 1 / `js` fidelity); the `enums` suite passes end-to-end)_
- Date: 2026-06-28

## Context

[0040](0040-ulib-testing-strategy.md) and [0043](0043-ulib-tools-build-verify-test.md) §4–§5 settled
the *strategy* for testing `ulib` patches (run the upstream suite, fidelity per representation seam,
xfail for intended divergences, a per-package manifest); [0047](0047-ulib-package-manifest-extra-dependencies.md)
added the manifest file. This ADR is the concrete `test` implementation. Four facts found while
scoping it shape it:

- **Upstream suites come from git, not the registry.** Registry tarballs ship `src/` only. The
  upstream repos do ship tests — e.g. `purescript-arrays` v7.3.0 and `purescript-strings` v6.0.1 each
  have `test/Test/Main.purs` built on **`Test.Assert`** (Effect + `Console`, throwing on failure). So
  `test` must clone the upstream repo at the pinned ref.
- **The suite is one per-package `Test.Main`, not per-module.** A package's `Test.Main` exercises the
  whole package at once. So **fidelity is naturally per-package**, not per-module — refining
  [0040](0040-ulib-testing-strategy.md) §2's per-module framing (which a per-module-runnable suite
  would have allowed, but the upstream suites are monolithic).
- **The test frameworks are available.** `assert`, `quickcheck`, `spec`, `spec-node` are all in the
  package set (`spec`/`spec-node` already resolved into `.spago/p`; `assert` is in the set, not yet
  resolved).

> **Correction (2026-06-29):** Two implementation-time findings revised the *mechanism* of §2 (the
> *strategy* — clone the pinned upstream suite, run it on node at `js` fidelity — held):
>
> 1. **The clones ship `bower.json` + `pulp`, not a spago workspace.** `purescript-arrays` v7.3.0 (and
>    the other pinned refs) carry `bower.json` and a `package.json`/`pulp` test script — no
>    `spago.yaml`/`spago.dhall`. So "`spago test` inside the clone, using *its own* `spago.yaml`"
>    (§2.3) is not possible as written: `ulib-tools` **generates** a standalone `spago.yaml` in the
>    prepared clone instead (see the revised §2).
> 2. **The standalone-clone approach has a module-collision constraint.** Because the suite package's
>    own modules live in the clone's `src/` (not pulled in as a dependency), a package whose patch
>    pulls a dependency that transitively depends *back* on it collides: `prelude`'s patch imports
>    `Record.Unsafe` (the `record` package), and `record` depends on `prelude`, so the set's `prelude`
>    would be pulled alongside the local one — two `Prelude` modules. `prelude` is therefore **deferred**
>    (it sits at the bottom of the graph; most packages do not hit this). `arrays`/`enums` and other
>    high-in-the-graph packages are unaffected.
> 3. **A `ulib` package directory can span more than one registry package.** `ulib/foldable-traversable`
>    holds `Data.Unfoldable`/`Data.Unfoldable1`, which belong to the `unfoldable` repo — so one
>    directory does not map to one upstream suite. A directory that spans packages needs a multi-suite
>    manifest (or a second `ulib` directory); `foldable-traversable`/`unfoldable` are **deferred** to
>    that follow-up. The clean 1:1 packages (`arrays`, `enums`) are unaffected.
- **`purvm` cannot run a `Test.Assert` suite today.** boot's FFI provides `Effect.Console.log/error`
  but **no `Effect.Exception`/`throwException` leaf and no `Test.Assert` host binding**. An
  assert-based suite throws on failure, which `purvm` cannot perform — so the **native** and
  **bespoke** paths (run the suite on `purvm`) are blocked until `purvm` gains an assertion/exception
  capability or the suite is reshaped to need none. The **JS** path (run on node) is unblocked.

## Decision

### 1. A `test` block in the per-package manifest

Extend `ulib/<package>/ulib.json` ([0047](0047-ulib-package-manifest-extra-dependencies.md)) with an
optional `test` object:

```json
{
  "dependencies": ["purvasm-json"],
  "test": {
    "repo": "https://github.com/purescript/purescript-arrays",
    "ref": "v7.3.0",
    "testMain": "Test.Main",
    "testDeps": ["assert", "quickcheck"],
    "fidelity": "js",
    "xfail": []
  }
}
```

`fidelity` is **per package** (`js` | `native` | `bespoke`) — the refinement above. `repo`/`ref` pin
the upstream suite's source; `testMain` defaults to `Test.Main`; `testDeps` are the suite's extra
dependencies; `xfail` is the documented-divergence ledger ([0040](0040-ulib-testing-strategy.md) §3).
A package with no `test` block is not yet tested.

### 2. Phase 1 — the JS-fidelity path (implemented now)

For `fidelity: "js"` packages — the representation-equivalent ones (`arrays`, `foldable-traversable`,
`unfoldable`, `prelude`, `enums`) — `ulib-tools test`:

1. **Fetch** the upstream repo at `ref` into a cache (`git clone --depth 1 --branch <ref>`), keyed on
   `repo@ref` so re-runs are offline.
2. **Prepare** the clone: overlay the `ulib` patch `.purs` over its `src/`; make `purvasm-base` (with
   its `.js` dual-target fallback, so `Purvasm.*` `foreign import`s resolve on the JS build —
   [0038](0038-base-package-and-ulib-patches.md)) and the declared `dependencies` (e.g.
   `purvasm-json`) available to the clone's build; ensure `testDeps` are resolvable.
3. **Run** the suite with stock `purs` + node — delegating build/dependency-resolution/run to
   `spago test` inside the prepared clone. ~~(its own `spago.yaml` already declares the package's deps
   and test config)~~. Green = pass. Representation-equivalent packages carry **no `xfail`** (a needed
   xfail means the package is mis-categorised and belongs on the native/bespoke path).

> **Revision (2026-06-29):** because the clone has no `spago.yaml` (Correction above), step 3 first
> **generates** one in the prepared clone:
> - **package `dependencies`** = the upstream package's own deps (read from the clone's `bower.json`,
>   `purescript-` prefix stripped) + `purvasm-base` + any `ulib.json`-declared deps ([0047](0047-ulib-package-manifest-extra-dependencies.md));
> - **`test.dependencies`** = the manifest's `testDeps` (the block is mandatory once a `test:` key is
>   present, so it is always emitted, `[]` if empty);
> - **`workspace.packageSet.registry`** = the workspace `spago.yaml`'s registry version (so the suite
>   resolves against the same set);
> - **`workspace.extraPackages`** = `purvasm-base` and each in-repo declared dep, as local `path:`
>   entries (absolute, since spago resolves them against the generated file's own location).
>
> The patched modules are overlaid onto the clone's `src/` (the clone *is* the registry package, so
> they replace their originals in place), and the clone's `test/` is the suite. Clones are cached
> under `.ulib-test-cache/<repo-slug>@<ref>` (gitignored) so re-runs are offline.

### 3. Phase 2 — native, bespoke, and the `purvasm-base` parity suite (deferred)

`fidelity: "native"` (the `Int`/`Number` candidate seams) and `fidelity: "bespoke"` (the `String`/
`Char` UTF-8 suites, [0043](0043-ulib-tools-build-verify-test.md) §5), plus the `purvasm-base` parity
suite ([0040](0040-ulib-testing-strategy.md) §6), all need to **run on `purvm`** — which cannot
execute a `Test.Assert`/exception suite today (Context). They are **deferred** to a follow-up ADR that
first resolves that capability, choosing between:

- **(a) a differential value-capture harness** — the suite (or a bespoke UTF-8 suite) computes a
  *value* that `purvm` runs and `ulib-tools` compares against an expected baseline, so no exceptions
  or `Test.Assert` are needed on `purvm`; or
- **(b) assertion leaves in boot** — add `Effect.Exception`/`Test.Assert` host support so the upstream
  suite runs natively as-is.

Until then `ulib-tools test` reports native/bespoke packages as **skipped (Phase 2)**, naming them so
the gap is visible (no silent omission).

### 4. The command

`ulib-tools test [--package P]` runs the `js`-fidelity packages' suites (Phase 1), honouring each
package's `xfail`, and reports per-package pass / xfail / fail / skipped. With `--package` it restricts
to one. It joins `verify`, `verify-deps`, and `build` in `prepare-release.sh`.

## Consequences

- The bulk of `ulib` — the representation-equivalent packages — gets real upstream-suite coverage
  immediately, on the sound JS build, by leveraging `spago test` rather than re-implementing
  compile-and-run.
- The manifest gains its testing fields (the rest of [0043](0043-ulib-tools-build-verify-test.md) §4),
  co-located with the deps ([0047](0047-ulib-package-manifest-extra-dependencies.md)).
- The `purvm`-can't-assert blocker is surfaced and scoped rather than hit mid-implementation; the
  highest-value seams for native confidence (`String`/`Char`) wait for a deliberate harness decision.
- A git/network dependency enters the test path, mitigated by a per-`repo@ref` cache.
- `fidelity` being per-package (not per-module) is simpler and matches the monolithic upstream suites.

> **Finding (2026-06-29):** the first real run already earned its keep. `enums` passes end-to-end
> (validating the `Purvasm.Char`-backed `Data.Enum` patch on the JS build). `arrays`'s suite, by
> contrast, **fails** at `Test.Data.Array.ST.Partial`: the `ulib` reified `STArray` as an
> `STRef`-wrapped `Purvasm.Array` ([0039](0039-ulib-st-array-and-st-uncurried.md)), but
> `Data.Array.ST.Partial` is an *unpatched* registry module whose JS FFI assumes the native-array
> representation — so it operates on the wrong shape. This is a genuine `ulib` gap the harness
> surfaced (the `arrays` patch needs a `Data.Array.ST.Partial` shadow over the reified representation),
> not a harness bug; `arrays` therefore has **no `test` block yet**, pending that patch. The shipped
> manifest is `enums` only, so the release gate stays green.

## Alternatives considered

- **Manual stage + raw `purs` + node** (mirror `build`/`verify`'s staging, compile to JS, run the
  emitted `testMain`). Rejected for Phase 1 in favour of `spago test` in the clone: spago already
  resolves the package's deps and test config from the upstream `spago.yaml`, so delegating avoids
  re-implementing dependency resolution, JS codegen, and entry-running. (Kept as the fallback if
  injecting `purvasm-base`/`purvasm-json` into the clone's workspace proves awkward.)
- **Run everything on `purvm` now** (native for all). Rejected: `purvm` cannot perform the
  `Test.Assert` throw, so it cannot run these suites yet; forcing it would mean stubbing assertions
  into a custom runner before the harness is designed (Phase 2).
- **Per-module fidelity + per-module suites.** Rejected: upstream suites are one monolithic per-package
  `Test.Main`; per-module fidelity has nothing per-module to run. Per-package fidelity matches reality.
- **Vendor upstream suites into the repo.** Deferred (as in [0043](0043-ulib-tools-build-verify-test.md)):
  a `repo@ref` cache gives reproducibility without committing third-party tests.
