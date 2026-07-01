# 0065. `ulib` directory = one registry package: split `foldable-traversable`, testing `unfoldable` on its own

- Status: Proposed
- Date: 2026-07-01

## Context

The [0048](0048-ulib-tools-test-upstream-suite-execution.md) `test` command clones one upstream
`purescript-*` repo per `ulib` directory, overlays that directory's patches, and runs the repo's
suite. This assumes **one `ulib` directory maps to one registry package** — true for every directory
(`arrays`, `enums`, `prelude`, `strings`, `integers`, `foreign-object`, `argonaut-core`) **except**
`ulib/foldable-traversable`, which holds five modules spanning two registry packages:

- `Data.Foldable`, `Data.FunctorWithIndex`, `Data.Traversable` — the `foldable-traversable` package;
- `Data.Unfoldable`, `Data.Unfoldable1` — the `unfoldable` package.

So `foldable-traversable` cannot carry a single `test` block: one clone (`purescript-foldable-traversable`)
cannot host both suites, and `unfoldable`'s upstream suite never runs. [0048](0048-ulib-tools-test-upstream-suite-execution.md)
recorded this as deferred ("a directory that spans packages needs a multi-suite manifest, or a second
`ulib` directory"). This ADR chooses between those two.

Facts found while scoping (both suites prototyped against a prepared clone before this ADR):

- All five modules are **registry modules** (none are `ulib`-introduced, unlike
  `Foreign.Object.Internal`) — each exists in its owning repo's `src/` at the pinned ref.
- `unfoldable` 6.0.0 already depends on `foldable-traversable`, so `Data.Unfoldable`'s import of
  `Data.Foldable`/`Data.Traversable` is covered — the two packages test **cleanly in isolation** (no
  cross-collision: `foldable-traversable`'s deps never pull `unfoldable`, and `unfoldable` pulls the
  *registry* `foldable-traversable`, whose modules do not clash with the local `Data.Unfoldable*`).
- Both suites (`Test.Main`, `Test.Assert`) pass on the JS build with only their `bower` deps +
  `purvasm-base` — no extra manifest `dependencies`, no `xfail`. `foldable-traversable`'s test deps are
  `assert`/`console`/`integers`/`unsafe-coerce`; `unfoldable`'s are `assert`/`console`.
- `foldable-traversable` is the **only** multi-package directory in the current `ulib`.

## Decision

**Restore the one-directory-per-registry-package invariant by splitting the directory**, rather than
teaching the manifest and `test` command about multiple suites per directory.

- Move `Data.Unfoldable.purs` and `Data.Unfoldable1.purs` from `ulib/foldable-traversable/` into a new
  `ulib/unfoldable/`.
- `ulib/foldable-traversable/ulib.json` gets a `test` block: repo `purescript/purescript-foldable-traversable`,
  ref `v6.0.0`, `testDeps` `assert`/`console`/`integers`/`unsafe-coerce`, `fidelity` `js`, no `xfail`.
- `ulib/unfoldable/ulib.json` gets a `test` block: repo `purescript/purescript-unfoldable`, ref
  `v6.0.0`, `testDeps` `assert`/`console`, `fidelity` `js`, no `xfail`.

This needs **no change to `ulib-tools`**: `build` and `verify` already glob all patches regardless of
directory, and `test` already treats each directory as one package. `foldable-traversable` and
`unfoldable` join `arrays` and `enums` in the `prepare-release.sh` `test` gate.

## Consequences

- Every `ulib` directory now maps to exactly one registry package — a single invariant that `build`,
  `verify`, `verify-deps`, and `test` can all rely on, and under which **all** of a directory's modules
  (including any future `ulib`-introduced module with no registry counterpart) overlay onto that
  directory's one clone with no special handling.
- `unfoldable` gains real upstream-suite coverage it had none of before; `foldable-traversable` too.
- The directory grouping (`Data.Unfoldable*` alongside `Data.Foldable*`) is lost as a filesystem
  cue. The relationship is conceptual, not a build dependency; it is not worth a manifest+tooling
  mechanism to preserve as co-location.

## Alternatives considered

- **A multi-suite manifest** — make `ulib.json`'s `test` a list of suites, each with its own
  `repo`/`ref`/`testDeps`, and have `test` overlay onto each clone only the modules that clone owns.
  Rejected as the wrong trade for the current `ulib`:
  - It is generality for a **single** case (`foldable-traversable` is the only multi-package directory);
    the split fixes that case with zero code.
  - Assigning modules to suites needs either explicit per-suite ownership (manifest + command
    complexity) or auto-detection by "module present in the clone's `src/`" — but auto-detection
    silently drops a `ulib`-introduced module (no registry counterpart, so absent from every clone),
    which must be overlaid for its package to compile. The split has no such failure mode: one
    directory, one clone, all modules overlaid.
  - It keeps a directory that violates the otherwise-universal one-package invariant, so every tool
    keeps carrying the "a directory may be several packages" case.

  Revisit this only if multi-package directories become common **and** carry `ulib`-introduced modules —
  neither is true today.
- **Test only `foldable-traversable`, leave `unfoldable` overlaid-but-untested.** Rejected: it is
  silent omission of a package that has a perfectly runnable suite
  ([0048](0048-ulib-tools-test-upstream-suite-execution.md)'s no-silent-omission principle).
