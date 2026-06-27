# 0043. `ulib-tools`: a PureScript CLI to build, interface-verify, and test the `ulib` patches

- Status: ~~Proposed~~ **Accepted** _(2026-06-27: promoted — maintainer authorised implementation; §1 amended to extract shared node-effect helpers into a `cli-lib` package)_
- Date: 2026-06-26

## Context

[0040](0040-ulib-testing-strategy.md) settled the `ulib` *testing strategy* (upstream
suites by representation-seam fidelity; `purs`-side interface verification). It does **not**
say what the tooling is — `ulib-tools/` today is a single `install.sh` that compiles the
patches to corefn ([0038](0038-base-package-and-ulib-patches.md)). To execute 0040 we need
concrete tools, and several implementation facts now constrain the shape:

- **The registry tarballs ship no tests.** The packages resolved under `.spago/p/`
  (`arrays-7.3.0`, `strings-6.0.1`, `prelude-6.0.2`, `foldable-traversable-6.0.0`,
  `enums-6.0.1`, …) contain `src/` only — no `test/`. 0040 §1's "run the package's own
  bundled test suite" therefore requires fetching the suite from the package's **upstream git
  repo at the pinned tag**, not from the registry source.
- **No manifest, no parity suite, no per-module classification exists yet.** `ulib/` is loose
  `.purs` files; `purvasm-base/` is a `spago` package with no `test:` section. 0040 §2 makes
  the `purvasm-base` parity suite the *classifier* that gates the JS-first set, so it is on the
  critical path and must be built first.
- **`purvm` has no default `ulib` location.** The `--ulib` flag exists only on the `native`
  command (boot/bin/main.ml), is always passed explicitly, and resolves a flat
  `<dir>/<Module>/corefn.json` layout with presence-driven last-wins (boot/lib/link/link.ml).
  There is no env var, install prefix, or "distribution" concept — so "bundle the built corefn
  with purvasm" has no home today.

The build-time interface check (0040 §5) needs only the `purs` toolchain and is feasible now;
the link-time externs gate stays deferred (boot cannot read externs).

Maintainer decisions taken for this ADR (the forks 0040 left open):

1. **Implementation language: a PureScript node CLI** (sibling of `cli/`).
2. **Distribution: a default install location with override** — `purvm` resolves `ulib`
   by default; flag/env override it.
3. **The representation-divergent seam (`String`/`Char`) is tested by bespoke UTF-8-aware
   suites**, not by the upstream suite. This *refines* 0040 for that seam (see §5).

## Decision

### 1. `ulib-tools` is a PureScript `spago` package exposing one CLI with three subcommands

Mirroring `cli/` (argparse-basic + node-fs/process + `run`, tested with `spec`/`spec-node`),
so the orchestration logic — manifest parsing, fidelity routing, xfail handling, the docs diff
— is typed and itself unit-testable, rather than growing `sh` + `jq`. The CLI shells out to the
external toolchain (`purs`, `spago`, `git`, `node`, `purvm`) via the Node child-process FFI.

`ulib-tools` is an **independent `spago` package**, not source co-located under `cli/src`: it
must *not* inherit `cli`'s `compiler` dependency (it orchestrates external tools, never links the
compiler library), and it carries its own `spec` test suite (a `spago` package has a single test
main). The node-effect helpers `cli` already owns — `Purvasm.CLI.Effect.{Env,Filesystem,Log}`
and the synchronous Node interpreter/child-process FFI `Purvasm.CLI.Node` — are the genuine reuse
surface, and they are `compiler`-independent. They are therefore **extracted into a shared
`cli-lib` package** that both `cli` and `ulib-tools` depend on (the "extract a shared local
package" option), rather than duplicated. Module names are preserved across the move
(`Purvasm.CLI.*`) to keep `cli`'s imports unchanged; `cli`'s `compiler`-dependent entry modules
(`Main`, `Native`, `Build`, `Compile`, `Options`, `Version`) stay in `cli`.

- **`ulib-tools build`** — supersedes `install.sh`: overlay each `ulib/<package>/<Module>.purs`
  over the resolved registry sources plus `purvasm-base`, compile the lot to corefn with the
  pinned `purs`, and extract the patched modules into the flat lib layout. Adds `--install`
  (write to the default location, §3) and keeps an explicit `--out` for ad-hoc builds.
  `install.sh` is retired once `build` reaches parity.

  > **Progress (2026-06-28):** `install.sh` removed. `ulib-tools build` reached byte-parity with it
  > and now exceeds it — `install.sh` could not stage a patch's declared dependencies (ADR-0047), so
  > it failed on `argonaut-core`'s `purvasm-json` dependency, whereas `build` resolves it.
- **`ulib-tools verify`** — build-time patch-faithfulness (0040 §5): run `purs docs --format
  json` for the upstream module and the patched module and diff the public surface (exports +
  signatures). Independent of boot; catches `STArray(..)`-style surface drift mechanically.

  > **Progress (2026-06-27):** the pinned `purs` (0.15.16) has **no `docs --format json`** (only
  > `markdown | html | etags | ctags`), so `verify` instead compiles both a **registry baseline**
  > and the **patched tree** to corefn and diffs each patched module's **export-name set**
  > (corefn `exports` + flattened `reExports`). This catches the `STArray(..)`-style *export*
  > drift (a patch narrowing the registry surface) but **not type-signature** drift — corefn
  > carries names, not signatures. Signature-level faithfulness needs externs / `purs publish` and
  > is deferred. A ulib module with no registry counterpart (e.g. `Foreign.Object.Internal`,
  > `Data.String.Internal.Utf8` — helper modules of a reimplemented package, ADR-0044) has nothing
  > upstream to narrow and is reported as a *new module*, not a failure. Implemented in
  > `Purvasm.UlibTools.{Verify,Stage}`.
- **`ulib-tools test`** — run each module's behaviour test at the fidelity its manifest entry
  declares (§4–§5), honouring the xfail list, and report per-module pass/xfail/fail.

### 2. A per-package manifest in `ulib-tools/`

Operationalises 0040 §4. One descriptor per patched package
(`ulib-tools/manifests/<package>.json`):

```
{ package, pinnedVersion, repo, testMain, testDeps,
  modules: [ { name, fidelity: "js" | "native" | "bespoke", xfail: [ … ] } ] }
```

`pinnedVersion` is the **upstream git tag** (the suite's source, §4) and pins the same `.purs`
the patch was written against; `repo` is its git URL. `fidelity` is per **module** (the seam is
per-module, not per-package — 0040 §2) with the package supplying the default. `xfail` is the
documented-divergence ledger (0040 §3). `testMain`/`testDeps` absorb the missing cross-package
convention.

### 3. Distribution: a default `ulib` location resolved by `purvm`, written by `build --install`

`purvm` gains default `ulib` resolution with precedence **`--ulib` flag > `$PURVASM_ULIB` env >
built-in default install dir**. The built-in default is an install-relative
`share/purvasm/ulib/` (resolved relative to the executable, with the in-repo dev default
overridable by `$PURVASM_ULIB`). `ulib-tools build --install` writes the extracted corefn there.
This is the smallest change that makes "bundle the corefn with purvasm" real: the artifacts ship
next to the binary and are picked up without a per-invocation flag, while the flag/env still
override for development. The on-disk layout is unchanged (flat `<Module>/corefn.json`), so the
loader (boot/lib/link/link.ml) is untouched; only flag resolution in boot/bin/main.ml changes.
(Scope: applied to the `native` command, which is the self-host build path; the bytecode `build`
command may adopt the same resolution later.)

> **Correction (2026-06-28):** the env var is **`PURVASM_LIB`** (renamed from `PURVASM_ULIB` above),
> and the build flag is **`--prepare-release`** (renamed from `--install`, which read as OS-install).
> `ulib-tools build --prepare-release` resolves `$PURVASM_LIB`, else `dist/ulib`; `purvm`'s matching
> default resolution is still pending (boot change).

### 4. Behaviour tests: upstream suite from git, routed by fidelity

For a `js`/`native` module, `test` shallow-clones the package's upstream repo at
`pinnedVersion` (cached under a `ulib-tools` work dir), overlays the `ulib` patch onto its
`src/`, and runs the package's own `test/` suite — the suite is the oracle (0040 §1):

- **`fidelity: "js"`** (representation-equivalent — arrays, foldable-traversable, unfoldable,
  the structural prelude): build with stock `purs` / `purs-backend-es` and run on node. Sound
  because the `Purvasm.*` JS fallback matches native there (0040 §2).
- **`fidelity: "native"`** (the *candidate* seams `Int` bit-ops/overflow and the `Int`↔`Number`
  conversion of [0041](0041-int-number-conversion-primops.md), `Number` IEEE corners and the
  math leaves of [0042](0042-data-number-math-native-leaves.md) — to be classified by the parity
  suite, §6): build to corefn, link through `purvm`, and run under the standing differential
  discipline against the oracle.

Both honour the module's `xfail` list, so UTF-16-class assumptions that fail by design are
recorded divergences, not red builds (0040 §3).

### 5. The `String`/`Char` seam: bespoke UTF-8 suites (refining 0040 §2 for this seam)

For `fidelity: "bespoke"` modules — confirmed today as `strings/*` and `Char`-using code —
`ulib-tools test` runs **purvasm-authored, UTF-8-aware** suites instead of the upstream suite,
built to corefn and run on `purvm` under the differential discipline. Rationale: the upstream
`Data.String` suite encodes UTF-16 code-unit assumptions so pervasively that running it
natively would be mostly xfail, leaving little signal; a bespoke suite asserts the *intended*
ADR-0006 UTF-8 semantics directly. This **refines** 0040 — which positioned native-upstream +
xfail as the primary path and bespoke suites as reserved for purvasm-only invariants — for the
one seam where the upstream oracle is the wrong oracle. 0040 is not rewritten; this record is
the refinement ([0040](0040-ulib-testing-strategy.md) §2 alternative "hand-write
purvasm-specific suites" is promoted from rejected-as-primary to *chosen for `String`/`Char`*).

### 6. Build order: `purvasm-base` parity suite first

Per 0040 §2 the `purvasm-base` parity suite is the classifier that decides which modules may
ride the JS build, so it is built first: a `test:` section in `purvasm-base/spago.yaml` whose
suite runs each primitive on both the JS fallback and `purvm` and asserts equality for the
representation-equivalent set, flagging `String`/`Char` (and any surprising `Int`/`Number`
corner) as divergent. Its results seed the `fidelity` field of the manifests. Subsequent order:
`verify` (cheap, boot-independent) → `build`/`--install` → `test` per package.

## Consequences

- `ulib-tools` grows from one shell script into a typed, testable CLI consistent with the rest
  of the PureScript-first toolchain; manifest/xfail/fidelity logic lives in PureScript, not
  `jq`.
- The manifests become the single home for per-module fidelity, the pinned upstream tag, and
  the divergence ledger — the explicit operational form of 0040 §3–§4.
- `purvm` gains a real distribution story: corefn ships beside the binary and resolves by
  default, with flag/env override preserved for dev. One small boot change, no loader change.
- A network/git dependency enters the test path (cloning upstream suites at the pinned tag),
  mitigated by a local cache keyed on tag.
- The `String`/`Char` confidence comes from purvasm-authored UTF-8 suites — higher signal than
  an xfail-riddled upstream run, at the cost of authoring those suites (the work 0040 hoped to
  avoid, accepted here only for the divergent seam).
- `purvasm-base`'s parity suite is the first deliverable and unblocks the JS-vs-native routing
  for everything else.

## Alternatives considered

- **Extend `install.sh` with `sh` + `jq`.** Rejected: JSON-manifest parsing, fidelity routing,
  xfail bookkeeping and the docs diff outgrow shell quickly and resist unit testing; the
  project is PureScript-first and `purs`/node are already required.
- **Write `ulib-tools` in OCaml inside `boot/`.** Rejected: it is `purs`/node/`spago`-driven
  build-time glue; homing it in `boot` misplaces the responsibility and would be thrown away
  with `boot` at self-host (cf. [[boot-then-selfhost-bootstrap]]).
- **Commit a `ulib/dist/` of generated corefn / embed corefn in the `purvm` binary.** Rejected
  as the default: committing generated artifacts couples reviews to build output, and embedding
  grows the boot binary and would be re-litigated at the bespoke-native-backend migration. A
  default install dir with override is the lighter, reversible choice.
- **Run the upstream `String` suite natively with a large xfail list.** Rejected for the
  `String`/`Char` seam (see §5): mostly-xfail yields little signal; a bespoke UTF-8 suite tests
  the intended semantics directly. Retained, in principle, for `Int`/`Number` candidate seams,
  where divergences are corner cases rather than pervasive.
- **Vendor upstream test suites into the repo** instead of fetching at the pinned tag. Deferred:
  a cache achieves reproducibility without committing third-party suites; revisit if offline or
  hermetic builds become a requirement.
