# 0057. PureScript-CI: a `spago`/`purs-tidy` gate parallel to OCaml-CI

- Status: ~~Proposed~~ **Accepted** _(2026-06-29: promoted — maintainer authorised implementation after review (`adr-0057-review.md`); blocker (ADR-0056's `purvasm-system`/`purvasm-fs` enumeration) fixed and review notes 1–3 folded into §2/Alternatives before acceptance)_
- Date: 2026-06-29

## Context

CI today covers only the OCaml `boot/` tree. `.github/workflows/ocaml-ci.yaml` builds,
tests, and format-checks `boot/` under `nix develop`, and exposes a single required status
check (`ci-gate`) that branch protection points at. It triggers on every push/PR but gates the
heavy work behind a `dorny/paths-filter` that fires only when `boot/**` changed, so a
docs-only change still produces a green `ci-gate` without running the matrix.

The PureScript half of the repo — the `spago` workspace (`compiler`, `cli`, `cli-lib`,
`packages/purvasm-base`, `packages/purvasm-json`, `packages/purvasm-system`,
`packages/purvasm-fs`, `ulib-tools`, `sample`) — has **no CI**. It
is now the larger and faster-moving surface (the self-host compiler, the JSON core, the
`ulib` tooling) and ships the unit/E2E suites that encode the invariants the type system
cannot (`compiler`: 59 unit + 3 byte-identity E2E; `purvasm-json`: 25; `ulib-tools`: 21).
Regressions there are currently caught only by local runs.

Facts that constrain the shape (verified 2026-06-29 against the pinned toolchain — `purs`
0.15.16, `spago` 1.0.4, `purs-tidy` 0.10.0, all from `flake.nix`):

- **`flake.nix` already provides the full PureScript toolchain** in its `devShell` (`purs`,
  `spago`, `purs-tidy-0_10_0`, `purs-backend-es`, `esbuild`, `nodejs_24`, `pnpm`). The same
  `nix develop` + `cachix` path the OCaml CI uses brings it into `PATH` with zero extra setup
  and a single source of truth for versions.
- **`spago build` over the whole workspace is clean** (0 warnings, 0 errors).
- **The three test-bearing packages pass.** Each carries a `test:` section in its
  `spago.yaml` with a single test `main`: `compiler` → `Test.Unit.Purvasm.Compiler` (+ an
  E2E main `Test.E2E.Purvasm.Compiler`), `purvasm-json` → `Test.Unit.Json.Core`, `ulib-tools`
  → `Test.Unit.Purvasm.UlibTools`. `cli`/`cli-lib`/`purvasm-base`/`purvasm-system`/`purvasm-fs`/`sample`
  have no `test:` section (a `spago` package has a single test main), so they are build-only here.
- **The tree is not `purs-tidy`-clean today.** `compiler`, `cli-lib`, and `sample` have
  unformatted files; `cli`, `packages/purvasm-base`, `packages/purvasm-json`, and `ulib-tools`
  are clean. A strict format gate would therefore be red on day one unless the tree is
  formatted first.

Maintainer decisions taken for this ADR (2026-06-29):

1. **Format the tree clean first, then a strict format gate** — bring-up applies
   `purs-tidy format-in-place` across all packages so the gate is green from the first run,
   matching how OCaml-CI treats `dune build @fmt` as a hard gate.
2. **Toolchain via `nix develop` + `cachix`**, identical to OCaml-CI — one version source
   (`flake.nix`/`flake.lock`), no second pin to drift.
3. **An independent `purescript-ci.yaml`** parallel to `ocaml-ci.yaml`, with its own required
   `ps-ci-gate` check, rather than folding PureScript jobs into the existing workflow.

## Decision

### 1. A new `purescript-ci.yaml`, mirroring OCaml-CI's gate topology

A standalone workflow `PureScript-CI` parallel to `OCaml-CI`, reusing the same three-job
shape so branch-protection semantics are identical and the two halves stay symmetric:

- **`changes`** — `dorny/paths-filter` deciding whether the heavy job runs. The `code` filter
  fires on the PureScript surface: `compiler/**`, `cli/**`, `cli-lib/**`, `packages/**`,
  `ulib-tools/**`, `sample/**`, and the workspace roots `spago.yaml`, `spago.lock`,
  `flake.nix`, `flake.lock`, and `.github/workflows/purescript-ci.yaml` itself. (`ulib/**` is
  loose `.purs` patches compiled by `ulib-tools`, not part of the `spago` build, so it is
  excluded from the build trigger.)
- **`ci`** — runs iff `changes.code == 'true'`. Setup is byte-for-byte the OCaml-CI setup:
  `actions/checkout@v5` → `cachix/install-nix-action@v30` → `cachix/cachix-action@v17` (cache
  name `purvasm`) → `nicknovitski/nix-develop@v1`. Then the PureScript gates (§2).
- **`ps-ci-gate`** — `needs: [ci]`, `if: always()`, red iff `ci` failed or was cancelled,
  green when `ci` passed or was skipped (docs-only). Branch protection adds this as a second
  required check alongside `ci-gate`.

Top-level `on:` is `push` to `**` and `pull_request` (no `paths-ignore`), so `ps-ci-gate` is
**always** produced — otherwise a docs-only PR would never report it and branch protection
would wedge. `concurrency` cancels superseded runs on the same ref, as in OCaml-CI.

### 2. Gate steps: build → test → format-check

Run inside the `nix develop` environment, from the repo root (the `spago` workspace root):

- **Build** — `spago build`. Compiles the whole workspace; fails on any warning-as-error or
  error. (Plain `spago build`; the workspace has no `--strict` configured today, so this
  matches the local contract.)
- **Test** — the three test-bearing packages, by explicit `-p <pkg> -m <main>` so each runs
  its declared suite (a `spago` test main is per-package):
  - `spago test -p purvasm-json -m Test.Unit.Json.Core`
  - `spago test -p ulib-tools -m Test.Unit.Purvasm.UlibTools`
  - `spago test -p compiler -m Test.Unit.Purvasm.Compiler` (unit)
  - `spago test -p compiler -m Test.E2E.Purvasm.Compiler` (the byte-identity E2E oracle)

  The E2E suite is **in-process and fixture-based** — it embeds boot's real `purs 0.15.16`
  `corefn.json` verbatim and compares the lowered `.pmo`/`.pmi` bytes with `shouldEqual`; it
  does **not** shell out to the boot binary (no `ChildProcess`/`exec`). So the workflow needs
  only the PureScript toolchain, never the OCaml one — keeping the two CIs cleanly separated.

  Each is a separate, named step so a failure names the suite. New test-bearing packages are
  added here as they gain a `test:` section.
- **Format check** — `purs-tidy check` over the workspace's tracked `.purs` files. To avoid
  passing a non-existent `test/` path for the build-only packages (a naive
  `purs-tidy check */src */test` errors on a missing path), the step enumerates tracked
  sources and excludes the `ulib/**` patches, e.g.
  `git ls-files '*.purs' | grep -v '^ulib/' | xargs purs-tidy check`. Mirrors the per-package
  `check` script already in the `package.json`s (`purs-tidy check src test`) and OCaml-CI's
  `dune build @fmt`. Pinned `purs-tidy-0_10_0` from the flake, so local `format` and CI
  `check` agree.

### 3. Bring-up: format the tree clean before enabling the gate

Because the format gate is strict (§2) and `compiler`/`cli-lib`/`sample` are currently dirty,
bring-up runs `purs-tidy format-in-place` across all packages (equivalent to each package's
`pnpm format`) as a **separate, format-only commit** preceding the workflow, so the first CI
run is green. The formatting diff is mechanical (whitespace/layout only, no semantic change)
and is committed by the maintainer per the repo's git-ownership convention.

## Consequences

- The PureScript surface gains the same merge-blocking protection the OCaml surface has: no PR
  merges with a broken `spago build`, a failing unit/E2E suite, or an unformatted file.
- Two required checks (`ci-gate`, `ps-ci-gate`) must both be added to branch protection. A
  change touching only one half runs only that half's heavy job; the other half's gate goes
  green via the skip path, so cross-cutting PRs are not double-penalised on time.
- Version drift is avoided: both workflows draw their toolchains from `flake.nix`/`flake.lock`
  through `cachix`, so a toolchain bump is one PR that both CIs pick up.
- A one-time, repo-wide formatting commit lands first. It is large but mechanical; reviewers
  should diff it with whitespace-insensitive view.
- `cli`/`cli-lib`/`purvasm-base`/`purvasm-system`/`purvasm-fs`/`sample` are build- and
  format-checked but not test-run (no `test:` main). When any gains a suite, add a `spago test`
  step. `ulib/**` patches are not
  exercised by this workflow — their build/verify/test path is `ulib-tools` (ADR-0043/0048),
  a separate concern not wired into PR CI here.
- E2E coverage in CI is the `compiler` byte-identity oracle only; the heavier `ulib-tools`
  upstream-suite execution (ADR-0048, network/git-dependent) is deliberately left out of the
  PR gate.

## Alternatives considered

- **Fold PureScript jobs into `ocaml-ci.yaml` under one `ci-gate`.** Rejected (maintainer
  decision 3): the two halves have independent toolchains, triggers, and failure modes;
  separate workflows keep each gate's responsibility legible and let branch protection reason
  about them independently. A single gate would also conflate "OCaml broke" with "PureScript
  broke" in one red check.
- **`purescript-contrib/setup-purescript` instead of `nix develop`.** Rejected (maintainer
  decision 2): it would pin `purs`/`spago`/`purs-tidy` versions a second time, separate from
  the flake, inviting drift between local (`nix develop`) and CI. Reusing `nix develop` keeps
  one source of truth at the cost of Nix evaluation time, already amortised by `cachix`.
- **Soft/non-blocking format gate, or build+test only, deferring `purs-tidy`.** Rejected
  (maintainer decision 1): a non-enforced formatter rots; the project already treats
  formatting as a hard gate on the OCaml side. Formatting the tree once and enforcing
  thereafter is the consistent choice.
- **Explicit `actions/cache` of `.spago`/`output` keyed on `spago.lock`.** Deferred: `cachix`
  already caches the Nix toolchain, and `spago build` over the resolved workspace is fast
  enough that registry/output caching is a later optimisation, not a bring-up requirement.
  Revisit if CI wall-clock becomes a constraint.
- **Gate on warnings via `spago build --strict`/`--pedantic-packages`.** Deferred: the build
  step uses plain `spago build` so CI matches the local contract (warnings — e.g.
  `ImplicitQualifiedImport`, unused imports — are non-blocking today). Promoting warnings to
  errors is a worthwhile tightening but is a separate policy change that would first require
  driving the existing warnings to zero; it is not part of this bring-up. Revisit once the
  tree is warning-clean.
- **Run every package's `test` (including a synthesised `cli`/`sample` suite).** Rejected:
  those packages have no `test:` main today; inventing one for CI's sake is scope the suites
  do not yet justify. They are added when a real suite exists.
