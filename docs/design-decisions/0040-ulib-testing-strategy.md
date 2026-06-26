# 0040. `ulib` testing: upstream suites by representation-seam fidelity; `purs`-side interface verify

- Status: Accepted
- Date: 2026-06-26

## Context

`ulib` modules are **interface-compatible patches** on registry packages
([0038](0038-base-package-and-ulib-patches.md)): a foreign-impl completion or a
reimplementation over `Purvasm.*`, never a from-scratch API. How to *test* them is a
project-wide question, not a per-module one, so it is settled here before the suites grow.

Three facts shape the strategy:

- **A patch is behaviour-preserving by definition.** So the natural oracle is **upstream's
  own test suite**: clone the patched package, overlay the patch, run the suite the package
  already ships. This is differential testing against the upstream spec (the purs-wasm
  `ulib`-tooling route).
- **purvasm deliberately diverges on `String`/`Char`.** ADR-0006 makes `String` a UTF-8 byte
  sequence and `Char` a code point; `purvasm-base`'s JS fallback for `Purvasm.String`/`Char`
  is **UTF-16 code units** (the right notion for JS-hosted code, and on stock `purs` the
  *registry* `Data.String` is used, not the shadow). So the dual-target parity that the
  strategy leans on **holds for arithmetic/structural primitives but not for `String`/`Char`**;
  `Int` (OCaml 63-bit vs JS 32-bit bit-ops/overflow) and `Number` (`-0`/`NaN`/`Infinity`) are
  further *candidate* seams.
- **The interface-compatibility gate is deferred** ([0038](0038-base-package-and-ulib-patches.md)
  §3): boot cannot read externs yet, so it *assumes* a reached patch is interface-compatible.

## Decision

### 1. Behaviour test = upstream suite over the patched package

For each patched package, clone its **pinned** registry source, overlay the `ulib` patch, and
run the package's **own bundled test suite**. The suite is the oracle; the patch passes iff it
reproduces upstream behaviour (modulo §3).

### 2. Fidelity is chosen per module by the representation seam it touches

Dual-target equivalence differs by seam, so the build target for a module's test is conditional:

- **Representation-equivalent** modules (arrays, foldable-traversable, unfoldable, the
  structural prelude — `Functor`/`Apply`/`Bind`/`Eq`/`Ord` shape) → test on the **JS build**
  (stock `purs` / `purs-backend-es`): cheap and fast, sound because the `Purvasm.*` JS fallback
  matches native there.
- **Representation-divergent** modules (`strings/*` and `Char`-using code — *confirmed*; `Int`
  bit-ops/overflow and `Number` IEEE corners — *to be classified*) → test on the **native build**
  (boot / purvasm) under the standing differential discipline. A JS build of these is **unsound**:
  e.g. a `Data.String` shadow's UTF-8 codec runs over UTF-16 JS primitives and yields neither a
  real bug nor an ADR-0006 divergence, but a third false signal from the test rig itself.
- **`purvasm-base`'s own parity suite is built first** and does double duty: it *guarantees*
  JS-fallback ≡ native for the equivalent set, and it is the *classifier* that decides which
  modules may ride the JS build and which require native.

### 3. A per-package expected-divergence (xfail/skip) list

Because purvasm intentionally changes some semantics (ADR-0006), parts of an upstream suite
**fail by design** (UTF-16 assumptions on a UTF-8 string). The harness declares these as
expected divergences rather than requiring all-green; the list is the explicit home for the
*documented exceptions* to the behaviour-preserving claim.

### 4. A small per-package manifest in `ulib-tools`

No cross-package test convention exists, so `ulib-tools` carries a descriptor per patched
package: `{ package, pinnedVersion, testMain, testDeps, xfail[] }`. `pinnedVersion` is needed
anyway (which version's suite to clone — the patch keeps a specific version's `.purs`), and
`xfail` is §3; the rest absorbs the missing convention (entry module, extra test deps).

### 5. Interface verification, split in two

- **Build-time patch-faithfulness** — does the `ulib` module's public surface (exports +
  signatures) match the registry module's? Run **in the `purs` toolchain** (`purs docs --format
  json`, or the externs, for both the upstream and the patched module, then diff), as an
  `ulib-tools verify` step. This is **independent of boot** (`install.sh` already drives `purs`)
  and is feasible now; it mechanically catches the `STArray(..)`-style surface drift.
- **Link-time presence-driven compatibility gate** — boot reading externs to confirm a reached
  patch matches the user's module ([0038](0038-base-package-and-ulib-patches.md) §3) — **stays
  deferred** (boot-externs blocked); the PureScript self-host enforces it later.

## Consequences

- A principled, low-authoring-cost test path: upstream suites are the oracle; we write
  manifests and xfail lists, not new assertions.
- The fidelity ladder is explicit and *justified per module*, not a blanket "JS for speed":
  String/Char get native confidence where it matters most.
- `purvasm-base`'s parity suite is on the critical path — it gates the JS-first set — so it is
  the first thing built.
- Part of the externs work is unblocked now (build-time faithfulness diff), decoupled from the
  boot-externs blocker that holds the runtime gate.
- The behaviour-preserving claim gains an explicit exceptions ledger (the xfail lists), keeping
  ADR-0006-class divergences visible rather than silently tolerated.

## Alternatives considered

- **One uniform JS build for all `ulib` tests** (assume dual-target parity everywhere). Rejected:
  the `String`/`Char` seam (and candidate `Int`/`Number` seams) makes a JS build of those shadows
  produce test-rig artifacts, not signal. Fidelity must be per-seam.
- **Hand-write purvasm-specific suites per `ulib` module.** Rejected as the primary path: high
  authoring cost and it re-derives the upstream spec, drifting from it. Upstream suites are the
  better oracle; bespoke tests are reserved for purvasm-only invariants not covered upstream
  (e.g. growable-buffer capacity edges in [0039](0039-ulib-st-array-and-st-uncurried.md)).
- **Invest in a faithful UTF-8 JS fallback for `Purvasm.String`/`Char`** so the JS build becomes
  uniformly sound. Deferred: testing-only motivation; native differential is the higher-fidelity
  end-state for strings anyway, and the JS fallback's UTF-16 shortcut is the right notion for
  actual JS-hosted code.
- **Wait for the boot-externs compatibility gate before any interface check.** Rejected: the
  build-time faithfulness diff needs only the `purs` toolchain; coupling it to the deferred
  boot-externs work needlessly delays a cheap, high-value check.
