# 0090. Foreign signatures threaded through the build env

- Status: ~~Proposed~~ **Accepted** _(2026-07-10: accepted by the maintainer)_
- Date: 2026-07-10

## Abstract

Wire foreign-signature reconstruction (ADR-0080's FSR) into the `build` driver and thread each module's
**`ForeignShape`s** — the *whole* shape `{ arity, vsat, retVsat }`, not arity alone — as a build-driver
fact, mirroring how `DictMachinery` is threaded (ADR-0085/0086) in *shape* but not its optimiser-only home.
After the load phase, if a module's `foreignNames` is non-empty the driver **selectively** fires FSR for it;
the module's own shapes and its dependencies' exported shapes are assembled as build-driver-level
**`ForeignFacts`** and **always** attached to `LoweredModule.foreignSigs`, so codegen has them in **every
mode**. Under `--opt` the same shapes are **additionally** exposed to the optimiser's `LocalFacts`/`BuildEnv`
(for effect analysis, ADR-0034). On completion, the module's **exported** shapes fold into `ForeignFacts`
for its dependents.

## Context

- The native backend team is bringing up **user-module C FFI**. A `foreign import f :: …` in a user module
  needs its **calling shape** at build time: codegen lowers `AForeign f` and recognises native leaves by
  `arity` (ADR-0073), and the optimiser's effect placement (ADR-0034) needs `vsat`/`ret_vsat` (does
  saturating `f` perform? is the saturated result a deferred effect?).
- ADR-0080 reconstructs exactly this — `ForeignShape { arity, vsat, retVsat }` per foreign, from source via
  the CST parser (`Purvasm.Compiler.ForeignSig.reconstructModule`) or from a ulib-overlaid module's
  validated `foreignSigs` (`Purvasm.CLI.ForeignSigs.moduleForeignSigs`, already **module-local and
  selective** — empty when `foreignNames` is empty). But ADR-0080 §3/§4 left the **build-time consumer**
  pending: today FSR runs only under `--check-foreign-sigs` as a diagnostic, feeding nothing into the build.
- What *is* wired is a stopgap: the CLI passes the **ulib native-foreign manifest** arities
  (`Map String Int`) into `llvmBackend` as `foreignArity`, for ulib `.c` leaves only. That is **arity-only**
  (no effect bits) and **ulib-only** (no user-module foreigns) — insufficient for both the optimiser and
  user C FFI. **The LLVM-driver-local arity plumbing (added by the backend team ahead of need) is a
  transitional backend workaround; this record defines the backend-neutral build-driver ownership that
  *supersedes* it.** Foreign shape is a build-process fact, owned by the build driver — not a backend
  detail; the Driver-local path is to be **absorbed and replaced**, not conformed to. (Read this ADR as
  "restore the driver to the right shape," not "legitimise the current Driver.")
- The seam already threads one cross-module fact this way (`DictMachinery`, ADR-0085/0086): dependency facts
  in `BuildEnv`, current-module facts in `LocalFacts`, a `BuildSummary` folded to dependents by
  `extendSummary`, with ADR-0084's self-pollution invariant kept structurally. Foreign shapes are a second
  fact of exactly this shape.

## Decision

### 1. The threaded fact is the whole `ForeignShape`, not arity

The build fact flowed is `Map String ForeignShape` (`ForeignShape = { arity, vsat, retVsat }`, qualified key
→ shape, `Purvasm.Compiler.ForeignSig`) — the **whole shape**, one source of truth: arity for codegen
(`shape.arity`), `vsat`/`ret_vsat` for the optimiser's effect analysis (ADR-0034). The standalone
arity-only `foreignArity` path is subsumed: codegen reads `arity` from the threaded shapes; the ulib
manifest is retained **only for `.c` linking** (its `sources` paths), not for arity. *Where* the shapes are
threaded — a build-driver fact in every mode, additionally surfaced to the optimiser under `--opt` — is
§3.

### 2. Selective FSR firing after load, via a `CompilerAction` capability — with a failure channel

FSR is a host effect (reads `.purs` source / the ulib `foreignSigs`) that can **fail** (parse error, missing
provenance, manifest decode). ADR-0087's build boundary is *no throw — halt with a returned `Left
BuildError`*, so the capability returns a failure as data, and FSR failure is a first-class `BuildError`
(never collapsed to an empty map — `ForeignSig.reconstructModule` is a hard diagnostic, ADR-0080):

```
CompilerAction.foreignSigsOf :: CF.Module -> m (Either ForeignSigError (Map String ForeignShape))
data BuildError = … | ForeignSigFailed ModuleName ForeignSigError
```

`ForeignSigError` is a concrete library type (the reduced FSR issue, as `LoadError` is for loads — the host
maps its richer original into it). The host implements the capability with `ForeignSigs.moduleForeignSigs
env` (over its `FS` stack), mapping a reconstruction failure to `Left`. The driver calls it **only when
`mod.foreignNames` is non-empty** (a cheap fast-path; `moduleForeignSigs` also returns empty for a
foreign-free module, so the guard is not a correctness dependency), and on `Left` halts the build with
`ForeignSigFailed`.

### 3. Foreign shapes are a **build-driver** fact, threaded in **every mode** — not optimiser-`LocalFacts`

Codegen needs the shape (arity, native-leaf recognition) under `--no-opt` too, but the seam's
`LocalFacts`/`BuildEnv` and the `optimizeModule` fixpoint run **only under `--opt`** (ADR-0086 Addendum: the
neutral driver is the identity under `--no-opt`, so `localFactsOf` is computed only inside `runOptimizer`).
Putting own shapes on the optimiser's `LocalFacts` would therefore lose them for `--no-opt` codegen. So
foreign shapes are threaded at the **build-driver level, in both modes**, mirroring `DictMachinery`'s
*shape* but not its optimiser-only home:

- The driver maintains a **`ForeignFacts` env** — the accumulated **dependencies'** exported shapes —
  threaded through the module fold in **both** modes.
- Per module, after `loadModule`/`declsOfModule`: own shapes = `foreignSigsOf mod` (when
  `foreignNames ≠ ∅`, else empty); the module's **visible** shapes = `ForeignFacts` (deps) ∪ own.
- The driver puts the **visible** shapes on `LoweredModule.foreignSigs` (§4) — **always**, so codegen has
  them under `--no-opt` and `--opt` alike.
- On completion, the module's **exported** shapes (own ∩ CoreFn `exports`) fold into `ForeignFacts` for
  dependents. Export-filtering (unlike `DictMachinery`, which publishes all) is deliberate: a foreign a
  module does not export is unreferenceable by dependents.

**Two-tier for the optimiser.** Only **under `--opt`**, the driver *additionally* surfaces the same shapes
to the optimiser — own on `LocalFacts.foreignSigs`, deps on `BuildEnv.foreignSigs` — so `optimizeModule`'s
effect analysis (ADR-0034) reads them. This keeps ADR-0086's "`LocalFacts` = *optimiser*-stable facts"
honest (foreign shapes are stable — no pass mutates them — and reach the optimiser only where it runs), and
avoids the loss above by not making `LocalFacts` the primary carrier.

`ForeignFacts` (and `BuildEnv`) carry dependency shapes only, so ADR-0084's self-pollution invariant holds.
`foreignSigs` and the CoreFn `exports` are **source-derived, not `AnfModule`-derived**, so the *driver*
(effectful) computes and injects them; `localFactsOf` (pure, `AnfModule`-only) is unchanged.

### 4. Codegen exposure

`LoweredModule` gains `foreignSigs :: Map String ForeignShape` — the module's **visible** shapes
(`ForeignFacts` deps ∪ own), assembled by the driver and populated in **both** modes (§3), so
`Backend.lowerModule` / `lowerEntry` read `arity` (for `AForeign` lowering + native-leaf recognition) from
it under `--no-opt` and `--opt` alike. The `LlvmBackendOptions.foreignArity` / manifest-arity input to
`llvmBackend` is retired for arity (the shapes supply it); the manifest stays only as the `.c` link plan.

## Consequences

- The optimiser's effect analysis (ADR-0034) has the `vsat`/`ret_vsat` it needs, for both own and imported
  foreigns, at the seam — not blocked on an arity-only map.
- User-module C FFI's **signature half** is solved: a user `foreign import`'s **shape/arity** is
  reconstructed and reaches codegen and the optimiser without a ulib manifest entry. This is **only the
  signature metadata** — the `.c` source location, the exactly-one-provider check, and the link plan are
  **separate provider/source metadata** and stay ADR-0073/0080's concern (this record does not solve them).
  Signature metadata ≠ provider source metadata; 0090 solves the former only.
- One source of truth for foreign shape; the arity-only `foreignArity` plumbing is subsumed (manifest kept
  for linking only). Byte-identity of the native `.ll` is unaffected for foreign-free modules (FSR does not
  fire); a module *with* foreigns already went through the manifest/`AForeign` path, now shape-driven.
- Extends ADR-0080 (its named build-time consumer lands), ADR-0085/0086 (foreign shapes thread like
  `DictMachinery` in *shape*, but as a **build-driver fact in every mode** — the optimiser's `BuildEnv`/
  `LocalFacts` carry them only under `--opt`, §3), and ADR-0087 (`CompilerAction` gains `foreignSigsOf`
  returning `Either ForeignSigError …`; `BuildError` gains `ForeignSigFailed`; `LoweredModule` gains
  `foreignSigs`).
- FSR is CST-lexing (minutes on large frontiers, ADR-0080) — but now only for modules that actually declare
  foreigns, and only once per build per module, folded into the existing load/compile pass rather than a
  separate sweep.

## Alternatives considered

- **Keep the arity-only `foreignArity` map** (the current stopgap). Rejected: no effect bits (breaks the
  optimiser's ADR-0034 analysis) and ulib-only (no user C FFI) — the two things this record exists to fix.
- **A whole-program foreign-shape map in the backend `context`** (not env-threaded). Simpler, but breaks the
  per-module modularity/self-pollution discipline the seam holds for every other cross-module fact, and
  hands the optimiser a whole-program map it must not depend on (ADR-0084). The env threading is the
  established pattern.
- **Fire FSR for every module unconditionally.** Rejected: CST-lexing a foreign-free module is wasted
  minutes; `foreignNames`-gating is exact and free (the corefn already lists them).
- **Load the shapes from a persisted `.pmi` summary instead of reconstructing** (ADR-0084 cache). A valid
  later optimisation (warm rebuilds), but orthogonal: the in-memory env thread is needed regardless (a full
  build reconstructs in topological order), exactly as `DictMachinery` is in-memory-first (ADR-0085 §3).
