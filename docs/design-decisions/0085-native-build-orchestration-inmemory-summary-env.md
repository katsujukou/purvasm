# 0085. Native build orchestration over an in-memory summary env

- Status: ~~Proposed~~ **Accepted** _(2026-07-08: accepted by the maintainer; both tracks signed off on the in-memory-env / persisted-summary split and the co-owned env type)_
- Date: 2026-07-08

## Abstract

**Native build orchestration**: replace the shortcut driver (`surfaceOf` via a redundant bytecode `compileModule` + monolithic `nativeSplit`, CoreFn→ANF lowered twice/module, no summary thread) with a **per-module compile unit** (`translExpr → normalize → DictElim(env) → (optimize) seam → emit .ll` + a module summary) driven by `build` as a **topological fold threading an in-memory dependency-summary `env`** — the clean pattern boot's *bytecode* build already uses (compile-unit + orchestrator), plus the summary thread it lacks. **DictElim is required lowering** (runs `--no-opt` too, module-level, env-consuming) so its cross-module **dictionary machinery rides the in-memory env — always**, split by *lifetime* from the optimiser's **persisted `--opt`-only `.pmi` `Summary`** (ADR-0084, `Nothing`-omitted, byte-identical): same type at the seam, orthogonal persistence. Kills the double-lowering; the `(optimize)` slot after DictElim is the optimiser track's seam (`Simplify` moves there from its per-binding stopgap). Env dict-machinery = accessor `φ` + instance `{φ→impl}`. Unblocks cross-module DictElim (both tracks were stalled on the missing summary)

## Context

The Level-2 native backend now emits per-module `.ll` and links a runnable binary
([0082](0082-native-codegen-port-to-level-2.md)'s B2 separate-compilation shape). But its *build
driver* is a shortcut that is far from the target architecture, and it structurally blocks the next
slice.

Today `Purvasm.CLI.Build` does two disjoint things per module:

- `surfaceOf` calls **`interfaceOf (compileModule m)`** — a full *bytecode* compile — solely to read
  each module's export kinds for the cross-module surface; and
- `nativeSplit` **re-lowers** every module (`translExpr → normalize → classifyNonrec`) to native
  `Gdef`s and emits `.ll`.

So the CoreFn→ANF front half runs **twice per module**, there is no single per-module compile unit, and
nothing threads a **dependency summary** between modules. That last gap is not cosmetic: it makes
**cross-module `DictElim` impossible**, which stalls slice-1b (arithmetic). A surface `x + y` is CoreFn
`Data.Semiring.add semiringInt x y`, whose instance `semiringInt` and accessor `add` live in an
*imported* module (`Data.Semiring`); collapsing `add semiringInt → intAdd` therefore needs that
dependency's dictionary machinery. Without a per-module summary threaded to dependents, the collapse
cannot fire. Both the native and optimiser tracks independently hit exactly this wall.

Two standing facts frame the fix. **`DictElim` is *required lowering*, not optimisation**
([0082](0082-native-codegen-port-to-level-2.md) §1, [0027](0027-dictelim.md)): it runs on **both**
`--no-opt` and `--opt`, because without it dictionary access is wrong-shape and stuck
([[purescript-dictionaries-are-newtypes]]). And **boot's *bytecode* build is already the right
pattern** — a per-module `compile` unit (`Pvm.Compile.compile_module` → `.pvmo`/`.pvmi`, with
recompilation avoidance) driven by a `build` orchestrator — whereas boot's *native* path is the
monolithic B1 ([0082](0082-native-codegen-port-to-level-2.md) discussion). L2's native build should
follow the clean bytecode-build pattern, plus the one thing boot's bytecode build lacks: a threaded
cross-module summary.

The optimiser track has already landed `Simplify` (`MiddleEnd.Optimizer.Simplify`) wired **per-binding**
through `Compile.optimizeAnf`, and a persisted, `--opt`-only `.pmi` field `summary :: Maybe Summary`
(`Summary = Summary Json`, omitted when `Nothing` so the `.pmi` stays byte-identical to boot,
[0084](0084-binary-pmo-pmi-and-cross-module-summary.md)). This record must not collide with either.

## Decision

### 1. A per-module compile unit

One function lowers a single CoreFn module to its native object, in dependency-summary context:

```
CoreFn.Module + env  ──▶  translExpr → normalize → DictElim(env) → (optimize) → emit(.ll)  +  moduleSummary
```

- `translExpr`/`normalize` produce the module's `Gdef`s (as `Driver.gdefsOfModule` does today).
- **`DictElim`** runs next, **module-level** (it needs the module's whole binding set to see local
  accessors/instances) and **`env`-consuming** (imported accessors/instances come from dependencies'
  summaries). It always runs — `--no-opt` included.
- **`(optimize)`** is an explicit **seam**, after `DictElim`, module-level, **receiving the same
  `env`**: the optimiser track plugs `Simplify → Dbe → EffectAnalysis → specialization → NbE` here, and
  its passes read the env (cross-module inline-candidate / spec-callee bodies, effect placement, foreign
  shapes). It is a no-op slot until they do; passing `env` to it is part of the seam contract.
- `emit` is the existing `moduleLl`; the unit also returns the module's **summary** (below).

The unit is a plain function so a `compile` command can call it for a single module (incremental
single-module builds), and `build` can call it per module.

### 2. `build` = topological fold threading an **in-memory** env

`build` loads the entry's import closure, orders it topologically (a dependency precedes its
dependents), then **folds** the per-module compile over that order, **threading an in-memory `env`** of
already-compiled modules' summaries. Because the order is dependency-first, a module's compile sees its
dependencies' summaries in `env`. After all modules: link (`NativeLink`). This is the orchestrator/
traverse the current monolith is missing; it also **removes the `compileModule`/`surfaceOf`
redundancy** — the surface and the summary both come from the module's own single compile.

### 3. Two summary layers, different lifetimes — the key correctness point

`DictElim` is required lowering, so its cross-module input (dictionary machinery) must be available
**even under `--no-opt`**. The optimiser's persisted `.pmi` summary is `--opt`-only (omitted under
`--no-opt` for byte-identity). These cannot be the same channel. They are split by *lifetime*:

- **In-memory build env — dictionary machinery, always.** Rebuilt each `build`, threaded through the
  fold, carrying what `DictElim` needs regardless of `--opt`. It is **not** persisted and so does not
  touch the `.pmi` byte-identity. Owned by the native track. This is the concrete carrier of
  [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) §4's **"`--opt` always recompiles"** decision:
  a full build re-lowers every module in topological order, so a dependency's machinery is always in the
  env by the time its dependents compile — no persisted summary is needed for a *full* build. The
  persisted layer earns its keep only for **incremental** (cross-`build`) rebuilds.
- **Persisted `.pmi` `Summary` — optimiser inputs, `--opt`-only.** The
  [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) cross-*build* cache (inline candidates /
  specialization callees / impurity, later also dictionary machinery for incremental builds), `Nothing`
  under `--no-opt`. Owned by the optimiser track.

They share a **type** at the seam but are **orthogonal** in persistence and gating. The first cut uses
only the in-memory env; wiring dictionary machinery into the persisted `.pmi` (so a warm rebuild need
not recompile a dependency to recover it) is a later increment on
[0084](0084-binary-pmo-pmi-and-cross-module-summary.md)'s hash cascade.

### 4. The env is an extensible, serializable typed record; dictionary machinery is its first field

The env's per-module value is a **typed record, grown by both tracks** — not a native-only structure.
Its **first field is the dictionary machinery** `DictElim` needs, the two things boot's `dict_elim.ml`
collects from the spine:

- **accessors** — a method projection `\d -> case d of v -> v.φ`, i.e. the field `φ` it projects;
- **instances** — a newtype-identity-wrapped dictionary `(\x -> x) { φ: impl, … }`, i.e. the map
  `{ φ → impl }` of its members.

The optimiser track then adds fields on the *same* record as its passes land: inline-candidate bodies
(NbE), specialization-callee bodies, impurity / effect placement (`EffectAnalysis`), and foreign shapes
(ADR-0080). Dictionary machinery is the first such field, not a separate channel.

Two constraints on the record:

- **Serializable plain data — no closures.** So the in-memory env value corresponds directly to the
  persisted `.pmi` `Summary` ([0084](0084-binary-pmo-pmi-and-cross-module-summary.md)): a warm
  incremental build that reads a dependency's summary from disk must reconstruct the same env value it
  would have gotten by recompiling. Dictionary machinery is already plain data (`φ`, `{φ → impl}`,
  operand atoms), so it qualifies; later fields must stay so too. A distilled form (accessor → field,
  instance → member map) is preferred over shipping raw bodies where it suffices.
- **The type is the seam.** Its shape is co-owned and settled with the optimiser track at integration;
  the native track ships the dictionary-machinery field first.

### 5. Division of labour

- **Native track (this record):** the build orchestrator, the per-module compile unit exposing the
  `(optimize)` seam, `DictElim` (intra- then cross-module via the env), and the in-memory
  dictionary-machinery env.
- **Optimiser track:** everything in the `(optimize)` seam (`Simplify` already exists, moving from its
  per-binding stopgap into the module-level seam) and the persisted `.pmi` `Summary` content.

## Consequences

- Cross-module `DictElim` becomes possible under `--no-opt`, unblocking slice-1b arithmetic and the
  optimiser track (which was blocked on the same missing summary).
- The double CoreFn→ANF lowering (the `compileModule` surface hack) is eliminated; a module's object is
  a pure function of its own source **plus its dependencies' summaries** — the modular, per-module-
  cacheable property ([[optimizer-modular-not-whole-program]]).
- A clean `(optimize)` seam decouples required lowering (native track) from behavioural optimisation
  (optimiser track), so the two develop independently and integrate at one point.
- Byte-identity is preserved: `DictElim`'s env is in-memory (never persisted), and the persisted `.pmi`
  summary stays `--opt`-only/`Nothing`-omitted.
- New owed work: a per-module **uniquify** for where-hoisted duplicate bare keys (boot's
  `uniquify_toplevel` was whole-program; removed with B1) — needed only if L2 hoists colliding keys
  within a module; verified when it arises.

## Alternatives considered

- **Persist dictionary machinery in the `.pmi` and read it there (no in-memory env).** Rejected for the
  first cut: it would either break the `--no-opt` `.pmi` byte-identity (the machinery must be present
  under `--no-opt`, but boot's `.pmi` has none) or force a separate always-on `.pmi` section now. The
  in-memory env sidesteps both; persistence is a later [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) increment.
- **Keep the monolithic `nativeSplit` and add a second pass for cross-module dicts.** Rejected: it keeps
  the double-lowering and has no natural place for the `(optimize)` seam or the summary thread.
- **A subprocess-per-module `compile` orchestrator (like a C toolchain driver).** Overkill in-process;
  the fold threading an in-memory env is the same topology without process overhead. A standalone
  `compile` command still falls out (it calls the same unit, reading summaries from disk once
  [0084](0084-binary-pmo-pmi-and-cross-module-summary.md) persists them).
- **Module-level vs per-binding `DictElim`.** `DictElim` must be module-level (it resolves against the
  module's whole binding set + imported summaries); per-binding cannot see sibling instances. `Simplify`
  stays per-binding-body inside the module-level `(optimize)` seam.
