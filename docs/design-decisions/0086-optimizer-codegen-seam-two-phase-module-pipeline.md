# 0086. The optimiser/codegen seam over a backend-neutral ANF module

- Status: ~~Proposed~~ **Accepted** _(2026-07-09: accepted by the maintainer; supplemented with the two-`DictElim`-mandate note in §3)_
- Date: 2026-07-09

## Abstract

 **Optimiser/codegen seam** — refines ADR-0082 §1 / ADR-0085 §1/§3 (no behavioural reversal). Reframes **`DictElim` as an optimisation** (not "required lowering"): it belongs to the optimiser track and runs under `--no-opt` only as a **temporary boot-parity bridge** (byte-identity to boot's incomplete optimiser), **removed at wall-3 self-host**. The optimiser transforms a **backend-neutral `AnfModule` = {name, decls of normalised ANF `(key, Expr)`}** (the existing `ANF.Pretty` shape), never `Gdef` — so its output feeds the VM/LLVM/JS gate legs uniformly; **`classifyDecl :: Decl → Gdef`** moves to the backend *after* the optimiser (byte-identity-safe, **group-atomic**: a `recursive` `Decl` classifies to a single `Grec`, never per-member). **Two module-level phases**: `preOptimizeModule` (boot-parity shim — DictElim only, always, returns the ephemeral `LocalFacts` = **stable dict machinery only**, known bodies recomputed pass-internally) → `optimizeModule` (the `fix(DictElim∘Simplify∘Spec∘Inline∘…)` fixpoint, `--opt` only, returns a `BuildSummary`) → `extendSummary` folds a **finished** module's `BuildSummary` into `BuildEnv` for dependents. `BuildEnv` holds **dependencies only**; current-module facts thread as ephemeral `LocalFacts` — so ADR-0084's **self-pollution invariant is structural** (a module can't read its own summary by construction). The in-memory `BuildSummary` (**distinct from** the persisted `.pmi` `Artifact.Summary`) carries dict machinery (always/in-memory) + optimiser summary (`--opt`) split by lifetime; only the pinned projection `persistedSummary :: BuildSummary → Maybe Artifact.Summary` reaches the `.pmi` (`Nothing` under `--no-opt`, so machinery never breaks byte-identity). Draws the ownership line: optimiser owns the seam + `MiddleEnd.Optimizer.DictElim`; codegen owns `declsOfModule`/`classifyDecl`/emit and a thin fold. Kills the parallel `acc.dict` channel and the backend's `Gdef`↔`Expr` adapters; retypes the `optimize`-iteration hooks `Expr`→`AnfModule`

## Context

The native build's per-module fold ([0085](0085-native-build-orchestration-inmemory-summary-env.md))
has been standing up alongside the optimiser track, and the seam between them has tangled. Concretely,
`Backend.LLVM.Driver.compileModules` — a **codegen-track** module — currently owns the whole
optimisation-adjacent apparatus:

```
fold { dict :: DictMachinery, build :: BuildEnv } over modules in dependency order:
  own   = machineryOf (spineBindings gdefs0)                 -- ① collect dict machinery   [MiddleEnd.DictElim]
  full  = mergeMachinery own acc.dict
  elimd = map (mapGdefBody (dictElimExpr gkeys full)) gdefs0 -- ② DictElim                 [MiddleEnd.DictElim]
  gdefs = map (mapGdefBody (optimize acc.build)) elimd       -- ③ optimize : BuildEnv → Expr → Expr
  build'= applySummary acc.build (interfaceOfAnf mod gdefs)  -- ④ grow env (AFTER optimize)
```

Four things are wrong with this, and they compound:

1. **The optimisation apparatus lives in codegen.** `spineBindings`, `mapGdefBody`,
   `mergeMachinery`, and `import MiddleEnd.DictElim` all sit in `Backend.LLVM.Driver`. The codegen
   track is carrying machinery collection, `DictElim` application, and env threading that are not its
   concern — it should only *lower an already-optimised module to `.ll`*.
2. **Dictionary machinery is threaded *outside* `BuildEnv`** (a separate `acc.dict`). The env-growing
   step that could carry it, `applySummary` (④), runs *after* `optimize` (③) — structurally too late
   to feed the `DictElim` (②) of the same module. So machinery got its own parallel channel, and now
   there are two envs threaded through one fold.
3. **`optimize` is `BuildEnv → Expr → Expr`** — a per-binding-body rewrite. This was a mis-scoping
   (the seam was described to the optimiser track as per-body). An optimiser is a **module** transform:
   it must see sibling bindings (cross-binding inlining, specialization homing, DCE), and its
   fixpoint iterates over the whole module, not one body.
4. **No distinction between a *pre-optimise* bridge and the real *optimise* fixpoint.** `DictElim` is
   applied once, unconditionally, wedged between lowering and `optimize`, with no name for *why* it
   runs under `--no-opt` and no home for the iterated `fix (DictElim ∘ Simplify ∘ …)` the optimiser
   actually is.

Underneath (4) is a **conceptual distortion inherited from [0082](0082-native-codegen-port-to-level-2.md) §1
and [0085](0085-native-build-orchestration-inmemory-summary-env.md) §1/§3**: they frame `DictElim` as
*"required lowering, not an optimisation."* That framing is an artefact, not an intrinsic truth.
`DictElim` **is** an optimisation — it collapses statically-known dictionary dispatch, exactly the kind
of reduction the optimiser exists to do. The reason Level-2 must run it even under `--no-opt` is
narrower and external: **boot implements its optimiser incompletely — it happens to do `DictElim` and
little else on its un-optimised native path** — and Level-2's backend gate demands `--no-opt` `.ll`
**byte-identical to boot** ([0082](0082-native-codegen-port-to-level-2.md) §2). So Level-2's `--no-opt`
must reproduce boot's `DictElim`-and-stop output. That makes the `--no-opt` `DictElim` a **boot-parity
bridge**, not a permanent architectural layer — and it is **removed when boot retires** (the wall-3
native self-host, at which point byte-identity to boot is no longer a gate). Calling it "required
lowering" bakes a temporary parity obligation into the architecture as if it were fundamental, and that
mislabelling is what put `DictElim` on the codegen track in the first place.

There is also a standing **layering tension** the current shape violates. `Gdef`
(`Gfun`/`Gcaf`/`Grec`, `Backend.LLVM.Types`) is a **backend** classification. But the optimiser's
correctness gate runs its output through **VM / LLVM-native / JS** legs
([0082](0082-native-codegen-port-to-level-2.md) §2): the *same* optimised program feeds three backends.
An optimiser that rewrites `Array Gdef` is coupled to one backend's classification and does not fit the
bytecode or JS legs. The optimiser must operate on a **backend-neutral** normalised-ANF form; `Gdef`
classification is a backend concern that belongs *after* the optimiser.

This record refines [0082](0082-native-codegen-port-to-level-2.md) §1 and
[0085](0085-native-build-orchestration-inmemory-summary-env.md) §1/§3. It does **not reverse** them:
the observable behaviour is unchanged — `--no-opt` still runs `DictElim` and stays byte-identical to
boot, `--opt` still runs the full optimiser, and the two-summary-lifetime split
([0085](0085-native-build-orchestration-inmemory-summary-env.md) §3) is preserved. What changes is the
**ownership line, the seam's type, and the vocabulary** — so the codegen and optimiser tracks can each
proceed behind a stable interface instead of contending over `compileModules`.

## Decision

### 1. `DictElim` is an optimisation; the `--no-opt` run is a temporary boot-parity bridge

Reframe [0082](0082-native-codegen-port-to-level-2.md) §1's *"required lowering, not an optimisation."*
`DictElim` is an optimisation pass and belongs to the **optimiser track**. It runs under `--no-opt`
**only** because Level-2's backend gate requires `--no-opt` `.ll` byte-identical to boot, and boot's
incomplete optimiser does `DictElim` on that path. That obligation is a **boot-parity bridge** with a
defined end of life: **it is removed when boot retires** (wall-3 native self-host). Until then, the
architecture names it as a bridge, not as a permanent lowering layer — which is what moves it off the
codegen track and onto the optimiser track where it belongs.

Consequence for the module map: `MiddleEnd.DictElim` moves to **`MiddleEnd.Optimizer.DictElim`** (an
optimiser-subsystem module). The codegen track stops importing it.

### 2. A backend-neutral ANF module is the optimiser's unit

The optimiser transforms a **backend-neutral** module of normalised ANF (named `AnfModule` to avoid the
CoreFn `Module` clash) — the shape the `MiddleEnd.ANF.Pretty` printer already uses:

```
AnfModule = { name :: String, decls :: Array Decl }
Decl      = { recursive :: Boolean, members :: Array (Tuple String Expr) }  -- normalised ANF, unclassified
```

A function binding is carried as `(key, Ret (CLam ps body))` — the full `Expr`, not a peeled body — so
both machinery *collection* and `DictElim` *rewriting* see the whole lambda uniformly (no
`spineBindings`/`mapGdefBody` `Gdef`↔`Expr` adapter pair). `Gdef` classification moves to the
**backend**, applied to each final `Decl` *after* the optimiser, immediately before `.ll` emission.
`DictElim` and every optimiser pass are thereby `Gdef`-free and shared across the VM/LLVM/JS legs.

The classification is **per-`Decl`, not per-member — `classifyDecl :: Decl -> Gdef`** — and must respect
the `Decl`'s `recursive` flag: a `recursive: true` group classifies **atomically** to a single `Grec`
(the by-need recursive group, [0070](0070-v1-byneed-recursive-caf-force.md) §4 / the `Erecfn` /
force-cell direct-call shape, [0077](0077-cross-module-direct-calls-pmi-arity.md)), **never** by
classifying its members independently as non-recursive `Gfun`/`Gcaf` — which would erase the group
atomicity and the `Grec`/`Erecfn` distinction. Only a `recursive: false` `Decl` (a single member) goes
through the non-recursive classifier (`classifyNonrec`: syntactic lambda ⇒ `Gfun`, else strict `Gcaf`).

This reorder is byte-identity-safe: classification decides `Gfun`/`Gcaf`/`Grec` from a binding's outer
shape and the `recursive` flag, both of which `DictElim` preserves (it rewrites inner bodies, never a
binding's top-level lambda/arity or a group's recursiveness). Under `--no-opt`, `optimizeModule` is
skipped, only the bridge `DictElim` runs on the neutral module, then classification — identical `Gdef`s
to today.

### 3. Two module-level phases; `BuildEnv` holds dependencies only, current-module facts thread separately

`BuildEnv` holds **only dependency-derived facts** — never the module currently being processed. The
current module's **stable** facts are computed fresh as an **ephemeral `LocalFacts`** and threaded
through that module's own phases; only after the module is done is a **`BuildSummary`** of it folded into
`BuildEnv` for its *dependents*. This makes [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)'s
self-pollution invariant — *a module never reads its own summary* — **structural in the types**, not a
discipline the driver must remember: the env that processes `module0` provably contains no `module0`
facts. (An earlier draft of this record folded the current module's machinery into the env with an
`extendMachinery env0 module0`, which *looked like* seeding the dependency env with the current module —
the anti-pattern this split removes.)

**`LocalFacts` carries only fixpoint-stable facts — the dictionary machinery, and nothing that a pass
mutates.** The dictionary machinery (accessors `φ`, instances `{φ → impl}`) is stable across the
`optimizeModule` fixpoint: `DictElim` and the other passes rewrite bodies but do not add/remove a
module's accessor/instance bindings or change which record an instance denotes. So `LocalFacts` can be
computed once in `preOptimizeModule` and handed to the fixpoint without going stale. It **must not**
carry *known local bodies*: those change every iteration (`Simplify`/`Dbe` rewrite them, specialization
*adds* `$spec` decls, DCE removes them), so a body snapshot taken in `preOptimizeModule` would be stale
by the second iteration. Known-body facts are **pass-internal to `optimizeModule`** — recomputed from
the current module state each iteration — never carried in `LocalFacts`. (Dependents' cross-module
known bodies are a *different* channel: the post-fixpoint `BuildSummary` §4, computed from the final
`module2`, not from `LocalFacts`.)

The seam (the codegen driver calls these; it owns none of their bodies):

```
emptyBuildEnv     :: BuildEnv

preOptimizeModule :: BuildEnv -> AnfModule
                  -> { module :: AnfModule, localFacts :: LocalFacts }

optimizeModule    :: BuildEnv -> LocalFacts -> AnfModule
                  -> { module :: AnfModule, summary :: BuildSummary }

summaryOfLocal    :: LocalFacts -> BuildSummary       -- the --no-opt dependents' channel (machinery only)

extendSummary     :: BuildEnv -> BuildSummary -> BuildEnv

persistedSummary  :: BuildSummary -> Maybe Artifact.Summary   -- projection to the .pmi (§below)
```

The per-module step:

```
compileOne env0 module0 =
  let { module: module1, localFacts } = preOptimizeModule env0 module0
      { module: module2, summary }     =
          if opt then optimizeModule env0 localFacts module1
                 else { module: module1, summary: summaryOfLocal localFacts }
      env1 = extendSummary env0 summary        -- for DEPENDENTS; never seen by module0
  in { env: env1, module: module2 }
```

- **`preOptimizeModule env0 module0`** — the **boot-parity bridge (compatibility shim)**: collect
  `module0`'s own `LocalFacts` (its stable dictionary machinery — *not* known bodies, per above), and
  apply **`DictElim` only**, resolving against `env0`'s *dependency* machinery **∪** the just-collected
  `localFacts` machinery (ephemerally — the union is not stored in `env0`). Runs **always** (`--no-opt`
  included); the sole reason it runs under `--no-opt` is boot byte-identity (§1), and it is the pass
  deleted at boot retirement. It is **not** "required lowering" — it is a compatibility shim with a
  defined expiry.
- **`optimizeModule env0 localFacts module1`** — the real optimiser: the iterated fixpoint
  `fix (DictElim ∘ Simplify ∘ Specialize ∘ Inline ∘ …)` under `env0` (dependencies) **+** `localFacts`
  (current module's stable machinery; known-body facts recomputed pass-internally each iteration),
  returning the optimised module **and** its `BuildSummary` for dependents. `DictElim` appears here too —
  because it *is* an optimisation, a legitimate fixpoint member (a later pass can expose a newly-static
  dispatch it then collapses). Run under `--opt` only; `preOptimizeModule` is not its "first iteration"
  but the minimal shim that runs when the fixpoint is switched off.
- **`extendSummary env0 summary`** — fold a finished module's `BuildSummary` into the env for its
  dependents. (Today's `applySummary`, renamed and re-typed to take a produced `BuildSummary` rather than
  an `Interface`.)

**The two `DictElim` call sites have opposite implementation mandates** (maintainer's acceptance note).
The `preOptimizeModule` (boot-parity) `DictElim` is **byte-identity-first**: faithful to boot's
`--no-opt` output, reproducing a boot quirk where one exists, because its whole reason to run is the
byte-identity gate (§1). The `optimizeModule` (fixpoint) `DictElim` is **correctness-first**: a real
optimiser pass, free to collapse dispatch boot's incomplete optimiser would leave, and answerable only
to the behavioural gate. The two **share one implementation while that stays frictionless**; **if the
mandates diverge, splitting `DictElim` into two definitions — one per phase — is explicitly sanctioned.**
That split is self-cleaning: the boot-parity copy already carries an expiry (deleted at boot retirement,
§1), leaving the correctness-first copy as the sole survivor.

**`BuildSummary` (in-memory seam) is *not* the persisted `.pmi` `Artifact.Summary`
([0084](0084-binary-pmo-pmi-and-cross-module-summary.md)) — the two must not be conflated.** The seam
type is deliberately named `BuildSummary` to keep them distinct: it is the **in-memory** value the fold
threads, and it carries two components split by lifetime
([0085](0085-native-build-orchestration-inmemory-summary-env.md) §3):

- the **dictionary machinery** (accessors `φ`, instances `{φ → impl}`) — needed by dependents'
  `DictElim`, therefore present **always** (`--no-opt` too) and **in-memory only, never persisted**; and
- the **optimiser pruned-module summary** (inline candidates / spec-callees / effect-placement / foreign
  shapes) — `--opt` only.

Only the second component is persistable, and **only via the pinned projection `persistedSummary ::
BuildSummary -> Maybe Artifact.Summary`**: it returns `Just` the optimiser component under `--opt` and
**`Nothing` otherwise** — so a machinery-only `BuildSummary` (the `--no-opt`/`summaryOfLocal` case) can
**never** reach the `.pmi`. This is the guard against the byte-identity hazard: without it, an
implementer serialising a `BuildSummary` directly would emit the always-present machinery into the
`--no-opt` `.pmi` and break byte-identity with boot. The `.pmi` writer calls `persistedSummary` and
writes the field only on `Just` (`Nothing`-omitted, [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)).
`BuildEnv`'s concrete shape, `LocalFacts`, and the optimiser component of `BuildSummary` remain
**optimiser-track-owned** (this record fixes the *seam*, not the optimiser internals).

### 4. The ownership line

- **Optimiser track (`MiddleEnd.Optimizer.*`) owns:** the types `BuildEnv`/`LocalFacts`/`BuildSummary`,
  the seam `emptyBuildEnv`/`preOptimizeModule`/`optimizeModule`/`summaryOfLocal`/`extendSummary`/`persistedSummary`,
  and `MiddleEnd.Optimizer.DictElim`. The whole optimisation apparatus is behind these names.
- **Codegen track (`Backend.LLVM.*`) owns:** `declsOfModule` (CoreFn module → the neutral `AnfModule`:
  `translExpr → normalize`, no classification), the thin per-module fold that *calls* the seam,
  `classifyDecl` (`Decl` → `Gdef`, group-atomic §2) applied after the seam, and `.ll` emission. The
  driver imports the seam, never `DictElim`/machinery internals.

The `Backend.LLVM.Driver.compileModules` fold shrinks to the `compileOne` above plus classification and
emission; `spineBindings`/`mapGdefBody`/`mergeMachinery` leave the backend (absorbed into the optimiser
seam or deleted).

### 5. The optimiser-iteration hooks

The `Purvasm.Compiler` `CompilerAction` hooks
(`onBeforeOptimize`/`onEnterOptimizeIter`/`onContinueOptimizeIter`/`onLeaveOptimizeIter`) are the
observation points of the §3 `optimize` fixpoint — an *iteration*, matching the `fix (…)` model. (An
earlier sketch also named `onOptimizeFinish`; it landed as `onLeaveOptimizeIter`, the once-at-the-end
point.) Their payload is retyped from `String → ANF.Expr → m Unit` (per-body, the same mis-scoping as
context-problem 3) to **`AnfModule → m Unit`** (per-module — the module name rides in the `AnfModule`),
consistent with the module-level seam.

## Consequences

- The codegen and optimiser tracks decouple behind a stable seam
  (`emptyBuildEnv`/`preOptimizeModule`/`optimizeModule`/`summaryOfLocal`/`extendSummary`/`persistedSummary`
  + `Optimizer.DictElim`, over `BuildEnv`/`LocalFacts`/`BuildSummary`/`AnfModule`); each proceeds without
  editing the other's modules. `compileModules` stops being the contended surface.
- The self-pollution invariant ([0084](0084-binary-pmo-pmi-and-cross-module-summary.md)) becomes
  **structural**: `BuildEnv` holds only dependency facts, current-module facts are the ephemeral
  `LocalFacts`, and a module's `BuildSummary` is folded in only *after* it is processed — so a module
  cannot read its own summary by construction, not by convention.
- `DictElim` is where it conceptually belongs (the optimiser subsystem), and the `--no-opt` run has a
  name and an expiry (**boot-parity bridge, removed at wall-3 self-host**) instead of masquerading as a
  permanent lowering layer.
- The optimiser is backend-neutral (operates on `Module`/`Decl`, never `Gdef`), so its output feeds the
  VM/LLVM/JS legs of the [0082](0082-native-codegen-port-to-level-2.md) §2 gate uniformly; `Gdef`
  classification is a backend post-step.
- Dictionary machinery rides `BuildEnv` at the correct lifetime; the parallel `acc.dict` channel and
  the backend's `Gdef`↔`Expr` spine adapters disappear.
- Byte-identity and the two-lifetime summary split are preserved unchanged (§2, §3) — this is a
  refinement, not a behavioural reversal.
- `--no-opt` remains a durable bisection capability *for now*; when boot retires, `preOptimize` (the
  bridge) is deleted and `--no-opt` either goes away or becomes "run the fixpoint zero times", a
  decision deferred to that milestone.
- Owed edits to the accepted records on acceptance of this one: dated refinement addenda on
  [0082](0082-native-codegen-port-to-level-2.md) §1 and
  [0085](0085-native-build-orchestration-inmemory-summary-env.md) §1/§3 pointing here (per the README's
  in-place-drift convention), not rewrites.

## Alternatives considered

- **Keep `DictElim` as "required lowering" on the codegen track** (the status quo /
  [0082](0082-native-codegen-port-to-level-2.md) §1 framing). Rejected: it is the mislabelling that put
  optimisation apparatus in the backend and gave the `--no-opt` `DictElim` no expiry. Naming it a
  temporary boot-parity bridge on the optimiser track is both more honest and what unblocks the
  ownership split.
- **Two envs threaded through the driver** (the current `acc.dict` + `acc.build`). Rejected: it forces
  the driver to know the machinery lifetime and re-introduces backend ownership of a summary channel.
  One `BuildEnv` (dependencies) + an ephemeral `LocalFacts` keeps the lifetime split without a second
  driver-threaded env.
- **Seed the current module's facts into `BuildEnv` before its own phases** (an `extendMachinery env0
  module0 → BuildEnv`, this record's own earlier draft). Rejected: the env that processes a module would
  then contain that module's facts, contradicting [0084](0084-binary-pmo-pmi-and-cross-module-summary.md)'s
  self-pollution invariant and re-opening it as a discipline. Returning `localFacts` from
  `preOptimizeModule` and folding `BuildSummary` only *after* keeps the invariant structural (§3).
- **Keep `optimize : Expr → Expr` (per-body) and let the driver map it over bindings.** Rejected:
  cross-binding optimisation (inlining a sibling, homing a specialization, module DCE) and the fixpoint
  iteration are inherently module-level; a per-body seam cannot express them and would leak the loop
  back into the driver.
- **Move `Gdef` to the middle-end and keep `Array Gdef` as the optimiser's unit** (the rejected fork).
  Smaller reorder, but couples the optimiser to a backend classification (`Gfun`/`Gcaf`/`Grec`) that
  does not fit the JS/bytecode legs and re-imports a backend vocabulary into the shared optimiser. The
  neutral `Module`/`Decl` (already the `ANF.Pretty` shape) is the backend-agnostic unit the §2 gate
  needs.
- **Fold `preOptimize` into `optimize` as its first iteration** (no separate bridge). Rejected: it
  conflates the always-on boot-parity subset with the `--opt`-only fixpoint, so `--no-opt` would have
  to run (a truncated) `optimize`, and the bridge would have no clean deletion point at boot retirement.
  Two named phases keep the expiry explicit.
