# 0087. A backend-neutral build driver over an injected `CompilerAction`

- Status: ~~Proposed~~ **Accepted** _(2026-07-09: accepted by the maintainer)_
- Date: 2026-07-09

## Abstract

The build pipeline is orchestrated **twice** today — the VM path inline in `cli/Run.purs`
(`map (compileModuleWith opt) ordered` + `link`), the native path buried in
`Backend.LLVM.Driver.compileModules` (a fold that lives, wrongly, *inside the backend*) and driven by
`cli/Build.purs`. Neither shares the other's orchestration, and any future backend would make a third.
This record lifts orchestration into **one backend- and effect-neutral build driver in the `compiler`
library**, parameterized over two injected capabilities: a pure **`Backend o`** (the post-[0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md)-seam
codegen — per-module `lowerModule` **and** the whole-program `lowerEntry`, selecting VM/LLVM via the
output type `o`) and an effectful **`CompilerAction o m`** (the CLI-provided host capability — module
loading via an explicit-`LoadResult` `loadModule`, artifact emission (`emitFile` for each
`ModuleArtifacts o` = `.pmi` + backend IR, `emitEntry` for the program object), and the per-phase
**inspection hooks**, over a generic monad `m` — failures are returned `Left BuildError`, never thrown,
so `m` stays `Monad` with no `MonadError`). The driver's responsibility **ends at pure codegen
artifacts** — the per-module `ModuleArtifacts o` plus the program `entry` object (both emitted), bundled
as `BuildProducts o`: it owns import-closure loading, topological order, the
ADR-0086 per-module fold
(`preOptimizeModule → optimizeModule → extendSummary`), and hook dispatch. **Finalization (link) stays in
the CLI Build driver, per target** — VM merges code blocks into one `.pvm` (a pure spine fold), LLVM
native-links the objects into an executable (a `clang`/`lld` subprocess) — because the two share neither
input type (`Array o` vs `Array FilePath`) nor effect profile (pure vs child process), so a common `link`
capability would be a false unification. No behavioural change — it refines
[0085](0085-native-build-orchestration-inmemory-summary-env.md)'s orchestration (the fold and in-memory
env stay) and consumes [0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md)'s seam.

## Context

[0085](0085-native-build-orchestration-inmemory-summary-env.md) established the native build as a
topological fold threading an in-memory summary env, and
[0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) drew the optimiser/codegen seam
(`preOptimizeModule`/`optimizeModule`/`extendSummary` over a backend-neutral `AnfModule`). But *where*
that fold lives, and *who* drives it, is still wrong on three counts:

1. **Orchestration is duplicated across backends.** The VM path orchestrates inline in `cli/Run.purs`
   (load the import closure with a `foldM`, order it, `map (compileModuleWith opt)`, write `.pmo`/`.pmi`,
   `link`). The native path orchestrates in `Backend.LLVM.Driver.compileModules` (the per-module fold)
   wrapped by `cli/Build.purs` (load closure, `nativeSplit`, write objects, `NativeLink`). The two share
   nothing; any future backend would clone the same loop a third time.
2. **The native fold lives inside the backend.** `compileModules` — import ordering, the 0086 seam
   threading, reachability — is orchestration, not code generation, yet it sits in `Backend.LLVM.Driver`.
   The backend should own only *lower one module's `AnfModule` to `o`* — nothing about the multi-module
   build (orchestration) or the final assembly (link, §4).
3. **Per-phase inspection has no home.** The optimiser is now an iterated fixpoint
   ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) §3/§5), and the natural way to
   watch it — dump each iteration's `AnfModule`, trace which pass fired, `--emit-ir` between phases — has
   nowhere to attach. The `Purvasm.Compiler` `CompilerAction` already anticipates this with
   `onEnterOptimizeIter`/`onContinueOptimizeIter`/`onLeaveOptimizeIter`/`onBeforeCodegen` hooks (§3 — the
   iteration surface `build` now dispatches).

There is also a **layering rule** in play. The `compiler` library must not depend on the CLI's effects
(`cli-lib`'s `FS`/`PROC`/`LOG`/`ENV` `Run` stack); today the native fold is pure only because IO was
kept in the CLI by hand. As orchestration moves into the library, its effect needs (load a module,
write an artifact, log/inspect a phase) must cross the boundary as an **injected capability**, so the
library stays effect-agnostic and the CLI keeps its `Run`-stack requirements to itself.

## Decision

### 1. One generic build driver in the `compiler` library

A single `build` driver — backend- and effect-neutral — owns the multi-module orchestration **up to the
per-module artifact**, and no further:

- build the **import closure** and **topologically order** it (dependency before dependent) — this is
  `loadClosure`, a **library** function (§1.1 below), not a capability. A failed load short-circuits the
  whole build with `Left BuildError` (§2.1);
- **fold** the [0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) per-module step over that
  order, threading the in-memory `BuildEnv`: `declsOfModule` → `preOptimizeModule` →
  (`--opt`) `optimizeModule` → `extendSummary`, dispatching the inspection hooks around each phase;
- hand each finished `AnfModule` to the backend's `lowerModule` → the per-module IR `o`, pair it with the
  module's derived `Interface` into a `ModuleArtifacts o`, and write it via the injected `emitFile`;
- **after the fold**, run the backend's pure `lowerEntry` over the whole reachable module set → the
  program `entry` artifact (LLVM's `entry.ll`; §2), write it via the injected `emitEntry`, and **return**
  `Right (BuildProducts { modules, entry })` — modules and entry both emitted, so the CLI's finalization
  is link-only.

The driver **stops there**. It does **not** link — assembling the per-module artifacts into a runnable
whole is the CLI Build driver's per-target job (§4). This is exactly the fold
[0085](0085-native-build-orchestration-inmemory-summary-env.md) specified and `compileModules`
implements, up to but excluding finalization — **relocated** out of `Backend.LLVM.Driver` into the
library's driver, and made backend/effect-generic. The VM path's inline `cli/Run.purs` per-module loop
collapses into the same driver (its `link` call stays in the CLI).

#### 1.1. `loadClosure` is a library function over the single-module `loadModule` — not a capability

The effect boundary is drawn at the **smallest irreducible primitive**: `CompilerAction.loadModule ::
ModuleName -> m LoadResult` fetches **one** module's CoreFn by name (the host's FS/registry access),
returning an explicit `LoadResult` (§2). Everything above it — reading a module's `imports`, transitively
following them, cycle handling, and the topological sort — is **pure graph logic that needs no host
knowledge**, so it lives in the library as `loadClosure`, built *on top of* `loadModule`:

```
loadClosure :: CompilerAction o m -> ModuleName -> m (Either BuildError (Array { name :: String, mod :: CF.Module }))
--   from the entry: read imports, load each not-yet-visited via action.loadModule, recurse, topsort.
--   ROOT entry: Missing → Left (EntryMissing name)  (a non-existent entry is a build error, §2).
--   imported node, on the LoadResult (§2): Missing → skip; Failed e → onLoadFailed e then Left; Loaded → follow imports.
--   brackets each load with the onBeforeLoad / onLoadSucceeded / onLoadFailed hooks.
```

So `loadModule` stays **single-module** (a capability — the irreducible IO), and `loadClosure` is
**library driver code** (backend- and host-independent). This is precisely the walk `cli/Run.purs` does
inline today (`foldM go visited (importNames m)`), consolidated into the library rather than reimplemented
per CLI command. The per-module load hooks fire inside `loadClosure`, at each `loadModule` call.

### 2. Two injected capabilities, split by concern

The driver is `build :: Monad m => Backend o -> CompilerAction o m -> Options -> m (Either BuildError (BuildProducts o))`
— **`m` is only `Monad`; a build failure is a returned `Left`, not a thrown effect** (§2.1). The output
types are backend-parametric but backend-independent in shape:

```
ModuleArtifacts o = { interface :: Interface, backendIR :: o }   -- one module: its .pmi + backend IR
EmittedModule   o = { artifact :: ModuleArtifacts o, path :: String }   -- + where emitFile wrote it
BuildProducts   o = { modules :: Array (EmittedModule o)
                    , entry   :: { ir :: o, path :: String } }   -- the whole-program entry/init artifact + its path
BuildError        = EntryMissing ModuleName | LoadFailed LoadError | …   -- a concrete library type (§2/§2.1)
```

`BuildProducts` carries **both** the in-memory artifacts (for the VM's pure `Link.link`) and the emitted
**paths** (for the native `clang`/`lld` link), so CLI finalization (§4) is link-only for either target.

- **`Backend o`** — *pure, backend-specific codegen*. Two pure lowering functions, nothing else:
  - `lowerModule :: … -> AnfModule -> o` — one optimised module to its backend IR (VM: a `.pmo`
    `ModuleArtifact`; LLVM: the module's `.ll`). The driver pairs the result
    with the module's `Interface` (derived from the same optimised `AnfModule`, pure) into a
    `ModuleArtifacts o`.
  - `lowerEntry :: ProgramInfo -> o` — the **whole-program** entry/init artifact (LLVM's `entry.ll`
    carrying `pv_init_all` over the reachable inits + `@main`; VM's program entry). This is **pure
    codegen, not link** — it needs the *whole* module set (reachability), so the **driver runs it once
    after the fold** and puts it in `BuildProducts.entry` (§1). It is deliberately *not* left to CLI
    finalization, which owns only the effectful link.

  `declsOfModule` (CoreFn → `AnfModule`: `translExpr → normalize`) is **shared middle-end**, *not* in
  `Backend o` — every backend consumes the same neutral module
  ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) §2). `Backend o` is a plain record of
  **pure** functions — trivially testable (`m = Identity`), no effects, precisely *because* the one
  effectful, non-shareable step (link) was left out of it (§4). **The WIP `Purvasm.Compiler` currently
  places `lowerModule` inside `CompilerAction`; at implementation it must move to `Backend o`** — pure
  lowering does not belong in the effectful host capability.
- **`CompilerAction o m`** — *effectful, CLI-provided* (the `Purvasm.Compiler` sketch, host effects +
  hooks). It supplies the host effects over a **generic monad `m`** so the library never names an effect
  system:
  - `loadModule :: ModuleName -> m (LoadResult)` — the **single-module** primitive, returning an
    **explicit result** (below), *not* a bare `{path, mod}`; closure walking is the library's
    `loadClosure` (§1.1), not a field here.
  - `emitFile :: ModuleArtifacts o -> m String` — write **one module's whole artifact** (its `.pmi`
    *and* its backend IR `o` together). Taking `ModuleArtifacts o` — not a bare `o` — is what makes `.pmi`
    emission and backend-IR emission one unambiguous responsibility (the WIP already models this). **The
    returned `String` is the backend-IR / link-input path** (the `.pmo`/`.ll` the finalizer consumes); the
    `.pmi` is written as a side effect and **not** returned — the linker never needs it.
  - `emitEntry :: o -> m String` — write the **program entry/init** artifact (LLVM's `entry.ll`),
    returning its path. The entry object is emitted **by the library's emit phase**, alongside the module
    artifacts — *not* by the CLI — so finalization (§4) stays link-only and the native linker gets the
    `entry.ll` path from `BuildProducts.entry.path`. (The entry has no `.pmi`, so it takes a bare `o`, not
    a `ModuleArtifacts o`.)
  - `workdir` and the **hooks** record.

  `m` is constrained only to `Monad` — all real effects arrive through the capability's functions, so the
  CLI passes its `Run (FS + PROC + LOG + ENV + …)` stack and the library stays effect-agnostic. (This is
  why a record-of-capabilities is chosen over a `Run`/algebraic-effect signature the library would have
  to *import*.)

**`loadModule` returns an explicit `LoadResult`, never a bare record** (P1). A generic `m` carries no
`catch`, so the library cannot observe a failure a host encodes as a monadic exception; and a
missing-but-fine module (a `Prim`-like module with no `corefn.json`, which the current `cli/Build.purs`
*skips*) must be distinguished from a real error. So:

```
data LoadResult = Loaded { path :: String, mod :: CF.Module } | Missing | Failed LoadError
data BuildError = EntryMissing ModuleName | LoadFailed LoadError | …
```

`loadClosure`'s contract is then pinned, and it is **position-sensitive** — the root entry is not an
ordinary node:

- **the *root* `entryModule` must be `Loaded`.** `Missing` at the root is `Left (EntryMissing name)`, not
  an empty closure — a non-existent entry is a build error, never a silent success. Only the **first**
  `loadModule` (the entry) has this rule.
- **`Missing` on any *transitively-imported* node → skip it** (no import edge, e.g. a `Prim`-like module
  with no `corefn.json`).
- **`Failed e` (any node) → fire `onLoadFailed e` (reporting) and short-circuit with `Left (LoadFailed
  e)`**.
- **`Loaded m` → record it and follow `m.imports`.**

Failure is data in the result, not an exception the generic `m` would have to catch.

#### 2.1. Failure is a returned `Left`, not `MonadError` — `m` stays `Monad`

How does the driver *halt* on a `Failed`? Not via `MonadError e m` + `try`/`throw`. That was considered
and rejected on this record's own thesis: `MonadError` imports an effect capability (error) as a
typeclass constraint the library depends on — the exact `Run`-style leak §2 rejects — and it forces the
error type `e` to become a third parameter (`CompilerAction e o m`), which is the awkwardness it looks
like. It is also **unnecessary**: because `loadModule` already returns `LoadResult` as **data**, there is
nothing to `try` — the failure is a value the driver pattern-matches. So:

- **`m` stays constrained to `Monad`.** `build` returns `m (Either BuildError (BuildProducts o))`; the
  driver halts by **returning `Left`**, threading the short-circuit purely (an internal `ExceptT`/`Either`
  fold), no effect capability required.
- **`onLoadFailed :: LoadError -> m Unit` is a *reporting* hook, not a control-flow one.** The host logs
  or formats; the **library** decides to halt by returning `Left`. The hook needs no halting power (so no
  `abort :: BuildError -> m a` capability either — the other rejected option).
- **`BuildError` / `LoadError` are concrete *library* types** (e.g. `LoadError = { moduleName, detail ::
  String }`), not a polymorphic `e`. The host maps its own IO error into `LoadError` when it builds a
  `Failed`; its richer original stays host-side (it can log it in its `loadModule`/`onLoadFailed` impl).
  This keeps `CompilerAction o m` at two parameters and matches the WIP, which already types the failed
  hook with a concrete error.
- **Contract on `loadModule`:** it must encode *anticipated* failures as `Missing`/`Failed`, and **not
  throw** in `m` — so the driver never needs `catch`. A truly unexpected host exception (a bug) may
  propagate; it is not part of the protocol.

The backend axis (`Backend o`) and the effect axis (`CompilerAction o m`) are **orthogonal**: one host
can drive any backend, and a backend is pure regardless of host — which is precisely what lets VM and
native reuse one driver.

### 3. The hooks are the optimiser-phase inspection surface

The `CompilerAction` hooks fire at the phase boundaries the driver crosses. The optimiser-iteration
hooks — `onBeforeOptimize` / `onEnterOptimizeIter` / `onContinueOptimizeIter` / `onLeaveOptimizeIter`
(this sketch's earlier `onOptimizeFinish` landed as `onLeaveOptimizeIter` — the once-at-the-end
observation point of the fixpoint) — bracket the driver's
[0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) §3 `optimizeModule` iteration:
`onEnterOptimizeIter` once before the loop, `onContinueOptimizeIter round am` after each *changing* pass,
and `onLeaveOptimizeIter` once when the module stops changing (convergence, via `AnfModule`'s structural
`Eq`) or the cap is hit; `onBeforeCodegen` marks the hand-off to `Backend o`. Their
payload is the per-module **`AnfModule`** ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md)
§5's `Expr → AnfModule` retype), so `--emit-ir`, per-iteration IR dumps, and pass tracing all become
host-side hook implementations — no inspection logic in the library or the backend.

#### 3.1. `--emit-ir <module>` and `--opt-max-iter <N>` — the CLI knobs, realised entirely in the host

Two CLI knobs are realised **through the §3 hooks and `CompilerAction` alone** — no library or backend
change:

- **`--emit-ir <module>` is an *argument* naming one target module, not a stop-after-IR flag.** The build
  **runs to completion** (codegen, then link) exactly as without it — the knob only turns on *tracing* for
  the named module, and never diverts the pipeline. Like `purs-backend-es --trace-rewrite`, the host
  accumulates the target module's ANF IR **once per optimiser round**: the iteration hooks append
  `printModuleAnf am` to an `emitIrBuffer` — `onEnterOptimizeIter` the pre-optimised module,
  `onContinueOptimizeIter round am` each pass's result, `onLeaveOptimizeIter` the converged module — and on
  `onCleanUp` (or the module's compile finishing) the host writes the accumulated trace to `<module>.ir`.
  The buffer lives in the host's **State-like `m`** (a CLI-side `Ref`/`State`, since `m` is the host's to
  choose — the library stays `Monad`-generic), so the whole trace is a host hook implementation. This
  **supersedes** the old `--emit-ir` flag (dump every module's final ANF, then stop before codegen): it is
  now single-target, per-iteration, and build-completing.

- **`--opt-max-iter <N>` sets `CompilerAction.maxOptimizeIter`** — the hard bound on the driver's
  `optimizeModule` fixpoint loop (§3), **clamped to `[1, 10]`** (10 the heuristic cap). The loop's *normal*
  exit is **convergence** — it stops as soon as a round leaves the `AnfModule` unchanged (`AnfModule` has
  structural `Eq`); the cap is only the *outer-round* analogue of ADR-0082's inner `rewriteLimit`
  rewrite-fuel — a non-termination backstop, not the usual exit. (Today's only pass, `DictElim`, is
  idempotent, so the first round always converges.) Absent, `--opt-max-iter` defaults to the cap.

### 4. Finalization (link) is the CLI Build driver's per-target job — deliberately not a capability

The build ends, on the library side, at the per-module artifacts. **Assembling them into a runnable
whole is target-specific and does not generalise**, so it is left in the CLI, per command:

- **VM (`purvasm run`)** merges the per-module code blocks into one `.pvm` image — `Link.link`, a
  **pure** spine fold in the `compiler` library, called from the CLI.
- **Native (`purvasm build`)** native-links the emitted objects into an executable — `NativeLink`, a
  **`clang`/`lld` subprocess** over the files `emitFile` wrote, in the CLI over its `PROC` effect.

These share neither an input (`Array o` in memory vs `Array FilePath` on disk) nor an effect profile
(pure vs child process), so a common `link` capability would be a false unification — worse than two
honest per-target steps. `Link.link` and `NativeLink` therefore stay exactly where they are; the CLI
calls the right one on `build`'s `BuildProducts` (its `modules` **and** the `entry` artifact — for LLVM,
the `entry.ll` object the driver already produced via `lowerEntry`, §2) after the driver returns.
Finalization is **link only** — no codegen: the entry/init artifact is pure codegen and was already made
in the library, so the CLI step just `clang`s/links the objects (native) or folds the spine (VM). The
library's `build` never links, and `Backend o` stays pure.

### 5. Migration order

**First task after acceptance: sync the WIP `Purvasm.Compiler` to this record.** The sketch is still
the opposite of the decision on four points and must be reconciled before wiring: `loadModule ::
ModuleName -> m { path, mod }` → `m LoadResult`; `lowerModule` **moved out of `CompilerAction` into the
pure `Backend o`**; `emitEntry` **added** (with `build` returning `m (Either BuildError (BuildProducts
o))`); and the **inspection hooks retyped to take one `AnfModule`** — the WIP's `String -> ANF.Expr`
payload is just a module name plus its decls, which the neutral `AnfModule` (`{ name, decls }`, [0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md))
already carries, so each optimiser/codegen hook takes a single `AnfModule` rather than a `(name, expr)`
pair. These are named as design-blockers-once-implementing, not blockers for accepting the record.

Then native first: relocate `compileModules` into the library driver behind `Backend o` (`LLVM`) +
`CompilerAction`, and thin `cli/Build.purs` to *inject* the capability, call `build`, then `NativeLink`
its products. Then re-home the VM path (`cli/Run.purs`) onto the same driver with `Backend o`
(`Bytecode`), keeping its `Link.link` finalization in the CLI. Any future backend slots in as a new
`Backend o` with no driver change.

## Consequences

- One orchestration for all backends: the fold, topological order, and 0086 seam threading live once, in
  the library, backend-generic. Adding a backend is a new `Backend o`, not a new build loop.
- `Backend.LLVM.Driver.compileModules` leaves the backend; the backend shrinks to *pure codegen*
  (`lowerModule` per module + `lowerEntry` for the program). The [optimizer-modular-not-whole-program]
  fold is now visibly orchestration, not codegen.
- The entry/init artifact has an unambiguous home: **pure `Backend.lowerEntry`, run by the driver after
  the fold, carried in `BuildProducts.entry`** — so the native wiring never loses `entry.ll`, and CLI
  finalization stays link-only (no codegen).
- `loadModule`'s explicit `LoadResult` lets `loadClosure` skip `Missing` (e.g. `Prim`) and halt on
  `Failed` with `onLoadFailed`, without a `catch` constraint on the generic `m` — the missing/error
  distinction the current skip-on-missing behaviour relies on is now in the types.
- Build failure stays effect-agnostic: `build` returns `m (Either BuildError (BuildProducts o))`, `m` is
  only `Monad`, and no `MonadError`/`abort` capability (nor a third `e` type parameter) is introduced —
  halting is an ordinary `Left`, reporting is the `onLoadFailed` hook.
- The entry/init object is **emitted by the library** (`emitEntry`), so `BuildProducts.entry.path` hands
  the native linker its `entry.ll` and CLI finalization does no codegen — link only.
- The library's build core stays pure through finalization: because link (the one effectful,
  non-shareable step) is kept in the CLI per target, `Backend o` is genuinely pure and `build`'s only
  effects are the host capability's (`loadModule`/`emitFile`/hooks).
- Module-graph construction + topsort (`loadClosure`) stop being reimplemented per CLI command: they
  move into the library over the single-module `loadModule` primitive, so the effect boundary is the
  smallest irreducible IO and the graph logic is shared and backend/host-independent.
- Rich, uniform per-phase inspection (the user's primary motive): every optimiser iteration and codegen
  hand-off is a hook on the CLI-provided `CompilerAction`, identical across VM and native.
- The `compiler` library stays effect-agnostic (generic `m` + capability record); the CLI's `Run`-stack
  requirements never leak into it — the boundary [0085](0085-native-build-orchestration-inmemory-summary-env.md)
  wanted, now enforced by the injected capability rather than by hand.
- Refines, does not reverse, [0085](0085-native-build-orchestration-inmemory-summary-env.md): the
  per-module fold and in-memory `BuildEnv` are unchanged; only their *home* (library driver) and *driver*
  (injected capabilities + hooks) change. Consumes [0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md)'s
  seam verbatim.
- Owed on acceptance: a dated refinement addendum on
  [0085](0085-native-build-orchestration-inmemory-summary-env.md) §2 pointing here (the `build`
  orchestrator moves into the library behind `CompilerAction`), per the README in-place-drift convention.

## Alternatives considered

- **Keep the two orchestrations** (VM inline in the CLI, native in the backend). Rejected: duplication, a
  a third clone for any future backend, `compileModules` stranded in the backend, and no shared
  inspection surface — the
  status quo this record exists to end.
- **A `Run`/algebraic-effect signature instead of a capability record.** The CLI already uses `Run`, so
  the driver *could* take a concrete effect row. Rejected: it forces the `compiler` library to depend on
  a specific effect system and the CLI's rows, exactly the leak §Context names. A record of functions
  over a generic `m` keeps the library agnostic; the CLI is free to implement the record in `Run`.
- **One merged capability (backend + host in a single record).** Rejected: backend selection (VM/LLVM,
  pure) is orthogonal to effect injection (CLI, impure). Merging them couples a pure, testable concern to
  an effectful one and blocks driving one backend from different hosts (e.g. a test harness vs the CLI).
- **Put `declsOfModule` in `Backend o`** (per-backend CoreFn→module lowering). Rejected: the neutral
  `AnfModule` is shared by construction ([0086](0086-optimizer-codegen-seam-two-phase-module-pipeline.md) §2);
  duplicating `translExpr → normalize` per backend would re-open the divergence the neutral module closes.
- **A shared `link` capability on the backend** (`Backend o` carries a `link :: … -> m …`, VM lifting a
  pure fold, native shelling `clang`). Considered and rejected: VM and native finalization share neither
  input type (`Array o` vs `Array FilePath`) nor effect profile (pure vs subprocess), so the "shared"
  entry would be a lowest-common-denominator fiction, and it would drag `m` (hence effects) back into the
  otherwise-pure `Backend o`. Leaving link as two honest per-target CLI steps (§4) keeps `Backend o` pure
  and the false unification out of the design.
- **Have the library `build` do the linking too** (return a linked binary/image, not per-module
  products). Rejected for the same reason: it would force the effectful, target-specific finalization into
  the library core. The driver stops at pure codegen artifacts (`BuildProducts`); the CLI owns assembly.
- **Make the entry/init artifact in CLI finalization** (a pure `Backend` function the CLI calls
  alongside link). Rejected: it splits pure codegen across two homes and forces the CLI to hold codegen
  knowledge. The entry object is pure codegen over the whole module set, so the driver produces it right
  after the fold (`lowerEntry` → `BuildProducts.entry`); the CLI's finalization is then purely link.
- **`loadModule :: ModuleName -> m { path, mod }`** (a bare record, failures as monadic exceptions).
  Rejected: a generic `m` has no `catch`, so the library couldn't observe such a failure, and a benign
  missing module (`Prim`) couldn't be told from an error. An explicit `LoadResult`
  (`Loaded`/`Missing`/`Failed`) puts both in the type and pins `loadClosure`'s skip/halt contract.
- **`MonadError e m` (or an `abort :: BuildError -> m a` capability) for halting** (§2.1). Rejected:
  `MonadError` imports the error effect as a typeclass constraint the library depends on — the same
  `Run`-style leak the record rejects — and forces `e` into a third parameter (`CompilerAction e o m`).
  `abort :: … -> m a` is a disguised effectful throw. Both are unnecessary once `loadModule` returns
  data: the driver halts by returning `Left BuildError` over a plain `Monad`, and `onLoadFailed` reports.
- **Emit the entry object in the CLI** (or leave `BuildProducts.entry` as raw `o` for the CLI to write).
  Rejected: emission is a host effect and belongs to the library's emit phase alongside module emission,
  so finalization is uniformly link-only; the driver runs `emitEntry` and returns the entry's path.
