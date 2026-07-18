-- | The backend- and effect-neutral **build driver** (ADR-0087). `build` owns the multi-module
-- | orchestration up to the per-module artifact: it loads the entry's import closure (`loadClosure`), folds
-- | the ADR-0086 optimiser seam over it in dependency order, hands each optimised `AnfModule` to the
-- | injected `Backend`'s pure codegen, emits each module's artifact and the whole-program entry object via
-- | the injected `CompilerAction`, and returns the `BuildProducts` for the CLI to finalise (link) per
-- | target. It never links — assembling a runnable whole (VM: a `.pvm` spine fold; native: a `clang`/`lld`
-- | subprocess) is the CLI Build driver's per-target job (ADR-0087 §4).
-- |
-- | The two injected capabilities are orthogonal (ADR-0087 §2): the pure `Backend c o` (codegen, selecting
-- | VM/LLVM by the output type `o`) and the effectful `CompilerAction o m` (the CLI's host effects + the
-- | per-phase inspection hooks, over a generic `Monad m`). A build failure is a returned `Left BuildError`,
-- | never a thrown effect, so `m` stays `Monad` with no `MonadError` (§2.1).
module Purvasm.Compiler
  ( Backend
  , BuildError(..)
  , BuildProducts
  , CompilerAction
  , CompilerActionHooks
  , ContextModule
  , EmittedModule
  , EntryInput
  , ForeignSigError
  , ForeignSigMap
  , LoadError
  , LoadResult(..)
  , LoweredModule
  , ModuleArtifacts
  , ModuleName
  , Options
  , Progress
  , build
  , defaultHooks
  , loadClosure
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl, traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler.Bytecode.Artifact (Interface)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey)
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer (BuildEnv, emptyBuildEnv, extendSummary, localFactsOf, publishedForeignSigs, withForeignSigs)
import Purvasm.Compiler.MiddleEnd.Optimizer as Optimizer
import Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine (RejectionEvent(..), emptyQuarantine)

-- | A module's dotted name (the `loadModule` key).
type ModuleName = String

-- | A host load failure reduced to a library-neutral shape (ADR-0087 §2.1): the richer original error
-- | stays host-side (the host logs it in its `loadModule`/`onLoadFailed` impl). Concrete, not a polymorphic
-- | `e`, so `CompilerAction` stays at two type parameters.
type LoadError = { moduleName :: String, detail :: String }

-- | A qualified foreign key (`Module.Name.ident`) → its reconstructed calling shape (ADR-0080/0090).
type ForeignSigMap = Map String ForeignShape

-- | A foreign-signature reconstruction failure reduced to a library-neutral shape (ADR-0090), parallel to
-- | `LoadError`: the host maps its richer FSR issue (parse error, missing provenance, manifest decode) into
-- | it. FSR is a hard diagnostic (ADR-0080) — never collapsed to an empty map.
type ForeignSigError = { moduleName :: String, detail :: String }

-- | The result of loading **one** module (ADR-0087 §2). `Missing` is a benign absence (a `Prim`-like module
-- | with no `corefn.json`) — distinct from a real `Failed`. Making failure *data* is what lets the driver
-- | halt without a `catch`/`MonadError` constraint on the generic `m`.
data LoadResult
  = Loaded { path :: String, mod :: CF.Module }
  | Missing
  | Failed LoadError

-- | A build that could not complete (ADR-0087 §2). `EntryMissing` fires only for the **root** entry (a
-- | `Missing` transitive import is skipped, not an error); `LoadFailed` for any `Failed` load.
data BuildError
  = EntryMissing ModuleName
  | LoadFailed LoadError
  | ForeignSigFailed ForeignSigError

-- | The backend-neutral build knobs. Backend-specific knobs (native heap size, debug ABI, …) are closed
-- | over in the `Backend` value the CLI constructs, so they never reach this record.
type Options =
  { entryModule :: String
  , entryName :: String
  -- | `true` ⇒ the entry is an `Effect` run for effects (applied to unit); `false` ⇒ a bare value printed.
  , isEffect :: Boolean
  -- | `true` ⇒ `--opt` (iterate `optimizeModule` to a fixpoint, up to `maxOptimizeIter`); `false` ⇒
  -- | `--no-opt` (the optimiser-free reference lowering, ADR-0104 §3).
  , opt :: Boolean
  }

-- | One module's emittable artifact: its `.pmi` interface and its backend IR (`.pmo`/`.ll`). `emitFile`
-- | writes both; the returned path is the backend-IR / link-input, not the `.pmi` (ADR-0087 §2).
type ModuleArtifacts o = { interface :: Interface, backendIR :: o }

-- | A module's artifact plus where `emitFile` wrote its link input.
type EmittedModule o = { artifact :: ModuleArtifacts o, path :: String }

-- | Everything the CLI finalizer (link) needs: the per-module artifacts (in dependency order) and the
-- | whole-program entry/init object — both already emitted, so finalization is link-only.
type BuildProducts o =
  { modules :: Array (EmittedModule o)
  , entry :: { ir :: o, path :: String }
  }

-- | The **pre-optimisation, pre-FSR** view `moduleContext` reads to derive ONE module's context
-- | contribution (ADR-0104 §5-1): the module's CoreFn source and its freshly-lowered `AnfModule`. It
-- | carries **no** `foreignSigs` — the contribution is derived before the module's own FSR runs, so a
-- | shape map here could only ever be empty. Kept distinct from `LoweredModule` precisely so the
-- | contribution derivation never reads an empty-shape lie (ADR-0090): foreign shapes belong to the
-- | per-module lowering inputs, not the context.
type ContextModule =
  { source :: CF.Module
  , module :: AnfModule
  }

-- | An optimised module paired with its CoreFn source (the backend reads `imports`/`exports` for the
-- | interface and reachability) and its **visible** foreign shapes. The unit `lowerModule`/`lowerEntry`
-- | codegen consumes (distinct from `ContextModule`, the pre-FSR `moduleContext` contribution input).
type LoweredModule =
  { source :: CF.Module
  , module :: AnfModule
  -- | The module's **visible** foreign shapes (ADR-0090) — its own ∪ its dependencies' exported — assembled
  -- | by the driver's `ForeignFacts` thread and populated in **every** mode, so codegen reads `arity` /
  -- | effect bits here under `--no-opt` and `--opt` alike.
  , foreignSigs :: ForeignSigMap
  }

-- | The whole reachable program, for the entry/init codegen: every optimised module plus the (DictElim'd)
-- | entry expression.
type EntryInput = { modules :: Array LoweredModule, entry :: Expr }

-- | The **pure** backend codegen capability (ADR-0087 §2), parameterised over its output IR `o` (VM/LLVM)
-- | and an internal cross-module context `c`. Deliberately effect-free: link — the one effectful,
-- | non-shareable step — is left to CLI finalization (§4), so this record is trivially testable.
-- |
-- | The context is **dependency-directed** (ADR-0104 §5-1): it starts empty and the driver extends it
-- | per module through the fold, so no eager whole-closure pass runs before compilation. `c` is a
-- | merge-able bag of **module-keyed facts**, and `(c, mergeContext, emptyContext)` must form an
-- | **idempotent commutative monoid on valid fact sets**: associative, `emptyContext` the identity,
-- | and — load-bearing — `merge x x = x` with order immaterial. The driver merges **overlapping**
-- | projections routinely (on a diamond `Main → A, B → C`, `visible(A)` and `visible(B)` both contain
-- | `C`'s facts), and relies on idempotence for `visible(M) = merge of direct deps' visibles ∪ own` to
-- | equal the import-closure union. Facts keyed by module-qualified names merged with `Set`/`Map`
-- | unions satisfy this by construction (two contexts agree wherever they overlap — both carry module
-- | `C`'s one true contribution); an accumulating representation (arrays, counters) does NOT and would
-- | duplicate diamond facts:
-- |
-- |   * `emptyContext` — the facts of no modules at all; the merge identity.
-- |   * `mergeContext a b` — the fact-set union. With the laws above the argument order is
-- |     unobservable on the driver's inputs; implementations should still be deterministic for a
-- |     fixed argument order.
-- |   * `moduleContext deps cm` — ONE module's **own contribution** (the native `gkeys`/cross-module
-- |     surface facts), computed from its **pre-optimisation** lowering (a `ContextModule` — CoreFn
-- |     source + lowered ANF, so the surface can read `exports`, but **no** foreign shapes: FSR runs in
-- |     the fold) *under* `deps` — the projection of its import closure's facts, for a backend whose
-- |     contribution derivation must see through dependency-owned facts. The returned `c` is the
-- |     module's contribution ONLY, never `deps` folded back in — the driver owns accumulation.
-- |     The contributed `gkeys` are the stable *base* set of existing source declarations, **not** the
-- |     complete final set: the optimiser's Specialize pass (ADR-0089) adds post-opt top-level keys
-- |     (`$spec$` clones), so `lowerModule`/`lowerEntry` complete `gkeys` with each object's final gdef
-- |     keys before codegen.
-- |   * `interfaceOf` — the module's `.pmi` from its optimised ANF + CoreFn surface (backend-neutral in
-- |     result — byte-identical to the bytecode deriver — but the `ExportKind` classification is the
-- |     backend's).
-- |   * `lowerModule` — one optimised module → its backend IR. Receives the **import-closure
-- |     projection** (deps ∪ own), never a whole-closure view: facts are consulted only by membership /
-- |     key lookup on the module's own references, so the projection is emission-identical to the old
-- |     whole-program context by construction (references never leave the import closure).
-- |   * `lowerEntry` — the **whole-program** entry/init object (reachability over the module set);
-- |     receives the **final accumulated** context (the entry imports the world). Pure codegen, run by
-- |     the driver after the fold, *not* CLI link.
type Backend c o =
  { emptyContext :: c
  , mergeContext :: c -> c -> c
  , moduleContext :: c -> ContextModule -> c
  , interfaceOf :: c -> LoweredModule -> Interface
  , lowerModule :: c -> LoweredModule -> o
  , lowerEntry :: c -> EntryInput -> o
  }

-- | The **effectful** host capability (ADR-0087 §2), CLI-provided over a generic `Monad m`. `loadModule`
-- | is the single-module IO primitive (closure walking is the library's `loadClosure`, §1.1); `emitFile`
-- | writes one module's whole artifact and returns its link-input path; `emitEntry` writes the program
-- | entry object; `hooks` is the per-phase inspection surface (§3). `maxOptimizeIter` bounds the driver's
-- | `optimizeModule` fixpoint loop (ADR-0087 owns the iteration; the seam pass is a pure single step).
type CompilerAction o m =
  { workdir :: String
  , maxOptimizeIter :: Int
  , loadModule :: ModuleName -> m LoadResult
  -- | Reconstruct a module's foreign shapes (ADR-0090 §2): a host effect (reads `.purs` source / the ulib
  -- | `foreignSigs`) that can fail, returned as data (`Left` halts the build with `ForeignSigFailed`, no
  -- | throw — ADR-0087's boundary). The driver calls it only when the module declares foreigns.
  , foreignSigsOf :: CF.Module -> m (Either ForeignSigError ForeignSigMap)
  , emitFile :: ModuleArtifacts o -> m String
  , emitEntry :: o -> m String
  , hooks :: CompilerActionHooks m
  }

-- | How far the per-module fold has progressed, for a host progress indicator: `current` of `total`
-- | modules (1-based), reported by `onStartCompile` as each module's compile begins.
type Progress = { total :: Int, current :: Int }

-- | The per-phase inspection hooks (ADR-0087 §3), dispatched by `build` as it crosses each phase boundary.
-- | The optimiser-iteration hooks bracket the driver's `optimizeModule` fixpoint loop: `onEnterOptimizeIter`
-- | once before the loop, `onContinueOptimizeIter round am` after each *changing* pass (1-based `round`),
-- | and `onLeaveOptimizeIter` once when the module stops changing (convergence) or `maxOptimizeIter` is hit
-- | — so `--emit-ir`, per-iteration IR dumps, and pass tracing are host-side hook implementations.
-- | `onCodegenFailed` is part of the surface for a future pure codegen failure channel and is not
-- | dispatched yet.
type CompilerActionHooks m =
  { onBeforeLoad :: ModuleName -> m Unit
  , onLoadSucceeded :: { path :: String, mod :: CF.Module } -> m Unit
  , onLoadFailed :: LoadError -> m Unit
  , onStartCompile :: Progress -> CF.Module -> m Unit
  , onBeforeOptimize :: AnfModule -> m Unit
  , onEnterOptimizeIter :: AnfModule -> m Unit
  , onContinueOptimizeIter :: Int -> AnfModule -> m Unit
  , onLeaveOptimizeIter :: AnfModule -> m Unit
  -- | A round-growth backstop rejection (ADR-0089 Addendum 2026-07-16): one `BackstopFired` per
  -- | newly recorded rejection as the rounds run, and one `BackstopSummary` (attempts vs distinct
  -- | bindings) before `onLeaveOptimizeIter` when anything fired. The host's warning sink lives
  -- | here — the optimiser seam is pure and never logs.
  , onOptimizerBackstop :: RejectionEvent -> m Unit
  , onBeforeCodegen :: AnfModule -> m Unit
  , onCodegenFailed :: String -> m Unit
  , onCleanUp :: ModuleName -> m Unit
  }

defaultHooks :: forall m. Applicative m => CompilerActionHooks m
defaultHooks =
  { onBeforeLoad: noop
  , onLoadSucceeded: noop
  , onLoadFailed: noop
  , onStartCompile: noop2
  , onBeforeOptimize: noop
  , onEnterOptimizeIter: noop
  , onContinueOptimizeIter: noop2
  , onLeaveOptimizeIter: noop
  , onOptimizerBackstop: noop
  , onBeforeCodegen: noop
  , onCodegenFailed: noop
  , onCleanUp: noop
  }
  where
  noop :: forall a. a -> m Unit
  noop = const (pure unit)

  noop2 :: forall a b. a -> b -> m Unit
  noop2 _ = noop

-- | The dotted names a module imports.
importNames :: CF.Module -> Array String
importNames m = map (nameKey <<< _.moduleName) m.imports

-- | Load the entry module and the transitive closure of its imports (ADR-0087 §1.1), in **dependency
-- | order** (imports before importers). Built over the single-module `loadModule`, so the effect boundary
-- | is the smallest irreducible IO and the graph logic is host-independent. Position-sensitive: the root
-- | entry must be `Loaded` (`Missing` root → `EntryMissing`); a transitively-imported `Missing` is skipped
-- | (e.g. `Prim`); any `Failed` fires `onLoadFailed` and halts with `LoadFailed`.
loadClosure
  :: forall o m
   . Monad m
  => CompilerAction o m
  -> ModuleName
  -> m (Either BuildError (Array { name :: String, mod :: CF.Module }))
loadClosure action entry = do
  action.hooks.onBeforeLoad entry
  action.loadModule entry >>= case _ of
    Failed e -> action.hooks.onLoadFailed e $> Left (LoadFailed e)
    Missing -> pure (Left (EntryMissing entry))
    Loaded r -> do
      action.hooks.onLoadSucceeded r
      goImports (Map.singleton entry r.mod) (importNames r.mod) >>= case _ of
        Left e -> pure (Left e)
        Right mods -> pure (Right (topoOrder mods))
  where
  goImports visited names = foldM step (Right visited) names

  step (Left e) _ = pure (Left e)
  step (Right visited) name
    | Map.member name visited = pure (Right visited)
    | otherwise = do
        action.hooks.onBeforeLoad name
        action.loadModule name >>= case _ of
          Failed e -> action.hooks.onLoadFailed e $> Left (LoadFailed e)
          Missing -> pure (Right visited) -- a transitively-imported Missing is not an import edge
          Loaded r -> do
            action.hooks.onLoadSucceeded r
            goImports (Map.insert name r.mod visited) (importNames r.mod)

-- | Dependency order (imports before importers): a DFS post-order over the loaded closure, mirroring the
-- | CLI's `depOrder`.
topoOrder :: Map String CF.Module -> Array { name :: String, mod :: CF.Module }
topoOrder mods = Array.fromFoldable (List.reverse (snd (foldl visit (Set.empty /\ Nil) names)))
  where
  names = map fst (Map.toUnfoldable mods :: Array (String /\ CF.Module))

  visit acc@(seen /\ done) name
    | Set.member name seen = acc
    | otherwise = case Map.lookup name mods of
        Nothing -> Set.insert name seen /\ done
        Just m ->
          let
            seen1 /\ done1 = foldl visit (Set.insert name seen /\ done) (localDeps m)
          in
            seen1 /\ ({ name, mod: m } : done1)

  localDeps m = Array.filter (\n -> Map.member n mods) (importNames m)

-- | The program entry expression: the **bare** `<entry>.<name>` reference, for both run modes — matching
-- | boot's LLVM entry (the linked spine's `Var entry_key`, `link.ml`). The `isEffect` distinction is
-- | carried by the flag threaded to `lowerEntry`, **not** by the entry expression: the `Effect` entry stub
-- | hands this thunk to `pv_run_effect`, which itself applies it to unit (`run_effect main = apply main
-- | [unit]`, ADR-0067 §2). Applying to unit *here* as well would double-perform — the effect runs during
-- | the entry-expr eval, then `pv_run_effect` re-applies its (immediate) `Unit` result and faults.
entryExprOf :: Options -> Expr
entryExprOf opts = normalize (TmVar (opts.entryModule <> "." <> opts.entryName))

-- | The per-module fold state: the threaded optimiser env, the emitted artifacts / optimised modules
-- | accumulated in dependency order, and the dependency-directed backend context (ADR-0104 §5-1).
type FoldState c o =
  { env :: BuildEnv
  -- | ADR-0090's `ForeignFacts`: the accumulated **dependencies' exported** foreign shapes, threaded in
  -- | every mode. Feeds `LoweredModule.foreignSigs` (codegen) and, under `--opt`, the optimiser env.
  , foreignEnv :: ForeignSigMap
  -- | Each processed module's **import-closure projection** of the backend context (deps ∪ own),
  -- | memoised by module name: `visible(M) = merge(visible over M's direct local deps) ∪ contrib(M)`.
  -- | Merging direct deps' *projections* equals merging the closure's *contributions* because the
  -- | merge is idempotent on module-keyed facts — this is `projection(context, importClosure(M))`
  -- | without a per-module closure walk.
  , visibles :: Map String c
  -- | The running whole-program context: every processed module's contribution merged in dependency
  -- | order. `lowerEntry` consumes the final value (the entry imports the world).
  , total :: c
  , emitted :: Array (EmittedModule o)
  , lowered :: Array LoweredModule
  , progress :: Progress
  }

-- | Build a whole program to per-module artifacts + the entry object (ADR-0087 §1), stopping at pure
-- | codegen. The driver is **neutral** (ADR-0086 Addendum): load the closure, and per module in dependency
-- | order `CoreFn → normalise → AnfModule` (once — the same value feeds the backend context facts and the
-- | optimiser, ADR-0104 §5-1), then `if --opt` **iterate `optimizeModule` to a fixpoint** (up
-- | to `action.maxOptimizeIter` rounds) + `extendSummary`, `else` the identity — it runs **no `DictElim`
-- | of its own** (a backend's *required* lowering — e.g. native leaf/builtin resolution — is the backend's
-- | own concern inside `lowerModule`/`lowerEntry`, applied in both modes; ADR-0104 §3). The backend context
-- | is accumulated dependency-directed through the fold (no eager whole-closure pass): each module's
-- | contribution is derived under its import-closure projection and merged in. Emit each module via
-- | `emitFile`, then run the backend's whole-program `lowerEntry` (with the final accumulated context) over
-- | the (raw) entry + reachable set and emit it via `emitEntry`. Returns `BuildProducts` for CLI
-- | finalization (link). Never links.
build
  :: forall c o m
   . Monad m
  => Backend c o
  -> CompilerAction o m
  -> Options
  -> m (Either BuildError (BuildProducts o))
build backend action opts =
  loadClosure action opts.entryModule >>= case _ of
    Left err -> pure (Left err)
    Right loaded -> do
      let
        localNames = Set.fromFoldable (map _.name loaded)
        initialState =
          { env: emptyBuildEnv
          , foreignEnv: Map.empty
          , visibles: Map.empty
          , total: backend.emptyContext
          , emitted: []
          , lowered: []
          , progress: { total: Array.length loaded, current: 1 }
          }
        -- An FSR failure short-circuits the fold with `Left` (ADR-0090 §2), like `loadClosure`.
        step (Left e) _ = pure (Left e)
        step (Right st) item = stepModule localNames st item
      foldM step (Right initialState) loaded >>= case _ of
        Left err -> pure (Left err)
        Right final -> do
          -- The entry is handed to the backend **raw** (no seam `DictElim`); the native backend applies
          -- its required lowering privately in `lowerEntry`, the VM none (ADR-0104 §3). The context is
          -- the final accumulated one — the entry imports the world (ADR-0104 §5-1).
          let entryIr = backend.lowerEntry final.total { modules: final.lowered, entry: entryExprOf opts }
          entryPath <- action.emitEntry entryIr
          pure (Right { modules: final.emitted, entry: { ir: entryIr, path: entryPath } })
  where
  stepModule :: Set String -> FoldState c o -> { mod ∷ CF.Module, name ∷ String } -> m (Either BuildError (FoldState c o))
  stepModule localNames st item = do
    action.hooks.onStartCompile st.progress item.mod
    -- FSR (ADR-0090): the module's **own** shapes, only when it declares foreigns (`moduleForeignSigs` is
    -- also self-guarding, so the check is a fast-path). A failure halts the build with `ForeignSigFailed`.
    ownE <-
      if Array.null item.mod.foreignNames then pure (Right Map.empty)
      else action.foreignSigsOf item.mod
    case ownE of
      Left fe -> pure (Left (ForeignSigFailed fe))
      Right ownSigs -> do
        let
          am = declsOfModule item.mod
          -- The backend-context thread (ADR-0104 §5-1): project the import closure (merge the direct
          -- local deps' memoised projections — idempotent merge makes that the closure's union), derive
          -- this module's own contribution under it, and extend. `am` is the same value the optimiser
          -- receives below — `declsOfModule` runs once per module.
          depsCtx = foldl
            (\acc d -> maybe acc (\v -> backend.mergeContext v acc) (Map.lookup d st.visibles))
            backend.emptyContext
            (Array.filter (\n -> Set.member n localNames) (importNames item.mod))
          contrib = backend.moduleContext depsCtx { source: item.mod, module: am }
          moduleCtx = backend.mergeContext contrib depsCtx
          visible = Map.union ownSigs st.foreignEnv -- own ∪ deps (keys module-qualified, disjoint)
        action.hooks.onBeforeOptimize am
        optimised <-
          if opts.opt then do
            -- Two-tier (ADR-0090 §3): surface own → `LocalFacts`, deps → `BuildEnv`, for effect analysis.
            let
              lf = (localFactsOf st.env am) { foreignSigs = ownSigs }
              envF = withForeignSigs st.foreignEnv st.env
            r <- runOptimizer envF lf am
            pure
              { module: r.module
              , env: extendSummary st.env r.summary
              -- own shapes the published inline candidates reference (possibly non-exported): an
              -- inlined body carries the reference into consumers, so its shape must travel too.
              , published: publishedForeignSigs r.summary
              }
          else pure { module: am, env: st.env, published: Map.empty } -- --no-opt: identity, no summary, env unchanged
        action.hooks.onBeforeCodegen optimised.module
        let
          lm = { source: item.mod, module: optimised.module, foreignSigs: visible }
          artifact = { interface: backend.interfaceOf moduleCtx lm, backendIR: backend.lowerModule moduleCtx lm }
        path <- action.emitFile artifact
        action.hooks.onCleanUp item.name
        let
          -- Publish this module's **exported** foreign shapes to dependents (ADR-0090 §3): own ∩ exports —
          -- plus the shapes its published inline candidates reference (ADR-0089 slice 2 × ADR-0090:
          -- an inlined wrapper's private foreign lands in consumers, shape and all).
          exported = Set.fromFoldable (map (qualifiedKey item.mod.name) item.mod.exports)
          exportedOwn = Map.filterKeys (\k -> Set.member k exported) ownSigs
        pure
          ( Right
              { env: optimised.env
              , foreignEnv: Map.union exportedOwn (Map.union optimised.published st.foreignEnv)
              , visibles: Map.insert item.name moduleCtx st.visibles
              , total: backend.mergeContext contrib st.total
              , emitted: Array.snoc st.emitted { artifact, path }
              , lowered: Array.snoc st.lowered lm
              , progress: st.progress { current = st.progress.current + 1 }
              }
          )

  -- Iterate `optimizeModule` to a fixpoint (ADR-0087 owns the iteration; the seam is a pure step returning
  -- `{ module, summary, quarantine, events }`). Plain recursion — so `m` needs only `Monad` (ADR-0087's
  -- boundary), no `MonadRec`: the depth is a small config cap, not the module count. It stops **on
  -- convergence** (a round leaves the module unchanged — `AnfModule` has structural `Eq`) or, as a
  -- non-termination backstop, at `maxOptimizeIter`. Returns the last pass's `{ module, summary }`.
  -- `lf` is supplied by the caller (with the module's own foreign shapes injected, ADR-0090 §3), and
  -- `env` carries the dependency dict machinery + foreign shapes.
  --
  -- Sticky backstop quarantine (ADR-0089 Addendum 2026-07-16): the driver owns the quarantine's
  -- *lifetime* only — created empty here (before `onEnterOptimizeIter`), threaded opaquely through
  -- the rounds, gone when this module's fixpoint returns (never cross-module, never persisted).
  -- Each round's `RejectionEvent`s are dispatched to `onOptimizerBackstop` as they arrive; the
  -- final counts go through the same hook as a `BackstopSummary` (the Addendum's named option),
  -- only when something fired. `rejectionAttempts` counts NbE runs that tripped (a quarantined
  -- skip produces no event, so it is not an attempt); `distinctBindings` counts unique keys.
  runOptimizer env lf am = do
    action.hooks.onEnterOptimizeIter am
    let
      firedKey = case _ of
        BackstopFired r -> Just r.key
        BackstopSummary _ -> Nothing
      go n st prev = do
        let next = Optimizer.optimizeModule env lf st.quarantine prev
        traverse_ action.hooks.onOptimizerBackstop next.events
        let
          -- count `BackstopFired` only: the seam currently emits nothing else, but the counter's
          -- contract is "NbE runs that tripped", so a future summary-shaped event must not inflate it.
          fired = Array.mapMaybe firedKey next.events
          st' =
            { quarantine: next.quarantine
            , attempts: st.attempts + Array.length fired
            , keys: Set.union st.keys (Set.fromFoldable fired)
            }
        if next.module == prev || n + 1 >= action.maxOptimizeIter then do
          when (st'.attempts > 0) do
            action.hooks.onOptimizerBackstop
              (BackstopSummary { rejectionAttempts: st'.attempts, distinctBindings: Set.size st'.keys })
          action.hooks.onLeaveOptimizeIter next.module
            $> { module: next.module, summary: next.summary }
        else
          action.hooks.onContinueOptimizeIter (n + 1) next.module *> go (n + 1) st' next.module
    go 0 { quarantine: emptyQuarantine, attempts: 0, keys: Set.empty } am