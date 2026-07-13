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
import Data.Foldable (foldM, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
  -- | `--no-opt` (the byte-identity bridge only).
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

-- | The **pre-optimisation, pre-FSR** view a backend's `context` reads: a module's CoreFn source and its
-- | freshly-lowered `AnfModule`. It carries **no** `foreignSigs` — FSR has not run when `context` is
-- | derived (it runs per module inside the fold), so a shape map here could only ever be empty. Kept
-- | distinct from `LoweredModule` precisely so the whole-program surface derivation never reads an
-- | empty-shape lie (ADR-0090): foreign shapes belong to the per-module lowering inputs, not the context.
type ContextModule =
  { source :: CF.Module
  , module :: AnfModule
  }

-- | An optimised module paired with its CoreFn source (the backend reads `imports`/`exports` for the
-- | interface and reachability) and its **visible** foreign shapes. The unit `lowerModule`/`lowerEntry`
-- | codegen consumes (distinct from `ContextModule`, the pre-FSR whole-program `context` input).
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
-- | and an internal whole-program context `c` it derives once. Deliberately effect-free: link — the one
-- | effectful, non-shareable step — is left to CLI finalization (§4), so this record is trivially testable.
-- |
-- |   * `context` — derive the whole-program facts (the native `gkeys`/cross-module surface, stable under
-- |     the seam) from the **pre-optimisation** modules (as `ContextModule`s — CoreFn source + lowered ANF,
-- |     so the surface can read each module's `exports`, but **no** foreign shapes: FSR has not run yet),
-- |     once, before the fold.
-- |   * `interfaceOf` — the module's `.pmi` from its optimised ANF + CoreFn surface (backend-neutral in
-- |     result — byte-identical to the bytecode deriver — but the `ExportKind` classification is the
-- |     backend's).
-- |   * `lowerModule` — one optimised module → its backend IR.
-- |   * `lowerEntry` — the **whole-program** entry/init object (reachability over the module set); pure
-- |     codegen, run by the driver after the fold, *not* CLI link.
type Backend c o =
  { context :: Array ContextModule -> c
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

-- | The per-module fold state: the threaded optimiser env, and the emitted artifacts / optimised modules
-- | accumulated in dependency order.
type FoldState o =
  { env :: BuildEnv
  -- | ADR-0090's `ForeignFacts`: the accumulated **dependencies' exported** foreign shapes, threaded in
  -- | every mode. Feeds `LoweredModule.foreignSigs` (codegen) and, under `--opt`, the optimiser env.
  , foreignEnv :: ForeignSigMap
  , emitted :: Array (EmittedModule o)
  , lowered :: Array LoweredModule
  , progress :: Progress
  }

-- | Build a whole program to per-module artifacts + the entry object (ADR-0087 §1), stopping at pure
-- | codegen. The driver is **neutral** (ADR-0086 Addendum): load the closure, and per module in dependency
-- | order `CoreFn → normalise → AnfModule`, then `if --opt` **iterate `optimizeModule` to a fixpoint** (up
-- | to `action.maxOptimizeIter` rounds) + `extendSummary`, `else` the identity — it runs **no `DictElim`
-- | of its own** (a backend's `--no-opt` boot-parity need is its own private lowering). Emit each module via
-- | `emitFile`, then run the backend's whole-program `lowerEntry` over the (raw) entry + reachable set and
-- | emit it via `emitEntry`. Returns `BuildProducts` for CLI finalization (link). Never links.
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
        -- `ContextModule`: pre-FSR source + lowered ANF only. FSR runs per module inside the fold, so a
        -- `foreignSigs` here would be uniformly empty — the type omits it rather than lie (ADR-0090).
        ctx = backend.context (map (\l -> { source: l.mod, module: declsOfModule l.mod }) loaded)
        initialState =
          { env: emptyBuildEnv
          , foreignEnv: Map.empty
          , emitted: []
          , lowered: []
          , progress: { total: Array.length loaded, current: 1 }
          }
        -- An FSR failure short-circuits the fold with `Left` (ADR-0090 §2), like `loadClosure`.
        step (Left e) _ = pure (Left e)
        step (Right st) item = stepModule ctx st item
      foldM step (Right initialState) loaded >>= case _ of
        Left err -> pure (Left err)
        Right final -> do
          -- The entry is handed to the backend **raw** (no seam `DictElim`); the native backend bridges it
          -- privately in `lowerEntry`, the VM does not (ADR-0086 Addendum).
          let entryIr = backend.lowerEntry ctx { modules: final.lowered, entry: entryExprOf opts }
          entryPath <- action.emitEntry entryIr
          pure (Right { modules: final.emitted, entry: { ir: entryIr, path: entryPath } })
  where
  stepModule :: c -> FoldState o -> { mod ∷ CF.Module, name ∷ String } -> m (Either BuildError (FoldState o))
  stepModule ctx st item = do
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
          artifact = { interface: backend.interfaceOf ctx lm, backendIR: backend.lowerModule ctx lm }
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
              , emitted: Array.snoc st.emitted { artifact, path }
              , lowered: Array.snoc st.lowered lm
              , progress: st.progress { current = st.progress.current + 1 }
              }
          )

  -- Iterate `optimizeModule` to a fixpoint (ADR-0087 owns the iteration; the seam is a pure step returning
  -- `{ module, summary }`). Plain recursion — so `m` needs only `Monad` (ADR-0087's boundary), no
  -- `MonadRec`: the depth is a small config cap, not the module count. It stops **on convergence** (a round
  -- leaves the module unchanged — `AnfModule` has structural `Eq`) or, as a non-termination backstop, at
  -- `maxOptimizeIter`. Today's only pass (`DictElim`) is idempotent, so a module with no static dispatch
  -- converges in one round. Returns the last pass's `{ module, summary }`.
  -- `lf` is supplied by the caller (with the module's own foreign shapes injected, ADR-0090 §3), and
  -- `env` carries the dependency dict machinery + foreign shapes.
  runOptimizer env lf am = do
    action.hooks.onEnterOptimizeIter am
    let
      go n prev = do
        let next = Optimizer.optimizeModule env lf prev
        if next.module == prev || n + 1 >= action.maxOptimizeIter then
          action.hooks.onLeaveOptimizeIter next.module $> next
        else
          action.hooks.onContinueOptimizeIter (n + 1) next.module *> go (n + 1) next.module
    go 0 am