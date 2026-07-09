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
  , EmittedModule
  , EntryInput
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
import Purvasm.Compiler.CESK.Translate (nameKey)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer (BuildEnv, emptyBuildEnv, extendSummary, localFactsOf)
import Purvasm.Compiler.MiddleEnd.Optimizer as Optimizer

-- | A module's dotted name (the `loadModule` key).
type ModuleName = String

-- | A host load failure reduced to a library-neutral shape (ADR-0087 §2.1): the richer original error
-- | stays host-side (the host logs it in its `loadModule`/`onLoadFailed` impl). Concrete, not a polymorphic
-- | `e`, so `CompilerAction` stays at two type parameters.
type LoadError = { moduleName :: String, detail :: String }

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

-- | An optimised module paired with its CoreFn source (the backend reads `imports`/`exports` for the
-- | interface and reachability). The unit `Backend` codegen consumes.
type LoweredModule = { source :: CF.Module, module :: AnfModule }

-- | The whole reachable program, for the entry/init codegen: every optimised module plus the (DictElim'd)
-- | entry expression.
type EntryInput = { modules :: Array LoweredModule, entry :: Expr }

-- | The **pure** backend codegen capability (ADR-0087 §2), parameterised over its output IR `o` (VM/LLVM)
-- | and an internal whole-program context `c` it derives once. Deliberately effect-free: link — the one
-- | effectful, non-shareable step — is left to CLI finalization (§4), so this record is trivially testable.
-- |
-- |   * `context` — derive the whole-program facts (the native `gkeys`/cross-module surface, stable under
-- |     the seam) from the **pre-optimisation** modules (as `LoweredModule`s, so the surface can read each
-- |     module's CoreFn `exports`), once, before the fold.
-- |   * `interfaceOf` — the module's `.pmi` from its optimised ANF + CoreFn surface (backend-neutral in
-- |     result — byte-identical to the bytecode deriver — but the `ExportKind` classification is the
-- |     backend's).
-- |   * `lowerModule` — one optimised module → its backend IR.
-- |   * `lowerEntry` — the **whole-program** entry/init object (reachability over the module set); pure
-- |     codegen, run by the driver after the fold, *not* CLI link.
type Backend c o =
  { context :: Array LoweredModule -> c
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

-- | The program entry expression: an `Effect` entry (`isEffect`) is `<entry>.<name>` applied to unit (the
-- | `0` convention), run for effects; a bare value is read directly and printed.
entryExprOf :: Options -> Expr
entryExprOf opts =
  let
    key = opts.entryModule <> "." <> opts.entryName
  in
    if opts.isEffect then normalize (TmApp (TmVar key) (TmLit (LInt 0)))
    else normalize (TmVar key)

-- | The per-module fold state: the threaded optimiser env, and the emitted artifacts / optimised modules
-- | accumulated in dependency order.
type FoldState o =
  { env :: BuildEnv
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
        ctx = backend.context (map (\l -> { source: l.mod, module: declsOfModule l.mod }) loaded)
        initialState =
          { env: emptyBuildEnv
          , emitted: []
          , lowered: []
          , progress: { total: Array.length loaded, current: 1 }
          }
      final <- foldM (stepModule ctx) initialState loaded
      let
        -- The entry is handed to the backend **raw** (no seam `DictElim`); the native backend bridges it
        -- privately in `lowerEntry`, the VM does not (ADR-0086 Addendum).
        entryIr = backend.lowerEntry ctx { modules: final.lowered, entry: entryExprOf opts }
      entryPath <- action.emitEntry entryIr
      pure (Right { modules: final.emitted, entry: { ir: entryIr, path: entryPath } })
  where
  stepModule :: c -> FoldState o -> { mod ∷ CF.Module, name ∷ String } -> m (FoldState o)
  stepModule ctx st item = do
    action.hooks.onStartCompile st.progress item.mod
    let am = declsOfModule item.mod
    action.hooks.onBeforeOptimize am
    optimised <-
      if opts.opt then do
        r <- runOptimizer st.env am
        pure { module: r.module, env: extendSummary st.env r.summary }
      else pure { module: am, env: st.env } -- --no-opt: identity, no summary, env unchanged
    action.hooks.onBeforeCodegen optimised.module
    let
      lm = { source: item.mod, module: optimised.module }
      artifact = { interface: backend.interfaceOf ctx lm, backendIR: backend.lowerModule ctx lm }
    path <- action.emitFile artifact
    action.hooks.onCleanUp item.name
    pure
      { env: optimised.env
      , emitted: Array.snoc st.emitted { artifact, path }
      , lowered: Array.snoc st.lowered lm
      , progress: st.progress { current = st.progress.current + 1 }
      }

  -- Iterate `optimizeModule` to a fixpoint (ADR-0087 owns the iteration; the seam is a pure step returning
  -- `{ module, summary }`). Plain recursion — so `m` needs only `Monad` (ADR-0087's boundary), no
  -- `MonadRec`: the depth is a small config cap, not the module count. It stops **on convergence** (a round
  -- leaves the module unchanged — `AnfModule` has structural `Eq`) or, as a non-termination backstop, at
  -- `maxOptimizeIter`. Today's only pass (`DictElim`) is idempotent, so a module with no static dispatch
  -- converges in one round. Returns the last pass's `{ module, summary }`.
  runOptimizer env am = do
    let lf = localFactsOf am
    action.hooks.onEnterOptimizeIter am
    let
      go n prev = do
        let next = Optimizer.optimizeModule env lf prev
        if next.module == prev || n + 1 >= action.maxOptimizeIter then
          action.hooks.onLeaveOptimizeIter next.module $> next
        else
          action.hooks.onContinueOptimizeIter (n + 1) next.module *> go (n + 1) next.module
    go 0 am