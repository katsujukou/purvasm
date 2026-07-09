-- | The native (LLVM) build driver: **separate per-module compilation** (B2, ADR-0082 §3 / the modular
-- | principle [optimizer-modular-not-whole-program]). Each CoreFn module is lowered to its own native
-- | `Gdef`s independently (CoreFn → CESK → ANF), and emitted as its own `.ll` object; the init/entry
-- | object carries `pv_init_all` (the reachable inits, computed over the binding graph) + `@main`.
-- |
-- | This replaces boot's whole-program-link-then-partition (B1): no giant single `Let`-spine is built,
-- | so a module's object depends only on its own source (cacheable) and the architecture never holds the
-- | whole program as one term. Program-level reachability (which bindings the entry needs) is a graph
-- | over the gdefs' references — `entryLl`'s `reachableGdefs` — not a whole-program body load; unreachable
-- | symbols are dead-stripped by the linker (`--gc-sections`).
module Purvasm.Compiler.Backend.LLVM.Driver
  ( NativeOptions
  , LlvmBackendOptions
  , LlvmContext
  , gdefsOfModule
  , llvmBackend
  , nativeSplit
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler (Backend)
import Purvasm.Compiler.Backend.LLVM.FreeVars (cfExpr)
import Purvasm.Compiler.Backend.LLVM.Interface (interfaceOfAnf)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (classifyDecl, entryLl, gdefKeys, moduleLl, surfaceFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), Gdef(..), SplitOutput)
import Purvasm.Compiler.CESK.Translate (qualifiedKey)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule)
import Purvasm.Compiler.MiddleEnd.Optimizer (BuildEnv, emptyBuildEnv, extendSummary, optimizeModule, preOptimizeEntry, preOptimizeModule, summaryOfLocal)

-- | Native-build knobs (mirrors boot's `program_split` optionals). The cross-module export surface is
-- | now derived from each module's own gdefs (ADR-0085): a module's compile produces both its `.ll` and
-- | its export facts, so no redundant bytecode `compileModule` is needed to read them.
type NativeOptions =
  { isEffect :: Boolean
  , heapWords :: Int
  , debug :: Boolean
  -- | `true` on the `--opt` path: run the `(optimize)` seam and grow the cross-module `BuildEnv`.
  -- | `false` (`--no-opt`) skips both — only the required lowering (`DictElim`) runs, byte-identical to
  -- | boot.
  , opt :: Boolean
  }

-- | One CoreFn module → its native `Gdef`s: lower to the neutral `AnfModule` (`declsOfModule`, shared
-- | middle-end) then classify each `Decl` (`classifyDecl`, ADR-0086 §4 — a backend concern that runs after
-- | the seam). Used for the pre-optimisation program facts (cross-module surface / `gkeys`), which are
-- | stable under the seam's body rewrites, and by the interface differential test.
gdefsOfModule :: CF.Module -> Array Gdef
gdefsOfModule = map classifyDecl <<< _.decls <<< declsOfModule

-- | The native foreign keys a gdef's bodies reference.
cfGdef :: Gdef -> Set String
cfGdef = case _ of
  Gfun _ _ b -> cfExpr b
  Gcaf _ b -> cfExpr b
  Grec ms -> foldl (\s (Tuple _ b) -> Set.union s (cfExpr b)) Set.empty ms

-- | A module's cross-module export surface (ADR-0077 §2, ADR-0085): its **public** exports (CoreFn
-- | `exports`, qualified) mapped from their native gdef shape to a `CallFact` (`Gfun`→`Cfn`, a recursive
-- | `Grec` function member→`Crecfn`). Derived from the module's own gdefs — the same facts boot read from
-- | `interfaceOf`, without the redundant bytecode compile.
deriveSurface :: CF.Module -> Array Gdef -> Map String CallFact
deriveSurface m gdefs = foldl over Map.empty gdefs
  where
  exports = Set.fromFoldable (map (qualifiedKey m.name) m.exports)

  over acc = case _ of
    Gfun k ps _
      | Set.member k exports -> Map.insert k (Cfn (Array.length ps)) acc
      | otherwise -> acc
    Grec ms -> foldl overMember acc ms
    Gcaf _ _ -> acc

  overMember acc (Tuple mk rhs) = case rhs of
    Ret (CLam ps _) | Set.member mk exports -> Map.insert mk (Crecfn (Array.length ps)) acc
    _ -> acc

-- | The compiled result of the shared per-module pipeline: each module's optimised `AnfModule` and its
-- | classified gdefs, the combined gdefs, the `DictElim`'d entry, and the codegen context options.
type Compiled =
  { perModule :: Array { module :: AnfModule, gdefs :: Array Gdef }
  , allGdefs :: Array Gdef
  , entry :: Expr
  , cxOpts :: MakeCxOptions
  }

-- | The shared per-module pipeline over the ADR-0086 optimiser seam (ADR-0085/0087): lower each module to
-- | its neutral `AnfModule` (`declsOfModule`), then **fold over the modules in dependency order threading
-- | the in-memory `BuildEnv`** — `preOptimizeModule` (the byte-identity `DictElim` bridge) → (`--opt`)
-- | `optimizeModule` → `extendSummary` — and classify each finished module's `Decl`s into gdefs
-- | (`classifyDecl`, a backend concern after the seam). The pre-optimisation classification (`gdefsOfModule`)
-- | fixes the program-wide key set and cross-module surface (both stable under the seam's body rewrites).
-- | The entry expression is `DictElim`'d under the fully-accumulated env (`preOptimizeEntry`). `nativeSplit`
-- | (`.ll`) consumes this; per-module ANF inspection is now the build driver's `--emit-ir` hooks (ADR-0087
-- | §3.1), not a whole-module pretty dump.
compileModules :: NativeOptions -> Array CF.Module -> Expr -> Compiled
compileModules opts modules entry =
  let
    inlineAbi = not opts.debug
    compiled0 = map (\m -> { mod: m, am: declsOfModule m, gdefs0: gdefsOfModule m }) modules
    allGdefs0 = Array.concatMap _.gdefs0 compiled0
    gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs0)
    surface = foldl (\acc c -> Map.union (deriveSurface c.mod c.gdefs0) acc) Map.empty compiled0
    xfns = foldl (surfaceFn surface) Map.empty allGdefs0

    -- The legacy pure fold applies `optimizeModule` (a single seam pass, ADR-0086 §3) once. It is
    -- idempotent over an already-`DictElim`'d body, so under `--opt` this is byte-identical to `--no-opt`
    -- today; the *iterated* fixpoint lives in the effectful `Purvasm.Compiler.build` driver (ADR-0087),
    -- which this path is being superseded by.
    compileOne :: BuildEnv -> _ -> _
    compileOne env c =
      let
        pre = preOptimizeModule env c.am
        opted =
          if opts.opt then optimizeModule env pre.localFacts pre.module
          else { module: pre.module, summary: summaryOfLocal pre.localFacts }
      in
        { accum: extendSummary env opted.summary
        , value: { module: opted.module, gdefs: map classifyDecl opted.module.decls }
        }

    compiled = mapAccumL compileOne emptyBuildEnv compiled0
  in
    { perModule: compiled.value
    , allGdefs: Array.concatMap _.gdefs compiled.value
    , entry: preOptimizeEntry compiled.accum entry
    , cxOpts: { gkeys, xfns, inlineAbi }
    }

-- | Lower a whole program to per-module `.ll` objects + one init/entry object (B2, ADR-0085): emit each
-- | module's object from its post-`DictElim` gdefs; the entry object's `pv_init_all` runs only the
-- | entry-reachable inits (`entryLl` → `reachableGdefs` over the combined post-`DictElim` gdefs).
nativeSplit :: NativeOptions -> Array CF.Module -> Expr -> SplitOutput
nativeSplit opts modules entry =
  let
    c = compileModules opts modules entry
  in
    { modules: map (\m -> Tuple m.module.name (moduleLl c.cxOpts (Set.fromFoldable (Array.concatMap gdefKeys m.gdefs)) m.gdefs)) c.perModule
    , entry: entryLl c.cxOpts opts.isEffect opts.heapWords c.allGdefs c.entry
    , foreigns: foldl (\s g -> Set.union s (cfGdef g)) (cfExpr c.entry) c.allGdefs
    }

-- --- the ADR-0087 `Backend` value ------------------------------------------------------------------

-- | The backend-specific knobs the CLI closes into the LLVM `Backend` (native heap size, effect-vs-value
-- | entry, debug ABI) — everything `moduleLl`/`entryLl` need beyond the neutral module.
type LlvmBackendOptions =
  { isEffect :: Boolean
  , heapWords :: Int
  , debug :: Boolean
  }

-- | The whole-program context the LLVM backend derives once (ADR-0087 §2): the codegen options
-- | (`gkeys`/`xfns`/inline-ABI) plus the entry knobs `lowerEntry` needs.
type LlvmContext =
  { cxOpts :: MakeCxOptions
  , isEffect :: Boolean
  , heapWords :: Int
  }

-- | The LLVM backend as an ADR-0087 `Backend` (the effectful `Purvasm.Compiler.build` driver's pure
-- | codegen capability): per-module `.ll` (`lowerModule`), the whole-program init/entry `.ll`
-- | (`lowerEntry`), the module `.pmi` (`interfaceOf`), and the cross-module `context` derived from the
-- | pre-optimisation modules' surface. It calls the very same `moduleLl`/`entryLl`/`interfaceOfAnf` the
-- | pure `nativeSplit` path (and its byte-identity gate) uses, so the emitted `.ll`/`.pmi` bytes are
-- | identical — the driver only differs in *who* runs the seam fold (the library `build` vs `compileModules`).
llvmBackend :: LlvmBackendOptions -> Backend LlvmContext String
llvmBackend opts =
  { context: \modules ->
      let
        gdefs0Of lm = map classifyDecl lm.module.decls
        allGdefs0 = Array.concatMap gdefs0Of modules
        gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs0)
        surface = foldl (\acc lm -> Map.union (deriveSurface lm.source (gdefs0Of lm)) acc) Map.empty modules
        xfns = foldl (surfaceFn surface) Map.empty allGdefs0
      in
        { cxOpts: { gkeys, xfns, inlineAbi: not opts.debug }
        , isEffect: opts.isEffect
        , heapWords: opts.heapWords
        }
  , interfaceOf: \_ lm -> interfaceOfAnf lm.source (map classifyDecl lm.module.decls)
  , lowerModule: \ctx lm ->
      let
        gdefs = map classifyDecl lm.module.decls
        defined = Set.fromFoldable (Array.concatMap gdefKeys gdefs)
      in
        moduleLl ctx.cxOpts defined gdefs
  , lowerEntry: \ctx input ->
      entryLl ctx.cxOpts ctx.isEffect ctx.heapWords
        (Array.concatMap (\lm -> map classifyDecl lm.module.decls) input.modules)
        input.entry
  }
