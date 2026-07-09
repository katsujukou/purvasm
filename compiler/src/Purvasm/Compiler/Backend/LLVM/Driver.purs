-- | The native (LLVM) backend as an ADR-0087 `Backend` value (`llvmBackend`): the pure codegen capability
-- | the neutral `Purvasm.Compiler.build` driver drives. Separate per-module compilation (B2, ADR-0082 §3):
-- | each optimised `AnfModule` is classified to its own `Gdef`s and emitted as its own `.ll` object; the
-- | init/entry object carries `pv_init_all` (reachable inits over the binding graph) + `@main`.
-- |
-- | It also carries the **backend-private boot-parity `DictElim` bridge** (ADR-0086 Addendum): the shared
-- | build driver runs no `DictElim`, so the LLVM backend applies it in its own lowering (`lowerModule`/
-- | `lowerEntry`), resolving against the whole-program dictionary machinery derived once in `context`. This
-- | is a *transitional compatibility lowering* (boot's `--no-opt` `.ll` has dispatch collapsed, and Level-2
-- | must match byte-for-byte), **not** an optimiser pass — the real `DictElim` lives in the optimiser
-- | fixpoint, and this bridge is scheduled for removal when the LLVM codegen port off boot completes. The
-- | VM backend has no such bridge.
module Purvasm.Compiler.Backend.LLVM.Driver
  ( LlvmBackendOptions
  , LlvmContext
  , gdefsOfModule
  , llvmBackend
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler (Backend)
import Purvasm.Compiler.Backend.LLVM.Interface (interfaceOfAnf)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (classifyDecl, entryLl, gdefKeys, moduleLl, surfaceFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), Gdef(..))
import Purvasm.Compiler.CESK.Translate (qualifiedKey)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, dictElimExpr, emptyMachinery, machineryOf, mergeMachinery)

-- | One CoreFn module → its native `Gdef`s: lower to the neutral `AnfModule` (`declsOfModule`, shared
-- | middle-end) then classify each `Decl` (`classifyDecl`, ADR-0086 §4 — a backend concern that runs after
-- | the seam). Used for the pre-optimisation program facts (cross-module surface / `gkeys`), which are
-- | stable under the seam's body rewrites, and by the interface differential test.
gdefsOfModule :: CF.Module -> Array Gdef
gdefsOfModule = map classifyDecl <<< _.decls <<< declsOfModule

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

-- --- the ADR-0087 `Backend` value ------------------------------------------------------------------

-- | The backend-specific knobs the CLI closes into the LLVM `Backend` (native heap size, effect-vs-value
-- | entry, debug ABI) — everything `moduleLl`/`entryLl` need beyond the neutral module.
type LlvmBackendOptions =
  { isEffect :: Boolean
  , heapWords :: Int
  , debug :: Boolean
  }

-- | The whole-program context the LLVM backend derives once (ADR-0087 §2): the codegen options
-- | (`gkeys`/`xfns`/inline-ABI), the whole-program dictionary `machinery` the private bridge resolves
-- | against, plus the entry knobs `lowerEntry` needs.
type LlvmContext =
  { cxOpts :: MakeCxOptions
  , machinery :: DictMachinery
  , isEffect :: Boolean
  , heapWords :: Int
  }

-- | The backend-private boot-parity `DictElim` bridge (ADR-0086 Addendum): collapse statically-known
-- | dispatch to direct impl calls, resolving against the whole-program `machinery` and `gkeys` derived in
-- | `context`. Whole-program machinery is a no-collision superset of any module's dependency-visible
-- | machinery (keys are module-qualified), so it resolves byte-identically to the per-module threading of
-- | ADR-0086 §3 for well-typed programs. A **transitional compatibility lowering**, not an optimiser pass.
nativeByteIdentityBridgeDictElim :: LlvmContext -> Expr -> Expr
nativeByteIdentityBridgeDictElim ctx = dictElimExpr ctx.cxOpts.gkeys ctx.machinery

bridgeModule :: LlvmContext -> AnfModule -> AnfModule
bridgeModule ctx m = m { decls = map (mapDeclBodies (nativeByteIdentityBridgeDictElim ctx)) m.decls }

-- | The LLVM backend as an ADR-0087 `Backend` (the neutral `build` driver's pure codegen capability):
-- | per-module `.ll` (`lowerModule`), the whole-program init/entry `.ll` (`lowerEntry`), the module `.pmi`
-- | (`interfaceOf`), and the cross-module `context` (surface / `gkeys` / dictionary machinery) derived from
-- | the pre-optimisation modules. `lowerModule`/`lowerEntry` apply the private `DictElim` bridge before
-- | classifying, so native `--no-opt` stays byte-identical to boot even though the driver runs no `DictElim`.
llvmBackend :: LlvmBackendOptions -> Backend LlvmContext String
llvmBackend opts =
  { context: \modules ->
      let
        gdefs0Of lm = map classifyDecl lm.module.decls
        allGdefs0 = Array.concatMap gdefs0Of modules
        gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs0)
        surface = foldl (\acc lm -> Map.union (deriveSurface lm.source (gdefs0Of lm)) acc) Map.empty modules
        xfns = foldl (surfaceFn surface) Map.empty allGdefs0
        machinery = foldl
          (\acc lm -> mergeMachinery (machineryOf (Array.concatMap _.members lm.module.decls)) acc)
          emptyMachinery
          modules
      in
        { cxOpts: { gkeys, xfns, inlineAbi: not opts.debug }
        , machinery
        , isEffect: opts.isEffect
        , heapWords: opts.heapWords
        }
  -- The `.pmi` surface (export kinds/arities) is unchanged by `DictElim` (it rewrites bodies, not
  -- top-level keys/arities), so the interface is byte-identical whether taken over the raw or bridged module.
  , interfaceOf: \_ lm -> interfaceOfAnf lm.source (map classifyDecl lm.module.decls)
  , lowerModule: \ctx lm ->
      let
        gdefs = map classifyDecl (bridgeModule ctx lm.module).decls
        defined = Set.fromFoldable (Array.concatMap gdefKeys gdefs)
      in
        moduleLl ctx.cxOpts defined gdefs
  , lowerEntry: \ctx input ->
      entryLl ctx.cxOpts ctx.isEffect ctx.heapWords
        (Array.concatMap (\lm -> map classifyDecl (bridgeModule ctx lm.module).decls) input.modules)
        (nativeByteIdentityBridgeDictElim ctx input.entry)
  }
