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
import Data.Maybe (Maybe(..), isNothing)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler (Backend, ForeignSigMap)
import Purvasm.Compiler.Backend.LLVM.Interface (interfaceOfAnf)
import Purvasm.Compiler.Backend.LLVM.Monad (MakeCxOptions)
import Purvasm.Compiler.Backend.LLVM.Program (classifyDecl, classifyNonrec, entryLl, gdefInitKey, gdefKeys, moduleLl, surfaceFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), Gdef(..))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (qualifiedKey)
import Purvasm.Compiler.Ffi (resolver)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), mapAtoms)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, dictElimExpr, emptyMachinery, machineryOf, mergeMachinery, noForeignLift)

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
-- | Resolve the **compiler-builtin literal** free references — the resolver intrinsics whose definition
-- | is a plain literal (`Prim.undefined` and `Data.Unit.unit`, both the immediate `0`, ADR-0038) — to
-- | their literal atom, so `Prim.undefined` no longer reaches codegen as an unbound `AtomVar`. These are
-- | representation choices (a `Unit`/absurd placeholder), *not* FFI: unlike a primop (category B, the
-- | optimiser's `intrinsicPrim`) or a structural higher-order foreign (category C, a guest term
-- | `synthForeignGdefs` materialises as a gdef, ADR-0071 §6), they carry no code. boot resolves them at
-- | link (before its `dict_elim`), so rewriting
-- | to a literal atom here — not at codegen `readVar` (which would leave it an `AtomVar` that `evalAtoms`
-- | may root, unlike boot's immediate) — keeps the rooting boot-identical. Runs before the `DictElim`
-- | bridge to match boot's phase order (resolve → dict-elim).
resolveLitBuiltins :: Expr -> Expr
resolveLitBuiltins = mapAtoms case _ of
  a@(AtomVar k) -> case resolver k of
    Just (TmLit lit) -> AtomLit lit
    _ -> a
  a -> a

-- | Materialise a module's **compiler-resolved** foreign imports as synthesised gdefs (boot's link
-- | `foreign_groups`, ADR-0038 / ADR-0071 §6): for each `foreign import f` whose key `M.f` the resolver
-- | binds to a non-literal guest term, emit a gdef `M.f = <normalised term>` in **this** module's object
-- | (matching boot's `module_of_key` placement). This covers **both** rungs boot resolves at link:
-- | the compiler-kept **intrinsics** (primop etas, `charId`/`intDegree`, `unsafeCoerce`) *and* the
-- | **structural higher-order** guest terms (the `Effect`/`ST` monad glue, `Effect.Ref`/`STRef` as a
-- | one-cell mutable array over the array primops — ADR-0071 §6 / ADR-0072 §9, `Fn*`/`ST*` uncurried
-- | adapters, `Record.Builder`, `Data.Number.fromString`). boot draws no line between the two — every
-- | `resolver`-resolved foreign is a link-time `foreign_group` — so neither does the native backend; a
-- | structural foreign is a guest term substituted before codegen, **not** a host `.c` leaf. Literal
-- | builtins (`unit`/`undefined`) are excluded — inlined by `resolveLitBuiltins`; genuine native leaves
-- | (`resolver k = Nothing`) are excluded too — they lower to a `@pvf_` symbol (`resolveNativeForeigns`).
-- |
-- | The gdefs are emitted in **key order** and (at the call sites) **before** the module's own decls,
-- | mirroring boot's `foreign_groups` (sorted by key, ADR link.ml) prepended to `module_reached`. Under
-- | `--opt` the optimiser inlines/impurifies a saturated call, so the gdef is dead-stripped; under
-- | `--no-opt` the call stays and resolves to this gdef — the byte-identity reference (boot synthesises
-- | the same gdef). Reads only the CoreFn `source`, so it is row-polymorphic over its input — the
-- | whole-program `context` (a `ContextModule`) and the per-module lowering (a `LoweredModule`) both feed
-- | it (ADR-0090).
synthForeignGdefs :: forall r. { source :: CF.Module | r } -> Array Gdef
synthForeignGdefs lm = Array.sortWith gdefInitKey (Array.mapMaybe synth lm.source.foreignNames)
  where
  synth fn =
    let
      key = qualifiedKey lm.source.name fn
    in
      case resolver key of
        Just t | not (isLit t) -> Just (classifyNonrec key (normalize t))
        _ -> Nothing

  isLit = case _ of
    TmLit _ -> true
    _ -> false

-- | The **native leaves** among a module's (accumulated own ∪ deps) foreign shapes (ADR-0090): the
-- | foreign keys the compiler does NOT itself resolve (`resolver k = Nothing` — not a primop/`unsafeCoerce`
-- | intrinsic, not a literal builtin, not a structural higher-order term), mapped to their FSR closure
-- | arity. These are the genuine host-provided leaves (a `ulib` `.c` like `Data.Show.showNumberImpl`, or a
-- | runtime `pvf_` like `Purvasm.String.byteLength`) that lower to a `@pvf_<key>` link-time symbol. An
-- | intrinsic foreign is materialised as a gdef by `synthForeignGdefs` instead, so it is excluded here.
nativeLeafArities :: ForeignSigMap -> Map String Int
nativeLeafArities = Map.mapMaybeWithKey \k s -> if isNothing (resolver k) then Just s.arity else Nothing

-- | Resolve a **native leaf** free reference from `AtomVar` to `AtomForeign`, so codegen emits its
-- | `@pvf_<key>` symbol + a no-capture closure of its arity. Runs before the `DictElim` bridge (boot
-- | resolves native leaves before `dict_elim`), so a class method whose impl *is* a native leaf (e.g.
-- | `Show Number`'s `showNumberImpl`) is carried into the instance dictionary as the foreign closure.
resolveNativeForeigns :: Map String Int -> Expr -> Expr
resolveNativeForeigns leaves = mapAtoms case _ of
  AtomVar k | Map.member k leaves -> AtomForeign k
  a -> a

-- | The native-private required lowerings applied before `Gdef` classification (ADR-0086 Addendum), in
-- | boot's phase order: resolve native leaves (`leaves`) and compiler-builtin literals, then the
-- | boot-parity `DictElim` bridge.
nativeByteIdentityBridgeDictElim :: LlvmContext -> Map String Int -> Expr -> Expr
nativeByteIdentityBridgeDictElim ctx leaves =
  -- `noForeignLift`: nothing runs between this bridge and codegen, so a lifted intrinsic
  -- `AtomVar "Purvasm.Int.add"` would reach `readVar` unbound (no saturation pass follows here).
  dictElimExpr noForeignLift ctx.cxOpts.gkeys ctx.machinery
    <<< resolveLitBuiltins
    <<< resolveNativeForeigns leaves

bridgeModule :: LlvmContext -> Map String Int -> AnfModule -> AnfModule
bridgeModule ctx leaves m = m { decls = map (mapDeclBodies (nativeByteIdentityBridgeDictElim ctx leaves)) m.decls }

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
        -- synthesised intrinsic-foreign keys are top-level globals too (referenced cross-module by `$root`).
        synthKeys = Array.concatMap (Array.concatMap gdefKeys <<< synthForeignGdefs) modules
        gkeys = Set.fromFoldable (Array.concatMap gdefKeys allGdefs0 <> synthKeys)
        surface = foldl (\acc lm -> Map.union (deriveSurface lm.source (gdefs0Of lm)) acc) Map.empty modules
        xfns = foldl (surfaceFn surface) Map.empty allGdefs0
        -- Modules arrive in dependency order, so folding the accumulated machinery in as `imported`
        -- lets each module's instance recognition see through its dependencies' `$Dict` wrappers.
        machinery = foldl
          (\acc lm -> mergeMachinery (machineryOf acc (Array.concatMap _.members lm.module.decls)) acc)
          emptyMachinery
          modules
      in
        -- `foreignArity` is a per-module base — `lowerModule`/`lowerEntry` override it with the module's
        -- own native-leaf arities (`nativeLeafArities lm.foreignSigs`), threaded from FSR (ADR-0090).
        { cxOpts: { gkeys, xfns, foreignArity: Map.empty, inlineAbi: not opts.debug }
        , machinery
        , isEffect: opts.isEffect
        , heapWords: opts.heapWords
        }
  -- The `.pmi` surface (export kinds/arities) is unchanged by `DictElim` (it rewrites bodies, not
  -- top-level keys/arities), so the interface is byte-identical whether taken over the raw or bridged module.
  , interfaceOf: \_ lm -> interfaceOfAnf lm.source (map classifyDecl lm.module.decls)
  , lowerModule: \ctx lm ->
      let
        leaves = nativeLeafArities lm.foreignSigs
        -- Foreign gdefs first (boot's `foreign_groups`, key-sorted by `synthForeignGdefs`), then the
        -- module's own decls — the per-module view of boot's `foreign_groups @ module_reached`.
        gdefs = synthForeignGdefs lm <> map classifyDecl (bridgeModule ctx leaves lm.module).decls
        defined = Set.fromFoldable (Array.concatMap gdefKeys gdefs)
      in
        moduleLl (ctx.cxOpts { foreignArity = leaves }) defined gdefs
  , lowerEntry: \ctx input ->
      let
        -- the entry object's leaf set spans every module's leaves (the entry stub reaches any of them).
        allLeaves = foldl (\acc lm -> Map.union (nativeLeafArities lm.foreignSigs) acc) Map.empty input.modules
        -- Match boot's linked spine `foreign_groups @ module_reached`: **all** foreign gdefs across the
        -- program, sorted by key, precede **all** module decls (in dependency-spine order). `reachableGdefs`
        -- then prunes to the entry's closure, preserving this order for `pv_init_all`.
        allForeigns = Array.sortWith gdefInitKey (Array.concatMap synthForeignGdefs input.modules)
        allDecls = Array.concatMap (\lm -> map classifyDecl (bridgeModule ctx (nativeLeafArities lm.foreignSigs) lm.module).decls) input.modules
      in
        entryLl (ctx.cxOpts { foreignArity = allLeaves }) ctx.isEffect ctx.heapWords
          (allForeigns <> allDecls)
          (nativeByteIdentityBridgeDictElim ctx allLeaves input.entry)
  }
