-- | The native (LLVM) backend as an ADR-0087 `Backend` value (`llvmBackend`): the pure codegen capability
-- | the neutral `Purvasm.Compiler.build` driver drives. Separate per-module compilation (B2, ADR-0082 §3):
-- | each optimised `AnfModule` is classified to its own `Gdef`s and emitted as its own `.ll` object; the
-- | init/entry object carries `pv_init_all` (reachable inits over the binding graph) + `@main`.
-- |
-- | It also carries the **backend-required lowering** (`nativeRequiredLowering`, ADR-0104 §3): native
-- | leaf resolution (`AtomVar → AtomForeign`) and compiler-builtin literalisation, applied in BOTH modes
-- | before `Gdef` classification — required native lowering, not an optimisation. Under `--no-opt`,
-- | dictionaries stay dynamically dispatched, matching the VM's `--no-opt` semantics: the former
-- | boot-parity `DictElim` bridge (ADR-0086 Addendum) was removed by ADR-0104 §3/§5 step 3 — the real
-- | `DictElim` lives only in the optimiser fixpoint.
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
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), mapAtoms)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

-- | One CoreFn module → its native `Gdef`s: lower to the neutral `AnfModule` (`declsOfModule`, shared
-- | middle-end) then classify each `Decl` (`classifyDecl`, ADR-0086 §4 — a backend concern that runs after
-- | the seam). Used for the pre-optimisation program facts (cross-module surface / `gkeys`) — a stable
-- | *base* set of existing source declarations, which the seam's body rewrites do not disturb. It is **not**
-- | the complete final key set: Specialize (ADR-0089) adds post-opt `$spec$` clones; `moduleLl`/`entryLl`
-- | therefore complete `gkeys` from each object's final gdef keys. Also used by the interface differential test.
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

-- | The LLVM backend's cross-module context (ADR-0087 §2 / ADR-0104 §5-1) — a merge-able bag of
-- | module-keyed facts: the codegen options (`gkeys`/`xfns`/inline-ABI) plus the entry knobs
-- | `lowerEntry` needs. `lowerModule` receives the import-closure projection; `lowerEntry` the
-- | whole-program accumulation.
type LlvmContext =
  { cxOpts :: MakeCxOptions
  , isEffect :: Boolean
  , heapWords :: Int
  }

-- | Resolve the **compiler-builtin literal** free references — the resolver intrinsics whose definition
-- | is a plain literal (`Prim.undefined` and `Data.Unit.unit`, both the immediate `0`, ADR-0038) — to
-- | their literal atom, so `Prim.undefined` no longer reaches codegen as an unbound `AtomVar`. These are
-- | representation choices (a `Unit`/absurd placeholder), *not* FFI: unlike a primop (category B, the
-- | optimiser's `intrinsicPrim`) or a structural higher-order foreign (category C, a guest term
-- | `synthForeignGdefs` materialises as a gdef, ADR-0071 §6), they carry no code. Rewriting to a literal
-- | atom here — not at codegen `readVar` (which would leave it an `AtomVar` that `evalAtoms` may root,
-- | unlike an immediate) — keeps the reference rooting-free, the representation the immediate deserves.
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
-- | `--no-opt` the call stays and resolves to this gdef — the optimiser-free reference lowering
-- | (ADR-0104 §3). Reads only the CoreFn `source`, so it is row-polymorphic over its input — the
-- | per-module context contribution (`moduleContext`, a `ContextModule`) and the per-module lowering
-- | (a `LoweredModule`) both feed it (ADR-0090).
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
-- | intrinsic, not a literal builtin, not a structural higher-order term), mapped to their **physical
-- | closure arity** — `leafClosureArity` of the FSR shape, *not* the raw semantic `shape.arity` (which
-- | differs for a nullary `Effect` leaf; see `leafClosureArity`). These are the genuine host-provided
-- | leaves (a `ulib` `.c` like `Data.Show.showNumberImpl`, or a runtime `pvf_` like
-- | `Purvasm.String.byteLength`) that lower to a `@pvf_<key>` link-time symbol. An intrinsic foreign is
-- | materialised as a gdef by `synthForeignGdefs` instead, so it is excluded here.
nativeLeafArities :: ForeignSigMap -> Map String Int
nativeLeafArities = Map.mapMaybeWithKey \k s -> if isNothing (resolver k) then Just (leafClosureArity s) else Nothing

-- | The **leaf closure arity** — the arity of the no-capture closure the backend builds for a native leaf
-- | reference (`Emit.atom` `AForeign`), matching boot's `Ffi.foreign_arity`. This is **not** the raw FSR
-- | arrow count (`ForeignShape.arity`): a native `Effect`/`ST` leaf is a **thunk**, so a *nullary* one — an
-- | `Effect a` with no preceding arrow (`Purvasm.System.Process.argvImpl :: Effect (Array String)`, FSR
-- | `arity 0, retVsat`) — *is* the effect action, and its closure takes the unit-run: arity 1. A leaf with
-- | ≥ 1 data arg returns a *fresh* effect thunk when saturated (`leaf_write_line` builds a `\$u -> …`
-- | closure), so its closure arity stays the data-arg count. Building a nullary `Effect` leaf at arity 0
-- | makes `run`'s unit application **over-apply** an already-fired leaf onto its own result — e.g. `argv
-- | unit` applies `unit` to the returned `Array`, a "not callable (kind Array)" fault / heap corruption.
leafClosureArity :: ForeignShape -> Int
leafClosureArity s = if s.retVsat then max s.arity 1 else s.arity

-- | Resolve a **native leaf** free reference from `AtomVar` to `AtomForeign`, so codegen emits its
-- | `@pvf_<key>` symbol + a no-capture closure of its arity. Runs on whole decl bodies before
-- | classification, so a class method whose impl *is* a native leaf (e.g. `Show Number`'s
-- | `showNumberImpl`) is carried into the instance dictionary as the foreign closure — under dynamic
-- | dispatch the dictionary itself must hold a callable value, not an unbound `AtomVar`.
resolveNativeForeigns :: Map String Int -> Expr -> Expr
resolveNativeForeigns leaves = mapAtoms case _ of
  AtomVar k | Map.member k leaves -> AtomForeign k
  a -> a

-- | The native-required lowering applied before `Gdef` classification in BOTH modes (ADR-0104 §3):
-- | resolve native leaves (`leaves`) and compiler-builtin literals. Required lowering, not an
-- | optimisation — without it, leaf and builtin references reach codegen as unbound `AtomVar`s.
nativeRequiredLowering :: Map String Int -> Expr -> Expr
nativeRequiredLowering leaves =
  resolveLitBuiltins
    <<< resolveNativeForeigns leaves

lowerModuleDecls :: Map String Int -> AnfModule -> AnfModule
lowerModuleDecls leaves m = m { decls = map (mapDeclBodies (nativeRequiredLowering leaves)) m.decls }

-- | The LLVM backend as an ADR-0087 `Backend` (the neutral `build` driver's pure codegen capability):
-- | per-module `.ll` (`lowerModule`), the whole-program init/entry `.ll` (`lowerEntry`), the module `.pmi`
-- | (`interfaceOf`), and the cross-module context facts (surface / `gkeys`) contributed per module
-- | (`moduleContext`, ADR-0104 §5-1) from its pre-optimisation lowering. `lowerModule`/`lowerEntry`
-- | apply the required lowering (`nativeRequiredLowering`) before classifying; dictionaries stay
-- | dynamically dispatched under `--no-opt` (ADR-0104 §3 — the boot-parity `DictElim` bridge is gone).
llvmBackend :: LlvmBackendOptions -> Backend LlvmContext String
llvmBackend opts =
  { emptyContext:
      { cxOpts: { gkeys: Set.empty, xfns: Map.empty, foreignArity: Map.empty, inlineAbi: not opts.debug }
      , isEffect: opts.isEffect
      , heapWords: opts.heapWords
      }
  -- The `Set.union`/`Map.union` shape satisfies the seam's idempotent-commutative-monoid contract:
  -- the driver merges OVERLAPPING projections (diamond deps share their closure's facts), and two
  -- valid contexts agree wherever they overlap — every key carries its defining module's one true
  -- fact — so union is idempotent and the `Map.union` bias is unobservable. The scalar knobs are
  -- constants closed from `opts`.
  , mergeContext: \newer older ->
      { cxOpts:
          { gkeys: Set.union newer.cxOpts.gkeys older.cxOpts.gkeys
          , xfns: Map.union newer.cxOpts.xfns older.cxOpts.xfns
          , foreignArity: Map.empty
          , inlineAbi: not opts.debug
          }
      , isEffect: opts.isEffect
      , heapWords: opts.heapWords
      }
  -- One module's own contribution (ADR-0104 §5-1). The `deps` projection is unused since the
  -- `DictElim` bridge's removal (ADR-0104 §3) took the machinery derivation with it — the seam still
  -- passes it (the contract allows a backend to derive its contribution under its import closure).
  , moduleContext: \_deps cm ->
      let
        gdefs0 = map classifyDecl cm.module.decls
        -- synthesised intrinsic-foreign keys are top-level globals too (referenced cross-module by `$root`).
        synthKeys = Array.concatMap gdefKeys (synthForeignGdefs cm)
        gkeys = Set.fromFoldable (Array.concatMap gdefKeys gdefs0 <> synthKeys)
        surface = deriveSurface cm.source gdefs0
        xfns = foldl (surfaceFn surface) Map.empty gdefs0
      in
        -- `foreignArity` is a per-module base — `lowerModule`/`lowerEntry` override it with the module's
        -- own native-leaf arities (`nativeLeafArities lm.foreignSigs`), threaded from FSR (ADR-0090).
        { cxOpts: { gkeys, xfns, foreignArity: Map.empty, inlineAbi: not opts.debug }
        , isEffect: opts.isEffect
        , heapWords: opts.heapWords
        }
  -- The `.pmi` surface (export kinds/arities) is unchanged by the required lowering (it rewrites
  -- bodies, not top-level keys/arities), so the interface is taken over the raw module.
  , interfaceOf: \_ lm -> interfaceOfAnf lm.source (map classifyDecl lm.module.decls)
  , lowerModule: \ctx lm ->
      let
        leaves = nativeLeafArities lm.foreignSigs
        -- Foreign gdefs first (boot's `foreign_groups`, key-sorted by `synthForeignGdefs`), then the
        -- module's own decls — the per-module view of boot's `foreign_groups @ module_reached`.
        gdefs = synthForeignGdefs lm <> map classifyDecl (lowerModuleDecls leaves lm.module).decls
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
        allDecls = Array.concatMap (\lm -> map classifyDecl (lowerModuleDecls (nativeLeafArities lm.foreignSigs) lm.module).decls) input.modules
      in
        entryLl (ctx.cxOpts { foreignArity = allLeaves }) ctx.isEffect ctx.heapWords
          (allForeigns <> allDecls)
          (nativeRequiredLowering allLeaves input.entry)
  }
