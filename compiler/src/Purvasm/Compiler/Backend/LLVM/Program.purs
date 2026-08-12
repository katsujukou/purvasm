-- | The ANF → `.ll` emitters and `Gdef` machinery the native backend's per-module (B2) driver
-- | (`Backend.LLVM.Driver`) assembles: classify a binding into a `Gdef`, emit each as a root-handle
-- | global + `$init` (plus lifted code via `emitPending`), and build a module object (`moduleLl`) or the
-- | init/entry object (`entryLl`). A faithful transcription of boot's `codegen_llvm.ml`
-- | (`gdef_keys`/`classify_nonrec`/`reachable_gdefs`/`store_root_global`/`emit_init_fn`/`emit_gdef`/
-- | `emit_gdefs`/`emit_init_all`/`emit_entry_stub`/`module_ll`/`entry_ll`) — the ADR-0082 port; its
-- | boot byte-identity gate is retired (ADR-0104 §4) and emission is now L2-owned.
-- |
-- | All three `Gdef` classes are emitted: `Gfun`/`Gcaf` (self-hoisted closure / strict CAF) and `Grec`
-- | (a recursive group built by-need over a shared env via `Emit.buildGrec`, ADR-0070 §4).
module Purvasm.Compiler.Backend.LLVM.Program
  ( gdefKeys
  , gdefInitKey
  , classifyNonrec
  , classifyDecl
  , reachableGdefs
  , moduleLl
  , moduleLlWithEvents
  , entryLl
  , entryLlWithEvents
  , surfaceFn
  , foreignKeysOf
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Monad.State.Class (gets, modify_)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.CallClass (CallEvent, profileSlotNames)
import Purvasm.Compiler.Backend.LLVM.Abi (abiStamp, ctxHeaderVersion, declarations, forceValue, profileDeclarations)
import Purvasm.Compiler.Backend.LLVM.Emit (buildGrec, emitGcafInit, emitPending, expr, readVar)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (immUnit, mangle, mangleForeign)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, FnBody, MakeCxOptions, beginFn, emit, emitModule, emitStringConstant, forA, forA_, fresh, makeCx, renderChunks, renderFnBody, runCodegen, takeFn, unsafeEmitRawCall, unsafeEmitRawModule)
import Purvasm.Compiler.Backend.LLVM.Root (emitGfunInit, emitInitFnFramed, entryTeardown, openFrame)
import Purvasm.Compiler.Backend.LLVM.Safepoint (RtArg(..), RtOp(..), rtCall, rtCallVoid)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), EnvSrc(..), FnInfo, Gdef(..), Lifted(..), LiftedBody(..), SplitOutput)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)

-- | The root-handle-global keys a gdef defines.
gdefKeys :: Gdef -> Array String
gdefKeys = case _ of
  Gfun k _ _ -> [ k ]
  Gcaf k _ -> [ k ]
  Grec ms -> map fst ms

-- | The init symbol a gdef exposes for `pv_init_all` (a group is named after its first member).
gdefInitKey :: Gdef -> String
gdefInitKey = case _ of
  Gfun k _ _ -> k
  Gcaf k _ -> k
  -- a `case`, not `maybe (unsafeCrashWith …)`: PureScript is strict, so the crash operand would fire
  -- eagerly on every (non-empty) group.
  Grec ms -> case Array.head ms of
    Just (Tuple k _) -> k
    Nothing -> unsafeCrashWith "Program.gdefInitKey: empty Grec"

-- | Classify a non-recursive binding: a `Ret (CLam …)` is a function, anything else a strict CAF.
classifyNonrec :: String -> Expr -> Gdef
classifyNonrec key = case _ of
  Ret (CLam ps b) -> Gfun key ps b
  e -> Gcaf key e

-- | Classify a post-optimiser `Decl` into a `Gdef` (ADR-0086 §4: `Gdef` classification is a backend
-- | concern that runs **after** the seam). Group-atomic: a recursive `Decl` is one `Grec`; a
-- | non-recursive `Decl` (exactly one member, from a single CoreFn `NonRec` bind) is a `Gfun`/`Gcaf` via
-- | `classifyNonrec`.
classifyDecl :: Decl -> Gdef
classifyDecl d
  | d.recursive = Grec d.members
  | otherwise = case Array.head d.members of
      Just (Tuple key body) -> classifyNonrec key body
      Nothing -> unsafeCrashWith "Program.classifyDecl: empty non-recursive Decl"

-- | The top-level keys a gdef references (in its bodies), for reachability: `fv ∩ gkeys`, minus its own
-- | keys (params/locals are bare and never in `gkeys`).
gdefRefs :: Set String -> Gdef -> Set String
gdefRefs gkeys g =
  let
    bodies = case g of
      Gfun _ _ b -> [ b ]
      Gcaf _ b -> [ b ]
      Grec ms -> map snd ms
    fvs = foldl (\a b -> Set.union a (fvExpr Set.empty b)) Set.empty bodies
  in
    Set.difference (Set.intersection fvs gkeys) (Set.fromFoldable (gdefKeys g))

-- | The gdefs reachable from the entry (ADR-0072 §3), in the original spine order — the transitive
-- | closure of the entry's global references. `pv_init_all` initialises only these. Stack-safe: a
-- | `tailRec` work-list over keys, reached tracked by init-key.
reachableGdefs :: Set String -> Expr -> Array Gdef -> Array Gdef
reachableGdefs gkeys entry gdefs =
  let
    byKey :: Map String Gdef
    byKey = foldl (\m g -> foldl (\m2 k -> Map.insert k g m2) m (gdefKeys g)) Map.empty gdefs

    seed = Array.fromFoldable (Set.intersection (fvExpr Set.empty entry) gkeys)

    reached = tailRec step { reached: Set.empty, worklist: seed }

    step st = case Array.uncons st.worklist of
      Nothing -> Done st.reached
      Just { head: k, tail } -> case Map.lookup k byKey of
        Nothing -> Loop st { worklist = tail }
        Just g ->
          let
            ik = gdefInitKey g
          in
            if Set.member ik st.reached then Loop st { worklist = tail }
            else Loop
              { reached: Set.insert ik st.reached
              , worklist: tail <> Array.fromFoldable (gdefRefs gkeys g)
              }
  in
    Array.filter (\g -> Set.member (gdefInitKey g) reached) gdefs

-- --- emission ----------------------------------------------------------------------------------------

-- | Emit one gdef's `$init` function (and, for a `Gfun`, queue its code). `Gfun`: a closed top-level
-- | closure; its `$d` is exported iff the key is in the cross-module surface (`xfns`). `Gcaf`: a strict
-- | value under a transient frame. Each body RETURNS its `(globalKey, value)` pairs and the
-- | `Root` init wrapper plants the permanent roots after the body (and, for the framed shapes,
-- | after the transient frame is popped) — the ADR-0105 §2 phase order is wrapper-owned, so a
-- | permanent handle inside the transient frame is not expressible here.
emitGdef :: Gdef -> Codegen Unit
emitGdef = case _ of
  Gfun key ps body -> do
    xfns <- gets _.xfns
    let
      lifted = Lifted
        { name: mangle key
        , params: ps
        , captures: []
        , body: LBody body
        , selfName: Nothing
        , captureFns: []
        , exported: Map.member key xfns
        }
    modify_ \c -> c { pending = lifted : c.pending }
    -- the frameless init is a fixed shape owned by Root — no body callback exists to misuse.
    emitGfunInit key (Array.length ps)
  -- ADR-0106 slice 2: the Gcaf init is plan-driven behind the fixed-shape surface — the
  -- activation plan decides the frame; non-crossing definitions keep their direct tokens.
  Gcaf key e -> emitGcafInit key e
  Grec binds -> case Array.head binds of
    Nothing -> unsafeCrashWith "Program.emitGdef: empty Grec"
    Just (Tuple firstKey _) ->
      emitInitFnFramed firstKey \tok -> do
        -- stable member code symbols, so `gfns`'s pre-registered `$d` names line up.
        env' <- buildGrec (Just tok) (\m -> Just (mangle m)) Nil binds
        -- read each member's current cell value while its transient root is still live; the
        -- wrapper pops the frame and permanent-roots the returned raw values.
        forA binds (\(Tuple m _) -> Tuple m <$> readVar env' m)

-- | Register the unit's own function bindings for direct calls, emit each gdef's root-handle global
-- | definition(s) (init overwrites the 0 sentinel before any read), then its init function.
emitGdefs :: Array Gdef -> Codegen Unit
emitGdefs gdefs = do
  modify_ \c -> c { gfns = foldl registerGfn c.gfns gdefs }
  forA_ gdefs emitRootGlobals
  when (not (Array.null gdefs)) (emitModule "\n")
  forA_ gdefs emitGdef
  where
  registerGfn m = case _ of
    Gfun k ps _ -> Map.insert k { dsym: mangle k <> "$d", arity: Array.length ps, src: SSentinel } m
    Grec ms -> foldl
      ( \m2 (Tuple mkey rhs) -> case rhs of
          Ret (CLam ps _) -> Map.insert mkey { dsym: mangle mkey <> "$d", arity: Array.length ps, src: SForceCell } m2
          _ -> m2
      )
      m
      ms
    Gcaf _ _ -> m

  emitRootGlobals g =
    forA_ (gdefKeys g) (\k -> emitModule ("@" <> mangle k <> "$root = global i64 0\n"))

-- | Emit `pv_init_all`: call each binding's init in dependency (spine) order. Callers pass the reachable
-- | subset. The per-gdef `@…$init` call lines assembled here are module-SKELETON text (a chunk,
-- | not recipe emission) — the one call-carrying chunk allowed through `unsafeEmitRawModule`,
-- | pinned by file/shape/count in `tools/seam-audit.sh` ($init symbols are the program's own,
-- | always-safepoint init tier; the entry's own `pv_init_all` call goes through `RtInitAll`).
emitInitAll :: Array Gdef -> Codegen Unit
emitInitAll gdefs =
  let
    calls = joinWith "\n" (map (\g -> "  call void @" <> mangle (gdefInitKey g) <> "$init(ptr %ctx)") gdefs)
  in
    unsafeEmitRawModule ("define void @pv_init_all(ptr %ctx) {\nentry:\n" <> calls <> "\n  ret void\n}\n\n")

-- | Emit the `@main` entry stub body: `pv_runtime_new` → `pv_abi_check` → `pv_init_all` → evaluate the
-- | entry → print (pure) or run+drain (effect) → free → return. Returns the body text.
-- | ADR-0108 §3: register the profile's slot names with the runtime (instrumented builds only).
-- |
-- | The blob is the slot names joined by `\n`, in slot order; the runtime sizes its counter array
-- | from the count and labels each counter from the blob. Nothing is emitted in a normal build, so
-- | the shipped entry object is byte-identical.
emitApplyProfileRegister :: Codegen Unit
emitApplyProfileRegister = do
  profiling <- gets _.profileApply
  when profiling do
    let names = joinWith "\n" profileSlotNames
    emitStringConstant names >>= case _ of
      Nothing -> unsafeCrashWith "Program.emitApplyProfileRegister: empty slot-name blob"
      Just c -> rtCallVoid RtApplyProfileRegister
        [ Ptr c.name, I64 (show c.len), I64 (show (Array.length profileSlotNames)) ]

emitEntryStub :: Boolean -> Int -> Expr -> Codegen FnBody
emitEntryStub isEffect heapWords entry = do
  beginFn
  -- ctx birth: the one call the classified seam cannot render (it produces the `%ctx` every seam
  -- call takes, and returns `ptr`); everything after it routes through the seam. The raw-call
  -- emitter is allowlisted for exactly this line in `tools/seam-audit.sh`.
  unsafeEmitRawCall ("  %ctx = call ptr @pv_runtime_new(i64 " <> show heapWords <> ")")
  rtCallVoid RtAbiCheck [ I32 (show ctxHeaderVersion) ]
  -- ADR-0108 §3, instrumented builds only: hand the runtime THIS compiler's slot names, so the
  -- profile's schema is labelled from the one definition of the layout (`CallClass.profileSlotNames`)
  -- rather than from a copy in the runtime that could drift from it. Registration precedes
  -- `pv_init_all` because a CAF init can already dispatch.
  emitApplyProfileRegister
  rtCallVoid RtInitAll []
  -- the entry's roots are transient under its own frame; the permanent tier exists only
  -- inside `Root`'s init wrappers (`emitGfunInit` / `emitInitFnFramed`), so the entry stub
  -- has no expression for it (ADR-0105 §2: the entry stub has NO permanent-root capability).
  tok <- openFrame
  mv <- expr (Just tok) Nil false entry
  v <- case mv of
    Just v -> pure v
    Nothing -> unsafeCrashWith "Program.emitEntryStub: entry produced no value"
  if isEffect then do
    void (rtCall RtRunEffect [ V v ])
    rtCallVoid RtDrainOutput []
  else do
    fv <- forceValue v
    rtCallVoid RtPrintInt [ V fv ]
  entryTeardown tok
  takeFn

-- --- module / entry object decls -------------------------------------------------------------------

-- | `external` decls for referenced globals not defined in this object.
externGlobalDecls :: Set String -> Codegen String
externGlobalDecls defined = do
  externs <- gets _.externs
  pure $ joinWith "\n"
    (map (\k -> "@" <> mangle k <> "$root = external global i64") (Array.fromFoldable (Set.difference externs defined)))

-- | `declare` decls for the native foreign `AbiCodeFn` symbols this object references (ADR-0073 §3).
foreignDecls :: Codegen String
foreignDecls = do
  foreigns <- gets _.foreigns
  pure $ joinWith "\n"
    (map (\k -> "declare i64 @" <> mangleForeign k <> "(ptr, i64, ptr, i64)") (Array.fromFoldable foreigns))

-- | `declare tailcc` decls for the cross-module direct entries this object calls (ADR-0077 §3).
xfnDecls :: Codegen String
xfnDecls = do
  xdecls <- gets _.xdecls
  pure $ joinWith "\n"
    ( map (\(Tuple dsym arity) -> "declare tailcc i64 @" <> dsym <> "(ptr, i64" <> joinWith "" (Array.replicate arity ", i64") <> ")")
        (Map.toUnfoldable xdecls)
    )

-- | Emit one module object's `.ll`: its own root-handle globals + inits + internal code, plus `external`
-- | decls for other modules' globals and cross-module `$d` externs. No `pv_init_all`/`@main`.
moduleLl :: MakeCxOptions -> Set String -> Array Gdef -> String
moduleLl opts defined gdefs = (moduleLlWithEvents opts defined gdefs).ir

-- | `moduleLl`, also returning the object's ADR-0108 §1 call events — the SAME emission, so the
-- | events describe the `.ll` beside them rather than a re-derivation of it. The census reads this;
-- | the backend reads `moduleLl` and never sees the events.
moduleLlWithEvents :: MakeCxOptions -> Set String -> Array Gdef -> { ir :: String, events :: Array CallEvent }
moduleLlWithEvents opts defined gdefs =
  let
    -- The optimiser's Specialize pass (ADR-0089) materialises caller-homed `$spec$` clones as new
    -- module-local top-level gdefs *during* optimisation, so they are absent from the pre-optimisation
    -- whole-program `gkeys` derived in `context`. Fold this module's own post-optimiser gdef keys
    -- (`defined`, which includes those clones) into `gkeys` so `readVar` resolves a clone reference to a
    -- local `$root` load instead of crashing as unbound. `externGlobalDecls defined` still subtracts
    -- `defined`, so a locally-defined clone is never wrongly declared `external`; under `--no-opt` there are
    -- no clones and `defined ⊆ opts.gkeys`, so the union is a no-op (the reference lowering's emission is
    -- unchanged).
    -- ADR-0108 §1: `defined` is also the object's OWNERSHIP input — it is what lets `directTarget`
    -- tell a key this object defines but does not export as a function (a `Gcaf`) from a dependency
    -- with no published direct-call fact.
    Tuple parts ctx = runCodegen (makeCx (opts { gkeys = Set.union opts.gkeys defined, defined = defined })) do
      emitGdefs gdefs
      emitPending
      extern <- externGlobalDecls defined
      foreign_ <- foreignDecls
      xfn <- xfnDecls
      pure { extern, foreign_, xfn }
  in
    { ir:
        "; ModuleID = 'purvasm.module'\n\n"
          <> declarations
          <> profileDeclarations opts.profileApply
          <> "\n"
          <> abiStamp opts.inlineAbi
          <> "\n"
          <> parts.extern
          <> "\n"
          <> parts.foreign_
          <> "\n"
          <> parts.xfn
          <> "\n\n"
          <> renderChunks ctx.globals
          <> "\n"
          <> renderChunks ctx.md
    , events: Array.reverse (Array.fromFoldable ctx.callEvents)
    }

-- | Emit the init/entry object: `pv_init_all` (reachable inits in dependency order) + the `@main` stub.
entryLl :: MakeCxOptions -> Boolean -> Int -> Array Gdef -> Expr -> String
entryLl opts isEffect heapWords gdefs entry = (entryLlWithEvents opts isEffect heapWords gdefs entry).ir

-- | `entryLl`, also returning its ADR-0108 §1 call events (see `moduleLlWithEvents`).
entryLlWithEvents :: MakeCxOptions -> Boolean -> Int -> Array Gdef -> Expr -> { ir :: String, events :: Array CallEvent }
entryLlWithEvents opts isEffect heapWords gdefs entry =
  let
    -- Fold every emitted gdef's key (incl. optimiser `$spec$` clones, see `moduleLl`) into `gkeys` so both
    -- reachability (`pv_init_all` must call a referenced clone's `$init`, else its `$root` stays the `0`
    -- sentinel) and `readVar` see them. A no-op under `--no-opt` (no clones; keys ⊆ `opts.gkeys`).
    gkeys' = Set.union opts.gkeys (Set.fromFoldable (Array.concatMap gdefKeys gdefs))
    -- ADR-0108 §1: the entry object DEFINES NOTHING — it declares and calls the reachable `$init`s
    -- (`externGlobalDecls Set.empty` below: everything it references is `external`). So its
    -- ownership set is empty, and every global callee here classifies as a dependency.
    opts' = opts { gkeys = gkeys', defined = Set.empty }
    reach = reachableGdefs gkeys' entry gdefs
    Tuple parts ctx = runCodegen (makeCx opts') do
      emitInitAll reach
      entryBody <- emitEntryStub isEffect heapWords entry
      emitPending
      extern <- externGlobalDecls Set.empty
      foreign_ <- foreignDecls
      xfn <- xfnDecls
      pure { entryBody, extern, foreign_, xfn }
    initDecls = joinWith "\n" (map (\g -> "declare void @" <> mangle (gdefInitKey g) <> "$init(ptr)") reach)
  in
    { ir:
        "; ModuleID = 'purvasm.init'\n\n"
          <> declarations
          <> profileDeclarations opts.profileApply
          <> "\n"
          <> abiStamp opts.inlineAbi
          <> "\n"
          <> initDecls
          <> "\n"
          <> parts.extern
          <> "\n"
          <> parts.foreign_
          <> "\n"
          <> parts.xfn
          <> "\n\n"
          <> renderChunks ctx.globals
          <> "\n"
          <> renderChunks ctx.md
          <> "\ndefine i32 @main() {\nentry:\n"
          <> renderFnBody parts.entryBody
          <> "}\n"
    , events: Array.reverse (Array.fromFoldable ctx.callEvents)
    }

-- | Build the cross-module export surface (`xfns`) from the published call facts (`surface`): an exported
-- | top-level function whose natively-lowered shape agrees with the `.pmi` fact becomes direct-callable.
surfaceFn :: Map String CallFact -> Map String FnInfo -> Gdef -> Map String FnInfo
surfaceFn surface acc = case _ of
  Gfun k ps _ -> case Map.lookup k surface of
    Just (Cfn n) | n == Array.length ps -> Map.insert k { dsym: mangle k <> "$d", arity: n, src: SSentinel } acc
    _ -> acc
  Gcaf _ _ -> acc
  Grec ms -> foldl
    ( \a (Tuple m rhs) -> case rhs, Map.lookup m surface of
        Ret (CLam ps _), Just (Crecfn n) | n == Array.length ps -> Map.insert m { dsym: mangle m <> "$d", arity: n, src: SForceCell } a
        _, _ -> a
    )
    acc
    ms

-- | The native foreign keys a `SplitOutput` references, as a sorted list.
foreignKeysOf :: SplitOutput -> Array String
foreignKeysOf o = Array.fromFoldable o.foreigns
