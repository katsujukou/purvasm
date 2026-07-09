-- | The ANF → `.ll` emitters and `Gdef` machinery the native backend's per-module (B2) driver
-- | (`Backend.LLVM.Driver`) assembles: classify a binding into a `Gdef`, emit each as a root-handle
-- | global + `$init` (plus lifted code via `emitPending`), and build a module object (`moduleLl`) or the
-- | init/entry object (`entryLl`). A faithful transcription of boot's `codegen_llvm.ml`
-- | (`gdef_keys`/`classify_nonrec`/`reachable_gdefs`/`store_root_global`/`emit_init_fn`/`emit_gdef`/
-- | `emit_gdefs`/`emit_init_all`/`emit_entry_stub`/`module_ll`/`entry_ll`), byte-identical to boot's
-- | `.ll` (ADR-0082 §2).
-- |
-- | Slice-1a cut: `Gfun`/`Gcaf` and the entry stub; `Grec` (recursive groups) is a later slice and
-- | crashes with a labelled `unsafeCrashWith`.
module Purvasm.Compiler.Backend.LLVM.Program
  ( gdefKeys
  , gdefInitKey
  , classifyNonrec
  , classifyDecl
  , reachableGdefs
  , moduleLl
  , entryLl
  , surfaceFn
  , foreignKeysOf
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Monad.State.Class (gets, modify_)
import Data.Array as Array
import Data.Foldable (foldl, traverse_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (abiFrameOpen, abiPopFrame, abiRoot, abiStamp, ctxHeaderVersion, declarations, forceValue)
import Purvasm.Compiler.Backend.LLVM.Emit (emitPending, expr)
import Purvasm.Compiler.Backend.LLVM.FreeVars (fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (immUnit, mangle, mangleForeign)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, MakeCxOptions, beginFn, emit, emitModule, fresh, makeCx, renderChunks, runCodegen, takeFn)
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
  Grec ms -> maybe (unsafeCrashWith "Program.gdefInitKey: empty Grec") fst (Array.head ms)

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

-- | Root a value into `@<mangle key>$root` and store the handle (the caller has popped any transient
-- | frame, so the root lands in the never-popped init region).
storeRootGlobal :: String -> String -> Codegen Unit
storeRootGlobal key v = do
  h <- abiRoot v
  emit ("  store i64 " <> h <> ", ptr @" <> mangle key <> "$root")

-- | Wrap an init-body emitter in `define void @<mangle name>$init(ptr %ctx)`.
emitInitFn :: String -> Codegen Unit -> Codegen Unit
emitInitFn name body = do
  beginFn
  body
  emit "  ret void"
  text <- takeFn
  emitModule ("define void @" <> mangle name <> "$init(ptr %ctx) {\nentry:\n" <> text <> "}\n\n")

-- | Emit one gdef's `$init` function (and, for a `Gfun`, queue its code). `Gfun`: a closed top-level
-- | closure; its `$d` is exported iff the key is in the cross-module surface (`xfns`). `Gcaf`: a strict
-- | value under a transient frame.
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
    emitInitFn key do
      addr <- fresh
      emit ("  " <> addr <> " = ptrtoint ptr @" <> mangle key <> " to i64")
      clo <- fresh
      emit ("  " <> clo <> " = call i64 @pv_make_closure(ptr %ctx, i64 " <> addr <> ", i32 " <> show (Array.length ps) <> ", i64 " <> immUnit <> ")")
      storeRootGlobal key clo
  Gcaf key e ->
    emitInitFn key do
      frame <- abiFrameOpen
      modify_ \c -> c { frame = frame }
      mv <- expr Nil false e
      case mv of
        Just v -> do
          abiPopFrame frame
          storeRootGlobal key v
        Nothing -> unsafeCrashWith "Program.emitGdef: Gcaf body produced no value"
  Grec _ ->
    unsafeCrashWith "Program.emitGdef: Grec (recursive group) not yet supported (slice 2)"

-- | Register the unit's own function bindings for direct calls, emit each gdef's root-handle global
-- | definition(s) (init overwrites the 0 sentinel before any read), then its init function.
emitGdefs :: Array Gdef -> Codegen Unit
emitGdefs gdefs = do
  modify_ \c -> c { gfns = foldl registerGfn c.gfns gdefs }
  traverse_ emitRootGlobals gdefs
  when (not (Array.null gdefs)) (emitModule "\n")
  traverse_ emitGdef gdefs
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
    traverse_ (\k -> emitModule ("@" <> mangle k <> "$root = global i64 0\n")) (gdefKeys g)

-- | Emit `pv_init_all`: call each binding's init in dependency (spine) order. Callers pass the reachable
-- | subset.
emitInitAll :: Array Gdef -> Codegen Unit
emitInitAll gdefs =
  let
    calls = joinWith "\n" (map (\g -> "  call void @" <> mangle (gdefInitKey g) <> "$init(ptr %ctx)") gdefs)
  in
    emitModule ("define void @pv_init_all(ptr %ctx) {\nentry:\n" <> calls <> "\n  ret void\n}\n\n")

-- | Emit the `@main` entry stub body: `pv_runtime_new` → `pv_abi_check` → `pv_init_all` → evaluate the
-- | entry → print (pure) or run+drain (effect) → free → return. Returns the body text.
emitEntryStub :: Boolean -> Int -> Expr -> Codegen String
emitEntryStub isEffect heapWords entry = do
  beginFn
  emit ("  %ctx = call ptr @pv_runtime_new(i64 " <> show heapWords <> ")")
  emit ("  call void @pv_abi_check(i32 " <> show ctxHeaderVersion <> ")")
  emit "  call void @pv_init_all(ptr %ctx)"
  frame0 <- abiFrameOpen
  modify_ \c -> c { frame = frame0 }
  mv <- expr Nil false entry
  v <- case mv of
    Just v -> pure v
    Nothing -> unsafeCrashWith "Program.emitEntryStub: entry produced no value"
  if isEffect then do
    r <- fresh
    emit ("  " <> r <> " = call i64 @pv_run_effect(ptr %ctx, i64 " <> v <> ")")
    emit "  call void @pv_drain_output(ptr %ctx)"
  else do
    fv <- forceValue v
    emit ("  call void @pv_print_int(i64 " <> fv <> ")")
  abiPopFrame frame0
  emit "  call void @pv_runtime_free(ptr %ctx)"
  emit "  ret i32 0"
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
moduleLl opts defined gdefs =
  let
    Tuple parts ctx = runCodegen (makeCx opts) do
      emitGdefs gdefs
      emitPending
      extern <- externGlobalDecls defined
      foreign_ <- foreignDecls
      xfn <- xfnDecls
      pure { extern, foreign_, xfn }
  in
    "; ModuleID = 'purvasm.module'\n\n"
      <> declarations
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

-- | Emit the init/entry object: `pv_init_all` (reachable inits in dependency order) + the `@main` stub.
entryLl :: MakeCxOptions -> Boolean -> Int -> Array Gdef -> Expr -> String
entryLl opts isEffect heapWords gdefs entry =
  let
    reach = reachableGdefs opts.gkeys entry gdefs
    Tuple parts ctx = runCodegen (makeCx opts) do
      emitInitAll reach
      entryBody <- emitEntryStub isEffect heapWords entry
      emitPending
      extern <- externGlobalDecls Set.empty
      foreign_ <- foreignDecls
      xfn <- xfnDecls
      pure { entryBody, extern, foreign_, xfn }
    initDecls = joinWith "\n" (map (\g -> "declare void @" <> mangle (gdefInitKey g) <> "$init(ptr)") reach)
  in
    "; ModuleID = 'purvasm.init'\n\n"
      <> declarations
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
      <> parts.entryBody
      <> "}\n"

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
