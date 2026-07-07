-- | Whole-program assembly (ADR-0072 §1/§3): classify the linked spine into `Gdef`s, emit each as a
-- | root-handle global + `$init`, drive the lifted code via `emitPending`, and assemble the per-module
-- | objects + the init/entry object. A faithful transcription of boot's `codegen_llvm.ml`
-- | (`gdef_keys`/`classify_nonrec`/`uniquify_toplevel`/`split_spine`/`reachable_gdefs`/`store_root_global`/
-- | `emit_init_fn`/`emit_gdef`/`emit_gdefs`/`emit_init_all`/`emit_entry_stub`/`module_ll`/`entry_ll`/
-- | `program_split`), byte-identical to boot's `.ll` (ADR-0082 §2).
-- |
-- | Slice-1a cut: `Gfun`/`Gcaf` and the entry stub; `Grec` (recursive groups) and the cross-module
-- | export surface are later slices and crash with a labelled `unsafeCrashWith`.
module Purvasm.Compiler.Backend.LLVM.Program
  ( gdefKeys
  , gdefInitKey
  , classifyNonrec
  , uniquifyToplevel
  , splitSpine
  , reachableGdefs
  , moduleOfKey
  , moduleLl
  , entryLl
  , surfaceFn
  , programSplit
  , foreignKeysOf
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Monad.State.Class (gets, modify_)
import Data.Array as Array
import Data.Foldable (foldl, foldr, traverse_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (abiFrameOpen, abiPopFrame, abiRoot, abiStamp, ctxHeaderVersion, declarations, forceValue)
import Purvasm.Compiler.Backend.LLVM.Emit (emitPending, expr)
import Purvasm.Compiler.Backend.LLVM.FreeVars (binderVars, cfExpr, fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (immUnit, mangle, mangleForeign)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, MakeCxOptions, beginFn, emit, emitModule, fresh, makeCx, renderChunks, runCodegen, takeFn)
import Purvasm.Compiler.Backend.LLVM.Types (CallFact(..), EnvSrc(..), FnInfo, Gdef(..), Lifted(..), LiftedBody(..), SplitOutput)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))

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

-- --- top-level key uniquification (ADR-0072 §2) ------------------------------------------------------

-- | Capture-avoiding rename of *free* variable occurrences per `ren` (a name → replacement map). A local
-- | binder shadowing a renamed name removes it from `ren` within that scope.
renAtom :: Map String String -> Atom -> Atom
renAtom ren = case _ of
  AtomVar x -> case Map.lookup x ren of
    Just x' -> AtomVar x'
    Nothing -> AtomVar x
  a -> a

renExpr :: Map String String -> Expr -> Expr
renExpr ren = case _ of
  Ret c -> Ret (renCexpr ren c)
  Let x c body -> Let x (renCexpr ren c) (renExpr (Map.delete x ren) body)
  LetRec binds body ->
    let
      ren' = foldl (\r b -> Map.delete b.var r) ren binds
    in
      LetRec (map (\b -> { var: b.var, rhs: renExpr ren' b.rhs }) binds) (renExpr ren' body)

renCexpr :: Map String String -> CExpr -> CExpr
renCexpr ren = case _ of
  CAtom a -> CAtom (renAtom ren a)
  CLam ps b -> CLam ps (renExpr (foldl (\r p -> Map.delete p r) ren ps) b)
  CApp f args -> CApp (renAtom ren f) (map (renAtom ren) args)
  CPrim op args -> CPrim op (map (renAtom ren) args)
  CArray args -> CArray (map (renAtom ren) args)
  CCtor n ar args -> CCtor n ar (map (renAtom ren) args)
  CRecord fs -> CRecord (map (\f -> { prop: f.prop, val: renAtom ren f.val }) fs)
  CAccessor a l -> CAccessor (renAtom ren a) l
  CUpdate a fs -> CUpdate (renAtom ren a) (map (\f -> { prop: f.prop, val: renAtom ren f.val }) fs)
  CIf a t e -> CIf (renAtom ren a) (renExpr ren t) (renExpr ren e)
  CCase scruts alts -> CCase (map (renAtom ren) scruts) (map (renAltRhs ren) alts)
  where
  renAltRhs r alt =
    let
      bvs = foldl (\a b -> Set.union a (binderVars b)) Set.empty alt.binders
      r' = foldr Map.delete r bvs
    in
      { binders: alt.binders
      , result: case alt.result of
          Uncond e -> Uncond (renExpr r' e)
          Guarded gs -> Guarded (map (\g -> { guard: renExpr r' g.guard, rhs: renExpr r' g.rhs }) gs)
      }

-- | Make every top-level binding key unique (ADR-0072 §2): walk the spine and, on a re-bound key,
-- | rename the shadowing (later) binding and capture-avoidingly rewrite its references in scope. A no-op
-- | when no key repeats. The spine walk is `tailRec` (stack-safe over a deep `Let` chain); the shared
-- | counter is threaded explicitly.
uniquifyToplevel :: Expr -> Expr
uniquifyToplevel e0 = res.expr
  where
  res = tailRec step { ren: Map.empty, seen: Set.empty, counter: 0, acc: Nil, e: e0 }

  freshKey base counter = base <> "$sh" <> show (counter + 1)

  -- `acc` accumulates the rebuilt spine prefix (reversed); on the terminal node we rewrite it and
  -- rebuild the whole expression from the prefix.
  step st = case st.e of
    Let k c rest ->
      let
        c' = renCexpr st.ren c
      in
        if Set.member k st.seen then
          let
            k' = freshKey k st.counter
          in
            Loop st
              { ren = Map.insert k k' st.ren
              , counter = st.counter + 1
              , acc = SLet k' c' : st.acc
              , e = rest
              }
        else
          Loop st { seen = Set.insert k st.seen, acc = SLet k c' : st.acc, e = rest }
    LetRec binds rest ->
      let
        folded = foldl
          ( \{ seen, ren, counter, renamed } b ->
              if Set.member b.var seen then
                { seen
                , ren: Map.insert b.var (freshKey b.var counter) ren
                , counter: counter + 1
                , renamed: { var: freshKey b.var counter, rhs: b.rhs } : renamed
                }
              else
                { seen: Set.insert b.var seen
                , ren
                , counter
                , renamed: { var: b.var, rhs: b.rhs } : renamed
                }
          )
          { seen: st.seen, ren: st.ren, counter: st.counter, renamed: Nil }
          binds
        binds' = Array.reverse (Array.fromFoldable (map (\b -> { var: b.var, rhs: renExpr folded.ren b.rhs }) folded.renamed))
      in
        Loop st
          { seen = folded.seen
          , ren = folded.ren
          , counter = folded.counter
          , acc = SRec binds' : st.acc
          , e = rest
          }
    main -> Done { expr: rebuild st.acc (renExpr st.ren main) }

  rebuild acc tailExpr = foldl (\body node -> wrap node body) tailExpr acc
  wrap node body = case node of
    SLet k c -> Let k c body
    SRec binds -> LetRec binds body

-- A rebuilt spine node (the reversed prefix `uniquifyToplevel` accumulates).
data SpineNode
  = SLet String CExpr
  | SRec (Array { var :: String, rhs :: Expr })

-- | Split a linked top-level spine into its global definitions (in spine = dependency order) and the
-- | entry expression (the first non-binding node). `tailRec` keeps a deep spine stack-safe.
splitSpine :: Expr -> Tuple (Array Gdef) Expr
splitSpine e0 = tailRec step (Tuple Nil e0)
  where
  step (Tuple acc e) = case e of
    Let k c rest -> Loop (Tuple (classifyNonrec k (Ret c) : acc) rest)
    LetRec binds rest -> Loop (Tuple (Grec (map (\b -> Tuple b.var b.rhs) binds) : acc) rest)
    main -> Done (Tuple (Array.reverse (Array.fromFoldable acc)) main)

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

-- | The module a qualified key belongs to: everything before its last `.` (a key without a `.` is its
-- | own module).
moduleOfKey :: String -> String
moduleOfKey k = case String.lastIndexOf (Pattern ".") k of
  Just i -> String.take i k
  Nothing -> k

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

-- | Options `programSplit` takes (mirrors boot's optional args).
type ProgramSplitOptions =
  { isEffect :: Boolean
  , heapWords :: Int
  , surface :: Map String CallFact
  , debug :: Boolean
  }

-- | Lower a linked program to separate module objects + one init/entry object (ADR-0072 §1/§3). Top-level
-- | bindings are partitioned by owning module, preserving first-appearance module order and per-module
-- | spine order.
programSplit :: ProgramSplitOptions -> Expr -> SplitOutput
programSplit opts e =
  let
    inlineAbi = not opts.debug
    Tuple gdefs entry = splitSpine (uniquifyToplevel e)
    gkeys = Set.fromFoldable (Array.concatMap gdefKeys gdefs)
    xfns = foldl (surfaceFn opts.surface) Map.empty gdefs
    cxOpts = { gkeys, xfns, inlineAbi }
    parts = partitionByModule gdefs
    modules = map
      ( \(Tuple m gs) ->
          Tuple m (moduleLl cxOpts (Set.fromFoldable (Array.concatMap gdefKeys gs)) gs)
      )
      parts
  in
    { modules
    , entry: entryLl cxOpts opts.isEffect opts.heapWords gdefs entry
    , foreigns: cfExpr e
    }

-- Build the cross-module export surface (`xfns`) from the published call facts (`surface`): an exported
-- top-level function whose natively-lowered shape agrees with the `.pmi` fact becomes direct-callable.
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

-- Partition gdefs by owning module, preserving first-appearance module order and per-module spine order.
partitionByModule :: Array Gdef -> Array (Tuple String (Array Gdef))
partitionByModule gdefs =
  let
    result = foldl
      ( \st g ->
          let
            m = moduleOfKey (gdefInitKey g)
          in
            case Map.lookup m st.buckets of
              Just gs -> st { buckets = Map.insert m (g : gs) st.buckets }
              Nothing -> st { order = m : st.order, buckets = Map.insert m (g : Nil) st.buckets }
      )
      { order: Nil, buckets: Map.empty }
      gdefs
  in
    map
      ( \m -> Tuple m (Array.reverse (Array.fromFoldable (fromMaybe Nil (Map.lookup m result.buckets))))
      )
      (Array.reverse (Array.fromFoldable result.order))

-- | The native foreign keys a `SplitOutput` references, as a sorted list.
foreignKeysOf :: SplitOutput -> Array String
foreignKeysOf o = Array.fromFoldable o.foreigns
