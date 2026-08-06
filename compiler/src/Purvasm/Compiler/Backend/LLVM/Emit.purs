-- | The recursive ANF → `.ll` lowering: atoms, computations, expressions, and the two-entry lifted
-- | function emission (ADR-0072 §4, ADR-0076 §1). A faithful transcription of boot's `codegen_llvm.ml`
-- | (`atom`/`read_var`/`expr`/`cexpr`/`emit_ret`/`make_closure`/`arg_buffer`/`lift`/`emit_function`/
-- | `emit_pending`) — the ADR-0082 port; its boot byte-identity gate is retired (ADR-0104 §4) and
-- | emission is now L2-owned.
-- |
-- | Stack-safety (maintainer-flagged): the deep-linear ANF `Let`/`LetRec` spine is walked with
-- | `tailRecM` (`State` is `MonadRec`), and `emitPending` drains its LIFO queue with `tailRecM` too, so
-- | neither grows the JS stack with binding/emission depth. Tree recursion (`if`/`case` branches) stays
-- | ordinary, bounded by control-flow nesting.
-- |
-- | ADR-0105/0106 structure: every runtime/guest call this module emits goes through the
-- | classified seam (`Backend.LLVM.Safepoint` — the same rows `Liveness` classifies from),
-- | and every transient root through `Root.ensureRooted` (idempotent: an already-rooted
-- | token reuses its owned slot) via the activation's `FrameToken`, threaded lexically
-- | (`Maybe FrameToken` on the recipe signatures; `Nothing` = the plan elided the frame). Raw `emit` remains only for pure IR (branch/phi/gep/load/store/alloca/ptrtoint)
-- | and the module-skeleton `ret` lines.
-- |
-- | Coverage: the whole pure-value language (ADR-0082 §3, slices 1–3) — atoms (`Var`/`Int`/`Bool`/
-- | `Number`/`String`), `Ret`/`Let`/`LetRec`, `CAtom`/`CPrim`/`CIf`, calls and closures
-- | (`CApp` direct + `musttail` + generic `pv_apply`, `CLam`, let-bound lambdas, captures, self-calls),
-- | recursion (`buildGrec` shared by `LetRec` and the top-level `Grec`), constructors/records/arrays
-- | (`CCtor`/`CRecord`/`CAccessor`/`CUpdate`/`CArray`), and pattern matching (`CCase` over the shared
-- | `MatchCompile` decision tree). The one remaining stub is `AForeign` (a foreign leaf referenced as a
-- | value, slice 4 — needs the foreign-arity source), which crashes with a labelled `unsafeCrashWith`
-- | rather than emit wrong IR.
module Purvasm.Compiler.Backend.LLVM.Emit
  ( atom
  , readVar
  , emitRet
  , cexpr
  , expr
  , emitGcafInit
  , buildGrec
  , argBuffer
  , makeClosure
  , lift
  , emitFunction
  , emitPending
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Class (gets, modify_)
import Data.Array (range)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (abiSettle, forceValue)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (ctorTag, imm, immBool, immInt, immUnit, labelId, mangle, mangleForeign, sortRecordFields)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, PhiIncoming, beginFn, emit, emitAnfLabel, emitDefine, snapshotReloads, emitGuestRet, emitGuestStore, emitGuestSwitch, emitPayloadAshr, emitPhi, emitLowBitAnd, closeHopArm, emitStringConstant, foldA, forA, forA_, fresh, freshFn, freshLabel, mintCloWord, mintEnvWord, mintLoad, mintParam, takeFn)
import Purvasm.Compiler.Backend.LLVM.Prim (inlinePrim)
import Purvasm.Compiler.Backend.LLVM.Liveness (activationPlan, atomCanSafepoint, envPseudo, forcedAtomCanSafepoint, needsFrame)
import Purvasm.Compiler.Backend.LLVM.Root (FrameToken, emitGcafInitEngine, ensureRooted, musttailWith, openFrame, retWith, tailcallWith)
import Purvasm.Compiler.Backend.LLVM.Safepoint (RtArg(..), RtOp(..), guestDirect, rtCall, rtCallVoid)
import Purvasm.Compiler.Backend.LLVM.Types (BindingV(..), Env, EnvSrc(..), FnInfo, Lifted(..), LiftedBody(..), bindDirectFnVar, bindDirectVar, bindFnVar, bindVar, lookupEnv)
import Purvasm.Compiler.Backend.LLVM.Value (Val, rootedVal, vImm, vRootedGlobal)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (DTree(..), Proj(..)) as MC
import Purvasm.Compiler.MiddleEnd.MatchCompile (compile) as MatchCompile
import Purvasm.Compiler.Primitive (PrimOp(..))
import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)
import Purvasm.Number (floatBitsHi, floatBitsLo)

-- | Does the activation plan give THIS definition a root slot? `rootAll` (the init/`LClosure`
-- | conservative fallback) roots everything; otherwise only the plan's crossing set does.
shouldRoot :: String -> Codegen Boolean
shouldRoot n = do
  rootAll <- gets _.rootAll
  cr <- gets _.crossing
  pure (rootAll || Set.member n cr)

-- | The 0-based indices `[0 .. arity-1]` (empty when `arity = 0`, unlike `range 0 (arity-1)`).
paramIndices :: Int -> Array Int
paramIndices arity = if arity <= 0 then [] else range 0 (arity - 1)

-- | The boxed (`Number`/`String`) literals a binder compares against (boot's `binder_boxed_lits`). Their
-- | allocation cannot happen inside the match (a mid-match safepoint would invalidate the raw
-- | field/element operands nested binders hold), so a `case` pre-allocates and roots one value per
-- | distinct literal and the matcher only compares against the rooted handle.
binderBoxedLits :: Binder -> Array Literal
binderBoxedLits = case _ of
  BLit l@(LNumber _) -> [ l ]
  BLit l@(LString _) -> [ l ]
  BLit _ -> []
  BNull -> []
  BVar _ -> []
  BNamed _ inner -> binderBoxedLits inner
  BCtor _ subs -> Array.concatMap binderBoxedLits subs
  BArray subs -> Array.concatMap binderBoxedLits subs
  BRecord fields -> Array.concatMap (\f -> binderBoxedLits f.binder) fields

-- | Order two boxed literals the way boot's `sort_uniq compare` does (only `Number`/`String` reach here;
-- | `Number` sorts before `String`, matching boot's `C.lit` constructor order). Keyed structurally so a
-- | pathological NaN literal cannot fail its own lookup.
compareBoxed :: Literal -> Literal -> Ordering
compareBoxed a b = case a, b of
  LNumber x, LNumber y -> compare x y
  LString x, LString y -> compare x y
  LNumber _, LString _ -> LT
  LString _, LNumber _ -> GT
  _, _ -> EQ

-- | Sort-and-dedup the boxed literals (boot's `sort_uniq compare`): the emission order of their rooted
-- | constants at case entry must be deterministic — the L2-owned goldens and the ADR-0104 §2 stage
-- | fixpoint compare emitted text.
sortUniqBoxed :: Array Literal -> Array Literal
sortUniqBoxed = Array.nubByEq (\a b -> compareBoxed a b == EQ) <<< Array.sortBy compareBoxed

-- | An atom to its i64 operand token. A rooted `AtomVar` yields its slot's rooted token (the
-- | renderer reloads at consumption); `Int`/`Bool` literals are epoch-immune immediates.
atom :: Env -> Atom -> Codegen Val
atom env = case _ of
  AtomVar x -> readVar env x
  AtomLit (LInt n) -> pure (vImm (immInt n))
  AtomLit (LBool b) -> pure (vImm (immBool b))
  AtomLit (LNumber f) ->
    -- Boxed `Number` (ADR-0064 §1): pass the IEEE-754 bit pattern as the i64 payload.
    rtCall RtNewNumber [ I64 (int64BitsDecimal { hi: floatBitsHi f, lo: floatBitsLo f }) ]
  AtomLit (LString s) -> do
    Tuple p len <- stringConstant s
    rtCall RtNewStr [ Ptr p, I64 (show len) ]
  AtomForeign k -> do
    -- A native foreign leaf resolves by link-time symbol (ADR-0073 §3): reference its `AbiCodeFn`
    -- `@pvf_<mangle key>` and wrap it in a no-capture closure of the leaf's **physical closure arity**.
    -- That is `foreignArity` — which the driver derives from the FSR shape (`Driver.leafClosureArity`),
    -- **not** the raw semantic `ForeignShape.arity`: a nullary `Effect` leaf is physical arity 1 (it *is*
    -- the thunk), while its semantic arity is 0. ADR-0090 makes the shape the single source of truth, so a
    -- missing entry is a wiring bug — crash at compile time rather than default to a wrong closure arity
    -- (which would link but under/over-apply at runtime).
    modify_ \c -> c { foreigns = Set.insert k c.foreigns }
    arity <- gets (Map.lookup k <<< _.foreignArity) >>= case _ of
      Just a -> pure a
      Nothing -> unsafeCrashWith ("Backend.LLVM.Emit.atom: missing native foreign arity for " <> k <> " (FSR must provide every native leaf's shape, ADR-0090)")
    addr <- fresh
    emit ("  " <> addr <> " = ptrtoint ptr @" <> mangleForeign k <> " to i64")
    rtCall RtMakeClosure [ I64 addr, I32 (show arity), V (vImm immUnit) ]

-- | Materialise a string literal as a module-level `@.str.N` byte constant (boot's `string_constant`),
-- | returning the `getelementptr`-to-first-byte pointer operand and the byte length. An empty string is
-- | a null pointer of length 0 (no constant emitted, matching boot's early return). The constant
-- | itself is derived entirely inside `Monad.emitStringConstant` from the raw guest string.
stringConstant :: String -> Codegen (Tuple String Int)
stringConstant s = emitStringConstant s >>= case _ of
  Nothing -> pure (Tuple "null" 0)
  Just r -> do
    p <- fresh
    emit ("  " <> p <> " = getelementptr [" <> show r.len <> " x i8], ptr " <> r.name <> ", i64 0, i64 0")
    pure (Tuple p r.len)

-- | Read a variable: a DIRECT local hands back its token AS-IS (ADR-0105 §6.2 alias
-- | inheritance — a read is not a validity event, so a stale direct value stays stale and the
-- | use-point check catches it); a rooted local yields its ROOTED token — NO reload emits
-- | here (ADR-0105 §6.4): the renderer-owned reload cache materialises the current value at
-- | consumption (hit → reuse, miss → reload just before the consuming instruction). A
-- | top-level global yields a rooted token over its `@<mangle>$root` cell the same way (the
-- | read only registers the extern reference).
readVar :: Env -> String -> Codegen Val
readVar env x = case lookupEnv x env of
  Just entry -> case entry.bind of
    DirectV v -> pure v
    RootedV rv -> pure (rootedVal rv)
  Nothing -> do
    gkeys <- gets _.gkeys
    if Set.member x gkeys then do
      modify_ \c -> c { externs = Set.insert x c.externs }
      pure (vRootedGlobal ("@" <> mangle x <> "$root"))
    else
      unsafeCrashWith ("Backend.LLVM.Emit.readVar: unbound variable " <> x <> " (unresolved foreign?)")

-- | An atom to its forced value: a variable is forced (a demand site, e.g. an `if` condition or a primop
-- | operand); a literal/foreign is passed through unforced (never a cell).
forceAtom :: Env -> Atom -> Codegen Val
forceAtom env = case _ of
  a@(AtomVar _) -> atom env a >>= forceValue
  a -> atom env a

-- | Evaluate a list of atoms to their **current** value operands, mutually protected against each
-- | other's safepoints (ADR-0072 §6): an atom is rooted only when a *later* atom can safepoint,
-- | so the common all-vars/immediates list emits no `pv_root`. `force` forces each variable
-- | (a suspension may run guest code — itself a safepoint). A rooted atom flows on as its
-- | rooted token; the consuming renderer reloads on cache miss (ADR-0105 §6.4).
evalAtoms :: Maybe FrameToken -> Boolean -> Env -> Array Atom -> Codegen (Array Val)
evalAtoms frame force env atoms = do
  -- Stack-safety (2026-07-16 bugfix): a sequenced `State` step is a live host frame on the JS
  -- backend — a `Regex.Core.Unicode`-scale array literal (1,290 operands) was ~0.5× the
  -- default stack — so the pass is a `tailRecM` loop with a precomputed suffix scan for the
  -- later-safepoint test. (The former second pass — eager reload of every rooted slot — is
  -- retired by ADR-0105 §6.4: a rooted slot yields its rooted token and the consumption
  -- renderer reloads on cache miss.)
  vals <- tailRecM evalStep { i: 0, acc: Nil }
  pure (Array.fromFoldable (List.reverse vals))
  where
  -- the SAME row-derived classifiers the liveness analysis consults (seam single-source;
  -- a hardcoded arm here is exactly the analysis-vs-lowering drift class the seam exists to
  -- prevent — found live by the 2026-08-06 force counterfactual, where flipping the row
  -- moved the plan but not this pass).
  canSafepoint = if force then forcedAtomCanSafepoint else atomCanSafepoint

  isImmediate = case _ of
    AtomLit (LInt _) -> true
    AtomLit (LBool _) -> true
    _ -> false

  one = if force then forceAtom env else atom env

  -- `laterCan !! i` ⇔ the original `any canSafepoint rest` at element `i`.
  laterCan :: Array Boolean
  laterCan = Array.fromFoldable
    (Array.foldr (\a st -> { flag: st.flag || canSafepoint a, out: st.flag : st.out }) { flag: false, out: Nil } atoms).out

  -- Evaluate+root in list order: an atom a LATER atom can stale gets a slot and flows on as
  -- its rooted token; everything else flows as-is.
  evalStep st = case Array.index atoms st.i of
    Nothing -> pure (Done st.acc)
    Just a -> do
      v <-
        if isImmediate a then one a
        else do
          v0 <- one a
          if fromMaybe false (Array.index laterCan st.i) then rootedVal <$> ensureRooted frame v0 else pure v0
      pure (Loop { i: st.i + 1, acc: Cons v st.acc })

-- | Expect a produced value from a non-tail sub-expression (an `if`/`let` branch always yields one).
requireValue :: Maybe Val -> Codegen Val
requireValue = case _ of
  Just v -> pure v
  Nothing -> unsafeCrashWith "Backend.LLVM.Emit: non-tail expression produced no value"

-- | Emit the current function's return: pop the shadow-stack frame (iff this activation opened
-- | one — ADR-0105 frame elision), then `ret` — `Root.retWith`, the fused pop+terminator.
emitRet :: Maybe FrameToken -> Val -> Codegen Unit
emitRet = retWith

-- | Finish a produced value in the current tail context: in tail position emit the `ret` (and produce
-- | no value); otherwise hand the operand back.
finish :: Maybe FrameToken -> Boolean -> Val -> Codegen (Maybe Val)
finish frame tail v =
  if tail then do
    emitRet frame v
    pure Nothing
  else pure (Just v)

-- | Resolve a saturated call's statically-known direct target (ADR-0076 §2, ADR-0077 §2): a self-call,
-- | a let-bound lambda / recursive-group member, a same-module top-level function, or another module's
-- | *exported* function (`xfns`, the dependency's `.pmi` surface). `Nothing` falls back to the generic
-- | `pv_apply`/trampoline path. Resolution mirrors `readVar`'s order — local scope first, then the
-- | enclosing self binding, then this module's own globals, then the export surface — so a local
-- | rebinding never masquerades as the function, and same-module facts win over the interface. A
-- | cross-module hit is recorded in `xdecls` for its per-signature `declare tailcc` extern.
directTarget :: Env -> Atom -> Int -> Codegen (Maybe FnInfo)
directTarget env f nargs = case f of
  AtomVar x -> case lookupEnv x env of
    Just entry -> do
      sc <- gets _.selfCtx
      case sc of
        Just s
          | s.name == x
          , Just h0 <- s.captureHandle
          , entry.key == h0
          , s.fnInfo.arity == nargs -> pure (Just s.fnInfo)
        _ -> pure case entry.knownFn of
          Just info | info.arity == nargs -> Just info
          _ -> Nothing
    Nothing -> do
      gkeys <- gets _.gkeys
      if Set.member x gkeys then do
        sc <- gets _.selfCtx
        case sc of
          Just s
            | s.name == x
            , Nothing <- s.captureHandle
            , s.fnInfo.arity == nargs -> pure (Just s.fnInfo)
          _ -> do
            gfns <- gets _.gfns
            case Map.lookup x gfns of
              Just info | info.arity == nargs -> pure (Just info)
              -- own-module fact says unsaturated: never fall through to the surface.
              Just _ -> pure Nothing
              Nothing -> do
                xfns <- gets _.xfns
                case Map.lookup x xfns of
                  Just info | info.arity == nargs -> do
                    modify_ \c -> c { xdecls = Map.insert info.dsym info.arity c.xdecls }
                    pure (Just info)
                  _ -> pure Nothing
      else pure Nothing
  _ -> pure Nothing

-- | Lower a computation. Slice-1a handles `CAtom`; the rest are later slices.
cexpr :: Maybe FrameToken -> Env -> Boolean -> CExpr -> Codegen (Maybe Val)
cexpr frame env tail = case _ of
  CAtom a -> atom env a >>= finish frame tail
  -- GER run point (ADR-0099): `perform t ≃ t unit`. Delegate to the `CApp` path so the direct /
  -- `musttail` / generic `pv_apply` machinery (and tail position) is reused unchanged.
  CPerform t -> cexpr frame env tail (CApp t [ AtomLit (LInt 0) ])
  CPrim op args -> do
    -- A primop consumes its operands' *values* (e.g. `RecordGet` on a by-need dict), so force them.
    ops <- evalAtoms frame true env args
    inlinePrim op ops >>= case _ of
      Just t -> finish frame tail t
      Nothing -> do
        t <- rtCall (RtPrim op) (map V ops)
        finish frame tail t
  CIf a t e -> do
    -- A Boolean demand site: force a by-need cell reaching the condition before reading its payload bit.
    c <- forceAtom env a
    -- payload != 0 ⇒ true (ADR-0064 §1).
    p <- fresh
    emitPayloadAshr p c
    b <- fresh
    emit ("  " <> b <> " = icmp ne i64 " <> p <> ", 0")
    lt <- freshLabel "then"
    le <- freshLabel "else"
    -- the construct's branch-point snapshot (§6.4): taken after the condition's own
    -- consumption, so its reloads are shared into both arms and the join.
    snap <- snapshotReloads
    emit ("  br i1 " <> b <> ", label %" <> lt <> ", label %" <> le)
    if tail then do
      emitAnfLabel snap lt
      void (expr frame env true t)
      emitAnfLabel snap le
      void (expr frame env true e)
      pure Nothing
    else do
      lend <- freshLabel "endif"
      emitAnfLabel snap lt
      vt <- requireValue =<< expr frame env false t
      -- the block a value flows from may differ from `lt` after nested control flow. The
      -- incoming freeze IS the arm close (Monad.closeHopArm — §6.2 round 3).
      bt <- freshLabel "thenv"
      inT <- closeHopArm { hop: bt, merge: lend } vt
      emitAnfLabel snap le
      ve <- requireValue =<< expr frame env false e
      be <- freshLabel "elsev"
      inE <- closeHopArm { hop: be, merge: lend } ve
      emitAnfLabel snap lend
      r <- fresh
      Just <$> emitPhi r [ inT, inE ]
  CLam ps body -> do
    l <- lift env ps body
    makeClosure env l >>= finish frame tail
  CApp f args -> do
    directTarget env f (Array.length args) >>= case _ of
      Just info -> do
        -- Direct known-arity call (ADR-0076 §2/§3): the env word is derived per the callee's shape;
        -- a cell force is a safepoint (the suspension may run guest code), so operands are re-read
        -- from their roots after it.
        Tuple envOp ops <- case info.src of
          SSelf -> do
            sc <- gets _.selfCtx
            s <- case sc of
              Just s -> pure s
              Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: self-call outside a self context"
            ops <- evalAtoms frame false env args
            let
              envOp = case s.envBind of
                DirectV v -> v
                RootedV rv -> rootedVal rv
            pure (Tuple envOp ops)
          SSentinel -> do
            ops <- evalAtoms frame false env args
            pure (Tuple (vImm immUnit) ops)
          SClosureEnv -> do
            all <- evalAtoms frame false env (Array.cons f args)
            case Array.uncons all of
              Just { head: fv, tail: ops } -> do
                e <- rtCall RtReadField [ V fv, I64 "2" ]
                pure (Tuple e ops)
              Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: empty CApp operand list"
          SForceCell -> do
            fh <- atom env f >>= ensureRooted frame
            argHs <- forA args (\a -> atom env a >>= ensureRooted frame)
            forcedVal <- rtCall RtForceIfByneed [ V (rootedVal fh) ]
            e <- rtCall RtReadField [ V forcedVal, I64 "2" ]
            pure (Tuple e (map rootedVal argHs))
        inDir <- gets _.inDirect
        if tail && inDir then do
          -- musttail (ADR-0076 §3): the fused pop+musttail+ret — every operand (env word
          -- included) is computed before the pop; no safepoint in between.
          musttailWith frame { dsym: info.dsym, env: envOp, args: ops }
          pure Nothing
        else do
          r <- guestDirect { dsym: info.dsym, env: envOp, args: ops }
          -- Settle (ADR-0076 §3): the callee may have stashed a generic tail bounce no enclosing
          -- `pv_apply` loop will take on this direct path — run it to a real value.
          r' <- abiSettle r
          if tail then do
            emitRet frame r'
            pure Nothing
          else pure (Just r')
      Nothing -> do
        -- `f` and the args are mutually protected: a foreign callee or a `String` arg may allocate.
        all <- evalAtoms frame false env (Array.cons f args)
        case Array.uncons all of
          Just { head: fv, tail: ops } ->
            if tail then do
              -- Trampoline tail call (ADR-0071 §4): stash the pending tail, pop this frame, return.
              Tuple p n <- argBuffer ops
              tailcallWith frame { fv, argp: p, nargs: n }
              pure Nothing
            else do
              Tuple p n <- argBuffer ops
              t <- rtCall RtApply [ V fv, Ptr p, I64 (show n) ]
              pure (Just t)
          Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: empty CApp operand list"
  CCtor name arity args ->
    let
      nargs = Array.length args
    in
      if nargs > arity then
        unsafeCrashWith ("Backend.LLVM.Emit.cexpr: over-applied constructor " <> name <> " (" <> show nargs <> "/" <> show arity <> ")")
      else if nargs < arity then do
        -- An unsaturated constructor is a first-class function that accumulates the remaining fields
        -- (ADR-0072 §5): synthesise a builder closure `\$ctorarg0 … -> Ctor(name, …)` and apply the
        -- fields supplied so far (`nargs = 0` is just the builder).
        let
          params = map (\i -> "$ctorarg" <> show i) (paramIndices arity)
          body = Ret (CCtor name arity (map AtomVar params))
        builder <- lift env params body >>= makeClosure env
        if nargs == 0 then finish frame tail builder
        else do
          bh <- ensureRooted frame builder
          ops <- evalAtoms frame false env args
          Tuple p n <- argBuffer ops
          t <- rtCall RtApply [ V (rootedVal bh), Ptr p, I64 (show n) ]
          finish frame tail t
      else if arity == 0 then
        -- nullary → an immediate tag (ADR-0064 §1).
        finish frame tail (vImm (imm (ctorTag name)))
      else do
        ops <- evalAtoms frame false env args
        Tuple p n <- argBuffer ops
        t <- rtCall RtNewAdt [ I32 (show (ctorTag name)), Ptr p, I64 (show n) ]
        finish frame tail t
  CArray elems ->
    if Array.null elems then do
      t <- rtCall RtEmptyArray []
      finish frame tail t
    else do
      ops <- evalAtoms frame false env elems
      Tuple p n <- argBuffer ops
      t <- rtCall RtNewArray [ Ptr p, I64 (show n) ]
      finish frame tail t
  CRecord fields ->
    -- Hash each label, sort by unsigned id ascending (ADR-0069 §1), pass parallel id/value buffers.
    let
      sorted = sortRecordFields (map (\f -> Tuple f.prop f.val) fields)
      n = Array.length sorted
    in
      if n == 0 then do
        t <- rtCall RtNewRecord [ Ptr "null", Ptr "null", I64 "0" ]
        finish frame tail t
      else do
        let ids = map (\(Tuple l _) -> labelId l) sorted
        -- Values are mutually protected: a later `String`/`Number` field must not stale an earlier one.
        vals <- evalAtoms frame false env (map snd sorted)
        -- label ids are raw metadata words, epoch-immune in the id buffer.
        Tuple idp _ <- argBuffer (map vImm ids)
        Tuple valp _ <- argBuffer vals
        t <- rtCall RtNewRecord [ Ptr idp, Ptr valp, I64 (show n) ]
        finish frame tail t
  CAccessor a label -> do
    -- A dictionary projection (ADR-0070 §5): force a by-need record before reading its field.
    r <- forceAtom env a
    t <- rtCall RtRecordGet [ V r, I64 (labelId label) ]
    finish frame tail t
  CUpdate a ups -> do
    -- Functional update: fold `record_set` (each returns a new record). The base is forced (a by-need
    -- dict update); the accumulator is rooted across each value's evaluation and reloaded before the set.
    rh0 <- forceAtom env a >>= ensureRooted frame
    rhFinal <- foldA
      ( \rh up -> do
          v <- atom env up.val
          t <- rtCall RtRecordSet [ V (rootedVal rh), I64 (labelId up.prop), V v ]
          ensureRooted frame t
      )
      rh0
      ups
    finish frame tail (rootedVal rhFinal)
  CCase scruts alts -> do
    -- The shared Maranget decision tree (ADR-0083) lowered to LLVM. Occurrences — scrutinees and every
    -- extracted sub-value — are rooted and read through their rooted tokens (the renderer-owned
    -- reload cache, ADR-0105 §6.4): the tree shares sub-occurrences across rows, and a guarded
    -- row's fall-through reuses them after its guard body may have safepointed. Boxed literals
    -- are pre-rooted once at entry so the tree walk never allocates.
    let { scrutBinds, tree } = MatchCompile.compile scruts alts
    -- Root each scrutinee (forced — matching dereferences its structure).
    occEnv0 <- forA scrutBinds (\(Tuple occ a) -> Tuple occ <$> (forceAtom env a >>= ensureRooted frame))
    -- Hoist + root every boxed literal any arm compares against.
    litEnv <- forA (sortUniqBoxed (Array.concatMap (\alt -> Array.concatMap binderBoxedLits alt.binders) alts))
      (\l -> Tuple l <$> (atom env (AtomLit l) >>= ensureRooted frame))
    failLabel <- freshLabel "nomatch"
    merge <- if tail then pure "" else freshLabel "casejoin"
    -- the case-level snapshot: taken after the scrutinee/boxed-literal rooting (pre-dispatch),
    -- so it dominates every path into the fail block and the join.
    caseSnap <- snapshotReloads
    let
      lookupOcc oenv occ = case Array.find (\(Tuple k _) -> k == occ) oenv of
        Just (Tuple _ h) -> h
        Nothing -> unsafeCrashWith ("Backend.LLVM.Emit.cexpr: unbound case occurrence " <> occ)

      cur oenv occ = rootedVal (lookupOcc oenv occ)

      boxedHandle l = case Array.find (\(Tuple k _) -> k == l) litEnv of
        Just (Tuple _ h) -> h
        Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: boxed literal not hoisted"

      -- Read a sub-value raw (allocation-free), then root it — extending `oenv` (most-recent first).
      extract oenv parent (Tuple subOcc pr) = do
        let parentCur = cur oenv parent
        rawVal <- case pr of
          MC.Pfield j ->
            rtCall RtReadField [ V parentCur, I64 (show (1 + j)) ]
          MC.Pelem j ->
            rtCall (RtPrim IndexArray) [ V parentCur, V (vImm (immInt j)) ]
          MC.Precord l ->
            rtCall RtRecordGet [ V parentCur, I64 (labelId l) ]
        h <- ensureRooted frame rawVal
        pure (Array.cons (Tuple subOcc h) oenv)

      bindLeaf oenv binds = foldl (\e (Tuple v occ) -> bindVar e v (lookupOcc oenv occ)) env binds

      -- A matched body's value reaches the phi through a fresh single-predecessor block (the CIf
      -- idiom), carrying the epoch at its arm's end for the per-arm phi verification (§6.2).
      runBody env' e phis =
        if tail then do
          void (expr frame env' true e)
          pure phis
        else do
          v <- requireValue =<< expr frame env' false e
          vb <- freshLabel "altv"
          inc <- closeHopArm { hop: vb, merge } v
          pure (Array.cons inc phis)

      lower oenv dt phis = case dt of
        MC.Dfail _ -> do
          emit ("  br label %" <> failLabel)
          pure phis
        MC.Dleaf binds e -> runBody (bindLeaf oenv binds) e phis
        MC.Dguard binds clauses ft -> do
          let env' = bindLeaf oenv binds
          let
            guards cls acc = case Array.uncons cls of
              Nothing -> lower oenv ft acc
              Just { head: clause, tail: rest } -> do
                -- A Boolean demand site: force a by-need guard result before reading its bit.
                gv <- forceValue =<< requireValue =<< expr frame env' false clause.guard
                pay <- fresh
                emitPayloadAshr pay gv
                bb <- fresh
                emit ("  " <> bb <> " = icmp ne i64 " <> pay <> ", 0")
                yes <- freshLabel "gyes"
                no <- freshLabel "gno"
                snap <- snapshotReloads
                emit ("  br i1 " <> bb <> ", label %" <> yes <> ", label %" <> no)
                emitAnfLabel snap yes
                acc' <- runBody env' clause.rhs acc
                emitAnfLabel snap no
                guards rest acc'
          guards clauses phis
        MC.DswitchCtor occ arms default -> do
          -- Dispatch by representation first (immediate nullary vs field-carrying pointer), then by tag.
          let scrut = cur oenv occ
          low <- fresh
          emitLowBitAnd low scrut
          isImm <- fresh
          emit ("  " <> isImm <> " = icmp eq i64 " <> low <> ", 1")
          immBlk <- freshLabel "ctimm"
          ptrBlk <- freshLabel "ctptr"
          -- branch-point snapshot (§6.4): the scrutinee's reload above is shared into the
          -- representation arms, the tag arms and the default.
          snap <- snapshotReloads
          emit ("  br i1 " <> isImm <> ", label %" <> immBlk <> ", label %" <> ptrBlk)
          defaultLbl <- freshLabel "ctdef"
          armLbls <- forA arms (\(Tuple tag arm) -> { tag, arm, l: _ } <$> freshLabel "ctarm")
          let
            casesFor keep = joinWith " "
              ( map (\a -> "i64 " <> show (ctorTag a.tag) <> ", label %" <> a.l)
                  (Array.filter (\a -> keep a.arm.extracts) armLbls)
              )
          -- immediates → the nullary (no-extract) arms, keyed by payload tag.
          emitAnfLabel snap immBlk
          itag <- fresh
          emitPayloadAshr itag scrut
          emit ("  switch i64 " <> itag <> ", label %" <> defaultLbl <> " [ " <> casesFor Array.null <> " ]")
          -- pointers → the field-carrying arms, keyed by the tag at raw word 0.
          emitAnfLabel snap ptrBlk
          ptag <- rtCall RtReadRaw [ V scrut, I64 "0" ]
          emitGuestSwitch ptag defaultLbl (casesFor (not <<< Array.null))
          phis' <- foldA
            ( \acc a -> do
                emitAnfLabel snap a.l
                oenv' <- foldA (\oe ex -> extract oe occ ex) oenv a.arm.extracts
                lower oenv' a.arm.sub acc
            )
            phis
            armLbls
          emitAnfLabel snap defaultLbl
          lower oenv default phis'
        MC.DswitchLit occ arms default -> case Array.uncons arms of
          Nothing -> lower oenv default phis
          Just { head: Tuple headLit _ } -> case headLit of
            LNumber _ -> lowerBoxedChain oenv occ arms default phis
            LString _ -> lowerBoxedChain oenv occ arms default phis
            _ -> do
              -- Immediate literals → a direct LLVM switch on the tagged word.
              let scrut = cur oenv occ
              defaultLbl <- freshLabel "swdef"
              armLbls <- forA arms (const (freshLabel "swarm"))
              let
                immOf = case _ of
                  LInt n -> immInt n
                  LBool b -> immBool b
                  _ -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: non-immediate literal in immediate switch"
                cases = Array.zipWith (\(Tuple l _) lbl -> "i64 " <> immOf l <> ", label %" <> lbl) arms armLbls
              emitGuestSwitch scrut defaultLbl (joinWith " " cases)
              snap <- snapshotReloads
              phis' <- foldA
                ( \acc (Tuple (Tuple _ sub) lbl) -> do
                    emitAnfLabel snap lbl
                    lower oenv sub acc
                )
                phis
                (Array.zip arms armLbls)
              emitAnfLabel snap defaultLbl
              lower oenv default phis'
        MC.DswitchLen occ arms default -> do
          lenVal <- rtCall (RtPrim LengthArray) [ V (cur oenv occ) ]
          defaultLbl <- freshLabel "swdef"
          armLbls <- forA arms (const (freshLabel "swarm"))
          let cases = Array.zipWith (\(Tuple n _) lbl -> "i64 " <> immInt n <> ", label %" <> lbl) arms armLbls
          emitGuestSwitch lenVal defaultLbl (joinWith " " cases)
          snap <- snapshotReloads
          phis' <- foldA
            ( \acc (Tuple (Tuple _ arm) lbl) -> do
                emitAnfLabel snap lbl
                oenv' <- foldA (\oe ex -> extract oe occ ex) oenv arm.extracts
                lower oenv' arm.sub acc
            )
            phis
            (Array.zip arms armLbls)
          emitAnfLabel snap defaultLbl
          lower oenv default phis'
        MC.DexpandRecord occ extracts sub -> do
          oenv' <- foldA (\oe ex -> extract oe occ ex) oenv extracts
          lower oenv' sub phis

      -- Boxed literals cannot be `switch`ed — an equality chain against the hoisted rooted handles,
      -- via the runtime `Eq` primop (the tested source of truth for IEEE / bytes).
      lowerBoxedChain oenv occ arms default phis = case Array.uncons arms of
        Nothing -> lower oenv default phis
        Just { head: Tuple l sub, tail: rest } -> do
          let
            prim = case l of
              LNumber _ -> EqNumber
              _ -> EqString
          eqVal <- rtCall (RtPrim prim) [ V (cur oenv occ), V (rootedVal (boxedHandle l)) ]
          pay <- fresh
          emitPayloadAshr pay eqVal
          ok <- fresh
          emit ("  " <> ok <> " = icmp ne i64 " <> pay <> ", 0")
          armLbl <- freshLabel "ltarm"
          next <- freshLabel "ltnext"
          snap <- snapshotReloads
          emit ("  br i1 " <> ok <> ", label %" <> armLbl <> ", label %" <> next)
          emitAnfLabel snap armLbl
          phis' <- lower oenv sub phis
          emitAnfLabel snap next
          lowerBoxedChain oenv occ rest default phis'
    phis <- lower occEnv0 tree []
    emitAnfLabel caseSnap failLabel
    rtCallVoid RtCaseFail []
    emit "  unreachable"
    if tail then pure Nothing
    -- A non-tail `case` produces a value through the phi, so at least one arm body must reach `merge`
    -- (`phis` non-empty). A wholly-`Dfail` tree (no `Dleaf`/`Dguard` body — only reachable for a source
    -- `case` with no alternatives, which the surface never produces) would emit an entry-less `phi i64`
    -- (invalid LLVM); pin the invariant rather than emit it.
    else if Array.null phis then
      unsafeCrashWith "Backend.LLVM.Emit.cexpr: non-tail CCase reached no arm body (all-fail decision tree)"
    else do
      emitAnfLabel caseSnap merge
      r <- fresh
      Just <$> emitPhi r (Array.reverse phis)

-- | Emit a `Gcaf`'s `$init` — the FIXED-SHAPE public surface (ADR-0106 slice 2): callers
-- | supply DATA only (the key and the body `Expr`); the activation plan decides the frame
-- | and drives the body's rooting exactly as `emitFunction` drives an `LBody` (a `Gcaf`
-- | body is an ordinary expression with no params/captures/self), and `Root`'s engine owns
-- | the phase order. The permanent tier is untouched: the candidate snapshots pre-pop
-- | (`GlobalSlot` handle-copies) and roots permanently after it.
emitGcafInit :: String -> Expr -> Codegen Unit
emitGcafInit key e =
  let
    plan = activationPlan { params: [], captures: [], selfName: Nothing } e
  in
    emitGcafInitEngine
      { key
      , framed: needsFrame plan
      , body: \mtok -> do
          -- after the engine's beginFn (which resets to the rootAll fallback): the plan
          -- drives this body's rooting, exactly as emitFunction's LBody arm.
          modify_ \c -> c { rootAll = false, crossing = plan.crossing }
          requireValue =<< expr mtok Nil false e
      }

-- | Push a lifted lambda onto the pending-emit queue (LIFO).
pushPending :: Lifted -> Codegen Unit
pushPending l = modify_ \c -> c { pending = Cons l c.pending }

-- | A pre-lifted recursive-group function member: its source name, the `Lifted` record emitted as its
-- | own two-entry function, and the direct-call `FnInfo` its (force-cell) siblings resolve through.
type MemberFn = { m :: String, lifted :: Lifted, info :: FnInfo }

-- | Build a recursive group as all-by-need `ByNeed` cells over one shared env (ADR-0070 §4, mirroring
-- | the runtime `build_group`): each member is a cell whose nullary suspension, over the shared env,
-- | builds the member's value (a function member's suspension builds its pre-lifted closure; `apply`
-- | auto-forces a by-need callee). Returns `env` extended with each member bound to its rooted cell
-- | handle. Shared by the in-function `LetRec` and the top-level `Grec` init unit; `named` supplies
-- | stable top-level member code symbols (so `gfns`'s pre-registered `$d` names line up), else members
-- | get fresh `recfn_N` names.
buildGrec :: Maybe FrameToken -> (String -> Maybe String) -> Env -> Array (Tuple String Expr) -> Codegen Env
buildGrec frame named env binds = do
  gkeys <- gets _.gkeys
  let
    members = map fst binds
    k = Array.length members
    memberSet = Set.fromFoldable members
    enclosingLocals = Set.fromFoldable (map fst env)
    readableGlobals = Set.difference gkeys enclosingLocals
    -- members excluded, and top-level globals not shadowed by an enclosing local (read via `$root`).
    outsideFvs = foldl (\acc (Tuple _ rhs) -> Set.union acc (fvExpr memberSet rhs)) Set.empty binds
    outside = Set.toUnfoldable (Set.difference outsideFvs readableGlobals) :: Array String
    -- the shared env layout: the k member cells, then the outside captures.
    sharedLayout = members <> outside
    sharedNames = Set.fromFoldable sharedLayout
  -- Pre-lift each function member under a stable name; a shared name shadows an equally-named global,
  -- so it is captured (`globalsUnshadowed = gkeys \ sharedNames`).
  xfns <- gets _.xfns
  memberFns0 <- map Array.catMaybes $ flip forA
    ( \(Tuple m rhs) -> case rhs of
        Ret (CLam ps b) -> do
          Tuple name top <- case named m of
            Just n -> pure (Tuple n true)
            Nothing -> do
              nm <- freshFn "recfn_"
              pure (Tuple nm false)
          let
            bound = Set.fromFoldable ps
            globalsUnshadowed = Set.difference gkeys sharedNames
            captures = Set.toUnfoldable (Set.difference (fvExpr bound b) globalsUnshadowed)
            info = { dsym: name <> "$d", arity: Array.length ps, src: SForceCell }
            lifted = Lifted
              { name
              , params: ps
              , captures
              , body: LBody b
              , selfName: Just m
              , captureFns: []
              , exported: top && Map.member m xfns
              }
          pure (Just { m, lifted, info })
        _ -> pure Nothing
    )
    binds
  -- Every member lambda may reach its siblings through its captures — give each the group's info.
  let
    groupInfos = map (\r -> Tuple r.m r.info) memberFns0
    memberFns = map (\r -> case r.lifted of Lifted lm -> r { lifted = Lifted (lm { captureFns = groupInfos }) }) memberFns0
  forA_ memberFns (\r -> pushPending r.lifted)
  -- One suspension per member over the shared env: a function member's suspension builds its
  -- pre-lifted closure; any other member's suspension evaluates its RHS.
  suspNames <- flip forA
    ( \(Tuple m rhs) -> do
        name <- freshFn "susp_"
        let
          body = case Array.find (\r -> r.m == m) memberFns of
            Just r -> LClosure r.lifted
            Nothing -> LBody rhs
        pushPending
          ( Lifted
              { name, params: [ "$u" ], captures: sharedLayout, body, selfName: Nothing, captureFns: [], exported: false }
          )
        pure name
    )
    binds
  -- 1. shared env array = [unit × k] ++ [outside-capture values]; root it.
  outsideVals <- forA outside (readVar env)
  let elems = Array.replicate k (vImm immUnit) <> outsideVals
  Tuple envP envN <- argBuffer elems
  envArr <- rtCall RtNewArray [ Ptr envP, I64 (show envN) ]
  envH <- ensureRooted frame envArr
  -- 2. placeholder cells; store each into env[i] (reloading env/cell after each allocation).
  cellHs <- flip forA
    ( \i -> do
        cell <- rtCall RtNewByneedPlaceholder []
        ch <- ensureRooted frame cell
        rtCallVoid RtWriteField [ V (rootedVal envH), I64 (show i), V (rootedVal ch) ]
        pure ch
    )
    (paramIndices k)
  -- 3. build each suspension closure over the shared env; backpatch it into its cell.
  flip forA_
    ( \(Tuple name ch) -> do
        addr <- fresh
        emit ("  " <> addr <> " = ptrtoint ptr @" <> name <> " to i64")
        susp <- rtCall RtMakeClosure [ I64 addr, I32 "1", V (rootedVal envH) ]
        rtCallVoid RtByneedSetSuspension [ V (rootedVal ch), V susp ]
    )
    (Array.zip suspNames cellHs)
  -- 4. bind each member to its cell — function members carry their direct-call info.
  pure $ foldl
    ( \e (Tuple m ch) -> case Array.find (\r -> r.m == m) memberFns of
        Just r -> bindFnVar e m ch r.info
        Nothing -> bindVar e m ch
    )
    env
    (Array.zip members cellHs)

-- | Lower an expression. The `Let`/`LetRec` spine is walked with `tailRecM` (stack-safe); the tail flag
-- | is constant along the spine and applies to the final `Ret`.
expr :: Maybe FrameToken -> Env -> Boolean -> Expr -> Codegen (Maybe Val)
expr frame env0 tail = tailRecM step <<< Tuple env0
  where
  step (Tuple env e) = case e of
    Ret c -> Done <$> cexpr frame env tail c
    Let x c body -> case c of
      -- A let-bound lambda is a direct-call candidate (ADR-0076 §2): keep its lifted identity on the
      -- binding so saturated calls skip the generic dispatch. Non-recursive, so no self.
      CLam ps lbody -> do
        l <- lift env ps lbody
        let Lifted lr = l
        v <- makeClosure env l
        let
          info =
            { dsym: lr.name <> "$d"
            , arity: Array.length ps
            , src: if Array.null lr.captures then SSentinel else SClosureEnv
            }
        rootIt <- shouldRoot x
        if rootIt then do
          rv <- ensureRooted frame v
          pure (Loop (Tuple (bindFnVar env x rv info) body))
        else pure (Loop (Tuple (bindDirectFnVar env x v info) body))
      _ -> do
        mv <- cexpr frame env false c
        case mv of
          Just v -> do
            rootIt <- shouldRoot x
            if rootIt then do
              rv <- ensureRooted frame v
              pure (Loop (Tuple (bindVar env x rv) body))
            else pure (Loop (Tuple (bindDirectVar env x v) body))
          Nothing ->
            unsafeCrashWith "Backend.LLVM.Emit.expr: non-tail cexpr produced no value"
    LetRec binds body -> do
      env' <- buildGrec frame (const Nothing) env (map (\r -> Tuple r.var r.rhs) binds)
      pure (Loop (Tuple env' body))

-- | Materialise an i64 arg buffer for a call (an `alloca` holding the operands), returning the pointer
-- | operand and count. Zero args → a null pointer. Each operand token is verified at ITS store
-- | (ADR-0105 §6.2 — the buffer handover consumption point; verification only, no epoch bump).
argBuffer :: Array Val -> Codegen (Tuple String Int)
argBuffer operands =
  let
    n = Array.length operands
  in
    if n == 0 then pure (Tuple "null" 0)
    else do
      buf <- fresh
      emit ("  " <> buf <> " = alloca [" <> show n <> " x i64]")
      -- Stack-safety (2026-07-16 bugfix): per-operand `forWithIndex_` over `State` is a live host
      -- frame per element — a `Regex.Core.Unicode`-scale array literal's store sequence was the
      -- remaining ~0.3×-stack overflow after `evalAtoms` was hardened. Same order, `tailRecM` loop.
      tailRecM
        ( \i -> case Array.index operands i of
            Nothing -> pure (Done unit)
            Just v -> do
              p <- fresh
              emit ("  " <> p <> " = getelementptr [" <> show n <> " x i64], ptr " <> buf <> ", i64 0, i64 " <> show i)
              emitGuestStore v p
              pure (Loop (i + 1))
        )
        0
      p0 <- fresh
      emit ("  " <> p0 <> " = getelementptr [" <> show n <> " x i64], ptr " <> buf <> ", i64 0, i64 0")
      pure (Tuple p0 n)

-- | Build a closure value for a lifted lambda: assemble the captured-env array from the current values
-- | of its free variables, then `pv_make_closure` over the lifted function's address.
makeClosure :: Env -> Lifted -> Codegen Val
makeClosure env (Lifted l) = do
  envWord <- case l.captures of
    [] -> pure (vImm immUnit)
    caps -> do
      vals <- forA caps (readVar env)
      Tuple p n <- argBuffer vals
      rtCall RtNewArray [ Ptr p, I64 (show n) ]
  addr <- fresh
  emit ("  " <> addr <> " = ptrtoint ptr @" <> l.name <> " to i64")
  rtCall RtMakeClosure [ I64 addr, I32 (show (Array.length l.params)), V envWord ]

-- | Register an inline lambda for hoisting and return its `Lifted` record (captures fixed in sorted
-- | order). A top-level global is read via its `$root` handle, never captured — except a global shadowed
-- | by an enclosing local, which is captured (ADR-0076 §2).
lift :: Env -> Array String -> Expr -> Codegen Lifted
lift env params body = do
  name <- freshFn "fn_"
  gkeys <- gets _.gkeys
  let
    bound = Set.fromFoldable params
    localNames = Set.fromFoldable (map fst env)
    globalsUnshadowed = Set.difference gkeys localNames
    captures = Set.toUnfoldable (Set.difference (fvExpr bound body) globalsUnshadowed)
    l = Lifted
      { name
      , params
      , captures
      , body: LBody body
      , selfName: Nothing
      , captureFns: []
      , exported: false
      }
  modify_ \c -> c { pending = Cons l c.pending }
  pure l

-- | Emit one lifted function as its two entries (ADR-0076 §1): the `tailcc` direct entry `@<name>$d`
-- | (params as parameters, body in tail position) and the generic `@<name>` unpack-and-call wrapper.
emitFunction :: Lifted -> Codegen Unit
emitFunction (Lifted l) = do
  let arity = Array.length l.params
  -- direct entry
  beginFn
  -- ADR-0105 slice 2: the activation plan decides which definitions get root slots and whether a
  -- frame exists at all. An `LClosure` wrapper keeps the conservative root-on-create fallback
  -- (rootAll, always framed); an `LBody` is plan-driven. A rooted definition's reads flow as its
  -- rooted token, materialised through the renderer-owned reload cache at consumption (§6.4:
  -- one reload per slot per safepoint window/ANF block, not one per use).
  plan <- case l.body of
    LBody e -> do
      let p = activationPlan { params: l.params, captures: l.captures, selfName: l.selfName } e
      modify_ \c -> c { rootAll = false, crossing = p.crossing }
      pure (Just p)
    LClosure _ -> pure Nothing
  let framed = maybe true needsFrame plan
  -- the frame capability: minted here iff the plan needs one; every rooting/pop site below
  -- receives it lexically (ADR-0105 §2 `ensureRooted`'s fresh arm requires the token).
  mtok <- if framed then Just <$> openFrame else pure Nothing
  shouldRootName <- do
    rootAll <- gets _.rootAll
    cr <- gets _.crossing
    pure (\n -> rootAll || Set.member n cr)
  let
    -- a parameter is a function-entry value: its token mints at the prologue epoch (§6.2).
    bindParam env (Tuple p i) = do
      pTok <- mintParam i
      if shouldRootName p then do
        rv <- ensureRooted mtok pTok
        pure (bindVar env p rv)
      else pure (bindDirectVar env p pTok)
  env1 <- foldA bindParam Nil (Array.mapWithIndex (\i p -> Tuple p i) l.params)
  -- the `%env` word is itself a function-entry value; its token mints once at the prologue
  -- (the capture reads and any later self-call inherit it — never re-stamped, §6.2).
  envTok <- mintEnvWord
  -- captures: positional reads from the env word `%env` (the shared/captured array); a capture that is
  -- a known recursive-group function member carries its direct-call info (`captureFns`). `selfHandle`
  -- is the identity of the captured self binding (a member calling itself through its capture).
  let
    stepCap (Tuple env sh) (Tuple i c) = do
      v <- rtCall RtReadField [ V envTok, I64 (show i) ]
      env' <- case Array.find (\(Tuple k _) -> k == c) l.captureFns of
        Just (Tuple _ info) ->
          if shouldRootName c then ensureRooted mtok v <#> \rv -> bindFnVar env c rv info
          else pure (bindDirectFnVar env c v info)
        Nothing ->
          if shouldRootName c then ensureRooted mtok v <#> \rv -> bindVar env c rv
          else pure (bindDirectVar env c v)
      let
        sh' =
          if Just c == l.selfName then case lookupEnv c env' of
            Just en -> Just en.key
            Nothing -> sh
          else sh
      pure (Tuple env' sh')
  Tuple env2 selfHandle <- case l.captures of
    [] -> pure (Tuple env1 Nothing)
    _ -> foldA stepCap (Tuple env1 Nothing) (Array.mapWithIndex Tuple l.captures)
  -- the self-call shortcut (ADR-0076 §2): while this body runs, a saturated call to `selfName`
  -- re-enters this very function with this very `%env`. Root the env word iff the plan says the
  -- `%env` pseudo-name crosses (a self-call sits after a safepoint — the raw `%env` SSA value
  -- would be stale); otherwise the inherited prologue token is exact at every self-call (and
  -- the §6.2 use-point check holds the plan to that claim).
  savedSelf <- gets _.selfCtx
  savedDirect <- gets _.inDirect
  case l.selfName of
    Just nm -> do
      envBind <-
        if shouldRootName envPseudo then RootedV <$> ensureRooted mtok envTok
        else pure (DirectV envTok)
      modify_ \c -> c
        { selfCtx = Just
            { name: nm
            , captureHandle: selfHandle
            , envBind
            , fnInfo: { dsym: l.name <> "$d", arity, src: SSelf }
            }
        }
    Nothing -> modify_ \c -> c { selfCtx = Nothing }
  modify_ \c -> c { inDirect = true }
  case l.body of
    LBody e -> void (expr mtok env2 true e)
    LClosure lm -> do
      clo <- makeClosure env2 lm
      emitRet mtok clo
  modify_ \c -> c { selfCtx = savedSelf, inDirect = savedDirect }
  body <- takeFn
  let
    linkage = if l.exported then "" else "internal "
    dparams = foldMap (\i -> ", i64 %p" <> show i) (paramIndices arity)
  emitDefine
    ( "define " <> linkage <> "tailcc i64 @" <> l.name <> "$d(ptr %ctx, i64 %env" <> dparams <> ") {\n"
        <> "entry:\n"
    )
    body
  -- generic wrapper
  beginFn
  envw <- case l.captures of
    [] -> pure (vImm immUnit)
    _ -> do
      cloTok <- mintCloWord
      rtCall RtReadField [ V cloTok, I64 "2" ]
  args <- flip forA
    ( \i -> do
        p <- fresh
        emit ("  " <> p <> " = getelementptr i64, ptr %args, i64 " <> show i)
        mintLoad p
    )
    (paramIndices arity)
  r <- guestDirect { dsym: l.name <> "$d", env: envw, args }
  emitGuestRet r
  wbody <- takeFn
  emitDefine
    ( "define internal i64 @" <> l.name <> "(ptr %ctx, i64 %clo, ptr %args, i64 %nargs) {\n"
        <> "entry:\n"
    )
    wbody

-- | Drain the pending-lambda queue LIFO (each `emitFunction` may enqueue more), stack-safely.
emitPending :: Codegen Unit
emitPending = tailRecM go unit
  where
  go _ = do
    pending <- gets _.pending
    case pending of
      Nil -> pure (Done unit)
      Cons l rest -> do
        modify_ \c -> c { pending = rest }
        emitFunction l
        pure (Loop unit)
