-- | The recursive ANF → `.ll` lowering: atoms, computations, expressions, and the two-entry lifted
-- | function emission (ADR-0072 §4, ADR-0076 §1). A faithful transcription of boot's `codegen_llvm.ml`
-- | (`atom`/`read_var`/`expr`/`cexpr`/`emit_ret`/`make_closure`/`arg_buffer`/`lift`/`emit_function`/
-- | `emit_pending`), byte-identical to boot's `.ll` (ADR-0082 §2).
-- |
-- | Stack-safety (maintainer-flagged): the deep-linear ANF `Let`/`LetRec` spine is walked with
-- | `tailRecM` (`State` is `MonadRec`), and `emitPending` drains its LIFO queue with `tailRecM` too, so
-- | neither grows the JS stack with binding/emission depth. Tree recursion (`if`/`case` branches) stays
-- | ordinary, bounded by control-flow nesting.
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
import Data.Array (range, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldM, foldMap, foldl, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (abiFrameOpen, abiGet, abiPopFrame, abiRoot, abiSettle)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (ctorTag, escapeStringBytes, imm, immBool, immInt, immUnit, labelId, mangle, mangleForeign, sortRecordFields)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, beginFn, emit, emitGlobal, emitModule, fresh, freshFn, freshLabel, freshStrName, getFrame, setFrame, takeFn)
import Purvasm.Compiler.Backend.LLVM.Prim (inlinePrim, primSym)
import Purvasm.Compiler.Backend.LLVM.Types (Env, EnvSrc(..), FnInfo, Lifted(..), LiftedBody(..), bindFnVar, bindVar, lookupEnv)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Purvasm.Compiler.MiddleEnd.MatchCompile (DTree(..), Proj(..)) as MC
import Purvasm.Compiler.MiddleEnd.MatchCompile (compile) as MatchCompile
import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)
import Purvasm.Number (floatBitsHi, floatBitsLo)

-- | Root a freshly produced value and return its handle (root-on-create).
root :: String -> Codegen String
root = abiRoot

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
-- | constants at case entry is byte-identity-relevant.
sortUniqBoxed :: Array Literal -> Array Literal
sortUniqBoxed = Array.nubByEq (\a b -> compareBoxed a b == EQ) <<< Array.sortBy compareBoxed

-- | An atom to its i64 operand. `AtomVar` reloads through its root handle (or a global's `$root`);
-- | `Int`/`Bool` literals are immediates (no emission).
atom :: Env -> Atom -> Codegen String
atom env = case _ of
  AtomVar x -> readVar env x
  AtomLit (LInt n) -> pure (immInt n)
  AtomLit (LBool b) -> pure (immBool b)
  AtomLit (LNumber f) -> do
    -- Boxed `Number` (ADR-0064 §1): pass the IEEE-754 bit pattern as the i64 payload.
    t <- fresh
    emit ("  " <> t <> " = call i64 @pv_new_number(ptr %ctx, i64 " <> int64BitsDecimal { hi: floatBitsHi f, lo: floatBitsLo f } <> ")")
    pure t
  AtomLit (LString s) -> do
    Tuple p len <- stringConstant s
    t <- fresh
    emit ("  " <> t <> " = call i64 @pv_new_str(ptr %ctx, ptr " <> p <> ", i64 " <> show len <> ")")
    pure t
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
    clo <- fresh
    emit ("  " <> clo <> " = call i64 @pv_make_closure(ptr %ctx, i64 " <> addr <> ", i32 " <> show arity <> ", i64 " <> immUnit <> ")")
    pure clo

-- | Materialise a string literal as a module-level `@.str.N` byte constant (boot's `string_constant`),
-- | returning the `getelementptr`-to-first-byte pointer operand and the byte length. An empty string is
-- | a null pointer of length 0 (no constant emitted, matching boot's early return).
stringConstant :: String -> Codegen (Tuple String Int)
stringConstant s =
  let
    { escaped, len } = escapeStringBytes s
  in
    if len == 0 then pure (Tuple "null" 0)
    else do
      name <- freshStrName
      emitGlobal (name <> " = private unnamed_addr constant [" <> show len <> " x i8] c\"" <> escaped <> "\"\n")
      p <- fresh
      emit ("  " <> p <> " = getelementptr [" <> show len <> " x i8], ptr " <> name <> ", i64 0, i64 0")
      pure (Tuple p len)

-- | Read a variable's current value (post-safepoint): a local reloads via its root handle
-- | (`pv_get`); a top-level global loads its `@<mangle>$root` handle then reloads that (ADR-0072 §2/§3).
readVar :: Env -> String -> Codegen String
readVar env x = case lookupEnv x env of
  Just entry -> abiGet entry.handle
  Nothing -> do
    gkeys <- gets _.gkeys
    if Set.member x gkeys then do
      modify_ \c -> c { externs = Set.insert x c.externs }
      handle <- fresh
      emit ("  " <> handle <> " = load i64, ptr @" <> mangle x <> "$root")
      abiGet handle
    else
      unsafeCrashWith ("Backend.LLVM.Emit.readVar: unbound variable " <> x <> " (unresolved foreign?)")

-- | Read the current value of a rooted handle (post-safepoint).
getCurrent :: String -> Codegen String
getCurrent = abiGet

-- | Force a value iff it is a by-need cell, with the immediate fast path inline (ADR-0072 §6): an
-- | immediate word (LSB set) can never be a cell, so the extern crossing is paid only for pointer words.
-- | The slow path (`pv_force_if_byneed`) is the only safepoint.
forceValue :: String -> Codegen String
forceValue v = do
  chk <- freshLabel "fchk"
  slow <- freshLabel "fslow"
  done_ <- freshLabel "fdone"
  emit ("  br label %" <> chk)
  emit (chk <> ":")
  bit <- fresh
  emit ("  " <> bit <> " = and i64 " <> v <> ", 1")
  imm <- fresh
  emit ("  " <> imm <> " = icmp ne i64 " <> bit <> ", 0")
  emit ("  br i1 " <> imm <> ", label %" <> done_ <> ", label %" <> slow)
  emit (slow <> ":")
  forced <- fresh
  emit ("  " <> forced <> " = call i64 @pv_force_if_byneed(ptr %ctx, i64 " <> v <> ")")
  emit ("  br label %" <> done_)
  emit (done_ <> ":")
  r <- fresh
  emit ("  " <> r <> " = phi i64 [ " <> v <> ", %" <> chk <> " ], [ " <> forced <> ", %" <> slow <> " ]")
  pure r

-- | An atom to its forced value: a variable is forced (a demand site, e.g. an `if` condition or a primop
-- | operand); a literal/foreign is passed through unforced (never a cell).
forceAtom :: Env -> Atom -> Codegen String
forceAtom env = case _ of
  a@(AtomVar _) -> atom env a >>= forceValue
  a -> atom env a

-- | Evaluate a list of atoms to their **current** value operands, mutually protected against each
-- | other's safepoints (ADR-0072 §6): an atom is rooted+reloaded only when a *later* atom can safepoint,
-- | so the common all-vars/immediates list emits no `pv_root`/`pv_get`. `force` forces each variable
-- | (a suspension may run guest code — itself a safepoint). Byte-identical to boot's `eval_atoms`: the
-- | evaluation+root pass runs in list order, then the reload (`pv_get`) pass.
evalAtoms :: Boolean -> Env -> Array Atom -> Codegen (Array String)
evalAtoms force env atoms = do
  slots <- go (List.fromFoldable atoms)
  reloaded <- traverse reload slots
  pure (Array.fromFoldable reloaded)
  where
  canSafepoint = case _ of
    AtomLit (LInt _) -> false
    AtomLit (LBool _) -> false
    AtomLit (LNumber _) -> true
    AtomLit (LString _) -> true
    AtomForeign _ -> true
    AtomVar _ -> force

  isImmediate = case _ of
    AtomLit (LInt _) -> true
    AtomLit (LBool _) -> true
    _ -> false

  one = if force then forceAtom env else atom env

  -- Evaluate+root in list order; `Left` = raw operand, `Right` = rooted handle (reloaded in a second
  -- pass), mirroring boot's ``` `Raw / `Rooted ``` split.
  go = case _ of
    Nil -> pure Nil
    Cons a rest -> do
      slot <-
        if isImmediate a then Left <$> one a
        else do
          v <- one a
          if any canSafepoint rest then Right <$> root v else pure (Left v)
      slots <- go rest
      pure (Cons slot slots)

  reload = case _ of
    Left v -> pure v
    Right h -> getCurrent h

-- | Expect a produced value from a non-tail sub-expression (an `if`/`let` branch always yields one).
requireValue :: Maybe String -> Codegen String
requireValue = case _ of
  Just v -> pure v
  Nothing -> unsafeCrashWith "Backend.LLVM.Emit: non-tail expression produced no value"

-- | Emit the current function's return: pop the shadow-stack frame, then `ret`.
emitRet :: String -> Codegen Unit
emitRet v = do
  frame <- getFrame
  abiPopFrame frame
  emit ("  ret i64 " <> v)

-- | Finish a produced value in the current tail context: in tail position emit the `ret` (and produce
-- | no value); otherwise hand the operand back.
finish :: Boolean -> String -> Codegen (Maybe String)
finish tail v =
  if tail then do
    emitRet v
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
          , entry.handle == h0
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
cexpr :: Env -> Boolean -> CExpr -> Codegen (Maybe String)
cexpr env tail = case _ of
  CAtom a -> atom env a >>= finish tail
  -- GER run point (ADR-0099): `perform t ≃ t unit`. Delegate to the `CApp` path so the direct /
  -- `musttail` / generic `pv_apply` machinery (and tail position) is reused unchanged.
  CPerform t -> cexpr env tail (CApp t [ AtomLit (LInt 0) ])
  CPrim op args -> do
    -- A primop consumes its operands' *values* (e.g. `RecordGet` on a by-need dict), so force them.
    ops <- evalAtoms true env args
    inlinePrim op ops >>= case _ of
      Just t -> finish tail t
      Nothing -> do
        let
          Tuple sym needsCtx = primSym op
          argl = joinWith ", " (map (\o -> "i64 " <> o) ops)
        t <- fresh
        if needsCtx then emit ("  " <> t <> " = call i64 @" <> sym <> "(ptr %ctx, " <> argl <> ")")
        else emit ("  " <> t <> " = call i64 @" <> sym <> "(" <> argl <> ")")
        finish tail t
  CIf a t e -> do
    -- A Boolean demand site: force a by-need cell reaching the condition before reading its payload bit.
    c <- forceAtom env a
    -- payload != 0 ⇒ true (ADR-0064 §1).
    p <- fresh
    emit ("  " <> p <> " = ashr i64 " <> c <> ", 1")
    b <- fresh
    emit ("  " <> b <> " = icmp ne i64 " <> p <> ", 0")
    lt <- freshLabel "then"
    le <- freshLabel "else"
    emit ("  br i1 " <> b <> ", label %" <> lt <> ", label %" <> le)
    if tail then do
      emit (lt <> ":")
      void (expr env true t)
      emit (le <> ":")
      void (expr env true e)
      pure Nothing
    else do
      lend <- freshLabel "endif"
      emit (lt <> ":")
      vt <- requireValue =<< expr env false t
      -- the block a value flows from may differ from `lt` after nested control flow.
      bt <- freshLabel "thenv"
      emit ("  br label %" <> bt)
      emit (bt <> ":")
      emit ("  br label %" <> lend)
      emit (le <> ":")
      ve <- requireValue =<< expr env false e
      be <- freshLabel "elsev"
      emit ("  br label %" <> be)
      emit (be <> ":")
      emit ("  br label %" <> lend)
      emit (lend <> ":")
      r <- fresh
      emit ("  " <> r <> " = phi i64 [ " <> vt <> ", %" <> bt <> " ], [ " <> ve <> ", %" <> be <> " ]")
      pure (Just r)
  CLam ps body -> do
    l <- lift env ps body
    makeClosure env l >>= finish tail
  CApp f args -> do
    directTarget env f (Array.length args) >>= case _ of
      Just info -> do
        -- Direct known-arity call (ADR-0076 §2/§3): the env word is derived per the callee's shape;
        -- a cell force is a safepoint (the suspension may run guest code), so operands are re-read
        -- from their roots after it.
        Tuple envOp ops <- case info.src of
          SSelf -> do
            sc <- gets _.selfCtx
            envH <- case sc of
              Just s -> pure s.envHandle
              Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: self-call outside a self context"
            ops <- evalAtoms false env args
            envOp <- getCurrent envH
            pure (Tuple envOp ops)
          SSentinel -> do
            ops <- evalAtoms false env args
            pure (Tuple immUnit ops)
          SClosureEnv -> do
            all <- evalAtoms false env (Array.cons f args)
            case Array.uncons all of
              Just { head: fv, tail: ops } -> do
                e <- fresh
                emit ("  " <> e <> " = call i64 @pv_read_field(ptr %ctx, i64 " <> fv <> ", i64 2)")
                pure (Tuple e ops)
              Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: empty CApp operand list"
          SForceCell -> do
            fh <- atom env f >>= root
            argHs <- traverse (\a -> atom env a >>= root) args
            -- `forced` is numbered before `getCurrent fh`'s load chain (boot: `let forced = fresh in
            -- emit … forced (get_current fh)` — OCaml right-to-left arg eval emits the load chain first
            -- but numbers `forced` first).
            forced <- fresh
            fhc <- getCurrent fh
            emit ("  " <> forced <> " = call i64 @pv_force_if_byneed(ptr %ctx, i64 " <> fhc <> ")")
            e <- fresh
            emit ("  " <> e <> " = call i64 @pv_read_field(ptr %ctx, i64 " <> forced <> ", i64 2)")
            ops <- traverse getCurrent argHs
            pure (Tuple e ops)
        let arglist = foldMap (\o -> ", i64 " <> o) ops
        inDir <- gets _.inDirect
        if tail && inDir then do
          -- musttail (ADR-0076 §3): pop this frame first — the callee opens its own — with every
          -- operand (env word included) computed before the pop; no safepoint in between.
          frame <- getFrame
          abiPopFrame frame
          r <- fresh
          emit ("  " <> r <> " = musttail call tailcc i64 @" <> info.dsym <> "(ptr %ctx, i64 " <> envOp <> arglist <> ")")
          emit ("  ret i64 " <> r)
          pure Nothing
        else do
          r <- fresh
          emit ("  " <> r <> " = call tailcc i64 @" <> info.dsym <> "(ptr %ctx, i64 " <> envOp <> arglist <> ")")
          -- Settle (ADR-0076 §3): the callee may have stashed a generic tail bounce no enclosing
          -- `pv_apply` loop will take on this direct path — run it to a real value.
          r' <- abiSettle r
          if tail then do
            emitRet r'
            pure Nothing
          else pure (Just r')
      Nothing -> do
        -- `f` and the args are mutually protected: a foreign callee or a `String` arg may allocate.
        all <- evalAtoms false env (Array.cons f args)
        case Array.uncons all of
          Just { head: fv, tail: ops } ->
            if tail then do
              -- Trampoline tail call (ADR-0071 §4): stash the pending tail, pop this frame, return.
              Tuple p n <- argBuffer ops
              emit ("  call void @pv_tailcall(ptr %ctx, i64 " <> fv <> ", ptr " <> p <> ", i64 " <> show n <> ")")
              frame <- getFrame
              abiPopFrame frame
              emit ("  ret i64 " <> immUnit)
              pure Nothing
            else do
              Tuple p n <- argBuffer ops
              t <- fresh
              emit ("  " <> t <> " = call i64 @pv_apply(ptr %ctx, i64 " <> fv <> ", ptr " <> p <> ", i64 " <> show n <> ")")
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
        if nargs == 0 then finish tail builder
        else do
          bh <- root builder
          ops <- evalAtoms false env args
          Tuple p n <- argBuffer ops
          bv <- getCurrent bh
          t <- fresh
          emit ("  " <> t <> " = call i64 @pv_apply(ptr %ctx, i64 " <> bv <> ", ptr " <> p <> ", i64 " <> show n <> ")")
          finish tail t
      else if arity == 0 then
        -- nullary → an immediate tag (ADR-0064 §1).
        finish tail (imm (ctorTag name))
      else do
        ops <- evalAtoms false env args
        Tuple p n <- argBuffer ops
        t <- fresh
        emit ("  " <> t <> " = call i64 @pv_new_adt(ptr %ctx, i32 " <> show (ctorTag name) <> ", ptr " <> p <> ", i64 " <> show n <> ")")
        finish tail t
  CArray elems ->
    if Array.null elems then do
      t <- fresh
      emit ("  " <> t <> " = call i64 @pv_empty_array()")
      finish tail t
    else do
      ops <- evalAtoms false env elems
      Tuple p n <- argBuffer ops
      t <- fresh
      emit ("  " <> t <> " = call i64 @pv_new_array(ptr %ctx, ptr " <> p <> ", i64 " <> show n <> ")")
      finish tail t
  CRecord fields ->
    -- Hash each label, sort by unsigned id ascending (ADR-0069 §1), pass parallel id/value buffers.
    let
      sorted = sortRecordFields (map (\f -> Tuple f.prop f.val) fields)
      n = Array.length sorted
    in
      if n == 0 then do
        t <- fresh
        emit ("  " <> t <> " = call i64 @pv_new_record(ptr %ctx, ptr null, ptr null, i64 0)")
        finish tail t
      else do
        let ids = map (\(Tuple l _) -> labelId l) sorted
        -- Values are mutually protected: a later `String`/`Number` field must not stale an earlier one.
        vals <- evalAtoms false env (map snd sorted)
        Tuple idp _ <- argBuffer ids
        Tuple valp _ <- argBuffer vals
        t <- fresh
        emit ("  " <> t <> " = call i64 @pv_new_record(ptr %ctx, ptr " <> idp <> ", ptr " <> valp <> ", i64 " <> show n <> ")")
        finish tail t
  CAccessor a label -> do
    -- A dictionary projection (ADR-0070 §5): force a by-need record before reading its field.
    r <- forceAtom env a
    t <- fresh
    emit ("  " <> t <> " = call i64 @pv_record_get(ptr %ctx, i64 " <> r <> ", i64 " <> labelId label <> ")")
    finish tail t
  CUpdate a ups -> do
    -- Functional update: fold `record_set` (each returns a new record). The base is forced (a by-need
    -- dict update); the accumulator is rooted across each value's evaluation and reloaded before the set.
    rh0 <- forceAtom env a >>= root
    rhFinal <- foldM
      ( \rh up -> do
          v <- atom env up.val
          r <- getCurrent rh
          t <- fresh
          emit ("  " <> t <> " = call i64 @pv_record_set(ptr %ctx, i64 " <> r <> ", i64 " <> labelId up.prop <> ", i64 " <> v <> ")")
          root t
      )
      rh0
      ups
    getCurrent rhFinal >>= finish tail
  CCase scruts alts -> do
    -- The shared Maranget decision tree (ADR-0083) lowered to LLVM. Occurrences — scrutinees and every
    -- extracted sub-value — are rooted and re-read (`getCurrent`): the tree shares sub-occurrences
    -- across rows, and a guarded row's fall-through reuses them after its guard body may have
    -- safepointed. Boxed literals are pre-rooted once at entry so the tree walk never allocates.
    let { scrutBinds, tree } = MatchCompile.compile scruts alts
    -- Root each scrutinee (forced — matching dereferences its structure).
    occEnv0 <- traverse (\(Tuple occ a) -> Tuple occ <$> (forceAtom env a >>= root)) scrutBinds
    -- Hoist + root every boxed literal any arm compares against.
    litEnv <- traverse (\l -> Tuple l <$> (atom env (AtomLit l) >>= root))
      (sortUniqBoxed (Array.concatMap (\alt -> Array.concatMap binderBoxedLits alt.binders) alts))
    failLabel <- freshLabel "nomatch"
    merge <- if tail then pure "" else freshLabel "casejoin"
    let
      lookupOcc oenv occ = case Array.find (\(Tuple k _) -> k == occ) oenv of
        Just (Tuple _ h) -> h
        Nothing -> unsafeCrashWith ("Backend.LLVM.Emit.cexpr: unbound case occurrence " <> occ)

      cur oenv occ = getCurrent (lookupOcc oenv occ)

      boxedHandle l = case Array.find (\(Tuple k _) -> k == l) litEnv of
        Just (Tuple _ h) -> h
        Nothing -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: boxed literal not hoisted"

      -- Read a sub-value raw (allocation-free), then root it — extending `oenv` (most-recent first).
      extract oenv parent (Tuple subOcc pr) = do
        raw <- fresh
        parentCur <- cur oenv parent
        case pr of
          MC.Pfield j ->
            emit ("  " <> raw <> " = call i64 @pv_read_field(ptr %ctx, i64 " <> parentCur <> ", i64 " <> show (1 + j) <> ")")
          MC.Pelem j ->
            emit ("  " <> raw <> " = call i64 @pv_prim_index_array(ptr %ctx, i64 " <> parentCur <> ", i64 " <> immInt j <> ")")
          MC.Precord l ->
            emit ("  " <> raw <> " = call i64 @pv_record_get(ptr %ctx, i64 " <> parentCur <> ", i64 " <> labelId l <> ")")
        h <- root raw
        pure (Array.cons (Tuple subOcc h) oenv)

      bindLeaf oenv binds = foldl (\e (Tuple v occ) -> bindVar e v (lookupOcc oenv occ)) env binds

      -- A matched body's value reaches the phi through a fresh single-predecessor block (the CIf idiom).
      runBody env' e phis =
        if tail then do
          void (expr env' true e)
          pure phis
        else do
          v <- requireValue =<< expr env' false e
          vb <- freshLabel "altv"
          emit ("  br label %" <> vb)
          emit (vb <> ":")
          emit ("  br label %" <> merge)
          pure (Array.cons (Tuple v vb) phis)

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
                gv <- forceValue =<< requireValue =<< expr env' false clause.guard
                pay <- fresh
                emit ("  " <> pay <> " = ashr i64 " <> gv <> ", 1")
                bb <- fresh
                emit ("  " <> bb <> " = icmp ne i64 " <> pay <> ", 0")
                yes <- freshLabel "gyes"
                no <- freshLabel "gno"
                emit ("  br i1 " <> bb <> ", label %" <> yes <> ", label %" <> no)
                emit (yes <> ":")
                acc' <- runBody env' clause.rhs acc
                emit (no <> ":")
                guards rest acc'
          guards clauses phis
        MC.DswitchCtor occ arms default -> do
          -- Dispatch by representation first (immediate nullary vs field-carrying pointer), then by tag.
          scrut <- cur oenv occ
          low <- fresh
          emit ("  " <> low <> " = and i64 " <> scrut <> ", 1")
          isImm <- fresh
          emit ("  " <> isImm <> " = icmp eq i64 " <> low <> ", 1")
          immBlk <- freshLabel "ctimm"
          ptrBlk <- freshLabel "ctptr"
          emit ("  br i1 " <> isImm <> ", label %" <> immBlk <> ", label %" <> ptrBlk)
          defaultLbl <- freshLabel "ctdef"
          armLbls <- traverse (\(Tuple tag arm) -> { tag, arm, l: _ } <$> freshLabel "ctarm") arms
          let
            casesFor keep = joinWith " "
              ( map (\a -> "i64 " <> show (ctorTag a.tag) <> ", label %" <> a.l)
                  (Array.filter (\a -> keep a.arm.extracts) armLbls)
              )
          -- immediates → the nullary (no-extract) arms, keyed by payload tag.
          emit (immBlk <> ":")
          itag <- fresh
          emit ("  " <> itag <> " = ashr i64 " <> scrut <> ", 1")
          emit ("  switch i64 " <> itag <> ", label %" <> defaultLbl <> " [ " <> casesFor Array.null <> " ]")
          -- pointers → the field-carrying arms, keyed by the tag at raw word 0.
          emit (ptrBlk <> ":")
          ptag <- fresh
          emit ("  " <> ptag <> " = call i64 @pv_read_raw(ptr %ctx, i64 " <> scrut <> ", i64 0)")
          emit ("  switch i64 " <> ptag <> ", label %" <> defaultLbl <> " [ " <> casesFor (not <<< Array.null) <> " ]")
          phis' <- foldM
            ( \acc a -> do
                emit (a.l <> ":")
                oenv' <- foldM (\oe ex -> extract oe occ ex) oenv a.arm.extracts
                lower oenv' a.arm.sub acc
            )
            phis
            armLbls
          emit (defaultLbl <> ":")
          lower oenv default phis'
        MC.DswitchLit occ arms default -> case Array.uncons arms of
          Nothing -> lower oenv default phis
          Just { head: Tuple headLit _ } -> case headLit of
            LNumber _ -> lowerBoxedChain oenv occ arms default phis
            LString _ -> lowerBoxedChain oenv occ arms default phis
            _ -> do
              -- Immediate literals → a direct LLVM switch on the tagged word.
              scrut <- cur oenv occ
              defaultLbl <- freshLabel "swdef"
              armLbls <- traverse (const (freshLabel "swarm")) arms
              let
                immOf = case _ of
                  LInt n -> immInt n
                  LBool b -> immBool b
                  _ -> unsafeCrashWith "Backend.LLVM.Emit.cexpr: non-immediate literal in immediate switch"
                cases = Array.zipWith (\(Tuple l _) lbl -> "i64 " <> immOf l <> ", label %" <> lbl) arms armLbls
              emit ("  switch i64 " <> scrut <> ", label %" <> defaultLbl <> " [ " <> joinWith " " cases <> " ]")
              phis' <- foldM
                ( \acc (Tuple (Tuple _ sub) lbl) -> do
                    emit (lbl <> ":")
                    lower oenv sub acc
                )
                phis
                (Array.zip arms armLbls)
              emit (defaultLbl <> ":")
              lower oenv default phis'
        MC.DswitchLen occ arms default -> do
          len <- fresh
          occCur <- cur oenv occ
          emit ("  " <> len <> " = call i64 @pv_prim_length_array(ptr %ctx, i64 " <> occCur <> ")")
          defaultLbl <- freshLabel "swdef"
          armLbls <- traverse (const (freshLabel "swarm")) arms
          let cases = Array.zipWith (\(Tuple n _) lbl -> "i64 " <> immInt n <> ", label %" <> lbl) arms armLbls
          emit ("  switch i64 " <> len <> ", label %" <> defaultLbl <> " [ " <> joinWith " " cases <> " ]")
          phis' <- foldM
            ( \acc (Tuple (Tuple _ arm) lbl) -> do
                emit (lbl <> ":")
                oenv' <- foldM (\oe ex -> extract oe occ ex) oenv arm.extracts
                lower oenv' arm.sub acc
            )
            phis
            (Array.zip arms armLbls)
          emit (defaultLbl <> ":")
          lower oenv default phis'
        MC.DexpandRecord occ extracts sub -> do
          oenv' <- foldM (\oe ex -> extract oe occ ex) oenv extracts
          lower oenv' sub phis

      -- Boxed literals cannot be `switch`ed — an equality chain against the hoisted rooted handles,
      -- via the runtime `Eq` primop (the tested source of truth for IEEE / bytes).
      lowerBoxedChain oenv occ arms default phis = case Array.uncons arms of
        Nothing -> lower oenv default phis
        Just { head: Tuple l sub, tail: rest } -> do
          let
            prim = case l of
              LNumber _ -> "pv_prim_eq_number"
              _ -> "pv_prim_eq_string"
          eq <- fresh
          boxed <- getCurrent (boxedHandle l)
          scrut <- cur oenv occ
          emit ("  " <> eq <> " = call i64 @" <> prim <> "(ptr %ctx, i64 " <> scrut <> ", i64 " <> boxed <> ")")
          pay <- fresh
          emit ("  " <> pay <> " = ashr i64 " <> eq <> ", 1")
          ok <- fresh
          emit ("  " <> ok <> " = icmp ne i64 " <> pay <> ", 0")
          armLbl <- freshLabel "ltarm"
          next <- freshLabel "ltnext"
          emit ("  br i1 " <> ok <> ", label %" <> armLbl <> ", label %" <> next)
          emit (armLbl <> ":")
          phis' <- lower oenv sub phis
          emit (next <> ":")
          lowerBoxedChain oenv occ rest default phis'
    phis <- lower occEnv0 tree []
    emit (failLabel <> ":")
    emit "  call void @pv_case_fail()"
    emit "  unreachable"
    if tail then pure Nothing
    -- A non-tail `case` produces a value through the phi, so at least one arm body must reach `merge`
    -- (`phis` non-empty). A wholly-`Dfail` tree (no `Dleaf`/`Dguard` body — only reachable for a source
    -- `case` with no alternatives, which the surface never produces) would emit an entry-less `phi i64`
    -- (invalid LLVM); pin the invariant rather than emit it.
    else if Array.null phis then
      unsafeCrashWith "Backend.LLVM.Emit.cexpr: non-tail CCase reached no arm body (all-fail decision tree)"
    else do
      emit (merge <> ":")
      r <- fresh
      let entries = joinWith ", " (map (\(Tuple v b) -> "[ " <> v <> ", %" <> b <> " ]") (Array.reverse phis))
      emit ("  " <> r <> " = phi i64 " <> entries)
      pure (Just r)

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
buildGrec :: (String -> Maybe String) -> Env -> Array (Tuple String Expr) -> Codegen Env
buildGrec named env binds = do
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
  memberFns0 <- map Array.catMaybes $ traverse
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
  traverse_ (\r -> pushPending r.lifted) memberFns
  -- One suspension per member over the shared env: a function member's suspension builds its
  -- pre-lifted closure; any other member's suspension evaluates its RHS.
  suspNames <- traverse
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
  outsideVals <- traverse (readVar env) outside
  let elems = Array.replicate k immUnit <> outsideVals
  Tuple envP envN <- argBuffer elems
  envArr <- fresh
  emit ("  " <> envArr <> " = call i64 @pv_new_array(ptr %ctx, ptr " <> envP <> ", i64 " <> show envN <> ")")
  envH <- root envArr
  -- 2. placeholder cells; store each into env[i] (reloading env/cell after each allocation).
  cellHs <- traverse
    ( \i -> do
        cell <- fresh
        emit ("  " <> cell <> " = call i64 @pv_new_byneed_placeholder(ptr %ctx)")
        ch <- root cell
        envp <- getCurrent envH
        cw <- getCurrent ch
        emit ("  call void @pv_write_field(ptr %ctx, i64 " <> envp <> ", i64 " <> show i <> ", i64 " <> cw <> ")")
        pure ch
    )
    (paramIndices k)
  -- 3. build each suspension closure over the shared env; backpatch it into its cell.
  traverse_
    ( \(Tuple name ch) -> do
        envp <- getCurrent envH
        addr <- fresh
        emit ("  " <> addr <> " = ptrtoint ptr @" <> name <> " to i64")
        susp <- fresh
        emit ("  " <> susp <> " = call i64 @pv_make_closure(ptr %ctx, i64 " <> addr <> ", i32 1, i64 " <> envp <> ")")
        cellp <- getCurrent ch
        emit ("  call void @pv_byneed_set_suspension(ptr %ctx, i64 " <> cellp <> ", i64 " <> susp <> ")")
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
expr :: Env -> Boolean -> Expr -> Codegen (Maybe String)
expr env0 tail = tailRecM step <<< Tuple env0
  where
  step (Tuple env e) = case e of
    Ret c -> Done <$> cexpr env tail c
    Let x c body -> case c of
      -- A let-bound lambda is a direct-call candidate (ADR-0076 §2): keep its lifted identity on the
      -- binding so saturated calls skip the generic dispatch. Non-recursive, so no self.
      CLam ps lbody -> do
        l <- lift env ps lbody
        let Lifted lr = l
        v <- makeClosure env l
        h <- root v
        let
          info =
            { dsym: lr.name <> "$d"
            , arity: Array.length ps
            , src: if Array.null lr.captures then SSentinel else SClosureEnv
            }
        pure (Loop (Tuple (bindFnVar env x h info) body))
      _ -> do
        mv <- cexpr env false c
        case mv of
          Just v -> do
            h <- root v
            pure (Loop (Tuple (bindVar env x h) body))
          Nothing ->
            unsafeCrashWith "Backend.LLVM.Emit.expr: non-tail cexpr produced no value"
    LetRec binds body -> do
      env' <- buildGrec (const Nothing) env (map (\r -> Tuple r.var r.rhs) binds)
      pure (Loop (Tuple env' body))

-- | Materialise an i64 arg buffer for a call (an `alloca` holding the operands), returning the pointer
-- | operand and count. Zero args → a null pointer.
argBuffer :: Array String -> Codegen (Tuple String Int)
argBuffer operands =
  let
    n = Array.length operands
  in
    if n == 0 then pure (Tuple "null" 0)
    else do
      buf <- fresh
      emit ("  " <> buf <> " = alloca [" <> show n <> " x i64]")
      forWithIndex_ operands \i v -> do
        p <- fresh
        emit ("  " <> p <> " = getelementptr [" <> show n <> " x i64], ptr " <> buf <> ", i64 0, i64 " <> show i)
        emit ("  store i64 " <> v <> ", ptr " <> p)
      p0 <- fresh
      emit ("  " <> p0 <> " = getelementptr [" <> show n <> " x i64], ptr " <> buf <> ", i64 0, i64 0")
      pure (Tuple p0 n)

-- | Build a closure value for a lifted lambda: assemble the captured-env array from the current values
-- | of its free variables, then `pv_make_closure` over the lifted function's address.
makeClosure :: Env -> Lifted -> Codegen String
makeClosure env (Lifted l) = do
  envWord <- case l.captures of
    [] -> pure immUnit
    caps -> do
      vals <- traverse (readVar env) caps
      Tuple p n <- argBuffer vals
      arr <- fresh
      emit ("  " <> arr <> " = call i64 @pv_new_array(ptr %ctx, ptr " <> p <> ", i64 " <> show n <> ")")
      pure arr
  addr <- fresh
  emit ("  " <> addr <> " = ptrtoint ptr @" <> l.name <> " to i64")
  clo <- fresh
  emit ("  " <> clo <> " = call i64 @pv_make_closure(ptr %ctx, i64 " <> addr <> ", i32 " <> show (Array.length l.params) <> ", i64 " <> envWord <> ")")
  pure clo

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
  frame <- abiFrameOpen
  setFrame frame
  handles <- traverseWithIndex (\i _ -> root ("%p" <> show i)) l.params
  let env1 = foldl (\env (Tuple p h) -> bindVar env p h) Nil (zip l.params handles)
  -- captures: positional reads from the env word `%env` (the shared/captured array); a capture that is
  -- a known recursive-group function member carries its direct-call info (`captureFns`). `selfHandle`
  -- is the handle of the captured self binding (a member calling itself through its capture).
  let
    stepCap (Tuple env sh) (Tuple i c) = do
      v <- fresh
      emit ("  " <> v <> " = call i64 @pv_read_field(ptr %ctx, i64 %env, i64 " <> show i <> ")")
      h <- root v
      let sh' = if Just c == l.selfName then Just h else sh
      let
        env' = case Array.find (\(Tuple k _) -> k == c) l.captureFns of
          Just (Tuple _ info) -> bindFnVar env c h info
          Nothing -> bindVar env c h
      pure (Tuple env' sh')
  Tuple env2 selfHandle <- case l.captures of
    [] -> pure (Tuple env1 Nothing)
    _ -> foldM stepCap (Tuple env1 Nothing) (Array.mapWithIndex Tuple l.captures)
  -- the self-call shortcut (ADR-0076 §2): while this body runs, a saturated call to `selfName`
  -- re-enters this very function with this very `%env`. Root the env word itself — a self-call
  -- re-supplies it, and the raw `%env` SSA value is stale after the first safepoint (the array moves).
  savedSelf <- gets _.selfCtx
  savedDirect <- gets _.inDirect
  case l.selfName of
    Just nm -> do
      envH <- root "%env"
      modify_ \c -> c
        { selfCtx = Just
            { name: nm
            , captureHandle: selfHandle
            , envHandle: envH
            , fnInfo: { dsym: l.name <> "$d", arity, src: SSelf }
            }
        }
    Nothing -> modify_ \c -> c { selfCtx = Nothing }
  modify_ \c -> c { inDirect = true }
  case l.body of
    LBody e -> void (expr env2 true e)
    LClosure lm -> do
      clo <- makeClosure env2 lm
      emitRet clo
  modify_ \c -> c { selfCtx = savedSelf, inDirect = savedDirect }
  body <- takeFn
  let
    linkage = if l.exported then "" else "internal "
    dparams = foldMap (\i -> ", i64 %p" <> show i) (paramIndices arity)
  emitModule
    ( "define " <> linkage <> "tailcc i64 @" <> l.name <> "$d(ptr %ctx, i64 %env" <> dparams <> ") {\n"
        <> "entry:\n"
        <> body
        <> "}\n\n"
    )
  -- generic wrapper
  beginFn
  envw <- case l.captures of
    [] -> pure immUnit
    _ -> do
      e <- fresh
      emit ("  " <> e <> " = call i64 @pv_read_field(ptr %ctx, i64 %clo, i64 2)")
      pure e
  args <- traverse
    ( \i -> do
        p <- fresh
        emit ("  " <> p <> " = getelementptr i64, ptr %args, i64 " <> show i)
        v <- fresh
        emit ("  " <> v <> " = load i64, ptr " <> p)
        pure v
    )
    (paramIndices arity)
  r <- fresh
  emit ("  " <> r <> " = call tailcc i64 @" <> l.name <> "$d(ptr %ctx, i64 " <> envw <> foldMap (\a -> ", i64 " <> a) args <> ")")
  emit ("  ret i64 " <> r)
  wbody <- takeFn
  emitModule
    ( "define internal i64 @" <> l.name <> "(ptr %ctx, i64 %clo, ptr %args, i64 %nargs) {\n"
        <> "entry:\n"
        <> wbody
        <> "}\n\n"
    )

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
