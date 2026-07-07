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
-- | This is the slice-1a cut (ADR-0082 §3): atoms (`AtomVar`/`Int`/`Bool`), `Ret`/`Let`, `CAtom`, and
-- | the function machinery. Arithmetic/`if`/`CApp`/closures/`case`/`Number`/`String`/foreign are later
-- | slices and currently crash with a labelled `unsafeCrashWith` rather than emit wrong IR.
module Purvasm.Compiler.Backend.LLVM.Emit
  ( atom
  , readVar
  , emitRet
  , cexpr
  , expr
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
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Backend.LLVM.Abi (abiFrameOpen, abiGet, abiPopFrame, abiRoot)
import Purvasm.Compiler.Backend.LLVM.FreeVars (fvExpr)
import Purvasm.Compiler.Backend.LLVM.Mangle (immBool, immInt, immUnit, mangle)
import Purvasm.Compiler.Backend.LLVM.Monad (Codegen, beginFn, emit, emitModule, fresh, freshFn, getFrame, setFrame, takeFn)
import Purvasm.Compiler.Backend.LLVM.Types (Env, Lifted(..), LiftedBody(..), bindVar, lookupEnv)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))

-- | Root a freshly produced value and return its handle (root-on-create).
root :: String -> Codegen String
root = abiRoot

-- | The 0-based indices `[0 .. arity-1]` (empty when `arity = 0`, unlike `range 0 (arity-1)`).
paramIndices :: Int -> Array Int
paramIndices arity = if arity <= 0 then [] else range 0 (arity - 1)

-- | An atom to its i64 operand. `AtomVar` reloads through its root handle (or a global's `$root`);
-- | `Int`/`Bool` literals are immediates (no emission).
atom :: Env -> Atom -> Codegen String
atom env = case _ of
  AtomVar x -> readVar env x
  AtomLit (LInt n) -> pure (immInt n)
  AtomLit (LBool b) -> pure (immBool b)
  AtomLit (LNumber _) ->
    unsafeCrashWith "Backend.LLVM.Emit.atom: Number literal not yet supported (slice 1b)"
  AtomLit (LString _) ->
    unsafeCrashWith "Backend.LLVM.Emit.atom: String literal not yet supported (slice 3)"
  AtomForeign _ ->
    unsafeCrashWith "Backend.LLVM.Emit.atom: foreign reference not yet supported (slice 4)"

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

-- | Lower a computation. Slice-1a handles `CAtom`; the rest are later slices.
cexpr :: Env -> Boolean -> CExpr -> Codegen (Maybe String)
cexpr env tail = case _ of
  CAtom a -> atom env a >>= finish tail
  CPrim _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CPrim not yet supported (slice 1b)"
  CIf _ _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CIf not yet supported (slice 1b)"
  CApp _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CApp not yet supported (slice 1b)"
  CLam _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CLam not yet supported (slice 2)"
  CCtor _ _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CCtor not yet supported (slice 3)"
  CArray _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CArray not yet supported (slice 3)"
  CRecord _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CRecord not yet supported (slice 3)"
  CAccessor _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CAccessor not yet supported (slice 3)"
  CUpdate _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CUpdate not yet supported (slice 3)"
  CCase _ _ ->
    unsafeCrashWith "Backend.LLVM.Emit.cexpr: CCase not yet supported (slice 3)"

-- | Lower an expression. The `Let`/`LetRec` spine is walked with `tailRecM` (stack-safe); the tail flag
-- | is constant along the spine and applies to the final `Ret`.
expr :: Env -> Boolean -> Expr -> Codegen (Maybe String)
expr env0 tail = tailRecM step <<< Tuple env0
  where
  step (Tuple env e) = case e of
    Ret c -> Done <$> cexpr env tail c
    Let x c body -> case c of
      CLam _ _ ->
        unsafeCrashWith "Backend.LLVM.Emit.expr: let-bound lambda not yet supported (slice 2)"
      _ -> do
        mv <- cexpr env false c
        case mv of
          Just v -> do
            h <- root v
            pure (Loop (Tuple (bindVar env x h) body))
          Nothing ->
            unsafeCrashWith "Backend.LLVM.Emit.expr: non-tail cexpr produced no value"
    LetRec _ _ ->
      unsafeCrashWith "Backend.LLVM.Emit.expr: LetRec not yet supported (slice 2)"

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
  case l.captures of
    [] -> pure unit
    _ ->
      unsafeCrashWith "Backend.LLVM.Emit.emitFunction: captures not yet supported (slice 2)"
  case l.selfName of
    Just _ ->
      unsafeCrashWith "Backend.LLVM.Emit.emitFunction: self-recursion not yet supported (slice 2)"
    Nothing -> modify_ \c -> c { selfCtx = Nothing }
  savedDirect <- gets _.inDirect
  modify_ \c -> c { inDirect = true }
  case l.body of
    LBody e -> void (expr env1 true e)
    LClosure lm -> do
      clo <- makeClosure env1 lm
      emitRet clo
  modify_ \c -> c { inDirect = savedDirect }
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
