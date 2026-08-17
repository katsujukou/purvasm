-- | The stack interpreter ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md)),
-- | ported from boot's `Vm.Machine` with the two format changes ADR-0110 §4 makes.
-- |
-- | An explicit state machine over a heap-allocated operand stack and frame stack: the host call stack
-- | is never used for a guest call, so deep guest recursion is stack-safe and a tail call reuses the
-- | current activation (TCE). The loop itself runs under `tailRecM` for the same reason — a
-- | self-recursive `Effect` action is not a host tail call.
-- |
-- | Applications follow the uncurried eval/apply protocol (ADR-0025): saturated enters an activation,
-- | under-applied builds a partial (`VPap`/`VCtor`), over-applied saturates and applies the rest
-- | through an `ApplyMore` continuation. This is the **guest** level, and all of it is the VM's own —
-- | the runtime's `apply` enters only where a carrier does (ADR-0110 §1.1, ADR-0111 §2), which is the
-- | FFI slice's business, not this one's.
-- |
-- | The tree-shaped `case` (ADR-0110 §4(b)) is why frames come in two flavours. An arm is a *nested
-- | block*, not a jump target: entering one pushes a block frame that shares the activation's
-- | environment, and falling off its end is the arm's normal exit — which is precisely what the
-- | linear form's single end-join label existed to express. `Return` therefore unwinds block frames
-- | until it leaves the enclosing activation.
-- |
-- | `Frame` and `run` stay internal: a caller starts a program with `runBlock`, and the frame stack
-- | is the machine's own bookkeeping, not something a consumer should be able to hand-assemble.
module Purvasm.VM.Machine
  ( Env
  , force
  , newEnv
  , runBlock
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Purvasm.VM.Array as VMArray
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Instruction (CodeBlock, GuardClause, Instruction(..), Literal(..))
import Purvasm.VM.Prim as VMPrim
import Purvasm.VM.Value (Closure, Thunk(..), Value(..))

-- | What outlives a single run: the top-level table free names resolve through (which also closes
-- | recursion for top-level functions — a function finds itself there), and the instruction counter,
-- | the deterministic cost metric the optimiser is measured on (ADR-0026).
type Env =
  { globals :: Ref (Map String Value)
  , executed :: Ref Int
  }

newEnv :: Map String Value -> Effect Env
newEnv globals = { globals: _, executed: _ } <$> Ref.new globals <*> Ref.new 0

-- | A code frame: the running block, its instruction pointer, and the environment of locals.
-- |
-- | `env` is held in a `Ref` that `Bind` *repoints* to an extended map, so a closure that snapshotted
-- | the ref earlier is unaffected — immutable-environment semantics. A `share` frame is the exception
-- | recursive-group construction needs: there a closure captures the ref itself, so the knot-tying
-- | backpatch becomes visible to it. `activation` distinguishes a guest function's frame from a
-- | nested block (a `case` arm, a guard body), which shares its parent's env and whose end is an
-- | ordinary exit rather than a fault.
type CodeFrame =
  { block :: CodeBlock
  , ip :: Ref Int
  , env :: Ref (Map String Value)
  , share :: Boolean
  , activation :: Boolean
  }

-- | A guard chain in progress (ADR-0013): the clause whose guard is currently being evaluated, and
-- | where to go when it is false. Immutable — advancing the chain pushes the next one.
type GuardFrame =
  { clauses :: Array GuardClause
  , index :: Int
  , fallthrough :: CodeBlock
  , env :: Ref (Map String Value)
  , share :: Boolean
  }

data Frame
  = Code CodeFrame
  -- Apply the collected arguments to the value the frame above returns — eval/apply's
  -- "apply-the-rest" (ADR-0025).
  | ApplyMore (List Value)
  | Guard GuardFrame

-- | Force through by-need cells at every point a value's shape is inspected. Values are only
-- | *stored* without forcing, which is what lets a cyclic top-level group build.
force :: Value -> Effect Value
force value = case value of
  VThunk cell -> Ref.read cell >>= case _ of
    Built v -> force v
    Unbuilt build -> do
      Ref.write Building cell
      v <- build unit
      Ref.write (Built v) cell
      force v
    Building -> stuck "black hole: a recursive value was forced while being built"
  _ -> pure value

-- | Run a block as a top-level activation (the program entry, or one CAF), on a fresh stack.
runBlock :: Env -> CodeBlock -> Map String Value -> Effect Value
runBlock env block locals = do
  frame <- activationFrame block locals
  run env (Code frame : Nil)

activationFrame :: CodeBlock -> Map String Value -> Effect CodeFrame
activationFrame block locals = do
  ip <- Ref.new 0
  envRef <- Ref.new locals
  pure { block, ip, env: envRef, share: false, activation: true }

-- | Run a frame stack to completion on a fresh operand stack; the result is the lone value left.
-- | Re-entrant: recursive-group construction runs each member through it again.
run :: Env -> List Frame -> Effect Value
run env frames0 = do
  stack <- Ref.new (Nil :: List Value)
  frames <- Ref.new frames0

  let
    push v = Ref.modify_ (v : _) stack

    pop = Ref.read stack >>= case _ of
      v : rest -> Ref.write rest stack $> v
      Nil -> stuck "operand stack underflow"

    -- Pop n values, returned in the order they were pushed.
    popN n = go n Nil
      where
      go k acc
        | k <= 0 = pure acc
        | otherwise = pop >>= \v -> go (k - 1) (v : acc)

    pushFrame f = Ref.modify_ (f : _) frames

    popFrame = Ref.read frames >>= case _ of
      _ : rest -> Ref.write rest frames
      Nil -> stuck "frame stack underflow"

    lookupName envRef name = do
      locals <- Ref.read envRef
      case Map.lookup name locals of
        Just v -> pure v
        Nothing -> do
          globals <- Ref.read env.globals
          case Map.lookup name globals of
            Just v -> pure v
            -- The foreign frontier resolves a name the tables miss (ADR-0111); until it lands, a
            -- miss is simply unbound.
            Nothing -> stuck ("unbound variable: " <> name)

    -- Unwind to the caller: block frames (a `case` arm, a guard body) are discarded along with the
    -- activation they belong to, since `Return` returns from the function, not from the block.
    returnFromActivation = Ref.read frames >>= go
      where
      go = case _ of
        Code fr : rest
          | fr.activation -> Ref.write rest frames
          | otherwise -> go rest
        Guard _ : rest -> go rest
        _ -> stuck "return outside a function activation"

    -- A computed value is pushed; in tail position it is the activation's result, so return.
    produce tail v = do
      push v
      when tail returnFromActivation

    enterBlock envRef share block = do
      ip <- Ref.new 0
      pushFrame (Code { block, ip, env: envRef, share, activation: false })

    enterClosure tail (closure :: Closure) args = do
      locals <- Ref.read closure.env
      ip <- Ref.new 0
      envRef <- Ref.new (bindParams locals closure.params args)
      when tail returnFromActivation
      pushFrame (Code { block: closure.body, ip, env: envRef, share: false, activation: true })

    applyClosure tail closure args = do
      let np = Array.length closure.params
      let na = List.length args
      if na == np then enterClosure tail closure args
      else if na < np then produce tail (VPap closure args)
      else do
        -- Over-application: saturate, then apply the rest to the result. In tail position the
        -- current activation is abandoned first (TCE), so the continuation sits under the callee.
        when tail returnFromActivation
        pushFrame (ApplyMore (List.drop np args))
        enterClosure false closure (List.take np args)

    doCall tail f args = force f >>= case _ of
      VClosure closure -> applyClosure tail closure args
      VPap closure got -> applyClosure tail closure (got <> args)
      VCtor tag arity got -> do
        let all = got <> args
        let na = List.length all
        if na == arity then produce tail (VData tag (Array.fromFoldable all))
        else if na < arity then produce tail (VCtor tag arity all)
        else stuck ("constructor " <> tag <> " over-applied")
      VCarrier _ -> stuck "application of a native value: the FFI boundary has not landed (ADR-0111)"
      _ -> stuck "application of a non-function"

    -- Knot-tying (ADR-0030): build each member in a `share` frame over one env ref (so the members'
    -- closures capture that ref), then backpatch the ref — and the enclosing frame — with the whole
    -- group. The self/mutual references sit under those closures, so they resolve once it holds.
    makeRec fr members = do
      locals <- Ref.read fr.env
      shared <- Ref.new locals
      built <- traverse (buildMember shared) members
      let group = foldl (\acc (name /\ v) -> Map.insert name v acc) locals built
      Ref.write group shared
      Ref.write group fr.env

    buildMember shared (name /\ block) = do
      ip <- Ref.new 0
      v <- run env
        (Code { block, ip, env: shared, share: true, activation: true } : Nil)
      pure (name /\ v)

    -- Enter the guard chain's next clause, or its fall-through when the clauses are exhausted.
    enterGuard gf =
      case Array.index gf.clauses gf.index of
        Nothing -> enterBlock gf.env gf.share gf.fallthrough
        Just clause -> do
          pushFrame (Guard gf)
          enterBlock gf.env gf.share clause.guard

    -- Dispatch a switch: run the matching arm, else the default.
    enterArm fr arm = enterBlock fr.env fr.share arm

    step = Ref.read frames >>= case _ of
      Nil -> Done <$> (pop >>= force)
      ApplyMore rest : _ -> do
        v <- pop
        popFrame
        doCall false v rest
        pure (Loop unit)
      Guard gf : _ -> do
        decided <- pop >>= force
        popFrame
        case decided, Array.index gf.clauses gf.index of
          VBool true, Just clause -> do
            enterBlock gf.env gf.share clause.rhs
            pure (Loop unit)
          VBool false, _ -> do
            enterGuard (gf { index = gf.index + 1 })
            pure (Loop unit)
          _, _ -> stuck "guard: non-boolean condition"
      Code fr : _ -> do
        ip <- Ref.read fr.ip
        case Array.index fr.block ip of
          Nothing
            -- A block's end is its normal exit: the arm's value is on the stack and control resumes
            -- in the enclosing frame. An activation must leave through `Return`.
            | fr.activation -> stuck "instruction pointer past end of chunk"
            | otherwise -> popFrame $> Loop unit
          Just instruction -> do
            Ref.write (ip + 1) fr.ip
            Ref.modify_ (_ + 1) env.executed
            exec fr instruction
            pure (Loop unit)

    exec fr = case _ of
      PushInt n -> push (VInt n)
      PushNumber f -> push (VNumber f)
      PushBool b -> push (VBool b)
      PushString s -> push (VString s)
      Load name -> lookupName fr.env name >>= push
      ForeignRef key _ ->
        stuck ("unbound native foreign: " <> key <> " (the foreign frontier is ADR-0111)")
      Bind name -> do
        v <- pop
        Ref.modify_ (Map.insert name v) fr.env
      Closure params body -> do
        envRef <-
          if fr.share then pure fr.env
          else Ref.read fr.env >>= Ref.new
        push (VClosure { params, body, env: envRef })
      MakeRec members -> makeRec fr members
      Ctor tag arity k -> do
        args <- popN k
        push
          if k == arity then VData tag (Array.fromFoldable args)
          else VCtor tag arity args
      Record labels -> do
        values <- popN (Array.length labels)
        push (VRecord (recordOf labels values))
      Array k -> do
        values <- popN k
        cell <- VMArray.fromValues (Array.fromFoldable values)
        push (VArray cell)
      GetField label -> pop >>= force >>= case _ of
        VRecord m -> case Map.lookup label m of
          Just v -> push v
          Nothing -> stuck ("accessor: missing label " <> label)
        _ -> stuck "accessor: not a record"
      Proj i -> pop >>= force >>= case _ of
        VData _ fields -> case Array.index fields i of
          Just v -> push v
          Nothing -> stuck ("projection: field " <> show i <> " out of range")
        _ -> stuck "projection: not a data value"
      ProjArray i -> pop >>= force >>= case _ of
        VArray cell -> VMArray.index cell i >>= case _ of
          Just v -> push v
          Nothing -> stuck ("array projection: index " <> show i <> " out of range")
        _ -> stuck "array projection: not an array"
      Update labels -> do
        values <- popN (Array.length labels)
        pop >>= force >>= case _ of
          VRecord m -> push (VRecord (Map.union (recordOf labels values) m))
          _ -> stuck "update: not a record"
      Prim op k -> do
        args <- popN k >>= traverse force
        VMPrim.eval op (Array.fromFoldable args) >>= push
      Call k -> do
        args <- popN k
        f <- pop
        doCall false f args
      TailCall k -> do
        args <- popN k
        f <- pop
        doCall true f args
      Return -> returnFromActivation
      Jump rel -> Ref.modify_ (_ + rel) fr.ip
      JumpUnless rel -> pop >>= force >>= case _ of
        VBool false -> Ref.modify_ (_ + rel) fr.ip
        VBool true -> pure unit
        _ -> stuck "if: non-boolean condition"
      SwitchCtor arms default -> pop >>= force >>= case _ of
        VData tag _ -> enterArm fr (lookupArm (\t -> t == tag) arms default)
        _ -> stuck "switch on a non-data value"
      SwitchLit arms default -> do
        v <- pop >>= force
        case Array.head arms of
          -- A discriminant of the wrong *kind* is type-impossible; a wrong value of the right kind is
          -- an ordinary non-match and takes the default (ADR-0031).
          Just (l /\ _) | not (litKindEq l v) -> stuck "literal switch on a wrong-kind value"
          _ -> enterArm fr (lookupArm (\l -> litEq l v) arms default)
      SwitchLen arms default -> pop >>= force >>= case _ of
        VArray cell -> do
          n <- VMArray.length cell
          enterArm fr (lookupArm (_ == n) arms default)
        _ -> stuck "array-length switch on a non-array value"
      Guarded clauses fallthrough ->
        enterGuard { clauses, index: 0, fallthrough, env: fr.env, share: fr.share }
      Fail message -> stuck message

  tailRecM (\_ -> step) unit

-- | The arm whose discriminant matches, else the default.
lookupArm :: forall d. (d -> Boolean) -> Array (d /\ CodeBlock) -> CodeBlock -> CodeBlock
lookupArm matches arms default = case Array.find (\(d /\ _) -> matches d) arms of
  Just (_ /\ block) -> block
  Nothing -> default

bindParams :: Map String Value -> Array String -> List Value -> Map String Value
bindParams locals params args =
  foldl (\acc (name /\ v) -> Map.insert name v acc) locals
    (Array.zip params (Array.fromFoldable args))

recordOf :: Array String -> List Value -> Map String Value
recordOf labels values =
  foldl (\acc (label /\ v) -> Map.insert label v acc) Map.empty
    (Array.zip labels (Array.fromFoldable values))

-- | Value-level literal equality: same kind *and* same value, for selecting an arm.
litEq :: Literal -> Value -> Boolean
litEq l v = case l, v of
  LInt n, VInt m -> n == m
  LBool b, VBool c -> b == c
  LString s, VString t -> s == t
  LNumber f, VNumber g -> f == g
  _, _ -> false

-- | Same kind, any value — telling "wrong value" (a non-match) from "wrong kind" (stuck).
litKindEq :: Literal -> Value -> Boolean
litKindEq l v = case l, v of
  LInt _, VInt _ -> true
  LBool _, VBool _ -> true
  LString _, VString _ -> true
  LNumber _, VNumber _ -> true
  _, _ -> false
