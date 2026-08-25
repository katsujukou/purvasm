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
  , checkManifest
  , defineGlobal
  , executed
  , force
  , newEnv
  , runBlock
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Purvasm.VM.Array as VMArray
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Foreign (applyForeign, toPv)
import Purvasm.Abi.Mangle (ctorTag)
import Purvasm.VM.Foreign as Foreign
import Purvasm.VM.Instruction (CodeBlock, GuardClause, Instruction(..), Literal(..))
import Purvasm.VM.Loader as Loader
import Purvasm.VM.Prim as VMPrim
import Purvasm.VM.Value (Closure, Thunk(..), Value(..))

-- | What outlives a single run: the top-level table free names resolve through (which also closes
-- | recursion for top-level functions — a function finds itself there), and the instruction counter,
-- | the deterministic cost metric the optimiser is measured on (ADR-0026).
-- |
-- | `host`, `providers` and `foreigns` are the foreign frontier's (ADR-0111 §2). The host handle is
-- | established **lazily**, on the first `ForeignRef` a run actually executes: opening it is an
-- | effect that can fail, and a program with no native leaf — every unit test here, and any pure
-- | guest program — has no reason to perform it. Resolution is cached per key because a resolved leaf
-- | is a *value* the VM builds once (§2), and because `pv_make_closure` allocates.
-- |
-- | **`providers` is fixed when the `Env` is built, and there is no way to extend it.** That is what
-- | makes §4's exactly-one an invariant rather than a check: with a mutable provider set, resolving a
-- | key against `host-runtime`, then adding a module that also defines it, then mentioning the key
-- | again would answer from the cache and never see the collision — and the carrier already handed
-- | out would keep working besides, so clearing the cache on registration would not close it either.
-- | An immutable set makes that ordering unrepresentable. It also costs nothing: loading is explicit
-- | (§4), so every provider is known before a program starts.
-- | Opaque, and that is load-bearing rather than tidy. A record type synonym would still be a record
-- | at every call site, so `env { providers = … }` would rebuild the provider set while SHARING the
-- | resolution cache's `Ref` — reinstating exactly the ordering the immutable set exists to forbid,
-- | from ordinary safe PureScript. Hiding the constructor is what makes "fixed at construction" true
-- | of the value and not just of the constructor's signature.
newtype Env = Env
  { globals :: Ref (Map String Value)
  , executed :: Ref Int
  , host :: Ref (Maybe Loader.ModuleHandle)
  , foreigns :: Ref (Map String { arity :: Int, value :: Value })
  , providers :: Array Loader.ModuleHandle
  }

-- | Build a run environment over the globals and the providers this run may resolve against.
-- |
-- | The providers are taken here and never again: loading is explicit and opt-in (§4), so the caller
-- | knows them all before the program starts, and fixing them is what keeps exactly-one from
-- | depending on when a module was registered. Order carries no precedence — every provider is asked
-- | and exactly one must answer — so "which `show` am I running?" cannot be decided by load order.
newEnv :: Map String Value -> Array Loader.ModuleHandle -> Effect Env
newEnv globals providers = map Env $
  { globals: _, executed: _, host: _, foreigns: _, providers }
    <$> Ref.new globals
    <*> Ref.new 0
    <*> Ref.new Nothing
    <*> Ref.new Map.empty

-- | How many instructions a run has executed — the deterministic cost metric the optimiser is
-- | measured on (ADR-0026). An accessor rather than a field, so the counter can be read without the
-- | provider set and the resolution cache coming with it.
executed :: Env -> Effect Int
executed (Env env) = Ref.read env.executed

-- | Publish a global, as an image's definitions are loaded (ADR-0110 §6's slice 2). Here rather than
-- | in the loader because the globals table is the `Env`'s, and the `Env` is opaque.
-- |
-- | Order matters to the caller, not to this: a strict CAF may read globals published before it, so a
-- | loader walks the image's definitions in the order the linker wrote them.
defineGlobal :: Env -> String -> Value -> Effect Unit
defineGlobal (Env env) name value = Ref.modify_ (Map.insert name value) env.globals

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
  -- A carrier can hold a by-need cell of the runtime's own (ADR-0070), and the VM cannot tell by
  -- looking. `pv_force_if_byneed` answers for it and passes a non-cell through unchanged, so this is
  -- the same rule the VM applies to its own thunks, extended to what a leaf handed back (§3).
  VCarrier origin fv -> VCarrier origin <$> Foreign.forceCarrier fv
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
run wrapped@(Env env) frames0 = do
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
      -- A carrier holds a *runtime* closure — a resolved leaf, or an effect thunk one returned — so
      -- the runtime applies it (ADR-0111 §2). No arity is consulted here on purpose: over- and
      -- under-application, and forcing within that carrier, are the runtime's paths, and they are the
      -- ones a compiled program takes. The VM contributes the conversion and nothing else.
      VCarrier origin fv -> do
        converted <- traverse (\arg -> force arg >>= toPv origin) (Array.fromFoldable args)
        result <- applyForeign fv converted
        -- The result inherits the origin: an effect thunk returned by `writeLineImpl` is still
        -- `writeLineImpl`'s as far as a later diagnostic is concerned.
        produce tail (VCarrier origin result)
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
      v <- run wrapped
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

    -- Resolve a native leaf once and cache it (ADR-0111 §2). The search spans both provider classes:
    -- `host-runtime` — the runtime staticlib linked into this executable, where `show`, stdio, FS and
    -- `argv` live, so a program using only those needs no `--ffi` and no manifest — and every module
    -- the caller loaded.
    resolveForeign key physicalArity =
      -- The arity is validated on EVERY reference, before the cache is consulted, and the cache
      -- records the arity it was built with. Both halves matter, and neither is defensive
      -- programming against the compiler: the arity reaches `pv_make_closure` as a `uint32_t` and a
      -- leaf then indexes `args` by it, so a malformed image that resolved a key once at arity 1
      -- and mentioned it again at 3 would otherwise reuse the first closure and hand native code an
      -- argument vector it reads past. A key whose arity disagrees with its first occurrence is a
      -- corrupt image, not a program error — the compiler derives one arity per key from the
      -- PureScript type (ADR-0110 §4(a)) — so it is refused rather than re-resolved.
      case Loader.arity physicalArity of
        Nothing -> stuck ("native foreign " <> key <> " has a negative arity (" <> show physicalArity <> ")")
        Just checked -> do
          cached <- Ref.read env.foreigns
          case Map.lookup key cached of
            Just entry
              | entry.arity == physicalArity -> pure entry.value
              | otherwise -> stuck
                  ( "native foreign " <> key <> " is referenced at arity " <> show physicalArity
                      <> " but was resolved at arity "
                      <> show entry.arity
                      <> " (a corrupt image: the compiler derives one arity per key)"
                  )
            Nothing -> do
              host <- Ref.read env.host >>= case _ of
                Just h -> pure h
                Nothing -> do
                  h <- Loader.hostRuntime
                  Ref.write (Just h) env.host
                  pure h
              -- Ask each provider SEPARATELY and require exactly one to answer (ADR-0111 §4). This
              -- is why `resolve` is per-handle rather than a global `dlsym`: a module that defines a
              -- key the runtime already defines is then *detected* — the "runtime-shadow" failure —
              -- instead of one of them winning by archive order or load order. There is no shadowing
              -- rule anywhere, deliberately.
              --
              -- Which providers answer is an EXISTENCE question, so it is asked with `declares`:
              -- `resolve` builds a closure, and asking every provider that way would allocate one per
              -- candidate to keep just the winner. Exactly one closure is built, after the answer is
              -- known.
              case Array.filter (\provider -> Loader.declares provider key) ([ host ] <> env.providers) of
                [] -> stuck ("unbound native foreign: " <> key)
                [ one ] -> case Loader.resolve one key checked of
                  Nothing -> stuck ("unbound native foreign: " <> key)
                  Just fv -> do
                    -- The origin travels with the value so a later boundary error can name the leaf
                    -- that demanded the crossing (ADR-0111 §3).
                    let v = VCarrier key fv
                    Ref.modify_ (Map.insert key { arity: physicalArity, value: v }) env.foreigns
                    pure v
                many -> stuck (collision key many)

    step = Ref.read frames >>= case _ of
      Nil -> Done <$> (pop >>= force)
      ApplyMore rest : _ -> do
        v <- pop
        popFrame
        doCall false v rest
        pure (Loop unit)
      Guard gf : _ -> do
        -- A guard is a Boolean-demanding site like `JumpUnless`, and a leaf can supply the Boolean:
        -- without the demand a perfectly good guard reads as `non-boolean condition`.
        decided <- pop >>= force >>= decodeBool
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
      ForeignRef key physicalArity -> resolveForeign key physicalArity >>= push
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
        -- The field of a leaf's data value, read where the bytecode already knows the shape it
        -- demands. It comes back as a carrier, like everything else that crossed (§3's "coming out").
        VCarrier origin fv -> Foreign.adtField fv i >>= case _ of
          Just field -> push (VCarrier origin field)
          Nothing -> stuck ("projection: field " <> show i <> " out of range")
        _ -> stuck "projection: not a data value"
      -- `asCell` is what makes an array a leaf RETURNED usable here: it never had a VM cell, so it is
      -- given one that forwards to it rather than being copied into one (ADR-0111 §3).
      ProjArray i -> pop >>= force >>= VMArray.asCell >>= case _ of
        Just cell -> VMArray.index cell i >>= case _ of
          Just v -> push v
          Nothing -> stuck ("array projection: index " <> show i <> " out of range")
        Nothing -> stuck "array projection: not an array"
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
      JumpUnless rel -> pop >>= force >>= decodeBool >>= case _ of
        VBool false -> Ref.modify_ (_ + rel) fr.ip
        VBool true -> pure unit
        _ -> stuck "if: non-boolean condition"
      SwitchCtor arms default -> pop >>= force >>= case _ of
        VData tag _ -> enterArm fr (lookupArm (\t -> t == tag) arms default)
        -- A data value a leaf returned is opaque, so the arm is chosen by TAG rather than by name:
        -- `pv_adt_tag` reads the constructor's tag and each arm's name hashes to one through the same
        -- `ctorTag` codegen uses (ADR-0111 §3). This is the accessor §3 had to add — without it a leaf
        -- could not return a `Maybe` at all.
        VCarrier _ fv -> do
          let tag = Foreign.adtTag fv
          enterArm fr (lookupArm (\name -> ctorTag name == tag) arms default)
        _ -> stuck "switch on a non-data value"
      SwitchLit arms default -> do
        v <- pop >>= force >>= decodeLike (map (\(l /\ _) -> l) (Array.head arms))
        case Array.head arms of
          -- A discriminant of the wrong *kind* is type-impossible; a wrong value of the right kind is
          -- an ordinary non-match and takes the default (ADR-0031).
          Just (l /\ _) | not (litKindEq l v) -> stuck "literal switch on a wrong-kind value"
          _ -> enterArm fr (lookupArm (\l -> litEq l v) arms default)
      SwitchLen arms default -> pop >>= force >>= VMArray.asCell >>= case _ of
        Just cell -> do
          n <- VMArray.length cell
          enterArm fr (lookupArm (_ == n) arms default)
        Nothing -> stuck "array-length switch on a non-array value"
      Guarded clauses fallthrough ->
        enterGuard { clauses, index: 0, fallthrough, env: fr.env, share: fr.share }
      Fail message -> stuck message

  tailRecM (\_ -> step) unit

-- | Check the keys a build-emitted manifest declares as **workspace-provided**, before the program
-- | runs (ADR-0111 §4).
-- |
-- | The scoping is the whole design, and it is [0091](0091-user-native-ffi-c-sibling-rust-dir.md) §1's
-- | transplanted: the referenced-key set **over-approximates** what a run needs, because an image
-- | reaches a `ForeignRef` inside a *reachable definition* whose branch may never execute, and the VM
-- | has neither dead-strip nor a liveness result to tell the difference. Checking every key eagerly
-- | would therefore reject programs that run fine. What the build DOES know is which keys the user
-- | authored a provider for — there, a referenced key is genuinely meant to be provided, and a
-- | missing `.so` is the likely error — so those are checked up front and everything else stays lazy.
-- |
-- | No arity is involved: this asks whether a provider *defines* the key, which `declares` answers
-- | without building anything. That is why a manifest can carry keys alone.
checkManifest :: Env -> Array String -> Effect Unit
checkManifest (Env env) keys = do
  host <- Ref.read env.host >>= case _ of
    Just h -> pure h
    Nothing -> do
      h <- Loader.hostRuntime
      Ref.write (Just h) env.host
      pure h
  for_ keys \key ->
    case Array.filter (\provider -> Loader.declares provider key) ([ host ] <> env.providers) of
      [ _ ] -> pure unit
      [] -> stuck
        ( "no native provider for " <> key
            <> ": the build declared it as workspace-provided, so its module was expected to be loaded"
        )
      many -> stuck (collision key many)

-- | The exactly-one failure, worded once: a key two providers answer for is never resolved by
-- | precedence, because "which `show` am I running?" is not a question a user should have to ask.
collision :: String -> Array Loader.ModuleHandle -> String
collision key providers =
  key <> " provided by both " <> String.joinWith " and " (map Loader.describe providers)

-- | Decode a carrier the way a `Boolean`-demanding site must (ADR-0111 §3): the site knows what it
-- | wants, so it demands it, and the runtime's shape check enforces the demand.
decodeBool :: Value -> Effect Value
decodeBool = case _ of
  VCarrier _ fv -> pure (VBool (Foreign.booleanOf fv))
  v -> pure v

-- | Decode a carrier to the kind a literal switch is discriminating on. With no arms there is
-- | nothing to demand, so the value is left alone and the default arm takes it.
decodeLike :: Maybe Literal -> Value -> Effect Value
decodeLike literal value = case value, literal of
  VCarrier _ fv, Just (LInt _) -> pure (VInt (Foreign.intOf fv))
  VCarrier _ fv, Just (LBool _) -> pure (VBool (Foreign.booleanOf fv))
  VCarrier _ fv, Just (LString _) -> pure (VString (Foreign.stringOf fv))
  VCarrier _ fv, Just (LNumber _) -> pure (VNumber (Foreign.numberOf fv))
  _, _ -> pure value

-- | The arm whose discriminant matches, else the default.
-- | Polymorphic in what an arm CARRIES, so one selector serves both dispatch shapes: a nested block
-- | (§4(b)) and a relative offset (the pre-§4(b) images the reader still meets). The selection rule is
-- | the same in both — first match, else the default — and that is the part worth having once.
lookupArm :: forall d a. (d -> Boolean) -> Array (d /\ a) -> a -> a
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
