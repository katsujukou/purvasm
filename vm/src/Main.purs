-- | The owned VM's native entry point
-- | ([ADR-0110](../../docs/design-decisions/0110-owned-vm-purescript-native.md) §2).
-- |
-- | It runs a guest program built **in memory**: the image reader is slice 2, and the foreign
-- | frontier is [0111](../../docs/design-decisions/0111-vm-dynamic-native-ffi.md). What this entry
-- | exists to establish is §2's claim — that an interpreter written in PureScript compiles to a
-- | native executable and runs — because everything downstream (`dlopen`, `pv_make_closure`, the
-- | link-time retention of the foreign API) is only reachable from a natively compiled VM. On node it
-- | runs the same program through the same code, so the two targets can be compared by eye.
-- |
-- | It also takes `--ffi <path>` and loads that provider before running, which is what
-- | `tools/vm-loader-e2e.sh` drives: loading with `RTLD_NOW` binds every reference a module makes, so
-- | a module that loads is a module whose whole `pv_*` surface the host exported (ADR-0111 §1.1), and
-- | a module built against another foreign ABI is refused before its initialisers run (§5). Neither
-- | is observable without a natively compiled host, and neither needs a leaf to be *called*.
module Main (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import Purvasm.Stdio (writeLine)
import Purvasm.System.Process as Process
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Instruction (CodeBlock, Instruction(..), PrimOp(..))
import Purvasm.VM.Loader as Loader
import Purvasm.VM.Machine (newEnv, runBlock)
import Purvasm.VM.Value (Value(..))

-- | `let go n acc = if n == 0 then acc else go (n - 1) (acc + n) in go 10 0` — a tail-recursive
-- | guest loop, so the run exercises the frame discipline rather than a single primop.
program :: CodeBlock
program =
  [ MakeRec [ "go" /\ [ go, Return ] ]
  , Load "go"
  , PushInt 10
  , PushInt 0
  , Call 2
  , Return
  ]
  where
  go = Closure [ "n", "acc" ]
    [ Load "n"
    , PushInt 0
    , Prim EqInt 2
    , JumpUnless 3
    , Load "acc"
    , Return
    , Jump 0
    , Load "go"
    , Load "n"
    , PushInt 1
    , Prim SubInt 2
    , Load "acc"
    , Load "n"
    , Prim AddInt 2
    , TailCall 2
    , Return
    ]

-- | The ADR-0111 slice-2 milestone, as a guest program: the corpus's **runtime** leaves run on the VM
-- | with no module loaded and no manifest, because the runtime staticlib linked into this executable
-- | is itself a provider class (§1.1/§4's `host-runtime`).
-- |
-- | It is written as the compiler would lower it. `Purvasm.Stdio.writeLineImpl :: String -> Effect Unit`
-- | has physical closure arity 1 (`leafClosureArity` folds `retVsat` in), so saturating it yields the
-- | effect *thunk*, and running the effect is applying that thunk to the run marker — `CPerform t`
-- | lowers to `t` applied to `LInt 0` on this backend and on LLVM alike, so the VM passes exactly what
-- | a compiled program passes.
-- |
-- | `show` is the pure leaf in the same program: its result is a carrier, and handing that carrier
-- | straight to `writeLine` is the point — a value that came from a leaf is never decoded (§3), it is
-- | passed on.
runtimeLeaves :: CodeBlock
runtimeLeaves = stringArm <> intArm <> numberArm <> [ Return ]
  where
  -- A VM `String` handed straight to a leaf. This is the arm that would break silently if the
  -- boundary's claim were wrong: nothing converts here, so what `writeLineImpl` reads is the very
  -- word the VM was holding, and a mismatch in representation would print rubbish rather than fail.
  stringArm =
    [ ForeignRef "Purvasm.Stdio.writeLineImpl" 1
    , PushString "boundary: a VM string"
    , Call 1
    , PushInt 0
    , Call 1
    ]

  -- A VM `Int` in, and a carrier back out that is passed on WITHOUT being decoded (§3): `showIntImpl`
  -- returns a runtime `String` which goes straight into `writeLineImpl`.
  intArm =
    [ ForeignRef "Purvasm.Stdio.writeLineImpl" 1
    , ForeignRef "Data.Show.showIntImpl" 1
    , PushInt 42
    , Call 1
    , Call 1
    , PushInt 0
    , Call 1
    ]

  -- A VM `Number` in, through a leaf that actually READS it: `floatBitsHi 1.0` must print
  -- 1072693248 (0x3FF00000, the high half of IEEE-754 1.0). A wrong `Number` representation could
  -- not produce that number by accident, so this is the arm's proof rather than a smoke test.
  numberArm =
    [ ForeignRef "Purvasm.Stdio.writeLineImpl" 1
    , ForeignRef "Data.Show.showIntImpl" 1
    , ForeignRef "Purvasm.Number.floatBitsHi" 1
    , PushNumber 1.0
    , Call 1
    , Call 1
    , Call 1
    , PushInt 0
    , Call 1
    ]

-- | A corrupt image, refused: the same key mentioned at two different arities. The compiler derives
-- | one arity per key from the PureScript type (ADR-0110 §4(a)), so a disagreement is not a program
-- | error to report at the call — it is an image that must not be run, because a leaf indexes its
-- | argument vector by the arity the closure was built with.
arityMismatch :: CodeBlock
arityMismatch =
  [ ForeignRef "Data.Show.showIntImpl" 1
  , ForeignRef "Data.Show.showIntImpl" 2
  , Return
  ]

-- | Render a result for the trace. Deliberately partial in spirit — this is a smoke entry, not the
-- | runner's observation contract (§5), which arrives with the corpus gate.
describe :: Value -> String
describe = case _ of
  VInt n -> show n
  VNumber f -> show f
  VBool b -> show b
  VString s -> show s
  VData tag _ -> "data " <> tag
  _ -> "<value>"

probe :: Loader.ModuleHandle -> String -> Int -> String
probe host key n = case Loader.arity n of
  Nothing -> "bad arity"
  Just a -> case Loader.resolve host key a of
    Just _ -> "resolved"
    Nothing -> "absent"

-- | The provider paths named by `--ffi <path>`, in order. Loading a shared object runs arbitrary
-- | native code, so it is explicit and opt-in (ADR-0111 §4): nothing is discovered from the working
-- | directory or the environment. A `--ffi` with no path is an error rather than a silent skip — the
-- | caller meant to load something.
ffiPaths :: Array String -> Effect (Array String)
ffiPaths args = case Array.uncons args of
  Nothing -> pure []
  Just { head: "--ffi", tail } -> case Array.uncons tail of
    Just path -> Array.cons path.head <$> ffiPaths path.tail
    Nothing -> stuck "--ffi needs a provider path"
  Just { tail } -> ffiPaths tail

-- | `--self-test <name>`: run one deliberately-stuck program and nothing else.
-- |
-- | It is a mode rather than an in-process assertion because **a stuck run cannot be caught on this
-- | target**: purvasm's `Effect.Exception` is a throw-only shadow (ADR-0074), so `try` around
-- | `runBlock` does not come back — the process writes the diagnostic and exits. The harness
-- | therefore observes the refusal the only way it can, as a separate run's exit status and stderr.
selfTest :: Array String -> Maybe String
selfTest args = case Array.uncons args of
  Nothing -> Nothing
  Just { head: "--self-test", tail } -> Array.head tail
  Just { tail } -> selfTest tail

runSelfTest :: String -> Effect Unit
runSelfTest = case _ of
  "arity-mismatch" -> do
    env <- newEnv Map.empty
    void (runBlock env arityMismatch Map.empty)
    writeLine "arity-mismatch: unexpectedly resolved"
  other -> stuck ("unknown --self-test: " <> other)

main :: Effect Unit
main = do
  -- Providers load *before* the program runs, so a module's own initialisers cannot be mistaken for
  -- program output — which is what makes the ADR-0111 §5 stale-module gate observable.
  args <- Process.argv
  case selfTest (Array.drop 1 args) of
    Just name -> runSelfTest name
    Nothing -> ordinaryRun (Array.drop 1 args)

-- | The ordinary run: load whatever `--ffi` names, then the guest programs.
ordinaryRun :: Array String -> Effect Unit
ordinaryRun args = do
  paths <- ffiPaths args
  for_ paths \path -> do
    handle <- Loader.load path
    writeLine ("loaded: " <> Loader.describe handle)
  env <- newEnv Map.empty
  result <- runBlock env program Map.empty
  executed <- Ref.read env.executed
  writeLine ("result: " <> describe result)
  writeLine ("instructions: " <> show executed)
  -- ADR-0111 slice 2 probe: can the host resolve a runtime leaf the VM itself never calls?
  host <- Loader.hostRuntime
  writeLine ("provider: " <> Loader.describe host)
  writeLine ("resolve Data.Show.showIntImpl: " <> probe host "Data.Show.showIntImpl" 1)
  writeLine ("resolve Purvasm.Stdio.writeLineImpl: " <> probe host "Purvasm.Stdio.writeLineImpl" 1)
  writeLine ("resolve Nope.nope: " <> probe host "Nope.nope" 1)
  -- ADR-0111 slice 2: resolve, fire, and run the effect — all through `host-runtime`. The line below
  -- this one is written by the guest program, not by `main`.
  writeLine "runtime leaves:"
  leafEnv <- newEnv Map.empty
  leafResult <- runBlock leafEnv runtimeLeaves Map.empty
  writeLine ("leaf result: " <> describe leafResult)

