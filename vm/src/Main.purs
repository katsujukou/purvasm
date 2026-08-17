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

main :: Effect Unit
main = do
  -- Providers load *before* the program runs, so a module's own initialisers cannot be mistaken for
  -- program output — which is what makes the ADR-0111 §5 stale-module gate observable.
  args <- Process.argv
  paths <- ffiPaths (Array.drop 1 args)
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
