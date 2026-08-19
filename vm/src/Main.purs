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
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Purvasm.Stdio (writeLine)
import Purvasm.System.Process as Process
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Instruction (CodeBlock, Instruction(..), Literal(..), PrimOp(..))
import Purvasm.VM.Loader as Loader
import Purvasm.VM.Machine (executed, newEnv, runBlock)
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

loadProvider :: String -> Effect Loader.ModuleHandle
loadProvider path = do
  handle <- Loader.load path
  writeLine ("loaded: " <> Loader.describe handle)
  pure handle

-- | A leaf that only a **loaded module** can provide, exercising the two things slice 2 could not:
-- | resolution across both provider classes, and the `Boolean` boundary arm — no runtime leaf takes a
-- | `Boolean` (nothing in `runtime/src/leaf.rs` reads `pv_bool_payload`), so this fixture is the first
-- | thing that can read one.
loadedProvider :: CodeBlock
loadedProvider =
  [ ForeignRef "Purvasm.Stdio.writeLineImpl" 1
  , ForeignRef "Test.Loader.describeBoolImpl" 1
  , PushBool true
  , Call 1
  , Call 1
  , PushInt 0
  , Call 1
  , Return
  ]

-- | The aliasing gate (ADR-0111 §3): one array, bound twice AND stored inside a data value, handed to
-- | a leaf that writes element 0 — then observed through every alias.
-- |
-- | This is the invariant that a passing unit test cannot establish, because it is about *identity*
-- | rather than about any one value: an elementwise copy at the boundary would leave each of these
-- | three reads seeing the old element, and every other test in the tree would still pass.
-- |
-- | `SetArray` here also runs AFTER promotion, so the VM's own write goes through `pv_write_field`
-- | into the same object — checked by reading it back through the leaf.
aliasing :: CodeBlock
aliasing = build <> leafWrites <> observeAll <> vmWrites <> [ Return ]
  where
  -- Each call keeps its function ADJACENT to its arguments: a `Call n` pops n arguments and then the
  -- function, so anything left on the stack by an earlier statement must sit *below* the function,
  -- not between it and its arguments.
  callLeaf key arity args = [ ForeignRef key arity ] <> args <> [ Call arity ]
  runEffect thunk = thunk <> [ PushInt 0, Call 1 ]
  writeLine value = runEffect (callLeaf "Purvasm.Stdio.writeLineImpl" 1 value)

  -- one array, three aliases: two names, and a field of a data value
  build =
    [ PushString "before"
    , Array 1
    , Bind "a"
    , Load "a"
    , Bind "b"
    , Load "a"
    , Ctor "Box" 1 1
    , Bind "boxed"
    ]

  -- the leaf writes element 0 of the array it was handed; this is what promotes it
  leafWrites = runEffect
    (callLeaf "Test.Loader.writeArrayImpl" 3 [ Load "a", PushInt 0, PushString "written by the leaf" ])

  observeAll =
    writeLine [ Load "a", ProjArray 0 ]
      <> writeLine [ Load "b", ProjArray 0 ]
      <> writeLine [ Load "boxed", Proj 0, ProjArray 0 ]

  -- the reverse direction: the VM writes through its own `SetArray` to an array that has ALREADY
  -- crossed, and a leaf reads it back off the same object
  vmWrites =
    [ Load "a", PushInt 0, PushString "written by the VM", Prim SetArray 3 ]
      <> writeLine (callLeaf "Test.Loader.readArrayImpl" 2 [ Load "a", PushInt 0 ])

-- | The two migration cases §3's steps 1 and 3 exist for: an EMPTY array (no blank-array constructor
-- | to build, so `pv_empty_array` and stop) and a CYCLIC one (an array reachable from itself, which
-- | terminates only because the cell is forwarded before any element is migrated).
-- |
-- | A cycle that did not terminate would hang rather than fail, so this program existing and exiting
-- | IS the assertion.
cyclicAndEmpty :: CodeBlock
cyclicAndEmpty = emptyCrosses <> cyclicCrosses <> [ Return ]
  where
  callLeaf key arity args = [ ForeignRef key arity ] <> args <> [ Call arity ]
  runEffect thunk = thunk <> [ PushInt 0, Call 1 ]
  writeLine value = runEffect (callLeaf "Purvasm.Stdio.writeLineImpl" 1 value)

  -- Step 1: an empty array has no slot, so every other array leaf here would be out of range — it can
  -- only cross into something that just measures it. Promotion happens because it crosses AT ALL.
  emptyCrosses =
    [ Array 0, Bind "empty" ]
      <> writeLine (callLeaf "Data.Show.showIntImpl" 1 (callLeaf "Test.Loader.lengthOfImpl" 1 [ Load "empty" ]))

  -- Step 3: an array whose only element is itself. Crossing it terminates only because the cell is
  -- forwarded BEFORE the elements are migrated — otherwise the migration recurs forever.
  cyclicCrosses =
    [ PushInt 0
    , Array 1
    , Bind "cyclic"
    , Load "cyclic"
    , PushInt 0
    , Load "cyclic"
    , Prim SetArray 3
    ]
      <> writeLine (callLeaf "Data.Show.showIntImpl" 1 (callLeaf "Test.Loader.lengthOfImpl" 1 [ Load "cyclic" ]))

-- | The slice-4 milestone: values that came from a leaf are **consumed** by ordinary bytecode.
-- |
-- | Nothing here is a new instruction. Each site simply meets a carrier where it used to meet a VM
-- | value, and decodes it by demanding the shape it already required (ADR-0111 §3) — so what this
-- | program really checks is that the FFI stopped being visible above the boundary.
-- |
-- | Two array entrances are exercised deliberately, because they are different code paths that must
-- | share one invariant: an array the GUEST built (promoted when it crossed) and one a LEAF returned
-- | (a carrier from birth, given a forwarding cell). `SetArray` runs on the latter, which the review
-- | pointed out is reachable without any promotion ever happening.
carrierElimination :: CodeBlock
carrierElimination = arithmetic <> leafArray <> [ Return ]
  where
  callLeaf key arity args = [ ForeignRef key arity ] <> args <> [ Call arity ]
  runEffect thunk = thunk <> [ PushInt 0, Call 1 ]
  writeLine value = runEffect (callLeaf "Purvasm.Stdio.writeLineImpl" 1 value)
  showInt value = callLeaf "Data.Show.showIntImpl" 1 value

  -- `floatBitsHi 1.0` gives a carrier `Int`; adding 1 to it is a scalar primop meeting a carrier.
  -- 1072693249 is 0x3FF00001, so the arithmetic really happened on the decoded payload.
  arithmetic =
    writeLine
      ( showInt
          ( callLeaf "Purvasm.Number.floatBitsHi" 1 [ PushNumber 1.0 ]
              <> [ PushInt 1, Prim AddInt 2 ]
          )
      )

  -- An array the leaf returned: length it, index it, write to it, and read the write back — all
  -- through instructions that never knew about the FFI.
  leafArray =
    callLeaf "Test.Loader.makeArrayImpl" 1 [ PushString "from the leaf" ]
      <> [ Bind "arr" ]
      <> writeLine (showInt [ Load "arr", Prim LengthArray 1 ])
      <> writeLine [ Load "arr", PushInt 0, Prim IndexArray 2 ]
      <> [ Load "arr", PushInt 1, PushString "set on a leaf array", Prim SetArray 3, Bind "same" ]
      <> writeLine [ Load "same", PushInt 1, Prim IndexArray 2 ]
      -- and the leaf sees the VM's write on the same object
      <> writeLine (callLeaf "Test.Loader.readArrayImpl" 2 [ Load "arr", PushInt 1 ])

-- | Every **control** site that had to learn about carriers, driven once each (ADR-0111 §3).
-- |
-- | `carrierElimination` covers the value sites — arithmetic and the array operations — but a site
-- | that merely *branches* on a leaf's value is just as much an elimination site, and none of them
-- | was exercised until a review pointed at `Guarded`, which turned out to be undecoded. So this
-- | program exists to make the coverage structural rather than incidental: one arm per site, each
-- | printing a distinct line, driven by Booleans and Ints a **leaf** produced.
carrierControl :: CodeBlock
carrierControl = jumpUnlessArm <> guardedArms <> switchLitArm <> arrayArms <> [ Return ]
  where
  callLeaf key arity args = [ ForeignRef key arity ] <> args <> [ Call arity ]
  runEffect thunk = thunk <> [ PushInt 0, Call 1 ]
  writeLine value = runEffect (callLeaf "Purvasm.Stdio.writeLineImpl" 1 value)
  say text = writeLine [ PushString text ]
  isPositive n = callLeaf "Test.Loader.isPositiveImpl" 1 [ PushInt n ]

  -- `JumpUnless` over a Boolean the leaf returned. The `else` line must never appear.
  jumpUnlessArm =
    isPositive 1
      <> [ JumpUnless (Array.length (say "jumpUnless: took the true branch")) ]
      <> say "jumpUnless: took the true branch"

  -- A guard chain whose condition is the leaf's Boolean — both outcomes, so neither a stuck guard nor
  -- an always-true one passes. The false clause falls through to the chain's fall-through block.
  guardedArms =
    [ Guarded [ { guard: isPositive 1, rhs: say "guarded: true clause fired" } ] (say "guarded: WRONG fall-through")
    , Guarded [ { guard: isPositive (-1), rhs: say "guarded: WRONG true clause" } ] (say "guarded: false fell through")
    ]

  -- `SwitchLit` discriminating on an Int the leaf produced (0x3FF00000 for 1.0).
  switchLitArm =
    callLeaf "Purvasm.Number.floatBitsHi" 1 [ PushNumber 1.0 ]
      <> [ SwitchLit [ LInt 1072693248 /\ say "switchLit: matched the leaf's Int" ] (say "switchLit: WRONG default") ]

  -- `SwitchLen` and `ProjArray` over an array the leaf returned — a carrier from birth, so neither
  -- site ever sees a VM-built array here.
  arrayArms =
    callLeaf "Test.Loader.makeArrayImpl" 1 [ PushString "projArray: read from the leaf's array" ]
      <>
        [ Bind "arr"
        , Load "arr"
        , SwitchLen [ 2 /\ say "switchLen: matched the leaf's array" ] (say "switchLen: WRONG default")
        ]
      <> writeLine [ Load "arr", ProjArray 0 ]

-- | `--self-test <name>`: run ONE named program and nothing else.
-- |
-- | A mode rather than an in-process assertion, because **a stuck run cannot be caught on this
-- | target**: purvasm's `Effect.Exception` is a throw-only shadow (ADR-0074), so `try` around
-- | `runBlock` does not come back — the process writes the diagnostic and exits. The harness
-- | therefore observes a refusal the only way it can, as a separate run's exit status and stderr.
-- |
-- | Each probe also names what it needs. `loaded-provider` demands a module defining
-- | `Test.Loader.describeBoolImpl`, which is exactly why it cannot live in the ordinary run: that run
-- | is handed whatever `--ffi` names, and every other gate here passes a module that defines
-- | something else entirely.
selfTest :: Array String -> Maybe String
selfTest args = case Array.uncons args of
  Nothing -> Nothing
  Just { head: "--self-test", tail } -> Array.head tail
  Just { tail } -> selfTest tail

runSelfTest :: Array String -> String -> Effect Unit
runSelfTest args = case _ of
  "arity-mismatch" -> do
    env <- newEnv Map.empty []
    void (runBlock env arityMismatch Map.empty)
    writeLine "arity-mismatch: unexpectedly resolved"
  -- A module exporting a key the runtime already defines: exactly-one must catch it rather than let
  -- either win silently (ADR-0111 §4's runtime-shadow case).
  "runtime-shadow" -> do
    handles <- loadAll args
    env <- newEnv Map.empty handles
    void (runBlock env [ ForeignRef "Data.Show.showIntImpl" 1, Return ] Map.empty)
    writeLine "runtime-shadow: unexpectedly resolved"
  -- With a provider loaded, resolution spans both classes (ADR-0111 §4): this calls a leaf the
  -- runtime does NOT define, so an answer can only have come from the loaded module.
  "loaded-provider" -> runWithProviders args loadedProvider
  "aliasing" -> runWithProviders args aliasing
  "carrier-elimination" -> runWithProviders args carrierElimination
  "carrier-control" -> runWithProviders args carrierControl
  "cyclic" -> runWithProviders args cyclicAndEmpty
  other -> stuck ("unknown --self-test: " <> other)

-- | Load whatever `--ffi` names and run one block against exactly those providers.
runWithProviders :: Array String -> CodeBlock -> Effect Unit
runWithProviders args block = do
  handles <- loadAll args
  env <- newEnv Map.empty handles
  void (runBlock env block Map.empty)

-- | The providers named by `--ffi`, loaded in order.
loadAll :: Array String -> Effect (Array Loader.ModuleHandle)
loadAll args = ffiPaths args >>= traverse Loader.load

main :: Effect Unit
main = do
  -- Providers load *before* the program runs, so a module's own initialisers cannot be mistaken for
  -- program output — which is what makes the ADR-0111 §5 stale-module gate observable.
  args <- Process.argv
  case selfTest (Array.drop 1 args) of
    Just name -> runSelfTest (Array.drop 1 args) name
    Nothing -> ordinaryRun (Array.drop 1 args)

-- | The ordinary run: load whatever `--ffi` names, then the guest programs.
ordinaryRun :: Array String -> Effect Unit
ordinaryRun args = do
  paths <- ffiPaths args
  handles <- traverse loadProvider paths
  env <- newEnv Map.empty handles
  result <- runBlock env program Map.empty
  count <- executed env
  writeLine ("result: " <> describe result)
  writeLine ("instructions: " <> show count)
  -- ADR-0111 slice 2 probe: can the host resolve a runtime leaf the VM itself never calls?
  host <- Loader.hostRuntime
  writeLine ("provider: " <> Loader.describe host)
  writeLine ("resolve Data.Show.showIntImpl: " <> probe host "Data.Show.showIntImpl" 1)
  writeLine ("resolve Purvasm.Stdio.writeLineImpl: " <> probe host "Purvasm.Stdio.writeLineImpl" 1)
  writeLine ("resolve Nope.nope: " <> probe host "Nope.nope" 1)
  -- ADR-0111 slice 2: resolve, fire, and run the effect — all through `host-runtime`. The line below
  -- this one is written by the guest program, not by `main`.
  writeLine "runtime leaves:"
  leafEnv <- newEnv Map.empty handles
  leafResult <- runBlock leafEnv runtimeLeaves Map.empty
  writeLine ("leaf result: " <> describe leafResult)

