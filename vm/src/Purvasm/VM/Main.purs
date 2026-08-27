-- | The owned VM's native entry point: it reads a linked bytecode image and runs it.
-- |
-- | `--image <path>` names the program. `--ffi <path>` loads a native provider before the program
-- | starts, and `--manifest <path>` names the keys the build says a loaded module must supply — both
-- | are the runner's to pass, never discovered beside the image, so what a hosted program may reach
-- | is decided by whoever launched it. `--` separates the VM's own arguments from the program's.
-- |
-- | `--self-test <name>` is the diagnostic entry the loader gate drives. It is not a user feature:
-- | each case builds a small guest program in memory to exercise one property of the foreign frontier
-- | that no unit test can reach, because it needs a natively compiled host with real `dlopen`.
module Purvasm.VM.Main (main) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Purvasm.Stdio (writeErrLine, writeLine)
import Purvasm.System.Process as Process
import Purvasm.VM.Error (stuck)
import Purvasm.VM.Instruction (CodeBlock, Instruction(..), Literal(..), PrimOp(..))
import Purvasm.VM.Host as Host
import Purvasm.VM.Loader as Loader
import Data.Either (Either(..))
import Purvasm.FS as FS
import Purvasm.VM.Image as Image
import Purvasm.VM.Program as Program
import Purvasm.VM.Machine (Env, checkManifest, executed, newEnv, runBlock)
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

-- | The image path named by `--image <path>`, if any.
imagePath :: Array String -> Effect (Maybe String)
imagePath args = case Array.uncons args of
  Nothing -> pure Nothing
  Just { head: "--image", tail } -> case Array.uncons tail of
    Just path -> pure (Just path.head)
    Nothing -> stuck "--image needs a path"
  Just { tail } -> imagePath tail

-- | Run a linked image, under ADR-0110 §5's **typed terminal demand**.
-- |
-- | The image's `main` chunk already applies the entry to the run marker — the linker builds it as
-- | `<module>.main` applied to `LInt 0` — so running it *is* performing the effect. The flag decides
-- | only how the result is observed, and the two modes are not a formatting choice:
-- |
-- |   * an `Effect` entry is **run and discarded**, because its final value is frequently a carrier (a
-- |     `main` ending in `Console.log` returns whatever that leaf's `pv_apply` returned) and observing
-- |     it would fail the commonest program there is;
-- |   * a **value** entry is printed, and a carrier there is the named escape error — the VM cannot
-- |     render a value it is forbidden to introspect (ADR-0111 §3).
runImage :: Env -> Boolean -> String -> Effect Unit
runImage env counting path = FS.readTextFile path >>= case _ of
  Nothing -> stuck ("cannot read the image at " <> path)
  Just text -> case Image.decodeImage text of
    Left e -> stuck ("cannot read the image at " <> path <> ": " <> e)
    Right image -> do
      Program.load env image
      result <- runBlock env image.main Map.empty
      -- On stderr, in boot's wording, so one reader parses either runner (ADR-0110 §6, step C): the
      -- guest owns stdout, and the count must not appear in output a differential compares.
      when counting do
        n <- executed env
        writeErrLine ("instructions " <> show n)
      if image.isEffect then pure unit
      else case result of
        VCarrier origin _ -> stuck
          ( "native value escaped as the program result (from " <> origin
              <> "): a value entry must produce something the VM can render"
          )
        v -> writeLine (describe v)

-- | The arguments meant for the guest: everything after a `--` separator, empty when there is none.
-- |
-- | A separator rather than "whatever is left over" because the VM's own flags are open-ended: a
-- | positional guest argument that happened to look like a future flag would change meaning when
-- | that flag lands, and it would do so silently. So `--image`, `--count`, `--ffi` and `--manifest`
-- | are the VM's and never the guest's, and the guest's are exactly what it asked for.
guestArgs :: Array String -> Array String
guestArgs args = case Array.elemIndex "--" args of
  Nothing -> []
  Just i -> Array.drop (i + 1) args

-- | Whether a bare flag appears in the arguments.
flagSet :: String -> Array String -> Boolean
flagSet flag = Array.elem flag

-- | The manifest path named by `--manifest <path>`, if any.
-- |
-- | A flag rather than discovery-beside-the-image, for now: there is no image yet (ADR-0110's slice 2),
-- | so there is nothing to sit beside. When the reader lands, the manifest is found next to the image
-- | and this flag becomes the override.
manifestPath :: Array String -> Effect (Maybe String)
manifestPath args = case Array.uncons args of
  Nothing -> pure Nothing
  Just { head: "--manifest", tail } -> case Array.uncons tail of
    Just path -> pure (Just path.head)
    Nothing -> stuck "--manifest needs a path"
  Just { tail } -> manifestPath tail

-- | The workspace-provided keys a build-emitted manifest declares (ADR-0111 §4).
-- |
-- | The banner is checked rather than assumed: a VM meeting a manifest it does not understand must
-- | say so, not silently check nothing — an eager gate that quietly becomes a no-op is worse than no
-- | gate, because the build still reports having emitted one.
readManifest :: String -> Effect (Array String)
readManifest path = FS.readTextFile path >>= case _ of
  Nothing -> stuck ("cannot read the foreign manifest at " <> path)
  Just text -> case Array.uncons (String.split (String.Pattern "\n") text) of
    Nothing -> stuck ("empty foreign manifest at " <> path)
    Just { head: banner, tail: rest }
      -- The FIRST physical line is the banner: skipping blank lines to find it would accept a file
      -- whose shape the writer never produces, and this check is the only thing standing between a
      -- misread manifest and a check that silently passes.
      | banner /= manifestBanner -> stuck ("unrecognised foreign manifest format at " <> path <> ": " <> banner)
      | otherwise -> case Array.unsnoc rest of
          -- The writer ends every manifest with a newline, so exactly one trailing empty segment is
          -- expected and no other empty line is. An empty key silently dropped is a key not checked.
          Just { init: keys, last: "" }
            | not (Array.any (_ == "") keys) -> pure keys
          _ -> stuck
            ( "malformed foreign manifest at " <> path
                <> ": expected the banner, then one key per line, ending in a newline"
            )

-- | The manifest format this VM understands. Shared in spirit with the build's writer; a mismatch is
-- | refused above rather than misread.
manifestBanner :: String
manifestBanner = "purvasm-foreign-manifest:v1"

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

-- | Data values across the boundary, both directions and both shapes (ADR-0111 §3, slice 5).
-- |
-- | This is what `pv_adt_tag` was added for. A data value a leaf returned is opaque like any carrier,
-- | so `SwitchCtor` cannot compare names — it compares the value's TAG against each arm's
-- | `ctorTag name`, the same derivation codegen uses, which is why the bytecode can keep carrying
-- | names (ADR-0110 §4) and still dispatch on a native ADT.
-- |
-- | Both constructors are exercised deliberately, because they are represented differently and only
-- | one of them is a heap object: `Just x` is an ADT, `Nothing` is the immediate whose payload is the
-- | tag. Nothing in the VM can tell them apart, which is exactly why the accessor answers for both.
dataLeaves :: CodeBlock
dataLeaves = inboundJust <> inboundNothing <> outbound <> [ Return ]
  where
  callLeaf key arity args = [ ForeignRef key arity ] <> args <> [ Call arity ]
  runEffect thunk = thunk <> [ PushInt 0, Call 1 ]
  writeLine value = runEffect (callLeaf "Purvasm.Stdio.writeLineImpl" 1 value)
  say text = writeLine [ PushString text ]

  -- The occurrence is bound before the switch, exactly as `MatchCompile` lowers a `case`: the arm
  -- needs the value again to project a field out of it.
  dispatch n justArm =
    callLeaf "Test.Loader.lookupImpl" 1 [ PushInt n ]
      <>
        [ Bind "m"
        , Load "m"
        , SwitchCtor
            [ "Data.Maybe.Just" /\ justArm
            , "Data.Maybe.Nothing" /\ say "dispatch: took Nothing"
            ]
            (say "dispatch: WRONG default")
        ]

  -- `Just`: a heap ADT the leaf built, dispatched on and then PROJECTED — the field comes back as a
  -- carrier and goes straight out to `writeLine` without being decoded (§3's "coming out").
  inboundJust = dispatch 1 (writeLine [ Load "m", Proj 0 ])

  -- `Nothing`: no heap object at all, so this arm proves the accessor's immediate case.
  inboundNothing = dispatch 0 (say "dispatch: WRONG Just")

  -- Outbound: the VM builds both shapes and a leaf reads their tags back.
  outbound =
    writeLine (callLeaf "Test.Loader.describeMaybeImpl" 1 [ PushString "x", Ctor "Data.Maybe.Just" 1 1 ])
      <> writeLine (callLeaf "Test.Loader.describeMaybeImpl" 1 [ Ctor "Data.Maybe.Nothing" 0 0 ])

-- | `Proj` with a negative index, against a data value a LEAF returned.
-- |
-- | A local `VData` is refused by `Data.Array.index`; a carrier has no such guard of its own, and the
-- | accessor's `+ 1` would turn `-1` into slot 0 — the raw tag, the one word the separate ADT accessor
-- | exists to keep out of value positions. Both representations must therefore give the SAME stuck
-- | diagnostic, which is what this and its local twin below assert.
negativeProjCarrier :: CodeBlock
negativeProjCarrier =
  [ ForeignRef "Test.Loader.lookupImpl" 1
  , PushInt 1
  , Call 1
  , Proj (-1)
  , Return
  ]

-- | The same demand against a VM-built data value, so the two diagnostics can be compared.
negativeProjLocal :: CodeBlock
negativeProjLocal =
  [ PushString "x"
  , Ctor "Data.Maybe.Just" 1 1
  , Proj (-1)
  , Return
  ]

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
  -- Loading alone, reported: whether a module binds against this host is the whole question for the
  -- retention, ABI-version and diagnosis legs, and none of them needs a leaf to be CALLED.
  "load" -> void (traverse loadProvider =<< ffiPaths args)
  -- Resolution, firing, argument conversion, the carrier pass-through and the effect run, all
  -- through the runtime staticlib linked into this executable — with no module loaded and no
  -- manifest, which is what makes it a test of retention rather than of loading.
  "runtime-leaves" -> do
    env <- newEnv Map.empty []
    result <- runBlock env program Map.empty
    writeLine ("result: " <> describe result)
    leafEnv <- newEnv Map.empty []
    leafResult <- runBlock leafEnv runtimeLeaves Map.empty
    -- Reported through `describe`, which does NOT decode a carrier: what a leaf returned stays
    -- opaque, and printing the string it holds would break the invariant the boundary exists for.
    writeLine ("leaf result: " <> describe leafResult)
  "loaded-provider" -> runWithProviders args loadedProvider
  "aliasing" -> runWithProviders args aliasing
  "carrier-elimination" -> runWithProviders args carrierElimination
  "carrier-control" -> runWithProviders args carrierControl
  "data-leaves" -> runWithProviders args dataLeaves
  "negative-proj-carrier" -> runWithProviders args negativeProjCarrier
  "negative-proj-local" -> runWithProviders args negativeProjLocal
  "cyclic" -> runWithProviders args cyclicAndEmpty
  other -> stuck ("unknown --self-test: " <> other)

-- | Load whatever `--ffi` names and run one block against exactly those providers.
runWithProviders :: Array String -> CodeBlock -> Effect Unit
runWithProviders args block = do
  handles <- loadAll args
  env <- newEnv Map.empty handles
  manifestPath args >>= case _ of
    Nothing -> pure unit
    Just path -> readManifest path >>= checkManifest env
  void (runBlock env block Map.empty)

-- | The providers named by `--ffi`, loaded in order.
loadAll :: Array String -> Effect (Array Loader.ModuleHandle)
loadAll args = ffiPaths args >>= traverse Loader.load

main :: Effect Unit
main = do
  -- Providers load *before* the program runs, so a module's own initialisers cannot be mistaken for
  -- program output — which is what makes the ADR-0111 §5 stale-module gate observable.
  args <- Process.argv
  let rest = Array.drop 1 args
  case selfTest rest of
    Just name -> runSelfTest rest name
    Nothing -> imagePath rest >>= case _ of
      Just path -> do
        handles <- loadAll rest
        -- Before anything of the guest's runs: the argv it observes is ITS command line, not the
        -- VM's (ADR-0075 §4). Without this the program reads `--image` where its first argument
        -- belongs — the runtime's leaf reports the process's argv, and the process is the VM.
        Host.setGuestArgv ([ path ] <> guestArgs rest)
        env <- newEnv Map.empty handles
        manifestPath rest >>= case _ of
          Nothing -> pure unit
          Just manifest -> readManifest manifest >>= checkManifest env
        runImage env (flagSet "--count" rest) path
      Nothing -> usage

-- | What this program is, and the one argument it cannot do without.
usage :: Effect Unit
usage = do
  writeErrLine "purvasm-vm: runs a linked purvasm bytecode image."
  writeErrLine ""
  writeErrLine "  purvasm-vm --image <path> [--ffi <path>]... [--manifest <path>] [--count] [-- <args>...]"
  writeErrLine ""
  writeErrLine "  --image <path>     the program to run (required)"
  writeErrLine "  --ffi <path>       load a native provider before the program starts; repeatable"
  writeErrLine "  --manifest <path>  the keys a loaded provider must supply, checked before the run"
  writeErrLine "  --count            report the instruction count on stderr when the program ends"
  writeErrLine "  -- <args>...       passed to the program as its own arguments"
  stuck "no --image given"
