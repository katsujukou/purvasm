-- | The interpreter's invariants that the types cannot hold
-- | ([ADR-0110](../../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §5): the
-- | eval/apply protocol's three arities, tail-call frame discipline, by-need forcing and its black
-- | hole, and dispatch on each discriminant kind — including the two the tree-shaped `case` newly
-- | distinguishes, an arm that falls off its block and an arm that returns from the activation.
module Test.Unit.Purvasm.VM.Machine (spec) where

import Prelude

import Data.Either (either)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message, try)
import Effect.Ref as Ref
import Purvasm.VM.Instruction (CodeBlock, Instruction(..), Literal(..), PrimOp(..))
import Purvasm.VM.Machine (newEnv, runBlock)
import Purvasm.VM.Value (Thunk(..), Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Run a block with no globals and no locals, and describe the result. Results are compared through
-- | `render` rather than `Eq`, since a `Value` carries mutable cells and closures that have no
-- | meaningful structural equality.
runPure :: CodeBlock -> Aff String
runPure block = liftEffect do
  env <- newEnv Map.empty
  render =<< runBlock env block Map.empty

-- | Run a block that is expected to be stuck, and give back the diagnostic.
runStuck :: CodeBlock -> Aff String
runStuck block = liftEffect do
  env <- newEnv Map.empty
  result <- try (runBlock env block Map.empty)
  either (pure <<< message) (\v -> ("unexpectedly produced " <> _) <$> render v) result

render :: Value -> Effect String
render = case _ of
  VInt n -> pure (show n)
  VNumber f -> pure (show f)
  VBool b -> pure (show b)
  VString s -> pure (show s)
  VData tag _ -> pure ("data " <> tag)
  VCtor tag _ _ -> pure ("partial ctor " <> tag)
  VClosure _ -> pure "closure"
  VPap _ _ -> pure "pap"
  VArray _ -> pure "array"
  VRecord _ -> pure "record"
  VCarrier origin _ -> pure ("carrier from " <> origin)
  VThunk _ -> pure "thunk"

-- | `\x y -> x + y`, as a block that leaves the closure on the stack.
addClosure :: Instruction
addClosure = Closure [ "x", "y" ]
  [ Load "x", Load "y", Prim AddInt 2, Return ]

spec :: Spec Unit
spec = describe "Purvasm.VM.Machine" do
  describe "eval/apply" do
    it "enters an activation when an application saturates" do
      result <- runPure [ addClosure, PushInt 2, PushInt 3, Call 2, Return ]
      result `shouldEqual` "5"

    it "builds a partial application when it under-applies" do
      result <- runPure [ addClosure, PushInt 2, Call 1, Return ]
      result `shouldEqual` "pap"

    it "resumes a partial application with the remaining arguments" do
      result <- runPure [ addClosure, PushInt 2, Call 1, PushInt 3, Call 1, Return ]
      result `shouldEqual` "5"

    it "applies the rest to the result when it over-applies" do
      -- `(\x -> \y -> x + y) 2 3` — the outer closure takes one argument, so the second is applied
      -- to its result through the over-application continuation.
      let
        curried = Closure [ "x" ]
          [ Closure [ "y" ] [ Load "x", Load "y", Prim AddInt 2, Return ], Return ]
      result <- runPure [ curried, PushInt 2, PushInt 3, Call 2, Return ]
      result `shouldEqual` "5"

    it "is stuck applying a non-function" do
      diagnostic <- runStuck [ PushInt 1, PushInt 2, Call 1, Return ]
      diagnostic `shouldSatisfy` contains "application of a non-function"

  describe "constructors" do
    it "saturates to a data value" do
      result <- runPure [ PushInt 1, PushInt 2, Ctor "Pair" 2 2, Return ]
      result `shouldEqual` "data Pair"

    it "collects arguments while under-applied" do
      result <- runPure [ PushInt 1, Ctor "Pair" 2 1, Return ]
      result `shouldEqual` "partial ctor Pair"

    it "is stuck when over-applied" do
      diagnostic <- runStuck
        [ PushInt 1, Ctor "Pair" 2 1, PushInt 2, PushInt 3, Call 2, Return ]
      diagnostic `shouldSatisfy` contains "over-applied"

  describe "tail calls" do
    it "runs a deep self-recursive tail loop to completion" do
      -- `go n acc = if n == 0 then acc else go (n - 1) (acc + n)`, entered in tail position and run
      -- deep enough that host recursion would exhaust the stack. The bound is 65535 rather than a
      -- rounder number so the sum stays inside the signed 32-bit range and the assertion is about
      -- the loop rather than about `Int` wrapping.
      let
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
      result <- runPure
        [ MakeRec [ "go" /\ [ go, Return ] ]
        , Load "go"
        , PushInt 65535
        , PushInt 0
        , Call 2
        , Return
        ]
      result `shouldEqual` "2147450880"

  describe "case dispatch" do
    it "takes the matching constructor arm" do
      result <- runPure
        [ PushInt 7
        , Ctor "Just" 1 1
        , SwitchCtor [ "Nothing" /\ [ PushInt 0 ], "Just" /\ [ PushInt 1 ] ] [ PushInt 2 ]
        , Return
        ]
      result `shouldEqual` "1"

    it "takes the default when no arm names the constructor" do
      result <- runPure
        [ Ctor "Nil" 0 0
        , SwitchCtor [ "Cons" /\ [ PushInt 1 ] ] [ PushInt 2 ]
        , Return
        ]
      result `shouldEqual` "2"

    it "resumes the enclosing block when an arm falls off its end" do
      -- The arm leaves 1 on the stack and ends; the `+ 10` after the switch must still run.
      result <- runPure
        [ Ctor "Unit" 0 0
        , SwitchCtor [ "Unit" /\ [ PushInt 1 ] ] [ PushInt 0 ]
        , PushInt 10
        , Prim AddInt 2
        , Return
        ]
      result `shouldEqual` "11"

    it "returns from the activation when an arm returns" do
      -- The arm's `Return` must unwind the block frame *and* the activation, skipping the `+ 10`.
      let
        f = Closure [ "x" ]
          [ Load "x"
          , SwitchCtor [ "Unit" /\ [ PushInt 1, Return ] ] [ PushInt 0 ]
          , PushInt 10
          , Prim AddInt 2
          , Return
          ]
      result <- runPure [ f, Ctor "Unit" 0 0, Call 1, Return ]
      result `shouldEqual` "1"

    it "dispatches on a literal, and takes the default on a same-kind non-match" do
      matched <- runPure
        [ PushInt 2, SwitchLit [ LInt 1 /\ [ PushString "one" ], LInt 2 /\ [ PushString "two" ] ] [ PushString "other" ], Return ]
      matched `shouldEqual` "\"two\""
      defaulted <- runPure
        [ PushInt 9, SwitchLit [ LInt 1 /\ [ PushString "one" ] ] [ PushString "other" ], Return ]
      defaulted `shouldEqual` "\"other\""

    it "is stuck on a wrong-kind literal discriminant" do
      diagnostic <- runStuck
        [ PushString "x", SwitchLit [ LInt 1 /\ [ PushInt 1 ] ] [ PushInt 0 ], Return ]
      diagnostic `shouldSatisfy` contains "wrong-kind"

    it "dispatches on array length" do
      result <- runPure
        [ PushInt 1
        , PushInt 2
        , Array 2
        , SwitchLen [ 0 /\ [ PushString "empty" ], 2 /\ [ PushString "pair" ] ] [ PushString "other" ]
        , Return
        ]
      result `shouldEqual` "\"pair\""

  describe "guards" do
    it "takes the first clause whose guard holds" do
      result <- runPure
        [ Guarded
            [ { guard: [ PushBool false ], rhs: [ PushInt 1 ] }
            , { guard: [ PushBool true ], rhs: [ PushInt 2 ] }
            ]
            [ PushInt 3 ]
        , Return
        ]
      result `shouldEqual` "2"

    it "falls through when every guard is false" do
      result <- runPure
        [ Guarded [ { guard: [ PushBool false ], rhs: [ PushInt 1 ] } ] [ PushInt 3 ]
        , Return
        ]
      result `shouldEqual` "3"

    it "is stuck on a non-boolean guard" do
      diagnostic <- runStuck
        [ Guarded [ { guard: [ PushInt 0 ], rhs: [ PushInt 1 ] } ] [ PushInt 3 ], Return ]
      diagnostic `shouldSatisfy` contains "non-boolean"

  describe "records" do
    it "reads a field, and is stuck on one the record does not have" do
      result <- runPure [ PushInt 1, PushInt 2, Record [ "a", "b" ], GetField "b", Return ]
      result `shouldEqual` "2"
      diagnostic <- runStuck [ PushInt 1, Record [ "a" ], GetField "b", Return ]
      diagnostic `shouldSatisfy` contains "missing label b"

    it "lets an update override an existing field and keep the others" do
      overridden <- runPure
        [ PushInt 1, PushInt 2, Record [ "a", "b" ], PushInt 9, Update [ "a" ], GetField "a", Return ]
      overridden `shouldEqual` "9"
      kept <- runPure
        [ PushInt 1, PushInt 2, Record [ "a", "b" ], PushInt 9, Update [ "a" ], GetField "b", Return ]
      kept `shouldEqual` "2"

    it "makes RecordUnion left-biased" do
      -- `union` picks the *first* record's field on a shared label (ADR-0069); the arguments are
      -- pushed in order, so this is `{a: 1} ∪ {a: 2}` and must answer 1.
      result <- runPure
        [ PushInt 1
        , Record [ "a" ]
        , PushInt 2
        , Record [ "a" ]
        , Prim RecordUnion 2
        , GetField "a"
        , Return
        ]
      result `shouldEqual` "1"

    it "supports the dynamic field operations" do
      -- The label is the *first* argument of the dynamic record primops, so it is pushed before the
      -- record it applies to.
      present <- runPure
        [ PushString "a", PushInt 1, Record [ "a" ], Prim RecordHas 2, Return ]
      present `shouldEqual` "true"
      deleted <- runPure
        [ PushString "a"
        , PushString "a"
        , PushInt 1
        , Record [ "a" ]
        , Prim RecordDelete 2
        , Prim RecordHas 2
        , Return
        ]
      deleted `shouldEqual` "false"

  describe "arrays" do
    it "makes a write through one binding visible through another" do
      -- `let xs = [1, 2] in let ys = xs in (SetArray ys 0 42; IndexArray xs 0)` — the array is one
      -- object, so the write must be visible through the other name (ADR-0110 §3).
      result <- runPure
        [ PushInt 1
        , PushInt 2
        , Array 2
        , Bind "xs"
        , Load "xs"
        , Bind "ys"
        , Load "ys"
        , PushInt 0
        , PushInt 42
        , Prim SetArray 3
        , Bind "_"
        , Load "xs"
        , PushInt 0
        , Prim IndexArray 2
        , Return
        ]
      result `shouldEqual` "42"

    it "is stuck on an out-of-bounds index or write" do
      read' <- runStuck [ PushInt 1, Array 1, PushInt 5, Prim IndexArray 2, Return ]
      read' `shouldSatisfy` contains "index out of bounds"
      written <- runStuck
        [ PushInt 1, Array 1, PushInt 5, PushInt 0, Prim SetArray 3, Return ]
      written `shouldSatisfy` contains "set out of bounds"

    it "is stuck allocating a negative length" do
      diagnostic <- runStuck [ PushInt (-1), Prim NewArray 1, Return ]
      diagnostic `shouldSatisfy` contains "negative length"

  describe "by-need cells" do
    it "forces a cell once and memoises the result" do
      built <- liftEffect do
        counter <- Ref.new 0
        cell <- Ref.new (Unbuilt \_ -> Ref.modify_ (_ + 1) counter $> VInt 41)
        env <- newEnv (Map.singleton "caf" (VThunk cell))
        _ <- runBlock env [ Load "caf", PushInt 1, Prim AddInt 2, Return ] Map.empty
        _ <- runBlock env [ Load "caf", PushInt 1, Prim AddInt 2, Return ] Map.empty
        Ref.read counter
      built `shouldEqual` 1

    it "is stuck on a self-forcing cell (a black hole)" do
      diagnostic <- liftEffect do
        cell <- Ref.new Building
        env <- newEnv (Map.singleton "caf" (VThunk cell))
        result <- try (runBlock env [ Load "caf", Return ] Map.empty)
        pure (either message (const "unexpectedly produced a value") result)
      diagnostic `shouldSatisfy` contains "black hole"

  describe "environments" do
    it "does not let a later Bind reach a closure that captured earlier" do
      -- `let x = 1; f = \_ -> x; x = 2 in f unit` — rebinding `x` repoints the frame's environment,
      -- and `f` captured the earlier snapshot, so it still sees 1.
      result <- runPure
        [ PushInt 1
        , Bind "x"
        , Closure [ "_" ] [ Load "x", Return ]
        , Bind "f"
        , PushInt 2
        , Bind "x"
        , Load "f"
        , PushInt 0
        , Call 1
        , Return
        ]
      result `shouldEqual` "1"

    it "ties the knot for a mutually recursive group" do
      -- `isEven n = n == 0 || isOdd (n - 1)`, spelled with the two members referring to each other.
      let
        isEven = Closure [ "n" ]
          [ Load "n"
          , PushInt 0
          , Prim EqInt 2
          , JumpUnless 3
          , PushBool true
          , Return
          , Jump 0
          , Load "isOdd"
          , Load "n"
          , PushInt 1
          , Prim SubInt 2
          , TailCall 1
          , Return
          ]
        isOdd = Closure [ "n" ]
          [ Load "n"
          , PushInt 0
          , Prim EqInt 2
          , JumpUnless 3
          , PushBool false
          , Return
          , Jump 0
          , Load "isEven"
          , Load "n"
          , PushInt 1
          , Prim SubInt 2
          , TailCall 1
          , Return
          ]
      result <- runPure
        [ MakeRec [ "isEven" /\ [ isEven, Return ], "isOdd" /\ [ isOdd, Return ] ]
        , Load "isEven"
        , PushInt 10
        , Call 1
        , Return
        ]
      result `shouldEqual` "true"

  describe "ForeignRef" do
    it "refuses a negative arity before it can reach pv_make_closure" do
      -- The number becomes a `uint32_t` at the ABI, where a negative one is an enormous arity and a
      -- leaf then indexes its argument vector by it. The check runs before the host is opened, which
      -- is also why this one refusal is observable without a natively compiled VM.
      diagnostic <- runStuck [ ForeignRef "M.leafImpl" (-1), Return ]
      diagnostic `shouldSatisfy` contains "negative arity"

    it "names the key it refused" do
      diagnostic <- runStuck [ ForeignRef "M.leafImpl" (-1), Return ]
      diagnostic `shouldSatisfy` contains "M.leafImpl"

contains :: String -> String -> Boolean
contains needle haystack = String.contains (String.Pattern needle) haystack
