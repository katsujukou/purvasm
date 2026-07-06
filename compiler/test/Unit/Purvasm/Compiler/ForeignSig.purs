-- | The ADR-0080 §2 interpretation contract, pinned as the ADR-0034 dual summary
-- | `(arity, vsat, retVsat)`: `vsat` = saturating performs, `retVsat` = the saturated result
-- | is a later-forced effect thunk. Includes the uncurried families (`EffectFn{N}`/`STFn{N}`
-- | are vsat, `Fn{N}` pure) grounded in boot's ADR-0039 adapters, and the documented CST
-- | limitation (a synonym hiding an effect head) as a test rather than a surprise.
module Test.Unit.Purvasm.Compiler.ForeignSig where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.ForeignSig (reconstructModule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- A tiny module around one foreign declaration (plus one exported wrapper, mirroring the
-- private-`Impl` idiom this pass exists for).
modWith :: String -> String
modWith decl =
  "module Test.M (safe) where\n\n"
    <> "import Prelude\n\n"
    <> decl
    <> "\n\nsafe :: Int\nsafe = 42\n"

sigOf :: String -> Either String { arity :: Int, vsat :: Boolean, retVsat :: Boolean }
sigOf decl = case reconstructModule (modWith decl) of
  Left issue -> Left (show issue)
  Right { sigs } -> case sigs of
    [ _ /\ shape ] -> Right shape
    other -> Left ("expected exactly one sig, got " <> show (map show other))

-- Shorthands for the three summaries a floor type can take.
pure_ :: Int -> Either String { arity :: Int, vsat :: Boolean, retVsat :: Boolean }
pure_ n = Right { arity: n, vsat: false, retVsat: false }

retThunk :: Int -> Either String { arity :: Int, vsat :: Boolean, retVsat :: Boolean }
retThunk n = Right { arity: n, vsat: false, retVsat: true }

performs :: Int -> Either String { arity :: Int, vsat :: Boolean, retVsat :: Boolean }
performs n = Right { arity: n, vsat: true, retVsat: false }

spec :: Spec Unit
spec = describe "Purvasm.Compiler.ForeignSig" do
  describe "reconstructModule" do
    it "names the module and keys the foreign by identifier" do
      case reconstructModule (modWith "foreign import addImpl :: Int -> Int -> Int") of
        Right { moduleName, sigs } -> do
          moduleName `shouldEqual` "Test.M"
          map (\(n /\ _) -> n) sigs `shouldEqual` [ "addImpl" ]
        Left issue -> fail (show issue)

    it "skips foreign import data (types are not values)" do
      case reconstructModule (modWith "foreign import data Handle :: Type") of
        Right { sigs } -> sigs `shouldEqual` []
        Left issue -> fail (show issue)

    it "rejects an unparseable module (hard diagnostic, ADR-0080 §1)" do
      isLeft (reconstructModule "module where {") `shouldEqual` true

  describe "interpretType: pure shapes" do
    it "counts plain arrows" do
      sigOf "foreign import f :: Int -> Int -> Int" `shouldEqual` pure_ 2

    it "a constant foreign has arity 0" do
      sigOf "foreign import maxInt :: Int" `shouldEqual` pure_ 0

    it "peels forall and does not count arrows inside an argument" do
      -- the fromNumberImpl idiom: a rank-2 function argument stays one parameter
      sigOf "foreign import fromNumberImpl :: (forall a. a -> Maybe a) -> Number -> Int"
        `shouldEqual` pure_ 2

    it "peels a kind annotation" do
      sigOf "foreign import k :: (Int -> Int) :: Type" `shouldEqual` pure_ 1

    it "an applied non-effect constructor is pure" do
      sigOf "foreign import m :: Int -> Maybe Int" `shouldEqual` pure_ 1

    it "record and row arguments are opaque single parameters" do
      sigOf "foreign import r :: { a :: Int, b :: String } -> Int" `shouldEqual` pure_ 1

  describe "interpretType: effect return heads (retVsat)" do
    it "an Effect leaf's saturation returns a thunk" do
      sigOf "foreign import logImpl :: String -> Effect Unit" `shouldEqual` retThunk 1

    it "an effect thunk is arity 0, retVsat" do
      sigOf "foreign import now :: Effect Number" `shouldEqual` retThunk 0

    it "peels parens around the whole type" do
      sigOf "foreign import g :: (Int -> Effect Int)" `shouldEqual` retThunk 1

    it "sees Effect through a qualified name" do
      sigOf "foreign import q :: Int -> E.Effect Unit" `shouldEqual` retThunk 1

    it "an ST return head is retVsat like Effect" do
      sigOf "foreign import newRef :: forall r a. a -> ST r (STRef r a)"
        `shouldEqual` retThunk 1

  describe "interpretType: uncurried families (ADR-0034 dual, ADR-0039 adapters)" do
    it "a bare EffectFn2 value performs on saturation (arity 2, vsat)" do
      -- the case the single-bit interpreter got wrong: arity 0, not effectful
      sigOf "foreign import fooImpl :: EffectFn2 A B C" `shouldEqual` performs 2

    it "runEffectFn2 returns an Effect thunk (arity 3, retVsat — not vsat)" do
      sigOf "foreign import runEffectFn2 :: EffectFn2 A B C -> A -> B -> Effect C"
        `shouldEqual` retThunk 3

    it "mkEffectFn2 performs on saturation like boot's mkSTFn (arity 3, vsat)" do
      sigOf "foreign import mkEffectFn2 :: (A -> B -> Effect C) -> EffectFn2 A B C"
        `shouldEqual` performs 3

    it "a bare STFn2 value performs on saturation (arity 2, vsat)" do
      sigOf "foreign import barImpl :: STFn2 A B R C" `shouldEqual` performs 2

    it "runSTFn3 returns an ST thunk (arity 4, retVsat)" do
      sigOf "foreign import runSTFn3 :: STFn3 A B C R D -> A -> B -> C -> ST R D"
        `shouldEqual` retThunk 4

    it "a pure Fn2 value is arity 2, no effect" do
      sigOf "foreign import baz :: Fn2 A B C" `shouldEqual` pure_ 2

    it "runFn4 is a pure arity-5 saturated apply" do
      sigOf "foreign import runFn4 :: Fn4 A B C D E -> A -> B -> C -> D -> E"
        `shouldEqual` pure_ 5

    it "an EffectFn head as a curried return adds its uncurried arity" do
      -- `A -> EffectFn2 B C D`: 1 curried arrow + 2 uncurried, all saturated performs
      sigOf "foreign import h :: A -> EffectFn2 B C D" `shouldEqual` performs 3

  describe "interpretType: documented limitations" do
    it "EffectFnN with no digit is not a family (opaque)" do
      sigOf "foreign import ef :: EffectFn Int Int" `shouldEqual` pure_ 0

    it "a synonym hiding Effect is NOT seen through (CST not desugared)" do
      -- `type Eff = Effect` in scope would make this retVsat; the head name is `Eff`.
      sigOf "foreign import s :: Int -> Eff Unit" `shouldEqual` pure_ 1
