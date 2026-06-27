module Test.Unit.Json.Core.Printer (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Json.Core.Parser (parse)
import Json.Core.Printer (print)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Unit.Json.Core.Rep (TJ(..), builder, eliminator)

pr :: TJ -> String
pr = print eliminator

spec :: Spec Unit
spec = describe "Json.Core.Printer" do
  describe "print" do
    it "renders scalars" do
      pr TNull `shouldEqual` "null"
      pr (TBool true) `shouldEqual` "true"
      pr (TBool false) `shouldEqual` "false"
      pr (TNum 1.0) `shouldEqual` "1.0"
    it "escapes strings" do
      pr (TStr "a\"b\\c") `shouldEqual` "\"a\\\"b\\\\c\""
      pr (TStr "\n\t") `shouldEqual` "\"\\n\\t\""
      pr (TStr "\x01") `shouldEqual` "\"\\u0001\""
    it "renders arrays and objects compactly" do
      pr (TArr [ TNum 1.0, TBool true ]) `shouldEqual` "[1.0,true]"
      pr (TObj [ Tuple "a" (TNum 1.0), Tuple "b" TNull ]) `shouldEqual`
        "{\"a\":1.0,\"b\":null}"

  describe "round-trip (parse <<< print)" do
    let
      roundtrips v = parse builder (pr v) `shouldEqual` Right v
    it "holds for nested values, escapes and astral code points" do
      roundtrips (TNum (-12.5))
      roundtrips (TStr "héllo\n\"world\"\t\x1F600")
      roundtrips
        ( TObj
            [ Tuple "xs" (TArr [ TNum 1.0, TNum 2.0 ])
            , Tuple "nested" (TObj [ Tuple "ok" (TBool true) ])
            , Tuple "nil" TNull
            ]
        )
