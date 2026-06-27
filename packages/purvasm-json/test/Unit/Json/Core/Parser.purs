module Test.Unit.Json.Core.Parser (spec) where

import Prelude

import Data.Array (length, range, replicate)
import Data.Either (Either(..), isLeft)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Json.Core.Parser (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Unit.Json.Core.Rep (TJ(..), builder)

p :: String -> Either String TJ
p = parse builder

spec :: Spec Unit
spec = describe "Json.Core.Parser" do
  describe "literals" do
    it "parses null/true/false" do
      p "null" `shouldEqual` Right TNull
      p "true" `shouldEqual` Right (TBool true)
      p "false" `shouldEqual` Right (TBool false)
    it "rejects a truncated literal" do
      p "tru" `shouldSatisfy` isLeft

  describe "numbers" do
    it "parses integers, signs and zero" do
      p "0" `shouldEqual` Right (TNum 0.0)
      p "42" `shouldEqual` Right (TNum 42.0)
      p "-7" `shouldEqual` Right (TNum (-7.0))
    it "parses fractions and exponents" do
      p "3.14" `shouldEqual` Right (TNum 3.14)
      p "1e3" `shouldEqual` Right (TNum 1000.0)
      p "2.5E-2" `shouldEqual` Right (TNum 0.025)
      p "-0.5e+1" `shouldEqual` Right (TNum (-5.0))
    it "rejects malformed numbers" do
      p "01" `shouldSatisfy` isLeft -- leading zero then digit
      p "1." `shouldSatisfy` isLeft -- fraction needs a digit
      p "1e" `shouldSatisfy` isLeft -- exponent needs a digit
      p "-" `shouldSatisfy` isLeft
      p ".5" `shouldSatisfy` isLeft -- must start with a digit

  describe "strings" do
    it "parses plain strings" do
      p "\"\"" `shouldEqual` Right (TStr "")
      p "\"abc\"" `shouldEqual` Right (TStr "abc")
    it "parses simple escapes" do
      p "\"a\\nb\"" `shouldEqual` Right (TStr "a\nb")
      p "\"\\\"\\\\\\/\\b\\f\\r\\t\"" `shouldEqual` Right (TStr "\"\\/\x08\x0C\r\t")
    it "parses BMP \\u escapes" do
      p "\"\\u0041\\u00e9\"" `shouldEqual` Right (TStr "Aé")
    it "combines a surrogate pair into one code point" do
      p "\"\\uD83D\\uDE00\"" `shouldEqual` Right (TStr "\x1F600")
    it "rejects bad strings" do
      p "\"ab" `shouldSatisfy` isLeft -- unterminated
      p "\"\\q\"" `shouldSatisfy` isLeft -- invalid escape
      p "\"\\uD83D\"" `shouldSatisfy` isLeft -- lone high surrogate
      p "\"\\uDC00\"" `shouldSatisfy` isLeft -- lone low surrogate
      p "\"\\u00G0\"" `shouldSatisfy` isLeft -- non-hex digit

  describe "arrays" do
    it "parses empty and nested arrays" do
      p "[]" `shouldEqual` Right (TArr [])
      p "[1,[2,3]]" `shouldEqual`
        Right (TArr [ TNum 1.0, TArr [ TNum 2.0, TNum 3.0 ] ])
    it "rejects a trailing comma" do
      p "[1,]" `shouldSatisfy` isLeft

  describe "objects" do
    it "parses empty objects and preserves member order" do
      p "{}" `shouldEqual` Right (TObj [])
      p "{\"a\":1,\"b\":[2]}" `shouldEqual`
        Right (TObj [ Tuple "a" (TNum 1.0), Tuple "b" (TArr [ TNum 2.0 ]) ])
    it "rejects a missing colon" do
      p "{\"a\" 1}" `shouldSatisfy` isLeft

  describe "whitespace and framing" do
    it "ignores insignificant whitespace" do
      p "  [ 1 ,\n2 ]\t" `shouldEqual` Right (TArr [ TNum 1.0, TNum 2.0 ])
    it "rejects empty input and trailing content" do
      p "" `shouldSatisfy` isLeft
      p "1 2" `shouldSatisfy` isLeft

  describe "stack safety (long sibling lists)" do
    let
      n = 100000
      arrLen = case _ of
        Right (TArr xs) -> length xs
        _ -> -1
      objLen = case _ of
        Right (TObj kvs) -> length kvs
        _ -> -1
    it "parses a very large flat array without overflowing the stack" do
      arrLen (p ("[" <> joinWith "," (replicate n "1") <> "]")) `shouldEqual` n
    it "parses a very large flat object without overflowing the stack" do
      let input = "{" <> joinWith "," (map (\i -> "\"k" <> show i <> "\":1") (range 0 (n - 1))) <> "}"
      objLen (p input) `shouldEqual` n
