module Test.Unit.Json.Core.Parser (spec) where

import Prelude

import Data.Array (length, range, replicate)
import Data.Either (Either(..), isLeft)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Json.Core.Parser (parse)
import Purvasm.String as PS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Unit.Json.Core.Rep (TJ(..), builder)

p :: String -> Either String TJ
p = parse builder

-- The parser yields a `String` of UTF-8 bytes (ADR-0006/0054). A non-ASCII PureScript string literal
-- is a *different* byte sequence on the JS test backend (UTF-16) than on native (UTF-8), so a literal
-- like `"é"` cannot be the expected value on both. Assert the canonical UTF-8 **bytes** instead —
-- representation-faithful on either backend (ADR-0040).
bytesOf :: String -> Array Int
bytesOf s = let n = PS.byteLength s in if n <= 0 then [] else map (PS.byteAt s) (range 0 (n - 1))

asBytes :: Either String TJ -> Either String (Array Int)
asBytes = map case _ of
  TStr s -> bytesOf s
  _ -> [ -1 ]

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
    it "parses BMP \\u escapes to canonical UTF-8 bytes" do
      asBytes (p "\"\\u0041\\u00e9\"") `shouldEqual` Right [ 0x41, 0xC3, 0xA9 ] -- A, é (U+00E9)
    it "combines a surrogate pair into one supplementary code point (UTF-8 bytes)" do
      asBytes (p "\"\\uD83D\\uDE00\"") `shouldEqual` Right [ 0xF0, 0x9F, 0x98, 0x80 ] -- U+1F600 😀
    it "decodes an escaped object key" do
      p "{\"a\\nb\":1}" `shouldEqual` Right (TObj [ Tuple "a\nb" (TNum 1.0) ])
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
