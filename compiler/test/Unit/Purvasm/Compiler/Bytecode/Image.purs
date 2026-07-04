-- | Invariants of the JSON encoding the types cannot enforce: the serialiser matches
-- | `Yojson.Safe.to_string` (compact, insertion-ordered, standard escaping), and a
-- | `Number` is encoded as its exact IEEE-754 bits in a *signed* 64-bit decimal string
-- | (boot's `Int64.to_string (Int64.bits_of_float f)`).
module Test.Unit.Purvasm.Compiler.Bytecode.Image where

import Prelude

import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Image (Json(..), floatToJson, imageToString, stringify)
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Bytecode.Image" do
  describe "floatToJson" do
    it "encodes a Number as its signed-64-bit IEEE-754 bit pattern (decimal string)" do
      stringify (floatToJson 1.0) `shouldEqual` "\"4607182418800017408\""
      stringify (floatToJson 0.0) `shouldEqual` "\"0\""
      stringify (floatToJson 0.5) `shouldEqual` "\"4602678819172646912\""
      stringify (floatToJson 2.5) `shouldEqual` "\"4612811918334230528\""
      stringify (floatToJson (-1.0)) `shouldEqual` "\"-4616189618054758400\""

  describe "stringify" do
    it "emits compact, insertion-ordered objects and arrays (no spaces)" do
      stringify (JObj [ "b" /\ JInt 2, "a" /\ JArr [ JInt 1, JBool true ] ])
        `shouldEqual` """{"b":2,"a":[1,true]}"""

    it "escapes quote, backslash, and the named control characters" do
      stringify (JStr "a\"b\\c") `shouldEqual` "\"a\\\"b\\\\c\""
      stringify (JStr "x\ny\tz") `shouldEqual` "\"x\\ny\\tz\""

    it "escapes other control characters as \\u00XX (lowercase, 4 digits)" do
      stringify (JStr "\x01") `shouldEqual` "\"\\u0001\""

  describe "imageToString" do
    it "serialises a linked image with version/gdefs/main/effect keys in order" do
      imageToString
        { gdefs: [ "M.x" /\ Gcaf [ PushInt 1, Return ] ]
        , main: [ Load "M.x", Return ]
        , isEffect: false
        }
        `shouldEqual`
          """{"version":3,"gdefs":[["M.x",["caf",[["pi",1],["rt"]]]]],"main":[["ld","M.x"],["rt"]],"effect":false}"""
