-- | Invariants of the JSON encoding the types cannot enforce: the serialiser matches
-- | `Yojson.Safe.to_string` (compact, insertion-ordered, standard escaping), and a
-- | `Number` is encoded as its exact IEEE-754 bits in a *signed* 64-bit decimal string
-- | (boot's `Int64.to_string (Int64.bits_of_float f)`).
module Test.Unit.Purvasm.Compiler.Bytecode.Image where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.String as String
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Image (Json(..), floatToJson, imageToString, imageToStringWithArities, primTag, stringify)
import Purvasm.Compiler.Bytecode.Instruction (Instruction(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
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

  describe "primTag" do
    it "tags the Int bitwise family with boot's constructor names (Image.prim_tags)" do
      primTag AndInt `shouldEqual` "AndInt"
      primTag OrInt `shouldEqual` "OrInt"
      primTag XorInt `shouldEqual` "XorInt"
      primTag ShlInt `shouldEqual` "ShlInt"
      primTag ShrInt `shouldEqual` "ShrInt"
      primTag ZshrInt `shouldEqual` "ZshrInt"
      primTag ComplementInt `shouldEqual` "ComplementInt"

  describe "imageToString" do
    it "serialises a linked image with version/gdefs/main/effect keys in order" do
      imageToString
        { gdefs: [ "M.x" /\ Gcaf [ PushInt 1, Return ] ]
        , main: [ Load "M.x", Return ]
        , isEffect: false
        }
        `shouldEqual`
          """{"version":3,"gdefs":[["M.x",["caf",[["pi",1],["rt"]]]]],"main":[["ld","M.x"],["rt"]],"effect":false}"""

    it "writes a foreign reference without its arity, the only form boot's reader accepts" do
      -- The legacy form is not merely "the old one": boot's VM is frozen, so this encoding is fixed
      -- for as long as the two runners coexist (ADR-0110 §6, step C).
      imageToString
        { gdefs: []
        , main: [ ForeignRef "Data.Show.showIntImpl", Return ]
        , isEffect: false
        }
        `shouldEqual`
          """{"version":3,"gdefs":[],"main":[["fr","Data.Show.showIntImpl"],["rt"]],"effect":false}"""

  describe "imageToStringWithArities" do
    it "stamps the owned VM's version and writes each leaf's physical arity" do
      imageToStringWithArities (Map.fromFoldable [ "Data.Show.showIntImpl" /\ 1 ])
        { gdefs: []
        , main: [ ForeignRef "Data.Show.showIntImpl", Return ]
        , isEffect: false
        }
        `shouldEqual`
          Right """{"version":5,"gdefs":[],"main":[["fr","Data.Show.showIntImpl",1],["rt"]],"effect":false}"""

    it "finds a leaf nested inside a closure, not only one in the main chunk" do
      -- The check walks the same tree the writer does. A leaf reachable only from inside a closure or
      -- a recursive group is the case where a shallower check would pass and then write the
      -- impossible arity — refusal has to see everything the writer sees.
      isLeft
        ( imageToStringWithArities Map.empty
            { gdefs: [ "M.f" /\ Gfun [ "x" ] [ Closure [ "y" ] [ ForeignRef "M.leaf", Return ], Return ] ]
            , main: [ Return ]
            , isEffect: false
            }
        ) `shouldEqual` true

    it "finds a leaf inside a recursive group's member chunk" do
      isLeft
        ( imageToStringWithArities Map.empty
            { gdefs: []
            , main: [ MakeRec [ "g" /\ [ ForeignRef "M.leaf", Return ] ], Return ]
            , isEffect: false
            }
        ) `shouldEqual` true

    it "names every leaf it cannot describe, so one build reports them all" do
      case
        imageToStringWithArities Map.empty
          { gdefs: []
          , main: [ ForeignRef "M.a", ForeignRef "M.b", Return ]
          , isEffect: false
          }
        of
        Left e -> do
          String.contains (String.Pattern "M.a") e `shouldEqual` true
          String.contains (String.Pattern "M.b") e `shouldEqual` true
        Right _ -> shouldEqual "a rejection" "an image"

    it "writes an arity-0 leaf as 0 — a foreign constant is not a missing fact" do
      -- `leafClosureArity` answers 0 for a non-effect leaf with no arguments. Conflating that with
      -- "no arity known" would refuse a legitimate program.
      imageToStringWithArities (Map.fromFoldable [ "M.k" /\ 0 ])
        { gdefs: [], main: [ ForeignRef "M.k", Return ], isEffect: false }
        `shouldEqual`
          Right """{"version":5,"gdefs":[],"main":[["fr","M.k",0],["rt"]],"effect":false}"""
