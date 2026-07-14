module Test.Unit.Purvasm.UlibTools.UnicodeData (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Purvasm.UlibTools.UnicodeData (parsePin, parseUnicodeData, renderCaseMapModule, renderPin, selfCheck)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Purvasm.UlibTools.UnicodeData" do
    describe "parseUnicodeData" do
      it "reads fields 0/12/13, one entry per non-blank line" do
        parseUnicodeData ucdSample `shouldEqual` Right
          { upper: [ Tuple 0x0061 0x0041 ]
          , lower: [ Tuple 0x0041 0x0061 ]
          }
      it "treats a blank mapping field as no mapping in that direction" do
        parseUnicodeData "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;;\n"
          `shouldEqual` Right { upper: [], lower: [] }
      it "sorts the result by code point even if the input is out of order" do
        let
          src =
            "0062;LATIN SMALL LETTER B;Ll;0;L;;;;;N;;;0042;;\n"
              <> "0061;LATIN SMALL LETTER A;Ll;0;L;;;;;N;;;0041;;\n"
        parseUnicodeData src `shouldEqual` Right
          { upper: [ Tuple 0x0061 0x0041, Tuple 0x0062 0x0042 ]
          , lower: []
          }
      it "fails on a non-hex code point" do
        isLeft (parseUnicodeData "ZZZZ;X;Lu;0;L;;;;;N;;;;;\n") `shouldEqual` true
      it "fails on a non-hex mapping field" do
        isLeft (parseUnicodeData "0041;X;Lu;0;L;;;;;N;;;;ZZZZ;\n") `shouldEqual` true
      it "fails on a truncated record (fewer than 15 fields), not just a blank field 12/13" do
        -- Only 10 fields -- a naive out-of-range lookup for field 12/13 would treat this the same
        -- as a genuinely blank field ("no mapping"), silently accepting a truncated download.
        isLeft (parseUnicodeData "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N\n") `shouldEqual` true
      it "fails on a record with more than 15 fields" do
        isLeft (parseUnicodeData "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0061;;EXTRA\n")
          `shouldEqual` true

    describe "selfCheck" do
      it "passes on a table satisfying every documented invariant" do
        selfCheck validTables `shouldEqual` Right unit
      it "fails when a table is not sorted by code point" do
        isLeft (selfCheck validTables { upper = [ Tuple 0x017F 0x0053, Tuple 0x0061 0x0041 ] })
          `shouldEqual` true
      it "fails when a table has a duplicate code point key" do
        isLeft (selfCheck validTables { upper = validTables.upper <> [ Tuple 0x0061 0x0041 ] })
          `shouldEqual` true
      it "fails when the ASCII round-trip is missing" do
        isLeft (selfCheck validTables { upper = [ Tuple 0x0131 0x0049, Tuple 0x017F 0x0053 ] })
          `shouldEqual` true
      it "fails when U+00DF has a spurious simple uppercase entry" do
        isLeft (selfCheck validTables { upper = validTables.upper <> [ Tuple 0x00DF 0x1E9E ] })
          `shouldEqual` true
      it "fails when an unmapped code point (e.g. NUL) is spuriously present" do
        isLeft (selfCheck validTables { upper = validTables.upper <> [ Tuple 0x0000 0x0000 ] })
          `shouldEqual` true

    describe "renderCaseMapModule" do
      it "attributes the UCD version and renders the flattened tables" do
        let
          rendered = renderCaseMapModule { ucdVersion: "15.0.0" }
            { upper: [ Tuple 0x0061 0x0041 ], lower: [ Tuple 0x0041 0x0061 ] }
        contains (Pattern "Unicode Character Database 15.0.0") rendered `shouldEqual` true
        contains (Pattern "module Data.String.Internal.CaseMap") rendered `shouldEqual` true
        contains (Pattern "tableUpper") rendered `shouldEqual` true
        contains (Pattern "97, 65") rendered `shouldEqual` true
        contains (Pattern "tableLower") rendered `shouldEqual` true
        contains (Pattern "65, 97") rendered `shouldEqual` true

    describe "parsePin / renderPin" do
      it "round-trips ucdVersion/sha256 through render then parse" do
        let pin = { ucdVersion: "15.0.0", sha256: "abc123" }
        parsePin (renderPin pin) `shouldEqual` Right pin
      it "fails when sha256 is missing" do
        isLeft (parsePin """{"ucdVersion":"15.0.0"}""") `shouldEqual` true
      it "fails on a non-object top level" do
        isLeft (parsePin "[]") `shouldEqual` true
  where
  -- One row declaring `A`'s Simple_Lowercase_Mapping, one declaring `a`'s Simple_Uppercase_Mapping.
  ucdSample =
    "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0061;\n"
      <> "0061;LATIN SMALL LETTER A;Ll;0;L;;;;;N;;;0041;;\n"

  -- Covers every code point `selfCheck` asserts on, and nothing it forbids.
  validTables =
    { upper: [ Tuple 0x0061 0x0041, Tuple 0x0131 0x0049, Tuple 0x017F 0x0053 ]
    , lower: [ Tuple 0x0041 0x0061, Tuple 0x0130 0x0069, Tuple 0x1E9E 0x00DF ]
    }
