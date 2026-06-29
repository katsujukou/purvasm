module Test.Unit.Json.Core.Utf8 (spec) where

import Prelude

import Json.Core.Utf8 (encodeCp)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Pin `encodeCp` to the canonical UTF-8 bytes for 1-/2-/3-/4-byte code points. Canonical UTF-8 is
-- | unique, so this guarantees byte-parity with the ulib `Data.String.Internal.Utf8.putCp` encoder
-- | (ADR-0054) without importing the ulib (which stock `purs` cannot resolve).
spec :: Spec Unit
spec = describe "Json.Core.Utf8.encodeCp (canonical UTF-8)" do
  it "encodes 1-/2-/3-/4-byte code points" do
    encodeCp 0x41 `shouldEqual` [ 0x41 ] -- 'A'  (1 byte)
    encodeCp 0x00E9 `shouldEqual` [ 0xC3, 0xA9 ] -- 'é'  (2 bytes)
    encodeCp 0x20AC `shouldEqual` [ 0xE2, 0x82, 0xAC ] -- '€'  (3 bytes)
    encodeCp 0x1F600 `shouldEqual` [ 0xF0, 0x9F, 0x98, 0x80 ] -- '😀' (4 bytes)
  it "encodes the boundary code points of each length" do
    encodeCp 0x00 `shouldEqual` [ 0x00 ]
    encodeCp 0x7F `shouldEqual` [ 0x7F ]
    encodeCp 0x80 `shouldEqual` [ 0xC2, 0x80 ]
    encodeCp 0x7FF `shouldEqual` [ 0xDF, 0xBF ]
    encodeCp 0x800 `shouldEqual` [ 0xE0, 0xA0, 0x80 ]
    encodeCp 0xFFFF `shouldEqual` [ 0xEF, 0xBF, 0xBF ]
    encodeCp 0x10000 `shouldEqual` [ 0xF0, 0x90, 0x80, 0x80 ]
    encodeCp 0x10FFFF `shouldEqual` [ 0xF4, 0x8F, 0xBF, 0xBF ]
