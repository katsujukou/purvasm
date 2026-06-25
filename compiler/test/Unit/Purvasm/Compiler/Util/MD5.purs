-- | `md5Hex` must match OCaml's `Digest.string` (boot's `.pmi` hash). Checked against the
-- | RFC 1321 test vectors plus the actual interface surface boot hashes for `DiaA`.
module Test.Unit.Purvasm.Compiler.Util.MD5 where

import Prelude

import Purvasm.Compiler.Util.MD5 (md5Hex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Util.MD5" do
  it "matches the RFC 1321 test vectors" do
    md5Hex "" `shouldEqual` "d41d8cd98f00b204e9800998ecf8427e"
    md5Hex "a" `shouldEqual` "0cc175b9c0f1b6a831c399e269772661"
    md5Hex "abc" `shouldEqual` "900150983cd24fb0d6963f7d28e17f72"
    md5Hex "message digest" `shouldEqual` "f96b697d7cb7938d525a2f31aaf161d0"
    md5Hex "abcdefghijklmnopqrstuvwxyz" `shouldEqual` "c3fcd3d76192e4007dfb496cca67e13b"

  it "matches across a 64-byte block boundary" do
    md5Hex "The quick brown fox jumps over the lazy dog"
      `shouldEqual` "9e107d9d372bb6826bd81d3542a419d6"
    md5Hex "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
      `shouldEqual` "57edf4a22be3c955ac49da2e2107b67a"

  it "matches boot's DiaA interface surface hash" do
    md5Hex "DiaA.Two:caf\nDiaA.both:caf" `shouldEqual` "abfec547bb4356605e4c57f967084fce"
