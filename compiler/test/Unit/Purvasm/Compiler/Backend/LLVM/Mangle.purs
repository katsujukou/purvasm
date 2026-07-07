-- | The LLVM backend's immediate/symbol encodings must be byte-identical to boot's `codegen_llvm.ml`,
-- | since the `.ll` byte-identity gate (ADR-0082 §2) compares the emitted text directly. Expected
-- | values are anchored on boot's actual `--no-opt` output (e.g. `@pv_g_Slice1_2eidentInt$root`,
-- | `@pv_g_go$root`, `@pv_g_Example_2eFib_2eLib_2efib$root`).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Mangle where

import Prelude

import Purvasm.Compiler.Backend.LLVM.Mangle (escapeIdent, imm, immBool, immInt, immUnit, mangle, mangleForeign)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Mangle" do
  describe "imm / immInt / immBool / immUnit" do
    it "tags small non-negative payloads as (p << 1) | 1" do
      imm 0 `shouldEqual` "1"
      imm 1 `shouldEqual` "3"
      immInt 42 `shouldEqual` "85"

    it "renders the Unit/sentinel and Booleans" do
      immUnit `shouldEqual` "1"
      immBool false `shouldEqual` "1"
      immBool true `shouldEqual` "3"

    it "renders negatives as signed i64 two's-complement decimal" do
      immInt (-1) `shouldEqual` "-1"
      immInt (-2) `shouldEqual` "-3"

    it "renders the 32-bit boundaries (crossing into the high i64 word)" do
      immInt 1073741824 `shouldEqual` "2147483649" -- 2^30 → 2^31 + 1
      immInt 2147483647 `shouldEqual` "4294967295" -- maxInt → 2^32 - 1
      immInt (-2147483648) `shouldEqual` "-4294967295" -- minInt → -(2^32 - 1)

  describe "escapeIdent" do
    it "passes alphanumerics through" do
      escapeIdent "fib" `shouldEqual` "fib"
      escapeIdent "go" `shouldEqual` "go"

    it "escapes every non-alphanumeric byte as _HH, including '_' itself" do
      escapeIdent "A.B" `shouldEqual` "A_2eB"
      escapeIdent "A_B" `shouldEqual` "A_5fB"
      escapeIdent "Main.main" `shouldEqual` "Main_2emain"
      escapeIdent "Example.Fib.Lib.fib" `shouldEqual` "Example_2eFib_2eLib_2efib"

  describe "mangle / mangleForeign" do
    it "prefixes a global's symbol base with pv_g_ (matches boot's $root symbols)" do
      mangle "Slice1.identInt" `shouldEqual` "pv_g_Slice1_2eidentInt"
      mangle "go" `shouldEqual` "pv_g_go"
      mangle "Example.Fib.Lib.fib" `shouldEqual` "pv_g_Example_2eFib_2eLib_2efib"

    it "prefixes a native foreign leaf's symbol with pvf_" do
      mangleForeign "Data.Show.showNumberImpl"
        `shouldEqual` "pvf_Data_2eShow_2eshowNumberImpl"
