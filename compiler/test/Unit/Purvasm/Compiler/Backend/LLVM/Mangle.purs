-- | The LLVM backend's immediate/symbol encodings, pinned by L2-owned goldens (provenance: boot's
-- | `codegen_llvm.ml` `--no-opt` output, e.g. `@pv_g_Slice1_2eidentInt$root`, `@pv_g_go$root`,
-- | `@pv_g_Example_2eFib_2eLib_2efib$root`). Mangling is link-time ABI (`@pvf_` leaf symbols must
-- | match the runtime/ulib `.c` exports) and immediates are the value representation, so these are
-- | NOT freely re-baselineable emission cosmetics (ADR-0104 §4).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Mangle where

import Prelude

import Data.Maybe (Maybe(..))
import Purvasm.Abi.Mangle (unescapeIdent)
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

  describe "unescapeIdent" do
    it "inverts escapeIdent over the escapes that occur in practice" do
      unescapeIdent "A_2eB" `shouldEqual` Just "A.B"
      unescapeIdent "A_5fB" `shouldEqual` Just "A_B"
      unescapeIdent "Example_2eFib_2eLib_2efib" `shouldEqual` Just "Example.Fib.Lib.fib"

    it "inverts it over the ones that do NOT, which is the point of being exact" do
      -- A prime is a perfectly ordinary PureScript identifier and escapes to `_27`. A partial
      -- inverse that knew only `_2e`/`_5f` would leave it as `foo_27`, which re-mangles to
      -- `foo_5f27` — a different symbol, reported as a missing provider (ADR-0111 §4's manifest).
      unescapeIdent (escapeIdent "App.foo'") `shouldEqual` Just "App.foo'"
      unescapeIdent (escapeIdent "a-b") `shouldEqual` Just "a-b"
      unescapeIdent (escapeIdent "x$y") `shouldEqual` Just "x$y"
      unescapeIdent (escapeIdent "M.<>") `shouldEqual` Just "M.<>"

    it "round-trips every key the other tests here name" do
      let keys = [ "fib", "A.B", "A_B", "Main.main", "Data.Show.showNumberImpl", "App.foo'" ]
      map (unescapeIdent <<< escapeIdent) keys `shouldEqual` map Just keys

    it "refuses what escaping could not have produced" do
      -- `_61` is well-formed hex for 'a', and escaping never emits it (alphanumerics pass through).
      -- Accepting it would give one key two spellings and cost the encoding its injectivity.
      unescapeIdent "_61" `shouldEqual` Nothing
      unescapeIdent "a_2" `shouldEqual` Nothing -- truncated escape
      unescapeIdent "a_2E" `shouldEqual` Nothing -- uppercase hex is not what the writer emits
      unescapeIdent "a_zz" `shouldEqual` Nothing -- not hex at all

  describe "mangle / mangleForeign" do
    it "prefixes a global's symbol base with pv_g_ (matches boot's $root symbols)" do
      mangle "Slice1.identInt" `shouldEqual` "pv_g_Slice1_2eidentInt"
      mangle "go" `shouldEqual` "pv_g_go"
      mangle "Example.Fib.Lib.fib" `shouldEqual` "pv_g_Example_2eFib_2eLib_2efib"

    it "prefixes a native foreign leaf's symbol with pvf_" do
      mangleForeign "Data.Show.showNumberImpl"
        `shouldEqual` "pvf_Data_2eShow_2eshowNumberImpl"
