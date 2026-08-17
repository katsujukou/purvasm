-- | The native link step's parse of `purvasm.h`
-- | ([ADR-0111](../../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §1.1).
-- |
-- | This is a **safety boundary**, not a convenience: its result becomes the export allowlist of an
-- | executable that hosts `dlopen`ed providers, so anything it wrongly includes is something a guest
-- | can reach, and anything it wrongly drops is a provider that fails to load. Both directions are
-- | tested here, on inlined header fixtures rather than through a native build, because a native
-- | build can only show the *happy* path — a header that has drifted is precisely the case no
-- | successful link would reveal.
module Test.Unit.Purvasm.CLI.NativeLink (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.String as String
import Purvasm.CLI.NativeLink (foreignAuthorApi, generatedBanner)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | A header in the shape of the real one: prose that mentions API names, declarations above the
-- | banner, and generated-code declarations below it.
header :: String -> String
header banner =
  String.joinWith "\n"
    [ "/*"
    , " * purvasm.h — the stable C-ABI a native `foreign` leaf is written against."
    , " *"
    , " * To keep a value live across an allocation, root it:"
    , " *     PVWord mark = pv_frame(ctx);"
    , " *     v = pv_get(ctx, h);"
    , " *"
    , " * NOT exposed here: the runtime lifecycle (`pv_runtime_new`/`pv_runtime_free`)."
    , " */"
    , "#ifndef PURVASM_H"
    , "#define PURVASM_H"
    , ""
    , "PVWord pv_frame(PVContext *ctx);"
    , "PVWord pv_root(PVContext *ctx, PVWord v);"
    , "size_t pv_str_copy(PVContext *ctx, PVWord s, uint8_t *dst, size_t cap);"
    , "PVWord pv_int(int32_t v);"
    , ""
    , "#define PVF_EXPORT(ident) PVF_CAT(PVF_CAT(pvf_, PVF_MODULE), PVF_CAT(_2e, ident))"
    , ""
    , banner
    , " * A `PVContext*` points to storage whose FIRST BYTES are a `pv_ctx_header`."
    , " */"
    , "typedef struct pv_ctx_header {"
    , "  uint64_t *roots_base;"
    , "} pv_ctx_header;"
    , ""
    , "void pv_abi_check(uint32_t version);"
    , "void pv_settle(PVContext *ctx);"
    , ""
    , "#endif"
    ]

realBanner :: String
realBanner = " * ==== " <> generatedBanner <> " (ADR-0079) — NOT part of the foreign-author API above."

spec :: Spec Unit
spec = describe "Purvasm.CLI.NativeLink" do
  describe "foreignAuthorApi" do
    it "takes the declarations above the banner" do
      foreignAuthorApi (header realBanner)
        `shouldEqual` Right [ "pv_frame", "pv_root", "pv_str_copy", "pv_int" ]

    it "excludes the generated-code ABI below the banner" do
      -- `pv_abi_check` and `pv_settle` are codegen's, and putting either in the allowlist would hand
      -- a guest-loaded provider an entry the header tells it never to call.
      case foreignAuthorApi (header realBanner) of
        Right names -> do
          Array.elem "pv_abi_check" names `shouldEqual` false
          Array.elem "pv_settle" names `shouldEqual` false
        Left e -> shouldEqual e "the parse to succeed"

    it "ignores prose that names an API without calling it" do
      -- The comment block mentions `pv_runtime_new`/`pv_runtime_free` as things a provider may NOT
      -- use, and shows `pv_frame(ctx)` in an example — the example names a real API, the prose does
      -- not, and neither may decide the list.
      case foreignAuthorApi (header realBanner) of
        Right names -> do
          Array.elem "pv_runtime_new" names `shouldEqual` false
          Array.elem "pv_runtime_free" names `shouldEqual` false
        Left e -> shouldEqual e "the parse to succeed"

    it "refuses a header with no banner, rather than parsing the whole file" do
      -- Fail-open here would put the generated-code ABI into the export allowlist of a program that
      -- hosts untrusted providers. This is the case the boundary exists for.
      isLeft (foreignAuthorApi (header " * (banner removed)")) `shouldEqual` true

    it "refuses a header with more than one banner" do
      isLeft (foreignAuthorApi (header realBanner <> "\n" <> realBanner)) `shouldEqual` true

    it "says how many banners it found" do
      case foreignAuthorApi (header " * (banner removed)") of
        Left e -> String.contains (String.Pattern "found 0") e `shouldEqual` true
        Right _ -> shouldEqual "a rejection" "a parse"
