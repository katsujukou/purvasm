-- | The native link step's parses of `purvasm.h`
-- | ([ADR-0111](../../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §1.1's retained
-- | API and §5's foreign-ABI version reference).
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
import Purvasm.CLI.NativeLink (foreignAbiStamp, foreignAuthorApi, foreignManifest, generatedBanner)
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
    , " * NOT exposed here: the runtime lifecycle (`pv_runtime_new`/`pv_runtime_free`), nor the"
    , " * host-control `pv_runtime_set_guest_argv`."
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
    , "#define PV_FOREIGN_ABI_VERSION 1"
    , "void PV_FOREIGN_ABI_SYM(PV_FOREIGN_ABI_VERSION)(void);"
    , "static void (*const pv_foreign_abi_stamp)(void) PVF_USED = PV_FOREIGN_ABI_SYM(PV_FOREIGN_ABI_VERSION);"
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

-- | The header with its version `#define` spelled as `token`.
versioned :: String -> Either String String
versioned token =
  foreignAbiStamp
    (String.replaceAll (String.Pattern "ABI_VERSION 1") (String.Replacement ("ABI_VERSION " <> token)) (header realBanner))

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
          -- The host-control entry is named in the prose for exactly the reason it must never be in the
          -- allowlist (ADR-0110 §4(a) Correction): exporting it would let a loaded provider
          -- reconfigure the runtime that hosts it.
          Array.elem "pv_runtime_set_guest_argv" names `shouldEqual` false
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

    it "leaves the version reference to `foreignAbiStamp`" do
      -- The stamp is declared through a macro paste, so no reader of declaration lines can name it —
      -- and the `pv_foreign_abi_stamp` *definition* line must not be mistaken for one either, or the
      -- allowlist would export a per-object static that no provider can resolve against.
      case foreignAuthorApi (header realBanner) of
        Right names -> do
          Array.elem "pv_foreign_abi_stamp" names `shouldEqual` false
          Array.elem "pv_foreign_abi_v1" names `shouldEqual` false
        Left e -> shouldEqual e "the parse to succeed"

  describe "foreignManifest" do
    it "writes the banner, then one key per line, sorted, ending in a newline" do
      foreignManifest [ "pvf_B_2eb", "pvf_A_2ea" ]
        `shouldEqual` Right "purvasm-foreign-manifest:v1\nA.a\nB.b\n"

    it "recovers a key EXACTLY, including escapes a diagnostic demangle would miss" do
      -- `App.foo'` links as `pvf_App_2efoo_27`. Written as `App.foo_27` it would re-mangle to
      -- `pvf_App_2efoo_5f27` in the reader, and the eager check would report a missing provider for
      -- a key this very link had just resolved.
      foreignManifest [ "pvf_App_2efoo_27" ]
        `shouldEqual` Right "purvasm-foreign-manifest:v1\nApp.foo'\n"

    it "is empty but well-formed when the workspace provides nothing" do
      foreignManifest [] `shouldEqual` Right "purvasm-foreign-manifest:v1\n"

    it "refuses a symbol whose key cannot be recovered, rather than writing an approximation" do
      -- The manifest is re-mangled by its reader, so an inexact key is a wrong contract — unlike a
      -- diagnostic, where an approximate name is better than none.
      isLeft (foreignManifest [ "pvf_App_2efoo_2" ]) `shouldEqual` true
      isLeft (foreignManifest [ "not_a_pvf_symbol" ]) `shouldEqual` true

  describe "foreignAbiStamp" do
    it "derives the version reference from the header's #define" do
      foreignAbiStamp (header realBanner) `shouldEqual` Right "pv_foreign_abi_v1"

    it "follows a bumped version" do
      -- The whole point of deriving it: a host exporting `…_v1` while providers reference `…_v2`
      -- would refuse every provider, and the two live in the same file precisely so they cannot drift.
      versioned "7" `shouldEqual` Right "pv_foreign_abi_v7"

    it "refuses a header that defines no version" do
      isLeft (foreignAbiStamp (String.replaceAll (String.Pattern "#define PV_FOREIGN_ABI_VERSION 1") (String.Replacement "") (header realBanner)))
        `shouldEqual` true

    it "refuses a header that defines it twice" do
      isLeft (foreignAbiStamp (header realBanner <> "\n#define PV_FOREIGN_ABI_VERSION 2\n")) `shouldEqual` true

    it "refuses a non-integer version" do
      isLeft (versioned "v1") `shouldEqual` true

    it "carries a multi-digit version through unchanged" do
      versioned "12" `shouldEqual` Right "pv_foreign_abi_v12"

    -- The version is a token the header PASTES, not a number: normalising it (parse, then reprint)
    -- silently renames the symbol, and the host would then export a name no provider references —
    -- which fails as "symbol not found" at every `dlopen`, far from the header that caused it.
    it "refuses a leading zero rather than normalising it" do
      -- `01` makes providers reference `pv_foreign_abi_v01`; answering `pv_foreign_abi_v1` here is
      -- the exact mismatch, and it is invisible until a provider fails to load.
      isLeft (versioned "01") `shouldEqual` true

    it "refuses a signed version, which cannot be part of an identifier at all" do
      isLeft (versioned "+1") `shouldEqual` true
      isLeft (versioned "-1") `shouldEqual` true

    it "is not fooled by a macro whose name merely starts with it" do
      -- `…_VERSION_MINOR 3` shares the whole prefix; taking it would export `pv_foreign_abi_v3`.
      isLeft (foreignAbiStamp (String.replaceAll (String.Pattern "ABI_VERSION 1") (String.Replacement "ABI_VERSION_MINOR 3") (header realBanner)))
        `shouldEqual` true
