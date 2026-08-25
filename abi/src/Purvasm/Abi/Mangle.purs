-- | The native ABI's linker-symbol mangling — **one derivation, two consumers**.
-- |
-- | The LLVM backend emits these symbols and the owned VM's foreign frontier resolves them
-- | ([ADR-0111](../../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §2), so the two
-- | must agree exactly. They agree by construction only if there is one definition: a copy would
-- | drift, and the failure mode is silent — "no native provider" for a key that exists, under a
-- | symbol name nobody typed.
-- |
-- | The encodings are link-time ABI (ADR-0072 §2 / ADR-0073 §3) and are pinned by the compiler's
-- | goldens; this package is their home, below both consumers.
module Purvasm.Abi.Mangle
  ( ctorTag
  , escapeIdent
  , unescapeIdent
  , mangle
  , mangleForeign
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldMap)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Int.Bits ((.&.))
import Purvasm.Abi.Fnv1a64 (fnv1a64Bytes)
import Purvasm.Abi.Utf8 (utf8Bytes)

-- | The injective identifier escape (ADR-0072 §2): alphanumerics pass through, every other byte
-- | (including `_` itself) becomes `_HH` (lowercase hex), so distinct keys never collide
-- | (`A.B` → `A_2eB`, `A_B` → `A_5fB`). Qualified keys are ASCII, so iterating code units matches
-- | boot's byte iteration.
escapeIdent :: String -> String
escapeIdent key = foldMap escapeChar (toCharArray key)
  where
  escapeChar c =
    let
      code = toCharCode c
    in
      if isAlphaNum code then singleton c
      else "_" <> pad2 (toStringAs hexadecimal code)

  isAlphaNum code =
    (code >= 48 && code <= 57) -- 0-9

      || (code >= 65 && code <= 90) -- A-Z
      || (code >= 97 && code <= 122) -- a-z

  pad2 s = if length s < 2 then "0" <> s else s

-- | The **exact** inverse of [escapeIdent]: `Nothing` for anything that escaping could not have
-- | produced.
-- |
-- | Exact rather than best-effort, because a caller uses it to recover a key that then gets mangled
-- | again — a build writes a foreign manifest, a VM reads it and re-derives the symbol (ADR-0111 §4).
-- | A partial inverse breaks that round trip on the first key it does not know: decoding only `_2e`
-- | and `_5f` turns `pvf_App_2efoo_27` (`App.foo'`) into `App.foo_27`, which re-mangles to
-- | `pvf_App_2efoo_5f27` — a symbol nothing defines, reported as a missing provider for a key the
-- | link had already checked.
-- |
-- | An escape that decodes to an alphanumeric (`_61`) is REFUSED even though it is well-formed hex:
-- | escaping never produces one, so accepting it would make two spellings denote the same key and
-- | cost the encoding its injectivity — the property ADR-0072 §2 chose it for.
unescapeIdent :: String -> Maybe String
unescapeIdent = go "" <<< toCharArray
  where
  go acc chars = case Array.uncons chars of
    Nothing -> Just acc
    Just { head: c, tail: rest }
      | c /= '_' -> if isAlphaNumChar c then go (acc <> singleton c) rest else Nothing
      | otherwise -> case Array.take 2 rest of
          [ hi, lo ] -> do
            code <- fromStringAs hexadecimal (singleton hi <> singleton lo)
            -- Reject an escape of something escaping would have passed through, and any digit the
            -- lowercase-hex writer would not have emitted.
            if isAlphaNum code || not (isLowerHex hi && isLowerHex lo) then Nothing
            else do
              ch <- fromCharCode code
              go (acc <> singleton ch) (Array.drop 2 rest)
          _ -> Nothing

  isAlphaNumChar c = isAlphaNum (toCharCode c)

  isAlphaNum code =
    (code >= 48 && code <= 57)
      || (code >= 65 && code <= 90)
      || (code >= 97 && code <= 122)

  isLowerHex c =
    let
      n = toCharCode c
    in
      (n >= 48 && n <= 57) || (n >= 97 && n <= 102)

-- | A top-level binding's linker symbol base: `pv_g_<escape key>` (ADR-0072 §2).
mangle :: String -> String
mangle key = "pv_g_" <> escapeIdent key

-- | A native foreign leaf's `AbiCodeFn` linker symbol: `pvf_<escape key>` (ADR-0073 §3).
mangleForeign :: String -> String
mangleForeign key = "pvf_" <> escapeIdent key

-- | A constructor's runtime tag: `fnv1a64(name).lo` masked to 31 bits (ADR-0069 §2's derivation,
-- | shared with record label ids).
-- |
-- | **Two consumers, one derivation** — the reason this is here rather than in the backend. Codegen
-- | mints it when it emits an ADT; the owned VM mints it when a data value crosses the FFI boundary
-- | and when a `SwitchCtor` dispatches on one a leaf returned (ADR-0111 §3). The tag is a pure
-- | function of the constructor NAME, which is what lets the bytecode keep carrying names and stay
-- | free of any backend's encoding (ADR-0110 §4) — but only if both sides compute it identically.
ctorTag :: String -> Int
ctorTag name = (fnv1a64Bytes (utf8Bytes name)).lo .&. 0x7fffffff
