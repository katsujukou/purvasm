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
  ( escapeIdent
  , mangle
  , mangleForeign
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldMap)
import Data.Int (hexadecimal, toStringAs)
import Data.String (length)
import Data.String.CodeUnits (singleton, toCharArray)

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

-- | A top-level binding's linker symbol base: `pv_g_<escape key>` (ADR-0072 §2).
mangle :: String -> String
mangle key = "pv_g_" <> escapeIdent key

-- | A native foreign leaf's `AbiCodeFn` linker symbol: `pvf_<escape key>` (ADR-0073 §3).
mangleForeign :: String -> String
mangleForeign key = "pvf_" <> escapeIdent key
