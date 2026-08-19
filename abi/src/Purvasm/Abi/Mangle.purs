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
  , mangle
  , mangleForeign
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldMap)
import Data.Int (hexadecimal, toStringAs)
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
