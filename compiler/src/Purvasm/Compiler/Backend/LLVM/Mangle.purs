-- | Pure textual encodings the LLVM backend emits (ADR-0072 §2): tagged-word immediate constants
-- | and linker-symbol mangling. A faithful transcription of the corresponding helpers in boot's
-- | `codegen_llvm.ml` (`imm`/`imm_int`/`imm_bool`/`imm_unit`, `escape_ident`/`mangle`/`mangle_foreign`),
-- | byte-identical to boot's output — the `.ll` byte-identity gate depends on it (ADR-0082 §2).
-- |
-- | The `fnv1a_64`-derived encodings (`label_id`, `ctor_tag`) are deferred until constructors/records
-- | land (they need a full 64-bit multiply and are unused by slice 1).
module Purvasm.Compiler.Backend.LLVM.Mangle
  ( imm
  , immInt
  , immBool
  , immUnit
  , escapeIdent
  , mangle
  , mangleForeign
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldMap)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (shl, shr, (.|.))
import Data.String.CodeUnits (length, singleton, toCharArray)
import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)

-- | A scalar immediate (ADR-0064 §1): the payload as the i64 constant `(payload << 1) | 1`, rendered
-- | as signed decimal exactly like boot's `Int64.to_string`. PureScript's `Int` is 32-bit, so the low
-- | word is the 32-bit `(payload << 1) | 1` and the high word is the arithmetic sign-extension
-- | `payload >> 31`; together they name the same i64 two's-complement bit pattern boot emits (verified
-- | across the sign boundary, e.g. `payload = -2^31` → `"-4294967295"`).
imm :: Int -> String
imm payload = int64BitsDecimal { hi: payload `shr` 31, lo: (payload `shl` 1) .|. 1 }

-- | An `Int` literal immediate. boot wraps to 32 bits first (`Int32.of_int`); PureScript's `Int` is
-- | already 32-bit, so that wrap is the identity here.
immInt :: Int -> String
immInt = imm

-- | A `Boolean` literal immediate: `false`/`true` tag as `imm 0`/`imm 1` (→ `"1"`/`"3"`).
immBool :: Boolean -> String
immBool b = imm (if b then 1 else 0)

-- | The `Unit` / sentinel immediate: `imm 0` (→ `"1"`).
immUnit :: String
immUnit = imm 0

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
