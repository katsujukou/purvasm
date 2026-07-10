-- | Pure textual encodings the LLVM backend emits (ADR-0072 §2): tagged-word immediate constants
-- | and linker-symbol mangling. A faithful transcription of the corresponding helpers in boot's
-- | `codegen_llvm.ml` (`imm`/`imm_int`/`imm_bool`/`imm_unit`, `escape_ident`/`mangle`/`mangle_foreign`),
-- | byte-identical to boot's output — the `.ll` byte-identity gate depends on it (ADR-0082 §2).
-- |
-- | The `fnv1a_64`-derived encodings (`labelId`, `ctorTag`, `sortRecordFields`) build on the pure 64-bit
-- | hash in `Util.Fnv1a64`; `escapeStringBytes`/`utf8Bytes` back the `String`-literal constant.
module Purvasm.Compiler.Backend.LLVM.Mangle
  ( imm
  , immInt
  , immBool
  , immUnit
  , escapeIdent
  , mangle
  , mangleForeign
  , utf8Bytes
  , escapeStringBytes
  , labelId
  , ctorTag
  , sortRecordFields
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (and, shl, shr, (.&.), (.|.))
import Data.Maybe (maybe)
import Data.String.CodePoints (toCodePointArray)
import Data.String.CodeUnits (length, singleton, toCharArray)
import Data.String.Common (toUpper)
import Data.Tuple (Tuple(..), fst, snd)
import Purvasm.Compiler.Util.Fnv1a64 (fnv1a64Bytes, unsignedCompareI64)
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

-- | The UTF-8 byte sequence of a string (ADR-0006: `String` is a UTF-8 byte sequence). boot's `String`
-- | is already bytes; PureScript's is UTF-16, so re-encode each code point to its 1–4 UTF-8 bytes, so a
-- | string constant's byte length and escaping match boot's `String.length`/byte iteration.
utf8Bytes :: String -> Array Int
utf8Bytes s = Array.concatMap enc (toCodePointArray s)
  where
  enc cp =
    let
      n = fromEnum cp
    in
      if n < 0x80 then [ n ]
      else if n < 0x800 then
        [ 0xC0 .|. (n `shr` 6), 0x80 .|. (n `and` 0x3F) ]
      else if n < 0x10000 then
        [ 0xE0 .|. (n `shr` 12), 0x80 .|. ((n `shr` 6) `and` 0x3F), 0x80 .|. (n `and` 0x3F) ]
      else
        [ 0xF0 .|. (n `shr` 18)
        , 0x80 .|. ((n `shr` 12) `and` 0x3F)
        , 0x80 .|. ((n `shr` 6) `and` 0x3F)
        , 0x80 .|. (n `and` 0x3F)
        ]

-- | A string literal's LLVM `c"…"` byte body and its byte length (boot's `string_constant` escape loop):
-- | a printable ASCII byte (`0x20`–`0x7e`, not `"`/`\`) passes through; every other byte becomes
-- | `\HH` (uppercase hex), byte-for-byte with `Printf.sprintf "\\%02X"`.
escapeStringBytes :: String -> { escaped :: String, len :: Int }
escapeStringBytes s =
  let
    bytes = utf8Bytes s
  in
    { escaped: foldMap escByte bytes, len: Array.length bytes }
  where
  escByte b =
    if b >= 0x20 && b <= 0x7e && b /= 0x22 && b /= 0x5c then
      maybe "" singleton (fromCharCode b)
    else
      "\\" <> upperPad2 (toStringAs hexadecimal b)

  upperPad2 h = toUpper (if length h < 2 then "0" <> h else h)

-- | A record label's id as an LLVM `i64` constant operand (ADR-0069 §2): the FNV-1a-64 of the label's
-- | UTF-8 bytes, rendered signed-decimal (`int64BitsDecimal`) — LLVM reads a negative `i64` literal as
-- | its two's-complement bit pattern, so the u64 the runtime hashes round-trips exactly.
labelId :: String -> String
labelId label = int64BitsDecimal (fnv1a64Bytes (utf8Bytes label))

-- | A constructor's runtime tag (ADR-0064 §1/§2): FNV-1a-64 of the name, masked to 31 bits so a nullary
-- | ctor's immediate `(tag << 1) | 1` stays inside the 63-bit payload. Only construct/match consistency
-- | matters — the tag is internal, never observed — so only the low 32 bits (`lo`) are needed.
ctorTag :: String -> Int
ctorTag name = (fnv1a64Bytes (utf8Bytes name)).lo .&. 0x7fffffff

-- | Sort a record's `(label, value)` fields by *unsigned* FNV-1a-64 label id ascending — the order the
-- | runtime `new_record` requires (ADR-0069 §1). The id is computed once per field.
sortRecordFields :: forall a. Array (Tuple String a) -> Array (Tuple String a)
sortRecordFields =
  map snd
    <<< Array.sortBy (\x y -> unsignedCompareI64 (fst x) (fst y))
    <<< map (\p -> Tuple (fnv1a64Bytes (utf8Bytes (fst p))) p)
