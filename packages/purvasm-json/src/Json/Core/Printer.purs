-- | A representation-neutral JSON serialiser (ADR-0046). Given an `Eliminator` for a node type
-- | `j`, `print` renders it to a compact JSON string. Strings are escaped per the JSON grammar;
-- | numbers are rendered with the `Show Number` instance (valid JSON — integral values keep a
-- | `.0`, a documented divergence from JS `JSON.stringify` that round-trips identically).
module Json.Core.Printer
  ( print
  ) where

import Prelude

import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.String.CodePoints (CodePoint, singleton, toCodePointArray)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Json.Core.Types (Eliminator)

-- | Render a JSON value to a compact (no-whitespace) JSON string.
print :: forall j. Eliminator j -> j -> String
print elim = go
  where
  go :: j -> String
  go = elim
    { onNull: "null"
    , onBoolean: \b -> if b then "true" else "false"
    , onNumber: show
    , onString: encodeString
    , onArray: \xs -> "[" <> joinWith "," (map go xs) <> "]"
    , onObject: \kvs ->
        "{" <> joinWith "," (map (\(Tuple k v) -> encodeString k <> ":" <> go v) kvs) <> "}"
    }

-- | Quote and escape a `String` as a JSON string literal.
encodeString :: String -> String
encodeString s = "\"" <> foldMap escapeCodePoint (toCodePointArray s) <> "\""

escapeCodePoint :: CodePoint -> String
escapeCodePoint cp = case fromEnum cp of
  0x22 -> "\\\""
  0x5C -> "\\\\"
  0x08 -> "\\b"
  0x0C -> "\\f"
  0x0A -> "\\n"
  0x0D -> "\\r"
  0x09 -> "\\t"
  c
    | c < 0x20 -> "\\u" <> hex4 c
    | otherwise -> singleton cp

-- | A code point in `[0, 0xFFFF]` as four lowercase hex digits.
hex4 :: Int -> String
hex4 n = hexDigit (n / 4096) <> hexDigit (n / 256) <> hexDigit (n / 16) <> hexDigit n

hexDigit :: Int -> String
hexDigit n = case mod n 16 of
  0 -> "0"
  1 -> "1"
  2 -> "2"
  3 -> "3"
  4 -> "4"
  5 -> "5"
  6 -> "6"
  7 -> "7"
  8 -> "8"
  9 -> "9"
  10 -> "a"
  11 -> "b"
  12 -> "c"
  13 -> "d"
  14 -> "e"
  _ -> "f"
