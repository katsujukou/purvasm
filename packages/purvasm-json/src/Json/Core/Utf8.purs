-- | Byte-level UTF-8 helpers for the byte-oriented `Json.Core.Parser` (ADR-0054), defined here over
-- | the four `Purvasm.String` primitives. They are deliberately **not** taken from the ulib
-- | `Data.String.Internal.Utf8`: the ulib is a native-link overlay, not a `spago` package, so stock
-- | `purs` (which builds and tests `purvasm-json`) cannot resolve it, and depending on another
-- | subsystem's `Internal` module violates the layering rule. `encodeCp` yields the canonical UTF-8
-- | bytes of a code point — canonical UTF-8 is unique, so it is byte-identical to the ulib `putCp`
-- | encoding by construction (a test pins it).
module Json.Core.Utf8
  ( sliceBytes
  , encodeCp
  , bytesToString
  ) where

import Prelude

import Data.Array (length, unsafeIndex) as Array
import Partial.Unsafe (unsafePartial)
import Purvasm.String as PS

-- | Copy bytes `[from, to)` of `s` (empty if `to <= from`) — the ADR-0103 bulk slice leaf: one
-- | leaf call instead of a `byteAt` apply per byte. Both offsets must be code-point boundaries;
-- | the parser only slices at ASCII delimiters (quotes, number edges), which always are.
sliceBytes :: String -> Int -> Int -> String
sliceBytes s from to = if to <= from then "" else PS.byteSlice from to s

-- | The canonical UTF-8 bytes of code point `cp` (1–4 bytes, in order). Byte-identical to the ulib
-- | `Data.String.Internal.Utf8.putCp` encoding.
encodeCp :: Int -> Array Int
encodeCp cp =
  if cp < 0x80 then [ cp ]
  else if cp < 0x800 then
    [ 0xC0 + cp `div` 64, 0x80 + cp `mod` 64 ]
  else if cp < 0x10000 then
    [ 0xE0 + cp `div` 4096, 0x80 + (cp `div` 64) `mod` 64, 0x80 + cp `mod` 64 ]
  else
    [ 0xF0 + cp `div` 262144, 0x80 + (cp `div` 4096) `mod` 64, 0x80 + (cp `div` 64) `mod` 64, 0x80 + cp `mod` 64 ]

-- | Materialise an array of bytes (each 0–255) into a string.
bytesToString :: Array Int -> String
bytesToString arr = go 0 (PS.unsafeNew n)
  where
  n = Array.length arr
  go i out = if i >= n then out else go (i + 1) (PS.unsafeSetByte out i (unsafePartial (Array.unsafeIndex arr i)))
