-- | Code-point access over the four `purvasm-base` byte primitives (the ADR-0054 discipline,
-- | mirroring `Json.Core.Utf8`): defined locally so `purvasm-regex` does NOT depend on the
-- | `strings` registry package — whose ulib shadow depends on this package (ADR-0081 §2);
-- | a `Data.String.CodePoints` import here would be the 0047 dependency cycle.
-- |
-- | Dual-target: on purvasm a `Purvasm.String` "byte" is a UTF-8 byte; on the JS backends it
-- | is a UTF-16 code unit (the documented divergence in purvasm-base's JS provider). The
-- | mode is detected once by probing the byte length of a known non-ASCII literal (2 under
-- | UTF-8, 1 under UTF-16), so both backends decode to identical code points.
module Regex.Core.Utf8
  ( toCodePoints
  , fromCodePoints
  , cpAt
  , sliceBytes
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (foldl)
import Data.List (List(..), reverse)
import Purvasm.String (byteAt, byteLength, byteSlice, unsafeNew, unsafeSetByte)

-- The backend probe: "Å" (U+00C5) is two UTF-8 bytes but one UTF-16 unit.
isUtf8 :: Boolean
isUtf8 = byteLength "Å" == 2

-- | The code point at byte/unit index `i` and its width — the matcher's inline decode
-- | (ADR-0081 §1 byte-oriented scanning): no whole-input conversion. `i` must be in range
-- | and on a code-point boundary (the matcher only advances by returned widths).
cpAt :: String -> Int -> { cp :: Int, width :: Int }
cpAt s i =
  if isUtf8 then do
    let b = byteAt s i
    if b < 0x80 then { cp: b, width: 1 }
    else if b < 0xE0 then { cp: (b - 0xC0) * 0x40 + (byteAt s (i + 1) - 0x80), width: 2 }
    else if b < 0xF0 then
      { cp:
          (b - 0xE0) * 0x1000 + (byteAt s (i + 1) - 0x80) * 0x40
            + (byteAt s (i + 2) - 0x80)
      , width: 3
      }
    else
      { cp:
          (b - 0xF0) * 0x40000 + (byteAt s (i + 1) - 0x80) * 0x1000
            + (byteAt s (i + 2) - 0x80) * 0x40
            + (byteAt s (i + 3) - 0x80)
      , width: 4
      }
  else do
    let u = byteAt s i
    if u >= 0xD800 && u < 0xDC00 && i + 1 < byteLength s then
      { cp: (u - 0xD800) * 0x400 + (byteAt s (i + 1) - 0xDC00) + 0x10000, width: 2 }
    else { cp: u, width: 1 }

-- | Copy bytes/units `[lo, hi)` out (empty if `hi <= lo`) — the ADR-0103 bulk slice leaf,
-- | representation-agnostic: units in, units out, both modes verbatim. Both offsets must be
-- | code-point boundaries; the matcher only produces offsets advanced by decoded widths.
sliceBytes :: String -> Int -> Int -> String
sliceBytes s lo hi = if hi <= lo then "" else byteSlice lo hi s

-- | Decode a string into its code points (input valid by the runtime contract, ADR-0067 §5).
toCodePoints :: String -> Array Int
toCodePoints s = fromFoldable (reverse (go Nil 0))
  where
  n = byteLength s

  go acc i =
    if i >= n then acc
    else if isUtf8 then do
      let b = byteAt s i
      if b < 0x80 then go (Cons b acc) (i + 1)
      else if b < 0xE0 then
        go (Cons ((b - 0xC0) * 0x40 + (byteAt s (i + 1) - 0x80)) acc) (i + 2)
      else if b < 0xF0 then
        go
          ( Cons
              ( (b - 0xE0) * 0x1000 + (byteAt s (i + 1) - 0x80) * 0x40
                  + (byteAt s (i + 2) - 0x80)
              )
              acc
          )
          (i + 3)
      else
        go
          ( Cons
              ( (b - 0xF0) * 0x40000 + (byteAt s (i + 1) - 0x80) * 0x1000
                  + (byteAt s (i + 2) - 0x80) * 0x40
                  + (byteAt s (i + 3) - 0x80)
              )
              acc
          )
          (i + 4)
    else do
      let u = byteAt s i
      if u >= 0xD800 && u < 0xDC00 && i + 1 < n then
        go (Cons ((u - 0xD800) * 0x400 + (byteAt s (i + 1) - 0xDC00) + 0x10000) acc) (i + 2)
      else go (Cons u acc) (i + 1)

-- | Encode code points back into a string — the ADR-0052 linear build protocol: a fresh
-- | `unsafeNew` buffer, threaded linearly, filled once.
fromCodePoints :: Array Int -> String
fromCodePoints cps = (foldl write { buf: unsafeNew total, at: 0 } cps).buf
  where
  width cp =
    if isUtf8 then
      if cp < 0x80 then 1 else if cp < 0x800 then 2 else if cp < 0x10000 then 3 else 4
    else if cp < 0x10000 then 1
    else 2
  total = foldl (\acc cp -> acc + width cp) 0 cps

  write st cp = do
    let
      buf =
        if isUtf8 then case width cp of
          1 -> unsafeSetByte st.buf st.at cp
          2 ->
            unsafeSetByte (unsafeSetByte st.buf st.at (0xC0 + cp / 0x40))
              (st.at + 1)
              (0x80 + cp `mod` 0x40)
          3 ->
            unsafeSetByte
              ( unsafeSetByte (unsafeSetByte st.buf st.at (0xE0 + cp / 0x1000))
                  (st.at + 1)
                  (0x80 + (cp / 0x40) `mod` 0x40)
              )
              (st.at + 2)
              (0x80 + cp `mod` 0x40)
          _ ->
            unsafeSetByte
              ( unsafeSetByte
                  ( unsafeSetByte (unsafeSetByte st.buf st.at (0xF0 + cp / 0x40000))
                      (st.at + 1)
                      (0x80 + (cp / 0x1000) `mod` 0x40)
                  )
                  (st.at + 2)
                  (0x80 + (cp / 0x40) `mod` 0x40)
              )
              (st.at + 3)
              (0x80 + cp `mod` 0x40)
        else if cp < 0x10000 then unsafeSetByte st.buf st.at cp
        else
          unsafeSetByte
            (unsafeSetByte st.buf st.at (0xD800 + (cp - 0x10000) / 0x400))
            (st.at + 1)
            (0xDC00 + (cp - 0x10000) `mod` 0x400)
    { buf, at: st.at + width cp }
