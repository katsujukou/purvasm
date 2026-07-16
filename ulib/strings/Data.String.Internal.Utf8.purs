-- | ulib INTERNAL helper (ADR-0038): the UTF-8 codec shared by the `Data.String.*` shadows. It is
-- | **not** a registry module — it has no upstream counterpart and is never exported to users. The
-- | `Data.String.CodeUnits` / `Data.String.Common` shadows import it; at the user's build it is
-- | *injected* from the lib (the user never imports it), per ADR-0038's plan→recompute→materialize
-- | resolution. Lives under the `*.Internal.*` namespace so it cannot collide with a registry module.
-- |
-- | All arithmetic is ordinary `Prelude` `Int` (this module sits above `Prelude`); `mod`/`div` are the
-- | Euclidean intrinsics, so on the non-negative byte / code-point values here they are plain
-- | remainder / quotient. Everything operates on raw `Int` code points and byte offsets over the
-- | byte-level `Purvasm.String` primitives — no `Char`, so the dependency surface is just `Purvasm.String`.
-- |
-- | The whole-string walks (slice / search / index↔offset conversion) delegate to the ADR-0103 bulk
-- | leaves — one leaf call instead of one `byteAt` apply per byte, which is what made the guest-loop
-- | versions the dominant compile-time cost (sidenote-0017). Only the per-code-point codec
-- | (`decodeAt` / `putCp` / `utf8Len`) and the offset stepping stay guest loops: they are used by
-- | callers that interleave decoding with per-character predicates, where no bulk leaf applies.
module Data.String.Internal.Utf8
  ( byteLenOfLead
  , nextOffset
  , decodeAt
  , utf8Len
  , putCp
  , byteOffsetOfCp
  , cpIndexOfByteOffset
  , sliceBytes
  , byteIndexOf
  , byteLastIndexOf
  ) where

import Prelude

import Purvasm.String as PS

-- | The number of UTF-8 bytes a lead byte introduces (1-4), so the byte offset of the next code
-- | point is `o + byteLenOfLead (byteAt s o)` — used by the index↔offset walks that need no `cp`.
byteLenOfLead :: Int -> Int
byteLenOfLead b0
  | b0 < 0x80 = 1
  | b0 < 0xE0 = 2
  | b0 < 0xF0 = 3
  | otherwise = 4

-- | The byte offset just past the code point starting at byte offset `o`.
nextOffset :: String -> Int -> Int
nextOffset s o = o + byteLenOfLead (PS.byteAt s o)

-- | Decode the code point starting at byte offset `o` (which must be `< byteLength`), returning it
-- | and the next code point's byte offset. The 6-bit continuation payload of a byte `b` is
-- | `b `mod` 64`; the lead byte's payload is its low `7 - n` bits (`b0 `mod` 32/16/8`).
decodeAt :: String -> Int -> { cp :: Int, next :: Int }
decodeAt s o =
  let
    b0 = PS.byteAt s o
    cont k = PS.byteAt s (o + k) `mod` 64
  in
    if b0 < 0x80 then { cp: b0, next: o + 1 }
    else if b0 < 0xE0 then { cp: (b0 `mod` 32) * 64 + cont 1, next: o + 2 }
    else if b0 < 0xF0 then { cp: (b0 `mod` 16) * 4096 + cont 1 * 64 + cont 2, next: o + 3 }
    else { cp: (b0 `mod` 8) * 262144 + cont 1 * 4096 + cont 2 * 64 + cont 3, next: o + 4 }

-- | The number of UTF-8 bytes the code point `cp` encodes to.
utf8Len :: Int -> Int
utf8Len cp
  | cp < 0x80 = 1
  | cp < 0x800 = 2
  | cp < 0x10000 = 3
  | otherwise = 4

-- | Write `cp`'s UTF-8 bytes into `s` starting at byte offset `o` (the bytes must fit), returning
-- | the threaded string. `Purvasm.String.unsafeSetByte` mutates in place and returns the string, so
-- | nesting the calls writes all the bytes and yields the final string.
putCp :: String -> Int -> Int -> String
putCp s o cp =
  if cp < 0x80 then set s o cp
  else if cp < 0x800 then
    set (set s o (0xC0 + cp `div` 64)) (o + 1) (0x80 + cp `mod` 64)
  else if cp < 0x10000 then
    set (set (set s o (0xE0 + cp `div` 4096)) (o + 1) (0x80 + (cp `div` 64) `mod` 64)) (o + 2) (0x80 + cp `mod` 64)
  else
    set (set (set (set s o (0xF0 + cp `div` 262144)) (o + 1) (0x80 + (cp `div` 4096) `mod` 64)) (o + 2) (0x80 + (cp `div` 64) `mod` 64)) (o + 3) (0x80 + cp `mod` 64)
  where
  set = PS.unsafeSetByte

-- | The byte offset of the `i`-th code point, clamped: `i <= 0` → `0`, `i >= length` → `byteLength`.
byteOffsetOfCp :: String -> Int -> Int
byteOffsetOfCp s i = PS.byteLength (PS.takeCodePoints i s)

-- | The code-point index of the byte offset `b` (which must be a code-point boundary, which a UTF-8
-- | substring match always is) — the number of code points before it.
cpIndexOfByteOffset :: String -> Int -> Int
cpIndexOfByteOffset s b = PS.codePointLength (PS.byteSlice 0 b s)

-- | Copy the bytes `[from, to)` of `s` (empty if `to <= from`). Both offsets must be code-point
-- | boundaries within the string — `Purvasm.String.byteSlice` faults otherwise (every in-tree
-- | caller passes offsets produced by a boundary walk or a substring match, which satisfy this).
sliceBytes :: String -> Int -> Int -> String
sliceBytes s from to = if to <= from then "" else PS.byteSlice from to s

-- | The first byte offset `>= from` at which `needle`'s bytes occur in `hay`, or `-1`. UTF-8 is
-- | self-synchronizing, so a byte match of a valid needle always begins on a code-point boundary.
byteIndexOf :: String -> String -> Int -> Int
byteIndexOf = PS.byteIndexOf

-- | The last byte offset `<= fromByte` at which `needle` occurs in `hay`, or `-1`. A negative
-- | `fromByte` is clamped to `0` (so an empty needle still matches at offset `0`).
byteLastIndexOf :: String -> String -> Int -> Int
byteLastIndexOf = PS.byteLastIndexOf
