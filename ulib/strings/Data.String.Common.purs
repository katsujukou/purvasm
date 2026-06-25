-- | ulib SHADOW of `strings`' `Data.String.Common` (ADR-0038 / ADR-0006), targeting strings 6.0.1.
-- |
-- | The structural foreigns — `replace` / `replaceAll` / `split` / `joinWith` / `trim` — are
-- | reimplemented in PureScript over the byte-level `Purvasm.String` primitives (UTF-8, code-point
-- | semantics, ADR-0006), so they run standalone on purvasm. `trim`'s whitespace set is the finite
-- | ECMAScript `WhiteSpace` + `LineTerminator` set, hard-coded.
-- |
-- | `toLower` / `toUpper` / `_localeCompare` are KEPT as foreign imports: full Unicode case mapping
-- | (~1400 entries) and locale collation (CLDR) need large host data, so they stay a JS host import
-- | (ADR-0006 — host-data-dependent operations fall back to JS; a program using them runs under the
-- | loader). The public interface is unchanged, so this shadows the registry module.
-- |
-- | NOTE: the UTF-8 byte codec (`decodeAt` / `sliceBytes` / `byteIndexOf`) is shared with the
-- | `Data.String.CodeUnits` shadow via `Data.String.Internal.Utf8` (ADR-0038); only `blit`
-- | stays local to this module.
module Data.String.Common
  ( null
  , localeCompare
  , replace
  , replaceAll
  , split
  , toLower
  , toUpper
  , trim
  , joinWith
  ) where

import Prelude

import Data.String.Internal.Utf8 (byteIndexOf, decodeAt, sliceBytes)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Purvasm.Array as PA
import Purvasm.String as PS

-------------------------------------------------------------------------------
-- UTF-8 byte helpers (private). The codec proper (`decodeAt` / `sliceBytes` / `byteIndexOf`) is
-- shared via `Data.String.Internal.Utf8` (ADR-0038); only `blit` — used directly across this
-- module's `replace` / `joinWith` builders, not just by `sliceBytes` — stays local.
-------------------------------------------------------------------------------

-- | Copy `src` bytes `[from, to)` into `dst` starting at `off`, returning the threaded `dst`.
blit :: String -> Int -> Int -> String -> Int -> String
blit src from to dst off = go from off dst
  where
  go i o d = if i >= to then d else go (i + 1) (o + 1) (PS.unsafeSetByte d o (PS.byteAt src i))

-- | The ECMAScript `String.prototype.trim` whitespace set (`WhiteSpace` + `LineTerminator`).
isWhitespace :: Int -> Boolean
isWhitespace cp =
  cp == 0x20 || cp == 0x09 || cp == 0x0A || cp == 0x0B || cp == 0x0C || cp == 0x0D
    || cp == 0xA0
    || cp == 0x1680
    || (cp >= 0x2000 && cp <= 0x200A)
    || cp == 0x2028
    || cp == 0x2029
    || cp == 0x202F
    || cp == 0x205F
    || cp == 0x3000
    || cp == 0xFEFF

-------------------------------------------------------------------------------

-- | Returns `true` if the given string is empty.
null :: String -> Boolean
null s = s == ""

-- | Compare two strings in a locale-aware fashion (kept foreign — needs host CLDR collation data).
localeCompare :: String -> String -> Ordering
localeCompare = _localeCompare LT EQ GT

foreign import _localeCompare
  :: Ordering
  -> Ordering
  -> Ordering
  -> String
  -> String
  -> Ordering

-- | Replaces the first occurence of the pattern with the replacement string.
replace :: Pattern -> Replacement -> String -> String
replace (Pattern p) (Replacement r) s =
  let
    i = byteIndexOf s p 0
  in
    if i < 0 then s
    else
      let
        pn = PS.byteLength p
        rn = PS.byteLength r
        sn = PS.byteLength s
        out0 = PS.unsafeNew (sn - pn + rn)
        out1 = blit s 0 i out0 0
        out2 = blit r 0 rn out1 i
      in
        blit s (i + pn) sn out2 (i + rn)

-- | Replaces all occurences of the pattern with the replacement string.
replaceAll :: Pattern -> Replacement -> String -> String
replaceAll (Pattern p) (Replacement r) s =
  if pn == 0 then interleave
  else build 0 0 0 (PS.unsafeNew total)
  where
  pn = PS.byteLength p
  rn = PS.byteLength r
  sn = PS.byteLength s
  occ = countOcc 0 0
  countOcc from acc = let i = byteIndexOf s p from in if i < 0 then acc else countOcc (i + pn) (acc + 1)
  total = sn + occ * (rn - pn)
  -- non-empty pattern: copy each segment between matches, inserting `r` at every match
  build prevEnd from off out =
    let
      i = byteIndexOf s p from
    in
      if i < 0 then blit s prevEnd sn out off
      else
        let
          out1 = blit s prevEnd i out off
          off1 = off + (i - prevEnd)
          out2 = blit r 0 rn out1 off1
        in
          build (i + pn) (i + pn) (off1 + rn) out2
  -- empty pattern: JS inserts `r` around every code point (`"abc"` → `"<r>a<r>b<r>c<r>"`)
  interleave = go 0 (blit r 0 rn (PS.unsafeNew (rn + sn + occCp * rn)) 0) rn
    where
    occCp = cpCount 0 0
    cpCount o k = if o >= sn then k else cpCount (decodeAt s o).next (k + 1)
    go o out off =
      if o >= sn then out
      else
        let
          d = decodeAt s o
          out1 = blit s o d.next out off
          out2 = blit r 0 rn out1 (off + (d.next - o))
        in
          go d.next out2 (off + (d.next - o) + rn)

-- | Splits the second string along occurences of the first.
split :: Pattern -> String -> Array String
split (Pattern sep) s =
  if PS.byteLength sep == 0 then splitChars
  else fill 0 0 0 (PA.unsafeNew (occ + 1))
  where
  nn = PS.byteLength sep
  sn = PS.byteLength s
  occ = countOcc 0 0
  countOcc from acc = let i = byteIndexOf s sep from in if i < 0 then acc else countOcc (i + nn) (acc + 1)
  fill k prevEnd from out =
    let
      i = byteIndexOf s sep from
    in
      if i < 0 then PA.unsafeSet out k (sliceBytes s prevEnd sn)
      else fill (k + 1) (i + nn) (i + nn) (PA.unsafeSet out k (sliceBytes s prevEnd i))
  -- empty separator: one element per code point; an empty string splits to `[]`
  splitChars =
    if sn == 0 then PA.unsafeNew 0
    else go 0 0 (PA.unsafeNew (cpCount 0 0))
    where
    cpCount o k = if o >= sn then k else cpCount (decodeAt s o).next (k + 1)
    go o k out = if o >= sn then out else let d = decodeAt s o in go d.next (k + 1) (PA.unsafeSet out k (sliceBytes s o d.next))

-- | Returns the argument converted to lowercase (kept foreign — needs host Unicode case tables).
foreign import toLower :: String -> String

-- | Returns the argument converted to uppercase (kept foreign — needs host Unicode case tables).
foreign import toUpper :: String -> String

-- | Removes leading and trailing whitespace (ECMAScript `WhiteSpace` + `LineTerminator`).
trim :: String -> String
trim s = walk 0 0 0 false
  where
  n = PS.byteLength s
  -- single pass: `start` = byte offset of the first non-ws code point, `lastEnd` = the offset just
  -- past the last non-ws code point; the trimmed string is `[start, lastEnd)`.
  walk o start lastEnd seen =
    if o >= n then sliceBytes s start lastEnd
    else
      let
        d = decodeAt s o
      in
        if isWhitespace d.cp then walk d.next start lastEnd seen
        else walk d.next (if seen then start else o) d.next true

-- | Joins the strings in the array, inserting the separator between them.
joinWith :: String -> Array String -> String
joinWith sep xs =
  if m == 0 then PS.unsafeNew 0
  else build 0 0 (PS.unsafeNew total)
  where
  m = PA.length xs
  sepLen = PS.byteLength sep
  xAt k = PA.unsafeIndex xs k
  total = sumLen 0 0 + sepLen * (m - 1)
  sumLen k acc = if k >= m then acc else sumLen (k + 1) (acc + PS.byteLength (xAt k))
  build k off out =
    if k >= m then out
    else
      let
        x = xAt k
        lenX = PS.byteLength x
        out1 = blit x 0 lenX out off
      in
        if k == m - 1 then out1
        else build (k + 1) (off + lenX + sepLen) (blit sep 0 sepLen out1 (off + lenX))
