-- | ulib SHADOW of `strings`' `Data.String.CodeUnits` (ADR-0038 / ADR-0006), targeting strings 6.0.1.
-- |
-- | The registry module's foreigns are JavaScript string operations over **UTF-16 code units**.
-- | This backend stores a `String` as **UTF-8 bytes** (ADR-0006) and a `Char` is a Unicode
-- | **code point** (ADR-0006), so here every foreign is reimplemented in PureScript over the
-- | byte-level `Purvasm.String` primitives with **code-point** semantics (length / indices count code
-- | points, like Haskell `Data.Text`). The public interface is unchanged. The non-foreign helpers
-- | (`stripPrefix`, `charAt`, `indexOf`, …) are the upstream definitions verbatim; only `uncons` is
-- | rewritten to stay self-contained (the registry used `Data.String.Unsafe`).
-- |
-- | Divergence from the JS backend (documented, ADR-0006): a code point above U+FFFF counts as 1,
-- | not 2; substring search is byte-level (valid for UTF-8: it is self-synchronizing, so a match
-- | always lands on a code-point boundary) with the offset mapped back to a code-point index.
module Data.String.CodeUnits
  ( stripPrefix
  , stripSuffix
  , contains
  , singleton
  , fromCharArray
  , toCharArray
  , charAt
  , toChar
  , uncons
  , length
  , countPrefix
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , take
  , takeRight
  , takeWhile
  , drop
  , dropRight
  , dropWhile
  , slice
  , splitAt
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.String.Internal.Utf8 (byteIndexOf, byteLastIndexOf, byteOffsetOfCp, cpIndexOfByteOffset, decodeAt, putCp, sliceBytes, utf8Len)
import Data.String.Pattern (Pattern(..))
import Purvasm.Array as PA
import Purvasm.Char as PC
import Purvasm.String as PS

-------------------------------------------------------------------------------
-- `stripPrefix`, `stripSuffix`, and `contains` are CodeUnit/CodePoint agnostic
-- as they are based on patterns rather than lengths/indices, but they need to
-- be defined in here to avoid a circular module dependency
-------------------------------------------------------------------------------

-- | If the string starts with the given prefix, return the portion of the
-- | string left after removing it, as a `Just` value. Otherwise, return `Nothing`.
-- |
-- | ```purescript
-- | stripPrefix (Pattern "http:") "http://purescript.org" == Just "//purescript.org"
-- | stripPrefix (Pattern "http:") "https://purescript.org" == Nothing
-- | ```
stripPrefix :: Pattern -> String -> Maybe String
stripPrefix (Pattern prefix) str =
  let
    { before, after } = splitAt (length prefix) str
  in
    if before == prefix then Just after else Nothing

-- | If the string ends with the given suffix, return the portion of the
-- | string left after removing it, as a `Just` value. Otherwise, return
-- | `Nothing`.
-- |
-- | ```purescript
-- | stripSuffix (Pattern ".exe") "psc.exe" == Just "psc"
-- | stripSuffix (Pattern ".exe") "psc" == Nothing
-- | ```
stripSuffix :: Pattern -> String -> Maybe String
stripSuffix (Pattern suffix) str =
  let
    { before, after } = splitAt (length str - length suffix) str
  in
    if after == suffix then Just before else Nothing

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") "haystack with needle" == true
-- | contains (Pattern "needle") "haystack" == false
-- | ```
contains :: Pattern -> String -> Boolean
contains pat = isJust <<< indexOf pat

-------------------------------------------------------------------------------
-- all functions past this point are CodeUnit specific
-------------------------------------------------------------------------------

-- | Returns a string of length `1` containing the given character.
singleton :: Char -> String
singleton c = putCp (PS.unsafeNew (utf8Len cp)) 0 cp
  where
  cp = PC.toCodePoint c

-- | Converts an array of characters into a string.
fromCharArray :: Array Char -> String
fromCharArray arr = build 0 0 (PS.unsafeNew total)
  where
  m = PA.length arr
  cpAt k = PC.toCodePoint (PA.unsafeIndex arr k)
  total = sumLen 0 0
  sumLen k acc = if k >= m then acc else sumLen (k + 1) (acc + utf8Len (cpAt k))
  build k o out = if k >= m then out else build (k + 1) (o + utf8Len (cpAt k)) (putCp out o (cpAt k))

-- | Converts the string into an array of characters.
toCharArray :: String -> Array Char
toCharArray s = build 0 0 (PA.unsafeNew (length s))
  where
  n = PS.byteLength s
  build o k out =
    if o >= n then out
    else
      let
        d = decodeAt s o
      in
        build d.next (k + 1) (PA.unsafeSet out k (PC.fromCodePoint d.cp))

-- | Returns the character at the given index, if the index is within bounds.
charAt :: Int -> String -> Maybe Char
charAt = _charAt Just Nothing

_charAt :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Int -> String -> Maybe Char
_charAt just nothing i s =
  let
    cp = PS.codePointAt i s
  in
    if cp < 0 then nothing else just (PC.fromCodePoint cp)

-- | Converts the string to a character, if the length of the string is
-- | exactly `1`.
toChar :: String -> Maybe Char
toChar = _toChar Just Nothing

_toChar :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> String -> Maybe Char
_toChar just nothing s =
  if PS.byteLength s == 0 then nothing
  else
    let
      d = decodeAt s 0
    in
      if d.next == PS.byteLength s then just (PC.fromCodePoint d.cp) else nothing

-- | Returns the first character and the rest of the string,
-- | if the string is not empty.
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = case charAt 0 s of
  Nothing -> Nothing
  Just h -> Just { head: h, tail: drop 1 s }

-- | Returns the number of characters the string is composed of.
length :: String -> Int
length = PS.codePointLength

-- | Returns the number of contiguous characters at the beginning
-- | of the string for which the predicate holds.
countPrefix :: (Char -> Boolean) -> String -> Int
countPrefix p s = go 0 0
  where
  n = PS.byteLength s
  go o k =
    if o >= n then k
    else
      let
        d = decodeAt s o
      in
        if p (PC.fromCodePoint d.cp) then go d.next (k + 1) else k

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
indexOf :: Pattern -> String -> Maybe Int
indexOf = _indexOf Just Nothing

_indexOf :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Pattern -> String -> Maybe Int
_indexOf just nothing (Pattern x) s =
  let
    bi = byteIndexOf s x 0
  in
    if bi < 0 then nothing else just (cpIndexOfByteOffset s bi)

-- | Returns the index of the first occurrence of the pattern in the
-- | given string, starting at the specified index. Returns `Nothing` if there is
-- | no match.
indexOf' :: Pattern -> Int -> String -> Maybe Int
indexOf' = _indexOfStartingAt Just Nothing

_indexOfStartingAt :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Pattern -> Int -> String -> Maybe Int
_indexOfStartingAt just nothing (Pattern x) startAt s =
  if startAt < 0 || startAt > length s then nothing
  else
    let
      bi = byteIndexOf s x (byteOffsetOfCp s startAt)
    in
      if bi < 0 then nothing else just (cpIndexOfByteOffset s bi)

-- | Returns the index of the last occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf = _lastIndexOf Just Nothing

_lastIndexOf :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Pattern -> String -> Maybe Int
_lastIndexOf just nothing (Pattern x) s =
  let
    bi = byteLastIndexOf s x (PS.byteLength s)
  in
    if bi < 0 then nothing else just (cpIndexOfByteOffset s bi)

-- | Returns the index of the last occurrence of the pattern in the
-- | given string, starting at the specified index and searching
-- | backwards towards the beginning of the string.
lastIndexOf' :: Pattern -> Int -> String -> Maybe Int
lastIndexOf' = _lastIndexOfStartingAt Just Nothing

_lastIndexOfStartingAt :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Pattern -> Int -> String -> Maybe Int
_lastIndexOfStartingAt just nothing (Pattern x) startAt s =
  let
    bi = byteLastIndexOf s x (byteOffsetOfCp s (max 0 startAt))
  in
    if bi < 0 then nothing else just (cpIndexOfByteOffset s bi)

-- | Returns the first `n` characters of the string.
take :: Int -> String -> String
take = PS.takeCodePoints

-- | Returns the last `n` characters of the string.
takeRight :: Int -> String -> String
takeRight i s = drop (length s - i) s

-- | Returns the longest prefix (possibly empty) of characters that satisfy
-- | the predicate.
takeWhile :: (Char -> Boolean) -> String -> String
takeWhile p s = take (countPrefix p s) s

-- | Returns the string without the first `n` characters.
drop :: Int -> String -> String
drop = PS.dropCodePoints

-- | Returns the string without the last `n` characters.
dropRight :: Int -> String -> String
dropRight i s = take (length s - i) s

-- | Returns the suffix remaining after `takeWhile`.
dropWhile :: (Char -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Returns the substring at indices `[begin, end)`. If either index is negative, it is normalised
-- | to `length s - index`. `""` is returned if either index is out of bounds or `begin > end`.
slice :: Int -> Int -> String -> String
slice b e s =
  let
    len = length s
    norm i = if i < 0 then len + i else i
  in
    sliceBytes s (byteOffsetOfCp s (norm b)) (byteOffsetOfCp s (norm e))

-- | Splits a string into two substrings, where `before` contains the
-- | characters up to (but not including) the given index, and `after` contains
-- | the rest of the string, from that index on.
splitAt :: Int -> String -> { before :: String, after :: String }
splitAt i s = { before: take i s, after: drop i s }
