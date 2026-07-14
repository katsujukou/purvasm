-- | Pure parsing/rendering for `ulib-tools unicode-gen` (ADR-0101): turn the Unicode Character
-- | Database's `UnicodeData.txt` into the flat `[cp, mapped, …]` SIMPLE (1:1) case-mapping tables
-- | backing `Data.String.Common`'s `toLower`/`toUpper` — the typed replacement for
-- | `gen/gen_casemap.py`. Kept free of effects (no fetching, no filesystem) so parsing, the
-- | self-check invariants, and rendering are unit-testable directly.
module Purvasm.UlibTools.UnicodeData
  ( CaseTable
  , CaseTables
  , Pin
  , parseUnicodeData
  , selfCheck
  , renderCaseMapModule
  , parsePin
  , renderPin
  ) where

import Prelude

import Data.Argonaut.Core (fromObject, fromString, stringify, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (for_)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object as Object

-- | Sorted `(codepoint, mapping)` pairs for one case-mapping direction.
type CaseTable = Array (Tuple Int Int)

type CaseTables = { upper :: CaseTable, lower :: CaseTable }

-- | The `ulib/strings/gen/unicode-data.json` pin (ADR-0101): which Unicode Character Database
-- | version, and the sha256 of its exact `UnicodeData.txt` bytes, `unicode-gen` trusts. The only
-- | committed reference to that file's content — `UnicodeData.txt` itself is never committed.
type Pin = { ucdVersion :: String, sha256 :: String }

-- | Parse a pin file.
parsePin :: String -> Either String Pin
parsePin src = do
  json <- jsonParser src
  obj <- note "unicode-data.json: top level is not an object" (toObject json)
  ucdVersion <- note "unicode-data.json: 'ucdVersion' must be a string"
    (Object.lookup "ucdVersion" obj >>= toString)
  sha256 <- note "unicode-data.json: 'sha256' must be a string"
    (Object.lookup "sha256" obj >>= toString)
  pure { ucdVersion, sha256 }

-- | Render a pin file. Inverse of `parsePin`.
renderPin :: Pin -> String
renderPin { ucdVersion, sha256 } =
  stringify $ fromObject $ Object.fromFoldable
    [ Tuple "ucdVersion" (fromString ucdVersion)
    , Tuple "sha256" (fromString sha256)
    ]

-- | Parse `UnicodeData.txt`'s per-code-point records (`;`-separated fields; field 0 is the hex code
-- | point, fields 12/13 are `Simple_Uppercase_Mapping`/`Simple_Lowercase_Mapping`, taken verbatim —
-- | NOT derived from a host case-folding routine, which would apply `SpecialCasing.txt`'s *full*
-- | mapping instead, see `Data.String.Common`'s doc comment). Blank fields ⇒ no mapping in that
-- | direction. Every non-blank line must have exactly the UCD's 15 `;`-separated fields (0-14) — a
-- | short record (e.g. a truncated download) is a hard parse error, never treated as blank-field
-- | "no mapping", so a truncated input can never silently produce an incomplete-but-accepted table.
-- | The result is sorted by code point (the file already is; sorted defensively).
parseUnicodeData :: String -> Either String CaseTables
parseUnicodeData src = do
  records <- traverse parseLine (nonBlankLines src)
  let
    upper = Array.mapMaybe (\r -> Tuple r.cp <$> r.su) records
    lower = Array.mapMaybe (\r -> Tuple r.cp <$> r.sl) records
  pure { upper: Array.sortWith fst upper, lower: Array.sortWith fst lower }
  where
  nonBlankLines = Array.filter (not <<< String.null) <<< String.split (Pattern "\n") <<< String.trim

  ucdFieldCount = 15

  parseLine line = do
    let fields = String.split (Pattern ";") line
    when (Array.length fields /= ucdFieldCount) do
      Left $ "UnicodeData.txt: malformed line (expected " <> show ucdFieldCount <> " fields, got "
        <> show (Array.length fields)
        <> "): "
        <> line
    cpStr <- note ("UnicodeData.txt: unreachable (field-count already checked): " <> line)
      (Array.index fields 0)
    cp <- note ("UnicodeData.txt: bad code point '" <> cpStr <> "'") (fromStringAs hexadecimal cpStr)
    su <- hexFieldAt fields 12
    sl <- hexFieldAt fields 13
    pure { cp, su, sl }

  hexFieldAt fields i = case Array.index fields i of
    Nothing -> Left
      ("UnicodeData.txt: unreachable (field " <> show i <> " missing after field-count check)")
    Just "" -> Right Nothing
    Just s -> case fromStringAs hexadecimal s of
      Nothing -> Left ("UnicodeData.txt: bad hex mapping '" <> s <> "'")
      Just v -> Right (Just v)

-- | Structural invariants + known Unicode facts that must hold regardless of which UCD version is
-- | fetched (ported from `gen_casemap.py`'s `self_check`, ADR-0101). Runs on every generation so a
-- | regression to a full-casing-derived method, a corrupted/truncated download, or a lookup-table
-- | bug fails loudly here rather than silently shipping a wrong `.purs`.
selfCheck :: CaseTables -> Either String Unit
selfCheck { upper, lower } = do
  checkSorted "upper" upper
  checkSorted "lower" lower
  checkNoDuplicateKeys "upper" upper
  checkNoDuplicateKeys "lower" lower
  let
    upperMap = Map.fromFoldable upper
    lowerMap = Map.fromFoldable lower

  -- ASCII round-trip.
  at "upper" upperMap 0x0061 >>= expect "upper[U+0061 'a']" 0x0041
  at "lower" lowerMap 0x0041 >>= expect "lower[U+0041 'A']" 0x0061

  -- U+0130 (İ)'s SIMPLE lowercase is plain U+0069 'i', even though a host case-folding routine's
  -- FULL lowercase is the 2-code-point U+0069 U+0307 — the regression this check pins against.
  at "lower" lowerMap 0x0130 >>= expect "lower[U+0130 'İ']" 0x0069
  -- Its counterpart U+0131 (ı) has a genuine, unrelated simple uppercase U+0049.
  at "upper" upperMap 0x0131 >>= expect "upper[U+0131 'ı']" 0x0049

  -- U+00DF (ß) has NO simple uppercase in UnicodeData.txt (its only uppercase form, "SS", is a
  -- SpecialCasing.txt-only full mapping) — it must be ABSENT from the table, not self-mapped.
  when (Map.member 0x00DF upperMap) (Left "upper: U+00DF (ß) must have no simple uppercase entry")
  -- Its one-directional counterpart U+1E9E (ẞ) does have a simple lowercase, U+00DF.
  at "lower" lowerMap 0x1E9E >>= expect "lower[U+1E9E 'ẞ']" 0x00DF

  -- A UTF-8-byte-length-changing mapping (U+017F 'ſ', 2 bytes -> U+0053 'S', 1 byte) — exercises
  -- that the table itself carries genuine byte-length-changing pairs.
  at "upper" upperMap 0x017F >>= expect "upper[U+017F 'ſ']" 0x0053

  -- Boundary / absent code points: an unmapped code point must be ABSENT from both tables, never
  -- present-but-identity (absence is `lookupCp`'s identity fallback).
  for_ [ 0x0000, 0x0030, 0x4E2D ] \c ->
    when (Map.member c upperMap || Map.member c lowerMap)
      (Left ("upper/lower: " <> hex c <> " must have no case mapping"))
  where
  checkSorted name table =
    let
      cps = map fst table
    in
      if cps == Array.sort cps then Right unit else Left (name <> " table not sorted by code point")

  checkNoDuplicateKeys name table =
    let
      cps = map fst table
    in
      if Array.length (Array.nub cps) == Array.length cps then Right unit
      else Left (name <> " table has a duplicate code point key")

  at name m k = note (name <> ": missing " <> hex k) (Map.lookup k m)
  expect name expected actual =
    if actual == expected then Right unit
    else Left (name <> ": expected " <> hex expected <> ", got " <> hex actual)
  hex n = "U+" <> String.toUpper (toStringAs hexadecimal n)

-- | Render `Data.String.Internal.CaseMap` from the parsed tables, attributing the given UCD version.
renderCaseMapModule :: { ucdVersion :: String } -> CaseTables -> String
renderCaseMapModule { ucdVersion } { upper, lower } =
  header <> "\n\n" <> renderTable "Upper" upper <> "\n\n" <> renderTable "Lower" lower <> "\n"
  where
  header =
    """-- | GENERATED by `ulib-tools unicode-gen` (Unicode Character Database """ <> ucdVersion
      <>
        """, pinned in
-- | ulib/strings/gen/unicode-data.json) -- do not edit; regenerate with `ulib-tools unicode-gen`
-- | and commit (ADR-0101).
-- |
-- | Flat @[cp, mapped, cp, mapped, ...]@ SIMPLE (1:1) case-mapping tables, probed by binary search,
-- | backing `Data.String.Common`'s `toLower`/`toUpper`. Sourced directly from UnicodeData.txt's
-- | Simple_Uppercase_Mapping / Simple_Lowercase_Mapping fields (12/13) -- NOT from a host case-folding
-- | routine, which would apply SpecialCasing.txt's full (occasionally multi-code-point) mappings
-- | instead (see that module's doc comment for the resulting, precisely-scoped divergence).
module Data.String.Internal.CaseMap
  ( toUpperCp
  , toLowerCp
  ) where

import Prelude

import Data.Array (length, unsafeIndex)
import Partial.Unsafe (unsafePartial)

-- | The simple uppercase mapping of a code point, or `cp` unchanged if none applies.
toUpperCp :: Int -> Int
toUpperCp cp = lookupCp tableUpper cp

-- | The simple lowercase mapping of a code point, or `cp` unchanged if none applies.
toLowerCp :: Int -> Int
toLowerCp cp = lookupCp tableLower cp

-- | Binary search over a sorted `[cp, mapped, ...]` table; `cp` itself when absent.
lookupCp :: Array Int -> Int -> Int
lookupCp table cp = go 0 (length table / 2 - 1)
  where
  at i = unsafePartial (unsafeIndex table i)
  go lo hi
    | lo > hi = cp
    | otherwise =
        let
          mid = (lo + hi) / 2
          k = at (2 * mid)
        in
          if cp < k then go lo (mid - 1)
          else if cp > k then go (mid + 1) hi
          else at (2 * mid + 1)"""

  renderTable name table =
    "table" <> name <> " :: Array Int\n"
      <> "table"
      <> name
      <> " =\n"
      <> "  [ "
      <> body
      <> "\n  ]"
    where
    flat = Array.concatMap (\(Tuple a b) -> [ a, b ]) table
    rows = chunksOf 10 flat
    body = String.joinWith "\n  , " (map (String.joinWith ", " <<< map show) rows)

  chunksOf n xs
    | Array.null xs = []
    | otherwise = [ Array.take n xs ] <> chunksOf n (Array.drop n xs)
