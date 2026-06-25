-- | ulib SHADOW of `strings`' `Data.String.CodePoints` (ADR-0038 / ADR-0006), targeting strings 6.0.1.
-- |
-- | The registry module exists because a JS `String` is UTF-16, so iterating *code points* needs
-- | surrogate-pair handling on top of the code-*unit* `Data.String.CodeUnits`. On this backend a
-- | `String` is UTF-8 and a `Char` is already a code point (ADR-0006), so `Data.String.CodeUnits`
-- | is ALREADY code-point indexed — `CodePoint` and `Char` coincide (both an `Int` code point) and
-- | there are no surrogates. So every operation here is the `Data.String.CodeUnits` one with the
-- | element wrapped as `CodePoint` (the surrogate machinery is dropped). The `CodePoint` type and
-- | its instances are the upstream definitions verbatim; the public interface is unchanged.
module Data.String.CodePoints
  ( module Exports
  , CodePoint
  , codePointFromChar
  , singleton
  , fromCodePointArray
  , toCodePointArray
  , codePointAt
  , uncons
  , length
  , countPrefix
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , take
  , takeWhile
  , drop
  , dropWhile
  , splitAt
  ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (contains, stripPrefix, stripSuffix) as Exports
import Data.String.CodeUnits as CU
import Data.String.Common (toUpper)
import Data.String.Pattern (Pattern)
import Purvasm.Char as PC

-- | CodePoint is an `Int` bounded between `0` and `0x10FFFF`, corresponding to
-- | Unicode code points.
newtype CodePoint = CodePoint Int

derive instance eqCodePoint :: Eq CodePoint
derive instance ordCodePoint :: Ord CodePoint

instance showCodePoint :: Show CodePoint where
  show (CodePoint i) = "(CodePoint 0x" <> toUpper (toStringAs hexadecimal i) <> ")"

instance boundedCodePoint :: Bounded CodePoint where
  bottom = CodePoint 0
  top = CodePoint 0x10FFFF

instance enumCodePoint :: Enum CodePoint where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumCodePoint :: BoundedEnum CodePoint where
  cardinality = Cardinality (0x10FFFF + 1)
  fromEnum (CodePoint n) = n
  toEnum n
    | n >= 0 && n <= 0x10FFFF = Just (CodePoint n)
    | otherwise = Nothing

-- A `CodePoint` and a `Char` are both an `Int` code point on this backend (ADR-0006); the
-- conversions are the identity on the underlying `Int` (`Purvasm.Char`).
cpToChar :: CodePoint -> Char
cpToChar (CodePoint n) = PC.fromCodePoint n

charToCp :: Char -> CodePoint
charToCp c = CodePoint (PC.toCodePoint c)

-- | Creates a `CodePoint` from a given `Char`.
codePointFromChar :: Char -> CodePoint
codePointFromChar = charToCp

-- | Creates a string containing just the given code point.
singleton :: CodePoint -> String
singleton = CU.singleton <<< cpToChar

-- | Creates a string from an array of code points.
fromCodePointArray :: Array CodePoint -> String
fromCodePointArray = CU.fromCharArray <<< map cpToChar

-- | Creates an array of code points from a string.
toCodePointArray :: String -> Array CodePoint
toCodePointArray = map charToCp <<< CU.toCharArray

-- | Returns the code point at the given index, if within bounds.
codePointAt :: Int -> String -> Maybe CodePoint
codePointAt n s = charToCp <$> CU.charAt n s

-- | Returns the first code point and the rest of the string, if non-empty.
uncons :: String -> Maybe { head :: CodePoint, tail :: String }
uncons s = (\r -> { head: charToCp r.head, tail: r.tail }) <$> CU.uncons s

-- | Returns the number of code points in the string.
length :: String -> Int
length = CU.length

-- | Returns the number of leading code points all matching the predicate.
countPrefix :: (CodePoint -> Boolean) -> String -> Int
countPrefix p = CU.countPrefix (p <<< charToCp)

-- | Returns the number of code points preceding the first match of the pattern.
indexOf :: Pattern -> String -> Maybe Int
indexOf = CU.indexOf

-- | As `indexOf`, ignoring matches before the given index.
indexOf' :: Pattern -> Int -> String -> Maybe Int
indexOf' = CU.indexOf'

-- | Returns the number of code points preceding the last match of the pattern.
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf = CU.lastIndexOf

-- | As `lastIndexOf`, ignoring matches after the given index.
lastIndexOf' :: Pattern -> Int -> String -> Maybe Int
lastIndexOf' = CU.lastIndexOf'

-- | Returns the first `n` code points.
take :: Int -> String -> String
take = CU.take

-- | Returns the leading code points all matching the predicate.
takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile p s = take (countPrefix p s) s

-- | Drops the first `n` code points.
drop :: Int -> String -> String
drop = CU.drop

-- | Drops the leading code points all matching the predicate.
dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Splits the string at the given code-point index.
splitAt :: Int -> String -> { before :: String, after :: String }
splitAt = CU.splitAt
