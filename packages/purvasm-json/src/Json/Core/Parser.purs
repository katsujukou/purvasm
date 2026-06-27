-- | A representation-neutral, foreign-free JSON parser (ADR-0046). `parse` runs a recursive
-- | descent over the input's Unicode code points and constructs nodes through the supplied
-- | `Builder`, so the same parser serves every JSON front-end. Numbers are converted with
-- | `Data.Number.fromString` (the one demand-driven native leaf); strings honour the JSON escape
-- | grammar including `\uXXXX` surrogate pairs over UTF-8 strings (ADR-0006).
module Json.Core.Parser
  ( parse
  ) where

import Prelude

import Data.Array (fromFoldable, index, length, mapMaybe, slice)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldl)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String.CodePoints (fromCodePointArray, toCodePointArray)
import Data.Tuple (Tuple(..))
import Json.Core.Types (Builder)

-- Code points of the characters the grammar inspects, named so the parser reads as JSON rather
-- than as hex. (Definitions are the code points; the trailing comment shows the character.)

-- structural punctuation
leftBrace :: Int
leftBrace = 0x7B -- '{'

rightBrace :: Int
rightBrace = 0x7D -- '}'

leftSquare :: Int
leftSquare = 0x5B -- '['

rightSquare :: Int
rightSquare = 0x5D -- ']'

colon :: Int
colon = 0x3A -- ':'

comma :: Int
comma = 0x2C -- ','

quote :: Int
quote = 0x22 -- '"'

backslash :: Int
backslash = 0x5C -- '\'

slash :: Int
slash = 0x2F -- '/'

-- number lexemes
minus :: Int
minus = 0x2D -- '-'

plus :: Int
plus = 0x2B -- '+'

dot :: Int
dot = 0x2E -- '.'

digit0 :: Int
digit0 = 0x30 -- '0'

digit1 :: Int
digit1 = 0x31 -- '1'

digit9 :: Int
digit9 = 0x39 -- '9'

lowerE :: Int
lowerE = 0x65 -- 'e'

upperE :: Int
upperE = 0x45 -- 'E'

-- literal heads and escape selectors
lowerT :: Int
lowerT = 0x74 -- 't'

lowerF :: Int
lowerF = 0x66 -- 'f' (also the upper bound of the lowercase hex range)

lowerN :: Int
lowerN = 0x6E -- 'n'

lowerR :: Int
lowerR = 0x72 -- 'r'

lowerB :: Int
lowerB = 0x62 -- 'b'

lowerU :: Int
lowerU = 0x75 -- 'u'

lowerA :: Int
lowerA = 0x61 -- 'a'

upperA :: Int
upperA = 0x41 -- 'A'

upperF :: Int
upperF = 0x46 -- 'F'

-- whitespace and the control characters an escape expands to
space :: Int
space = 0x20 -- ' '

tab :: Int
tab = 0x09 -- '\t'

newline :: Int
newline = 0x0A -- '\n'

carriageReturn :: Int
carriageReturn = 0x0D -- '\r'

backspace :: Int
backspace = 0x08 -- '\b'

formfeed :: Int
formfeed = 0x0C -- '\f'

-- UTF-16 surrogate ranges, for decoding `\uXXXX` pairs
highSurrogateMin :: Int
highSurrogateMin = 0xD800

highSurrogateMax :: Int
highSurrogateMax = 0xDBFF

lowSurrogateMin :: Int
lowSurrogateMin = 0xDC00

lowSurrogateMax :: Int
lowSurrogateMax = 0xDFFF

-- a supplementary-plane code point is `0x10000 + (high - hMin) * 0x400 + (low - lMin)`
supplementaryBase :: Int
supplementaryBase = 0x10000

lowSurrogateCount :: Int
lowSurrogateCount = 0x400 -- 1024 low surrogates per high surrogate

-- | Parse a JSON string into a value of any representation `j`, given a `Builder` for `j`.
-- | `Left` carries a human-readable message with the offending code-point index.
parse :: forall j. Builder j -> String -> Either String j
parse builder s = do
  Tuple i1 v <- parseValue (skipWs 0)
  let i2 = skipWs i1
  if peek i2 == eof then Right v
  else Left ("unexpected trailing character at index " <> show i2)
  where
  cps :: Array Int
  cps = map fromEnum (toCodePointArray s)

  eof :: Int
  eof = -1

  peek :: Int -> Int
  peek i = fromMaybe eof (index cps i)

  isWs :: Int -> Boolean
  isWs c = c == space || c == tab || c == newline || c == carriageReturn

  skipWs :: Int -> Int
  skipWs i = if isWs (peek i) then skipWs (i + 1) else i

  isDigit :: Int -> Boolean
  isDigit c = c >= digit0 && c <= digit9

  -- A node parser yields the index just past what it consumed, plus the built value.
  parseValue :: Int -> Either String (Tuple Int j)
  parseValue i =
    let c = peek i in
    if c == leftBrace then parseObject (i + 1)
    else if c == leftSquare then parseArray (i + 1)
    else if c == quote then do
      Tuple i2 str <- parseRawString i
      Right (Tuple i2 (builder.jstring str))
    else if c == lowerT then parseLiteral i "true" (builder.jboolean true)
    else if c == lowerF then parseLiteral i "false" (builder.jboolean false)
    else if c == lowerN then parseLiteral i "null" builder.jnull
    else if c == minus || isDigit c then parseNumber i
    else Left ("unexpected character at index " <> show i)

  parseLiteral :: Int -> String -> j -> Either String (Tuple Int j)
  parseLiteral i word val =
    let wcps = map fromEnum (toCodePointArray word)
        n = length wcps
    in if matches i wcps 0 n then Right (Tuple (i + n) val)
       else Left ("invalid literal at index " <> show i)

  matches :: Int -> Array Int -> Int -> Int -> Boolean
  matches i wcps k n =
    if k >= n then true
    else if peek (i + k) == fromMaybe (-2) (index wcps k) then matches i wcps (k + 1) n
    else false

  parseArray :: Int -> Either String (Tuple Int j)
  parseArray i0 =
    let i = skipWs i0 in
    if peek i == rightSquare then Right (Tuple (i + 1) (builder.jarray []))
    else arrElems i Nil

  -- Written with an explicit `case` (not `do`) so the recursive call stays in self-tail
  -- position and is compiled to a loop — stack-safe for arbitrarily long arrays.
  arrElems :: Int -> List j -> Either String (Tuple Int j)
  arrElems i acc =
    case parseValue i of
      Left e -> Left e
      Right (Tuple i2 v) ->
        let i3 = skipWs i2
            c = peek i3
            acc' = v : acc
        in
          if c == comma then arrElems (skipWs (i3 + 1)) acc'
          else if c == rightSquare then Right (Tuple (i3 + 1) (builder.jarray (fromFoldable (reverse acc'))))
          else Left ("expected ',' or ']' at index " <> show i3)

  parseObject :: Int -> Either String (Tuple Int j)
  parseObject i0 =
    let i = skipWs i0 in
    if peek i == rightBrace then Right (Tuple (i + 1) (builder.jobject []))
    else objMembers i Nil

  -- As with `arrElems`, an explicit `case` keeps the recursive call in self-tail position so it
  -- compiles to a loop — stack-safe for arbitrarily many members.
  objMembers :: Int -> List (Tuple String j) -> Either String (Tuple Int j)
  objMembers i acc =
    if peek i /= quote then Left ("expected string key at index " <> show i)
    else case parseRawString i of
      Left e -> Left e
      Right (Tuple i2 key) ->
        let i3 = skipWs i2 in
        if peek i3 /= colon then Left ("expected ':' at index " <> show i3)
        else case parseValue (skipWs (i3 + 1)) of
          Left e -> Left e
          Right (Tuple i4 v) ->
            let i5 = skipWs i4
                c = peek i5
                acc' = Tuple key v : acc
            in
              if c == comma then objMembers (skipWs (i5 + 1)) acc'
              else if c == rightBrace then Right (Tuple (i5 + 1) (builder.jobject (fromFoldable (reverse acc'))))
              else Left ("expected ',' or '}' at index " <> show i5)

  -- Numbers: validate the JSON grammar, then hand the exact lexeme to `Data.Number.fromString`.
  parseNumber :: Int -> Either String (Tuple Int j)
  parseNumber i = do
    iEnd <- scanNumber i
    case Number.fromString (sliceStr i iEnd) of
      Just n -> Right (Tuple iEnd (builder.jnumber n))
      Nothing -> Left ("invalid number literal at index " <> show i)

  scanNumber :: Int -> Either String Int
  scanNumber i = do
    let iSign = if peek i == minus then i + 1 else i
    iInt <- scanIntPart iSign
    iFrac <- if peek iInt == dot then scanDigits1 (iInt + 1) else Right iInt
    if peek iFrac == lowerE || peek iFrac == upperE then
      let iE = iFrac + 1
          iE2 = if peek iE == plus || peek iE == minus then iE + 1 else iE
      in scanDigits1 iE2
    else Right iFrac

  scanIntPart :: Int -> Either String Int
  scanIntPart i =
    let c = peek i in
    if c == digit0 then Right (i + 1) -- a leading 0 stands alone
    else if c >= digit1 && c <= digit9 then Right (scanDigits0 (i + 1))
    else Left ("invalid number at index " <> show i)

  scanDigits0 :: Int -> Int
  scanDigits0 i = if isDigit (peek i) then scanDigits0 (i + 1) else i

  scanDigits1 :: Int -> Either String Int
  scanDigits1 i =
    if isDigit (peek i) then Right (scanDigits0 (i + 1))
    else Left ("expected digit at index " <> show i)

  -- Strings. Fast path slices an escape-free run directly; the first backslash switches to the
  -- accumulating slow path (a reversed list of code-unit values).
  parseRawString :: Int -> Either String (Tuple Int String)
  parseRawString i = strFast (i + 1) (i + 1)

  strFast :: Int -> Int -> Either String (Tuple Int String)
  strFast start cur =
    let c = peek cur in
    if c == eof then Left ("unterminated string at index " <> show start)
    else if c == quote then Right (Tuple (cur + 1) (sliceStr start cur))
    else if c == backslash then strEscape (cur + 1) (toRevList start cur)
    else if c < space then Left ("unescaped control character at index " <> show cur)
    else strFast start (cur + 1)

  strBody :: Int -> List Int -> Either String (Tuple Int String)
  strBody cur acc =
    let c = peek cur in
    if c == eof then Left ("unterminated string at index " <> show cur)
    else if c == quote then Right (Tuple (cur + 1) (intsToString (fromFoldable (reverse acc))))
    else if c == backslash then strEscape (cur + 1) acc
    else if c < space then Left ("unescaped control character at index " <> show cur)
    else strBody (cur + 1) (c : acc)

  -- `cur` points at the escape selector (the char after the backslash).
  strEscape :: Int -> List Int -> Either String (Tuple Int String)
  strEscape cur acc =
    case simpleEscape (peek cur) of
      Just v -> strBody (cur + 1) (v : acc)
      Nothing ->
        if peek cur == lowerU then unicodeEscape (cur + 1) acc
        else Left ("invalid escape at index " <> show cur)

  simpleEscape :: Int -> Maybe Int
  simpleEscape c =
    if c == quote then Just quote
    else if c == backslash then Just backslash
    else if c == slash then Just slash
    else if c == lowerB then Just backspace
    else if c == lowerF then Just formfeed
    else if c == lowerN then Just newline
    else if c == lowerR then Just carriageReturn
    else if c == lowerT then Just tab
    else Nothing

  -- `cur` points at the first of four hex digits.
  unicodeEscape :: Int -> List Int -> Either String (Tuple Int String)
  unicodeEscape cur acc = do
    u <- readHex4 cur
    if isHighSurrogate u then
      if peek (cur + 4) == backslash && peek (cur + 5) == lowerU then do
        lo <- readHex4 (cur + 6)
        if isLowSurrogate lo then strBody (cur + 10) (combineSurrogate u lo : acc)
        else Left ("invalid low surrogate at index " <> show (cur + 6))
      else Left ("expected low surrogate at index " <> show cur)
    -- A lone low surrogate is not a scalar value; reject it rather than let `toEnum` drop it
    -- silently when the string is built (keeps the high/low surrogate handling consistent).
    else if isLowSurrogate u then Left ("unexpected lone low surrogate at index " <> show cur)
    else strBody (cur + 4) (u : acc)

  isHighSurrogate :: Int -> Boolean
  isHighSurrogate u = u >= highSurrogateMin && u <= highSurrogateMax

  isLowSurrogate :: Int -> Boolean
  isLowSurrogate u = u >= lowSurrogateMin && u <= lowSurrogateMax

  combineSurrogate :: Int -> Int -> Int
  combineSurrogate hi lo =
    supplementaryBase + (hi - highSurrogateMin) * lowSurrogateCount + (lo - lowSurrogateMin)

  readHex4 :: Int -> Either String Int
  readHex4 i = do
    a <- hexAt i
    b <- hexAt (i + 1)
    c <- hexAt (i + 2)
    d <- hexAt (i + 3)
    Right (a * 4096 + b * 256 + c * 16 + d)

  hexAt :: Int -> Either String Int
  hexAt i =
    let c = peek i in
    if c >= digit0 && c <= digit9 then Right (c - digit0)
    else if c >= lowerA && c <= lowerF then Right (c - lowerA + 10)
    else if c >= upperA && c <= upperF then Right (c - upperA + 10)
    else Left ("invalid \\u escape at index " <> show i)

  sliceStr :: Int -> Int -> String
  sliceStr a b = intsToString (slice a b cps)

  intsToString :: Array Int -> String
  intsToString arr = fromCodePointArray (mapMaybe toEnum arr)

  -- The slice `[a, b)` as a reversed code-point list (seed for the slow string path).
  toRevList :: Int -> Int -> List Int
  toRevList a b = foldl (\l x -> x : l) Nil (slice a b cps)
