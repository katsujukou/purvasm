-- | A pure-PureScript MD5 (`md5Hex`), used for the `.pmi` interface hash so artifacts are
-- | byte-identical to boot's `Digest.string` (OCaml MD5). Pure (no Node `crypto`) so it
-- | also runs on the boot-native backend. PureScript `Int` is 32-bit with wrapping
-- | arithmetic (`| 0`), exactly the word size MD5 needs; the per-round constants `K` are
-- | generated from `sin` (same IEEE doubles as the canonical table).
module Purvasm.Compiler.Util.MD5 (md5Hex) where

import Prelude

import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.Foldable (foldMap, foldl)
import Data.Int (hexadecimal, round, toNumber, toStringAs)
import Data.Int.Bits (and, complement, or, shl, xor, zshr)
import Data.Maybe (fromMaybe)
import Data.Number (abs, floor, sin) as Num
import Data.String (length) as Str
import Data.String.CodePoints (toCodePointArray)
import Data.Tuple.Nested (type (/\), (/\))

type State = { a :: Int, b :: Int, c :: Int, d :: Int }

-- | UTF-8 bytes of a string (MD5 hashes the byte sequence, as OCaml's `Digest.string`).
utf8Encode :: String -> Array Int
utf8Encode = toCodePointArray >>> Array.concatMap (encode <<< fromEnum)
  where
  encode n
    | n < 0x80 = [ n ]
    | n < 0x800 = [ 0xC0 `or` (n `zshr` 6), cont n 0 ]
    | n < 0x10000 = [ 0xE0 `or` (n `zshr` 12), cont n 6, cont n 0 ]
    | otherwise = [ 0xF0 `or` (n `zshr` 18), cont n 12, cont n 6, cont n 0 ]
  cont n sh = 0x80 `or` ((n `zshr` sh) `and` 0x3F)

-- | Convert a `Number` in `[0, 2^32)` to its signed 32-bit `Int`.
toI32 :: Number -> Int
toI32 n = if n >= 2147483648.0 then round (n - 4294967296.0) else round n

kTable :: Array Int
kTable = (0 .. 63) <#> \i -> toI32 (Num.floor (Num.abs (Num.sin (toNumber (i + 1))) * 4294967296.0))

sTable :: Array Int
sTable =
  [ 7
  , 12
  , 17
  , 22
  , 7
  , 12
  , 17
  , 22
  , 7
  , 12
  , 17
  , 22
  , 7
  , 12
  , 17
  , 22
  , 5
  , 9
  , 14
  , 20
  , 5
  , 9
  , 14
  , 20
  , 5
  , 9
  , 14
  , 20
  , 5
  , 9
  , 14
  , 20
  , 4
  , 11
  , 16
  , 23
  , 4
  , 11
  , 16
  , 23
  , 4
  , 11
  , 16
  , 23
  , 4
  , 11
  , 16
  , 23
  , 6
  , 10
  , 15
  , 21
  , 6
  , 10
  , 15
  , 21
  , 6
  , 10
  , 15
  , 21
  , 6
  , 10
  , 15
  , 21
  ]

at :: Array Int -> Int -> Int
at xs i = fromMaybe 0 (xs !! i)

rotl :: Int -> Int -> Int
rotl x n = (x `shl` n) `or` (x `zshr` (32 - n))

md5Hex :: String -> String
md5Hex s = wordHex final.a <> wordHex final.b <> wordHex final.c <> wordHex final.d
  where
  bytes = utf8Encode s
  msgLen = Array.length bytes

  padded :: Array Int
  padded = bytes <> [ 0x80 ] <> Array.replicate zeros 0 <> lenBytes
    where
    zeros = (56 - (msgLen + 1)) `mod` 64 # \z -> if z < 0 then z + 64 else z
    bits = msgLen * 8
    lenBytes = (0 .. 7) <#> \i -> if i < 4 then (bits `zshr` (8 * i)) `and` 0xFF else 0

  blocks :: Array (Array Int)
  blocks = (0 .. (Array.length padded / 64 - 1)) <#> \i -> Array.slice (i * 64) (i * 64 + 64) padded

  final :: State
  final = foldl processBlock { a: 1732584193, b: -271733879, c: -1732584194, d: 271733878 } blocks

  processBlock :: State -> Array Int -> State
  processBlock st block =
    let
      m = (0 .. 15) <#> \j ->
        at block (j * 4) `or` (at block (j * 4 + 1) `shl` 8)
          `or` (at block (j * 4 + 2) `shl` 16)
          `or` (at block (j * 4 + 3) `shl` 24)
      r = foldl (op m) st (0 .. 63)
    in
      { a: st.a + r.a, b: st.b + r.b, c: st.c + r.c, d: st.d + r.d }

  op :: Array Int -> State -> Int -> State
  op m { a, b, c, d } i =
    let
      f /\ g =
        if i < 16 then ((b `and` c) `or` (complement b `and` d)) /\ i
        else if i < 32 then ((d `and` b) `or` (complement d `and` c)) /\ ((5 * i + 1) `mod` 16)
        else if i < 48 then (b `xor` c `xor` d) /\ ((3 * i + 5) `mod` 16)
        else (c `xor` (b `or` complement d)) /\ ((7 * i) `mod` 16)
      x = a + f + at kTable i + at m g
    in
      { a: d, b: b + rotl x (at sTable i), c: b, d: c }

  wordHex :: Int -> String
  wordHex w = (0 .. 3) # foldMap \i -> hexByte ((w `zshr` (8 * i)) `and` 0xFF)

  hexByte :: Int -> String
  hexByte b = let h = toStringAs hexadecimal b in if Str.length h == 1 then "0" <> h else h
