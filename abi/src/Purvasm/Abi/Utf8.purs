-- | `String` as the UTF-8 byte sequence the ABI defines it to be (ADR-0006).
-- |
-- | Here rather than in the backend for the same reason as [Purvasm.Abi.Fnv1a64]: these bytes are what
-- | a constructor tag and a record label id are hashed FROM, and the owned VM mints those numbers too
-- | (ADR-0111 §3). boot's `String` is already bytes; PureScript's is UTF-16, so a code point is
-- | re-encoded to its 1–4 UTF-8 bytes — which is also what makes a string constant's byte length and
-- | escaping match boot's byte iteration.
module Purvasm.Abi.Utf8
  ( utf8Bytes
  ) where

import Prelude

import Data.Array as Array
import Data.Enum (fromEnum)
import Data.Int.Bits (and, shr, (.|.))
import Data.String.CodePoints (toCodePointArray)

utf8Bytes :: String -> Array Int
utf8Bytes s = Array.concatMap enc (toCodePointArray s)
  where
  enc cp =
    let
      n = fromEnum cp
    in
      if n < 0x80 then [ n ]
      else if n < 0x800 then
        [ 0xC0 .|. (n `shr` 6), 0x80 .|. (n `and` 0x3F) ]
      else if n < 0x10000 then
        [ 0xE0 .|. (n `shr` 12), 0x80 .|. ((n `shr` 6) `and` 0x3F), 0x80 .|. (n `and` 0x3F) ]
      else
        [ 0xF0 .|. (n `shr` 18)
        , 0x80 .|. ((n `shr` 12) `and` 0x3F)
        , 0x80 .|. ((n `shr` 6) `and` 0x3F)
        , 0x80 .|. (n `and` 0x3F)
        ]
