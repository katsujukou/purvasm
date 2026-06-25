-- | The shared bytecode JSON encoding (ADR-0033): instructions, chunks, literals, and
-- | `Gdef`s, plus a compact JSON serialiser that matches OCaml `Yojson.Safe.to_string`
-- | byte-for-byte (insertion-ordered keys, no spaces, standard escaping). Keeping it
-- | byte-identical is what lets `.pmo`/`.pmi` equal boot's `.pvmo`/`.pvmi`.
module Purvasm.Compiler.Bytecode.Image where

import Prelude

import Data.Char (toCharCode)
import Data.Int (hexadecimal, toStringAs)
import Data.String (length) as Str
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Monoid (power)
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | Artifact-compatibility version, stamped into every `.pmo`/`.pmi` (boot's
-- | `Image.format_version`). Bump on any codegen change so a stale object is rejected.
formatVersion :: Int
formatVersion = 2

-- --- a JSON tree with a Yojson-faithful compact serialiser --------------------------

data Json
  = JBool Boolean
  | JInt Int
  | JStr String
  | JArr (Array Json)
  | JObj (Array (String /\ Json))

stringify :: Json -> String
stringify = case _ of
  JBool b -> if b then "true" else "false"
  JInt n -> show n
  JStr s -> jstr s
  JArr xs -> "[" <> joinWith "," (map stringify xs) <> "]"
  JObj kvs -> "{" <> joinWith "," (map (\(k /\ v) -> jstr k <> ":" <> stringify v) kvs) <> "}"

jstr :: String -> String
jstr s = "\"" <> (joinWith "" (map esc (toCharArray s))) <> "\""
  where
  esc c =
    let
      n = toCharCode c
    in
      if n == 0x22 then "\\\""
      else if n == 0x5C then "\\\\"
      else if n == 0x08 then "\\b"
      else if n == 0x09 then "\\t"
      else if n == 0x0A then "\\n"
      else if n == 0x0C then "\\f"
      else if n == 0x0D then "\\r"
      else if n < 0x20 then "\\u" <> pad4 (toStringAs hexadecimal n)
      else singleton c
  pad4 h = power "0" (4 - Str.length h) <> h

strs :: Array String -> Json
strs = JArr <<< map JStr

-- --- literals / primitives ----------------------------------------------------------

-- | A `Number` literal is stored as its exact IEEE-754 bits in a decimal string (boot's
-- | `float_to_json`), so it round-trips bit-for-bit through the text format.
foreign import floatBitsDecimalImpl :: Number -> String

floatToJson :: Number -> Json
floatToJson f = JStr (floatBitsDecimalImpl f)

litToJson :: Literal -> Json
litToJson = case _ of
  LInt n -> JArr [ JStr "i", JInt n ]
  LNumber f -> JArr [ JStr "n", floatToJson f ]
  LBool b -> JArr [ JStr "b", JBool b ]
  LString s -> JArr [ JStr "s", JStr s ]

primTag :: PrimOp -> String
primTag = case _ of
  AddInt -> "AddInt"
  SubInt -> "SubInt"
  MulInt -> "MulInt"
  DivInt -> "DivInt"
  ModInt -> "ModInt"
  AddNumber -> "AddNumber"
  SubNumber -> "SubNumber"
  MulNumber -> "MulNumber"
  DivNumber -> "DivNumber"
  EqInt -> "EqInt"
  EqString -> "EqString"
  EqNumber -> "EqNumber"
  EqBool -> "EqBool"
  LtInt -> "LtInt"
  LtString -> "LtString"
  LtNumber -> "LtNumber"
  AndBool -> "AndBool"
  OrBool -> "OrBool"
  NotBool -> "NotBool"
  Append -> "Append"
  IndexArray -> "IndexArray"
  LengthArray -> "LengthArray"
  NewArray -> "NewArray"
  SetArray -> "SetArray"

-- --- instructions / chunks / gdefs --------------------------------------------------

instrToJson :: Instruction -> Json
instrToJson i = case i of
  PushInt n -> t "pi" [ JInt n ]
  PushNumber f -> t "pn" [ floatToJson f ]
  PushBool b -> t "pb" [ JBool b ]
  PushString s -> t "ps" [ JStr s ]
  Load s -> t "ld" [ JStr s ]
  ForeignRef s -> t "fr" [ JStr s ]
  Bind s -> t "bd" [ JStr s ]
  Closure ps body -> t "cl" [ strs ps, chunkToJson body ]
  MakeRec ms -> t "mr" [ JArr (map (\(n /\ c) -> JArr [ JStr n, chunkToJson c ]) ms) ]
  Ctor tag arity n -> t "ct" [ JStr tag, JInt arity, JInt n ]
  Record ls -> t "rc" [ strs ls ]
  Array n -> t "arr" [ JInt n ]
  GetField l -> t "gf" [ JStr l ]
  Proj j -> t "pj" [ JInt j ]
  Proj_arr j -> t "pa" [ JInt j ]
  Update ls -> t "up" [ strs ls ]
  Prim op n -> t "pm" [ JStr (primTag op), JInt n ]
  Call n -> t "ca" [ JInt n ]
  TailCall n -> t "tc" [ JInt n ]
  Return -> t "rt" []
  Jump r -> t "jp" [ JInt r ]
  JumpUnless r -> t "ju" [ JInt r ]
  SwitchCtor cs d -> t "sc" [ JArr (map (\(tag /\ r) -> JArr [ JStr tag, JInt r ]) cs), JInt d ]
  SwitchLit cs d -> t "sl" [ JArr (map (\(l /\ r) -> JArr [ litToJson l, JInt r ]) cs), JInt d ]
  SwitchLen cs d -> t "sn" [ JArr (map (\(k /\ r) -> JArr [ JInt k, JInt r ]) cs), JInt d ]
  Fail m -> t "fl" [ JStr m ]
  where
  t tag rest = JArr ([ JStr tag ] <> rest)

chunkToJson :: CodeBlock -> Json
chunkToJson c = JArr (map instrToJson c)

gdefToJson :: Gdef -> Json
gdefToJson = case _ of
  Gfun ps c -> JArr [ JStr "fn", strs ps, chunkToJson c ]
  Gcaf c -> JArr [ JStr "caf", chunkToJson c ]
  Grec c -> JArr [ JStr "rec", chunkToJson c ]

-- --- the linked image (app.pvm) -----------------------------------------------------

-- | A linked, runnable program (boot's `Image.t`): its global definitions (in dependency
-- | order), the `main` chunk that runs the entry, and whether the entry is an `Effect`
-- | (so the runner performs it and suppresses the `Unit` result).
type Image =
  { gdefs :: Array (String /\ Gdef)
  , main :: CodeBlock
  , isEffect :: Boolean
  }

imageToJson :: Image -> Json
imageToJson img = JObj
  [ "version" /\ JInt formatVersion
  , "gdefs" /\ JArr (map (\(n /\ g) -> JArr [ JStr n, gdefToJson g ]) img.gdefs)
  , "main" /\ chunkToJson img.main
  , "effect" /\ JBool img.isEffect
  ]

imageToString :: Image -> String
imageToString = stringify <<< imageToJson
