-- | The shared bytecode JSON encoding (ADR-0033): instructions, chunks, literals, and
-- | `Gdef`s, plus a compact JSON serialiser that matches OCaml `Yojson.Safe.to_string`
-- | byte-for-byte (insertion-ordered keys, no spaces, standard escaping). Keeping it
-- | byte-identical is what lets `.pmo`/`.pmi` equal boot's `.pvmo`/`.pvmi`.
module Purvasm.Compiler.Bytecode.Image where

import Prelude

import Data.Array (concatMap, filter, fromFoldable, null) as Array
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Either (Either(..))
import Data.Int (hexadecimal, toStringAs)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.List (List(..), (:))
import Data.List as List
import Data.String (length) as Str
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Monoid (power)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Linearise (linearise)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Primitive (PrimOp(..))
import Purvasm.Compiler.Util.Int64Decimal (int64BitsDecimal)
import Purvasm.Number (floatBitsHi, floatBitsLo)

-- | Artifact-compatibility version, stamped into every `.pmo`/`.pmi` (boot's
-- | `Image.format_version`). Bump on any codegen change so a stale object is rejected.
formatVersion :: Int
formatVersion = 3

-- | The **linked image** format the owned VM reads: each native leaf's `ForeignRef` carries its
-- | physical arity (§4(a)) and each `case` keeps its tree shape (§4(b)).
-- |
-- | Only the `app.pvm` version field moves. `.pmo`/`.pmi` stay at `formatVersion` and stay linear:
-- | the arity is a *link-time* fact (the linker resolves it from the whole closure's FSR shapes), and
-- | nothing reads a `.pmo` but this compiler.
-- |
-- | Version **4** was this format with linear `case`s. It existed for one purpose — letting boot and
-- | the owned VM be compared on instruction counts over the same compilation (§6 step C) — and that
-- | comparison is done and recorded, so it is no longer produced or read. Version **3** still is:
-- | boot's frozen VM reads it, and the two runners are still held to the same *output*.
treeVersion :: Int
treeVersion = 5

-- | How a `ForeignRef` is written. The legacy form drops the arity, because that is the only form
-- | boot's reader accepts; `ArityFrom` writes it, looked up in the link-time leaf map.
data ForeignArity
  = ArityErased
  | ArityFrom (Map String Int)

-- --- a JSON tree with a Yojson-faithful compact serialiser --------------------------

data Json
  = JBool Boolean
  | JInt Int
  | JStr String
  | JArr (Array Json)
  | JObj (Array (String /\ Json))

-- | Serialise a `Json` tree (byte-identical output). The naive recursive form
-- | (`"[" <> joinWith "," (map stringify xs) <> "]"`) re-concatenates each node's whole subtree
-- | string at every level; on a backend where `<>` is an O(n) byte copy (not a rope) this is
-- | ~O(output × depth) and dominates native build time. Instead, flatten the tree to a token
-- | list (each leaf string allocated once, prepended O(1)) and `joinWith ""` exactly once at the
-- | end, so every byte is copied a constant number of times.
stringify :: Json -> String
stringify j = joinWith "" (Array.fromFoldable (List.reverse (go j Nil)))
  where
  go :: Json -> List String -> List String
  go val acc = case val of
    JBool b -> (if b then "true" else "false") : acc
    JInt n -> show n : acc
    JStr s -> jstr s : acc
    JArr xs -> "]" : goElems xs true ("[" : acc)
    JObj kvs -> "}" : goMembers kvs true ("{" : acc)

  goElems :: Array Json -> Boolean -> List String -> List String
  goElems xs first acc = foldl step (Tuple first acc) xs # snd'
    where
    step (Tuple isFirst a) x = Tuple false (go x (if isFirst then a else "," : a))
    snd' (Tuple _ a) = a

  goMembers :: Array (String /\ Json) -> Boolean -> List String -> List String
  goMembers kvs first acc = foldl step (Tuple first acc) kvs # snd'
    where
    step (Tuple isFirst a) (k /\ v) =
      Tuple false (go v (":" : jstr k : (if isFirst then a else "," : a)))
    snd' (Tuple _ a) = a

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
-- | `float_to_json`), so it round-trips bit-for-bit through the text format. The bits come from
-- | the `purvasm-base` float-bits read; the 64-bit decimal spelling is ordinary PureScript.
floatToJson :: Number -> Json
floatToJson f = JStr (int64BitsDecimal { hi: floatBitsHi f, lo: floatBitsLo f })

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
  AndInt -> "AndInt"
  OrInt -> "OrInt"
  XorInt -> "XorInt"
  ShlInt -> "ShlInt"
  ShrInt -> "ShrInt"
  ZshrInt -> "ZshrInt"
  ComplementInt -> "ComplementInt"
  AddNumber -> "AddNumber"
  SubNumber -> "SubNumber"
  MulNumber -> "MulNumber"
  DivNumber -> "DivNumber"
  IntToNumber -> "IntToNumber"
  NumberToInt -> "NumberToInt"
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
  RecordGet -> "RecordGet"
  RecordSet -> "RecordSet"
  RecordHas -> "RecordHas"
  RecordDelete -> "RecordDelete"
  RecordUnion -> "RecordUnion"

-- --- instructions / chunks / gdefs --------------------------------------------------

-- | The legacy encoding — every existing reader's, boot's included.
instrToJson :: Instruction -> Json
instrToJson = instrToJsonWith ArityErased

instrToJsonWith :: ForeignArity -> Instruction -> Json
instrToJsonWith fa i = case i of
  PushInt n -> t "pi" [ JInt n ]
  PushNumber f -> t "pn" [ floatToJson f ]
  PushBool b -> t "pb" [ JBool b ]
  PushString s -> t "ps" [ JStr s ]
  Load s -> t "ld" [ JStr s ]
  ForeignRef s -> case fa of
    ArityErased -> t "fr" [ JStr s ]
    -- Unreachable: `missingForeignArities` refuses the image before this runs. Written as an
    -- **impossible** arity rather than a plausible `0` (a foreign constant legitimately has arity 0),
    -- so a future caller that skips the check is refused by the reader instead of silently
    -- under-applying a leaf at run time.
    ArityFrom m -> t "fr" [ JStr s, JInt (fromMaybe (-1) (Map.lookup s m)) ]
  Bind s -> t "bd" [ JStr s ]
  Closure ps body -> t "cl" [ strs ps, chunkToJsonWith fa body ]
  MakeRec ms -> t "mr" [ JArr (map (\(n /\ c) -> JArr [ JStr n, chunkToJsonWith fa c ]) ms) ]
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
  -- The tree form (§4(b)) and the linearised one share their tags: a reader knows which it is looking
  -- at from the image's version stamp, exactly as it does for `fr`'s arity. Giving the flattened form
  -- its own tags would let a version-3 image carry a version-5 shape and still parse.
  SwitchCtor cs d -> t "sc" [ JArr (map (\(tag /\ b) -> JArr [ JStr tag, chunkToJsonWith fa b ]) cs), chunkToJsonWith fa d ]
  SwitchLit cs d -> t "sl" [ JArr (map (\(l /\ b) -> JArr [ litToJson l, chunkToJsonWith fa b ]) cs), chunkToJsonWith fa d ]
  SwitchLen cs d -> t "sn" [ JArr (map (\(k /\ b) -> JArr [ JInt k, chunkToJsonWith fa b ]) cs), chunkToJsonWith fa d ]
  Guarded cs ft ->
    t "gd"
      [ JArr (map (\c -> JArr [ chunkToJsonWith fa c.guard, chunkToJsonWith fa c.rhs ]) cs)
      , chunkToJsonWith fa ft
      ]
  SwitchCtorRel cs d -> t "sc" [ JArr (map (\(tag /\ r) -> JArr [ JStr tag, JInt r ]) cs), JInt d ]
  SwitchLitRel cs d -> t "sl" [ JArr (map (\(l /\ r) -> JArr [ litToJson l, JInt r ]) cs), JInt d ]
  SwitchLenRel cs d -> t "sn" [ JArr (map (\(k /\ r) -> JArr [ JInt k, JInt r ]) cs), JInt d ]
  Fail m -> t "fl" [ JStr m ]
  where
  t tag rest = JArr ([ JStr tag ] <> rest)

chunkToJson :: CodeBlock -> Json
chunkToJson = chunkToJsonWith ArityErased

chunkToJsonWith :: ForeignArity -> CodeBlock -> Json
chunkToJsonWith fa c = JArr (map (instrToJsonWith fa) c)

gdefToJson :: Gdef -> Json
gdefToJson = gdefToJsonWith ArityErased

gdefToJsonWith :: ForeignArity -> Gdef -> Json
gdefToJsonWith fa = case _ of
  Gfun ps c -> JArr [ JStr "fn", strs ps, chunkToJsonWith fa c ]
  Gcaf c -> JArr [ JStr "caf", chunkToJsonWith fa c ]
  Grec c -> JArr [ JStr "rec", chunkToJsonWith fa c ]

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

-- | boot's version-3 image: the `case`s flattened back to relative offsets on the way out
-- | (`Linearise`), because that is the only shape its reader knows.
imageToString :: Image -> String
imageToString = stringify <<< imageToJson <<< lineariseImage

-- | Every chunk of an image through `Linearise.linearise`.
lineariseImage :: Image -> Image
lineariseImage img = img
  { gdefs = map (\(n /\ g) -> n /\ lineariseGdef g) img.gdefs
  , main = linearise img.main
  }

lineariseGdef :: Gdef -> Gdef
lineariseGdef = case _ of
  Gfun ps c -> Gfun ps (linearise c)
  Gcaf c -> Gcaf (linearise c)
  Grec c -> Grec (linearise c)

-- | The same program in the owned VM's format: every `ForeignRef` gains the leaf's **physical
-- | closure arity** (ADR-0110 §4(a)) and every `case` keeps its **tree shape** (§4(b)).
-- |
-- | The arity is the fact boot's VM supplies from `Ffi.foreign_arity` and an owned VM has no way to
-- | know; `arities` is `NativeLeaf.nativeLeafArities` over the whole closure's FSR shapes, the same
-- | derivation the backends use to decide which keys are leaves at all. The tree is simply not
-- | flattened on the way out — unlike `imageToString`, which must, because boot's reader knows only
-- | offsets.
-- |
-- | `Left` when the image references a leaf the map does not describe. That is a wiring fault rather
-- | than a user error, but it is reported as data instead of a crash because it is genuinely
-- | reachable: the linker compiles the FFI ladder's structural terms itself, and one of those can
-- | reference a leaf no module in the closure declares.
imageToStringWithArities :: Map String Int -> Image -> Either String String
imageToStringWithArities arities img = case missingForeignArities arities img of
  missing
    | Array.null missing -> Right (stringify (imageToJsonWith (ArityFrom arities) img))
    | otherwise -> Left
        ( "the linked image references native leaves with no reconstructed arity: "
            <> joinWith ", " missing
            <> " (FSR must describe every native leaf, ADR-0090)"
        )

imageToJsonWith :: ForeignArity -> Image -> Json
imageToJsonWith fa img = JObj
  [ "version" /\ JInt version
  , "gdefs" /\ JArr (map (\(n /\ g) -> JArr [ JStr n, gdefToJsonWith fa g ]) img.gdefs)
  , "main" /\ chunkToJsonWith fa img.main
  , "effect" /\ JBool img.isEffect
  ]
  where
  version = case fa of
    ArityErased -> formatVersion
    ArityFrom _ -> treeVersion

-- | The `ForeignRef` keys the image references but `arities` does not describe, sorted. Collected
-- | over the same tree the writer walks — gdef bodies and the `main` chunk, descending into nested
-- | closure and recursive-group chunks — so a leaf reachable only from inside a closure cannot slip
-- | past the check and be written with the impossible arity.
missingForeignArities :: Map String Int -> Image -> Array String
missingForeignArities arities img =
  Array.filter (\k -> not (Map.member k arities)) (Set.toUnfoldable (foreignRefKeys img))

foreignRefKeys :: Image -> Set String
foreignRefKeys img =
  foldl (\acc (_ /\ g) -> Set.union acc (gdefKeys g)) (chunkKeys img.main) img.gdefs
  where
  gdefKeys = case _ of
    Gfun _ c -> chunkKeys c
    Gcaf c -> chunkKeys c
    Grec c -> chunkKeys c

  chunkKeys = foldl (\acc i -> Set.union acc (instrKeys i)) Set.empty

  instrKeys = case _ of
    ForeignRef k -> Set.singleton k
    Closure _ body -> chunkKeys body
    MakeRec ms -> foldl (\acc (_ /\ c) -> Set.union acc (chunkKeys c)) Set.empty ms
    SwitchCtor cs d -> arms (map (\(_ /\ b) -> b) cs) d
    SwitchLit cs d -> arms (map (\(_ /\ b) -> b) cs) d
    SwitchLen cs d -> arms (map (\(_ /\ b) -> b) cs) d
    Guarded cs ft -> arms (Array.concatMap (\c -> [ c.guard, c.rhs ]) cs) ft
    _ -> Set.empty

  arms blocks default = foldl (\acc b -> Set.union acc (chunkKeys b)) (chunkKeys default) blocks
