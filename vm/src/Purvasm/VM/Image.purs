-- | The image reader: today's `.pvm`, decoded into the VM's own vocabulary
-- | ([ADR-0110](../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §6's slice 2).
-- |
-- | This is the step that makes the owned VM able to run what `purvasm run` produces, and it reads
-- | the format **as it is today** rather than as §4 will leave it. Two consequences show up as
-- | explicit arms below, and both are temporary by design:
-- |
-- |   * a `case` arrives **linearised** — switches over relative offsets into a flat block — so it
-- |     decodes to the `*Rel` instructions. §4(b) replaces the format with the tree the producer
-- |     already had, and those instructions go with it;
-- |   * a foreign reference arrives **without its arity**, which the VM needs before it can build a
-- |     leaf's closure (§4(a)). Rather than invent one, the reader refuses such an image by name.
-- |     That is why slice 2's step A is verified on programs with no foreign leaf.
-- |
-- | Every failure is a `Left` with a path into the tree, because the thing being read is a *file*:
-- | "unexpected token" without a location is not a diagnosis a build can act on.
module Purvasm.VM.Image
  ( Gdef(..)
  , Image
  , decodeImage
  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Abi.Float64 as Float64
import Purvasm.VM.Image.Json (Json(..), entry, parseJson)
import Purvasm.VM.Instruction (CodeBlock, GuardClause, Instruction(..), Literal(..), PrimOp(..))

-- | A global's shape, as the linker wrote it: a function of known parameters, a by-need constant, or
-- | a recursive group member (which is a constant whose construction may refer to the group).
data Gdef
  = Gfun (Array String) CodeBlock
  | Gcaf CodeBlock
  | Grec CodeBlock

-- | A linked, runnable program: its globals in dependency order, the chunk that runs the entry, and
-- | whether that entry is an `Effect` (which decides the terminal demand, ADR-0110 §5).
type Image =
  { gdefs :: Array (String /\ Gdef)
  , main :: CodeBlock
  , isEffect :: Boolean
  }

-- | The format version this reader understands: leaf arities on `ForeignRef` (§4(a)) and tree-shaped
-- | `case` dispatch (§4(b)). Bumped in lockstep with the producer; anything else is refused rather
-- | than guessed, since a misparse of a *stale* image is the failure the stamp exists to make loud
-- | (ADR-0110 §Consequences).
-- |
-- | **One** version, now that the migration is over. The two it replaced are named in the refusal
-- | rather than read:
-- |
-- |   * **3** is boot's, and boot is the runner for it — this VM read foreign-free version-3 images
-- |     only for as long as the two runners were being compared on the same compilation (§6 step C),
-- |     which is done and recorded.
-- |   * **4** was this format with linearised `case`s. It existed for that comparison alone, and
-- |     nothing produces or keeps one.
supportedVersion :: Int
supportedVersion = 5

decodeImage :: String -> Either String Image
decodeImage text = do
  json <- parseJson text
  fields <- object "the image" json
  version <- field "version" fields >>= int "version"
  when (version /= supportedVersion) $ Left
    ( "unsupported image format version " <> show version
        <> " (this VM reads version "
        <> show supportedVersion
        <> reason version
        <> "); rebuild the image with a matching compiler"
    )
  gdefs <- field "gdefs" fields >>= array "gdefs" >>= traverse gdefEntry
  main <- field "main" fields >>= chunk "main"
  isEffect <- field "effect" fields >>= boolean "effect"
  pure { gdefs, main, isEffect }

-- | `version` is threaded down to every instruction rather than consulted once: an opcode's shape is
-- | only meaningful under the version that declared it, and a reader that accepts a shape from the
-- | wrong version is guessing at what the producer meant — the thing the stamp exists to prevent.
-- | What to add to an unsupported-version refusal when the version is one this VM used to read. A
-- | bare number tells the reader nothing about which runner it should have used.
reason :: Int -> String
reason = case _ of
  3 -> "; version 3 is boot's format — run it with `purvm`"
  4 -> "; version 4 was the linear-`case` form of the step-C comparison and is no longer produced"
  _ -> ""

gdefEntry :: Json -> Either String (String /\ Gdef)
gdefEntry j = case j of
  JArray [ JString name, body ] -> (\g -> name /\ g) <$> gdef name body
  _ -> Left "a gdef entry must be [name, definition]"

gdef :: String -> Json -> Either String Gdef
gdef name j = case j of
  JArray [ JString "fn", params, body ] -> Gfun <$> strings ("the parameters of " <> name) params <*> chunk name body
  JArray [ JString "caf", body ] -> Gcaf <$> chunk name body
  JArray [ JString "rec", body ] -> Grec <$> chunk name body
  _ -> Left ("unrecognised global definition for " <> name)

chunk :: String -> Json -> Either String CodeBlock
chunk what j = array what j >>= traverse (instruction what)

instruction :: String -> Json -> Either String Instruction
instruction what j = case j of
  JArray [ JString "pi", n ] -> PushInt <$> int what n
  JArray [ JString "pb", b ] -> PushBool <$> boolean what b
  JArray [ JString "ps", s ] -> PushString <$> string what s
  JArray [ JString "pn", n ] -> PushNumber <$> (string what n >>= number what)
  JArray [ JString "ld", s ] -> Load <$> string what s
  JArray [ JString "bd", s ] -> Bind <$> string what s
  JArray [ JString "cl", ps, body ] -> Closure <$> strings what ps <*> chunk what body
  JArray [ JString "mr", ms ] -> MakeRec <$> (array what ms >>= traverse (recMember what))
  JArray [ JString "ct", tag, arity, n ] -> Ctor <$> string what tag <*> int what arity <*> int what n
  JArray [ JString "rc", ls ] -> Record <$> strings what ls
  JArray [ JString "arr", n ] -> Array <$> int what n
  JArray [ JString "gf", l ] -> GetField <$> string what l
  JArray [ JString "pj", i ] -> Proj <$> int what i
  JArray [ JString "pa", i ] -> ProjArray <$> int what i
  JArray [ JString "up", ls ] -> Update <$> strings what ls
  JArray [ JString "pm", op, n ] -> Prim <$> (string what op >>= primOp what) <*> int what n
  JArray [ JString "ca", n ] -> Call <$> int what n
  JArray [ JString "tc", n ] -> TailCall <$> int what n
  JArray [ JString "rt" ] -> Right Return
  JArray [ JString "jp", r ] -> Jump <$> int what r
  JArray [ JString "ju", r ] -> JumpUnless <$> int what r
  -- Tree-shaped dispatch (§4(b)): an arm carries its own block. The same tags the linear form used —
  -- the version stamp says which shape they hold, so a stale image cannot parse as a fresh one.
  JArray [ JString "sc", cs, d ] ->
    SwitchCtor <$> (array what cs >>= traverse (arm what (string what))) <*> chunk what d
  JArray [ JString "sl", cs, d ] ->
    SwitchLit <$> (array what cs >>= traverse (arm what (literal what))) <*> chunk what d
  JArray [ JString "sn", cs, d ] ->
    SwitchLen <$> (array what cs >>= traverse (arm what (int what))) <*> chunk what d
  JArray [ JString "gd", cs, ft ] ->
    Guarded <$> (array what cs >>= traverse (guardClause what)) <*> chunk what ft
  JArray [ JString "fl", m ] -> Fail <$> string what m
  JArray [ JString "fr", k, n ] -> do
    key <- string what k
    arity <- int what n
    -- A leaf's physical closure arity is a count. The writer emits an impossible -1 only where its own
    -- check has already refused the image, so a negative here means the producer skipped that check —
    -- refuse it rather than build a closure that would under-apply the leaf at its first call.
    when (arity < 0) $ Left
      ("in " <> what <> ": the native leaf " <> key <> " has a negative arity (" <> show arity <> ")")
    pure (ForeignRef key arity)
  -- The pre-§4(a) spelling, which carries nothing to recover the arity from — boot's VM answers it
  -- from a compiled-in registry this VM deliberately does not have. Reachable only from an image the
  -- version check should already have refused, so it names the format rather than the instruction.
  JArray [ JString "fr", _ ] -> Left
    ( "in " <> what
        <> ": a foreign reference without an arity — a pre-version-"
        <> show supportedVersion
        <> " image that the version stamp did not declare as one"
    )
  _ -> Left ("in " <> what <> ": unrecognised instruction")

-- | One clause of a guard chain: its test and the body that runs when the test leaves `true`.
guardClause :: String -> Json -> Either String GuardClause
guardClause what j = case j of
  JArray [ g, r ] -> { guard: _, rhs: _ } <$> chunk what g <*> chunk what r
  _ -> Left ("in " <> what <> ": a guard clause must be [guard, body]")

recMember :: String -> Json -> Either String (String /\ CodeBlock)
recMember what j = case j of
  JArray [ JString name, body ] -> (\c -> name /\ c) <$> chunk name body
  _ -> Left ("in " <> what <> ": a recursive-group member must be [name, chunk]")

-- | One switch arm: its discriminant, decoded by `discriminant`, and the block it runs.
arm :: forall d. String -> (Json -> Either String d) -> Json -> Either String (d /\ CodeBlock)
arm what discriminant j = case j of
  JArray [ d, body ] -> (\x b -> x /\ b) <$> discriminant d <*> chunk what body
  _ -> Left ("in " <> what <> ": a switch arm must be [discriminant, block]")

-- | A `Number` literal: the writer emits the *signed 64-bit decimal spelling of the IEEE-754 bit
-- | pattern* rather than a decimal fraction, precisely so the value survives the round trip exactly
-- | (ADR-0038 §4). Reading it back is `Float64.numberOfBits`, the writer's own inverse, shared
-- | through `abi` so the two cannot drift.
number :: String -> String -> Either String Number
number what text =
  note ("in " <> what <> ": a Number literal must be a 64-bit decimal bit pattern, got " <> show text)
    (Float64.numberOfBits <$> Float64.bitsOfDecimal text)

literal :: String -> Json -> Either String Literal
literal what j = case j of
  JArray [ JString "i", n ] -> LInt <$> int what n
  JArray [ JString "b", b ] -> LBool <$> boolean what b
  JArray [ JString "s", s ] -> LString <$> string what s
  JArray [ JString "n", n ] -> LNumber <$> (string what n >>= number what)
  _ -> Left ("in " <> what <> ": unrecognised literal")

-- | The primop names are the constructor names both sides spell the same way, so this is a table
-- | rather than a derivation — and an exhaustive `case` on the other side keeps it honest.
primOp :: String -> String -> Either String PrimOp
primOp what tag = note ("in " <> what <> ": unknown primop " <> tag) (lookup tag)
  where
  lookup = case _ of
    "AddInt" -> Just AddInt
    "SubInt" -> Just SubInt
    "MulInt" -> Just MulInt
    "DivInt" -> Just DivInt
    "ModInt" -> Just ModInt
    "AndInt" -> Just AndInt
    "OrInt" -> Just OrInt
    "XorInt" -> Just XorInt
    "ShlInt" -> Just ShlInt
    "ShrInt" -> Just ShrInt
    "ZshrInt" -> Just ZshrInt
    "ComplementInt" -> Just ComplementInt
    "AddNumber" -> Just AddNumber
    "SubNumber" -> Just SubNumber
    "MulNumber" -> Just MulNumber
    "DivNumber" -> Just DivNumber
    "IntToNumber" -> Just IntToNumber
    "NumberToInt" -> Just NumberToInt
    "EqInt" -> Just EqInt
    "EqString" -> Just EqString
    "EqNumber" -> Just EqNumber
    "EqBool" -> Just EqBool
    "LtInt" -> Just LtInt
    "LtString" -> Just LtString
    "LtNumber" -> Just LtNumber
    "AndBool" -> Just AndBool
    "OrBool" -> Just OrBool
    "NotBool" -> Just NotBool
    "Append" -> Just Append
    "IndexArray" -> Just IndexArray
    "LengthArray" -> Just LengthArray
    "NewArray" -> Just NewArray
    "SetArray" -> Just SetArray
    "RecordGet" -> Just RecordGet
    "RecordSet" -> Just RecordSet
    "RecordHas" -> Just RecordHas
    "RecordDelete" -> Just RecordDelete
    "RecordUnion" -> Just RecordUnion
    _ -> Nothing

field :: String -> Array (Tuple String Json) -> Either String Json
field name fields = note ("the image has no `" <> name <> "` field") (entry name fields)

object :: String -> Json -> Either String (Array (Tuple String Json))
object what = case _ of
  JObject fields -> Right fields
  _ -> Left (what <> " must be a JSON object")

array :: String -> Json -> Either String (Array Json)
array what = case _ of
  JArray xs -> Right xs
  _ -> Left ("in " <> what <> ": expected an array")

strings :: String -> Json -> Either String (Array String)
strings what j = array what j >>= traverse (string what)

string :: String -> Json -> Either String String
string what = case _ of
  JString s -> Right s
  _ -> Left ("in " <> what <> ": expected a string")

boolean :: String -> Json -> Either String Boolean
boolean what = case _ of
  JBool b -> Right b
  _ -> Left ("in " <> what <> ": expected a boolean")

-- | A JSON number that is an `Int`. The format writes every integer as one, so a fractional value
-- | here means the file is not what it claims to be.
int :: String -> Json -> Either String Int
int what = case _ of
  JNumber n -> note ("in " <> what <> ": expected an integer, got " <> show n) (Int.fromNumber n)
  _ -> Left ("in " <> what <> ": expected a number")
