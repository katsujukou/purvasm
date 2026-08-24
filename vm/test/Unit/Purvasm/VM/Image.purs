-- | The image reader
-- | ([ADR-0110](../../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §6's slice 2).
-- |
-- | Tested here rather than only through a native run because a reader's interesting behaviour is what
-- | it does with input it did *not* produce: a truncated instruction, a version it does not know, a
-- | form the format does not yet carry. A successful `purvasm run` exercises none of that, and the
-- | decoder is pure, so all of it is reachable on any target.
-- |
-- | The two refusals below are the ones that define slice 2's scope. They are not gaps to be tolerated
-- | quietly — an image the VM half-reads is worse than one it declines — so each says which format
-- | change would remove it.
module Test.Unit.Purvasm.VM.Image (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Purvasm.VM.Image (Gdef(..), decodeImage)
import Purvasm.VM.Instruction (Instruction(..), Literal(..), PrimOp(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | An image with the given gdefs/main bodies spliced in, in the shape the linker writes.
image :: String -> String -> String -> String
image version gdefs main =
  "{\"version\":" <> version <> ",\"gdefs\":" <> gdefs <> ",\"main\":" <> main <> ",\"effect\":false}"

contains :: String -> String -> Boolean
contains needle = String.contains (String.Pattern needle)

spec :: Spec Unit
spec = describe "Purvasm.VM.Image" do
  describe "decodeImage" do
    it "reads the instruction vocabulary a linked image uses" do
      let
        main = "[[\"pi\",1],[\"pb\",true],[\"ps\",\"s\"],[\"ld\",\"g\"],[\"bd\",\"x\"],[\"rt\"]]"
      map _.main (decodeImage (image "3" "[]" main))
        `shouldEqual` Right [ PushInt 1, PushBool true, PushString "s", Load "g", Bind "x", Return ]

    it "reads a primop by the name both sides spell it with" do
      map _.main (decodeImage (image "3" "[]" "[[\"pm\",\"AddInt\",2]]"))
        `shouldEqual` Right [ Prim AddInt 2 ]

    it "refuses a primop name it does not know, rather than skipping the instruction" do
      isLeft (decodeImage (image "3" "[]" "[[\"pm\",\"AddQuaternion\",2]]")) `shouldEqual` true

    it "reads a linearised switch as the offset form, arms and default alike" do
      -- Today's format lowers a decision tree to relative offsets; §4(b) replaces that with the tree
      -- the producer already had, and these instructions go with it.
      map _.main (decodeImage (image "3" "[]" "[[\"sc\",[[\"Just\",2],[\"Nothing\",5]],9]]"))
        `shouldEqual` Right [ SwitchCtorRel [ "Just" /\ 2, "Nothing" /\ 5 ] 9 ]

    it "reads a literal switch's discriminants" do
      map _.main (decodeImage (image "3" "[]" "[[\"sl\",[[[\"i\",1],3],[[\"s\",\"a\"],4]],7]]"))
        `shouldEqual` Right [ SwitchLitRel [ LInt 1 /\ 3, LString "a" /\ 4 ] 7 ]

    it "reads the three global shapes, which are three evaluation strategies" do
      let
        gdefs =
          "[[\"f\",[\"fn\",[\"x\"],[[\"ld\",\"x\"],[\"rt\"]]]]"
            <> ",[\"c\",[\"caf\",[[\"pi\",1]]]]"
            <> ",[\"r\",[\"rec\",[[\"pi\",2]]]]]"
      case decodeImage (image "3" gdefs "[]") of
        Right decoded -> map (\(name /\ g) -> name /\ tagOf g) decoded.gdefs
          `shouldEqual` [ "f" /\ "fn", "c" /\ "caf", "r" /\ "rec" ]
        Left e -> shouldEqual e "a successful decode"

    it "refuses a version it does not understand" do
      -- A stale image misparsed is the failure the stamp exists to make loud.
      case decodeImage (image "2" "[]" "[]") of
        Left e -> e `shouldSatisfy` contains "unsupported image format version 2"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "reads a foreign reference with its physical arity" do
      -- §4(a): the arity is what lets the VM build the leaf's closure without a registry of its own.
      case decodeImage (image "4" "[]" "[[\"fr\",\"Data.Show.showIntImpl\",1]]") of
        Right decoded -> decoded.main `shouldEqual` [ ForeignRef "Data.Show.showIntImpl" 1 ]
        Left e -> shouldEqual e "a successful decode"

    it "reads an arity-0 leaf, which is a foreign constant rather than a mistake" do
      -- `leafClosureArity` answers 0 for a non-effect leaf with no arguments, so 0 must decode. It is
      -- the boundary of the negative-arity check below, and the two are easy to conflate.
      case decodeImage (image "4" "[]" "[[\"fr\",\"Test.Ffi.constant\",0]]") of
        Right decoded -> decoded.main `shouldEqual` [ ForeignRef "Test.Ffi.constant" 0 ]
        Left e -> shouldEqual e "a successful decode"

    it "refuses a negative arity rather than building a closure from it" do
      -- The writer emits -1 only where its own check has already refused the image, so this reaching
      -- the reader means the producer skipped that check. Building the closure anyway would
      -- under-apply the leaf at its first call — a fault far from its cause.
      case decodeImage (image "4" "[]" "[[\"fr\",\"Test.Ffi.broken\",-1]]") of
        Left e -> e `shouldSatisfy` contains "negative arity"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "refuses a foreign reference with no arity, naming the version that carries one" do
      -- A version-3 image spells the leaf without its arity and holds nothing to recover it from:
      -- boot's VM answers from a compiled-in registry this VM deliberately does not have. Inventing
      -- an arity would hand native code a closure called with the wrong number of arguments.
      case decodeImage (image "3" "[]" "[[\"fr\",\"Data.Show.showIntImpl\"]]") of
        Left e -> e `shouldSatisfy` contains "without an arity"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "refuses version-4 syntax in a version-3 image, rather than reading it anyway" do
      -- The stamp decides which shapes are admissible. A reader that accepts a shape from a version
      -- the image does not declare is guessing at what the producer meant, which is the whole thing
      -- the stamp exists to prevent — and here the guess would be about how to call native code.
      case decodeImage (image "3" "[]" "[[\"fr\",\"Data.Show.showIntImpl\",1]]") of
        Left e -> e `shouldSatisfy` contains "declares version 3"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "reads a Number literal back from its exact bit pattern" do
      -- The writer emits the *bit pattern's* decimal spelling, not a decimal fraction, so the value
      -- must come back bit-identical rather than close (ADR-0038 §4).
      case decodeImage (image "3" "[]" "[[\"pn\",\"4611686018427387904\"]]") of
        Right decoded -> decoded.main `shouldEqual` [ PushNumber 2.0 ]
        Left e -> shouldEqual e "a successful decode"

    it "reads the signed spellings, negative zero included" do
      -- `-0.0` is the one value where "close enough" and "exact" differ observably through division,
      -- and it is written with the minus the two's-complement pattern implies.
      case decodeImage (image "3" "[]" "[[\"pn\",\"-4616189618054758400\"],[\"pn\",\"-9223372036854775808\"]]") of
        Right decoded -> map (\i -> show i) decoded.main
          `shouldEqual` [ show (PushNumber (-1.0)), show (PushNumber (-0.0)) ]
        Left e -> shouldEqual e "a successful decode"

    it "reads a Number discriminant in a literal switch" do
      -- The other place a Number reaches the reader; it decodes through the same inverse.
      case decodeImage (image "3" "[]" "[[\"sl\",[[[\"n\",\"4607182418800017408\"],2]],5]]") of
        Right decoded -> decoded.main `shouldEqual` [ SwitchLitRel [ LNumber 1.0 /\ 2 ] 5 ]
        Left e -> shouldEqual e "a successful decode"

    it "refuses a Number that is not a bit pattern, rather than reading it as a fraction" do
      -- `1.5` is a plausible *value* and an impossible *encoding*; taking it would silently produce a
      -- different double from the one the compiler wrote.
      case decodeImage (image "3" "[]" "[[\"pn\",\"1.5\"]]") of
        Left e -> e `shouldSatisfy` contains "64-bit decimal bit pattern"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "says where in the image a malformed instruction was" do
      -- A reader's diagnostics are about a FILE, so "unexpected token" without a location is not
      -- something a build can act on.
      case decodeImage (image "3" "[[\"g\",[\"caf\",[[\"nope\"]]]]]" "[]") of
        Left e -> e `shouldSatisfy` contains "in g"
        Right _ -> shouldEqual "a rejection" "a decode"

    it "refuses a file that is not an image at all" do
      isLeft (decodeImage "not json") `shouldEqual` true
      isLeft (decodeImage "[]") `shouldEqual` true
      isLeft (decodeImage "{\"version\":3}") `shouldEqual` true
  where
  tagOf = case _ of
    Gfun _ _ -> "fn"
    Gcaf _ -> "caf"
    Grec _ -> "rec"
