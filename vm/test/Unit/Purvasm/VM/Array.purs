-- | The array cell's contract
-- | ([ADR-0110](../../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §3).
-- |
-- | This module carries the identity invariant the whole FFI boundary is built on: a purvasm array is
-- | mutated in place and `Ref` *is* a one-element array, so every alias must observe every write. The
-- | cell is what makes that true, and these tests are what keep it true — the promotion path
-- | (ADR-0111 §3) will extend them rather than replace them, since promotion must not change what an
-- | alias sees.
module Test.Unit.Purvasm.VM.Array (spec) where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message, try)
import Purvasm.VM.Array as VMArray
import Purvasm.VM.Value (ArrayCell, Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Read an element as an `Int`, so an assertion reads as a value rather than a shape.
peek :: ArrayCell -> Int -> Aff (Maybe Int)
peek cell i = liftEffect do
  slot <- VMArray.index cell i
  pure case slot of
    Just (VInt n) -> Just n
    _ -> Nothing

spec :: Spec Unit
spec = describe "Purvasm.VM.Array" do
  describe "allocation" do
    it "fills a fresh array with zeroes rather than leaving slots undefined" do
      cell <- liftEffect (VMArray.new 3)
      n <- liftEffect (VMArray.length cell)
      n `shouldEqual` 3
      peek cell 0 >>= shouldEqual (Just 0)
      peek cell 2 >>= shouldEqual (Just 0)

    it "allocates an empty array" do
      cell <- liftEffect (VMArray.new 0)
      n <- liftEffect (VMArray.length cell)
      n `shouldEqual` 0

    it "is stuck on a negative length" do
      diagnostic <- liftEffect do
        result <- try (VMArray.new (-1))
        pure (either message (const "unexpectedly allocated") result)
      diagnostic `shouldSatisfy` contains "negative length"

  describe "bounds" do
    it "reports an out-of-range read rather than reading past the end" do
      cell <- liftEffect (VMArray.fromValues [ VInt 1, VInt 2 ])
      peek cell 2 >>= shouldEqual Nothing
      peek cell (-1) >>= shouldEqual Nothing

    it "refuses an out-of-range write" do
      cell <- liftEffect (VMArray.fromValues [ VInt 1 ])
      ok <- liftEffect (VMArray.write cell 1 (VInt 9))
      ok `shouldEqual` false
      refused <- liftEffect (VMArray.write cell (-1) (VInt 9))
      refused `shouldEqual` false
      -- The refused writes must not have disturbed the array.
      peek cell 0 >>= shouldEqual (Just 1)

  describe "identity" do
    it "makes a write visible through every alias of the same array" do
      -- The two names below are the same cell, which is exactly what `VArray` sharing means.
      cell <- liftEffect (VMArray.fromValues [ VInt 1, VInt 2 ])
      let alias = cell
      _ <- liftEffect (VMArray.write cell 0 (VInt 42))
      peek alias 0 >>= shouldEqual (Just 42)

    it "keeps a nested array shared, so a write reaches it through its container" do
      inner <- liftEffect (VMArray.fromValues [ VInt 1 ])
      outer <- liftEffect (VMArray.fromValues [ VArray inner ])
      _ <- liftEffect (VMArray.write inner 0 (VInt 7))
      slot <- liftEffect (VMArray.index outer 0)
      case slot of
        Just (VArray reached) -> peek reached 0 >>= shouldEqual (Just 7)
        _ -> shouldEqual "an array" "something else"

contains :: String -> String -> Boolean
contains needle haystack = String.contains (String.Pattern needle) haystack
