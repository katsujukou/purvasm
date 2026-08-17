-- | ADR-0109 §5.2: the foreign-closure knob's PARSE. It is the one place the environment's spelling
-- | becomes the closed `ForeignClosureMode` that both the emitter and the activation plan then read,
-- | so the only way the two legs of a paired A/B can end up being the same program is a parse that
-- | quietly defaults. It does not: anything unrecognised is an error.
module Test.Unit.Purvasm.Compiler.Backend.LLVM.ForeignRef where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Purvasm.Compiler.Backend.LLVM.ForeignRef (ForeignCallMode(..), ForeignClosureMode(..), foreignCallEnvVar, foreignClosureEnvVar, parseForeignCallMode, parseForeignClosureMode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.ForeignRef" do
  describe "parseForeignClosureMode (the ADR-0109 §5.2 knob)" do
    it "defaults to the SHIPPED mode when the knob is absent" do
      parseForeignClosureMode Nothing `shouldEqual` Right Hoisted

    it "reads both legs of the A/B by name" do
      parseForeignClosureMode (Just "hoisted") `shouldEqual` Right Hoisted
      parseForeignClosureMode (Just "per-use") `shouldEqual` Right PerUse

    it "is FAIL-CLOSED: an unrecognised value is an error, never a default" do
      -- the failure this prevents is silent and total: a typo meaning "hoisted" would make the
      -- counterfactual leg the shipped leg, and the A/B would report a real change as no change.
      for_ [ "", "1", "0", "Hoisted", "peruse", "per_use", " per-use", "true" ] \bad ->
        case parseForeignClosureMode (Just bad) of
          Left msg -> do
            unless (String.contains (Pattern foreignClosureEnvVar) msg)
              (fail ("the diagnostic must name the variable: " <> msg))
          Right m -> fail ("expected " <> show bad <> " to be rejected, got " <> show m)

    it "names the variable and renders each mode as the value that selects it" do
      foreignClosureEnvVar `shouldEqual` "PURVASM_FOREIGN_CLOSURE"
      -- `show` is what the harness prints and what the parse accepts — one spelling, both ways.
      show Hoisted `shouldEqual` "hoisted"
      show PerUse `shouldEqual` "per-use"
      parseForeignClosureMode (Just (show PerUse)) `shouldEqual` Right PerUse
      parseForeignClosureMode (Just (show Hoisted)) `shouldEqual` Right Hoisted

  describe "parseForeignCallMode (the slice B/C stage knob)" do
    it "defaults to the SHIPPED stage when the knob is absent" do
      -- slice C since 2026-08-17, on its own measured endpoint. This row is what makes a default
      -- change a deliberate edit rather than a side effect of adding a constructor.
      parseForeignCallMode Nothing `shouldEqual` Right DirectApplyAndTail

    it "reads all THREE stages by name" do
      -- three, not two: a two-state knob cannot express "apply direct, tail still deferred", and
      -- without that stage slices B and C cannot be measured apart (ADR-0109 §5.1).
      parseForeignCallMode (Just "via-apply") `shouldEqual` Right ViaApply
      parseForeignCallMode (Just "direct-apply-only") `shouldEqual` Right DirectApplyOnly
      parseForeignCallMode (Just "direct-apply-and-tail") `shouldEqual` Right DirectApplyAndTail

    it "is FAIL-CLOSED, and round-trips each stage through its own rendering" do
      for_ [ "", "direct", "1", "Direct", "apply-only", "direct_apply_only", " via-apply" ] \bad ->
        case parseForeignCallMode (Just bad) of
          Left msg ->
            unless (String.contains (Pattern foreignCallEnvVar) msg)
              (fail ("the diagnostic must name the variable: " <> msg))
          Right m -> fail ("expected " <> show bad <> " to be rejected, got " <> show m)
      -- `show` is what the harness passes and what the parse accepts — one spelling, both ways.
      for_ [ ViaApply, DirectApplyOnly, DirectApplyAndTail ] \m ->
        parseForeignCallMode (Just (show m)) `shouldEqual` Right m
