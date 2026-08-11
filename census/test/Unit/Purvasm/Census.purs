-- | Unit-test aggregator for the `census` tool (the `test:unit` entry point). Each `spec` mirrors
-- | its `src` counterpart — `Test.Unit.Purvasm.Census.<Module>` for `Purvasm.Census.<Module>`.
module Test.Unit.Purvasm.Census where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.Census.Apply as Apply
import Test.Unit.Purvasm.Census.ByNeed as ByNeed

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ByNeed.spec
  Apply.spec
