-- | Unit-test aggregator for the `cli` package (the `test:unit` entry point). Each `spec` mirrors its
-- | `src` counterpart — `Test.Unit.Purvasm.CLI.<Module>` for `Purvasm.CLI.<Module>`.
module Test.Unit.Purvasm.CLI where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.CLI.NativeLink as NativeLink

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  NativeLink.spec
