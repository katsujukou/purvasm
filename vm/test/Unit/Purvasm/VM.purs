-- | Unit-test aggregator for the owned VM (the `test:unit` entry point). Each `spec` mirrors its
-- | `src` counterpart — `Test.Unit.Purvasm.VM.<Module>` for `Purvasm.VM.<Module>`.
module Test.Unit.Purvasm.VM where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.VM.Array as ArraySpec
import Test.Unit.Purvasm.VM.Loader as Loader
import Test.Unit.Purvasm.VM.Machine as Machine
import Test.Unit.Purvasm.VM.Prim as PrimSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  PrimSpec.spec
  ArraySpec.spec
  Machine.spec
  Loader.spec
