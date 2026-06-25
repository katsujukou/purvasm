-- | Unit-test aggregator (the `test:unit` entry point). Each `spec` lives in a module
-- | mirroring its `src` counterpart — `Test.Unit.Purvasm.Compiler.<Module>` for
-- | `Purvasm.Compiler.<Module>` — and tests that module's type-unenforceable invariants.
module Test.Unit.Purvasm.Compiler where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.Compiler.CESK.Translate as CESK.Translate
import Test.Unit.Purvasm.Compiler.MiddleEnd.Normalize as MiddleEnd.Normalize

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  MiddleEnd.Normalize.spec
  CESK.Translate.spec
