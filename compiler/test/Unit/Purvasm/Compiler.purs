-- | Unit-test aggregator (the `test:unit` entry point). Each `spec` lives in a module
-- | mirroring its `src` counterpart — `Test.Unit.Purvasm.Compiler.<Module>` for
-- | `Purvasm.Compiler.<Module>` — and tests that module's type-unenforceable invariants.
module Test.Unit.Purvasm.Compiler where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Purvasm.Compiler.Bytecode.Artifact as Bytecode.Artifact
import Test.Unit.Purvasm.Compiler.Bytecode.Image as Bytecode.Image
import Test.Unit.Purvasm.Compiler.Bytecode.Lower as Bytecode.Lower
import Test.Unit.Purvasm.Compiler.Bytecode.Lower.Match as Bytecode.Lower.Match
import Test.Unit.Purvasm.Compiler.CESK.Translate as CESK.Translate
import Test.Unit.Purvasm.Compiler.Compile as Compile
import Test.Unit.Purvasm.Compiler.Link as Link
import Test.Unit.Purvasm.Compiler.MiddleEnd.Normalize as MiddleEnd.Normalize
import Test.Unit.Purvasm.Compiler.Util.MD5 as Util.MD5

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  MiddleEnd.Normalize.spec
  CESK.Translate.spec
  Bytecode.Lower.spec
  Bytecode.Lower.Match.spec
  Bytecode.Image.spec
  Bytecode.Artifact.spec
  Compile.spec
  Link.spec
  Util.MD5.spec
