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
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Abi as Backend.LLVM.Abi
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Mangle as Backend.LLVM.Mangle
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Monad as Backend.LLVM.Monad
import Test.Unit.Purvasm.Compiler.CESK.Translate as CESK.Translate
import Test.Unit.Purvasm.Compiler.Compile as Compile
import Test.Unit.Purvasm.Compiler.Ffi as Ffi
import Test.Unit.Purvasm.Compiler.ForeignSig as ForeignSig
import Test.Unit.Purvasm.Compiler.Link as Link
import Test.Unit.Purvasm.Compiler.MiddleEnd.MatchCompile as MiddleEnd.MatchCompile
import Test.Unit.Purvasm.Compiler.MiddleEnd.Normalize as MiddleEnd.Normalize
import Test.Unit.Purvasm.Compiler.Util.Int64Decimal as Util.Int64Decimal
import Test.Unit.Purvasm.Compiler.Util.MD5 as Util.MD5

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  MiddleEnd.Normalize.spec
  MiddleEnd.MatchCompile.spec
  CESK.Translate.spec
  Bytecode.Lower.spec
  Bytecode.Lower.Match.spec
  Backend.LLVM.Mangle.spec
  Backend.LLVM.Monad.spec
  Backend.LLVM.Abi.spec
  Bytecode.Image.spec
  Bytecode.Artifact.spec
  Compile.spec
  Ffi.spec
  ForeignSig.spec
  Link.spec
  Util.MD5.spec
  Util.Int64Decimal.spec
