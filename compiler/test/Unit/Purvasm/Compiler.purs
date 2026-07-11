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
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Driver as Backend.LLVM.Driver
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Emit as Backend.LLVM.Emit
import Test.Unit.Purvasm.Compiler.MiddleEnd.ANF.FreeVars as MiddleEnd.ANF.FreeVars
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Interface as Backend.LLVM.Interface
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Mangle as Backend.LLVM.Mangle
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Monad as Backend.LLVM.Monad
import Test.Unit.Purvasm.Compiler.Backend.LLVM.Prim as Backend.LLVM.Prim
import Test.Unit.Purvasm.Compiler.Backend.Bytecode as Backend.Bytecode
import Test.Unit.Purvasm.Compiler.Build as Build
import Test.Unit.Purvasm.Compiler.CESK.Translate as CESK.Translate
import Test.Unit.Purvasm.Compiler.Compile as Compile
import Test.Unit.Purvasm.Compiler.Ffi as Ffi
import Test.Unit.Purvasm.Compiler.ForeignSig as ForeignSig
import Test.Unit.Purvasm.Compiler.Link as Link
import Test.Unit.Purvasm.Compiler.MiddleEnd.ANF.Pretty as MiddleEnd.ANF.Pretty
import Test.Unit.Purvasm.Compiler.MiddleEnd.MatchCompile as MiddleEnd.MatchCompile
import Test.Unit.Purvasm.Compiler.MiddleEnd.Normalize as MiddleEnd.Normalize
import Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer as MiddleEnd.Optimizer
import Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.DictElim as MiddleEnd.Optimizer.DictElim
import Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Nbe as MiddleEnd.Optimizer.Nbe
import Test.Unit.Purvasm.Compiler.MiddleEnd.Optimizer.Specialize as MiddleEnd.Optimizer.Specialize
import Test.Unit.Purvasm.Compiler.Util.Fnv1a64 as Util.Fnv1a64
import Test.Unit.Purvasm.Compiler.Util.Int64Decimal as Util.Int64Decimal
import Test.Unit.Purvasm.Compiler.Util.MD5 as Util.MD5

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  MiddleEnd.Normalize.spec
  MiddleEnd.MatchCompile.spec
  MiddleEnd.Optimizer.DictElim.spec
  MiddleEnd.Optimizer.Nbe.spec
  MiddleEnd.Optimizer.Specialize.spec
  MiddleEnd.Optimizer.spec
  MiddleEnd.ANF.Pretty.spec
  CESK.Translate.spec
  Bytecode.Lower.spec
  Bytecode.Lower.Match.spec
  Backend.LLVM.Mangle.spec
  Backend.LLVM.Monad.spec
  Backend.LLVM.Abi.spec
  Backend.LLVM.Prim.spec
  MiddleEnd.ANF.FreeVars.spec
  Backend.LLVM.Interface.spec
  Backend.LLVM.Emit.spec
  Backend.LLVM.Driver.spec
  Backend.Bytecode.spec
  Bytecode.Image.spec
  Bytecode.Artifact.spec
  Compile.spec
  Build.spec
  Ffi.spec
  ForeignSig.spec
  Link.spec
  Util.MD5.spec
  Util.Int64Decimal.spec
  Util.Fnv1a64.spec
