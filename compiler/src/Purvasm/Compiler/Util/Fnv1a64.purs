-- | The compiler's name for [Purvasm.Abi.Fnv1a64], which is where the implementation lives: a
-- | constructor tag is minted by the LLVM backend AND by the owned VM's FFI boundary (ADR-0111 §3),
-- | so the derivation belongs below both. Re-exported here so the compiler's own consumers — and the
-- | goldens that pin these encodings — keep referring to it by the name they always used.
module Purvasm.Compiler.Util.Fnv1a64
  ( module Purvasm.Abi.Fnv1a64
  ) where

import Purvasm.Abi.Fnv1a64 (I64, fnv1a64Bytes, mul64, unsignedCompareI64)
