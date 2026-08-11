-- | The ADR-0108 apply census as an ADR-0087 `Backend` — built by REPLACING the LLVM backend's two
-- | artifact producers and keeping everything else it has.
-- |
-- | That construction is the anti-drift property: the context, the merge, the per-module
-- | contribution, the interface and the codegen options are `llvmBackend`'s own values, so the
-- | census emits each object under exactly the options the real build emits it under, and reads the
-- | classification events off THAT emission (`Driver.moduleEmission` / `entryEmission` — the same
-- | functions `lowerModule`/`lowerEntry` take their `.ir` from). The instrument cannot classify
-- | differently from the compiler because it does not classify at all.
module Purvasm.Census.Apply.Backend
  ( applyCensusBackend
  ) where

import Prelude

import Purvasm.Census.Apply.Report (renderEvents)
import Purvasm.Compiler (Backend)
import Purvasm.Compiler.Backend.LLVM.Driver (LlvmBackendOptions, LlvmContext, entryEmission, llvmBackend, moduleEmission)
import Purvasm.Compiler.CESK.Translate (nameKey)

-- | The LLVM backend with its two artifacts swapped for the census report of the same emission.
applyCensusBackend :: LlvmBackendOptions -> Backend LlvmContext String
applyCensusBackend opts = (llvmBackend opts)
  { lowerModule = \ctx lm -> renderEvents (nameKey lm.source.name) (moduleEmission ctx lm).events
  , lowerEntry = \ctx input -> renderEvents "<entry>" (entryEmission ctx input).events
  }
