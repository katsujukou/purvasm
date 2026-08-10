-- | The ADR-0107 census as an ADR-0087 `Backend`: it is driven by the SAME
-- | `Purvasm.Compiler.build` as a real native build, over the same loaded closure, the same mode
-- | (`--opt`/`--no-opt`) and — through `Driver.moduleGdefs` / `Driver.entryProgram` — the same gdefs
-- | the LLVM backend emits. Reusing the driver is what makes the census reproducible: the module set
-- | is not a directory listing the tool assembles for itself, it is exactly what the compiler
-- | compiles.
module Purvasm.Census.ByNeed.Backend
  ( censusBackend
  ) where

import Prelude

import Purvasm.Census.ByNeed (censusEntry, censusGdefs)
import Purvasm.Census.ByNeed.Report (renderCensus)
import Purvasm.Compiler (Backend)
import Purvasm.Compiler.Backend.LLVM.Driver (entryProgram, moduleGdefs)
import Purvasm.Compiler.Backend.LLVM.Interface (interfaceOfAnf)
import Purvasm.Compiler.Backend.LLVM.Program (classifyDecl)
import Purvasm.Compiler.CESK.Translate (nameKey)

-- | The census backend. It carries no cross-module context — the fact lattice is activation-local by
-- | construction (globals are `May`, ADR-0107 §1), so a module's counts depend on nothing outside it.
-- | The interface is derived exactly as the LLVM backend does, so the driver's `.pmi` phase behaves
-- | identically and cannot perturb the walked modules.
censusBackend :: Boolean -> Backend Unit String
censusBackend isEffect =
  { emptyContext: unit
  , mergeContext: \_ _ -> unit
  , moduleContext: \_ _ -> unit
  , interfaceOf: \_ lm -> interfaceOfAnf lm.source (map classifyDecl lm.module.decls)
  , lowerModule: \_ lm -> renderCensus (nameKey lm.source.name) (censusGdefs (moduleGdefs lm))
  , lowerEntry: \_ input -> renderCensus "<entry>" (censusEntry isEffect (entryProgram input).entry)
  }
