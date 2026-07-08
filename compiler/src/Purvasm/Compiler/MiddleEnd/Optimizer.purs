-- | The in-memory cross-module optimiser environment and the `(optimize)` seam (ADR-0085 §1/§3/§4).
-- |
-- | As the native build compiles modules in dependency order, it threads a `BuildEnv`: each compiled
-- | module folds its interface in via `applySummary`, and each module's post-`DictElim` bodies are
-- | rewritten by `optimize` under the env its dependencies grew. This module owns only the *skeleton* —
-- | the env threading contract and the seam — that the native build wires; the **optimiser track owns
-- | the contents**: `BuildEnv`'s concrete shape (it decides what a dependent needs, currently an opaque
-- | placeholder), what `applySummary` extracts from an interface's `--opt` summary, and the
-- | Simplify → Dbe → … → NbE pipeline behind `optimize`.
module Purvasm.Compiler.MiddleEnd.Optimizer
  ( BuildEnv
  , emptyBuildEnv
  , applySummary
  , optimize
  ) where

import Prelude

import Purvasm.Compiler.Bytecode.Artifact (Interface)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)

-- | The per-build, in-memory optimiser environment threaded through the module fold. Opaque and empty
-- | for now — the optimiser track replaces this shape (ADR-0085 §4: "currently a newtype over `Json`")
-- | with whatever a dependent's passes actually consume (inline candidates, specialisable callees,
-- | effect/purity facts, foreign shapes, …).
newtype BuildEnv = BuildEnv Unit

-- | The starting env, before any module has contributed.
emptyBuildEnv :: BuildEnv
emptyBuildEnv = BuildEnv unit

-- | Fold a just-compiled module's interface into the build env, so its dependents' `optimize` sees it.
-- | Placeholder: leaves the env unchanged until the optimiser defines what an interface's `summary`
-- | contributes. The interface is computed straight from the module's ANF (`ANF → Pmi`), never via the
-- | `.pmo`.
applySummary :: BuildEnv -> Interface -> BuildEnv
applySummary env _ = env

-- | The `(optimize)` seam: rewrite one post-`DictElim` binding body under the build env. Runs only on the
-- | `--opt` path (the native build gates it). Placeholder identity until the optimiser pipeline lands.
optimize :: BuildEnv -> Expr -> Expr
optimize _ e = e
