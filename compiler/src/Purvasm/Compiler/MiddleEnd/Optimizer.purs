-- | The optimiser **seam** over a backend-neutral `AnfModule` (ADR-0086 §4, revised by its 2026-07-10
-- | Addendum). The neutral build driver folds this over the closure in dependency order under `--opt`,
-- | threading an in-memory `BuildEnv` that holds **dependencies only** — never the current module's own
-- | facts, so ADR-0084's self-pollution invariant (a module never reads its own summary) is *structural*:
-- |
-- |   * `localFactsOf am` — the module's **stable** facts (its own dictionary machinery + top-level keys),
-- |     computed once and handed to the fixpoint (they do not change across iterations).
-- |   * `optimizeModule env localFacts am` — **one pass** of the real optimiser
-- |     `DictElim ∘ Simplify ∘ Specialize ∘ Inlining ∘ …` over the module's bodies, returning
-- |     `{ module, summary }`. Only `DictElim` is wired today; the remaining passes are the optimiser
-- |     track's. **Iterating this pass to a fixpoint is the build driver's job** (ADR-0087) — the seam stays
-- |     a pure single step the driver can bracket with inspection hooks.
-- |
-- | This is the `--opt` path only. Under `--no-opt` the driver is the identity (ADR-0086 Addendum): it runs
-- | **no** `DictElim` here. `DictElim` is now purely an optimiser pass; the native backend's `--no-opt`
-- | boot-parity requirement is a *backend-private* bridge in `llvmBackend`, not a seam phase — so the VM's
-- | `--no-opt` keeps dictionaries applied (the optimiser's effect stays observable).
-- |
-- | The driver grows the env for dependents with `extendSummary`; `persistedSummary` projects the in-memory
-- | `BuildSummary` onto the `.pmi`'s optional `Summary` (ADR-0084 §5) — `Nothing` today, so the `.pmi` core
-- | stays byte-for-byte boot's until the optimiser publishes a per-module-optimised summary.
module Purvasm.Compiler.MiddleEnd.Optimizer
  ( BuildEnv
  , LocalFacts
  , BuildSummary
  , emptyBuildEnv
  , localFactsOf
  , optimizeModule
  , extendSummary
  , persistedSummary
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Purvasm.Compiler.Bytecode.Artifact (Summary)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declKeys, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, dictElimExpr, emptyMachinery, machineryOf, mergeMachinery)

-- | The per-build, in-memory optimiser environment threaded through the module fold — carrying the facts
-- | a module's **dependencies** contributed (their dictionary machinery, so an imported accessor/instance
-- | resolves like a local one; and their top-level keys, for `DictElim`'s liftable check). It never holds
-- | the current module's own facts (ADR-0084 self-pollution). The optimiser track extends this record
-- | with whatever a dependent's passes consume (inline candidates, specialisable callees, purity facts, …).
newtype BuildEnv = BuildEnv
  { dict :: DictMachinery
  , gkeys :: Set String
  }

-- | The current module's **stable** facts produced by `preOptimizeModule` and handed to `optimizeModule`:
-- | its own dictionary machinery and its own top-level keys. Stable = independent of which optimiser
-- | passes run, so it is safe to compute once in the byte-identity phase and reuse in the `--opt` phase.
type LocalFacts =
  { dict :: DictMachinery
  , gkeys :: Set String
  }

-- | What a compiled module contributes to its dependents' `BuildEnv`. Distinct from the persisted
-- | `Artifact.Summary` (`.pmi`): this is the in-memory build fact; `persistedSummary` projects the subset
-- | that reaches disk. Its real payload is the optimiser track's to define; today it mirrors `LocalFacts`.
newtype BuildSummary = BuildSummary
  { dict :: DictMachinery
  , gkeys :: Set String
  }

-- | The starting env, before any module has contributed.
emptyBuildEnv :: BuildEnv
emptyBuildEnv = BuildEnv { dict: emptyMachinery, gkeys: Set.empty }

-- | A module's **stable** facts — its own dictionary machinery and top-level keys — computed once from its
-- | `AnfModule` and handed to the `optimizeModule` fixpoint (they do not change across iterations). The
-- | driver computes this under `--opt` only (ADR-0086 Addendum); it is the half formerly folded into the
-- | removed `preOptimizeModule` (minus that phase's `DictElim` rewrite, now the optimiser pass / LLVM bridge).
localFactsOf :: AnfModule -> LocalFacts
localFactsOf am =
  { dict: machineryOf (Array.concatMap _.members am.decls)
  , gkeys: Set.fromFoldable (Array.concatMap declKeys am.decls)
  }

-- | One pass of the real optimiser (ADR-0086 §3, the `--opt` leg): `DictElim ∘ Simplify ∘ …` over the
-- | module's bodies, under the module-visible env (`env` deps ∪ `localFacts`), returning the rewritten
-- | `AnfModule` **and its `BuildSummary`** (ADR-0086 pins `{ module, summary }`, so a summary derived from
-- | the *optimised* module reaches dependents). The **driver iterates this to a fixpoint** (ADR-0087),
-- | threading `.module` and keeping the last pass's `.summary`. Only `DictElim` is wired today (idempotent,
-- | so a module with no static dispatch converges in one round); the Simplify → Dbe → … → NbE pipeline is
-- | the optimiser track's to add here.
optimizeModule :: BuildEnv -> LocalFacts -> AnfModule -> { module :: AnfModule, summary :: BuildSummary }
optimizeModule (BuildEnv env) lf am =
  let
    full = mergeMachinery lf.dict env.dict
    gkeys = Set.union lf.gkeys env.gkeys
  in
    { module: am { decls = map (mapDeclBodies (dictElimExpr gkeys full)) am.decls }
    , summary: summaryOfLocal lf
    }

-- | The summary a module contributes to dependents: its stable machinery. An **internal helper** of
-- | `optimizeModule` (which returns it) — not a driver-facing seam function (ADR-0086 Addendum): the
-- | neutral driver produces a summary only via `optimizeModule` under `--opt`; `--no-opt` produces none.
summaryOfLocal :: LocalFacts -> BuildSummary
summaryOfLocal lf = BuildSummary { dict: lf.dict, gkeys: lf.gkeys }

-- | Fold a just-compiled module's summary into the build env, so its dependents' phases see it.
extendSummary :: BuildEnv -> BuildSummary -> BuildEnv
extendSummary (BuildEnv env) (BuildSummary s) = BuildEnv
  { dict: mergeMachinery s.dict env.dict
  , gkeys: Set.union s.gkeys env.gkeys
  }

-- | Project the in-memory summary onto the `.pmi`'s optional `Summary` (ADR-0084 §5). `Nothing` today —
-- | so the `--no-opt` `.pmi` core is byte-for-byte boot's, and it stays `Nothing` until the optimiser
-- | publishes a per-module-optimised summary.
persistedSummary :: BuildSummary -> Maybe Summary
persistedSummary _ = Nothing
