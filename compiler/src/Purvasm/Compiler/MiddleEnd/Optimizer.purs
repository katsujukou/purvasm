-- | The optimiser/codegen **seam** over a backend-neutral `AnfModule` (ADR-0086 §4). The build driver
-- | folds these two module-level phases over the closure in dependency order, threading an in-memory
-- | `BuildEnv` that holds **dependencies only** — never the current module's own facts, so ADR-0084's
-- | self-pollution invariant (a module never reads its own summary) is *structural*, not a discipline:
-- |
-- |   * `preOptimizeModule env am` — the byte-identity-first phase. It runs `DictElim` as a **boot-parity
-- |     bridge** (ADR-0086 §3) so the `--no-opt` leg stays byte-identical to boot, and returns the
-- |     module's stable dictionary machinery as `LocalFacts` (recomputed here, not read from the env).
-- |   * `optimizeModule env localFacts am` — the correctness-first phase (the `--opt` leg): **one pass** of
-- |     `DictElim ∘ Simplify ∘ Specialize ∘ Inlining ∘ …` over the module's bodies, returning
-- |     `{ module, summary }` (ADR-0086's pinned shape). Only `DictElim` is wired today; the remaining
-- |     passes are the optimiser track's to fill behind this signature. **Iterating this pass to a fixpoint
-- |     is the build driver's job** (ADR-0087) — the seam stays a pure single step so the driver can bracket
-- |     each iteration with inspection hooks and keep the last pass's `summary`.
-- |
-- | The driver grows the env for later modules with `extendSummary (summaryOfLocal localFacts)`; the
-- | contributed summary is the module's stable machinery, the same whether or not `optimizeModule` ran (so
-- | under `--no-opt` the driver simply skips the pass). `persistedSummary` projects the in-memory
-- | `BuildSummary` onto the `.pmi`'s optional `Summary` (ADR-0084 §5) — `Nothing` today, so the `--no-opt`
-- | `.pmi` core stays byte-for-byte boot's, and `Nothing` until the optimiser publishes a
-- | per-module-optimised summary.
-- |
-- | This module owns the seam's **shape and env threading**; the optimiser track owns the **contents** (the
-- | single-pass Simplify → Dbe → … → NbE pipeline inside `optimizeModule`, and `BuildSummary`'s real
-- | payload); the driver owns the **iteration** (ADR-0087).
module Purvasm.Compiler.MiddleEnd.Optimizer
  ( BuildEnv
  , LocalFacts
  , BuildSummary
  , emptyBuildEnv
  , preOptimizeModule
  , optimizeModule
  , preOptimizeEntry
  , summaryOfLocal
  , extendSummary
  , persistedSummary
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Purvasm.Compiler.Bytecode.Artifact (Summary)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
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

-- | The top-level keys and own dictionary machinery a module defines, from its `AnfModule`.
localFactsOf :: AnfModule -> LocalFacts
localFactsOf am =
  { dict: machineryOf (Array.concatMap _.members am.decls)
  , gkeys: Set.fromFoldable (Array.concatMap declKeys am.decls)
  }

-- | The byte-identity-first phase (ADR-0086 §3): run `DictElim` over the module's bodies — the boot-parity
-- | bridge that keeps the `--no-opt` leg byte-identical — resolving imported accessors/instances from
-- | `env`'s machinery just like local ones, under the module-visible key set (`env.gkeys ∪ own`). Returns
-- | the rewritten module plus its stable `LocalFacts` (recomputed here, never read from the env).
preOptimizeModule :: BuildEnv -> AnfModule -> { module :: AnfModule, localFacts :: LocalFacts }
preOptimizeModule (BuildEnv env) am =
  let
    lf = localFactsOf am
    full = mergeMachinery lf.dict env.dict
    gkeys = Set.union lf.gkeys env.gkeys
  in
    { module: am { decls = map (mapDeclBodies (dictElimExpr gkeys full)) am.decls }
    , localFacts: lf
    }

-- | The correctness-first phase (ADR-0086 §3, the `--opt` leg): **one pass** of `DictElim ∘ Simplify ∘ …`
-- | over the module's bodies, under the same module-visible env the byte-identity phase used, returning the
-- | rewritten `AnfModule` **and its `BuildSummary`** (ADR-0086 pins `{ module, summary }`, so a summary
-- | derived from the *optimised* module — cross-module inline/spec facts — reaches dependents). The
-- | **driver iterates this pass to a fixpoint** (ADR-0087), threading `.module` and keeping the last pass's
-- | `.summary` — the seam stays a pure single step it can inspect per iteration. Only `DictElim` is wired
-- | today (idempotent over an already-elided body, so re-running it after `preOptimizeModule`, and across
-- | iterations, is a no-op — the `--opt` leg stays behaviourally equal to `--no-opt` until real passes
-- | land, and the summary is `summaryOfLocal` until the optimiser publishes one); the
-- | Simplify → Dbe → … → NbE pipeline is the optimiser track's to add here.
optimizeModule :: BuildEnv -> LocalFacts -> AnfModule -> { module :: AnfModule, summary :: BuildSummary }
optimizeModule (BuildEnv env) lf am =
  let
    full = mergeMachinery lf.dict env.dict
    gkeys = Set.union lf.gkeys env.gkeys
  in
    { module: am { decls = map (mapDeclBodies (dictElimExpr gkeys full)) am.decls }
    , summary: summaryOfLocal lf
    }

-- | Run the byte-identity `DictElim` bridge over the program **entry** expression (`Main.main` applied to
-- | unit) under the fully-accumulated env — the whole program's machinery and top-level keys. The entry is
-- | not an `AnfModule` (it has no bindings of its own), so it takes the final `BuildEnv` after the fold.
preOptimizeEntry :: BuildEnv -> Expr -> Expr
preOptimizeEntry (BuildEnv env) = dictElimExpr env.gkeys env.dict

-- | The `--no-opt` summary: with no optimiser passes run, a module contributes only its stable machinery.
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
