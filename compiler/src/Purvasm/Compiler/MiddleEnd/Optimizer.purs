-- | The optimiser **seam** over a backend-neutral `AnfModule` (ADR-0086 §4, revised by its 2026-07-10
-- | Addendum). The neutral build driver folds this over the closure in dependency order under `--opt`,
-- | threading an in-memory `BuildEnv` that holds **dependencies only** — never the current module's own
-- | facts, so ADR-0084's self-pollution invariant (a module never reads its own summary) is *structural*:
-- |
-- |   * `localFactsOf am` — the module's **stable** facts (its own dictionary machinery + top-level keys),
-- |     computed once and handed to the fixpoint (they do not change across iterations).
-- |   * `optimizeModule env localFacts am` — **one pass** of the real optimiser, `Nbe ∘ DictElim`
-- |     (ADR-0089: `DictElim`'s gate-free dispatch devirtualization, then the NbE general inliner —
-- |     which absorbed `Simplify`, ADR-0089 §6/§8), over the module's bodies, returning
-- |     `{ module, summary }` — the summary carrying the dict machinery **and** the slice-2 inline
-- |     candidates for dependents. Dbe/EffectAnalysis/Specialize are the optimiser track's to add.
-- |     **Iterating this pass to a fixpoint is the build driver's job** (ADR-0087) — the seam stays a
-- |     pure single step the driver can bracket with inspection hooks.
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
  , withForeignSigs
  , persistedSummary
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Bytecode.Artifact (Summary)
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declKeys, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, dictElimExpr, emptyMachinery, intrinsicLift, machineryOf, mergeMachinery)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe (candidatesOf, nbeBinding, nbeEnvOf)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)

-- | The per-build, in-memory optimiser environment threaded through the module fold — carrying the facts
-- | a module's **dependencies** contributed: their dictionary machinery (so an imported
-- | accessor/instance resolves like a local one), their top-level keys (for `DictElim`'s liftable
-- | check), and their **inline candidates** (ADR-0089 slice 2 — post-optimisation bodies the NbE
-- | gate site A may unfold). It never holds the current module's own facts (ADR-0084
-- | self-pollution).
newtype BuildEnv = BuildEnv
  { dict :: DictMachinery
  , gkeys :: Set String
  , inlines :: Map String InlineCandidate
  }

-- | The current module's **stable** facts produced by `localFactsOf` and handed to the `optimizeModule`
-- | fixpoint: its own dictionary machinery and its own top-level keys. Stable = no optimiser pass
-- | mutates them (passes rewrite bodies, never a module's accessor/instance bindings or key set), so
-- | they are computed once per module and reused across every fixpoint round.
type LocalFacts =
  { dict :: DictMachinery
  , gkeys :: Set String
  }

-- | What a compiled module contributes to its dependents' `BuildEnv`. Distinct from the persisted
-- | `Artifact.Summary` (`.pmi`): this is the in-memory build fact; `persistedSummary` projects the subset
-- | that reaches disk. The stable machinery/keys come from `LocalFacts`; the inline candidates are
-- | derived from the **optimised** module (ADR-0086 §3 — a summary derived from the final module
-- | reaches dependents), selected by the `candidatesOf` publish predicate (ADR-0089 §8 slice 2).
newtype BuildSummary = BuildSummary
  { dict :: DictMachinery
  , gkeys :: Set String
  , inlines :: Map String InlineCandidate
  }

-- | The starting env, before any module has contributed.
emptyBuildEnv :: BuildEnv
emptyBuildEnv = BuildEnv { dict: emptyMachinery, gkeys: Set.empty, inlines: Map.empty }

-- | A module's **stable** facts — its own dictionary machinery and top-level keys — computed once from its
-- | `AnfModule` and handed to the `optimizeModule` fixpoint (they do not change across iterations). The
-- | driver computes this under `--opt` only (ADR-0086 Addendum); it is the half formerly folded into the
-- | removed `preOptimizeModule` (minus that phase's `DictElim` rewrite, now the optimiser pass / LLVM bridge).
-- | Takes the dependency env because instance recognition must see through *imported* `$Dict` identity
-- | wrappers (an instance is routinely declared outside its class's module); the env still holds only
-- | dependency facts, so the self-pollution invariant is untouched.
localFactsOf :: BuildEnv -> AnfModule -> LocalFacts
localFactsOf (BuildEnv env) am =
  { dict: machineryOf env.dict (Array.concatMap _.members am.decls)
  , gkeys: Set.fromFoldable (Array.concatMap declKeys am.decls)
  }

-- | One pass of the real optimiser (ADR-0086 §3, the `--opt` leg): `Nbe ∘ DictElim` over the
-- | module's bodies (`DictElim` first — its gate-free atom swap devirtualizes dispatch, ADR-0089 §6;
-- | the NbE inliner then reduces what the collapse exposed — intrinsic saturation, β, folds), under
-- | the module-visible env (`env` deps ∪ `localFacts`), returning the rewritten `AnfModule` **and its
-- | `BuildSummary`** (ADR-0086 pins `{ module, summary }`, so a summary derived from the *optimised*
-- | module reaches dependents). The **driver iterates this to a fixpoint** (ADR-0087), threading
-- | `.module` and keeping the last pass's `.summary`. Sibling-body facts (`nbeEnvOf`) are
-- | pass-internal: recomputed from the DictElim'd decls each driver iteration, never stored
-- | (ADR-0086 §3). The Dbe → EffectAnalysis → Specialize pipeline is the optimiser track's to add
-- | here; cross-module inline candidates arrive with ADR-0089 slice 2.
-- |
-- | `intrinsicLift`: safe on this path only because the NbE intrinsic saturation runs next.
optimizeModule :: BuildEnv -> LocalFacts -> AnfModule -> { module :: AnfModule, summary :: BuildSummary }
optimizeModule (BuildEnv env) lf am =
  let
    full = mergeMachinery lf.dict env.dict
    gkeys = Set.union lf.gkeys env.gkeys
    elimd = map (mapDeclBodies (dictElimExpr intrinsicLift gkeys full)) am.decls
    nbe = nbeEnvOf intrinsicPrim env.inlines elimd
    optimised = elimd <#> \d ->
      d { members = d.members <#> \(Tuple k e) -> Tuple k (nbeBinding nbe k e) }
  in
    { module: am { decls = optimised }
    , summary: BuildSummary
        { dict: lf.dict
        , gkeys: lf.gkeys
        , inlines: candidatesOf intrinsicPrim env.inlines optimised
        }
    }

-- | Fold a just-compiled module's summary into the build env, so its dependents' phases see it.
extendSummary :: BuildEnv -> BuildSummary -> BuildEnv
extendSummary (BuildEnv env) (BuildSummary s) = BuildEnv
  { dict: mergeMachinery s.dict env.dict
  , gkeys: Set.union s.gkeys env.gkeys
  , inlines: Map.union s.inlines env.inlines
  }

-- | Project the in-memory summary onto the `.pmi`'s optional `Summary` (ADR-0084 §5). `Nothing` today —
-- | so the `--no-opt` `.pmi` core is byte-for-byte boot's, and it stays `Nothing` until the optimiser
-- | publishes a per-module-optimised summary.
persistedSummary :: BuildSummary -> Maybe Summary
persistedSummary _ = Nothing
