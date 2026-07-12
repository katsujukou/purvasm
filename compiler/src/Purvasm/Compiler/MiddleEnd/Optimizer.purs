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
-- |     candidates for dependents. `EffectAnalysis` (ADR-0095) supplies the pass's effect-fact
-- |     oracle; its dead-drop consumer lives inside the NbE gate (no separate `Dbe` pass).
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
  , publishedForeignSigs
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
import Effect.Console (warn)
import Effect.Unsafe (unsafePerformEffect)
import Purvasm.Compiler.Bytecode.Artifact (Summary)
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declKeys, mapDeclBodies)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery, dictElimExpr, emptyMachinery, intrinsicLift, machineryOf, mergeMachinery)
import Data.Lazy (force)
import Purvasm.Compiler.MiddleEnd.Optimizer.EffectAnalysis (EffectFact, liftShape, moduleEffects, moduleEffectsLazy)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe (candidatesOf, nbeBinding, nbeEnvOf)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (sizeExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (InlineCandidate)
import Purvasm.Compiler.MiddleEnd.Optimizer.Specialize (specializeModule)

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
  -- | Dependencies' exported foreign shapes (ADR-0090), for effect analysis (ADR-0034/0095). The **build
  -- | driver** owns the primary `ForeignFacts` thread (every mode, feeding codegen via
  -- | `LoweredModule.foreignSigs`); it injects the same deps here via `withForeignSigs` only under `--opt`,
  -- | where the optimiser runs. No pass mutates it, so it is not folded by `extendSummary`.
  , foreignSigs :: Map String ForeignShape
  -- | Dependencies' published effect summaries (ADR-0095 §2 / ADR-0096 §1): per-binding
  -- | `EffectFact`s (structurally computed, so they carry a *real* `mtouch`) folded through
  -- | `extendSummary` like `dict`/`inlines`.
  , effects :: Map String EffectFact
  }

-- | The current module's **stable** facts produced by `localFactsOf` and handed to the `optimizeModule`
-- | fixpoint: its own dictionary machinery and its own top-level keys. Stable = no optimiser pass
-- | mutates them (passes rewrite bodies, never a module's accessor/instance bindings or key set), so
-- | they are computed once per module and reused across every fixpoint round.
type LocalFacts =
  { dict :: DictMachinery
  , gkeys :: Set String
  -- | The module's **own** foreign shapes (ADR-0090), injected by the driver under `--opt`; empty from
  -- | `localFactsOf` (source-derived, not `AnfModule`-derived). For the optimiser's own-module effect analysis.
  , foreignSigs :: Map String ForeignShape
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
  -- | The module's own foreign shapes **referenced by the published candidates** (the ADR-0090
  -- | integration): an inlined wrapper body carries its (possibly non-exported) foreign reference
  -- | into the consumer, so the consumer's visible shapes must carry those arity/effect facts too
  -- | — the driver's exports-only `ForeignFacts` publication cannot see them. Read by the driver
  -- | via `publishedForeignSigs` and folded into that thread.
  , foreignSigs :: Map String ForeignShape
  -- | The module's per-binding effect summaries for dependents (ADR-0095 §2). The seam has no
  -- | export visibility, so **every** top-level key is published — the superset of the ADR's
  -- | "exports ∪ candidate-reachable privates" floor (under-publication is the failure mode the
  -- | ADR guards against: a private helper's fact regressing to `unknown` at the very site a
  -- | cross-module inline just created; over-publication only costs in-memory map entries).
  , effects :: Map String EffectFact
  }

-- | The starting env, before any module has contributed.
emptyBuildEnv :: BuildEnv
emptyBuildEnv = BuildEnv { dict: emptyMachinery, gkeys: Set.empty, inlines: Map.empty, foreignSigs: Map.empty, effects: Map.empty }

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
  , foreignSigs: Map.empty -- source-derived; the driver injects the module's own shapes under --opt
  }

-- | One pass of the real optimiser (ADR-0086 §3, the `--opt` leg): `Nbe ∘ DictElim` over the
-- | module's bodies (`DictElim` first — its gate-free atom swap devirtualizes dispatch, ADR-0089 §6;
-- | the NbE inliner then reduces what the collapse exposed — intrinsic saturation, β, folds), under
-- | the module-visible env (`env` deps ∪ `localFacts`), returning the rewritten `AnfModule` **and its
-- | `BuildSummary`** (ADR-0086 pins `{ module, summary }`, so a summary derived from the *optimised*
-- | module reaches dependents). The **driver iterates this to a fixpoint** (ADR-0087), threading
-- | `.module` and keeping the last pass's `.summary`. Sibling-body facts (`nbeEnvOf`) and the
-- | module's own effect summaries (`moduleEffects`, ADR-0095) are pass-internal: recomputed from
-- | the DictElim'd decls each driver iteration, never stored (ADR-0086 §3 / ADR-0095 §2).
-- |
-- | `intrinsicLift`: safe on this path only because the NbE intrinsic saturation runs next.
optimizeModule :: BuildEnv -> LocalFacts -> AnfModule -> { module :: AnfModule, summary :: BuildSummary }
optimizeModule (BuildEnv env) lf am =
  let
    full = mergeMachinery lf.dict env.dict
    gkeys = Set.union lf.gkeys env.gkeys
    elimd = map (mapDeclBodies (dictElimExpr intrinsicLift gkeys full)) am.decls
    nbe = nbeEnvOf intrinsicPrim env.inlines elimd
    -- The ADR-0095 effect-fact oracle. Dependency facts (their published summaries, their foreign
    -- shapes) and the module's own foreign shapes are stable; the module's own **binding**
    -- summaries are pass-local — recomputed here from the post-DictElim term each round (never
    -- stored in `LocalFacts`, never read back from this module's own summary, ADR-0095 §2) — so
    -- precision gained by a previous round's Specialize/NbE arrives with the driver's next round.
    -- Foreign shapes lift **dirty** (`liftShape`, ADR-0096 §1); dependency summaries arrive as
    -- full facts with their structurally computed `mtouch`.
    depEffects k = case Map.lookup k lf.foreignSigs of
      Just s -> Just (liftShape s)
      Nothing -> case Map.lookup k env.effects of
        Just s -> Just s
        Nothing -> liftShape <$> Map.lookup k env.foreignSigs
    -- own summaries stay lazy: only the callees the mark walk actually consults are computed.
    ownEffects = moduleEffectsLazy depEffects elimd
    effectOracle k = case Map.lookup k ownEffects of
      Just s -> Just (force s)
      Nothing -> depEffects k
    -- The round-growth backstop (ADR-0089 self-compile extension) wraps every binding: a round
    -- output that grew past `roundGrowthMax`× the round input keeps the input — term-preserving,
    -- never a mid-flight re-reduction — and **everything below derives from the post-backstop
    -- decls**, so an inflated term neither republishes into this module's next round nor leaks
    -- to dependents. The growth floor keeps legitimate small-binding unfolds (a 5-node CAF
    -- normalising to a 20-node lambda) out of the backstop's reach; the blow-up class it exists
    -- for sits orders of magnitude above it.
    optimised = elimd <#> \d ->
      d { members = d.members <#> \(Tuple k e) -> Tuple k (backstop k e (nbeBinding nbe effectOracle k e)) }
    -- Dictionary specialization (ADR-0093): `Specialize ∘ Nbe ∘ DictElim` per round. Discovery on
    -- the (post-backstop) NbE output; emitted clones and rewritten sites become part of the
    -- module the next round (and the summary below) derive from.
    localInlines = candidatesOf intrinsicPrim env.inlines optimised
    specialized = specializeModule am.name lf.gkeys (Map.union localInlines env.inlines) optimised
  in
    { module: am { decls = specialized }
    , summary:
        let
          inlines = candidatesOf intrinsicPrim env.inlines specialized
          -- own foreign shapes the candidate bodies reference — on either atom spelling (a foreign
          -- key rides `AtomForeign` or a plain qualified `AtomVar`); see `BuildSummary.foreignSigs`.
          referenced = Array.foldl
            (\acc c -> Set.union acc (Set.union (fvExpr Set.empty c.body) (cfExpr c.body)))
            Set.empty
            (Array.fromFoldable (Map.values inlines))
        in
          BuildSummary
            { dict: lf.dict
            , gkeys: lf.gkeys
            , inlines
            , foreignSigs: Map.filterKeys (\k -> Set.member k referenced) lf.foreignSigs
            -- ADR-0086 §3: a summary derived from the *final* module reaches dependents — the
            -- published effect facts are recomputed from the post-Specialize decls, not reused
            -- from the pre-NbE `ownEffects` this pass consumed.
            , effects: moduleEffects depEffects specialized
            }
    }

-- | The per-round growth cap (ADR-0089 self-compile extension): raising it needs a measured
-- | justification (the measured blow-up was ×12.6 module-wide, ×167 per binding, in one round).
roundGrowthMax :: Int
roundGrowthMax = 4

-- | Below this output size the backstop never fires: a small binding legitimately quadruples
-- | when a ≤64-tier body unfolds into it. Implementation refinement over the accepted sketch
-- | (which pinned only the ×4), recorded in the extension's Progress note.
growthFloor :: Int
growthFloor = 256

-- | Keep the round input when a binding's round output grew past `roundGrowthMax`× (above the
-- | floor). The warning goes through `unsafePerformEffect`: the seam step is pinned pure
-- | (ADR-0086), and a fired backstop is a diagnosis-worthy event that must not vanish into a
-- | silently smaller term.
backstop :: String -> Expr -> Expr -> Expr
backstop key input output =
  let
    inSize = sizeExpr input
    outSize = sizeExpr output
  in
    if outSize > growthFloor && outSize > roundGrowthMax * inSize then
      unsafePerformEffect do
        warn
          ( "purvasm: optimizer round-growth backstop fired at " <> key
              <> " (input "
              <> show inSize
              <> " -> output "
              <> show outSize
              <> " nodes); keeping the round input"
          )
        pure input
    else output

-- | Fold a just-compiled module's summary into the build env, so its dependents' phases see it.
extendSummary :: BuildEnv -> BuildSummary -> BuildEnv
extendSummary (BuildEnv env) (BuildSummary s) = BuildEnv
  { dict: mergeMachinery s.dict env.dict
  , gkeys: Set.union s.gkeys env.gkeys
  , inlines: Map.union s.inlines env.inlines
  -- Foreign shapes are threaded by the driver's `ForeignFacts` (re-injected via `withForeignSigs` each
  -- `--opt` step, ADR-0090), not folded through the summary; preserve whatever is set.
  , foreignSigs: env.foreignSigs
  , effects: Map.union s.effects env.effects
  }

-- | Inject the driver's dependency foreign shapes into the env for `optimizeModule` (ADR-0090). Called by
-- | the build driver **under `--opt` only**, from its `ForeignFacts` thread — the env is otherwise
-- | foreign-empty (the primary thread is driver-level, feeding codegen in every mode).
withForeignSigs :: Map String ForeignShape -> BuildEnv -> BuildEnv
withForeignSigs sigs (BuildEnv env) = BuildEnv (env { foreignSigs = sigs })

-- | The candidate-referenced foreign shapes a summary publishes (see `BuildSummary.foreignSigs`) —
-- | the driver folds these into its `ForeignFacts` thread alongside the exported shapes, so a
-- | consumer that inlined the candidate still sees the referenced foreign's arity/effect facts.
publishedForeignSigs :: BuildSummary -> Map String ForeignShape
publishedForeignSigs (BuildSummary s) = s.foreignSigs

-- | Project the in-memory summary onto the `.pmi`'s optional `Summary` (ADR-0084 §5). `Nothing` today —
-- | so the `--no-opt` `.pmi` core is byte-for-byte boot's, and it stays `Nothing` until the optimiser
-- | publishes a per-module-optimised summary.
persistedSummary :: BuildSummary -> Maybe Summary
persistedSummary _ = Nothing
