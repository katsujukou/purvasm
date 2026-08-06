# 0107. Provably-not-by-need operands: elide the force, de-safepoint the site

- Status: ~~Proposed~~ **Accepted** _(2026-08-06: explicit maintainer Accept after 1 review round)_
- Date: 2026-08-06
- Deciders: maintainer
- Technical story: ADR-0105 Results follow-up map (the "not-by-need" lever); sized by the
  2026-08-06 force-only-crossing census

## Context

Every demand site (`CPrim` operands, `CIf`/guard conditions, `CCase` scrutinees,
`CAccessor`/`CUpdate` bases) forces its operand through the 3-block
`fchk`/`fslow`/`fdone` chain, because any non-immediate variable MAY hold a `ByNeed` cell
(ADR-0070/0079). The force's slow path runs a thunk, so the site is also a SAFEPOINT —
which roots values across it and, per ADR-0105's model, contributes crossings. But most
forced operands provably never hold a cell: a value born from a literal, a saturated
constructor, a scalar primitive, or a closure build is a plain value for its whole life.

**Sizing (2026-08-06 census — the maintainer-required acceptance gate, taken BEFORE this
ADR was drafted).** Method: a repo-untouched counterfactual compiler (a scratch copy of the
compiled output with the seam's `RtForceIfByneed` row flipped to `sp = false`) emitted the
fixed self-host closure; exact 302-module pairing against the current emission. Numbers:

- **Crossing side** (the upper bound with ALL forces de-safepointed): root blocks
  45,527 → 43,684 (**−1,843, −4.0 %**), reloads −3.3 %, IR lines **−2.1 %**, frames
  elided 23.6 % → 24.0 % (+45 `$d` functions).
- **Forced-phi root class: 4,409 → 4,361 (−1.1 % only)** — the class mostly
  RECLASSIFIES (a forced value still crosses OTHER safepoints); it does not vanish.
- **The force chains themselves — which only THIS ADR can delete (the counterfactual
  keeps them): 8,768 chains = 100,870 lines = 4.9 % of the final IR.**
- **Provenance of forced operands** (`.ll`-level proxy): slot-reload 49.7 % / param
  25.5 % / call-result 14.7 % / field-record 5.9 % / settled 2.7 % / capture 1.2 %.
  Trivially-Never at USE sites ≈ 0 % — `forceAtom` only ever forces VARIABLES, so
  Never-provability is entirely a BINDING-provenance question. NOTE (review round 1):
  the ≈ 64 % "local-binding share" (slot-reload + call-result) is a `.ll`-proxy UPPER
  BOUND on where binding provenance is visible at all, NOT what this lattice can prove —
  `CApp`/call results are pinned `May`, so the lattice's own ceiling is strictly below
  it; the strict-RHS-bound fraction is unknown until slice 0 measures it at the ANF
  level.

**Total IR upper bound ≈ 7 %** (2.1 crossing + 4.9 chains) × the realizable Never
fraction. Strategic frame (ADR-0106's post-close A/B): clang WALL is flat under further
rooting reduction — this lever's justification is **memory/`.ll` plus RUN TIME** (a
branch + tag-check per force EXECUTION on every demand-site hot path, plus the extra
frame elision), which no remaining rooting-only lever offers.

## Decision

### 1. The fact lattice — two values, unknown is ALWAYS `May`

```
data ByNeedFact = NeverByNeed | MayBeByNeed
```

A misclassification is a SEMANTIC bug (reading a cell as a value), so the lattice is
pinned conservative:

- **`NeverByNeed` producers:** scalar/boxed literals; saturated `CCtor`/`CArray`/
  `CRecord` results; scalar-primitive results (`CPrim` arithmetic/comparison — NOT the
  projections, below); closure builds (`CLam`, the unsaturated-ctor builder); a value
  that is itself the RESULT of a force (`fdone` phi).
- **`MayBeByNeed`, pinned, no exceptions:** params, captures, `%env`-derived reads,
  globals (a `Gcaf` can alias a `Grec` member's CELL, and cross-module provenance needs
  `.pmi` facts — deferred), foreign results, `CApp`/`pv_apply`/direct-call results (a
  function may return an unforced cell), field/element/record projections
  (`CAccessor`/`RecordGet`/`IndexArray`/`ReadField` — containers store cells: by-need
  dict members), and ANYTHING not positively matched.
- **Propagation:** an alias inherits its source's fact; a branch/`case` result is the
  MEET (`Never` iff every arm is `Never`); `LetRec` members are `May` (they ARE cells).
- **The force-result producer carries a RUNTIME contract (review round 1):** "the result
  of a force is `Never`" holds only if `pv_force_if_byneed` collapses cell CHAINS to a
  non-cell (a cell whose stored value is another cell must be forced through). This is
  pinned by a provider-side runtime fixture (a chained-cell force under `gc_stress`,
  slice-1 verification); if the runtime does not iterate, the fixture fails and this
  producer is REMOVED from the `Never` list — the decision is recorded either way.

**Demand recipes in scope (pinned):** `CPrim` operands, the `CIf` condition, guard
results, `CCase` scrutinees, and the `CAccessor`/`CUpdate` bases — exactly the
`forceAtom`/`forceValue` sites the recipes emit. OUT of scope: `SForceCell` (it forces
a `Grec` member's CELL on purpose — the call path's semantics) and the entry stub's
force (one site, module-skeleton tier); both keep their forces unconditionally.

### 2. Fact identity, one decision set, two consumers

**Binding identity is an OCCURRENCE, never a source name (pinned).** The ANF carries
source binder strings with no no-shadowing contract, so a flat name-keyed map can conflate
an outer `Never` with an inner `May`, or same-named binders across branch arms —
unsound. The fact computation therefore keys on identity, in one of the two pinned forms
(implementation chooses, the property is normative):

- an opaque `BindingId` minted per binder OCCURRENCE while walking lexical scope
  (params, captures, `%env`, `Let`, `LetRec` members, and case-arm binders each mint;
  an inner binder of the same name is a DIFFERENT id; lookup follows scope, innermost
  first), or
- resolution to structural **`DemandSiteId`s**: the walk resolves each demand SITE's
  operand against the scope in place and records an opaque `ForceDecision` per site —
  no name survives into the decision set at all.

**The decision set is computed ONCE per activation and is the ONLY thing either consumer
reads**: the backward liveness pass and the forward emitter both take the same
`ForceDecision`s (threaded with the `ActivationPlan`), so they cannot re-derive — and
therefore cannot disagree about — a site's classification.

- **Liveness:** a site decided `ElideForce` stops contributing the `RtForceIfByneed`
  safepoint (its operand-materialisation classification is unchanged);
- **Emit:** the same decision skips `Abi.forceValue` at that site (the operand flows
  as-is; forcing a non-cell was a no-op, so eliding a PROVEN non-cell is
  behaviour-preserving).

**What actually carries the safety (pinned, corrected in review round 1).** The
soundness rests on exactly three legs: (a) the TOTAL, conservative classifier (unknown
falls to `May` over every producer, mechanically); (b) both consumers sharing the ONE
opaque decision set (no second derivation exists); (c) the `May`-totality gate fixture
(§4). The ADR-0105 token/epoch net is explicitly NOT a safety net for a wrong `Never`:
if the shared decision is wrong, force AND bump disappear TOGETHER, and a real cell flows
epoch-valid and unforced — silently; and the opposite drift (plan `May`, emitter
`Never`) merely over-roots without crashing. The net remains only an AUXILIARY backstop
for the narrower class where the plan alone misses an ACTUAL emitted safepoint.

### 3. Slices

- **Slice 0 — the ANF-level binding-RHS census (measurement only, no emission change).**
  Measurement conditions, pinned: the SAME fixed CoreFn closure as the 2026-08-06 census;
  `--no-opt` is the primary corpus (the standing census baseline) with a secondary
  `--opt` run reported alongside (the CLI default path — ratios may differ); a "forced
  site" is a demand-site recipe occurrence per §"demand recipes" below, INCLUDING guard
  results, aggregated by the same structural site identity the decision set uses (one
  site = one count, whatever its dynamic execution count). Report: the `Never` share of
  forced sites (the realizable fraction of the 8,768 chains), split by recipe, plus the
  `Never`-attributable crossing share. **This number decides whether slices 1+ proceed**
  (a ≈ 3 %-class result may re-prioritise apply-count first — maintainer's call at the
  checkpoint, read together with the `pv_apply` re-profile).
- **Slice 1 — the lattice + elision.** Fact computation, the fact-aware shared
  classifier, force elision at `Never` sites, plan/emission threading. Intentional
  divergence: re-baselines, census accounting (**chains deleted must equal
  `Never`-proven forced sites — attributable, per the 0105/0106 discipline**), full
  battery (unit incl. lattice totality/meet/alias laws, behavioural gate + stress +
  debug-ABI, fixpoint, bench).
- **Cross-module facts via `.pmi` are NOT part of this ADR's acceptance.**
  `globals = May` is the SETTLED contract of ADR-0107; lifting it is an OWED follow-up
  requiring its OWN ADR (or an explicitly re-accepted amendment) — its contract surface
  is known and non-trivial: the pre/post-optimisation phase a published fact describes,
  the mode key (`--opt`/`--no-opt`), module-own contribution vs import-closure
  projection, the compiler/schema/ABI/options key, and the dependency fingerprint
  invalidation cascade. Accepting 0107 authorises NOTHING of that.

### 4. Verification (beyond the standard battery)

- A NEW gate fixture in which a REAL `ByNeed` cell (a `Grec` member) flows through
  aliases, branch meets, and container round-trips into every in-scope demand recipe —
  every path must keep its force (`May` totality is load-bearing, and the fixture's
  full-content readback catches a wrongly-elided force as wrong output, not luck). The
  MEET arm is pinned concrete: an optimisation-opaque selector (e.g. a value read through
  a `Ref`, invisible to constant folding) chooses between a `Never` arm and the cell arm
  — `Never ⊓ May = May` — and the fixture SELECTS the `May` arm at run time, so an
  elided-meet bug executes the unforced cell path and corrupts the readback;
- gc-stress legs unchanged (elision only removes provably-no-op work, but the stress run
  exercises the de-safepointed windows);
- unit: lattice laws (unknown → `May` totality over every RHS constructor — a new ANF
  node must FALL to `May`, mechanically), alias/meet propagation incl. shadowing
  (outer-`Never`/inner-`May` same-named binders resolve by occurrence, per §2),
  per-recipe elision fixtures (forced `Never` operand emits no `fchk`; forced `May`
  operand unchanged).

#### Progress (2026-08-06): slice 0 — the census, and its verdict

The fact walk landed as `Backend.LLVM.ByNeed` (analysis-only; occurrence-keyed scope per
§2, the pinned two-valued lattice, catch-all-to-`May`) and ran over the fixed closure
(298/301 modules decoded; `--no-opt`; the `--opt` secondary is BLOCKED by the standing
`mod_282` optimiser stall — waiver precedent, reported as such). Sites: 8,364 (vs 8,768
emitted chains — the gap is the out-of-scope `SForceCell`/entry forces and non-var guard
operands). **The `Never` share is 0.2 % (16 sites)**: CCase scrutinees (3,833) and
`CAccessor` bases (3,821) dominate at 91 % and are ≈ 100 % `May` — they force exactly
the values that arrive THROUGH abstraction boundaries (params, dict globals, projections,
call results), and the dict CAFs really ARE by-need cells (ADR-0070's mutually-recursive
instance dicts), so even the deferred cross-module facts would not flip the accessor
class. `CPrim` operands reach only 3.3 % `Never`. **Gate verdict: far below the
≈ 3 %-class threshold — slices 1+ should NOT proceed on this corpus; the lever is
structurally low-yield because laziness is load-bearing at precisely the forced
boundaries. Recorded for the maintainer's checkpoint call; the apply-count track
(2.67 B dispatches, 95.8 % exact-fast) is the road.**

## Consequences

- The force chain's 4.9 % IR mass and its run-time branch+tag-check become removable in
  proportion to slice 0's measured Never share; frame elision rises past 24 %.
- The lattice is the first BINDING-PROVENANCE fact set in the backend — the
  mutation/ownership research track ([[mutation-ownership-research-track]]) and future
  `.pmi` fact publication build on the same shape.
- The `May` pins keep `Grec`/dict laziness (ADR-0070) fully intact.

## Alternatives considered

- **A richer lattice (`KnownCell`/3-valued).** Rejected for now: no consumer — elision
  only needs `Never`, and `KnownCell` (force-always) saves just the fast-path branch.
- **Type/representation-driven proof.** Rejected as in ADR-0105: the backend does not
  track representation types; the CoreFn-typed future (the PS-typechecker goal) can
  revisit.
- **De-safepointing WITHOUT eliding the chain.** Rejected: the census shows the crossing
  side alone is −2.1 % lines while the chains are 4.9 % — and the run-time win lives in
  the chain, not the classification.
- **Runtime-side speculation (branch-predict the non-cell path).** Out of scope: the
  branch is already the fast path; only removal helps the IR mass.
