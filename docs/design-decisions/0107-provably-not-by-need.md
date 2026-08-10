# 0107. Provably-not-by-need operands: elide the force, de-safepoint the site

- Status: ~~Proposed~~ **Accepted** _(2026-08-06: explicit maintainer Accept after 1 review
  round)_ — slice 0 COMPLETE and closed out 2026-08-06; checkpoint answered 2026-08-07 (slice 1
  PROCEEDS on the `--opt` default-path result, §"Checkpoint outcome"); **slice 1 IMPLEMENTED,
  measured and CLOSED 2026-08-08 after 4 review rounds** — on the accounting identity + the `.ll`
  reduction, with the run-time measurement an explicitly OWED follow-up (§"What closes slice 1")
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

**PROOF SITES vs EMISSION OCCURRENCES (pinned, amendment 2026-08-07 — before slice 1).**
The slice-0 close-out established that one ANF demand site can be EMITTED more than once:
`MatchCompile` specialises a row into several submatrices, so a `case` body — and every
demand site inside it — becomes a force chain per decision-tree leaf, not per source row.
"One site = one count" (§3's census rule) is therefore about proofs, not emissions, and the
two are pinned as distinct identities:

- a **`ProofSiteId`** identifies a demand site in the ANF, resolved against its scope in
  place (the `DemandSiteId` form above). It is what the lattice decides, ONCE.
- an **`EmissionOccurrenceId`** identifies one emitted force chain. Every occurrence
  carries the `ProofSiteId` it came from and reads THAT site's decision — an occurrence
  never re-decides, and duplicated occurrences of one proof site are decided identically by
  construction. A proof site's **multiplicity** is the number of its occurrences.

Two consequences are normative:

1. **Neither consumer may enumerate sites for itself.** Liveness (which walks the raw ANF)
   and the emitter (which walks the `MatchCompile` tree) must not re-derive independent
   site sequences that happen to line up — that is the drift class this ADR exists to
   exclude, and the tree makes the two sequences DIFFERENT LENGTHS, so "they line up" is
   not even the right shape. The one computation produces both views: the decision keyed by
   `ProofSiteId`, and the occurrence sequence in the emitter's own traversal order (the
   emitter consumes it positionally, exactly as it consumes its own label numbering).
2. **The accounting identity is stated over occurrences.** Slice 1's "chains deleted must
   equal `Never`-proven sites" reads: *deleted chains == the number of emission occurrences
   whose proof site is `Never`* (= Σ multiplicity over `Never` proof sites). Counting
   deleted chains against PROOF sites would under-count wherever the matcher duplicated a
   row, and the census's `sites == chains` gate is what keeps the two views reconciled.

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

#### Progress (2026-08-06, provisional): slice 0 — first census run

The fact walk landed as `Backend.LLVM.ByNeed` (analysis-only) and ran over the fixed
closure (298/301 modules decoded; `--no-opt`), reporting 8,364 sites against 8,768 emitted
chains and a 0.2 % (16-site) `Never` share. **Superseded — the run was provisional and its
numbers are NOT this ADR's record.** Three defects made it so: the walk counted `CCase`
over the SOURCE alternatives while the emitter emits a force per DECISION-TREE occurrence
(a row surviving into a specialised submatrix is emitted more than once), 3 modules never
reached the walk, and the 618-chain residual was left unexplained rather than accounted
for. Corrected numbers and the closing verdict are below.

#### Close-out (2026-08-06): slice 0 completed — and the gate answer INVERTS between corpora

Slice 0 is complete under all four close-out conditions (301/301 decoded, accounting
resolved, re-runnable runner, unit matrix). Its verdict is not the one the provisional run
suggested: **`--no-opt` says 0.14 % (stop), `--opt` — the CLI default and the daily build
path — says 16.79 % (proceed).** Both numbers are on the same tree, from the same walk,
each with its per-object accounting identity holding. The `--opt` leg was expected to be
unmeasurable (the standing `mod_282` optimiser stall); that stall is NATIVE-only, and the
Node compiler completes the `--opt` closure in minutes, so the secondary corpus this ADR
asked for exists after all. **The slices-1+ decision is therefore a live maintainer call,
not a settled stop** (§"the checkpoint question" below).

**Measurement conditions.** Entry `Purvasm.CLI.Native.main`, `--corefn-dir output` with the
`dist/ulib` overlay, run in BOTH modes (`--no-opt`, the pinned primary corpus; `--opt`, the
secondary). Both legs of each run — the real native emission and the census — are driven by
the SAME `Purvasm.Compiler.build` over the same loaded closure, and the census reads the
SAME gdefs the emitter does
(`Driver.moduleGdefs` / `Driver.entryProgram`), so the two cannot diverge by
re-derivation. Re-run with `tools/byneed-census.sh`.

The corpus IS the compiler, so it moved with this close-out: the `Driver.moduleGdefs` /
`Driver.entryProgram` extraction is itself in the censused closure. That is why the total
here (8,772 chains) is not literally the Context section's 8,768 — the difference is the
close-out's own source, not a change of method. Numbers taken on a tree are only
comparable to numbers taken on the same tree; the accounting identity below is what makes
this run self-checking regardless.

**Accounting (the condition the provisional run failed).** 302 objects in each mode — 301
module objects (301/301 decoded, none skipped) plus the entry object — and for EVERY object
the counted demand sites equal the `fchk` force chains in its emitted `.ll`: 8,772 = 8,772
under `--no-opt`, 29,975 = 29,975 under `--opt`, zero mismatching objects in either mode.
The old residual is gone because it was never a residual: it was decision-tree occurrences
the walk had not counted, three uncensused modules, and the entry object. `SForceCell` is
correctly outside the count — it emits a direct `pv_force_if_byneed` call, not a chain.

`--no-opt` (primary corpus, 8,772 sites):

| site class | sites | `Never` | `Never` % |
| --- | ---: | ---: | ---: |
| `CCase` scrutinee | 4,187 | 0 | 0.00 % |
| `CAccessor` base | 3,951 | 5 | 0.13 % |
| guard result | 339 | 1 | 0.29 % |
| `CUpdate` base | 175 | 2 | 1.14 % |
| `CPrim` operand | 112 | 0 | 0.00 % |
| `CIf` condition | 8 | 4 | 50.00 % |
| **total** | **8,772** | **12** | **0.14 %** |

`--opt` (the CLI default / daily build path, 29,975 sites):

| site class | sites | `Never` | `Never` % |
| --- | ---: | ---: | ---: |
| `CPrim` operand | 11,929 | 2,289 | 19.19 % |
| `CCase` scrutinee | 9,110 | 2,283 | 25.06 % |
| `CAccessor` base | 7,938 | 57 | 0.72 % |
| guard result | 490 | 394 | 80.41 % |
| `CUpdate` base | 488 | 4 | 0.82 % |
| `CIf` condition | 20 | 7 | 35.00 % |
| **total** | **29,975** | **5,034** | **16.79 %** |

**Reading the inversion.** It is not noise, and it is not the two modes disagreeing about
the same code: the optimiser changes the demand-site POPULATION. Inlining (NbE) and
dispatch collapse (DictElim) move values that used to arrive through abstraction
boundaries into locally-built ones — a scrutinee that was a dictionary projection becomes a
constructor built two bindings earlier, and `CPrim` operands go from 112 to 11,929 sites of
which nearly a fifth are locally provable. The `--no-opt` corpus measures the reference
lowering, where by construction almost every forced value crosses an abstraction boundary
and the lattice can prove nothing: **0.14 % is a true statement about a corpus nobody
ships.** Conversely `--opt`'s `CAccessor` class stays 0.72 % `May`-dominated, which is the
one thing both corpora agree on — ADR-0070's mutually-recursive instance dictionaries
really are cells, and no module-local fact set flips them.

**Sizing the `--opt` result.** 29,975 chains × 9–12 emitted lines each = 8.0–10.7 % of the
mode's 3.37 M-line IR; the 16.79 % `Never` share is therefore **≈ 1.35–1.79 % of total IR**,
plus a branch and tag-check per EXECUTION at those sites (the run-time argument this ADR
was justified on), plus an UNMEASURED share of the crossing side. The crossing share is
deliberately not estimated from the 16.79 %: the Context's −2.1 % is the bound with ALL
forces de-safepointed, and `Never` sites are not distributed like crossing-heavy sites, so
no proportional slice of it is defensible — slice 1's plan census measures it or it is not
claimed. The census is likewise STATIC (one proof site, one count), so the run-time share
of these sites is unmeasured too, and stays unclaimed until the benchmark says otherwise.

#### Checkpoint outcome (2026-08-07, maintainer)

> Slice 0 completed. The `--opt` default-path result (16.79 %) exceeds the accepted
> checkpoint threshold; Slice 1 proceeds. The `--no-opt` result (0.14 %) is retained as
> the optimiser-free reference baseline.

The `--no-opt` corpus keeps its role as the reference-lowering contrast — it is what says
this lever is worthless as a source-level cleanup, and that local provability is
manufactured by the optimiser rather than present in the lowering. The apply-count track
(2.67 B dispatches) stays the larger, independent lever and proceeds in parallel as its own
ADR; the two do not block each other.

**Slice 1's success conditions are NOT the static 5,034** (pinned at the checkpoint):

1. the accounting identity — deleted chains == `Never` emission occurrences (§2's
   proof-site/occurrence pin);
2. a measured `.ll` reduction on the fixed closure;
3. a runtime benchmark result. The run-time win is unmeasured today and is claimed only
   after it lands, not before.

_(Superseded in part by §"What closes slice 1" below, written after slice 1 landed: conditions 1
and 2 are what the slice closes on, and condition 3 became an explicitly OWED follow-up rather than
a completion condition — the effect is below this machine class's noise floor, and deferring the
measurement is not the same as claiming the result. The "claimed only after it lands" rule is
unchanged and still binding.)_

**If a future re-measurement is needed** (the population is what moves this number): the
front end gaining type / strictness / representation provenance; a material change in the
optimiser's inlining policy — it is what manufactures the provable sites, so its
aggressiveness IS this lever's yield; or a materially different self-host corpus.
Cross-module `.pmi` facts alone are NOT such a trigger: the `CAccessor` class stays
`May`-dominated in BOTH corpora because those dictionaries are genuinely cells, and this
census measured that rather than assumed it.

**Where the infrastructure lives.** The lattice, the census walk and the runner are the
`census` package (`census/src/Purvasm/Census/**`, command `census byneed`), tool-owned —
NOT a backend module with no consumer. Unit coverage is `Test.Unit.Purvasm.Census.ByNeed`
(CI-wired): the lattice laws, `May` totality over every non-producer `CExpr` constructor,
occurrence shadowing, the stack-safe spine, and — the one that keeps a re-run honest — an
emission-fidelity matrix asserting counted sites == `fchk` chains emitted by the REAL
`moduleLl` for each shape, including the decision-tree duplication that broke the
provisional run.

#### Progress (2026-08-07): slice 1 — the lattice and the elision

Implemented as `Backend.LLVM.ByNeed` (production), consumed through the plan.

**How the one decision set is realised.** `Liveness.activationPlan` computes the activation's
`FactMap` ONCE and publishes it in the `ActivationPlan`; the pass classifies force sites through it
(`forcedAtomCanSafepoint facts a` — an elided force stops being a safepoint) and the emitter reads
the SAME value out of the plan (`Ctx.byNeed`, set where `crossing` is set) to decide whether to emit
the chain. Neither derives facts of its own. The §2 occurrence-identity property is preserved under
a name-keyed map by POISONING: a name bound at more than one occurrence in an activation is `May`
everywhere, so every lookup is either the unique binding's fact or the safe value. Activation
boundaries (`CLam` bodies, `LetRec` members) start fresh fact sets, as their emission starts fresh
plans. `enabled` is a field OF the decision set, not of either consumer, so the measurement
counterfactual (`PURVASM_BYNEED_OFF=1`, fail-closed like the ABI-profile knob) switches plan and
emitter together — gating at the consumers left the fact-independent producers (a scalar-primitive
result is `Never` whatever the map says) still eliding, which the unit accounting matrix caught.

**Results on the fixed closure (`--opt`, entry `Purvasm.CLI.Native`, 303 objects).**

| | without the lattice | with it | delta |
| --- | ---: | ---: | ---: |
| force chains | 30,217 | 25,145 | **−5,072 (−16.79 %)** |
| `.ll` lines | 3,392,751 | 3,302,435 | **−90,316 (−2.66 %)** |

1. **The accounting identity holds exactly: deleted chains 5,072 == elided emission occurrences
   5,072** (`BYNEED_ACCOUNTING=1 tools/byneed-census.sh --opt`, the counterfactual leg measured, not
   inferred), and per object the census's emitted occurrences equal the `.ll`'s chains for all 303.
2. **The `.ll` reduction is 2.66 %**, LARGER than the chains alone (5,072 × 9–12 lines ≈ 1.4–1.8 %):
   the difference is the crossing side — elided forces are no longer safepoints, so the plan roots
   less. That share was left unclaimed at close-out and is now measured rather than estimated.
3. **Run-time: UNRESOLVED — an OWED follow-up, not a completion condition (pinned below).**
   `tools/byneed-ab.sh` builds each benchmark twice with the SAME compiler (lattice on / off via the
   counterfactual knob) from SNAPSHOTTED inputs, requires each run's exit status, owns every
   measurement knob, refuses to time a program the lattice does not change, and reports the MEDIAN
   of per-pair ratios with their range. Two runs of the earlier (min-of-K) form of the harness
   on the same binaries DISAGREED IN SIGN — `fib` 0.938 then 1.068, `count-state` 0.954 then 1.233,
   `quicksort` 0.957 then 1.091 — with absolute times drifting 20–120 % between runs on a machine
   carrying an editor/IDE and a load average near 35. A one-off 1.36× "regression" on
   `run-state-except` in the first run did not survive re-measurement (0.99 over nine interleaved
   pairs), and its apparent cause — `gc_total_ns` 322 ms vs 132 ms — did not either (55–93 ms for
   BOTH legs on repeat, with `gc_collections`, `gc_copied_words` and `gc_max_live_words` IDENTICAL
   between legs, as they should be: the lattice removes no allocation). The effect being looked for
   is a predicted-taken branch plus a tag check per executed force — plausibly ≲ 1 % — and the
   noise floor of that earlier harness was an order of magnitude larger. Hardening it (snapshotted
   inputs, harness-owned knobs, and — the one that mattered most — the per-program HEAP passed
   explicitly instead of left to drift) narrowed the same probe's spread roughly fortyfold: `fib`
   went from a per-pair range of [0.897–1.315] (width 0.42) to **med 0.9984, range
   [0.9966–1.0073]** (width 0.011). That is
   consistent with an effect below 1 %, and it is THREE pairs on one program on a shared desktop —
   a suggestion, not a result. **The run-time argument in this ADR's Context therefore stays
   UNPROVEN**, and the justification that survives is the IR/memory one.

**What closes slice 1 (pinned).** Conditions 1 and 2 — the accounting identity and a measured `.ll`
reduction — are met, and together with the behavioural/differential/fixpoint battery they are what
slice 1 closes on. **The run-time measurement is an OWED follow-up, not a completion condition**:
it needs a quiet dedicated or self-hosted Linux environment (not a shared CI runner), cases sized
to 3–10 s, ≥ 20 order-alternated pairs, the median and distribution of the PAIRED ratios, and — where
available — `perf stat` instructions / branches / branch-misses alongside wall time. Until that runs,
no run-time claim is made in either direction. Deferring it does not gate the slice; making a claim
without it would.

Per class (elided / emitted): `case-scrutinee` 2,305 / 6,887 · `prim-operand` 2,302 / 9,728 ·
`guard-result` 397 / 98 · `accessor-base` 57 / 7,935 · `if-cond` 7 / 13 · `update-base` 4 / 484.

**Three defects found in review and fixed before close (each is now a pinned test).**

- **The decision set was not opaque.** `FactMap`'s constructor was exported, so a consumer could
  have built or edited the set the two of them are supposed to SHARE. It is now abstract: the only
  ways to obtain one are `activationFacts` and `noFacts`, and the only ways to read one are the
  decision functions.
- **The guard site had two derivations.** Liveness classified the post-guard force with a
  term-only rule (`guardResultForced`) while the emitter elided it through the decision set — an
  over-root, not an unsound under-root, but exactly the split §2 forbids, and it charged crossings
  to chains that were no longer emitted. `guardResultForced` now takes the decision set. The
  liveness matrix pins it: with the lattice off an arm-bound name crosses the post-guard force;
  with it on, and the guard's result proven, it does not.
- **Captures were an input to the fact computation.** `activationFacts` took the lifted capture
  list, so an out-of-tree walk (the census) had to reproduce the emitter's lifting decision to get
  the same facts — the drift the §2 pin exists to prevent, and one that would have made the census
  quietly wrong on a body where a capture name is also bound internally. Captures are gone from the
  signature: the facts are computed from `(params, body)` alone, with the body's FREE names
  collected directly, which is both the soundness rule (a free name that is also bound inside must
  be poisoned — a reference before the inner binding resolves to the free one) and what makes the
  census's facts identical by construction.

That last fix exercised a hazard the codebase had recorded and deferred: `fvExpr`'s `Let` spine was
ordinary recursion ("an iterative-spine rewrite is a result-preserving hardening deferred until
real large modules exercise it" — its own module note). Calling it per activation overflowed the
host stack immediately on the self-host corpus, so `fvExpr`/`cfExpr` are now spine-iterative, with
100k-binding fixtures asserting the RESULT, not merely the absence of a crash.

**Verification.** Unit: 533 compiler (incl. the new lattice spec — `May` totality over every
non-producer constructor, shadow poisoning, activation boundaries, the disabled-set counterfactual)
plus 26 census (the accounting matrix, which emits every shape TWICE through the real `moduleLl` and
asserts emitted/total/elided against the on/off chain counts). Behavioural: the §4 fixture landed as
`Gate.ByNeedCell` — a real `Grec`-backed by-need cell reaching every in-scope recipe through an
alias chain, a `Ref`-selected branch MEET that SELECTS the cell arm at run time, and a container
round-trip, with full-content readback after heap churn; the gate is green in both modes, under
`PURVASM_GC_STRESS=1`, alongside the existing six fixtures and the debug-ABI leg. Differentials:
`native-run-diff` 7/7 (boot ≡ Level-2 ≡ expected) and `ffi-e2e` all green. **Self-host fixpoint
(ADR-0104 §2, smoke profile): HOLDS — 605/605 artifacts byte-identical on both comparisons**
(C3-link ≡ stage-3, stage-3 ≡ stage-4), i.e. the eliding compiler compiles itself to a fixpoint.
Every gate in this paragraph was re-run AFTER the three review fixes above; the fixpoint in
particular is the post-fix run, since the guard fix changes emission.

## Consequences

_(Written pre-measurement; settled by the slice-0 close-out and then by slice 1's results.)_

- The force chain's IR mass is removable in proportion to the measured `Never` share, and it has
  been collected: **0.14 % on `--no-opt` (nothing) but 16.79 % on `--opt`, landed as −5,072 chains
  and −2.66 % of that mode's `.ll`.** The run-time branch+tag-check per executed force is the part
  that stays unclaimed (§"What closes slice 1").
- The lattice is the first BINDING-PROVENANCE fact set the backend has had, and as of slice 1 it is
  a production fact set with two consumers, not only an instrument. The mutation/ownership research
  track ([[mutation-ownership-research-track]]) and any future `.pmi` fact publication can reuse its
  shape — including its most transferable findings: **local provability is manufactured by the
  optimiser, not present in the source lowering** (the same fact set is near-useless before inlining
  and materially populated after it), and **a fact set that takes a lowering decision as input
  cannot be reproduced by an out-of-tree instrument** — deriving it from `(params, body)` alone is
  what made the census and the compiler agree by construction.
- The `May` pins keep `Grec`/dict laziness (ADR-0070) fully intact, in both corpora.
- The census infrastructure (`census` package + `tools/byneed-census.sh` + the
  emission-fidelity unit matrix) is the reusable residue: the next static site census —
  the apply-count track's `MissReason` census — should be built the same way, one
  classification source shared by emitter and census, with a per-object accounting identity
  as its gate.

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
