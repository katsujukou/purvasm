# 0114. The `OParam` use-site drill: which parameters and sites carry the higher-order dispatches

- Status: ~~Proposed~~ **Accepted** _(2026-08-27: explicit maintainer Accept after 5 review rounds.
  **SLICE 1 ONLY** — the use-site drill. What comes after it (§2) is not authorised and needs an
  amendment plus a re-approval. The rounds settled: the measurement unit is not the specialisation
  decision unit; a program-unique site identity in two layers (`ProofSiteId` / `EmissionSiteId`);
  the annotation pass sits post-optimiser and pre-`MatchCompile` and covers `CPerform` as well as
  `CApp`; `BindingSite` carries `ParamIndex` while `BindOrigin` stays finite; ONE projection to the
  finite reason, in `causeReason`; and the toolchain provenance the headline run must prove)_
- Date: 2026-08-24
- Deciders: maintainer
- Technical story: ADR-0113's slice-2 profile left `local-unknown-fn/param` as the largest single
  dispatch population in the compiler — **81,252,445 executions, 24.85 % of all dispatches, at 654
  sites (4.16 %), `exec/site` 5.97×**, measured on the PRE-owned-VM-merge snapshot (see the corpus
  correction below; the post-merge static count is 661) — and put it out of scope by construction:
  no local fact exists to recover, so it is not an emitter lever
- Scope: **Slice 1 only.** It narrows the population for a LATER caller/argument drill; it does not
  decide whether caller-homed specialisation applies, and it authorises no lowering

> **Numbering note.** `0110`–`0112` are the owned-VM track, merged to main at `a873da0`;
> `0113` is this track's previous record. This one takes `0114`.

> **Corpus correction (2026-08-26), measured.** The owned-VM track merged into `main` at
> `a873da0` AFTER ADR-0113's measurements were taken. **The headline figures in this record's
> Technical story are therefore a PRE-OWNED-VM-MERGE snapshot** — kept, not deleted, and labelled as
> such. The post-merge static corpus was re-censused on `8450513`:
>
> | | pre-merge | post-merge | Δ |
> | --- | ---: | ---: | ---: |
> | objects | 305 | 310 | +5 |
> | dispatch sites | 15,710 | 15,979 | +269 |
> | `OParam` apply | 350 | 356 | +6 |
> | `OParam` tail | 304 | 305 | +1 |
> | **`OParam` total** | **654** | **661** | **+7** |
> | `OParam` site share | 4.163 % | 4.137 % | −0.026 pp |
>
> The +7 is attributed in full: `Purvasm.Compiler.Bytecode.Linearise` +5 apply,
> `Purvasm.Compiler.Backend.Bytecode` +1 apply / +1 tail. The five new objects are
> `Purvasm.Abi.{Fnv1a64,Mangle,Utf8}`, `Purvasm.Compiler.Bytecode.Linearise` and
> `Purvasm.Compiler.NativeLeaf`. All 310 objects reconcile.
>
> **The 81,252,445 executions do NOT carry over and are not extrapolated.** New modules are compiled,
> `BuildProducts.foreignSigs` grows, and the CLI/bytecode path does more work, so the execution
> weights move for reasons the static delta cannot predict. Slice 1 re-derives its own aggregate.
>
> **The measurement mechanism is unaffected**, checked rather than assumed: `runtime/src/apply.rs`,
> the `purvasm-stats:v1` schema, the keyed profile map, and the LLVM `Emit`/`CallClass`/`Safepoint`
> modules are byte-unchanged across the merge, so `pv_apply_entries`, `pv_tailcall_writes`, the
> reason slots and the keyed bump all still mean what ADR-0108/0113 established. The existing
> identities and self-tests remain valid.

## Context

Three ADRs of dispatch work have now converged on one population.

| | class | executions | share of dispatches | outcome |
| --- | --- | ---: | ---: | --- |
| ADR-0109 | `callee-foreign` | 430.2 M removed | was 57.8 % | CLOSED — direct-lowered |
| ADR-0113 | `local-unknown-fn`, recoverable part | 16.9 M (upper bound) | 5.16 % | STOPPED — too small |
| **here** | `local-unknown-fn/param` | **81.3 M** | **24.85 %** | unmeasured |

**And its form split is unlike anything else this track has measured: 29,749,261 `apply` +
51,503,184 `tail` — 63.4 % of the population is the TRAMPOLINE form.** For comparison,
`callee-foreign` was 2.1 % tail (ADR-0109) and ADR-0113's recoverable candidates were 13.3 %. A
majority-tail population is a different engineering problem: the operation is a `pv_tailcall` store,
not a `pv_apply` dispatch, and ADR-0109 slice C is the standing evidence that removing trampoline
stores is the harder sell — 9.42 M removed came back INCONCLUSIVE on wall clock. Whatever this
record's drill finds, a lowering aimed at `OParam` would be aimed mostly at the tail form, and that
has to be known before the design rather than discovered inside it.

`OParam` is the genuine higher-order call: the callee is a function parameter, and at the bind site
there is nothing to recover — no fact was dropped, none ever existed. ADR-0113 §5 named it out of
scope for exactly that reason and pointed at caller-homed specialisation, which is a different
mechanism living on the optimiser seam rather than in the emitter.

**This record does not design that mechanism, and — stated plainly because the first draft of this
ADR got it wrong — it does not decide whether that mechanism APPLIES either.**

The unit this drill measures and the unit caller-homed specialisation decides on are different, and
neither implies the other:

- **A use site that is monomorphic at run time need not be specialisable.** Monomorphic means one
  callee value arrived; specialisation needs the argument to be a STATICALLY KNOWN function at the
  caller. A parameter fed from a dictionary field, a data structure or another parameter can be
  perfectly monomorphic at run time and offer the caller nothing to clone against.
- **A use site that is polymorphic overall may be entirely specialisable.** If `f` is called from
  three callers with three different known functions, the SITE sees three callees while each CALLER
  is monomorphic — and cloning per caller removes every dispatch. Judged at the site, this looks
  like the case to avoid; judged at the caller, it is the ideal one.

So the decision unit for caller-homed specialisation is at minimum
**(enclosing function, parameter index, caller occurrence, the static fact of the actual argument)**
— a caller-side, compile-time relation. This drill measures **(use site, parameter)** with run-time
weights. That is a strictly weaker thing, and it is worth doing for a strictly weaker reason:

> **~660 sites at ~6× could be a dozen sites inside hot loops or a broad flat population**, and the
> caller/argument drill that WOULD decide applicability is the expensive one. Slice 1 exists to say
> which parameters of which functions are worth following back to their callers at all.

What slice 1 cannot do is return a verdict on specialisation. It returns a shortlist.

**The standing evidence for measuring first is this track's own, and it is fresh.** ADR-0113 looked
attractive on the aggregate (26.1 % of executions), was designed carefully, was implemented through
slice 1 — and its own slice-2 numbers stopped it at 5.16 % with the second knob stage unable to
satisfy its completion condition. The measurement cost two slices and saved an emitter change that
would have shipped a knob, three counter families and a golden re-baseline for an effect at the
noise floor. The same discipline applies here, where the prior art is worse: the HOS study on this
track records `purs-wasm`'s dedicated recursive-worker specialisation blowing the output up 4×, with
the root cause identified as a size/use metric being unable to tell a reducing clone from a
non-reducing one.

### What is already in hand

- **The drill machinery is reusable as-is.** ADR-0108 §4 built `pv_applyprofile_key(ptr, i64)` over a
  `BTreeMap<String, u64>`: the emitter hands it a string constant per site and the runtime counts
  executions per key. It carried 28 keys for the foreign drill; 661 (the post-merge site count) is
  the same order.
- **The classification is already exact.** `MissLocalUnknownFn OParam` is a leaf of `directTarget`'s
  tree, counted by both the static census and the `(form × reason)` slots, with the ADR-0113 §3
  identities holding per object and whole-program.
- **The reconciliation shape is precedented**: ADR-0108 §4's `Σ drill keys == the two callee-foreign
  slots` is the cross-mechanism check that makes a drill's numbers evidence rather than arithmetic.

### What is NOT known

1. how the 81.3 M executions concentrate — by call site, and by the function that contains it;
2. whether the hot sites are few enough to name;
3. **which callers supply the hot parameters, and whether the argument is a known function there** —
   the relation that actually decides specialisation. NOT measured by this record: it is caller-side
   and compile-time, and it is what slice 1's shortlist is for;
4. how the population splits by ARITY (the form split is known — 63.4 % tail, above — but its
   distribution ACROSS sites is not: one hot tail site and a broad apply population would read the
   same in aggregate).

Items 1, 2 and 4 are what slice 1 answers: compiler-side, cheap, and bounded. Item 3 is a different
drill on a different axis (caller occurrences and their static argument facts), sized by slice 1's
result and designed after it — see §2, which this record deliberately leaves undecided.

### The workload stays the NATIVE compiler, and the owned VM cannot be measured by these counters

Now that the owned VM is on `main` (ADR-0110/0111), one boundary has to be pinned before it is
crossed by accident:

- **`pv_apply` counts the carrier/native-closure boundary.** The owned VM applies GUEST closures
  itself, inside the interpreter; those applications never reach the runtime's dispatcher and are
  invisible to every counter this ADR uses. A VM run would report a small, unrepresentative number
  and it would look like a result.
- **So this record's workload is fixed**: the natively-compiled `Purvasm.CLI.Native` performing a
  compiler build, exactly as ADR-0108/0109/0113 measured. Profiling a VM runner is a different
  measurement needing its own instrumentation, and it is not this record's.
- **The CLI wiring, when it lands, moves the STATIC corpus even so.** `VM.Machine` and its closure
  become reachable from the entry once imported, whether or not `build` executes them — so they
  enter the site census while contributing no executions. That is a corpus change like any other and
  is handled the same way: state the snapshot, and attribute the delta with one classifier over both
  (ADR-0113 §3(c)).

## Decision

**ONE measurement slice, then a checkpoint.** No optimisation is authorised by this record, and no
lowering, inliner change or specialisation pass is begun in it. §2 describes what comes next only so
that slice 1 is designed to feed it — it is explicitly NOT authorised, and §3 states that boundary.

### §1 Slice 1 — the use-site drill, keyed by a program-unique site identity

`Emit` already emits a drill key at a classified site under `PURVASM_PROFILE_APPLY`. The `OParam`
arm gains one — but NOT a string assembled at the site, and not one built from `Lifted.name`.

**Why `Lifted.name` cannot be the key.** It is `fn_<n>`, minted per OBJECT by `freshFn`, so
`fn_2` exists in every object that lifts three lambdas. The runtime holds ONE `BTreeMap` for the
whole program, so two objects' sites would silently merge into one bucket — and a merged bucket is
indistinguishable from a hot site, which is precisely the thing this drill is meant to find. The
emitter also does not currently carry the emitting `Lifted.name` on `Ctx`, so the key cannot even
be assembled at the bump site today.

**The site identity is a value, minted once, read by everyone.**

The identity is a value with one producer and two renderings — the canonical key the LLVM constant
carries, and the label a report prints. They are separate functions because they have different
contracts: the key must stay parseable and stable, the label may be reformatted freely. See the
types below.

- **`object`** is the object's own identity, which is what makes the key program-unique;
- **`fn`** is the lifted function within it;
- **`param`** is the PARAMETER INDEX the callee came from, not the callee's name. The shortlist this
  drill produces is consumed by a caller-side drill, and "the third parameter of this function" is
  the thing a caller can be asked about; a local variable name is not;
- **`anf`** is a STRUCTURAL occurrence identity, derived from the ANF term rather than from a
  counter over classified sites: numbering the `OParam` sites in emission order would make the
  identity depend on the classification being measured, so changing what counts as `OParam` would
  renumber everything downstream and two snapshots could not be compared.

The same `EmissionSiteId` is carried by the `CallEvent`, consumed by the static census, and rendered
by `siteKey` for the keyed bump. One value, three readers: a drill key and a census row that disagree would be two
measurements of two different things, which is the failure ADR-0107's close-out records.

**Two site notions, and they are two TYPES, not one field with a comment.** Match compilation
(ADR-0083's decision tree) can DUPLICATE one ANF call occurrence into several emitted call sites, so
the thing a future optimisation would rewrite and the thing that executes are different objects with
a one-to-many relation between them:

**Where the occurrence identity lives, and why not in the ANF pass.** The first sketch put
`mintAnfOccurrence :: Codegen …` in the middle end, which is wrong twice over: `Codegen` is the
LLVM backend's monad and the ANF pass must not depend on a backend's internals (the subsystem rule),
and ANF's `CApp` has no annotation field to carry an id in — while the OPTIMISER creates new `CApp`
nodes, so "the ANF occurrence" is not even well-defined until the optimiser has finished.

So the identity is minted by a **backend-neutral annotation pass that runs POST-optimiser and
PRE-`MatchCompile`**, over an annotated copy of the ANF term. That placement is the whole design:
after the optimiser, the term is final, so an id means the same thing on both sides of a
measurement; before match compilation, one id still corresponds to one source-level call, which is
what a later specialisation would rewrite.

```text
-- 1. minted by the post-opt, pre-MatchCompile annotation pass. Backend-neutral: it names a node in
--    the ANF term and knows nothing about LLVM.
newtype AnfOccurrenceId
annotateOccurrences :: Expr -> AnnotatedExpr        -- the ONLY producer

-- 2. what an optimisation would act on: the occurrence bound to its function and parameter
newtype ProofSiteId
mkProofSiteId :: ObjectId -> FnId -> ParamIndex -> AnfOccurrenceId -> ProofSiteId

-- 3. what executes and is counted: one emitted call site; several may share a ProofSiteId
newtype EmissionSiteId
mkEmissionSiteId :: ProofSiteId -> Int -> EmissionSiteId   -- Int = a deterministic per-proof ordinal
proofOf :: EmissionSiteId -> ProofSiteId                   -- total, the ONLY relation

-- 4. the two renderings, kept apart on purpose
siteKey  :: EmissionSiteId -> String   -- CANONICAL: what the LLVM string constant carries
siteLabel :: EmissionSiteId -> String  -- human: what the report prints
```

**The plumbing, stated because a type without a route is a wish.** Each hand-off is named, with the
producer and the consumer:

| step | produced by | carried on | consumed by |
| --- | --- | --- | --- |
| `AnfOccurrenceId` | `annotateOccurrences`, post-opt / pre-`MatchCompile` | the annotated `CApp` AND `CPerform` nodes | the emitter |
| …across DTree duplication | `MatchCompile` COPIES it | every duplicated leaf AND guard arm | the emitter |
| `ParamIndex` | `emitFunction`'s parameter prologue | the `EnvEntry`'s `BindingSite` | `directTarget` |
| …out of the classifier | `directTarget` | `ParamTarget`, a `CallTarget` arm (below) | the emitter |
| `ProofSiteId` | the emitter, once per site | `EmissionSiteId` (below) | the report, aggregated |
| `EmissionSiteId` | the emitter, per emitted duplicate | `CParam`, inside `GenericCause` | keyed bump, census |
| the finite `MissReason` | `causeReason`, ONCE | — | census, `profileSlot` |

Two of those rows are the ones that were missing and are therefore stated as requirements:

- **`MatchCompile` must COPY the `AnfOccurrenceId` into every arm it duplicates**, not re-mint. That
  is what makes `ProofSiteId` genuinely upstream of the decision tree; without it the "proof" layer
  would be a second emission layer wearing a different name.
- **`ParamIndex` travels on the BINDING, and the REPORT class stays finite.** Putting the index
  inside `BindOrigin` — `OParam ParamIndex | OCapture | …` — was the obvious move and it breaks
  something ADR-0113 pinned: `bindOrigins :: Array BindOrigin` enumerates all seven origins, and
  `allMissReasons`, the census reason schema and the diagnostic-zero rows are all stated over that
  enumeration. A constructor carrying a `ParamIndex` is no longer enumerable, so the reason axis
  would become unbounded and the identities stated over it would stop being checkable.

  So the two roles are two types: the binding CARRIES the identity, and a total projection gives the
  finite class the report is keyed by.

  ```text
  -- what the environment entry holds
  data BindingSite
    = ParamBinding ParamIndex          -- a parameter, and WHICH one — inseparably
    | OtherBinding NonParamOrigin

  -- the six origins that are not a parameter; still a closed enumeration
  data NonParamOrigin
    = OCapture | OLetLambda | OLetValue | OGrecLambda | OGrecValue | OMatchBinder

  -- the FINITE report class, unchanged from ADR-0113: seven values, enumerable
  originClass :: BindingSite -> BindOrigin
  ```

  `bindOrigins` and every identity stated over it are untouched; the index rides along for the drill
  and is projected away for the census. A `{ origin, index :: Maybe ParamIndex }` record would have
  made "a parameter with no index" and "a match binder with an index" constructible, needing a
  run-time check for a state the type should forbid — the argument ADR-0108 used for `CallEvent`
  and ADR-0113 for `LocalFact`. Recovering the index at the call site instead, by matching the name
  against the parameter list, would be a second derivation that a shadowed name makes wrong.
- **The classifier must not PROJECT on the way out.** `BindingSite` keeps the index on the binding,
  but that is only half the route: if `directTarget` returns `GenericTarget (MissLocalUnknownFn
  OParam)` the index is already gone by the time the emitter wants to build a `ProofSiteId`, and
  recovering it means looking the name up in the environment a SECOND time — the derivation this
  design exists to avoid, and one a shadowed name makes wrong.

  So the parameter case gets its own arm, carrying what it knows and NOTHING ELSE:

  ```text
  data CallTarget
    = GuestTarget FnInfo
    | ForeignTarget ForeignRef
    | LocalCandidateTarget LocalCandidate    -- ADR-0113
    | ParamTarget ParamIndex                 -- NEW: a parameter callee, and WHICH parameter
    | GenericTarget MissReason
  ```

  **`ParamTarget` does not know what reason it becomes.** An earlier draft of this section gave the
  classifier a `missReasonOf (ParamTarget _) = MissLocalUnknownFn OParam` alongside the
  `causeReason` below, and called each of them "the only projection". Two functions deriving the
  same finite class independently is exactly the shape that lets a later change touch one and not
  the other — the defect ADR-0107's close-out records, where a second way of counting sites produced
  a plausible wrong number that survived a review. So the classifier layer projects NOTHING: it
  hands the index onward, the emitter attaches the site identity, and the finite reason is derived
  in ONE place, downstream, from the value that actually reached the event.

  If a classifier-stage tally is ever wanted, it is computed from the final `CallEvent` through that
  same one projection — not by re-deriving the reason where the target is still a target.

  **The route does not stop at the classifier.** `ParamTarget` only closes `directTarget`'s exit; if
  the DECISION then produced `EmitGeneric (MissLocalUnknownFn OParam) form`, the index would be lost
  one step later and the emitter would be back to a second lookup. So the generic channel carries a
  CAUSE rather than a bare reason, and both the decision and the event hold it:

  ```text
  -- what a generic dispatch is generic BECAUSE OF. One sum, carried end to end.
  data GenericCause
    = COpaque MissReason         -- everything ADR-0108/0113 classified
    | CParam EmissionSiteId      -- a parameter callee, carrying the site identity itself
                                 --   (built from ParamTarget's index by the emitter, which is the
                                 --    only place that knows the object, function and occurrence)

  -- THE one and only projection to the finite report class, anywhere in the pipeline
  causeReason :: GenericCause -> MissReason
  causeReason (COpaque r) = r
  causeReason (CParam _)  = MissLocalUnknownFn OParam

  data EmissionDecision = … | EmitGeneric GenericCause Form
  data CallEvent        = … | GenericApply GenericCause | GenericTail GenericCause
  ```

  The census keys by `causeReason`, the drill keys by the `EmissionSiteId` inside `CParam`, and
  both read the SAME value — so a keyed bump and its census row cannot describe different sites.
  `profileSlot` is unchanged in shape: it goes through `causeReason`, so the `(form × reason)` slot
  space and every ADR-0113 identity stated over it stay exactly as they are.

  `MissReason` itself is untouched — `OParam` stays a nullary constructor and `bindOrigins` stays
  enumerable, which is what those identities require. The index rides in the cause, not in the
  reason.
- **The annotation must cover `CPerform`, not only `CApp`.** `Emit` lowers `CPerform t` as
  `CApp t [unit]` (one line, `Emit.purs`), so a performed thunk whose callee is a parameter reaches
  the SAME `OParam` classification and bumps the same reason slot. An `AnfOccurrenceId` minted only
  at `CApp` nodes would leave those sites unkeyed, and the completion condition below —
  `Σ keyed bumps == the two OParam slots` — would fail by exactly that population. Annotating both
  is what makes the identity hold; noticing it here is cheaper than debugging a shortfall later.

**The canonical key and the label are separate functions.** The keyed bump needs a string the
emitter can put in an LLVM constant and the runtime can use as a `BTreeMap` key: stable, parseable,
and independent of how a report chooses to display it. `siteKey` is that; `siteLabel` is for humans
and may change freely. The earlier "rendering happens only at the report boundary" was wrong — the
LLVM constant IS a rendering, and it happens at emission.

- the drill KEYS by `EmissionSiteId` — that is what a bump can count;
- the report AGGREGATES to `ProofSiteId` as well, and prints both — a per-`ProofSiteId` row with its
  duplicate count is what the next ADR can act on, and the `EmissionSiteId` rows are where the
  weights actually are;
- **the mapping is reported, not assumed**: if one `ProofSiteId` has several `EmissionSiteId`s, the
  report says so per row. Collapsing them silently would hand a later specialisation a site count
  that does not match the code it would rewrite.

`ParamIndex` is its own producer, minted where the parameter list is bound (`emitFunction`'s
prologue) and not reconstructed by name at the call site: a name can be shadowed, an index cannot,
and the caller-side drill this feeds asks about "the n-th parameter".

Reporting only executions would hand the next ADR a number it cannot act on; reporting only ANF
occurrences would lose the weights. Both, with the relation between them, is the deliverable.

#### Completion conditions

All fail-closed; a missing input is a failure, never a zero.

1. **The cross-mechanism identity, to the unit and PER FORM**:
   ```text
   Σ keyed bumps (apply) == generic-apply/local-unknown-fn/param
   Σ keyed bumps (tail)  == generic-tail/local-unknown-fn/param
   ```
   The keys and the slots are written by different code down different paths, so their agreement is
   evidence rather than arithmetic (ADR-0108 §4). Never summed across forms: they are different
   emitted operations.
2. **The STATIC key set is authoritative.** The census emits one row per `OParam` site it
   classified; the drill emits executions. The two are joined as a FULL OUTER JOIN with absent
   treated as zero, so a site that never executed appears with 0 rather than vanishing — a cold site
   is a finding (it is code the optimiser could ignore), and a site present only in the runtime set
   is a bug that must fail rather than be ranked.
3. **Key hygiene, checked explicitly**: the distinct key count equals the static site count; no key
   appears twice; every key parses back into an `EmissionSiteId`.
4. **Placement**: the keyed bump is emitted AFTER operand materialisation and IMMEDIATELY before the
   dispatch, with no intervening call. ADR-0108 §3 already pins this for the slot bump and gives the
   reason — an allocating operand between the bump and the dispatch could collect, and a bump that
   is not adjacent to its dispatch counts a different event.
5. **The self-measurement property, stated in advance**: the corpus IS the compiler, so adding the
   drill changes it. The exactness condition is ADR-0108 §4's corrected form — (a) exact on the older
   pinned corpus, (b) exact cross-mechanism on the new one, (c) the delta accounted for by running
   ONE classifier over both snapshots — never "equals the previously published 81,252,445".
   ADR-0113 §3(c) is the worked example, including that the older snapshot may have to be
   reconstructed from its commit with a FULL workspace build.
6. **Emission unperturbed**: the uninstrumented `.ll` for a pinned CoreFn snapshot is byte-identical
   before and after, checked against the pinned snapshot and NOT against `output/`. The baseline is
   taken FRESH on the tree the slice starts from — not reused from an earlier session. This is not
   pedantry: the ADR-0113 baseline lived in a session-scoped temporary directory and was gone by the
   time it was next wanted, and `_build/emit-baseline-postmerge/` (with its `COMMIT` file) exists
   because of that.
7. **The headline run BUILDS its toolchain; it does not infer provenance from timestamps.** On
   2026-08-26 `dist/ulib` was a staging six weeks older than `ulib/` and the release runtime
   staticlib predated `runtime/src` likewise — every measurement in that window was perfectly
   reproducible and described a different library. It did not move this record's headline
   (`OParam` 661 either way); it moved `wrapper-entry` 15,430 → 15,428.

   The first fix attempted here was a timestamp comparison, and **it does not prove what it needs
   to prove**: an uncommitted edit made after the build has no commit newer than the artifact, so
   the artifact reads as fresh; an artifact built on another branch reads the same way. That was
   demonstrated, not argued, and it is now an asserted LIMIT in
   `tools/toolchain-manifest.sh`'s self-test rather than a claim in its header.

   So the condition is split by what each mechanism can carry:

   - **the headline run calls `toolchain_prepare`** — wired: `apply-profile.sh --selfhost` takes
     that branch, the fixture legs keep the cheap one. It builds the runtime staticlib, stages the
     ulib and builds `output/` in ONE leg. Sequential builds take minutes and a tree can be edited
     while they run, so the FULL source closure (HEAD plus every tracked and untracked-unignored
     file's content) is digested either side of the leg and a mismatch FAILS: the guarantee is
     "these artifacts came from ONE tree state", and it is checked rather than hoped. The manifest
     records `prepared 1`, the commit, the dirty flag and that source digest. The digest
     IDENTIFIES the tree state — it says whether two runs saw the same sources — but it does not
     REPRODUCE it: a dirty prepared run is identifiable, not replayable, and replaying one would
     need the patch or a source snapshot kept beside the manifest;
   - **a pinned `--toolchain` snapshot is verified against the manifest recorded WHEN IT WAS
     BUILT** (`toolchain_verify_snapshot … require-prepared`), by hash, and this is FAIL-CLOSED:
     a missing manifest is an error, not a warning, and a diagnostic-only manifest does not satisfy
     it. A missing-manifest warning would have made the provenance condition satisfiable by deleting
     a file. Each row carries an explicit `snapshot_rel` rather than a path the verifier guesses —
     the first version derived it with `basename`, so the runtime staticlib (copied to
     `<snap>/rt/`) reported MISSING on an UNCHANGED snapshot. The wrappers are rows too: both
     resolve their compiled modules relative to themselves, so a snapshot whose wrapper changed is a
     different classifier over the same CoreFn. Measuring a genuinely historical snapshot is a
     separate, explicit mode (`TOOLCHAIN_ALLOW_UNPREPARED=1`), not a softer default;
   - **`toolchain_check` is a DIAGNOSTIC**, advisory, run after the build (`output/` is the
     harness's own product, so checking it beforehand would refuse a tree the run is about to
     refresh). It catches the case that actually happened — an artifact left behind for weeks —
     warns when the tree is dirty, and is never the thing a published figure rests on.

   The declared source closures are deliberately wide (`ulib` also covers `ulib-tools`,
   `packages`, `base` and the spago manifests; `runtime` covers `Cargo.lock` and `include`;
   `corefn` covers `abi`, `vm` and every in-repo package), because a narrow closure reports `ok`
   for an artifact whose real input changed elsewhere — which is the same failure one level down.
8. **`tools/apply-profile.sh --self-test` gains injections for the new keys** — including a
   duplicate key, a runtime-only key, and a per-form sum that disagrees with its slot. A gate
   satisfiable by the absence of its own input is not a gate.

#### What slice 1 delivers

Two ranked tables from one run, with the relation between them stated per row:

- by **`EmissionSiteId`** — where the executions are;
- by **`ProofSiteId`** — what a later optimisation would rewrite, with its duplicate count,

each with cumulative shares, the form split, the arity split, and the cold sites at zero. In one
sentence: **which parameters of which functions are worth following back to their callers.**

It does NOT deliver a specialisation verdict, a monomorphism figure, or a lowering.


### §2 What comes after slice 1 — NOT DECIDED, and NOT AUTHORISED by this record

Slice 1's shortlist feeds a caller/argument drill. Its design is deliberately left open, because the
shortlist's shape decides it and because the two obvious shapes have failure modes that must be
settled on evidence rather than in advance. **Accepting this ADR authorises SLICE 1 ONLY.** Anything
below needs an amendment to this record and an explicit re-approval, exactly as ADR-0113 §4 required
of its slice 3 — and, as there, a NO is a legitimate outcome.

What is already known about the design space, so the amendment starts from facts:

**The caller-side relation is the one that decides applicability** (§Context): for each shortlisted
(function, parameter), which caller occurrences pass a STATICALLY KNOWN function. That is a
compile-time query on the optimiser seam, not a runtime histogram, and it is the measurement the
next record should centre on.

**A runtime callee-identity histogram is a WEAKER, different measurement**, and if it is taken at all
its contract has to be pinned first:

- **The identity is not a code-table index.** In the native self-host build a closure's code word is
  a **real `extern "C"` function address** (`Heap::code_is_address = true`); the `code_table` index
  is the bring-up/Miri path only. An address is **process-local**: it is not comparable across runs,
  across builds, or against any symbol table the report reads, so a raw address is a within-run
  equivalence key and nothing more. Rendering it as a function NAME needs a separate address→symbol
  map that does not exist today.
- **The generic apply entry sees more than closures.** `pv_apply` is reached by `Closure`, by
  `Pap` (under- and over-application) and by `ByNeed` (a forced cell), each already counted
  separately by `purvasm-stats:v1`. A monomorphism measurement therefore needs a TOTAL
  `CallableIdentity` — `ClosureCode` / `Pap` / `ByNeed` / a diagnostic invalid arm — and must state
  how each participates in the verdict. A `Pap` over one underlying function is arguably the same
  callee; a `ByNeed` cell is the same callee only after forcing. Collapsing them into "the code
  word" would report monomorphism that the emitter could not act on.
- **First-seen counters cannot answer the question.** A `same`/`different` pair against the FIRST
  callee measures the first callee's share, not the dominant one: a site whose first execution takes
  a rare path reads as polymorphic while being effectively monomorphic. It is usable as a
  CARDINALITY diagnostic and must not be used as a WIN/NO criterion. What answers the question is a
  full histogram, or a capped heavy-hitter count with an explicit overflow bucket — and on overflow
  the report must give an upper and lower bound on the dominant share and record **INCONCLUSIVE**
  when they do not separate.


### §3 The checkpoint, and what is NOT authorised

**What this record authorises: SLICE 1.** Nothing else. The approval boundary is stated here rather
than implied, because "two slices and a checkpoint" reads as permission for both.

Slice 1 produces a ranked (object, function, parameter, occurrence) table with cumulative shares, the
form split per row, the arity split, and the cold sites at zero. **The maintainer decides from that
table what the next drill measures and whether it happens** — as an amendment to this record or a new
one, exactly as ADR-0108 §4 handed the foreign result to ADR-0109 and ADR-0113 §4 handed its own
result to a decision that came back NO.

Explicitly out of scope here, and each would need its own record:

- **caller-homed specialisation** in any form — peel-once, recursive-worker, or dictionary-directed.
  The HOS prior-art study on this track is the input to that design, not to this one;
- **any emitter change.** `OParam` sites keep lowering exactly as they do today;
- **an inliner policy change.** The optimiser seam's inline gate is ADR-0089's business and moves for
  its own reasons;
- **the caller/argument drill and any runtime callee-identity measurement** (§2). Their contracts —
  the `CallableIdentity` totality, the address's process-local scope, the histogram cap and its
  overflow bounds — are open questions, and an Accept that silently covered them would authorise
  instrumentation whose failure modes are known but undecided.

A NO is a legitimate outcome of this record, and on this track a likely one: ADR-0113 reached NO
after two slices, and saying so cost far less than the alternative.

#### Amendment (2026-08-27): the implementation shape — ANF is PARAMETERISED over the call annotation

Accepted-amendment, recorded before the code lands because it touches the SHARED ANF type and the
bytecode backend, which §1 did not anticipate when it left the mechanism open.

**What §1 left open, and what measuring it showed.** §1 named two options: a backend-neutral
annotation pass, or an LLVM-local annotated ANF copy. Both cost more than it assumed:

- `CApp`/`CPerform` are read by **twelve modules**, including `Bytecode/Lower.purs` — so "just add a
  field" reaches the bytecode path, which carries boot parity;
- a full LLVM-local mirror of `Expr`/`CExpr`/`Rhs` is a large duplicate that must then be kept in
  step with the original by hand.

And a third option that suggests itself — run `foldCalls` and keep the ids in a parallel array or a
counter — **must not be used**: after match compilation duplicates an occurrence, the correspondence
between the emitter's walk and that array becomes positional, which is the second derivation this
whole design exists to remove.

**The shape, decided:** ANF is parameterised over TWO annotations — the call occurrence, and the
FUNCTION BOUNDARY that owns it.

```text
Expr Unit Unit
  ── annotateOccurrences ──▶  Expr AnfFunctionId AnfOccurrenceId
  ── MatchCompile (COPIES both) ──▶  DTree AnfFunctionId AnfOccurrenceId
  ── ParamTarget ──▶  ProofSiteId { object, sourceFn, param, callOcc }
  ── per-proof ordinal ──▶  EmissionSiteId  (+ the lifted fn_N as metadata)
```

**Why the function boundary needs an annotation too — a defect found while reading the code, before
any of this was written.** Match compilation does not only duplicate CALLS; it duplicates the
FUNCTIONS that contain them. `MatchCompile.goCtor` copies a wildcard row's RHS into every
constructor arm AND into the default, and the emitter lowers each resulting `Dleaf` independently.
If that RHS contains a `CLam`, `lift` runs once per leaf and `freshFn` mints a DIFFERENT `fn_N`
each time. Keying `ProofSiteId` by `Lifted.name` would therefore split one source lambda across
several proofs — and "a proof is what an optimiser rewrites once" would be false for exactly the
programs the drill is meant to rank.

So the source function is identified upstream, where the duplication has not happened yet, and
`Lifted.name` is demoted to reporting metadata on the EMISSION layer, where several lifted names
legitimately share one function proof.

```text
CLam     AnfFunctionId   params body
CApp     AnfOccurrenceId callee args
CPerform AnfOccurrenceId thunk
```

The ROOT body of every emitted function gets an `AnfFunctionId` as well — otherwise calls in a
function that contains no `CLam` would have no source-function identity at all.

- `ExprF fnAnn callAnn` / `CExprF fnAnn callAnn` / `RhsF fnAnn callAnn`, with `callAnn` reaching
  `CApp` and `CPerform` and `fnAnn` reaching `CLam`;
- the optimiser and the bytecode backend work at `ann = Unit` and are unaffected in meaning;
- the LLVM backend converts to `ann = AnfOccurrenceId` post-optimiser, which is where
  `annotateOccurrences` runs;
- **`MatchCompile` is polymorphic in BOTH annotations and COPIES them** into every leaf, guard and
  fallthrough it produces — including the copies `goCtor`/`goLit` make of a wildcard row. Re-minting
  either would make the proof layer a second emission layer wearing a different name;
- the emitter pays out per-`ProofSiteId` ordinals from a `Map ProofSiteId Int`, so one proof
  naturally yields several `EmissionSiteId`s;
- **`CPerform` must not be re-assembled into a fresh `CApp`.** `Emit` currently lowers it as
  `cexpr … (CApp t [unit])`, which builds a NEW node and drops the annotation. Both call forms
  delegate to one call-lowering function that takes the annotation as an argument.

Twelve modules take a mechanical change, and that is the argument FOR this shape rather than against
it: the type checker enumerates every consumer, where a parallel array or a hand-maintained mirror
would let one drift silently.

`foldCalls` still has a job — implementing the annotator and checking its totality — but it is never
the medium the id travels in.

**Pinned before the code lands**, in addition to §1's completion conditions:

1. **one `ProofSiteId` → several consecutive `EmissionSiteId`s** for a wildcard/default arm and for
   a duplicated guard, asserted on the emitted events rather than argued from the tree shape;
1b. **one FUNCTION proof → several lifted functions**: a fixture whose wildcard-row RHS is a lambda
   that calls its OWN parameter. Match compilation copies that RHS into every arm and the default,
   so the emitter lifts it several times under different `fn_N` — and every one of those emissions
   must aggregate onto ONE `sourceFn`. This is the row that fails if `ProofSiteId` is ever keyed by
   the lifted name again;
2. **totality over call occurrences**: every `CApp` AND every `CPerform` carries an annotation after
   `annotateOccurrences`, checked by a traversal that fails on an unannotated node;
3. **stack safety at 100k call occurrences** — the annotator uses an explicit work stack, as
   `foldAtoms` does, and is fixture-tested at that width;
4. **bytecode byte-identity**: the bytecode backend's output is unchanged, since `ann = Unit` there;
5. **LLVM byte-identity with instrumentation OFF**: the annotation changes no emitted code;
6. **`Σ keyed bumps == the two `OParam` slots`, per form** — §1's cross-mechanism identity, which
   is what would fail first if `CPerform` were left unannotated.

**Already landed under this amendment** (668/668 compiler unit): the type layer —
`ParamIndex` (produced ONLY by `indexParams`, an enumeration over the parameter list, so an
off-by-one or a negative index is unconstructible), `BindingSite`/`NonParamOrigin`/`originClass`
keeping `BindOrigin` finite, `AnfOccurrenceId` in the middle end, and the opaque
`ProofSiteId`/`EmissionSiteId` with `siteKey`/`proofKey`/`siteLabel`.


## Consequences

- One new drill key family and its reconciliation; no new counter FAMILY, since the existing
  `pv_applyprofile_key` map carries it. The key becomes a MINTED VALUE (`EmissionSiteId`) rather
  than a string assembled at the site, so `Ctx` gains the emitting function's identity and a
  `ParamIndex` producer — a small change with one useful property: a key cannot be built by hand
  anywhere, and the census and the bump cannot drift into two spellings.
- **Both measurement harnesses (`apply-census.sh`, `apply-profile.sh`) now report artifact staleness and record a provenance manifest** (`toolchain_check`,
  wired into `apply-census.sh` and `apply-profile.sh`). That is a behaviour change to tools this
  record does not own, and it is deliberate: the hole it closes was found while preparing this ADR's
  baseline, and leaving it open would have let slice 1 publish a number about a library nobody was
  running. Existing measurements are not invalidated — a stale input is still a valid measurement OF
  THAT INPUT, and ADR-0109/0113 name their snapshots — but new ones must say what they measured.
- The corpus moves again (the drill's own code is compiled), so every figure this record publishes
  names its snapshot and its classifier, and the ADR-0113 §3(c) reconstruction procedure is the
  fallback when an older snapshot is needed.
- The key space is BOUNDED by construction here: one key per static `OParam` site (661 on the
  post-merge corpus at `8450513`; 654 pre-merge), known before the run because the static census produces the same set. That is
  what makes slice 1 safe to run on a hot path — ADR-0108 §4 records that the map must survive
  ~434 M bumps without allocating per bump, which holds when the key set is closed. Any future
  callee-keyed measurement does NOT have this property, which is one reason it is left to §2.
- **The likeliest outcome of the TRACK is a NO**, and the record is written so that a NO is cheap to
  reach and fully attributable — which is what ADR-0113 demonstrated is worth paying for. Slice 1's
  own outcome is not a verdict at all: it is a shortlist, and a shortlist that turns out to be flat
  (no site carrying a meaningful share) is itself the answer that ends the track.

## Alternatives considered

- **Skip the drill and design the specialisation.** Rejected on this track's own evidence, twice
  over: ADR-0108 step 3 showed the site ranking would have aimed the work at the second-largest
  consumer, and ADR-0113 was stopped by numbers that only its own measurement could produce. The
  prior art adds a third: `purs-wasm` specialised without a monomorphism measurement and grew 4×.
- **Rank by sites and start with the largest function.** Rejected — the static and dynamic rankings
  have now inverted at every level this track has looked at, including INSIDE `local-unknown-fn`
  (`capture` 29.91 % of sites / 17.86 % of executions; `param` 4.16 % / 24.85 %).
- **Measure callee identity first and skip the use-site drill.** Rejected on ordering and on risk:
  the callee-keyed measurement has an unbounded key space on a hot path, its identity is a
  process-local address that names no function without a map that does not exist, and it must first
  decide how `Pap` and `ByNeed` participate. Slice 1 is bounded, cheap, and says how many sites the
  expensive one would have to cover — doing it the other way instruments ~661 sites to learn that a
  dozen mattered.
- **Treat run-time monomorphism at the use site as the applicability test.** Rejected, and it was
  this ADR's own first-draft error. The two units are independent in BOTH directions (§Context): a
  monomorphic site can be unspecialisable, and a polymorphic site can be perfectly specialisable per
  caller. A drill built on that equivalence would return confident verdicts about a question it had
  not measured.
- **Use the existing static census by function.** Rejected as sufficient: the census counts SITES,
  and this population's whole difficulty is that its site count (4.16 %) and its execution share
  (24.85 %) disagree by 6×. The census IS used, though — as the authoritative key set that the
  execution counts are outer-joined onto, so a cold site reads as zero instead of vanishing.
- **Treat the 24.85 % as reason enough to act.** Rejected. It is reason enough to MEASURE. What
  fraction of it is addressable depends on monomorphism, which is unmeasured; ADR-0113's 26.1 %
  aggregate resolved to a 5.16 % upper bound once split, and the same collapse is available here.
