# 0108. Apply-count attribution: one classification source, static census, then dynamic counters

- Status: ~~Proposed~~ **Accepted** _(2026-08-08: explicit maintainer Accept after 3 review rounds —
  the classifier's ownership input, the call-form axis on both the static and dynamic sides, the
  exact `pv_apply_entries` reconciliation, and the `CallEvent` sum that makes an invalid
  form/outcome pairing unrepresentable)_
- Date: 2026-08-07
- Deciders: maintainer
- Technical story: the ADR-0105 A/B's runtime finding (native self-host runtime ±0 under rooting
  reduction — the workload is apply-dominated) and ADR-0107's close-out, which sends the perf
  track here

## Context

The self-host build executes **2.67 B `pv_apply` dispatches**, of which **95.8 % are
`entry_exact_fast_hits`**. That pair is easy to misread, and the misreading decides the work:

- 95.8 % exact-fast says the dispatches mostly take the FAVOURABLE shape: a bare `Closure` whose
  arity matches its argument count. It does NOT establish that the dispatcher is cheap or that it
  is not worth optimising — the per-dispatch cost (argument-buffer construction, the boundary, the
  fast-path check itself) is unmeasured, and 4.2 % of 2.67 B is still 112 M slow dispatches. What
  the number rules out is only the hypothesis "the misses dominate".
- The larger and less explored question is why the calls reach `pv_apply` **at all**. A generic
  `CApp`/`CPerform` dispatch is a call the COMPILER could not turn into a direct known-arity call
  (`Emit.directTarget`), and each one costs at least an argument buffer and a boundary that a
  direct call does not. (Not every generic dispatch is of that kind — the unsaturated-`CCtor`
  builder application is generic without any classification behind it, which is why §2 gives it its
  own class rather than folding it in.) Which of the two costs — per-dispatch, or genericity
  itself — dominates is precisely what this ADR measures rather than assumes.

So the question is not "how fast is dispatch" but "**why is this call site generic**", per site,
and "**which of those reasons dominates at run time**". Today nothing in the toolchain can answer
either question:

- `Emit.directTarget :: Env -> Atom -> Int -> Codegen (Maybe FnInfo)` decides directness and
  **throws the reason away** — `Nothing` covers "callee isn't a variable", "unknown key",
  "arity mismatch against the own-module fact", "not in the cross-module surface", and more, all
  spelled the same.
- `PURVASM_STATS` counts dispatches by RUNTIME shape (`closure_exact`, `under_apply`, `over_apply`,
  `pap_dispatch`, `byneed_dispatch`). It cannot attribute a dispatch to a source-level class —
  "the `Run`/`Free` interpreter loop", "a `StateT` bind chain" — because the runtime has no idea
  which guest term it is executing. Re-running the aggregate counters produces the same
  unattributable totals we already have.
- `heap_apply_activations` is an ACTIVATION count, not an allocation count. The GC-side question
  ("what is the apply path allocating, and of what kind") is a different measurement that this ADR
  keeps separate rather than inferring.

ADR-0107's close-out established the method this ADR reuses: **one classification source consumed
by every party, plus a per-object accounting identity that makes the census self-checking**. There,
counting sites in a way that did not match what the emitter emitted produced a plausible, wrong
number (8,364 vs 8,768) that survived a review round. The same failure is available here — a
"reason census" that classifies differently from the emitter would mis-rank the very thing that
picks the next optimisation.

## Decision

Five steps, in this order. Each is a measurement until step 5; **no optimisation is authorised by
this ADR** — the optimisation that the census points at gets its own ADR, sized by the numbers this
one produces.

### 1. `directTarget` returns the reason, and the census consumes the EMITTER's classification

```text
data MissReason
  -- §4 slice 1 (2026-08-15) SPLIT what was `MissCalleeNotVar` into the two non-variable atoms ANF
  -- actually has. The split is total, and the two mean opposite things — see §4.
  = MissCalleeForeign       -- the callee atom is a FOREIGN symbol (a candidate lever: the emitter
                            --   holds `Ctx.foreignArity` for it and does not consult it here)
  | MissCalleeLiteral       -- the callee atom is a LITERAL (a DIAGNOSTIC class, like MissUnknownKey:
                            --   a well-typed program does not apply a literal, so non-zero is a
                            --   compiler-bug report and the census gate fails closed on it)
  | MissLocalUnknownFn      -- a local binding with no `knownFn` fact (a parameter, a capture, …)
  | MissArityLocal          -- a local `knownFn` whose arity ≠ the call's argument count
  | MissUnknownKey          -- neither a local binding nor a known global key (see below: a
                            --   DIAGNOSTIC class — it does not reach a dispatch in a valid build)
  | MissArityOwnModule      -- an own-module `gfns` fact says a different arity
  | MissOwnObjectNotFn      -- a key this OBJECT defines that is not a function (a `Gcaf`)
  | MissDepNoDirectFact     -- a key from outside this object with no published direct-call fact
  | MissArityCrossModule    -- a published `xfns` fact says a different arity

directTarget :: Env -> Atom -> Int -> Codegen (Either MissReason FnInfo)
```

**The own-object/dependency split needs an INPUT the classifier does not have today (pinned).**
`gkeys`/`gfns`/`xfns` cannot distinguish "a `Gcaf` this object defines" from "a dependency's key
with no published direct fact": a known key absent from both `gfns` and `xfns` is simply
unexplained, because `gfns` holds only THIS object's *function* bindings and `gkeys` is
whole-program. The classifier therefore takes the object's **own defined keys** as an explicit
input — the `defined` set the object emitters already have: `moduleLl` computes its own object's
key set and folds it into `gkeys`, and `entryLl` passes the EMPTY set (see the next bullet). It is
threaded into the codegen context. With it: `MissOwnObjectNotFn` = key ∈ `defined` ∧ ∉ `gfns`;
`MissDepNoDirectFact` = key ∉ `defined` ∧ ∉ `xfns`. Two consequences are pinned with it:

- **The unit is the OBJECT, not the source module — and the entry object DEFINES NOTHING.**
  `entryLl` declares and calls the reachable gdefs' `$init`s; it does not define their root globals
  or their code (`externGlobalDecls Set.empty` — everything it references is `external`). So the
  entry object's `defined` is the EMPTY set, not the reachable spine: every global callee there is
  a dependency, `MissOwnObjectNotFn` is unreachable in it by construction, and its rows are
  reported separately rather than summed into the per-module ones. Taking "reachable spine" as its
  ownership — the first draft of this ADR did — would misclassify every unpublished dependency
  function in the program's hottest object as an own-object CAF, i.e. it would corrupt the static
  ranking this ADR exists to produce.
- **`MissDepNoDirectFact` stays ONE reason.** Splitting it into "a dependency's `Gcaf`" and "a
  dependency's unpublished function" needs the `.pmi`'s `ExportKind`, which the emitter does not
  thread today (`xfns` carries only published *functions*). That refinement is OWED and named
  here rather than faked by a guess; the two share a reason until the kind is available.

Normative properties:

- **Total and exclusive — by mirroring the DECISION TREE, not by a priority list.** An earlier
  draft of this ADR pinned "the first applicable reason" in resolution order; that is wrong about
  the code. `directTarget`'s self-call shortcut, when its shape does not match, FALLS THROUGH to
  the local `knownFn` / global lookups and can still resolve to a direct target — so "the self
  shortcut did not apply" is not a terminal outcome and must never be recorded as one (there is
  accordingly no `MissSelfShape`). The refactor is therefore purely structural: every LEAF of the
  existing branch tree becomes either a `Target` or one `TerminalMiss`, and no new control flow is
  introduced. A unit matrix pins one site per leaf, including the fall-through paths.
- **Reasons that point at DIFFERENT levers stay apart.** `MissOwnObjectNotFn` (the lever is
  representation — how CAF-held functions are called) and `MissDepNoDirectFact` (the lever is
  ADR-0077's export surface) were one constructor in the first draft; conflating them would produce
  a ranked list whose top entry names no specific work.
- **`MissUnknownKey` is a DIAGNOSTIC class, excluded from the dynamic counters.** A callee variable
  that is neither a local binding nor a known global does not reach a dispatch in a valid build:
  `Emit.readVar` crashes on it (`unbound variable … (unresolved foreign?)`). It exists in the
  classification so the enumeration is total, and its static count should be ZERO on any object
  that emitted successfully — a non-zero one is a compiler bug report, not a lever. Step 3 does not
  instrument it.
- **The census consumes the emitter's classification, it does not re-run the classifier.**
  `directTarget` is a `Codegen` action over emitter state (`gkeys`/`gfns`/`xfns`/`selfCtx`/`Env`,
  and now `defined`), so an out-of-tree caller could only reproduce it by reproducing that state —
  the drift ADR-0107 §2 forbids. The emitter RECORDS events instead, and the census reads them off a
  real emission.
- **The event carries the CALL FORM as well as the outcome, and the LOWERING ARM records it.**
  `directTarget`'s answer does not determine the form: whether a target becomes a `musttail` or a
  plain direct call, and whether a miss becomes a `pv_apply` or a `pv_tailcall`, is decided AFTER it
  by the `tail`/`inDirect` branch in the lowering. And two classes never reach the classifier at
  all — the unsaturated-`CCtor` builder application and each lifted function's wrapper entry. So the
  event type is closed over ALL of them and is emitted where the code is actually emitted:

  ```text
  data CallEvent
    = DirectNonTail FnInfo      -- guestDirect at a call site
    | DirectMusttail FnInfo     -- musttailWith
    | GenericApply MissReason   -- rtCall RtApply at a non-tail CApp
    | GenericTail MissReason    -- tailcallWith
    | StructuralApply           -- an apply the classifier never saw (the `CCtor` builder)
    | WrapperEntry              -- per lifted FUNCTION, not per call site
  ```

  **The payload is per-constructor, deliberately** (the project's "preserve invariants through the
  type system" rule): a `{ form, outcome }` PAIR would make `DirectNonTail` + a `MissReason`, or
  `GenericApply` + an `FnInfo`, perfectly constructible — states that mean nothing and that the
  paragraph above could only ASSERT do not occur. As a sum, a direct form carries only a target and
  a generic form carries only a reason, so the invalid combinations are unrepresentable and the six
  constructors stand in one-to-one correspondence with §2's six accounting columns.

  `directTarget` remains the ONE source of `MissReason` — it just no longer pretends to be the one
  source of everything a call site is. Recording the outcome at the classifier and the form at the
  arm would be two half-events that could disagree; one event per accounted guest-call occurrence,
  written by the arm that emits it, cannot.
- **Counts are over EMISSION OCCURRENCES.** As in ADR-0107 §2, one source call site can be emitted
  more than once (`MatchCompile` duplicates rows into specialised submatrices), so the identity
  below is stated over occurrences, and events are recorded per emitted occurrence.

### 2. The static `MissReason` census, with a per-object accounting gate

A `census apply` command in the existing `census` package (the ADR-0107 instrument, extended —
same driver, same snapshotted-input harness), reporting per object per `MissReason`:
occurrences, plus the direct-call occurrences for the denominator.

**Gate (the identity that makes it self-checking):** for every emitted object, the events must
account for every GUEST-call occurrence, in the form the emitter actually uses. "Guest call" is the
scope of these six classes and of nothing else: the runtime machinery a lowering also emits
(`pv_root`, `pv_new_str`, `pv_force_if_byneed`, …) is emitted as an LLVM `call` too, and is
deliberately outside this accounting — it belongs to ADR-0105's seam, not to dispatch. The naive
"`generic == Σ MissReason`" is FALSE against today's `Emit`, in both directions:

- a generic call in TAIL position emits a **`pv_tailcall` trampoline store** (`tailcallWith`), not
  a `pv_apply` — so counting only `pv_apply` misses the entire generic-tail class;
- an **unsaturated `CCtor`** applies its synthesised builder closure through `pv_apply` with no
  `directTarget` involvement at all — a `pv_apply` that no `MissReason` will ever explain;
- `guestDirect` appears BOTH at direct call sites AND inside every lifted function's generic
  wrapper entry, so the `.ll`'s direct-call count is not the call-site count.

The classes and their `.ll` counterparts are therefore pinned as six, and every one is reconciled:

| class | emitted by | counted in the `.ll` as |
| --- | --- | --- |
| `direct-nontail` (target) | `guestDirect` at a `CApp` site | `tailcc` call to a `$d` symbol, minus wrapper entries |
| `direct-musttail` (target) | `musttailWith` | `musttail` call |
| `generic-apply` (reason) | `rtCall RtApply` at a non-tail `CApp` | `pv_apply` call |
| `generic-tail` (reason) | `tailcallWith` | `pv_tailcall` store |
| `structural-apply` (no reason) | the unsaturated-`CCtor` builder application — and any other lowering that applies a value the classifier never saw | `pv_apply` call |
| `wrapper` (per FUNCTION) | each lifted function's generic entry | `tailcc` call to its own `$d` |

```text
for every object:
  pv_apply    == generic-apply events + structural-apply events
  pv_tailcall == generic-tail events
  musttail    == direct-musttail events
  guestDirect == direct-nontail events + wrapper entries
```

`structural-apply` is a REPORTED class, not a residual: a lowering that applies a value without
consulting `directTarget` is a real category of generic dispatch (and possibly its own lever), so it
is named and counted rather than absorbed into a reason it does not belong to. A run whose columns
do not balance fails rather than reporting — the ADR-0107 close-out is the standing evidence for
what an unreconciled count is worth.

### 3. DYNAMIC counters, keyed by (call form × reason) — an opt-in instrumented build

Static counts rank code, not execution; a reason appearing at 40 % of sites may carry 2 % of
dispatches. The dynamic axis is therefore **execution counts, on the SAME axes the static census
splits** — a reason alone is not enough:

```text
applyprofile counters:
  generic_apply[MissReason]   -- executions of a `pv_apply` dispatch, by reason
  generic_tail[MissReason]    -- executions of a `pv_tailcall` trampoline store, by reason
  structural_apply            -- executions of an apply the classifier never saw (its own counter,
                               -- no reason axis, because there is no classification to key on)
```

Without the form axis the profile cannot be reconciled against the runtime's own totals
(`pv_apply_entries` vs `pv_tailcall_writes` in `purvasm-stats:v1`) and cannot say whether the
generic-tail class — invisible in `pv_apply` counts entirely — is hot; without the independent
`structural_apply` counter, a hot builder-application would be silently attributed to whatever
reason happened to sit beside it. `MissUnknownKey` gets no counter (see §1: it cannot execute).
The reconciliation is stated:

```text
Σ generic_apply[*] + structural_apply == pv_apply_entries      (EXACT)
Σ generic_tail[*]                     == pv_tailcall_writes    (EXACT)
```

Both are exact equalities, not approximations: `pv_apply_entries` increments **only** in the
exported `pv_apply` ABI entry (`abi.rs` — "the *only* site that counts `pv_apply_entries`"), and the
runtime's internal re-entries into `Heap::apply` (from `pv_settle`, `force`, and other helpers) are
deliberately excluded from it and counted separately as `heap_apply_activations`; there is a
runtime test pinning exactly that distinction. So no "modulo internal re-entries" slack is
warranted, and leaving it in the contract would weaken the gate to the point of admitting a
mis-attribution. If a future ABI producer of generic dispatch appears, it gets its OWN explicit
counter (`unattributed_apply`) and the equality is extended to include it — an accounting gate must
not have an unnamed remainder to absorb surprises into.

- a build knob (`--profile-apply`, or the `PURVASM_*` env form the measurement knobs already use —
  fail-closed, off by default) makes each generic dispatch site bump its `(form, reason)` counter
  before the dispatch;
- the counters are their OWN schema (`purvasm-applyprofile:v1`), emitted on its own line and
  produced only by an instrumented build. `purvasm-stats:v1` is NOT extended: it is the standing
  behavioural-gate contract (`gc_collections >= 1`, the schema check in `l2-native-behavioural.sh`),
  and mixing a measurement-only, build-profile-dependent row into it would make the gate's schema
  depend on how the binary was built. The two blocks are read together and versioned apart;
- the instrumented build is a MEASUREMENT vehicle: it is never the shipped emission, and the
  uninstrumented emission must be byte-identical to today's (checked the way ADR-0104/0105 check
  intentional-divergence: same-corefn pre/post `.ll` comparison).

Pinned: the instrumentation must not itself change dispatch classification (a counter bump is not
a safepoint and must not perturb rooting) — if it cannot be added without perturbation, the
perturbation is measured and reported, not waived.

### 4. Drill into the dominant reason only

Once one or two reasons dominate the dynamic counts, and ONLY then, the census reports those
reasons at site / function granularity (which functions, which call sites, which modules), so the
follow-up ADR is written against named code rather than an aggregate. The other reasons are
recorded and dropped.

**Step 4 plan of record (2026-08-13), against the step-3 result.** The dominant reason is
`callee-not-var` at 433,865,148 executions. It is drilled in slices, each of which must be checked
by an identity before the next is considered — the same discipline that made step 3 trustworthy.

*Slice 1 — split the constructor.* `MissCalleeNotVar` becomes two reasons, mirroring the two
non-variable atoms ANF actually has:

```
callee-not-var == callee-literal + callee-foreign
```

exact on BOTH axes: static sites, and dynamic executions summing to 433,865,148 on the step-3
workload. `callee-literal == 0` is EXPECTED for well-typed input — applying a literal is not a
thing a well-typed program does — but it is measured, not assumed. **A non-zero `callee-literal` is
not a lever and must not be ranked as one: it is an independent compiler-bug candidate** and gets
reported as such, the same treatment `unknown-key` already has.

*Slice 2 — attribute the foreign class.* Assuming it dominates, the question a design decision needs
answered is not "foreign calls are frequent" but:

- **which foreign symbol** is dispatched, and how often;
- **from which caller function** (added only if slice 2's per-symbol picture is split enough that the
  symbol alone does not decide it — and call-site granularity only after that);
- **whether the arity was known and matched**: the emitter carries `Ctx.foreignArity`, so each
  dispatch is classified `known-match` / `known-mismatch` / `unknown`;
- **apply vs tail**, since the two have different lowerings.

`known-match` is the number that decides the ADR: it is the population a direct lowering through
`Ctx.foreignArity` could actually capture. If it dominates, the lever is real and general; if the
mass sits in `unknown`/`known-mismatch`, or in a handful of higher-order combinators or one
provider, then the lever is narrow and a different design follows. Either way the answer comes from
the count, not from the plausibility of the story.

*Mechanism.* The fixed `(form × reason)` slot space cannot express this: slot indices are assigned
at emission time and shared program-wide, while the set of foreign symbols is per-module. The drill
therefore uses a KEYED counter (`pv_applyprofile_key`, a host-side map from an emitted string to a
count) reported on its own line, with the fixed slots kept as the backbone. That gives a
cross-mechanism check on top of the maintainer-pinned identities: Σ keyed foreign counters must
equal the `callee-foreign` slot, two independent mechanisms landing on one integer.

*Completion conditions (pinned).*

1. ~~the sub-classification sums EXACTLY to 433,865,148 on the step-3 workload~~ — **corrected
   2026-08-15, before the measurement was accepted.** This is unsatisfiable in that form, and the
   reason is structural rather than incidental: the corpus IS the compiler, so adding the drill adds
   call sites to the thing being measured. The drill cannot measure a compiler that predates the
   drill. Replaced by two conditions that are exact and that together carry the same assurance:

   a. on the STEP-3 pinned corpus, censused by the split classifier,
      `callee-foreign + callee-literal == 3,655` — the step-3 `callee-not-var` SITE count, to the
      unit. This pins the split as label-only: same corpus, same classifier decisions, new names;
   b. on the corpus actually profiled, `Σ drill keys == the two callee-foreign slots`, to the unit —
      two mechanisms written down different paths agreeing on one integer;

   and any difference between the step-3 and step-4 class totals must be ACCOUNTED FOR, not merely
   noted: one classifier is run over both CoreFn snapshots to show the delta is corpus growth rather
   than classification drift;
2. the whole-program apply/tail identities of §3 still hold, unchanged;
3. uninstrumented emission stays byte-identical (same-CoreFn comparison);
4. the instrumented compiler's emitted `.ll` set still equals the uninstrumented reference's;
5. self-host and fixture attributions are reported separately and never merged;
6. **any optimisation proposal arising from the numbers is a SEPARATE checkpoint.** Step 4 does not
   implement a lowering change, and no such change is designed until the attribution is reviewed.

Granularity is deliberately coarse first — constructor and foreign symbol. Caller and site
subdivision is added only if the dominant target splits enough that it cannot be decided, because a
census that emits 3,655 site rows is a data dump, not an answer.

### 5. Guest-heap allocation census by `Kind` — a SEPARATE axis

`heap_apply_activations` is an ACTIVATION count, not an allocation count. But the deeper trap is
that "what the apply path allocates" is not one kind of thing: the costs live in THREE different
allocators, and only the first is guest-heap `Kind` mass at all.

| axis | what it is | how it is counted |
| --- | --- | --- |
| **guest heap, by `Kind`** | `Closure`/`Pap`/`ByNeed`/`Record`/`Array`/`Str`/… — GC mass, collector pressure | per-`Kind` counters at the allocation sites |
| **host allocations** | `apply_loop`'s argument and continuation buffers are Rust `Vec`s — traffic the GC never sees and `gc_copied_words` never reflects | see the note below: buffer materialisations and reserved bytes, plus capacity GROWTH events |
| **generated stack** | the `.ll`'s `argv` is an `alloca` in the caller's frame — neither guest heap nor host malloc | static: `alloca` sites and their widths, from the emitted IR |

**On the host axis, "construction" is not "allocation" (pinned).** Creating a `Vec` is not a
malloc — an empty one never allocates, a `with_capacity` one allocates once, and a growing one
reallocates on a schedule the standard library owns; counting constructions and calling the number
"allocations" would misstate allocator traffic by an unknown factor. So the host axis reports what
can be counted honestly: **logical buffer materialisations** and **reserved bytes** (the sizes the
apply path asks for), plus **capacity-growth / reallocation events** — the only ones that are
certainly allocator calls. Anything stronger would need an allocator hook, which is a separate
decision and not taken here.

Of the guest-heap kinds, the one the apply path genuinely OWNS is **`Pap`** (built on
under-application). Closures are built by the code that closes over values, not by dispatch; the
buffers are host memory; `argv` is stack. So the split is reported as: total guest-heap `Kind`
profile, `Pap` as the apply-owned guest allocation, host `Vec` count/bytes, and the static `alloca`
inventory — never one number claiming to be "what apply allocates". Reporting only a guest-heap
total would let a large `Record`/`Str` mass hide (or invent) an apply-path allocation problem, and
attributing the host buffers to a `Kind` would invent GC pressure that does not exist.

All of it lives in the step-3 profile schema, never in the dispatch counts.

#### Progress (2026-08-08): steps 1 and 2 — the classifier, the events, and the static census

**Step 1.** `directTarget` returns `Either MissReason FnInfo`, the tree mirrored leaf-for-leaf (no
new control flow, no priority list — the self-call shortcut still falls through). The ownership
input is the object's `defined` key set, threaded through the codegen context: `moduleLl` passes its
own object's keys, `entryLl` passes the EMPTY set. Each lowering arm records a closed `CallEvent`,
and `moduleLlWithEvents`/`entryLlWithEvents` return the events beside the `.ll` of the same
emission. `CallClass` is a TYPE; a class becomes a string once, at the report edge.

**Emission is byte-identical** — same CoreFn closure, pre- and post-change compiler, `diff -r`
clean over all 303 objects — so the events cost nothing but their own bookkeeping.

**Step 2.** `census apply` is `llvmBackend` with ONLY its two artifact producers replaced
(`Driver.moduleEmission`/`entryEmission` — the very functions `lowerModule`/`lowerEntry` take their
`.ir` from). Context, merge, per-module contribution, interface and codegen options are the real
backend's values, so each object is emitted under exactly the options the shipping build uses and
the census reads the events off THAT emission. `tools/apply-census.sh` reconciles per object, and
both axes are gated: the six call forms against the `.ll`, and every generic class against the sum
of its own reasons (with `unknown-key` fail-closed at zero — a form-only gate would pass while the
reason rows, which are the actual ranking, were short or mis-keyed).

**Results (`--opt`, entry `Purvasm.CLI.Native`, 304 objects, all gates exact).**

| call form | count | share of call sites |
| --- | ---: | ---: |
| `generic-apply` | 13,931 | 57.3 % |
| `generic-tail` | 5,080 | 20.9 % |
| `direct-nontail` | 3,492 | 14.4 % |
| `direct-musttail` | 1,729 | 7.1 % |
| `structural-apply` | 98 | 0.4 % |
| `wrapper-entry` | 14,935 | (per FUNCTION, not a call site) |

**78.5 % of call sites are generic.** Why, by reason (share of the 19,011 classified generic sites):

| reason | apply | tail | total | share |
| --- | ---: | ---: | ---: | ---: |
| `local-unknown-fn` | 6,064 | 3,112 | 9,176 | **48.3 %** |
| `callee-not-var` | 3,184 | 465 | 3,649 | 19.2 % |
| `own-object-not-fn` | 1,782 | 979 | 2,761 | 14.5 % |
| `dep-no-direct-fact` | 1,801 | 369 | 2,170 | 11.4 % |
| arity mismatches (3 kinds) | 1,100 | 155 | 1,255 | 6.6 % |
| `unknown-key` | 0 | 0 | 0 | 0 % |

Two readings, both pinned as STATIC (site counts, not executions — step 3 is what ranks by
execution, and a reason at 48 % of sites may carry far more or far less of the dispatches):

- the dominant class by a wide margin is the **higher-order call** — a callee that is a parameter,
  a capture or a `let`-bound value, about which the emitter has no function fact at all;
- `unknown-key` is zero, exactly as §1 predicted for a diagnostic class that `readVar` would have
  crashed on. The prediction was worth making: a non-zero count would have been a compiler bug
  surfacing in a measurement, and the gate now fails closed on it.

**A gotcha the gate caught on its first run, worth recording.** One object of 304 disagreed:
`Backend.LLVM.Root` — the compiler's own emitter — reported one `musttail` more than the events
did. The corpus IS the compiler, so a module that emits LLVM carries the emitted syntax as string
constants: `@.str.29 = … c" = musttail call tailcc i64 @"`. A text search over the `.ll` counted the
compiler's own output syntax as a call. Every needle is now anchored to the two-space instruction
indent. This is the third counting caveat in that harness (after `declare` lines and the
`musttail`/direct double-match), and all three were found by the accounting identity rather than by
reading the script.

**Step 3.** The same classifier now also bumps a counter at run time. An object built with
`PURVASM_PROFILE_APPLY=1` emits one `pv_applyprofile_bump(slot)` per instrumented call site, where
the slot comes from `CallClass.profileSlot` — the single mapping that also names the slots
(`profileSlotNames`), handed to the runtime at start-up so the runtime labels nothing itself. Only
what can execute a dispatch gets a slot: the two generic forms × the executable reasons, plus
`structural-apply` — seven reasons at the time of writing, **eight after §4 split `callee-not-var`**
(17 slots). Direct calls, wrapper entries and `unknown-key` get none — reserving a counter that is
pinned at zero by construction would invite reading it as evidence.

`callee-literal`, added by that split, is the one deliberate exception to that rule: it is also
expected to read zero, but §4 requires it MEASURED rather than assumed, and a class with no counter
cannot be measured. The census gate fail-closes on it instead.

Three properties keep instrumentation from contaminating what it measures, and each is asserted
rather than argued:

- **the shipped path is byte-unchanged.** The profile symbols are declared by `profileDeclarations`,
  emitted only into an instrumented object, so an uninstrumented emission of the whole self-host
  closure is byte-identical to the pre-step-1 baseline (`diff -r`, exit 0). A seam sweep pins both
  directions: every `RtOp` has a declare *somewhere*, and these two have one *only* in the
  instrumented block;
- **classification is unchanged by being counted.** The recorded event stream is identical with
  instrumentation on and off — the bump observes the classifier, it does not participate in it;
- **the program is unchanged by being profiled.** Both new seam rows are `sp = false`, so the
  activation plan (ADR-0105) does not move, and `tools/apply-profile.sh` asserts each instrumented
  binary's output against the uninstrumented one (fixtures: the expected trace; `--selfhost`: the
  emitted `.ll` set);
- **the bump counts dispatches, not intentions.** `noteCall` sits after `evalAtoms`/`argBuffer`,
  immediately before the dispatch instruction — those steps can force a by-need cell or allocate,
  and announcing a dispatch before the work that might not reach it would count something else.
  Between a bump and its dispatch the only permitted instructions are pure loads (the callee's
  re-read from its root, ADR-0105 verify-then-use); a unit test walks the emitted text and asserts
  that the first CALL after each bump is that bump's own dispatch.

The reconciliation is the assurance argument, and it is EXACT — two independently-derived numbers
landing on the same integer, never a tolerance:

```
Σ generic-apply/<reason> + structural-apply == pv_apply_entries
Σ generic-tail/<reason>                     == pv_tailcall_writes
```

The right-hand sides are counters the runtime already kept for its own reasons; the left-hand sides
come from the compiler's classification. A mis-slotted event, a bump on a path with no call, or a
call on a path with no bump all break an equality. Four dispatch-heavy fixtures reconcile on both
axes, with unperturbed stdout:

| fixture | Σ apply | `pv_apply_entries` | Σ tail | `pv_tailcall_writes` |
| --- | ---: | ---: | ---: | ---: |
| `Gate.DictDispatch` | 25,537 | 25,537 | 2,015 | 2,015 |
| `Gate.Mixed` | 22,985 | 22,985 | 2,805 | 2,805 |
| `Gate.GcChurn` | 6,060 | 6,060 | 2,002 | 2,002 |
| `Gate.ByNeedCell` | 27,035 | 27,035 | 5 | 5 |

**The measurement this ADR exists for** — the self-host build, since that is the workload whose
2.67 B dispatches motivated the ADR. It is a checked-in harness leg, not a one-off:

```
tools/apply-profile.sh --selfhost --build-mode opt --work-mode no-opt
```

which snapshots the inputs and then runs four whole-closure legs: a reference emission by the
node-hosted compiler; a build of the compiler ITSELF with `PURVASM_PROFILE_APPLY=1`; that
instrumented compiler compiling the same pinned closure (`Purvasm.CLI.Native`) under a pinned heap;
and finally the STATIC census over that same snapshot.

The fourth leg is what makes the two rankings comparable, and it is not optional. Both harnesses
snapshotting `output/` is NOT the same as both measuring one corpus: they snapshot at different
times, and `output/` holds the compiler's own CoreFn, so any `spago build` in between changes the
program being measured — two such snapshots taken during this work differed in 85 files. The census
is therefore run from `$COREFN`, the very bytes the profiled compiler was built from, and in
`--build-mode`, because the sites that exist in the running binary are the sites of the compiler as
it was BUILT, not of the workload it compiles.

Pinning the CoreFn is only half of it: **the classifier is an input too.** A census that rebuilds
itself (`spago build`) and re-snapshots `output/` would derive its compiler from whatever the tree
holds at that moment — and this harness spends hours in its earlier legs, so "that moment" can be
long after the profiled binary was built. `apply-census.sh` therefore grew a `--toolchain` mode:
the caller hands it an already-pinned `{output, cli, census, ulib}` and nothing is built or copied
again. `apply-profile.sh` builds the toolchain ONCE up front, snapshots it, and every leg —
including the census — runs from that one copy. The census is invoked rather than reimplemented, so
the site numbers still arrive carrying its six-column and reason-axis gates.

Two mode axes are involved and they are not the same axis. `--build-mode` decides which call sites
exist in the running binary — the CORPUS, which must match the census's `--opt` for the
site-vs-execution comparison to be about weights rather than about two different programs.
`--work-mode` decides the execution weights; it is `--no-opt` because a native whole-closure `--opt`
compile is the profile still under the ADR-0104 §2 waiver. The mode moves the ranking a long way, so
the numbers below are always reported with both.

Behaviour-neutrality is asserted for this workload the same way it is for the fixtures, except that
here the program's output IS the emitted `.ll` set: the instrumented compiler must emit exactly what
the uninstrumented reference emitted. Both identities hold exactly at this scale:

```
emitted .ll set == the uninstrumented reference's                    (304 objects)
Σ generic-apply + structural = 626,997,553 + 2,268,285 = 629,265,838 == pv_apply_entries
Σ generic-tail                                         = 123,275,311 == pv_tailcall_writes
```

**752.5 M dispatches in one self-host build**, attributed (the 750.3 M with a `MissReason`; the
remaining 2.3 M are `structural-apply`, which has none):

Sites and executions below come from ONE snapshot — the census leg above — so the two columns
describe the same 19,110 classified call sites, and `exec/site` is a ratio of shares, not a
comparison of two corpora:

| reason | sites | share | executions | **share** | `exec/site` |
| --- | ---: | ---: | ---: | ---: | ---: |
| `callee-not-var` | 3,655 | 19.1 % | 433,865,148 | **57.8 %** | **3.02×** |
| `local-unknown-fn` | 9,229 | 48.3 % | 195,562,878 | 26.1 % | 0.54× |
| `dep-no-direct-fact` | 2,179 | 11.4 % | 59,090,843 | 7.9 % | 0.69× |
| `own-object-not-fn` | 2,770 | 14.5 % | 55,255,483 | 7.4 % | 0.51× |
| `arity-cross-module` | 861 | 4.5 % | 4,216,740 | 0.6 % | 0.12× |
| `arity-local` | 119 | 0.6 % | 1,855,134 | 0.2 % | 0.40× |
| `arity-own-module` | 297 | 1.6 % | 426,638 | 0.1 % | 0.04× |
| `unknown-key` | 0 | 0 % | 0 | 0 % | — |

(The per-form split of the executions: `callee-not-var` 424,601,573 apply + 9,263,575 tail;
`local-unknown-fn` 119,094,012 + 76,468,866; `dep-no-direct-fact` 47,778,582 + 11,312,261;
`own-object-not-fn` 29,124,383 + 26,131,100; the three arity reasons 6,399,003 + 99,509.)

**The two rankings disagree, and that is the finding.** The class that dominates the *code*
(`local-unknown-fn`, 48.3 % of sites) is not the class that dominates the *run*: `callee-not-var`
executes at 3.02× its share of the code, `local-unknown-fn` at 0.54× of its own. Ranking by sites
would have pointed the optimisation work at the second-largest consumer of dispatches — precisely
the failure mode step 3 was ordered to prevent, and the reason §3 pinned the static numbers as
"static" when they were the only ones available.

`arity-*` is worth a separate note: 6.7 % of sites, 0.9 % of executions — every one of its three
reasons is colder than its site count suggests (0.40×, 0.12×, 0.04×). A caller/callee arity
disagreement is real and fixable, but on this workload it is nearly cold.

This says nothing yet about what to DO. `MissCalleeNotVar` is the callee atom being `AtomLit` or
`AtomForeign` (ANF has no third non-variable atom), and a literal cannot be applied in well-typed
code — so the class is, on its face, the foreign-application path, on which the emitter already
carries an arity fact it does not consult (`Ctx.foreignArity`). That is a hypothesis with an obvious
lever, not a result: **step 4's drill must split the constructor and attribute by site/function
before any of it is believed.** The step order exists for this reason — a plausible lever named
from an aggregate is exactly what the ADR-0107 close-out is the standing evidence against.

`heap_apply_activations` (676.1 M) exceeds `pv_apply_entries` (629.3 M) on the same run, as §1
predicted: the excess is internal `Heap::apply` re-entry, which is not an ABI dispatch and is
deliberately outside both identities.

**Scope of this ranking.** It describes ONE workload — the compiler built `--opt` compiling
`Purvasm.CLI.Native` in `--no-opt` — and the harness prints both axes with every run for that
reason. The fixture leg, run for comparison, shows how far a ranking moves with the workload: the
same four fixtures rank `callee-not-var` at 77.1 % in `--opt` and `dep-no-direct-fact` first at
49 % in `--no-opt`. So a percentage from one corpus is not a fact about the compiler, and the ADR's
numbers come from `--selfhost` alone. What generalises is the METHOD, not the shares: sites and
executions are different measurements, and this one is checked against the runtime's own counters.

**Step 3 CLOSED 2026-08-12.** What it establishes, and only this: on one named workload — the
compiler built `--opt`, compiling `Purvasm.CLI.Native` in `--no-opt` — 752.5 M dispatches attribute
to the reasons above, checked against the runtime's own counters by two EXACT identities, with sites
and executions taken from one CoreFn snapshot and one pinned classifier. `callee-not-var` runs at
3.02× its share of the code and is the dominant consumer of dispatches; `local-unknown-fn`, which
dominates the code, runs at 0.54×.

What it does NOT establish: that any of this is cheap to fix, or what the 57.8 % consists of.
`MissCalleeNotVar` is still a two-constructor class (`AtomLit | AtomForeign`) measured as one, and
the "it is the foreign-application path, and `Ctx.foreignArity` is right there" reading remains a
hypothesis with no measurement behind it. Step 4 exists to decompose that number to a granularity
at which a design decision is possible — it is NOT the step that optimises anything.

Reusable by later steps: the instrumented build profile, the `(form × reason)` slot space and its
two identities, `--toolchain` pinning, and the rule that a ranking names its workload.

**Step 4 slice 1+2 measured 2026-08-13** (`tools/apply-profile.sh --selfhost --build-mode opt
--work-mode no-opt`; the drill is `pv_applyprofile_key`, reported on a third schema line
`purvasm-applyprofile-keys:v1`).

*The split.* `callee-literal` is **0** — on both axes, measured rather than assumed. So the class is
the foreign-application path entire, and the earlier reading was right for the right reason only
after being checked.

*The gates that carry this result* (both hardened in review, before the numbers were accepted). The
drill reconciliation is UNCONDITIONAL: an empty key file is Σ = 0 and is compared against the slot
total anyway, because skipping it would make the gate vacuous in exactly the case it exists for —
key emission or the third schema line regressing away entirely, while the slots still count hundreds
of millions of dispatches. `tools/apply-profile.sh --self-test` injects that fault and four others
and asserts the verdict, so the gate's load-bearing property is itself pinned. And the census's
reason gate now fail-closes on `callee-literal` as well as `unknown-key`, matching the contract this
ADR states for both: verified by injection, where the class/reason sums still balance and the gate
fires regardless.

*The cross-mechanism identity holds exactly*: `Σ drill keys == 434,445,743 == the two
callee-foreign slots`. The whole-program identities of §3 are unchanged
(`630,148,432 == pv_apply_entries`, `123,434,822 == pv_tailcall_writes`), the uninstrumented
emission is byte-identical (303/303, same-CoreFn), and the instrumented compiler emitted an `.ll`
set identical to the reference (304 objects).

*The answer.* **100.00 % of foreign dispatches are `known-match`** — 434,445,743 of 434,445,743, at
an arity the emitter already holds and that matches the call. There is no `known-mismatch` mass and
no `arity-unknown` at all. And the population is tiny: **28 distinct keys**, 24 distinct symbols.

| foreign symbol | executions | share |
| --- | ---: | ---: |
| `Purvasm.String.byteAt` | 198,072,908 | 45.6 % |
| `Purvasm.String.unsafeSetByte` | 153,300,082 | 35.3 % |
| `Purvasm.String.compareBytes` | 25,987,303 | 6.0 % |
| `Purvasm.String.appendBulk` | 18,687,915 | 4.3 % |
| `Purvasm.String.byteIndexOf` | 8,835,999 | 2.0 % |
| `Purvasm.String.unsafeNew` | 7,694,809 | 1.8 % |
| `Purvasm.String.byteLength` | 6,707,455 | 1.5 % |
| (17 more, each < 1 %) | 15,159,272 | 3.5 % |

Four symbols are 91.2 %; every one of the top seven is an ADR-0103 string-substrate leaf. By form:
425.2 M apply, 9.3 M tail. Caller-function subdivision was NOT needed — §4 said to add it only if
the symbol alone could not decide, and a distribution this concentrated decides.

**A property of self-measurement, stated rather than smoothed over.** The corpus IS the compiler, so
adding the drill changed it: `callee-foreign` sites went 3,655 → 3,659 and the class total
433,865,148 → 434,445,743 (+0.13 %). The §4 completion condition "sums exactly to 433,865,148" is
therefore unsatisfiable in that literal form — the drill cannot measure a compiler that predates the
drill. What IS exact, and was checked instead:

- on the STEP-3 pinned corpus, censused by the split classifier: `callee-foreign 3,655 +
  callee-literal 0 == 3,655`, the step-3 `callee-not-var` site count, to the unit;
- on THIS corpus, the two mechanisms agree to the unit (434,445,743), and both §3 identities hold.

The +4 sites are the drill's own code being compiled: the same classifier over the step-3 CoreFn
gives 3,655 and over this one 3,659, so the delta is corpus growth, not classification drift.

**What this licenses, and what it does not.** It says the dominant reason is one narrow, fully
arity-known population, which is the shape a `Ctx.foreignArity` direct lowering could capture, and
that four symbols carry most of it. It does NOT say what that lowering should be, what it costs in
the calling convention, or that dispatch removal converts to run time at any particular rate — the
ADR-0107 close-out is the standing evidence that a dispatch count is not a time measurement. That
design is a separate checkpoint, per §4's pinned condition 6, and is not begun here.

## Consequences

- `directTarget`'s type change touches every `CApp` lowering path and is a mechanical, reviewable
  diff; the emission it produces is unchanged (the `Left` cases are exactly today's `Nothing`),
  which is checkable by a same-corefn `.ll` comparison. The classification EVENTS add codegen
  state, so the same comparison must show they do not perturb emission either (they are recorded,
  never rendered).
- The `census` package gains a second command and stops being ADR-0107-specific — the intended
  shape from its introduction.
- The runtime gains a SECOND, independent profile schema (`purvasm-applyprofile:v1`) and an
  instrumented build profile that must be kept out of the shipped path. `purvasm-stats:v1` is
  deliberately NOT touched: it is the behavioural gate's contract, and its schema must not come to
  depend on how a binary was built.
- The output is a RANKED, attributable list of why the self-host build makes 2.67 B generic
  dispatches — with the classes that are NOT `MissReason`s (structural applies, wrapper entries)
  visible rather than folded in. That list — not this ADR — decides what gets optimised:
  caller-homed specialisation, a wider cross-module surface, how CAF-held functions are called, the
  `Run`/`Free` interpreter shape, or something the numbers name that we have not guessed.

## Alternatives considered

- **Re-run `PURVASM_STATS` and reason from the aggregates.** Rejected: the aggregates cannot
  attribute a dispatch to a source class, which is the entire question. They are already known.
- **Sample with a native profiler (perf/Instruments) instead of instrumenting.** Rejected as the
  primary method: a profiler attributes time to `pv_apply` frames, which we already know dominate;
  it cannot say WHY the compiler emitted a generic dispatch. Useful later as a cross-check on the
  dominant reason's cost.
- **Skip the static census; instrument first.** Rejected: without the static census there is no
  denominator and no accounting identity, so a dynamic count cannot be checked against anything —
  the ADR-0107 close-out is the standing evidence for how that goes wrong.
- **Infer the GC-side cost from `heap_apply_activations`.** Rejected explicitly: it counts
  activations, not allocations. Step 5 measures it.
