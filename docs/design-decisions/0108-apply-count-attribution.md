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
  = MissCalleeNotVar        -- the callee atom is a literal / foreign / computed value
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
  arm would be two half-events that could disagree; one event per emitted call, written by the arm
  that emits it, cannot.
- **Counts are over EMISSION OCCURRENCES.** As in ADR-0107 §2, one source call site can be emitted
  more than once (`MatchCompile` duplicates rows into specialised submatrices), so the identity
  below is stated over occurrences, and events are recorded per emitted occurrence.

### 2. The static `MissReason` census, with a per-object accounting gate

A `census apply` command in the existing `census` package (the ADR-0107 instrument, extended —
same driver, same snapshotted-input harness), reporting per object per `MissReason`:
occurrences, plus the direct-call occurrences for the denominator.

**Gate (the identity that makes it self-checking):** for every emitted object, the events must
account for EVERY emitted call, in the form the emitter actually uses. The naive
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
