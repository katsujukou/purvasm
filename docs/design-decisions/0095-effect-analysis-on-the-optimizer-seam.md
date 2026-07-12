# 0095. Effect analysis on the optimiser seam: purity facts unlock dead-drop of pure neutral calls

- Status: ~~Proposed~~ **Accepted** _(2026-07-12: promoted — maintainer accepted after two review rounds; scope = EffectAnalysis + dead-drop only)_
- Date: 2026-07-12

> **Revision (2026-07-12, review round 1).** Four findings folded in. (P1) The
> consumer slice is narrowed to **dead-drop only**: `may-perform = false`
> licenses *elimination when dead*, not *motion* — a call summary hides the
> mutable-memory reads `pinnedPrim` protects when direct, so sink/publish wait
> for a memory-effect summary extension (§3). (P2) The local-summary API is
> pinned: per-binding, lexically-scoped fact environments — a flat module map
> is wrong because `$q` locals are reused across bindings (§1). (P2) Published
> facts are widened to cover private helpers free-referenced by published
> candidates, the same boundary `foreignSigs` already plugs (§2). (P3) `.pmi`
> persistence is deferred to a follow-up; this record is in-memory only (§2).

> **Revision (2026-07-12, review round 2).** Two implementation disciplines
> pinned. (P1) **Dead-only gating:** a pure `CApp` never enters the ordinary
> `verdict` — that path marks eligible RHSs at single use too, and the mark
> *is* the sink, so routing pure calls through it would reintroduce motion one
> round later. Usage is consulted first; only `usage = Nothing` yields a drop
> mark; at any use count ≥ 1 a pure call stays pinned/ineligible (§3). (P2)
> Per-round own-binding summaries are a **pass-local result of
> `optimizeModule`**, not `LocalFacts` — ADR-0086 reserves `LocalFacts` for
> fixpoint-stable facts (the own foreign leaf shapes stay there); self-pollution
> holds because pass-local facts derive from the current term, never from the
> module's own `BuildSummary` (§2).

## Context

ADR-0034 defined the structural effect analysis (the dual per-value summary — `vsat`
"does saturating this value perform", `ret_vsat` "is the saturated result an
effect-thunk" — plus the DBE predicate) and implemented it **in boot**
(`Middle_end.Passes.Effect_analysis` + `Dbe`). Two of its properties do not carry
over to the Level-2 optimiser seam as-is:

- **It is whole-program.** Boot runs the analysis after `Link.link`, on the linked
  ANF; the `.pvmi` per-export purity propagation ADR-0034 sketched was never
  implemented. The Level-2 seam is per-module and dependency-directed (ADR-0085/0086),
  so facts must travel across modules explicitly.
- **It has one consumer.** Boot uses the facts only for dead-binding elimination.
  The Level-2 NbE inliner (ADR-0089) has a much richer consumer surface — and today
  it runs **without any purity facts at all**.

The cost of that absence is structural and benchmark-wide. ADR-0089 §5 realises
effect soundness by construction: every neutral call (`NApp`) is a **pinned
computation** — sequenced exactly where the input put it, never sunk to its use
site, never dropped even when dead, never eligible for the publish/mark gates.
This is the correct conservative default when "may this call perform?" is
unanswerable, but it over-approximates *every* call whose callee does not fully
reduce: pure residual calls are treated as if they were `Effect` leaves. The
consequences appear at three seam sites:

1. **Dead neutral calls are kept.** `Nbe/Analysis` never marks a pinned
   computation, even a dead one — the module and its unit tests both annotate this
   as "waits for purity facts" (ADR-0089 §5: the `pure ∧ ¬may-perform` class of
   ADR-0034 is exactly what relaxes dead-drop).
2. **Pure single-use calls do not sink.** Only pure primops sink to their use
   site today; a pure call pinned between a binding and its sole use blocks the
   downstream folds sinking exists to feed (case-of-known, projection peeking,
   distribution).
3. **Publish is over-conservative.** A CAF whose body is a saturated call is
   never published as an inline candidate ("would re-execute per use site") even
   when the call provably cannot perform.

Of these, only (1) is a purity-alone opportunity. (2) and (3) *move or
re-execute* the call, which additionally requires knowing that it reads no
mutable memory — more than the ADR-0034 summary tracks (see §3). This record
scopes its consumer to (1); (2)/(3) are named follow-ups.

Meanwhile the **input side of the analysis is already plumbed and unconsumed**.
ADR-0080 reconstructs `ForeignShape { arity, vsat, retVsat }` from source — the
exact leaf-bit triple ADR-0034's model needs — and ADR-0090 threads it through
`BuildEnv`/`LocalFacts`/`BuildSummary` with the stated purpose "for effect
analysis (ADR-0034)". No pass reads it. `Optimizer.purs` likewise documents the
vacant slot: "the Dbe → EffectAnalysis → Specialize pipeline is the optimiser
track's to add" (Specialize has since landed, ADR-0093).

Measurement locates the payoff. The 2026-07-12 opt/no-opt VM instruction ratios
(`benchmarks/out/opt-effect.txt`) put the effect/dictionary-heavy `count-state`
at the bottom (1.801; fib 3.131, json-parse 2.980), and the ADR-0093
investigation attributed part of the residual to bindings the seam cannot
touch because their spines are pinned calls. GER/Impurification — ADR-0034's
deferred second half — also needs these facts before it can ever fire; this
record deliberately does **not** include GER (see "Out of scope").

## Decision

Build the ADR-0034 effect analysis **into the Level-2 optimiser seam** as a
per-module, dependency-directed fact computation, and make the NbE inliner its
first consumer: purity facts enable **dead-drop of dead pure neutral calls**.
Pin relaxation for *motion* (sink to use site, publish widening) is
deliberately outside this slice — it needs memory-effect facts, not just
purity (§3). Boot is not modified (frozen; its whole-program analysis stays
as-is).

### 1. The fact and its computation

- **One summary shape.** A binding's effect summary is the same triple a foreign
  declares: `{ arity, vsat, retVsat }` (reuse `ForeignShape` as the summary type).
  Foreigns are simply the leaf case of the analysis — the first real consumer of
  ADR-0090's channel. The conservative default (`unknown`) is
  `{ arity: 0, vsat: true, retVsat: true }`, mirroring boot.
- **Structural, over the backend-neutral `AnfModule`.** A new
  `Optimizer/EffectAnalysis` module computes per-top-level-binding summaries by
  the ADR-0034 rules: construction/projection pure (I1 — building an `Effect`
  value is pure; the effect fires at the saturating force), exact saturation
  takes the callee's `vsat`, over-application is conservatively `unknown` (a
  two-level summary cannot see deeper), mutation primops (`NewArray`/`SetArray`)
  perform, recursive groups solved by **least fixpoint** per SCC (initialise
  pure, monotone bump, iterate to convergence).
- **Inputs, in order of consultation:** local bindings' own bodies; the module's
  `foreignSigs` (`LocalFacts`, ADR-0090); dependencies' exported summaries and
  foreign shapes (`BuildEnv`); anything else `unknown`.
- **Local summaries are lexically scoped, per top-level binding.** The
  dead-drop sites are local `let`s *inside* a quoted binding — aliases, PAPs,
  and lambdas whose callees are locals — so a flat per-module fact map cannot
  serve them: top-level facts alone cannot classify a local call, and a
  module-wide map keyed by local name is outright wrong because `$q` locals
  are reused across bindings. The analysis therefore exposes, for each
  top-level binding, a lexically-scoped local fact environment (a
  `Map localName ForeignShape` threaded along the same scoping discipline as
  the walk it feeds), and the mark walk (`inlineMarks`) consumes it — in the
  §3 **dead-only branch**, not via `classifyRhs`/`verdict` eligibility —
  either by fusing the effect walk into the mark walk or by handing the
  per-binding map alongside the `CExpr`. Which of the two is an implementation
  choice; the API contract — lexical scoping, per-binding lifetime, never a
  module-flat local map — is pinned here.

### 2. Fact flow across modules and rounds

- **In-memory:** summaries ride `BuildSummary` and fold into the next module's
  `BuildEnv`, exactly as `DictMachinery` and `foreignSigs` already do
  (ADR-0085's topological fold).
- **Where the module's own facts live — two lifetimes, not one.** The own
  *foreign leaf* shapes are fixpoint-stable and stay in `LocalFacts.foreignSigs`
  (ADR-0090), which is all ADR-0086 permits `LocalFacts` to carry. The own
  *binding* summaries are recomputed **every round from the current module
  term** (they change as the term optimises), so they are a **pass-local
  result inside `optimizeModule`** — computed, consumed by that round's NbE,
  and discarded; never stored in `LocalFacts`, never read back from the
  module's own `BuildSummary`. ADR-0086's self-pollution invariant holds
  structurally: pass-local facts derive from the current term, not from any
  summary of it.
- **The published set is exports ∪ candidate-reachable privates.** Export
  summaries alone are not enough: a published inline candidate may freely
  reference *private* top-level helpers, and after a consumer inlines the
  candidate those references are exactly where the facts are needed — with an
  exports-only summary they regress to `unknown` at the site cross-module
  inlining just created. So the module publishes the summaries of its exports
  **plus every own binding free-referenced (transitively) by a published
  candidate** — the same boundary `foreignSigs` already plugs for shapes on
  `BuildSummary`.
- **In-memory only (this record).** `persistedSummary` stays `Nothing` for
  effect facts: the `--opt`-only `.pmi` `Summary` extension (ADR-0084) today
  has an opaque write-out and **no read-back path**, so persisting would mean
  fixing a JSON schema, a decode-failure → `unknown` fallback, and a
  `BuildEnv` restore route — none of which this slice needs, since the
  ADR-0085 topological fold already delivers facts in-memory across a whole
  build. Persistence is a follow-up record (see "Out of scope").
- **Per-round recomputation.** Within `optimizeModule` the analysis runs after
  `DictElim` and before the NbE round it feeds; the driver's outer fixpoint
  (ADR-0087) then re-runs it each round, so precision gained by `Specialize`
  (devirtualised callees) arrives in the following round — ADR-0034's
  "specialise → effect-analyse" ordering, realised iteratively rather than as a
  one-shot pass order.

### 3. The consumer in NbE (this record's scope): dead-drop only

The analysis supplies a callee-facts oracle to the existing classification
sites; no new IR and no new gate machinery. The single consumer in this slice:

- **Dead-drop.** A dead, pure neutral call is dropped **in place** — the
  binding is eliminated, nothing else moves. This inherits ADR-0034's
  **partial-correctness relaxation verbatim**: a dead pure diverge/throw may
  vanish; containment holds because anything `may-perform` (including an
  effectful bottom) is outside the relaxed class. No separate `Dbe` pass:
  the drop lives in NbE's existing dead-code seam (mark/occurrence machinery),
  the same way Simplify was absorbed (ADR-0089 §6).
- **Gating discipline: a dead-only branch, not verdict eligibility.** The
  ordinary `verdict` marks an eligible RHS at single use too — and that mark
  *is* the sink (the marked binding re-materialises at its use site next
  round). So a pure `CApp` must **never** be routed through `classifyRhs`/
  `verdict` eligibility: doing so would reintroduce exactly the motion §3
  forbids, one round later. The classification instead consults **usage
  first**: only `usage = Nothing` (dead) places a mark — the drop mark — and
  at any use count ≥ 1 a pure call remains pinned/ineligible precisely as
  today. The carrier stays the existing `marks` set; a dead binding's mark
  already means "eliminate, nothing re-materialises anywhere", so no new IR
  and no second mark kind are needed.

**Why not sink or publish (motion): `may-perform = false` does not license
moving a call.** The ADR-0034 summary tracks *performing*, not *reading*: a
callee that merely wraps `IndexArray` — `read a = IndexArray a 0` — is
`vsat = false`, yet sinking its call across a `SetArray` on the same array
changes the value read:

```
let x = read a        -- vsat = false: "pure"
let _ = SetArray a 0 1
use x                 -- sinking `read a` here reads the mutated cell
```

A *direct* `IndexArray` is protected by `pinnedPrim` (read-order is one of its
non-effect reasons to pin), but that protection is structurally lost the
moment the read hides inside a call summary. This is why boot consumes `eperf`
for DBE **only**, and why ADR-0089 §5 fixed the EffectAnalysis relaxation as
"dead-drop only". Eliminating a *dead* binding is safe regardless — nothing
observes the un-performed read — but motion (gate-B sinking,
re-materialisation, publish-and-re-execute) is not. Sink and publish widening
therefore wait for a **memory-effect extension** of the summary (e.g. a
`mayReadMutable` bit propagated through calls the same way `vsat` is), a
follow-up slice whose fixture obligations include: a call hiding `IndexArray`
must not move across a `SetArray`.

### 4. Soundness contract

I1–I4 of ADR-0034 are inherited unchanged and remain the obligations of every
consumer. The three load-bearing conservatisms are pinned explicitly:

- an unresolved `bind`/`discard` or an unknown callee is `may-perform`;
- effect polymorphism (dictionary-passing `m`) stays `may-perform` until the
  dictionary is devirtualised — which is precisely what DictElim/Specialize do,
  and why the analysis re-runs per round rather than once;
- purity facts license **elimination when dead, never motion**: no consumer
  may reorder, sink, duplicate, or re-execute a call on the strength of
  `vsat = false` alone — the summary does not track mutable-memory reads
  (§3). Any future consumer that moves code must consume the memory-effect
  extension, not this record's facts.

### 5. Verification

- **Behaviour gates:** `--opt ≡ --no-opt ≡ oracle` (value + stdout order) on the
  existing effect fixtures plus new fixtures per invariant: dead pure call
  dropped / dead effectful call kept, `discard` sequencing (I2), `void`/
  `when`/`unless` (I3), nested `Effect (Effect _)` (I1), recursive effectful
  group not misclassified (SCC fixpoint).
- **No-motion guards (the §3 boundary):** a *dead* call whose callee hides a
  mutable read is dropped (safe — nothing observes the un-performed read),
  while a *live, single-use* call with the same callee stays exactly where the
  input put it, even across an interleaved `SetArray` — asserting that
  `vsat = false` did not leak into gate-B sinking. Both directions are
  structural assertions on the output, not just behaviour.
- **"Fires" tests:** structural assertions that relaxation actually happens
  (a behaviour-only differential cannot detect a pass that silently became a
  no-op — ADR-0034's progress-note discipline).
- **Benchmarks:** VM instruction ratios against the 2026-07-12
  `opt-effect.txt` baseline; self-compile size ratio stays within the ADR-0089
  backstop.

### Out of scope (follow-up records)

- **Sink and publish widening (motion)** — requires the memory-effect
  extension of the summary (`mayReadMutable` or equivalent, propagated through
  calls like `vsat`), with the §3 fixture obligations. The Context's sites (2)
  and (3) are its payoff; this record's facts and plumbing are its substrate.
- **`.pmi` persistence of effect facts** — the ADR-0084 `Summary` extension
  needs a fixed JSON schema, a decode-failure → `unknown` fallback, and a
  `BuildEnv` restore path before facts can round-trip; today `persistedSummary`
  is write-only scaffolding. Not needed while the in-memory fold covers whole
  builds.
- **GER / impurification** — flattening recognised `bindE`/`pureE` trees to
  direct style. It needs this record's facts *plus* a resolution story for the
  structural-foreign visibility gap: `DictElim`'s `intrinsicLift` deliberately
  declines to lift structural foreigns (`Effect.bindE` has no LLVM lowering for
  a bare reference), so `main`-style CAFs still carry a runtime `bind` field
  read even under `--opt`. Whether the answer is NbE's structural-rung unfold at
  saturation, an inline-the-guest-term lift in DictElim, or codegen
  materialisation is its own decision.
- **Precision upgrades**: pure-leaf annotations beyond what `ForeignShape`
  derives (ADR-0034's "ADR B"), relaxing `pinnedPrim` for provably-local
  allocations, exception-aware refinement (ADR-0074's throw-only world keeps
  the current relaxation valid).

## Consequences

- The seam gains its first purity-fact channel; ADR-0090's `foreignSigs`
  plumbing acquires its intended consumer.
- NbE's §5 dead-code conservatism becomes fact-directed instead of universal:
  dead pure residual calls drop. The larger motion levers (sink, publish) get
  their substrate here but land with the memory-effect follow-up — this slice's
  measured payoff is expected to be the smaller share, and that is deliberate:
  the boundary between "safe on purity alone" and "needs memory effects" is
  the record's main content.
- Cross-module effect facts exist in-memory (which boot never had); GER and any
  later effect-directed pass start from these facts instead of re-deriving
  them. Warm-rebuild consumers wait for the persistence follow-up.
- Two more per-round analyses (facts + consultation) add compile time; bounded
  by the same round structure and backstop as ADR-0089.
- DBE-style dead-drop preserves **partial correctness**, not full observational
  equivalence (ADR-0034's accepted relaxation, restated here so no consumer
  builds on a false premise).

## Alternatives considered

- **Port boot's whole-program post-link analysis.** Rejected: the seam is
  per-module by design (ADR-0085/0086); whole-program passes do not scale and
  contradict the dependency-directed architecture the optimiser track is built
  on.
- **A standalone `Dbe` pass before/after NbE (boot's shape).** Rejected: NbE's
  gate already owns occurrence counting and mark/drop; a second pass would
  duplicate that machinery and re-open the pass-ordering questions Simplify's
  absorption closed (ADR-0089 §6). "Dbe" survives as a rule inside NbE, not a
  pass.
- **Compute facts inside NbE's evaluator (fuse analysis into `Sem`).** Rejected
  for the first cut: the analysis is a small structural fold and its SCC
  fixpoint is awkward mid-evaluation; a separate pre-NbE computation keeps the
  evaluator unchanged and the facts independently unit-testable. Fusion remains
  open if per-round cost ever bites.
- **Take sink/publish in this same slice.** Rejected (review finding): the
  summary proves "does not perform", not "reads no mutable memory"; a call
  wrapping `IndexArray` is `vsat = false` yet must not cross a `SetArray`.
  Motion needs a summary extension and its own fixtures; bundling it here
  would ship an unsound relaxation behind a sound-looking fact.
- **Go straight to GER.** Rejected for this record: GER needs these facts
  anyway, and its structural-foreign visibility gap (see "Out of scope") is an
  independent decision; bundling both would couple a measured, low-risk
  relaxation to an unresolved design.
- **Skip the analysis; rely on Specialize/inlining to reduce everything.**
  Rejected: reduction cannot reach calls that stay residual by design (size
  gates, recursion, opaque leaves); without purity facts every such call pins
  its position forever — the status quo the ratios already price.
