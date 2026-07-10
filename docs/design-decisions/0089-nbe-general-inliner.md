# 0089. The NbE general inliner on the optimiser seam

- Status: Accepted
- Date: 2026-07-10

## Abstract

Build the Level-2 optimiser's **general inliner** as normalization-by-evaluation over the backend-neutral
ANF (the ADR-0086 seam unit): a per-binding `quote ∘ eval` fixpoint into a semantic domain where a redex
fires **only when its operands are known**, with the inline decision taken **per reference, before
unfolding**, from a cached `complexity`/`usage`/`size` analysis — the purs-backend-optimizer design,
close-read in [sidenote 0012](sidenotes/0012-purs-backend-optimizer-inliner-reference.md), which the
blow-up family ([0029](0029-general-inlining.md) rejection, sidenotes 0001–0005) resolves to. Lands in
two slices: **(1) the module-local engine** — `SemRef` spine-deferral carrier, the two gate sites with
the reference's pinned thresholds, **`CCase`-structured** case-of-known-constructor / projection folding
(a fresh design, not a port of the reference's `Branch` simplifiers — ADR-0083 keeps `CCase` through the
optimiser precisely for this), effect-soundness rules (neutral calls pinned in place, recursive groups
never unfolded), and VM-exact constant folding — absorbing `Simplify` (ADR-0028) and its
intrinsic-saturation; **(2) the cross-module body channel** — dependencies' per-binding
`(analysis, body)` inline candidates ride the **in-memory** `BuildSummary`/`extendSummary` (the ADR-0084
summary's in-memory subset; `persistedSummary` stays `Nothing`, `--opt` keeps recompiling). Slice 1 is
honestly expected to move the benchmark ratios **little** (the measured remaining cost is cross-module);
slice 2 is where `lessThan(ordInt)`-class chains and constructed dictionaries collapse. Blow-up defences
are the reduction-aware gate (primary), `Lazy` sharing in the domain, dedicated regression fixtures for
the known failure shapes, and the codegen-side per-function size budget (backstop, backend-owned,
sidenote 0012 §10). Gated behaviourally (`--opt ≡ --no-opt ≡ oracle`) plus a size/time-assert leg —
never byte-identity.

## Context

### The measured motivation

With `Simplify ∘ DictElim` wired on the seam (2026-07-10), the `--opt-effect` VM instruction ratios are:
fib 1.32, count-state 1.09, map-fold-array 1.70, quicksort 1.53, json-parse 1.93. The `--emit-ir` traces
show module-local optimisation is essentially **done** — `Data.Array.filter`'s loops are pure primop
sequences — and the remaining cost concentrates in exactly two shapes, both out of module-local reach:

1. **Cross-module small wrappers around dispatch.** Every `fib` call and every `quicksort` filter
   predicate runs `Data.Ord.lessThan(ordInt)` — a ~20–30-instruction chain (closure call → `compare`
   dispatch → `ordIntImpl` → `case LT of …`) that should be one `LtInt`. Collapsing it needs the
   *cross-module body* of `lessThan`, inlining of *non-flat* (case-carrying) bodies, and
   *case-of-known-constructor* folding — none of which `Simplify`'s flat-only/parameter-closed gate can
   express, by design (ADR-0028).
2. **Constructed dictionaries.** count-state's `bindStateT(monadIdentity)` (and the
   `(\_ -> superDict) Prim.undefined` nullary-superclass forcing visible in real traces) produce
   dictionaries by *function application*, not as static records — `DictElim`'s recognition cannot reach
   them; only β-reduction with the constructor's body can.

Extending `Simplify` with non-flat inlining under a static size/use threshold is the design
[0029](0029-general-inlining.md) **rejected** and sidenotes 0001–0005 document failing twice (the
`2^depth` diamond duplication class; the recursive-`Generic` size class). The family's standing
conclusion ([general-inliner-study-first], sidenote 0005 §6) is: build the purs-backend-optimizer
mechanism. That study is now done — [sidenote 0012](sidenotes/0012-purs-backend-optimizer-inliner-reference.md)
is the close reading this record builds on; §ref below means its sections.

### What already exists to build on

- The **seam** (ADR-0086/0087): `optimizeModule` is a pure single step over a backend-neutral
  `AnfModule`, iterated to a fixpoint by the driver, with per-round inspection hooks (`--emit-ir` on
  both `run` and `build`). The inliner slots in as a seam pass; nothing about the seam changes.
- **`Simplify`** (ADR-0028 port + the 2026-07-10 sibling-facts/`moduleKnown` and intrinsic-saturation
  extensions) — the copy-propagation / flat-inline / intrinsic-collapse subset the NbE engine subsumes.
- **`DictElim`** + cross-module `DictMachinery` threading (accessors, instances, identity wrappers).
- **`MatchCompile`** placement (ADR-0083): `CCase` stays structured through the optimiser — chosen
  *because* the inliner needs a structured scrutinee (§ref 9.1).
- The **cross-module summary channel** (ADR-0084 accepted; ADR-0086's `BuildSummary`/`extendSummary`
  seam types) — the designed carrier for slice 2's bodies; only its in-memory half is consumed here.

## Decision

### 1. Architecture: `quote ∘ eval` over the seam's ANF; the seam is unchanged

A new optimiser subsystem `MiddleEnd.Optimizer.Nbe` (split per module-responsibility norms:
`.Sem` the semantic domain, `.Analysis`, `.Eval`, `.Quote` — final split at implementation) providing
one seam-facing entry:

```
nbeBinding :: NbeEnv -> Expr -> Expr        -- one binding body, module facts + (slice 2) extern bodies in NbeEnv
```

`optimizeModule` calls it per binding (with module-level facts recomputed per driver round, like
`moduleKnown` today); the seam signature, hooks, and the driver's outer fixpoint are untouched. Inside,
`nbeBinding` runs the reference's *inner* loop — iterate `quote ∘ eval` while the quoted result's
analysis says a discovered rewrite is pending, bounded by a rewrite-fuel cap (the reference's
`rewriteLimit` analogue; exceeding it is a loud crash, not silent truncation — §ref 1). Evaluation is
**module-local by authority**: it may consult module siblings, the compiler-global intrinsic table, and
(slice 2) dependency-published bodies — never callers ([optimizer-modular-not-whole-program]).

### 2. The semantic domain: HOAS with `SemRef` spine deferral

The domain mirrors §ref 2/3, over ANF instead of `BackendSyntax`:

- `Sem*` reducible values — `SemLam` holds a **host function** (HOAS), so β is meta-application;
  purvasm's ANF is already uncurried (`CLam (Array String)`), so `SemLam` is arity-N from the start (no
  curried-spine reconstruction).
- `Neut*` neutrals — stuck variables, applications of unknowns, primops on unknowns, `CCase` on an
  unknown scrutinee.
- **`SemRef`** — the carrier for a reference to a top-level/local binding: it **accumulates the applied
  spine** (args / accessors / primops) and **defers the body's unfolding behind `Lazy`**. Unfolding
  happens only when the spine settles and the gate (§4) approves; `Lazy` sharing guarantees a
  multi-reference body is evaluated at most once (a pinned ADR-0082 defence). This is the structural fix
  for unfold-then-cap (§ref 3, the purs-wasm failure).

Variable representation: the reference uses de Bruijn levels; purvasm keeps **names** — HOAS makes
capture structurally impossible *during evaluation*, and the residual term's safety is carried by a
**pinned fresh-name discipline at `quote`**:

- `quote` **α-renames every binder it reifies** — no binder name from the input, from an unfolded
  sibling/cross-module body, or from a generator upstream survives into the residual term; and
- the supply draws from a **reserved prefix namespace disjoint from every other name producer in the
  pipeline** — distinct from source identifiers (PureScript cannot lex a `$`-initial identifier), from
  `purs`'s own synthesised names (`$__unused`, `…$Dict`), from `Normalize`'s `$a<n>`, and from
  `MatchCompile`'s `$dt<n>`; the concrete prefix is pinned at implementation (e.g. `$q<n>`) and
  documented on the supply.

Together these make collision impossible **by namespace disjointness, not by counter luck**: after
`quote`, every binder is a fresh reserved-prefix name, and the only free names in a residual body are
top-level/foreign keys (dotted or source-lexable — never reserved-prefix). Unique post-quote binders
are also what the analysis keys on. (A level bridge was considered and rejected — §Alternatives.)
`--opt` output must be **deterministic** (counter per binding, no global state leaks), but is not
byte-identical to anything.

### 3. The analysis: `complexity`/`usage`/`size`, cached per node

Port §ref 4 substantially as-is: a per-node cached analysis on the quoted, pass-internal tree
(`ExprSyntax analysis syntax` shape), computed bottom-up —

- `Complexity = Trivial | Deref | KnownSize | NonTrivial` (a lattice, append = max), with the
  reference's assignments (literals `Trivial`; local accessor `Deref`; lambda / non-empty literal
  aggregates `KnownSize`; `Let`/saturated `App`/`CCase`/`CUpdate` `NonTrivial`);
- `Usage` per binder: occurrence `total`, `captured :: None | Branch | Closure` (raised entering a
  lambda / a `CCase` alternative), call arities, `access`/`case` counts;
- `size` node count; a `rewrite` pending flag; a `deps` set (feeds dead-code pruning and slice 2's
  publication).

The pass-internal analysed tree never crosses the seam: `optimizeModule` still consumes and produces
plain `AnfModule`, so backends, hooks, and the driver's structural-`Eq` convergence are untouched.

### 4. The gate: per-reference, reduction-aware, checked **before** unfolding

Two gate sites, as in §ref 5, with the reference's clauses and thresholds (`< 5`, `< 16`, `< 64`,
`≤ 128`) adopted **as pinned starting values** (re-tuned only against the §7 gates):

- **(A) extern/top-level references** (module siblings now; slice-2 cross-module bodies later):
  `shouldInlineExternReference` / `shouldInlineExternApp` / `shouldInlineExternAccessor` — judged on the
  *callee's cached analysis* and the settled spine shape. A `NonTrivial`-and-large binding fails every
  clause and **stays a call — the blow-up body is never built** (the 0001 fix).
- **(B) local `let`** — `shouldInlineLet`: dead → dropped (pure rhs only, §5); `Trivial` → always;
  linear-and-uncaptured → always; the remaining clauses per the reference. A **multi-use, non-reducing,
  non-trivial** binding matches none → kept as one shared `let`, never copied (the 0002 diamond fix).

These are size **thresholds inside the gate**, not an output cap (the 0005 §4 distinction); there is no
post-hoc `normalFormSizeCap`.

### 5. Soundness rules pinned (effects, recursion, guards)

- **A neutral call is pinned in place.** A `Let x = <call whose head did not reduce>` may perform an
  effect when forced (ADR-0023's thunk model): it is **never** duplicated, reordered past another
  neutral call, or dropped-as-dead — even linear. Constructions (`CLam`, records, ctors, literals) move
  freely. This conservatively preserves ADR-0034's I1–I4 without needing purity facts; when
  `EffectAnalysis` lands, its `pure ∧ ¬may-perform` class relaxes dead-drop only (ADR-0034's sanctioned
  relaxation).
- **Recursive groups never unfold.** A `LetRec` member (and a slice-2 recursive extern) carries a
  self-stop while its own group is being evaluated (the reference's `envForGroup`/`addStop`); recursion
  stays a call. This is the carrier lesson of sidenote 0004 §4 (named binding = call = safe).
- **`CCase` folding is a fresh design over the structured tree** (§ref 9.1 — the reference's `Branch`
  simplifiers are a catalogue, not a port source). Slice 1 folds **case-of-known-value**: the scrutinee
  row is decidable against an alternative's binders (known ctor tag / literal / array length / record
  fields), every *earlier* alternative is decidably non-matching, and the selected alternative is
  `Uncond` — then the alt's binders bind the known sub-values and the rhs is evaluated. A `Guarded`
  selected alternative folds only if its guard chain reduces to a literal decision; otherwise the case
  stays (guard order is observable, ADR-0013). Projection folding (`CAccessor` on a known record) and
  the accessor/known-ctor field read fold under the same "known value" discipline. Case-of-case /
  branch distribution / unpacking are **deferred** to a named later slice (§8).
- **Constant folding is VM-exact.** `CPrim` on known literals folds with byte-for-byte boot-VM
  semantics (w32 wrapping, `div`/`mod`-by-zero → 0, ECMAScript `ToInt32`, IEEE `Number` incl. NaN —
  ADR-0007/0008/0041); anything doubtful stays a neutral primop. The existing `Ffi.intrinsicPrim`
  saturation becomes the evaluation-time foreign-lowering rule (§ref 7's table shape, seeded with the
  intrinsic rung; the structural rung is *not* auto-inlined in slice 1).

### 6. `Simplify` is absorbed; `DictElim` stays — permanently, as the directive substitute

Copy-propagation, flat saturated inlining, sibling facts (`moduleKnown`), and intrinsic saturation are
all natural consequences of §2–§5; when the NbE pass reproduces them under the behavioural gate,
`optimizeModule`'s pipeline becomes `fix (DictElim ∘ Nbe)` and **`Simplify` (the module) retires** —
kept only through bring-up as a bisection reference, then deleted, with retirement **gated by the §8
slice-1 test-transfer** (every existing `Simplify` positive/negative test ported as an NbE equivalent
and green first).

**`DictElim` stays a permanent pass, not a bridge artefact.** The reference's heuristic gate alone does
not reliably collapse type-class dispatch — real-world purs-backend-optimizer usage leans on `@inline`
directives, whose accessor forms `InlineProp`/`InlineSpineProp` exist specifically to name
instance-dictionary methods (`dict.method`, §ref 8), and unsaturated method applications are exactly
what `arity=N` directives force through. purvasm has no directive channel (deferred, §8);
**`DictElim` is its deterministic substitute for the dictionary-dispatch class**, and it is stronger
than a gate-driven inline on that class in three ways:

- Its rewrite is an **atom swap** (dispatch devirtualization), never a body copy: size monotonically
  non-increasing, so it needs **no gate** and carries **no blow-up risk** — it fires on every
  statically-known dispatch regardless of the method body's size or use count, where the NbE gate would
  (correctly) refuse to inline a large impl body yet the dispatch itself should still become a direct
  call.
- It collapses **unsaturated** dispatch (`accessor dict`, the floated partial application — e.g.
  `Control.Bind.bind(bindEffect)` → `Effect.bindE`) with no arity directive.
- Its machinery (accessors / instance field maps / identity wrappers) threads cross-module **as compact
  data**, no body publication required — dispatch devirtualizes even for impls slice 2 would never
  publish (large, multi-use), and it lowers what slice 2 must carry.

NbE **complements** it on the one class static recognition cannot reach: **constructed** dictionaries
(`bindStateT(monadIdentity)`, superclass thunk forcing) — β-reduction and projection folding expose the
record, and the fixpoint's next `DictElim`/fold round collapses the now-static dispatch. The two passes
are cooperative inside `fix (DictElim ∘ Nbe)`, not redundant.

(The LLVM backend's private byte-identity **bridge** shares `DictElim`'s implementation and expires at
boot retirement, per ADR-0086's Addendum — what expires is the bridge invocation, never the optimiser
pass.)

### 7. Verification gates (all pre-existing gate styles, no new kinds)

- **Behavioural**: `--opt ≡ --no-opt ≡ oracle` — today the VM leg (`run-benchmarks.sh --opt-effect`
  checks output equality on every run) plus the unit/E2E suites; the LLVM and JS legs join as ADR-0082
  §2 brings them up.
- **Blow-up regression fixtures** (the owed fixtures of [nbe-inliner-prior-art-study]), added as unit
  tests in the same increment as the engine: (i) the 0002 **diamond** (each level referencing the
  previous twice — must stay shared lets, linear size); (ii) a **recursive derived-`Generic`-shaped**
  dictionary DAG (must stay a call at the gate); (iii) a **deep dictionary-accessor chain** (must
  collapse — the positive control); (iv) an **effectful-thunk reordering trap** (must not move).
- **Size/time asserts** (the ADR-0082 third gate): per-module optimised-ANF size ratio vs `--no-opt`
  and wall-clock compile time asserted under a threshold in the E2E/benchmark harness, so a silent
  blow-up the behavioural gates cannot see fails loudly.
- **Instruction ratios recorded** per slice in `--opt-effect` — slice 1 is expected ~flat (stated up
  front, so a flat result is a pass, not a surprise); slice 2 must move fib / quicksort / count-state.

### 8. Slicing

1. **Slice 1 — the module-local engine**: §2–§6 complete, fixtures green, ratios recorded (expected
   ~flat), and **`Simplify` retirement gated explicitly**: every existing `Simplify` unit/IR
   *positive* test (copy-propagation, flat saturated inlining, sibling `moduleKnown` facts, intrinsic
   saturation in both atom spellings, and the scope/shadowing negatives) is **ported as an NbE
   equivalent and kept green** before the module is deleted — retirement is a test-transfer, not a
   judgement call.
2. **Slice 2 — cross-module bodies**: `BuildSummary`'s optimiser component gains the per-binding
   `(analysis, pruned body)` map, selected by a **spine-independent, conservative publish predicate**
   (a size-bounded superset shaped by ADR-0084's selection: inline candidates ∪ dict machinery ∪
   small; large-multi-use-pure dropped). The publish side deliberately does **not** try to precompute
   "gate-passing": the inline gate is use-site/spine-dependent (delayed-arity and per-argument-usage
   clauses), so the producer cannot decide it — **the consumer's gate site (A) always makes the final
   call**, and the predicate errs toward publishing anything a consumer clause could accept (bounded by
   the gate's largest size threshold). `extendSummary` threads it; **in-memory only**:
   `persistedSummary` stays `Nothing` — `--opt` always recompiles (sanctioned by ADR-0084, which owns
   eventual persistence). Self-pollution stays structural (a module's own summary is folded in only
   after it finishes, ADR-0086 §3).
3. **Named deferrals** (explicitly not this record's scope): case-of-case / branch distribution /
   record-ctor unpacking (the §ref 6 catalogue over `CCase`); `@inline`-style directives (§ref 8;
   needs a syntax decision + the CST channel — and the dictionary-dispatch class they chiefly serve in
   the reference is already covered deterministically by `DictElim`, §6, so their residual value is
   forcing *non-dictionary* cases the gate declines); caller-homed specialization (its own track,
   ADR-0082);
   `EffectAnalysis` purity facts; the **codegen-side per-lifted-function size budget** — the named
   backstop for whatever the gate misses (measured super-linear `-O2`, §ref 10) — which is
   backend-track-owned and lands at the LLVM boundary, not in the optimiser.

### 9. Engineering pins

- Deep linear spines (`Let` chains, alt arrays) use `tailRecM`/`foldM` discipline in eval/quote/analysis
  (the standing Level-2 stack-safety pin); tree recursion bounded by control nesting stays ordinary.
- No `Date.now`/randomness; fresh-name counters are per-binding and deterministic.
- Unit tests mirror the module split (`Test.…Optimizer.Nbe.*`), one `describe` per exported function,
  edge cases per the testing convention.

## Consequences

- The optimiser gains the mechanism every prior blow-up post-mortem pointed at, built from a verified
  close reading of the working reference rather than re-derived — with the two purvasm-specific
  divergences (structured `CCase`, names-not-levels) decided here rather than discovered mid-port.
- The measured cost classes get a designed path: slice 2 collapses the `lessThan(ordInt)` chain
  (cross-module body + non-flat inline + known-ctor fold) and constructed dictionaries
  (β on `bindStateT(monadIdentity)` + projection folding), the two shapes holding fib at 1.32 and
  count-state at 1.09.
- `Simplify` retires instead of accreting gate-shaped patches; the optimiser pipeline stays two passes
  (`DictElim ∘ Nbe`) with one analysis.
- The blow-up failure shapes become permanent executable fixtures, not lore.
- Slice 1 alone does not move benchmarks — accepted and stated; the engine is testable without the
  channel, and the channel is meaningless without the engine, so this order has the shortest
  unverifiable stretch.
- Risks named: the inner rewrite fuel is a crash-not-truncate bound (a pathological module fails loudly);
  compile-time cost of per-round analysis is watched by the size/time gate; the codegen size backstop is
  deferred to the backend track and explicitly tracked as such.

## Alternatives considered

- **Extend `Simplify` with size-capped non-flat inlining + known-ctor folding.** The 0029 proposal,
  rejected then and re-rejected now: without a reduction-aware, per-reference gate the duplication and
  size blow-up classes are unfixable by threshold tuning (sidenotes 0002/0004/0005).
- **Compile matches early and port the reference's `Branch` simplifiers.** Rejected by ADR-0083: `CCase`
  stays structured through the optimiser precisely so folding sees constructors; the `Branch` catalogue
  is reused as a checklist only.
- **A de Bruijn `Level` layer over the ANF** (faithful to the reference). Rejected: HOAS evaluation plus
  fresh-name `quote` gives the same capture-freedom without maintaining a parallel index discipline
  through `Normalize`/`MatchCompile`/backends that all speak names.
- **Whole-program NbE** (optimise the linked term). Rejected: violates the modular-optimisation
  principle and ADR-0085's per-module build; boot's whole-program passes are what Level 2 is walking
  away from.
- **Post-hoc output size cap instead of the gate** (purs-wasm's `normalFormSizeCap`). Rejected: the
  0005 lesson — the cap pays the construction cost of the blow-up before discarding it and mis-targets
  the per-declaration granularity; thresholds belong inside the per-reference gate.
- **Slice 2 first (publish bodies for the existing `Simplify`).** Rejected: `Simplify`'s flat-only gate
  cannot consume the bodies that matter (`lessThan` is non-flat), so the channel would ship with no
  measurable consumer and no gate exercising it.
- **Subsume `DictElim` into the NbE pass** (treat dictionary dispatch as ordinary inlining +
  projection). Rejected — and not merely for the bridge coupling: gate-driven inlining is the *wrong
  mechanism* for the dispatch class. The reference itself cannot collapse this class heuristically (its
  users reach for `@inline` directives whose `InlineProp`/`InlineSpineProp` forms name dictionary
  methods, §ref 8); `DictElim`'s gate-free atom swap devirtualizes dispatch without copying bodies,
  covers unsaturated `accessor dict` partials, works on large/multi-use impls the inline gate must
  refuse, and threads cross-module as data. `DictElim` is purvasm's directive substitute for this class
  and stays permanently (§6).
- **An `@inline` directive channel instead of (or before) `DictElim`+NbE.** Rejected for this record:
  directives are user-facing surface (syntax decision, CST wiring, ulib annotation policy) and the
  reference needs them *because* its dispatch elimination is heuristic; purvasm already has the
  deterministic pass. Directives stay a named deferral (§8) for forcing *non-dictionary* cases the gate
  declines.
