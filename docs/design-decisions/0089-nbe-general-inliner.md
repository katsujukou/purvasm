# 0089. The NbE general inliner on the optimiser seam

- Status: Accepted — extended by the **Accepted** [Addendum (2026-07-11)](#addendum-2026-07-11-slice-3--fold-guaranteed-case-of-case-distribution) (slice 3: fold-guaranteed case-of-case)
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

## Addendum (2026-07-11): Slice 3 — fold-guaranteed case-of-case distribution

- Status: ~~Proposed~~ **Accepted** _(2026-07-11: accepted by the maintainer, with the leaf-recursive
  decidability condition of the P1 review fix)_ — promotes one item of §8's named deferrals (the
  case-of-case / branch-distribution entry of the §ref 6 catalogue) into a scoped slice 3; everything
  else in that deferral list (record-ctor unpacking, directives, specialization, the codegen size
  budget) stays deferred.

### Measured motivation

Slices 1–2 (+ the guest-term/value-chain levers) leave one shape on the hot paths. fib's `<` is now
**call-free** but retains

```
let r = LtInt(a, b)
in let s = case r of true -> LT
                     _    -> (case EqInt(a, b) of true -> EQ; _ -> GT)
in case s of LT -> true; _ -> false
```

— an inner branch whose **every arm ends in a known constructor**, scrutinised by an outer case. The
same shape sits in every quicksort filter predicate (`--opt-effect`: fib 1.434, quicksort 1.579).
Distributing the outer case into the inner arms folds each copy on the spot (`LT` decides `true`,
`EQ`/`GT` decide `false`) and boolean-cases then collapse against `LtInt`'s result, finishing the
comparison to a **single primop**.

### Decision

1. **The rewrite, fold-guaranteed only.** A `let r = C in … case r of alts` (and the `CIf` analogue),
   where `C` is a `CCase`/`CIf` computation, distributes `alts` **recursively through `C`'s branch
   tree to its leaves**: an arm whose result is itself a `CCase`/`CIf` is descended into, and the
   condition is that **every *leaf* result of the tree decidably matches `alts`** (a known
   constructor/literal against decidable rows, the §5 discipline) — the motivating fib shape's `_`
   arm is exactly such a nested case (`_ -> case EqInt … of true -> EQ; _ -> GT`), whose *leaves*
   (`EQ`, `GT`) decide even though the arm's top-level result is not itself a value. Each
   distributed copy therefore **folds immediately at its leaf** — the rewrite is a strict shrink,
   never a duplication, so it needs **no new size threshold** (the reference's
   `size <= 128`/`KnownNeutral` guards are subsumed by the stronger fold-guarantee). A leaf whose
   result is not decidable blocks the whole rewrite (v1); an intermediate arm that `let`-binds
   computations before its leaf keeps them (they run on that arm's path exactly as before —
   position preserved); guarded alternatives, both in the tree and in `alts`, block it (ADR-0013
   order, as in §5).

   > **Correction (2026-07-11, review):** the fold-guarantee bounds how many alternatives *survive
   > per leaf* (one), **not their size** — an irrefutable row with a large (or binder-consuming)
   > right-hand side would still be duplicated once per leaf, breaking the strict-shrink claim. The
   > rewrite therefore additionally requires **every outer alternative's right-hand side to be a
   > bare atom** (`Ret (CAtom …)`, size one), which makes each surviving copy a single node and the
   > claim true in fact. The motivating shapes (boolean/literal results) satisfy this; anything
   > larger stays un-distributed pending a size-gated design if traces ever justify one.
2. **Effect-soundness is inherited, not new.** The outer case scrutinises an atom (ANF), so no
   computation sits between the inner branch and the dispatch; the inner branch stays at its
   sequenced position — only the *continuation* moves into its arms, once per arm, and the binder
   `r` must be used **only** as that scrutinee (usage `total == 1`; otherwise the rewrite is
   declined and the shared `let` stands, per gate B).
3. **Mechanics follow the marks pattern.** Discovery on the quoted term (the analysis already walks
   `let`/usage), application by the next `nbeBinding` round — no new machinery class; the concrete
   carrier (a mark set or a dedicated syntactic pre-pass inside the loop) is an implementation
   choice inside `Nbe`, invisible at the seam.
4. **Gates unchanged**: behavioural (`--opt ≡ --no-opt ≡ oracle`), the blow-up fixtures (a new
   fixture pins the fold-guarantee: an inner arm with an *unknown* result must block distribution),
   `.pmi` mode-stability, and the recorded `--opt-effect` ratios — fib and quicksort are the named
   movers.

### Alternatives considered

- **Port the reference's general `DistBranches` rewrites** (distribute applications/accessors/ops
  into branches under `Deref`-operand + `size <= 128` guards). More powerful, but re-opens
  duplication headroom ANF has no join points to recover; the fold-guaranteed subset captures the
  measured shape with zero residual growth. Revisit if traces show non-folding distribution wins.
- **Compile the comparison chains away earlier (match-compilation tricks).** Rejected: ADR-0083
  deliberately keeps `CCase` structured through the optimiser; this is precisely the reduction that
  placement was bought for.

### Progress (2026-07-11): slice 3 landed; one auxiliary decision surfaced

The distribution landed as specified (`Nbe.Distribute`, a syntactic pass on the quoted term inside
`nbeBinding`'s loop; folding delegated to the next round's case-of-known — the decidability check
mirrors `Eval.matchBinder`, and a disagreement costs only the fold, never correctness). The
implementation surfaced one auxiliary decision: **constant-class extern forcing**. In real corefn a
nullary constructor like `Data.Ordering.LT` is a **CAF binding** referenced by name (`AtomVar`), not
a `CCtor` node — so the fold-guarantee check correctly (but uselessly) blocked on every real
comparison tree until `Eval.refOf` learned to force an extern whose entry is constant-class
(`arity: Nothing`, `size ≤ 2`, forcing to a nullary constructor or literal) eagerly to its value.
Reifying such a value per use site duplicates nothing real — a nullary constructor is an unboxed
immediate (ADR-0064). Measured on the **original landing, before the atom-RHS Correction** (outputs
equal on all five benchmarks): fib 1.434 → **2.587**, quicksort 1.579 → **1.760**, json-parse
2.123 → **2.792**. With the Correction applied, the committed baseline is fib **1.951**, quicksort
**1.760** (unaffected), json-parse **2.573** — the gap to the original landing is the evidence
behind the bounded-continuation-duplication extension below.

### Proposed extension (2026-07-11): two residual boolean-case rules

- Status: ~~Proposed~~ **Accepted** _(2026-07-11: accepted by the maintainer)_.

The landed slice leaves fib's comparison as
`case LtInt(a,b) of true -> true; _ -> (let q = EqInt(a,b) in case q of true -> false; _ -> false)`
— two shapes a distribution cannot touch, each a one-clause reduction:

1. **Boolean case-eta.** A case over a (Boolean-valued) scrutinee whose alternatives are exactly the
   identity — `case b of true -> true; <irrefutable> -> false` — reduces to the scrutinee itself
   (and the negated form, `true -> false; <irrefutable> -> true`, to `NotBool(b)`). The scrutinee is
   an ANF atom (pure by construction), and no arm carries bindings, so nothing is dropped or moved.
2. **Constant-arm collapse.** A case whose alternatives all return the **same literal** directly
   (each rhs exactly `Ret <literal>`, no guards, no arm bindings), with an **irrefutable trailing
   row** (so some arm is guaranteed to match — a potentially-stuck match must be preserved),
   reduces to that literal. The scrutinee is an atom, so dropping the dispatch drops no
   computation (any computation feeding it is a separately pinned `let`, untouched).

Both live in `Eval` (`evalCase`'s neutral path — cheap shape checks before building the neutral) or
as `Distribute`-style syntax rules — an implementation choice inside `Nbe`, as with slice 3. On
fib's shape the two compose with the landed distribution to finish the comparison at a single
`LtInt(a, b)`; quicksort's predicates follow the same path. Gates unchanged (behavioural + fixtures
+ `.pmi` mode-stability); a new fixture pins each rule's blocking condition (an arm with bindings /
no irrefutable row).

### Proposed extension (2026-07-11): bounded continuation duplication

- Status: ~~Proposed~~ **Accepted** _(2026-07-11: accepted by the maintainer, with the leaf-independence
  condition of the P2 review fix)_. The atom-RHS guard (the Correction above) is the committed
  baseline; this extension relaxes it as a **separate slice**.

The atom-RHS guard blocked distribution sites that are semantically safe and measurably profitable:
fib's own body case (a 12-node recursive arm duplicated into two leaves) is worth ≈ +0.6 ratio
(measured 1.951 → 2.587 with the guard off). The blocked copies sit on **disjoint branch paths** —
dynamically exclusive, effects executed at most once per run — so the only cost is **static size**,
which is a size-gate question, not a semantics one.

1. **Firing conditions unchanged**: leaf-decidable (recursively, to the tree's leaves), guarded
   alternatives block, `r` single-use as the immediate scrutinee.
2. **Atom right-hand sides stay unconditionally allowed** — that tier remains a *strict shrink*.
3. **Non-atom right-hand sides are allowed under a duplication budget**: the sum over leaves of the
   *selected* outer alternative's quoted-ANF size must satisfy `Σ copiedRhsSize <= 16`. The bound
   starts at 16; raising it (at most to 32) requires a measured justification. This tier is
   honestly a **measured size tradeoff**, not a shrink — the ADR language keeps the two tiers
   distinct.

   Additionally (review, 2026-07-11), every bounded-tier outer alternative must be
   **leaf-independent**: none of its binder variables may occur free in its right-hand side
   (`binderVars ∩ fv(rhs) = ∅` — `BNull`/literal rows trivially; a `BVar`/sub-binder row only if
   the bound name is unused). A binder-consuming row substitutes the **leaf value** into the copied
   RHS on fold — duplication the RHS-size budget does not count (and which `quote` re-materialises
   per occurrence). The measured motivating sites (fib's `true`-literal + wildcard rows) are
   binder-free, so this costs nothing observed. The alternative — budgeting the *residual* size
   after substituting leaf-bound variables (or an upper bound on it) — is the honest general
   design but is deferred until a trace shows a binder-consuming site worth its complexity.
4. **Effect posture is inherited**: a copied right-hand side (including one that `let`-binds
   effectful computations) lands on exactly one branch path per copy and keeps its internal
   sequencing — dynamically nothing runs more often than before.
5. **Fixtures**: a large RHS is blocked (the budget); a small non-atom RHS distributes; an RHS
   containing an effectful `let` is position-preserved per path (no dynamic duplication); a
   **binder-consuming row is blocked** (leaf-independence — the uncounted substitution class). The
   existing atom-tier fixtures stay.

Evidence for the budget's value: fib 1.951 → ≈ 2.59, json-parse 2.573 → ≈ 2.79 (the pre-guard
measurements); quicksort is unaffected either way.

### Progress (2026-07-11): both extensions landed; the Context §1 chain is closed

The boolean-case rules (in `evalCase`, ahead of the neutral) and the bounded tier (in `Distribute`,
`Σ copiedRhsSize <= 16` under leaf-independence) landed together. Composed with the earlier slices
they close the loop this record's Context opened: `Bench.Fib.Main.lessThan` normalises to
`\a b -> LtInt(a, b)` and inlines at its call sites — the ~20–30-instruction dispatch chain of
Context §1 is now literally the single `LtInt`. Measured (outputs equal, `.pmi` mode-stable,
size×/time× gates green — the harness gained min-of-2 compile timing to keep the time gate
outlier-proof): fib **3.131** (68.1% fewer instructions than `--no-opt`), count-state 1.396,
map-fold-array 1.896, quicksort **1.866**, json-parse **2.975**; emitted size ratio 0.92–0.96 (the
optimiser is a net shrink).

### ~~Proposed~~ Accepted extension (2026-07-11): gate-A closedness — free globals do not un-close a body

**Status: Accepted (2026-07-11)**

#### Measured motivation

With the Context-§1 chain closed, the laggard is count-state at **1.396** (every other bench
1.87–3.13). Its converged IR shows why: the whole State-monad step survives as *dictionary-builder
applications* —

```
Bench.CountState.Main.bindStateT = Control.Monad.State.Trans.bindStateT(Data.Identity.monadIdentity)
Bench.CountState.Main.bind      = Bench.CountState.Main.bindStateT.bind
Bench.CountState.Main.pure      = …applicativeStateT(monadIdentity).pure
```

so every loop iteration pays the un-fused `tailRecM`/`bind`/`pure`/`get`/`put` machinery in
`Control.Monad.State.Trans`. These are exactly the parameterized instances `DictElim` cannot touch
(it eliminates *projections from known dictionaries*; a parameterized instance needs the builder
application *instantiated* — inlining's job, the `@inline arity=N` gap of §Context).

Gate A (`size < 16 || (closed && size < 64)`) refuses them because the candidate fact computes

```
closed = Set.isEmpty (fvExpr (Set.fromFoldable ps) body) && Set.isEmpty (cfExpr body)
```

and a sibling builder reference like `applyStateT` is a **qualified `AtomVar`** — it surfaces as a
free variable in the `fvExpr` half (`fvExpr` deliberately reports free `AtomVar`s as reachability
edges, with no local/global classification), not in the `cfExpr` (foreign-key) half. Every
realistic instance builder references its siblings through superclass fields (`bindStateT`'s body
references `applyStateT`, size ≈ 25), so the `closed && < 64` tier is dead code for the very shape
it was written for. (Pinning this matters: an implementation that dropped only the `cfExpr`
conjunct would change nothing for count-state.)

#### Decision

Redefine a candidate's `closed` as **capture-safety only**: no free *local* variables under the
lambda's parameters. Free *globals* (qualified `AtomVar`/`AtomForeign` keys) do not un-close a
body. The exact new predicate:

```
closed = Set.isEmpty (Set.difference (fvExpr (Set.fromFoldable ps) body) globalKeys)
```

- **`globalKeys` — the classifier's source of truth**: the defining module's own top-level keys
  (`LocalFacts.gkeys`, i.e. `declKeys` over all decls) ∪ the dependency closure's keys
  (`BuildEnv.gkeys`) ∪ the intrinsic domain (`intrinsicPrim`/structural resolver keys). This is
  the same set the driver already threads to `dictElimExpr` for its liftability classification;
  `candidatesOf` gains it as a parameter. A free name **not** in this set — a local escape, an
  unresolved or malformed key — keeps `closed = false`: the capture-safety claim stays mechanical,
  never convention-based (no `$q`-namespace heuristics).
- **The `cfExpr` conjunct is dropped**: an `AtomForeign` key is a link-time global by
  construction, and candidate bodies carrying (possibly non-exported) foreign keys is already the
  shipped ADR-0090 discipline (shapes forwarded via `BuildSummary.foreignSigs`).
- **Gate-B `closedParams` (`Analysis.purs`) is deliberately untouched**: this extension refines only
  the gate-A *candidate fact*, so the hole the earlier P2 closed — unconditionally copying a
  large multi-use global wrapper at local use sites — stays closed.

Soundness, by the three concerns the strict check conflated:

1. **Capture** — still excluded: free locals keep `closed = false` unchanged.
2. **Resolvability** — a qualified global key is position-independent: every top-level binding of
   every closure module is a linked gdef, and the system already ships candidate bodies that carry
   (possibly non-exported) foreign keys, with their shapes forwarded via
   `BuildSummary.foreignSigs` (the ADR-0090 integration). Plain globals need no shape forwarding
   at all.
3. **Initialization order** — a consumer imports the candidate's defining module, so any sibling
   global that body references initializes strictly earlier (topological CAF order over the
   transitive closure).

`.pmi` mode-stability (ADR-0084) is untouched — only `.pmo` bodies change. Thresholds stay 16/64;
the existing behavioural, `.pmi`, and size/time gates are the blow-up backstop.

#### Expected effect

`bindStateT(monadIdentity)`, `applicativeStateT(…)`, `monadStateStateT(…)` and — the loop itself —
`monadRecStateT(monadRecIdentity).tailRecM` come under the 64-tier and unfold at the consumer;
projection folding then exposes the raw `\ma f s -> …` bodies to the ordinary β/case machinery, and
the per-iteration `get/put/bind` chain can fuse toward `\s -> Tuple(Loop(i-1), s+1)` modulo
`Identity` wrapping. Measured after implementation; count-state is the target, the other four
benches are the no-regression watch.

#### Alternatives considered

- **Source-level inline directives** (purs-backend-optimizer's `@inline arity=N`): rejected —
  purvasm has no directive channel, and the gate-side fix is general rather than per-callsite.
- **Exported-globals-only filter**: adds no soundness (private globals are linked gdefs, and
  under `--opt` any dep-body change already obligates consumer recompilation — the persisted-summary
  track's invariant, ADR-0084); it would only re-refuse builders whose helpers happen to be private.
- **Raising the open-term 16-bound instead**: touches every candidate shape, not just the
  relocation-safe ones; strictly harder to reason about duplication cost.

#### Progress (2026-07-11): landed; measured; the next wall identified

Implemented as pinned (`candidatesOf`/`nbeEnvOf` gain the `globalKeys` classifier input, threaded
from the driver's existing `lf.gkeys ∪ env.gkeys`; the `cfExpr` conjunct and the chain conjunct are
subsumed). All gates green (outputs equal, `.pmi` 564/564, size×/time× within bounds; emitted size
rose 0.92→0.95 as more bodies travel — well inside the 1.5 gate).

Measured: count-state **1.396 → 1.448**, other benches unchanged (fib 3.131 / mf 1.896 / qs 1.866 /
jp 2.980). The IR shows why the win is partial: `monadStateStateT(monadIdentity)` (a NonRec builder)
now unfolds — that is this extension working — but the core `bindStateT`/`applyStateT`/
`applicativeStateT`/`monadStateT` builders are one CoreFn **Rec group** (mutual superclass-thunk
references), and §5's "recursive decls are never published" bars them before closedness is ever
consulted. Additionally `monadRecStateT` (NonRec) still fails the publish predicate on its
non-pure-value `let` chain. Unblocking parameterized-instance fusion is therefore a separate
follow-up decision — prior art exists (sidenote-0012: purs-backend-optimizer publishes recursive
groups and terminates via `envForGroup`/`addStop` `InlineNever` self-stops) — to be proposed on its
own evidence.

### Proposed extension (2026-07-11): parameterized-instance unfolding — recursive builder groups

**Status: Proposed**

#### Measured motivation

With closedness fixed, count-state (1.448; every other bench ≥ 1.87) still pays the whole StateT
step per iteration, because the builders that matter — `bindStateT`, `applyStateT`,
`applicativeStateT`, `monadStateT` — form one CoreFn **Rec group** (superclass fields are thunks
referencing sibling builders), and §5 pins "recursive decls are never published". That pin's
purpose is termination (a self-recursive function body unfolding inside itself forever); for
mutually-recursive *builder* groups it throws away exactly the parameterized instances the
`@inline arity=N` gap needs. Prior art directly on point (sidenote-0012 §5): the reference
publishes recursive groups and terminates by inserting an `InlineNever` **self-stop for the whole
group** before evaluating an inlined group member (`envForGroup`/`addStop`).

#### Decision (sketch, for review)

1. **Publish Rec-group members that are dictionary-shaped** — a lambda whose body tail is a
   record construction (`\dicts -> let … in { … }`, validated at publish like `shapeOf`'s other
   shapes) — with one new candidate fact: `group :: Set String`, the member's whole Rec-group key
   set (empty for NonRec, which changes nothing for existing candidates).
2. **Representation — a marks-discovered, single-use peephole deferral.** A *saturated*
   application of a grouped entry is, by default, exactly what it is today: a pinned neutral. The
   deferral is a **peephole on the one shape the trigger needs** — in ANF, `builder(dict).field`
   is spelled `let q = CApp builder [dict] in … CAccessor q field` — discovered and applied
   through the engine's existing **marks** pattern: the analysis on the quoted output marks a
   binder `q` whose rhs is a saturated grouped application **and whose use count is exactly one,
   that use being a projection/match scrutinee**; the next evaluation round binds a marked `q` to
   an `SRef` carrying the full spine (the existing under-application carrier extended to
   `nargs == arity`) instead of a pinned `SLet`, so the projection sees the ref, not an opaque
   variable. Single-use is what keeps sharing trivially intact: a **multi-use** grouped
   application is never deferred — it stays one pinned, shared `let` (the same discipline that
   fixed the 0002 diamond), so refusal can never re-materialise it per use site. *The purity
   claim licensing even the single-use move*: a dictionary builder's application constructs a
   record whose fields are values/thunks — deferring it past the `let` reorders nothing
   observable; the record-tail publish shape in (1) is the claim's syntactic anchor. A deferred
   ref whose fold is refused reifies at its **single** occurrence back to the identical
   application — refusal never grows or changes the term, by construction rather than by
   argument. (Fan-out over a multi-use dictionary — folding several projections of one shared
   `q` — is a possible later relaxation, on its own evidence.)
3. **Trigger — at the marked projection, commit only on a group-free residual.** At the
   projection/match that is the marked single use: force the entry's value — evaluated, per the
   prior art, under `externs` with **every group key removed** (`envForGroup`/`addStop`'s
   `InlineNever` analogue: sibling and self references inside the body degrade to neutral global
   calls; re-entry during that evaluation is structurally impossible) — apply the spine, select
   the projected field / matched alternative, and **commit the fold only if the selected
   residual's free names contain no group key** (checked on the selected part only; its size is
   already bounded by the entry's gate). Otherwise leave the deferred ref untouched. Unselected
   superclass-thunk fields die at eval time either way; the group-free commit check is what
   additionally covers a *selected* field that itself references a sibling — projection-onliness
   alone does not.
4. **The CAF-split spelling gets the same treatment one binding out.** ANF also splits the shape
   across top-level bindings (`Main.bindStateT = bindStateT(monadIdentity)`;
   `Main.bind = Main.bindStateT.bind`). A saturated application **whose head is a grouped
   dictionary builder** is therefore itself publishable as a value candidate (`arity: Nothing`,
   body = the application) — sound by the same purity claim, and the lone, shape-anchored
   exception to slice 2's "saturated CAF applications are never published" (whose rationale —
   re-executing an arbitrary init-once computation — is exactly what dictionary construction
   escapes). The consumer's projection then peeks through the alias into the deferred ref and (3)
   applies.
5. Everything else is unchanged: existing gate-A thresholds judge the entry; a self-recursive
   *function* (a 1-member Rec group) either fails the dictionary shape outright or, if it returns
   a record, has its self-reference stopped inside the entry and blocked from folding by the
   group-free commit check — the §5 guarantee enforced semantically rather than by exclusion.

Termination argument, in two parts. **Within one entry evaluation**: the group is absent from the
environment — no re-entry. **Across driver rounds**: a refused site reifies to the identical
application at its single occurrence (no growth, no new sites — by the single-use construction),
and a committed fold contributes a residual verified group-free (no new group sites) — so the set
of group application sites is non-increasing round over round and the module fixpoint's
convergence is preserved. The ratchet is closed by the **group-free commit check**, not by
projection-onliness. The deferral marks ride the existing convergence criterion
(`e' == e && marks' == marks`), so a stably-refused mark does not keep the binding's fixpoint
spinning.

#### Owed with implementation

- **Marks representation** (review note, 2026-07-11): an ordinary inline mark and a grouped
  deferred-ref mark have different application semantics at `Let` (inline-and-drop vs
  bind-as-deferred-ref), so they must not share one `Set String` — keep the carriers internally
  distinct (both still ride the `e' == e && marks' == marks` convergence criterion).
- Fixtures: a two-member mutual builder group (project one field — folds; a selected field that
  itself references a sibling — commit refused, term byte-stable; bare saturated application with
  no consuming projection — never deferred, stays the pinned neutral; **multi-use**
  `let q = builder(d) in { a: q, b: q }` — never deferred, the single shared `let` survives
  byte-stable; the CAF-split alias spelling folds through (4); a self-recursive function spelled
  as a 1-member Rec group — never unfolds inside itself).
- The non-pure-value chain in `monadRecStateT`'s body (a NonRec builder that still fails the
  publish predicate) is explicitly **out of scope** here — separate evidence, separate proposal.
- count-state is the target measurement; the other four benches and compile time× are the
  no-regression watch.
