# 0099. Generalized effect reflection and impurification: an explicit `CPerform` run-marker and canonical `Effect`/`ST` lowering

- Status: ~~Proposed~~ **Accepted** _(2026-07-13: accepted by the maintainer after five review rounds)_
- Date: 2026-07-13

> **Correction (2026-07-13, Slice 2 implementation — reviewer-directed).** Two refinements
> to the pass structure and `unsafePerformEffect`, folded during Slice 2:
>
> 1. **`Impurify` runs at two points, as the same idempotent pass** — `early` and `close`
>    — around the reduction core:
>    ```
>    DictElim → Impurify(early) → EffectAnalysis → NbE → Specialize → Impurify(close)
>             → enforceOuterKinds → summary
>    ```
>    `early` lowers the GER atoms **already exposed** *and* the **known `Effect` dispatch**
>    recognised directly via `DictMachinery` (`Control.Bind.bind Effect.bindEffect m k` →
>    canonical, §6 — the *main* path; waiting for NbE to expose the bare foreign is the
>    auxiliary path, not the primary one). `close` collects any GER atom that NbE's grouped
>    projection / `Specialize` newly exposed *this* round. Because no optimisation runs after
>    `close`, a freshly-produced `CPerform` reaches codegen as a conservative barrier — and
>    **no GER-owned structural foreign can leak to the backend even at `--opt-max-iter 1`**
>    (the earlier plan, relying only on the next round's `early`, left a bare `Effect.bindE`
>    for codegen when the fixpoint was capped at one round). NbE is **not** moved before
>    `Impurify`: `Impurify`'s job is to raise the `CPerform` run-boundary *before* NbE may
>    move or drop the term, so the analysis sees it.
> 2. **`unsafePerformEffect` is a compiler-known effect *eliminator*.** Its GER descriptor
>    `origin` is a distinct **`EffectEliminator`** — a GER-semantic role *separate* from
>    provider form (do not overload one enum) — and `--opt` lowers `unsafePerformEffect e` to
>    `CPerform e`. **Current state (Slice 2): the `ulib` PS shadow (`(unsafeCoerce eff) unit`)
>    is retained** — `Impurify` recognises and lowers the shadow's *use sites*, and the shadow
>    body itself is the correct `--no-opt` fallback. **The ownership flip — replacing the shadow
>    with a `foreign import unsafePerformEffect :: Effect a -> a` + a `resolver` guest term
>    `\e -> e unit` fallback + forced effect facts (`vsat = true, mtouch = true` at exact
>    saturation, since FSR mis-classifies `Effect a -> a` as pure — the argument, not the
>    result, carries the effect) — is deferred to a follow-up** (maintainer decision; see the
>    `unsafePerformEffect` Progress note). The shadow leaking the run representation into
>    PureScript source is the *reason* the flip is wanted, not yet done.
>
> The `GerDescriptor.origin` enum is therefore `StructuralGuest | EffectEliminator`;
> `structuralExclusions` (§3) stays the `StructuralGuest` subset (`pureE` / `bindE`), and
> `EffectEliminator` keys carry no structural-rung exclusion.

> **Progress (2026-07-13): Slice 1 implemented** (semantic scaffolding). `CPerform Atom`
> added to the ANF (`mapAtoms` / `FreeVars` / `Pretty`), `NPerform Sem` to the NbE `Comp`
> domain — `Eval.performSem` β-reduces `perform (\$u -> body)` in place and pins every
> other operand as a neutral `NPerform` (never dissolved to `t unit`), `Quote` reifies it
> back to `CPerform`. The NbE size/usage/audit walks, `EffectAnalysis`'s conservative
> `CPerform` arms (`eperfC`/`mtouchC` = `true`, `vsumC` = `unknownValue`, `sinkableCall`
> = `false`), and both backends' lowering (`CPerform t` → the one-argument unit call,
> tail-aware) are in place. The binding-surface guard is relocated to module scope
> (§4a): `LocalFacts.outerKinds`, captured once from the raw translated ANF in
> `localFactsOf`, is enforced by `enforceOuterKinds` after `Specialize`, before the
> module output and summary derive. No pass yet *produces* a `CPerform` (that starts in
> Slice 2), so the change is a no-op on real programs. **Gates:** 330 unit + 8 E2E green;
> `purs-tidy` clean; the `--opt-effect` gate shows every per-benchmark instruction count,
> ratio, and `size×` **unchanged** (byte-identical to baseline — the corpus produces no
> `CPerform`), `time×` < 4.0; self-compile `size×` 1.122 → 1.123 (the +0.22% both-mode
> instruction bump is the compiler growing by the new source, not an optimisation
> regression). Uncommitted (maintainer does git).
>
> **Slice 1 test coverage** (11 new fixtures, added after review round 1):
> - NbE `CPerform` (4): in-place β on a known unit-lambda, pinned-marker round-trip
>   (never dissolved to `t unit`), neutral sequencing, dead-perform-kept.
> - Bytecode + LLVM lowering (2): the VM lowers `perform t` to the tail-aware one-argument
>   unit call; the LLVM `.ll` of `CPerform t` is byte-identical to the explicit
>   `CApp t [unit]`.
> - `EffectAnalysis` conservative arms (4): `eperfC`/`mtouchC` = `true`, `vsumC` =
>   `unknownValue`, `sinkableCall` = `false`, and the load-bearing `\$u -> perform m`
>   has `vsat = true` (were `eperfC` false here, the dead-drop could delete a live effect).
> - Binding-surface guard §4a (2): the module-level `enforceOuterKinds` re-shares a lambda
>   whose pre-opt kind was pinned non-lambda (an `outerKinds`-override seam test that
>   **isolates the module guard** — mutation-verified to fail if `enforceOuterKinds` is
>   neutered); and the `.pmi` `ExportKind` mode-stability check (a real CAF `let g = \x ->
>   x in g` reduces to a lambda yet stays `Ecaf` under `--opt`, matching the `--no-opt`
>   raw decls — via `classifyDecl`/`gdefKindMap`).

> **Progress (2026-07-13): Slice 2 core implemented** (`pureE` / `bindE` canonical lowering).
> New `Optimizer/Impurify.purs`: the `GerDescriptor` table (`Effect.pureE` / `Effect.bindE`
> `StructuralGuest`, `Effect.Unsafe.unsafePerformEffect` `EffectEliminator`), `ownedKeys` /
> `structuralExclusions` derived from it, and `impurifyExpr machinery` — recognising a bare
> GER foreign at any saturation *and* a known `Effect` dispatch resolved through
> `DictMachinery` (§6, the main path), η-expanding under-applications and splicing a `let`
> for over-applications. Wired at **two points** (Correction): `early` (before
> `EffectAnalysis`/NbE) and `close` (after `Specialize`). `Effect.pureE`/`bindE` leave the
> NbE structural rung (§3, `Nbe.structural` guard) while the link-time `resolver` keeps them
> for `--no-opt`. Bare GER keys in **value positions** (a `CAtom`, an argument, a record /
> array / constructor field, a scrutinee) are η-expanded and (except at a `CAtom`)
> `let`-hoisted — not only applied heads — so no GER-owned structural foreign survives as a
> data reference (review round 1, P1-A); the `$ge` fresh supply seeds above the highest `$ge`
> already in the body (`maxGe`) so a second-point / next-round run cannot capture a prior
> `$ge` binder (P1-B). **Gates:** 342 unit (12 `Impurify` fixtures — pure/bind/unsafePerform
> lowering, dispatch-via-machinery, η-expansion, over-application splice, idempotency,
> non-GER pass-through; bare-`CAtom` / record-field / call-argument value-position hoisting;
> `$ge` non-capture) + 8 E2E green (the ADR-0098 fixture is rewritten as the Slice 2
> end-to-end: a real-artifact `Effect` do-block's `pure`/`bind`/`discard` dispatch GER-lowers
> to canonical `CPerform`, no residual `Effect.bindE`/`pureE`/dispatch); `purs-tidy` clean;
> `--opt-effect` — the 5 micro-benchmarks each drop a uniform **−11 instructions** at `--opt`
> (their `main :: Effect …` entry's glue now GER-lowers), ratios/`size×` otherwise unchanged,
> `time×` < 4.0; **VM self-compile completes** (the compiler is itself `Effect`-heavy, so
> this exercises GER lowering on real `Effect` code — no non-convergence), `size×` 1.126.
>
> **End-to-end verification (real `Effect` program, `examples/effect-ref` — do-block
> `bind`/`discard`, `Ref`, `whileE`, `whenM`).** On the **VM** at `--opt` (default
> `--opt-max-iter` 10): builds and runs correctly (`The final result is 16`), output
> **identical to `--no-opt`** (the `--opt == --no-opt == oracle` differential the ADR
> requires) — GER's use-site lowering is behaviour-preserving and iteration-safe (the
> `close` pass catches use sites regardless of round count). On the **LLVM backend**, the
> P1-A value-position hoisting **resolved the original `Effect.bindE` crash**: the `Effect`
> module — whose exported `bindEffect = { bind: Effect.bindE, … }` dictionary field is now
> η-expanded and hoisted, not a bare foreign — **emits successfully** (mod_37; it previously
> crashed at mod_28). The build now stops later, on a **different** structural foreign,
> `Effect.Ref._new` (a `Ref` combinator GER Slice 2 does not own). That residual reproduces
> **identically at `--no-opt`** (which runs none of GER) and is the **pre-existing
> [effect-track] blocker generalised**: Level-2 LLVM cannot materialise *any* bare structural
> foreign referenced from an exported binding (`Effect.Ref.*`, the loop combinators, …), a
> **backend concern** — not GER's optimiser-seam lowering, not a Slice-2 regression, not
> iteration-related. Fully compiling `Effect` on Level-2 LLVM belongs to the **backend
> team's track** (synthesising structural-foreign gdefs, boot's `runtimeMembers` analogue);
> whatever GER cannot reach as a *use site* (the `Ref` / loop keys land in Slices 4–5) still
> needs that backend materialisation for exported data references. Uncommitted.
>
> **`unsafePerformEffect` finding (Slice 2, deferred sub-decision).** The `EffectEliminator`
> descriptor is in place and **already lowers `unsafePerformEffect e` uses to `CPerform e`**
> on the plain-`AtomVar` spelling (the `ulib` shadow's call sites) at `--opt`, with the shadow
> body (`(unsafeCoerce eff) unit ≡ eff unit ≡ perform eff`) serving as the correct `--no-opt`
> fallback — verified by a unit fixture and self-compile. The reviewer-directed **flip to a
> `foreign import` + `resolver` fallback + forced effect facts** is therefore an *ownership*
> refinement (stop the shadow leaking the run representation into PureScript source), not a
> correctness fix — and it newly requires overriding FSR's shape (it classifies
> `Effect a -> a` as `Opaque`/pure, since the result — not the argument — carries the
> effect) plus a `ulib` rebuild + 3-backend verification. **Maintainer decision (2026-07-13):
> deferred to a follow-up.** Slice 2 keeps the current state — the `EffectEliminator`
> descriptor stays and lowers `unsafePerformEffect` *uses* to `CPerform` at `--opt`, the shadow
> serves `--no-opt`; the `foreign import` flip + forced facts + `resolver` fallback are the
> follow-up.

> **Progress (2026-07-13): effect-hot benchmark + Slice-3 measurement.** The GER track had no
> effect-thunk instrument (the ADR §Verification prerequisite): added `benchmarks/bench-effect-ref`
> (`Bench.EffectRef.Main` — a `forE` loop whose body is `when (even i) (void (Ref.modify (_+i)
> acc))`, spanning `bind`/`discard`/`when`/`void`/`map` + `Ref` + `forE`), registered in the
> `--opt-effect` gate. It builds and runs correctly (VM `--opt`, output `249500` for `n=1000`),
> `--opt` ratio **1.199** (16.6% reduction); the full gate stays green (fib 3.131 / count-state
> 1.801 / effect-ref 1.199 / map-fold 1.896 / quicksort 1.869 / json 2.980; self-compile `size×`
> 1.126; all `time×` < 4.0). **Measurement-driven Slice-3 scoping** (maintainer chose
> measure-first): inspecting `Bench.EffectRef.Main.pmo` at `--opt` shows `bind`/`discard`/`pure`/
> `when`/`unless` **already collapse** (Slice 2 + NbE — `discard` is byte-identical to the `bindE`
> `CPerform` tree; `when`'s `else` is `pureE unit → \$u -> unit`), while **`map`/`apply` do not**:
> `void` survives as a per-use `Effect.functorEffect` `map`-field projection. So Slice 3 reduces to
> the §5(a) `functorEffect.map` / `applyEffect.apply` **direct rewrite** (maintainer scope: map/apply
> only — `discard`/`when`/`unless` need no dedicated handling). The recognition of the `Effect`
> `Functor`/`Apply` instances **without name hardcoding** is non-trivial (the `map` impl is generic
> `liftA1` and its superclass link is a cross-module-opaque local); the design options are analysed
> in [sidenote 0014](sidenotes/0014-ger-map-apply-recognition-design.md), which recommends the
> **Rec-group anchor**, fail-closed (a full structural-ABI match of the five-dict `Effect`
> `Monad`-hierarchy shape, anchored on the `pureE`/`bindE` fields).

> **Progress (2026-07-13): Slice 3 implemented** (`map`/`apply` direct rewrite). `DictMachinery`
> gains `effectFamily :: Set String` (`mergeMachinery` unions it); `localFactsOf` fills the module's
> own set via `Impurify.effectFamilyOf` — the fail-closed structural-ABI validator (sidenote 0014,
> review round 2: exactly five members, the field-set multiset `{map}`/`{apply,_}`/`{pure,_}`/
> `{bind,_}`/`{_,_}`, and the `{pure,_}`/`{bind,_}` members holding `Effect.pureE`/`bindE`
> directly — surplus/deficit/duplicate/missing-anchor all reject). `Impurify.recognize` gains two
> arms gated on `effectFamily`: a `Data.Functor.map`/`Control.Apply.apply` **dispatch**
> (`map functorEffect f m`, seen at `early`) and — since NbE turns that dispatch into a runtime
> `functorEffect.map` **projection** for the group-recursive dict (ADR-0098, never folds), so
> `early`/the dispatch arm never sees it — a `CAccessor dict field` **projection** arm (caught at
> `close`), both emitting `map f m → \$u -> let a = perform m in f a` / `apply mf ma → \$u -> let f
> = perform mf in let a = perform ma in f a`. `discard`/`when`/`unless` are untouched (they already
> collapse via Slice 2, as measured). **Gates:** 354 unit (12 new `Impurify` fixtures — the
> validator's positive + five fail-closed negatives (incl. a two-method `{ apply, map }` shape),
> the map/apply dispatch + projection rewrites (both `map` and `apply` projection), the
> non-Effect-family no-ops) + 9 E2E green (new Slice-3 fixture: real-artifact `map`/`void` GER-
> lower to `CPerform`, no residual `functorEffect` projection); `purs-tidy` clean. **Measured (the
> Slice-3 payoff on `bench-effect-ref`):** the `--opt` ratio rises **1.199 → 1.965** (`--opt`
> instructions 100 217 → 61 134) — `void` no longer projects `functorEffect.map`, it collapses to
> `\m -> \$u -> perform m; unit`; the other benchmarks are unchanged, self-compile `size×` 1.126,
> all `time×` < 4.0; `effect-ref` VM `--opt` output stays correct (`249500`). Uncommitted.

## Context

[ADR-0098](0098-effect-instance-dict-visibility-grouped-projection.md) closed the
last visibility gap on the road to effect optimisation: with the recursive
`Effect` instance dictionary CAF now projectable, `Control.Bind.bind bindEffect m k`
folds to a **recognisable, under-saturated** `Effect.bindE m k` node (and the `pure`
/ `discard` chains to `Effect.pureE` / `Effect.bindE` likewise) instead of stalling
on the dictionary accessor. The combinators are exposed; nothing yet *consumes* them
as effect-sequencing structure.

This ADR proposes **Generalized Effect Reflection (GER)** and **impurification** for
purvasm — the mechanism that turns the exposed `Effect` surface into ordinary ANF
control flow so the general reduction kernel (the NbE inliner, ADR-0089) can collapse
`Effect` the way it already collapses a transparent `State` monad, and so the
`Effect` / `ST` guest terms can retire from the foreign provider (ADR-0094's final
direction).

The prior art is purescript-backend-wasm's generalized effect reflection and
impurification mechanism (referred to as "purs-wasm" below). This ADR adapts its one
load-bearing idea to purvasm and records where purvasm's different starting point
means most of it is deliberately **not** ported.

### Why purvasm's premise differs from purs-wasm's

In purs-wasm, GER **is the correctness foundation**. There, an `Effect` value's
representation depended on its *syntactic position* — a thunk (`$Clo`) in value
position, an eager host call (`RCallForeign`) directly under a `perform` — so effects
did not compose through `void` / `case` / `if`. Two bugs followed (a dropped `void`
effect; a `when` on a runtime boolean trapping on an `illegal cast` from a
representation mismatch), and `--no-opt` could not even *run* `Effect` because the
collapse *was* the lowering. GER re-established one uniform rule: every `Effect` value
is a nullary thunk, independent of position.

purvasm does not start from that hole:

- `Effect a` / `ST r a` are operationally `Unit -> a` thunks in **every** position.
- An effectful native foreign, saturated at its value parameters, **returns** an
  `Effect` thunk; the effect fires only when that thunk is applied to unit
  (`ForeignShape { arity, vsat, retVsat }`, ADR-0090; the FFI purity contract,
  ADR-0096 §3).
- The VM / LLVM entry runs `main :: Effect a` by applying it to unit.
- **`--no-opt` is already a faithful oracle** — dictionaries resolve at link time and
  the guest terms in `Ffi.purs` (`effBind` / `effPure` / `effFor` / …) execute
  structurally. purvasm already has the differential baseline that purs-wasm's
  ADR-0019 Decision 5 lists as *unmet*.

So purvasm must **not** port purs-wasm's host-foreign reflection. `log "a"` already
returns an `Effect` thunk; wrapping it again as `\$u -> perform (log "a")` in value
position would introduce double-thunking and saturation drift against
`ForeignShape`. The one load-bearing idea to borrow is the **`Perform` run-marker
and the discipline of never dissolving it into a plain application before purity has
been consulted** (purs-wasm's `effArities` reflection and its `perform e → App e [unit]`
strip are the parts to leave behind).

purvasm's GER is therefore **not** a pass that rebuilds a correct representation. It
is an optimiser-seam device: give the already-correct thunk representation an
**explicit run boundary**, and lower `Effect` / `ST` glue and structural combinators
into ordinary ANF so NbE and EffectAnalysis can reason about *where* an effect fires.

### The GER-visibility caveat inherited from ADR-0098

ADR-0098's memo pinned the boundary this ADR must honour: the NbE **structural rung**
(`NbeEnv.structural`, the on-demand guest-term extern entries) can still *unfold*
`Effect.bindE` to the `effBind` thunk guest term at `applySem` once it saturates —
the **opposite** of what GER wants, which is to keep `Effect.bindE` / `Effect.pureE`
as recognisable nodes for its own rewrite. ADR-0098 deferred the resolution to this
ADR and named two options: (1, preferred) exclude the GER-owned keys from the
structural rung and hand quoted ANF to GER; or (2) run GER reflection before the
generic structural unfold inside the evaluator.

## Decision

### 1. An explicit run-marker in the ANF: `CPerform`

Extend the backend-neutral ANF (`MiddleEnd/ANF.purs`) with one computation:

```purescript
data CExpr
  = ...
  | CPerform Atom          -- run an Effect/ST thunk: CPerform t ≃ CApp t [unit]
```

The operand is an atom (ANF invariant preserved). Its meaning is exactly a unit
application, but the optimiser keeps it **distinct** from `CApp t [unit]` as a run
marker:

- `CApp` is an ordinary function application.
- `CPerform` is the execution of an `Effect` / `ST` thunk.
- A neutral `CPerform` is an **effect-sequencing barrier**.
- Only a `CPerform` of a *known* thunk (a `\$u -> body` lambda) may β-reduce, and
  only **in place**.

All ANF traversals and seam machinery are updated for the new node: `mapAtoms`,
`ANF/FreeVars`, `ANF/Pretty`, the NbE `size` / `usage` / `Quote` / `Eval`, and
`Specialize`.

**Backend lowering needs no new runtime API.** After the optimiser, `CPerform t`
lowers to the existing unit application `CApp t [unit]` on both the VM and LLVM
backends. The distinction lives **only** on the optimiser seam.

### 2. NbE semantics: `NPerform`, always pinned initially

Add the corresponding neutral to the NbE `Comp` domain (`Nbe/Types.purs`):

```purescript
data Comp
  = ...
  | NPerform Sem
```

Initial-slice reduction rules — deliberately minimal, mirroring purs-wasm's lesson:

- `perform (\$u -> body)` β-reduces to `body` **at that position** (the do-block
  collapse).
- A neutral `perform x` (unknown thunk) stays **pinned** — it is a sequencing
  barrier, treated like the other `pinnedPrim` / `NApp` computations (ADR-0089 §5,
  ADR-0095).
- `perform` is **never** rewritten back to `CApp x [unit]` ahead of purity. This is
  the discipline purs-wasm's *"where can improve"* item 3 is still paying for: once
  the marker dissolves into a plain application, the head-based purity analysis can
  no longer see which higher-order argument gets performed.

**`EffectAnalysis` equations for `CPerform` (mandatory, not deferrable).** `CPerform`
is a new `CExpr`, so `vsumC` / `eperfC` / `mtouchC` / `sinkableCall`
(`Optimizer/EffectAnalysis.purs`) each need an arm for it or the pass will not
compile. The **initial slice pins the fully-conservative arms** — anything looser can
make the `vsat` of a lambda whose body contains a `CPerform` read `false`, and the
existing single-use dead-drop (ADR-0095) would then delete a live effect:

```text
eperfC     (CPerform _) = true            -- performing a thunk always may-perform
mtouchC    (CPerform _) = true            -- and may touch the store
vsumC      (CPerform _) = unknownValue    -- { arity: 0, vsat/retVsat/mtouch/retMtouch: true }
sinkableCall (CPerform _) = false         -- never sink a run point
```

With these, `bindE`'s lowering `\$u -> let x = perform m in …` has `vsat = true` (its
body's `eperfExpr` sees the `CPerform`), so the `Effect` thunk is correctly
may-perform when run, and `pureE`'s `\$u -> a` stays `vsat = false` (running it only
returns `a`).

A **later** precision refinement (out of the initial slice) may treat `CPerform t` as
the exact-saturated `CApp t [unit]` it lowers to and use the one-level shift already
applied to a saturated `CApp` (`vsumC`/`eperfC`/`mtouchC`'s `n == sf.arity` arm:
`eperfC = atomSum(t).vsat`, `vsumC` shifts `retVsat`/`retMtouch` down). That refinement
**must** fall back to the conservative arms above when `t`'s arity is not known to be
exactly 1 — a bare thunk of unknown arity stays `may-perform` / `unknownValue`.

`NPerform` participates in `EffectAnalysis` as a run point, not as a pure value.

### 3. The GER-owned keys leave the NbE structural rung (not the link resolver)

`Ffi.purs`'s `structural` map feeds **two distinct consumers**: (a) `NbeEnv.structural`
— the on-demand unfold entries the `--opt` NbE evaluator uses; and (b) `Ffi.resolver`
— the **link-time** implementation that materialises any bare foreign at link, which
is what `--no-opt` executes (and the fallback for any residual bare foreign at
`--opt`). This ADR touches **only consumer (a)**.

Per ADR-0098's preferred option (1): the keys GER rewrites — `Effect.pureE`,
`Effect.bindE`, and the structural combinators GER lowers (`Effect.forE` /
`foreachE` / `whileE` / `untilE`, and their `Control.Monad.ST.Internal` twins) — are
**removed from `NbeEnv.structural`** so the NbE evaluator can no longer unfold them to
the `Ffi.purs` guest terms and re-hide the very nodes GER wants to match. GER owns
their `--opt` lowering instead.

**Ownership transfer is per-key and atomic with its slice (P1).** The exclusion set
above is the *end state*; it must **not** be applied wholesale ahead of the
implementation, because §5's lowering lands across slices (core glue in Slice 2, the
`forE` / `whileE` combinators in Slice 4, `ST` in Slice 5). Removing a key from
`NbeEnv.structural` *before* `Impurify` owns it would strip both the `--opt` unfold and
leave no GER lowering — the term would fall through to a bare foreign with no
collapse. The rule to pin:

> A key leaves `NbeEnv.structural` **only in the same slice** that lands its
> `Impurify` recognition, η-expansion, and lowering. Until that slice, the key stays
> in the structural rung and behaves exactly as today.

To keep this consistent — and to avoid maintaining the semantic arity in two places
(the η-expansion needs it, and so does the recognition) — a single **`GerDescriptor`**
per key is the source of truth, with four fields:

- **`key`** — the foreign key.
- **`semanticArity`** — the number of **source / FSR exact-saturation arguments the
  canonical GER rewrite needs to fire**. This general phrasing covers both the
  *constructors* that produce an `Effect` thunk and the *eliminator* that consumes
  one: `pureE = 1`, `bindE = 2`, `forE = 3`, `unsafePerformEffect = 1`. It is *not*
  the trailing unit that runs a produced thunk — §5's firing points are `pureE a` /
  `bindE m k` / `forE lo hi f`, before the run unit, and the η-expansion example
  `bare bindE → \m k -> …` fills exactly these `semanticArity` parameters.
  (`unsafePerformEffect` has no trailing unit at all — it already *takes* a thunk — so
  a uniform "subtract one" would be wrong; anchoring the definition to "arguments the
  rewrite needs" makes constructor and eliminator uniform.)
- **`origin`** — the key's **GER-semantic role**, orthogonal to its provider form.
  `StructuralGuest` (a monad-glue combinator with an `Ffi.structural` guest term: `pureE`
  / `bindE` / `forE` / `whileE` / …) or `EffectEliminator` (runs a thunk and yields its
  value: `unsafePerformEffect`). The eliminator's provider form is a follow-up decision
  (currently a `ulib` shadow whose use sites GER lowers; the `foreign import` flip is
  deferred — see the Correction and the `unsafePerformEffect` Progress note); its `origin`
  is `EffectEliminator` regardless.
- **`lowering`** — the canonical GER rewrite (§5) that `Impurify` applies once the key
  is recognised at `semanticArity` (η-expanding an under-saturated occurrence first).
  Keeping it in the descriptor is what makes it the single source of truth — the
  rewrite never re-branches on `key` elsewhere.

Two sets are derived from the descriptors that have landed, and they are **not** the
same set:

- **`ownedKeys` = *all* landed descriptors** — every key `Impurify` recognises and
  lowers.
- **`structuralExclusions` = the landed `StructuralGuest` descriptors only** — the
  keys removed from `NbeEnv.structural`. An `EffectEliminator` descriptor is owned (so it
  is recognised and lowered) but carries no structural-rung exclusion.

So recognition and lowering land together for every owned key; structural-rung
exclusion happens only for the structural-backed subset. `semanticArity` is still
written once (in the descriptor) for both purposes.

**The link-time `resolver` guest terms stay.** They remain the faithful `--no-opt`
implementation (which never runs `Impurify`) and the link-time fallback. This is the
decoupling the P1 review requires: §3's removal is scoped to `NbeEnv.structural`, not
to `Ffi.resolver`. **Full deletion of the guest terms from the provider is *not* in
scope here** — it is a separate later step, and is sound only once the `--no-opt` path
has a replacement (a minimal always-on lowering, or a pure-PS shadow). Until then the
provider retains them; GER only stops NbE from *unfolding* them.

Because consumer (b) still resolves a bare GER key, a residual `Effect.bindE` would
lower correctly on the **VM** (the linker materialises it) but **not** on **LLVM** (no
lowering for a bare structural `AtomVar` — the ADR-0095 blocker). So the `--opt` path
must additionally guarantee that **no bare GER-owned foreign survives `Impurify`** —
which is why §5 η-expands them rather than declining (below).

### 4. The GER / Impurify pass and its place in the pipeline

Introduce a pass (working name `MiddleEnd/Optimizer/Impurify.purs`; GER is the
reflection stage inside it) run inside each `optimizeModule` round in the order:

```text
DictElim  →  Impurify (GER)  →  EffectAnalysis  →  NbE  →  Specialize
```

1. `DictElim` exposes the concrete `Effect` instance dispatch (ADR-0098).
2. `Impurify` rewrites the exposed `Effect` glue and combinators into canonical ANF
   carrying `CPerform` (§5).
3. `EffectAnalysis` classifies terms containing explicit runs.
4. `NbE` β-reduces known thunks in place and keeps neutral `perform`s as barriers.
5. `Specialize` advances the next round's dispatch resolution.

The **outer driver fixpoint** (ADR-0086) carries a round's `Specialize` / `NbE`
output back to the next round's `Impurify`. "Declining safely" is scoped precisely: a
dispatch `Impurify` cannot yet tie to a concrete `Effect` instance (its dictionary not
yet exposed this round) is left **as the higher-level dispatch** — `Control.Bind.bind
<dict> m k` or a still-stuck accessor — which DictElim / NbE / the next round handle.
Declining therefore never leaves a bare GER-owned foreign behind. The bare
`Effect.bindE` / `Effect.pureE` form only appears *after* recognition exposes it, and
at that point `Impurify` **must consume it** (§5, the η-expansion rule) — it is never
declined.

`Impurify` must be **idempotent**:

- a leaf already under a `CPerform` is not re-reflected;
- a canonical thunk is not double-wrapped;
- re-applying the pass to the same module leaves the term unchanged.

Like purs-wasm's `Impurify`, the walk should ultimately be **constant-stack** on
compiler-sized modules (purs-wasm uses a `Trampoline`). **Re-adjudicated at Slice 2
implementation:** the shipped pass is ordinary structural recursion (depth = control-flow
/ `let`-spine nesting), which meets the concrete bar — *no native-stack overflow on the
self-compile corpus* (verified) — but the fully iterative / trampolined hardening is
**owed, deferred** to match the sibling seam passes (`Quote`, `FreeVars`, which are also
ordinary recursion with the same deferral). Promoting it is tracked with theirs, not a
Slice-2 blocker.

### 4a. Binding-surface stability across `Impurify`

`Impurify` changes a top-level binding's **outer shape**: `f = pureE a` is a non-lambda
CAF (`Ret (CApp (AtomForeign "Effect.pureE") [a])`), but its canonical rewrite is a
bare `CLam` (`f = \$u -> a`); likewise a top-level `myLoop = forE lo hi f` becomes
`\$u -> letrec …`. This is exactly the CAF→function transition that
`preserveOuterShape` (`Nbe.purs:96`) exists to suppress — a top-level body that was
not a syntactic lambda must not become one, or the `.pmi` `ExportKind`/hash diverges
between `--opt` and `--no-opt` (ADR-0084 pins the `.pmi` core mode-stable) and the
native consumers' ADR-0077 call facts (**derived pre-optimisation**) mismatch the
emitted object.

But `preserveOuterShape` compares only NbE's **input and output**, and in this
pipeline `Impurify` runs *before* NbE — so by the time NbE sees the binding it is
already a lambda (`Ret (CLam …)`, the guard's "input already a lambda → keep" arm), and
the transition slips through unguarded.

**Decision.** The binding-surface guard is lifted to the **module pipeline level** and
its reference is each top-level body's **pre-optimisation** outer kind (lambda vs
non-lambda — the same body ADR-0077 call facts derive from), enforced on the **final**
optimised output. A non-lambda that ended as a lambda is re-shared as a CAF under the
reserved binder `$q0` (`let $q0 = \… in $q0`) — reusing the existing
`preserveOuterShape` mechanism and its reserved binder (the quote supply starts at
`$q1`, so no collision), just relocated so its reference predates `Impurify` rather
than only NbE.

**Where the reference lives (P2).** `optimizeModule` is a per-round fixpoint — each
round's input is the previous round's output — so "pre-optimisation" must mean the
**original ANF, captured once**, not a per-round input. Build
`outerKinds :: Map String OuterKind` (`OuterKind = Lambda | NonLambda`) **once** from
the **translated ANF handed to `localFactsOf`** (`Optimizer.purs:130`) — i.e. the raw
module *before any optimizer pass*, including before `DictElim`, so the basis matches
the `--no-opt` side and the ADR-0077 call facts derived from that same pre-opt ANF.
(Even though `DictElim` does not itself change an outer kind, anchoring `outerKinds` to
the raw translated term rather than the `DictElim`'d one keeps the invariant
unambiguous.) Hold it in `LocalFacts` — which ADR-0086 reserves for exactly this kind
of **fixpoint-stable** fact (it derives from the invariant original term, so it never
changes across rounds). Comparing against each round's *input* would be wrong: round
2's input is already the Impurified lambda, so the transition would again be invisible;
the once-captured map is the only stable reference.

**When the guard runs (P1).** The shape guard is applied **inside each
`optimizeModule` round, after `Specialize` and before any summary is derived from the
round's output** (`candidatesOf` / `moduleEffects` / … — the `BuildSummary`
`inlines`/`effects` fields). It must **not** be deferred to a post-convergence
re-wrap of the module alone: ADR-0086's discipline is that a round's published summary
and its emitted module derive from the **same** post-final term, and a late re-wrap
would derive `inlines`/`effects` from the bare lambda while the emitted module carried
the `$q0` wrap. Applying the guard every round is safe for convergence because the
`$q0` form is fixpoint-stable (`preserveOuterShape` already round-trips it: the wrap's
input is a non-lambda `let`, so it is not re-wrapped). The `.pmi` `--opt == --no-opt`
`ExportKind`/hash fixture (Slice 1) is the regression guard.

### 5. Canonical rewrites

**Core glue.** (ANF sketch; `$u` is the unit binder.)

```text
pureE a                →  \$u -> a
                          Ret (CLam ["$u"] (Ret (CAtom a)))

bindE m k              →  \$u -> let x  = perform m
                                 let kx = k x
                                 perform kx
                          CLam ["$u"] (Let x (CPerform m)
                                        (Let kx (CApp k [x])
                                          (Ret (CPerform kx))))

unsafePerformEffect e  →  perform e         -- CPerform e directly
```

The continuation application `k x` is let-bound so `CPerform`'s operand stays an atom.
When `k` is a known lambda, the following NbE round β-reduces `k x`, so an ordinary
do-block collapses to a plain `Let` spine.

**Bare / under-applied GER foreigns are η-expanded, never declined** (the P1-B
guarantee, and purs-wasm's `etaPure` / `etaBind` / `etaPerform` device). Since §3
removes these keys from `NbeEnv.structural`, a bare or partially-applied
`Effect.pureE` / `Effect.bindE` / combinator that survived to codegen at `--opt` would
be an un-lowerable structural `AtomVar` on LLVM. So `Impurify` recognises the
GER-owned keys **as foreign atoms wherever they appear** — at any saturation, not only
in a DictElim-produced application — and η-expands an under-saturated occurrence to the
combinator's full parameter list before applying the canonical rewrite (e.g. a bare
`Effect.bindE` → `\m k -> <bindE m k lowering>`; `Effect.bindE m` → `\k -> <…>`).
Over-application is not a hazard: the recognised combinator core is still lowered and
the surplus arguments (the unit that performs it, and any beyond) stay applied to the
result for NbE to reduce. The net invariant, satisfying §6: **no bare GER-owned
structural foreign survives `Impurify` on the `--opt` path.**

**`Functor` / `Apply` accessors (second stage).** purvasm does not have purs-wasm's
correctness hole here — an unknown call is conservatively `may-perform`, so a
discarded `map` is not wrongly DCE'd. The direct rewrite is still valuable: it exposes
the effect force without waiting for dictionary/helper inlining, stabilising the
collapse of `void` / `when` / `unless` / `traverse_` independent of helper size or the
inline gate.

```text
functorEffect.map f m    →  \$u -> let a = perform m in f a
applyEffect.apply mf ma  →  \$u -> let f = perform mf
                                   let a = perform ma
                                   f a
```

These are recognised via the `DictMachinery` identification of the concrete `Effect`
instance and its fields — **not** by hardcoding the literal dictionary names
(purs-wasm keys on `functorEffectKey` / `applyEffectKey`; purvasm already has the
machinery to do better, and it composes with ADR-0098's grouped projection).

**Structural combinators → ordinary control flow.** These lower to plain `LetRec` /
`CIf` ANF in this pass; the backend is too late. `Ffi.purs`'s guest terms are already
a near-complete reference for the desired control flow — the only change is replacing
the unit-application thunk force with `CPerform`.

```text
forE lo hi f    →  \$u -> letrec go i = if i < hi
                              then let _ = perform (f i) in go (i+1)
                              else unit
                          in go lo

whileE cond body →  \$u -> letrec go $u = let keep = perform cond
                              in if keep
                                 then let _ = perform body in go $u
                                 else unit
                          in go $u
```

`foreachE` / `untilE` follow the same shape. The result is handled entirely by
existing `LetRec` / `CIf` / `LtInt` / `AddInt` / tail-call lowering — **no
Effect-specific loop instruction**.

**Arity discipline (ADR-0034 I4, still load-bearing).** `f i` *constructs* an
`Effect` thunk; `perform (f i)` runs it. The uncurried `CLam` parameter count must
match the combinator-side saturation. Getting this wrong yields — in purvasm, unlike a
hard-trapping backend — a residual PAP or a silent wrong result, so it is a
first-class verification target (§8).

### 6. Consumption is direct — `DictElim` is not widened

`DictElim.intrinsicLift` deliberately declines structural foreigns (a bare
`Effect.bindE` `AtomVar` has no LLVM lowering and no guarantee a later pass consumes
it — DictElim.purs:191–194). **This ADR does not change that.** Widening
`intrinsicLift` to the whole structural resolver would break the backend seam.

Instead, `Impurify` consults `DictMachinery` itself and converts a **known** `Effect`
dispatch **directly to canonical ANF**, never emitting a transient bare
`AtomVar "Effect.bindE"`:

```text
Control.Bind.bind Effect.bindEffect m k   →   canonical bind/perform tree (§5)
```

So no residual structural foreign leaks into the pass output. (An alternative — a
richer `ForeignLift` policy keyed on "this residual arity is guaranteed consumed by
the next pass" — is rejected: it complicates the current `String -> Boolean` contract,
and having `Impurify` consume the dispatch directly keeps the responsibility clear.)

### 7. Self-tail `perform` fusion

A tail-recursive `Effect` loop of the general shape

```text
let t = self args
perform t
```

lowers naïvely to two calls (build the thunk via `self`, then run it), which can lose
tail recursion. Following purs-wasm's item-3 recommendation, the fix is **not** a
general purity strip of `Perform`; it is recognising the explicit self-tail shape of
the *same recursive group* directly:

```text
let t = self args
perform t          →   self (args <> [unit])
```

Gated on **all** of: tail position; callee is the current `self` / same recursive
group; arity matches the perform-unit convention; and no other observable computation
sits between the thunk construction and its run. Every other `CPerform` stays a marker.

### 8. Relationship to `mayReadMutable` (ADR-0096)

GER and `mayReadMutable` answer **different** questions and do not conflict:

- `mayReadMutable`: may a call be moved / duplicated / re-executed?
- GER / `CPerform`: *where* is an `Effect` / `ST` thunk executed?

GER does not license sinking a general call without `mayReadMutable`; conversely
`mayReadMutable` cannot express *which* higher-order argument (`void m`, `map f m`,
`when b m`) is performed — the callee head's effect summary does not carry it. GER
exposes that as structure. In the initial slice `CPerform` is **always pinned** and
only same-position β-reduction of a known lambda is allowed; any future motion of a
*pure* run must demand a strong `mayReadMutable`-bearing summary.

### 9. `ST`

`ST r a` is operationally the same unit thunk, so it shares `CPerform` and the same
canonical rewrites (`pure_` / `bind_` / `run` / `while` / `for` / `foreach` / `new` /
`read` / `write` / `modifyImpl`, and the `STFnN` adapters). `ST`'s region-local
mutation and its pure `run` boundary are **not** exploited for motion / elimination in
this ADR — `ST` lands as the same conservative barrier as `Effect`; region-local
memory optimisation is a separate decision (see the mutation/ownership research track).

## Implementation slices

### Slice 1 — explicit force marker (semantic scaffolding)

- `CPerform Atom` in the ANF; update `mapAtoms` / `FreeVars` / `Pretty` / NbE
  `size` / `usage` / `Quote` / `Eval` / `Specialize`, and the conservative
  `EffectAnalysis` arms (§2: `eperfC`/`mtouchC` `= true`, `vsumC = unknownValue`,
  `sinkableCall = false`).
- VM / LLVM lowering = unit application.
- `CPerform` always pinned; only `perform (known lambda)` same-position β-reduction.
- The binding-surface guard (§4a) captures each top-level body's **pre-optimisation**
  outer kind and enforces it on the final output.
- Gate: `--opt == --no-opt == oracle` (no ratio improvement required), **plus** a
  `.pmi` `ExportKind`/hash fixture proving `--opt == --no-opt` on an `Effect` CAF
  (`f = pure x`, `main = …`).

Each slice below lands its keys' `GerDescriptor`s (recognition + η-expansion +
lowering) **and** removes that slice's `StructuralGuest` keys — the structural-backed
subset, `structuralExclusions` (§3) — from `NbeEnv.structural` together (the atomic
per-slice rule); an `EffectEliminator` key is owned with no exclusion, and no key is
excluded before its slice.

### Slice 2 — core `Effect` glue

- `GerDescriptor`s for `pureE`, `bindE`, `unsafePerformEffect`. `pureE` / `bindE`
  leave `NbeEnv.structural` in this slice; `unsafePerformEffect` is a native foreign
  **not** present in `Ffi.structural`, so it is *owned as a descriptor* (recognised +
  lowered to `CPerform e`) but produces **no** structural-rung exclusion.
- An **unresolved dispatch** (concrete instance not yet exposed) is left as the
  higher-level dispatch and retried next round; a **bare / under-applied** GER-owned
  foreign is **η-expanded and lowered** (§5), never declined — so no bare structural
  foreign reaches the backend at `--opt`.
- IR-fires tests confirm the canonical `CPerform` tree; re-application is a fixpoint;
  a bare/partial `Effect.bindE` fixture confirms η-expansion leaves no residual
  structural foreign (VM **and** LLVM).

### Slice 3 — generalised dictionary surface

- `functorEffect.map`, `applyEffect.apply`, `discard`, `void`, runtime `when` /
  `unless`.
- Concrete `Effect`-instance recognition via `DictMachinery` (not string hardcoding).
  (These are accessors/derived combinators, not new `Ffi.structural` keys, so they
  add no structural-rung exclusions.)

### Slice 4 — structural combinator lowering

- `GerDescriptor`s for `forE` / `foreachE` / `whileE` / `untilE` → `LetRec` / `CIf`
  ANF; self-tail perform fusion (§7); these keys (and their `ST` loop twins, if landed
  here) leave `NbeEnv.structural` in this slice.
- Measure `Effect`-loop closure / force overhead and stack behaviour here.

### Slice 5 — `ST`

- Reuse the marker; `GerDescriptor`s for `run` / loop / ref combinators → ordinary
  ANF; these `Control.Monad.ST.Internal.*` keys leave `NbeEnv.structural` in this
  slice. Region-local optimisation deferred.

## Verification

purvasm's faithful `--no-opt` supports a **stronger** differential gate than
purs-wasm has.

**Correctness.**

- `--opt == --no-opt == oracle`, on **stdout / stderr order and count**, not just the
  return value.
- `void (log "a") *> log "b"`; `discard` sequencing; runtime `when` / `unless` on both
  branches; `Effect (Effect a)` (outer force must not run the inner); bare
  `main :: Effect a`; an effectful foreign stored in a record / array / `case` branch
  and then run; `forE` / `whileE` callback arity; early-exit / conditional-body
  `Effect` loops. VM **and** native.
- **Mode-stability (P1-A):** the `.pmi` `ExportKind`/hash of an `Effect` CAF
  (`f = pure x`, `main = …`) is identical under `--opt` and `--no-opt` (ADR-0084), and
  its ADR-0077 pre-opt call facts match the emitted object.

**Structural fires.**

- `bindE` → canonical `CPerform` tree; `pureE` → unit thunk; neutral `CPerform`
  survives; known-thunk `CPerform` β-reduces in place; a **live** effectful
  `CPerform` survives even when its result is unused; the pass is idempotent;
  `forE` / `whileE` → `LetRec` / `CIf`; no `Perform` marker dissolves into a plain
  call except via the self-tail rule (§7).
- **No residual structural foreign (P1-B):** a bare / under-applied `Effect.bindE` /
  `Effect.pureE` / combinator is η-expanded and lowered, so no GER-owned structural
  foreign survives `Impurify` to the backend at `--opt` (VM **and** LLVM); the
  link-time `resolver` still resolves the key for the `--no-opt` path.

**Performance / safety.**

- VM instruction ratio; closure allocation / force count; `Effect`-loop
  constant-stack behaviour; the self-compile `size×` / `time×` gate; no native-stack
  overflow of the `Impurify` walk on the self-compile corpus (the fully trampolined
  traversal is owed/deferred with the sibling passes — §4).

An **effect-hot benchmark** is a prerequisite instrument: the current corpus has no
`Effect`-thunk hot path (count-state's hot loop is `State` / `Identity` dictionaries).
Promoting an `Effect` / `Ref` loop benchmark (a light extension of the ADR-0075
corpus) is needed to measure GER at all — this may be filed as its own small ADR
before Slice 4.

## Consequences

Once this lands, ownership settles as:

- **foreign provider**: the pure-router end state (routes to true intrinsics /
  runtime API / native foreigns — ADR-0094) is the *goal*, not what this ADR reaches.
  At this ADR's time the `Effect` / `ST` guest terms **remain in the link-time
  `resolver` as an explicit exception** — the `--no-opt` / link fallback (§3) — since
  `--no-opt` never runs `Impurify`. Their removal is a later step (a `--no-opt`
  replacement).
- **ulib**: pure-PureScript shadows and ordinary library definitions.
- **Impurify / GER**: converts `Effect` / `ST` operational semantics to explicit
  `CPerform` and ordinary ANF.
- **EffectAnalysis + `mayReadMutable`**: decides elimination / motion.
- **NbE**: collapses canonical thunks / continuations / control flow by its general
  rules.
- **backend**: lowers `CPerform` faithfully to a unit application.

On the `--opt` path the `Effect` / `ST` guest terms no longer *drive the collapse*:
NbE no longer unfolds them, and the compiler reaches the same straight-line code from
a small set of meaning-preserving rewrite rules plus one explicit force marker rather
than by inlining a large alternative-implementation term set. The guest terms are
**not** removed from the foreign provider — the link-time `resolver` retains them as
the faithful `--no-opt` implementation and the link fallback (§3). Their eventual
full removal is a separate, later step, sound only once the `--no-opt` path has a
replacement (a minimal always-on lowering, or a pure-PS shadow), and is out of scope
here. Even so, this is the natural direction of ADR-0094 ("the foreign provider is a
route, not an implementation store") and connects to the ADR-0034 / ADR-0095
effect-fact channel.

purs-wasm's own write-up lists the same programme's remaining items; several are
**already met** in purvasm (its item 1 — a faithful `--no-opt` oracle — is
the `--no-opt` baseline purvasm ships) or **avoided by construction** (its item 2's
position-dependent representation never existed here; its host-foreign reflection is
not ported). The residual risk purvasm inherits is item 3's discipline — never strip
`Perform` before purity — which §2 and §7 pin directly.

## Alternatives considered

- **Port purs-wasm's host-foreign reflection verbatim.** Rejected: purvasm foreigns
  already return `Effect` thunks at saturation; reflecting them again in value
  position introduces double-thunking and saturation drift against `ForeignShape`.
- **Widen `DictElim.intrinsicLift` to the structural resolver** so `Effect.bindE`
  surfaces as a bare foreign for a later pass. Rejected (§6): breaks the backend seam
  (no LLVM lowering for a bare structural `AtomVar`) and re-opens the ADR-0098 hazard
  of NbE's structural rung unfolding the exposed key back to the thunk guest term.
- **Reuse `CApp t [unit]` instead of a distinct `CPerform`.** Rejected (§1–2): a plain
  application loses the run marker, and once the marker is gone the head-based purity
  analysis cannot see which higher-order argument is performed — exactly the trap
  purs-wasm's item 3 documents.
- **A richer `ForeignLift` policy** ("this residual arity is guaranteed consumed").
  Rejected (§6): complicates the `String -> Boolean` contract for no clearer
  ownership than having `Impurify` consume the dispatch directly.
