# 0098. Effect instance-dictionary visibility: folding method projections off recursive dictionary CAFs (the `bindE`/`pureE` exposure GER needs)

- Status: ~~Proposed~~ **Accepted** _(2026-07-13: accepted by the maintainer after review round 1; implementation held until the concurrent NbE bug-fix track lands)_
- Date: 2026-07-13

> **Revision (2026-07-13, review round 1).** P1 folded: the publication predicate
> is pinned to a dedicated **`pureRecordTail`** (each chain `let` RHS must be
> `pureValueRhs`), *not* the existing `recordTail` — which skips every `Let` RHS
> unconditionally and would admit a computation binding into a nullary grouped
> candidate whose outer binders `entryFromCandidate` all marks, letting an
> unrelated group-free projection commit while a `CApp`/prim/accessor/branch was
> substituted or dropped (see *The publication rule*). A **computation-chain
> non-publication** negative fixture is added to Verification.

## Context

Impurification (GER, ADR-0034) needs a concrete-`Effect` `do`-block to appear as
a recognisable `bindE`/`pureE`/`discard`/loop tree so it can lower it to direct
imperative bytecode. ADR-0034 deferred GER on the grounds that *"the do-block
dispatch does not resolve that far: it stalls at the nested `Monad → Bind → bind`
dictionaries … never collapse to `Effect.bindE`."* That note dates from boot's
whole-program `dict_elim.ml`; the Level-2 seam has since gained DictElim
(ADR-0027/0086), the NbE general inliner (ADR-0089, incl. its parameterized-
instance grouped-projection extension), and dictionary specialization (ADR-0093).
This record establishes **what actually stalls today** and fixes it as the
independent prerequisite the effect-analysis follow-up called out.

### What the seam actually produces (empirically established)

Threading real `output/` artifacts (`Effect`, `Control.Bind`,
`Control.Applicative`, `Control.Monad`) through the real `--opt` seam and probing
the three do-block dispatch shapes yields, **all stalling identically at the last
hop** — the method projection off the instance dictionary CAF:

| dispatch | residual (before this ADR) |
| --- | --- |
| `Control.Applicative.pure applicativeEffect x` | `applicativeEffect.pure` applied to `x` (stuck) |
| `Control.Bind.bind bindEffect m k` | `bindEffect.bind` applied to `m k` (stuck) |
| `Control.Bind.discard discardUnit bindEffect a k` | `bindEffect.bind` applied to `a k` (stuck) |

Two facts fall out of this, both correcting prior assumptions:

- **There is no `pure`-vs-`bind` asymmetry.** An earlier effect-track note held
  that the structural rung already unfolds `pureE` and only `bind` stalls. That
  reading came from an NbE *unit* test that feeds a **pre-exposed**
  `Effect.pureE a u`; it never exercises the accessor chain. In real code `pure`,
  `bind`, and `discard` stall at the same place.
- **The `discard → bind` two-hop chain now resolves.** `discardProbe` collapses
  `discard discardUnit`'s impl (`\dictBind -> bind dictBind`) all the way to the
  same `bindEffect.bind` projection — progress since ADR-0034's boot-era note,
  courtesy of ADR-0093 + NbE β. The residual stall is *only* the final
  dictionary-CAF projection.

### Why the projection cannot fold

`Effect`'s five instances are a **mutually-recursive** CoreFn binding group —
`Rec monadEffect, bindEffect, applyEffect, applicativeEffect, functorEffect` — the
cycle closed by `applyEffect = Apply$Dict { apply: Control.Monad.ap monadEffect, … }`
pulling `monadEffect` back in. After optimisation each instance is a **nullary
dictionary CAF** — a pure-value `let` chain whose tail is a record, e.g.
`bindEffect = let t = \_ -> applyEffect in { bind: Effect.bindE, Apply0: t }`
(the `$Dict` newtype wrapper erased, ADR-0018; `.bind` is the foreign
`Effect.bindE`, `.Apply0` a superclass thunk referencing a group sibling).

Two independent gates leave that CAF opaque to the projection:

1. **DictElim declines.** `dispatch` recognises the accessor + instance and
   computes `impl = Effect.bindE`, but `liftable intrinsicLift` refuses a
   structural foreign (`Effect.bindE` is neither an intrinsic primop nor a
   top-level gkey), so `pure applicativeEffect` / `bind bindEffect` is left
   intact for NbE.
2. **NbE has no candidate for the dict.** `candidatesOf` publishes a *recursive*
   group member only through its grouped-builder arm, which requires a **lambda**
   dict builder `\dict -> { … }` (ADR-0089's parameterized-instance shape). These
   instances are **nullary** dict CAFs — matched by neither the non-recursive
   value-candidate path (they are recursive) nor the grouped-builder path (they
   are not lambdas). With no published candidate, NbE treats `bindEffect` /
   `applicativeEffect` as opaque globals, so `.bind` / `.pure` never fold.

The differentiator is the **recursion**: a *non-recursive* dictionary CAF already
folds (the ADR-0089 "peeks a published record CAF through its chain" path). The
fold machinery — grouped-projection with a group-free commit check — **already
exists**; the recursive nullary dict CAF simply never gets a candidate to fire it.

## Decision

Publish a recursive-group member whose body is a **pure-value `let` chain ending
in a record** as an **`arity: Just 0` grouped candidate carrying the whole group's
key set**. A bare reference to it (spine length 0) then rides the *existing*
grouped-projection trigger unchanged, so the method projection folds under the
same safety machinery ADR-0089 built for parameterized instances.

### The publication rule

In `candidatesOf`'s grouped arm, alongside the existing lambda-builder clause, a
Rec member `e` qualifies when `e` is a **pure-value `let` chain ending in a
record**, is within the publish bound, and carries no specialization-clone
reference. It is published with `arity: Just 0`, `group =` the whole Rec group,
and `body = e`. A lambda builder still routes to the existing clause (matched
first); a recursive *function* never qualifies (its body is not a record tail).

The "pure-value chain" is **load-bearing and must be a dedicated predicate**, not
the existing `recordTail` — which skips *every* `Let` right-hand side
unconditionally and so admits a computation binding in the chain. That gap is a
soundness hole *for a nullary grouped candidate specifically*: `entryFromCandidate`
marks **all** of the candidate body's outer `let` binders (`chainBinders`), so
forcing the entry substitutes/erases those bindings and yields the bare record
tail. If a chain binder's right-hand side were a computation (`CApp`, a `CPrim`, a
`CAccessor`, or a branch), a projection of an *unrelated* group-free field
(`.bind`) could then commit while that computation was silently substituted or
dropped — moving/eliminating an eff.ful or store-touching call. So the predicate
must admit **only** pure value right-hand sides — reusing the existing
`pureValueRhs` (alias / lambda / ctor / array / record construction), rejecting
`CApp`/prim/accessor/branch:

```
pureRecordTail (Let _ rhs rest) = pureValueRhs rhs && pureRecordTail rest
pureRecordTail (Ret (CRecord _)) = true
pureRecordTail _                 = false
```

(The lambda-builder clause is unaffected: its `recordTail body` inspects the body
*inside* the `CLam`, whose `let`s are evaluated at application, not marked as outer
chain binders — the marking hazard is unique to the nullary CAF whose chain *is*
the outer binder set. The real `Effect` instances satisfy `pureRecordTail`: their
only chain binder is the `Apply0` superclass thunk `\_ -> applyEffect`, a `CLam`,
which `pureValueRhs` admits.)

### Why `arity: Just 0` grouped — not a plain value candidate

Routing a Rec member through the ordinary `known` peek (`arity: Nothing`,
`group` empty) *would* fold `bindEffect.bind` — but it **drops the grouped
safety**: the S8 self-stop (evaluate the body with the whole group removed from
the externs) and the **group-free commit check**. Without them, a projection that
selects a sibling-/self-referencing field would materialise that reference
inline, re-opening the one-call-per-round ratchet the grouped trigger exists to
bound (ADR-0089's parameterized-instance termination pin). `arity: Just 0` keeps
the grouped machinery engaged:

- `entryFromCandidate` evaluates the body **group-stopped** for any grouped
  candidate, arity notwithstanding.
- the existing grouped-projection trigger fires on `spine length == arity`, which
  for a bare reference is `0 == 0`.
- it **commits only when the selected field is group-free** — so `.bind →
  Effect.bindE` / `.pure → Effect.pureE` commit (the foreign is outside the
  group), while `.Apply0` (`\_ -> applyEffect`, a group sibling) **refuses** and
  the projection stays in its original form.

The deferred-saturated-ref mark (`deferMarks`) is **not** needed: there is no
saturated builder application to carry across a `let`; the target is a bare
reference.

### The exposed nodes stay recognisable (the GER interface)

At an `Effect` do-block the projected foreign is **under-saturated** —
`bind bindEffect m k` exposes `Effect.bindE m k` (2 of the guest term's 3
parameters; the trailing `Unit` thunk is applied only when the whole `Effect`
runs), and `pure` exposes `Effect.pureE x` (1 of 2). Under-saturation means the
NbE structural rung does **not** unfold them to their thunk guest terms, so they
survive as recognisable `Effect.bindE` / `Effect.pureE` nodes.

This visibility is **necessary but not sufficient** for GER, and this ADR
deliberately stops at visibility. Once GER exists, a *saturated* `Effect.bindE`
could still be unfolded to its guest term by the structural rung inside the same
NbE evaluation, before GER sees it. Resolving that belongs to the GER record,
which must pin one of: **(1, preferred)** exclude the GER-owned keys
(`Effect.bindE` / `Effect.pureE`, and the control combinators GER lowers) from the
NbE structural rung and hand GER the quoted ANF; or (2) run GER reflection ahead
of generic structural unfolding inside the evaluator. The intended sequencing is
**land this ADR first, then transfer the GER-owned keys out of the structural rung
when GER lands** — a clean responsibility split, and the reason ADR-0094 already
kept the `Effect.*` combinators out of its ulib-body relocation.

## Verification

Established on the real `Effect` instance group through the `optimizeProbe` seam
harness, and to be pinned as permanent fixtures:

- **Positive (real Effect group):** `pure applicativeEffect x → Effect.pureE x`;
  `bind bindEffect m k → Effect.bindE m k`; `discard discardUnit bindEffect a k →
  Effect.bindE a k`.
- **Group-free commit / sibling refusal:** `bindEffect.Apply0` (a group-sibling
  field) is left unchanged — the commit check refuses, no `applyEffect` leaks.
- **Computation-chain non-publication (the P1 predicate guard):** a record-tail
  Rec CAF whose `let` chain contains a *computation* right-hand side
  (`let r = f x in { … }`, `r` unselected) is **not** published — `pureRecordTail`
  rejects it — so no projection of it ever folds, and the computation is never
  substituted/dropped behind a group-free commit.
- **Whole-dict demand:** a dictionary *passed* rather than projected
  (`consumer bindEffect`) stays a reference — the record is never inlined.
- **Term stability:** every residual is a fixpoint under re-optimisation.
- **GER-facing:** the exposed `bindE` / `pureE` remain **un-unfolded** (a
  recognisable node), i.e. the structural rung does not eat them at the do-block's
  under-saturation.
- **No regression:** the full unit + E2E suites, and the benchmark instruction-
  count / self-compile gate (the corpus has no `Effect`-hot path, so ~zero
  movement is the expectation, not a win — the payoff is GER's, later).

## Consequences

### Benefits

- The `bindE`/`pureE`-visibility blocker ADR-0034 named is removed as an
  independent, testable slice — GER can be built against exposed nodes rather than
  a stalled accessor chain.
- The fix is a single publication clause reusing ADR-0089's existing grouped-
  projection + group-free-commit machinery; no new evaluator path, no new safety
  argument beyond "arity 0 is the spine-0 case of what already runs".
- It generalises: any concrete instance of a mutually-recursive class hierarchy
  whose method field is group-free now devirtualizes (not only `Effect` — `ST`,
  and user monads with superclass cycles, get the same).

### Risks / caveats

- **Visibility ≠ GER.** Stated above and in ADR-0034: without the structural-rung
  key transfer, a *saturated* exposed foreign can still be unfolded before GER. In
  scope for the GER record, not this one.
- **Publication volume.** Every recursive dict CAF within the publish bound now
  enters the in-memory candidate map. Bounded by the same `< 64` publish bound as
  every other candidate; the group-stopped evaluation reifies only the selected
  field, never the whole dict, and whole-dict demand stays a reference.

## Alternatives considered

- **Plain value candidate (`arity: Nothing`, empty group).** Verified to fold the
  projection, but drops the S8 self-stop and group-free commit — a different dict
  group could leak a sibling-referencing field/closure and re-open the ratchet S8
  closed. Rejected in favour of `arity: Just 0` grouped, which keeps the safety.
- **DictElim lifts structural foreigns on the `--opt` path (option B).** Exposes
  `Effect.bindE m k` before NbE, but widens DictElim's responsibility to structural
  guest-term materialization and bypasses the grouped safety machinery; and the
  structural rung would then unfold the exposed foreign to its thunk term — the
  opposite of what GER wants to pattern-match. Rejected.
- **Do nothing until GER, resolve dispatch inside GER.** Rejected: dictionary
  devirtualization is the optimiser's existing job (ADR-0027/0089), not GER's;
  folding it here keeps GER a pure control-flow lowering over already-exposed
  nodes.

## Affected components

- `compiler/src/Purvasm/Compiler/MiddleEnd/Optimizer/Nbe.purs` — one clause in
  `candidatesOf`'s grouped arm (a `pureRecordTail` nullary Rec member →
  `arity: Just 0` grouped candidate) plus the small `pureRecordTail` predicate
  (reusing the existing `pureValueRhs`). No change to `Eval` (`groupedProject` /
  `entryFromCandidate` already handle it).
- Tests — the fixtures above (a real-`Effect`-group positive plus the
  commit-refusal / whole-dict / stability / GER-visibility pins).
- ADR-0034 (GER) — inherits the structural-rung key-transfer obligation stated
  under *Decision*.
