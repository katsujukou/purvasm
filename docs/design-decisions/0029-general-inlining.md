# 0029. General inlining (α-renaming) and value folding

- Status: ~~Proposed~~ **Rejected** *(2026-06-22)*
- Date: 2026-06-21

> **Rejected (2026-06-22).** A static size/use-threshold general inliner is the
> *documented wall*, not a tuning problem — see `sidenotes/0001`–`0004` and
> [[inliner-blowup-reduction-aware]]. The discriminator is whether an inline
> *reduces* (β/projection/known-case/saturation fire afterward), not its size or
> use-count: the same small multi-use shape is load-bearing for the wins
> (monad/dict collapses) and the cause of the explosions (CPS/fusion diamonds →
> 2^depth; dictionary-record DAGs → ~30× materialised stuck bulk). The principled
> answer is a reduction-aware **NbE** normaliser (inline-or-share decided from the
> residual, with reducer sharing), deferred until after the bytecode VM. The
> flat-only inliner (ADR-0028) and DictElim (ADR-0027) stay — both blow-up-safe
> (flat bodies hold no nested calls; DictElim is atom-only). The Context/Decision
> below are kept as the considered-and-rejected design.

## Context

ADR-0028's inliner only inlines *flat, closed-but-for-parameters* callees (the
eta-expanded primops), which captured monomorphic arithmetic. The benchmark showed
`quicksort`/`n-queens`/`bintree-bfs` barely moved: their hot work is comparison
through `Data.Ord`, and those functions are small but *non-flat* (verified in the
generated CoreFn):

- `compare = \d -> case d of v -> v.compare`;
- `lessThan = \d -> let c = compare d in \a b -> case c a b of LT -> true | _ ->
  false`;
- the instance `ordInt = (\x -> x) { compare: ordIntImpl LT EQ GT, … }`, whose
  `compare` field is a *computed* value.

DictElim (atom-only) and flat inlining cannot touch any of these. To collapse a
monomorphic-at-site Ord call we must inline the small non-flat wrappers and then
fold the record projection and constructor match they expose.

## Decision

A general simplifier (extending ADR-0028's `Simplify`), run to a fixpoint and
dependency-directed (a binding uses only its callees / dependencies and
locally-known values — never all-callers, never per-caller specialisation):

- **Inlining with α-renaming** — inline a binding `f = \p₁…pₙ -> body` at an
  *exactly saturated* call when `f` is **non-recursive** and **small** (callee node
  count ≤ a fixed threshold). The callee's bound names are freshened (α-renamed)
  so the spliced body cannot capture at the call site; parameters bind to the
  (atom) arguments. Generalises ADR-0028's flat rule, which avoided both renaming
  and a size cap by restriction.
- **case-of-known-constructor** — a `case` whose scrutinee is a statically-known
  constructor application selects the matching alternative and binds its
  sub-patterns (an as-pattern binds the whole). This is the naive matcher's
  semantics, folded at compile time.
- **projection-on-known-record** — `r.l`, where `r` is a statically-known
  record, is the field's value.

Guards keep it correct and bounded: inline only non-recursive callees under the
size cap (bounds code growth and guarantees termination); α-rename to avoid
capture; fold only on statically-known values; iterate with a cap.

The rewrites compose: inlining `lessThan ordInt` / `compare ordInt`, then folding
the exposed `case ordInt of v -> v.compare` (case-of-known plus projection),
resolves the dictionary as DictElim does but through general machinery — so a
polymorphic-looking but monomorphic-at-site Ord call collapses toward
`ordIntImpl … x y`. The general inliner thus *subsumes* DictElim's effect; DictElim
stays as a cheap, specialised fast-path.

Verified by the ADR-0025 round-trip (same value/effects on every fixture) and
measured by ADR-0026 — expect `quicksort`/`n-queens`/`bintree-bfs` to improve,
with the `direct`/`anf` baseline columns unchanged.

## Consequences

- Monomorphic `Ord` (and any small-wrapper) dispatch loses its function-call and
  dictionary layers; expected step (and some alloc) drops on the comparison-heavy
  benches.
- The `Ordering` intermediate — `compare` returns `LT`/`EQ`/`GT`, immediately
  matched — is fully removed only if a `case`/`if`-commuting fold is also present.
  Whether its allocation persists is a *measured* question; if it shows up,
  case-commuting is the natural follow-up ADR (decided by the bench, not assumed).
- Modular by construction: every rewrite uses only a binding's own body, its
  callees, and locally-built values, so the pass ports to per-module batch
  compilation ([[optimizer-modular-not-whole-program]]).

## Alternatives considered

- **Keep DictElim + flat-only inlining.** Misses Ord (measured ~0% on
  comparison-heavy benches); the whole point here is to reach the non-flat
  wrappers.
- **Monomorphisation / specialisation** (clone a polymorphic function per
  instance). Also kills genuinely polymorphic dispatch, but is heavier and
  whole-program-leaning; deferred until inlining's reach is exhausted.
- **Unrestricted inlining** (any callee, any site). Code blow-up and
  non-termination on recursion; the size cap and non-recursive guard are the
  dependency-directed subset that stays safe.
