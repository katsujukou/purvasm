# Investigation: why the fixed-size-threshold inliner exploded (the pre-NbE `Simplify`)

- Date: 2026-06-22
- Status: historical — the mechanism that motivated the move to NbE; the cure is ADR 0020 (NbE) + ADR 0035 (sharing).
- Canonical record: [ADR 0020 §Context](../design-decisions/0020-reduction-aware-inliner.md). This note is a focused mechanism summary.
- Code: `MiddleEnd/Optimize/Inline.purs` (`inlineCandidates`, `generalInlineCap`), `MiddleEnd/Optimize/Simplify.purs` (the rule engine), `compiler/test/NbeStress.purs` (the diamond guard).

## The old inliner in one line

Before NbE, the MIR optimizer was a bottom-up rewrite fixed point (`Simplify.simplifyExpr`) fed a
**pre-selected inline set** chosen by a static threshold (`Inline.inlineCandidates`, `Inline.purs:79`):

```purescript
inline a top-level binding  iff  exprSize rhs <= generalInlineCap{-24-}  ||  useCount key <= 1
```

i.e. **small (≤24 nodes) OR single-use**, and the set is filtered **acyclic** (`Inline.purs:58`) so the
rewrite fixpoint terminates. Each `Var q` whose key is in the set is replaced by its RHS at **every** use
site, every pass, until `prog' == prog` or the `maxPasses` (≈1000) clamp.

## The explosion pattern: the diamond (2^depth)

Acyclicity makes the fixpoint *terminate*, but **acyclic is not bounded**. The pathological shape is a
**diamond**: a binding that uses another inlinable binding **more than once**.

```
b0 = leaf
bᵢ = f b₍ᵢ₋₁₎ b₍ᵢ₋₁₎      -- each level references the previous TWICE
```

Inlining `b_d` substitutes `b_{d-1}` **twice**, each of which substitutes `b_{d-2}` twice, … →
`b0` is copied **2^d** times. The set is acyclic (`b_i` only references lower `b_j`), so the fixpoint
*ends* — but only after building a `2^depth`-sized term. This is encoded verbatim as the regression
guard `Test.NbeStress` ("a depth-20 diamond inline DAG"); on the old engine its normal form doubles per
depth.

**Real-world trigger: CPS / `Fold`-newtype fusion** (`Snapshot.Fusion01`/`02`). `mapF` / `filterMapF` /
`(<<<)` / the `Fold` wrappers are each **small** (≤24) and used **several times**, so they are exactly
diamond nodes — inlining them compounds. Measured (with the round/pass caps lowered so it completes):
the program grows **~1.5× per simplify pass and never converges**; at the real cap `maxPasses=1000` a
single round builds a term on the order of `1.5^k`. It *terminates* (every walk is finite-tree; no
missing base case) but is **non-contracting** — as good as non-terminating in practice (the
`Optimize.Specialize` self-compile "hang" and the Fusion stack-overflow / minutes-long spins).

## Why a size/use threshold can NOT fix it

The decisive finding (ADR 0020): the **same** "small, multi-use top-level inline" is **load-bearing for
the wins**, so you cannot tighten the threshold without losing them:

| Same size + use-count, opposite outcome | inlining the small multi-use binding… |
| --- | --- |
| `bench/CountState` State-monad `bind`/`pure`/`get`/`modify` | collapses to straight-line arithmetic — a **shrink** |
| dictionary methods (`eq` etc) | saturate to intrinsics (`intEq`) — a **shrink** |
| CPS fusion `mapF`/`filterMapF`/`Fold` | produces *more* CPS — a **grow** |

The discriminator is **whether the inline reduces** (β / projection / known-case / saturation fires
afterward), **not** how big the binding is or how often it is used. A static size/occurrence heuristic is
blind to that: restrict to single-use → State/dict collapses regress; allow small multi-use → fusion
explodes. *That* is the wall.

Hypotheses tested and **falsified** (so the cause is pinned to the top-level inliner, not β/`substMany`):

- Rebinding β arguments with `let` instead of substituting (to share non-linear args): barely moved the
  sizes (4.85M → 5.24M) and regressed 10 e2e — reverted.
- Disabling the `smallLambda` multi-use let-inline: still explodes.
- **Disabling the top-level inline rule** (`Var q → ctx.inline[key]`): Fusion **converges and shrinks**
  (2.81M → 2.73M) instead of exploding to 11.2M → the top-level inliner is the duplicator.

## The cure (and a second, independent exponential)

- **ADR 0020 — NbE, reduction-aware.** Replace "pre-selected set fed to a rewriter" with "inlining as a
  *consequence of evaluation*": references unfold into a semantic domain (`Sem`), redexes fire in the
  meta-language **only when operands are known**, and `quote` decides per binding from its *residual*
  usage — **inline when it reduced, retain a shared `let` when it did not** (never copy a non-reducing
  multi-use value). This makes the diamond a single shared binding instead of `2^depth` copies.
- **ADR 0035 — sharing the NbE reducer (a SECOND exponential).** The NbE core was *itself* exponential
  for lack of memoization: `eval` recomputed each inline binding per use site (M1) and `quote`
  re-evaluated shared values per path (M2) — Θ(2^d) on the same diamond DAG, now in the *reducer's work*
  rather than the *output*. Fixed by Layer A (memoize each inline binding's `eval`) + Layer B (`SShared`
  tag + `quote` CSE into a hoisted `let`), making `normalize` polynomial.

## Contrast with the 2026-06-21 `genericShow` blow-up (see 0001)

Same root family ("acyclic-DAG inlining is not bounded by acyclicity"), **different shape**, so keep them
distinct:

| | old fixed-threshold exponential | `genericShow` blow-up (NbE, ADR 0035 Layer C) |
| --- | --- | --- |
| carrier | **multi-use** small top-level bindings (diamond) | **single-pass** acyclic **dictionary-record** DAG |
| growth | **2^depth** (multiplicative duplication) | **~33×** linear-ish deep unfold (materialised stuck bulk) |
| why it inlines | small OR single-use threshold | projection + β always fire (record + dict known) |
| why it doesn't reduce | non-reducing CPS retained per copy | terminal `case <opaque> of` stays stuck |
| fix | NbE reduction-aware inline-or-share (ADR 0020) | Layer C per-app guard + cap; principled = method-tagging (0001 §4) |

The through-line: **acyclicity guarantees termination, not bounded size.** A static size/use threshold
cannot tell a reducing inline from a non-reducing one; only evaluating (NbE) and deciding from the
residual can — which is why both stories converge on reduction-aware inlining.
