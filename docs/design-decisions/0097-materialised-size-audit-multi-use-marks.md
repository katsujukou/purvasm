# 0097. The materialised-size audit: multi-use re-materialising marks are gated on transitive size

- Status: Accepted
- Date: 2026-07-13

## Context

**A latent 2^depth blow-up in the NbE gate, found 2026-07-13** while building the
ADR-0096 fixtures (reproduced with the `const Nothing` effect oracle — pure
ADR-0089 machinery, present since the inliner landed; neither ADR-0095 nor
ADR-0096 is involved):

```
v1 = x + x
v2 = v1 + v1
…
vN = v(N-1) + v(N-1)        -- all live; every level uses the previous TWICE
```

Normalising this through `nbeBinding` **crashes with a stack overflow in
`Quote` at depth 12** (depth 8 completes; measured by a node probe against the
compiled output).

**Mechanism.** The gate-B `verdict`'s small-deref clause —
`u.capture <= CapBranch && f.cx <= Deref && rhsSize < 5` — marks **multi-use**
tiny computations for re-materialisation at every use site. That is sound and
intended for *one* level: a 3-node `AddInt` re-built twice is cheaper than a
shared binding. But the clause judges the **syntactic** rhs size, and the
rhs's atoms may reference *other marked binders*: after substitution, each
use-site copy carries the referenced binder's copies, which carry theirs — a
chain of multi-use marks compounds `2^depth` at quote time. This is
sidenote 0002's diamond (`each layer references the previous twice`) arriving
through the mark machinery instead of a size-threshold inliner — the exact
failure mode ADR-0089's fixtures exist to pin, except the existing diamond
fixture covers only the `CRecord`/KnownSize class, whose clause requires
`u.total == 1` and therefore cannot chain.

**Why the existing defences miss it:**

- The **round-growth backstop** (ADR-0089 self-compile extension) compares
  `nbeBinding`'s *result* against its input — but the explosion happens (and
  overflows the stack) **inside** the inner eval/quote fixpoint, before any
  result exists to compare.
- The **rewrite fuel** bounds round *count*, not round *size*.
- The **diamond regression fixture** (§7) pins the value-class shape
  (multi-use record stays one shared let), not the deref-class clause that
  re-materialises at multi-use by design.

The same hazard exists in the **small-lambda clause**
(`f.isAbs && rhsSize < 16`, which also fires at any use count): a multi-use
small lambda whose body references marked binders compounds identically. And
a `CAtom` alias (Trivial, always marked) is *transparent* — a diamond veiled
by aliases (`w = v1; v2 = w + w`) chains through it, so any fix must account
through aliases, not just direct references.

ADR-0096's cross-module fixture sidesteps the bug with a deliberately linear
chain and a comment pointing here; the compiler's own self-compile does not
currently contain a deep-enough diamond — but the shape is legal, natural
(`let a = x+x; b = a+a; …`), and crashes the compiler when a user writes it.

## Decision

Add a **materialised-size audit** to the gate-B analysis: a second, top-down
pass over the quoted term that computes each binder's *transitively
materialised size* and **strips any multi-use re-materialising mark whose
materialised size breaches its clause's own bound**. The clause's size test
becomes a promise the audit enforces transitively, instead of a syntactic
approximation the chain defeats.

### 1. The materialised size

`M(x)` is the node count of what `Eval` re-materialises at **one** use of `x`:
the rhs `c` with every *still-marked* reference expanded to what *it*
materialises, and every shared (unmarked or already-stripped) reference left a
1-node variable. It is computed by **recursing into `c`** (not from a
pre-tabulated free-var multiset):

```
M(x) = auditC(c)
auditC(CPrim/CCtor/…)   = 1 + Σ atomSize(operands)
auditC(CLam _ body)     = 1 + auditE(body)          -- recurse into the body
auditC(CIf/CCase …)     = 1 + … + Σ auditE(branch)  -- recurse into branches
atomSize(y)             = M(y) if y is kept-marked, else 1
```

The recursion is load-bearing (review **P2**): a marked binder inside a
lambda/branch **body** fans out at `Eval` time exactly as an outer reference
does, so its expansion must count toward the enclosing binder's `M`. A
free-var-only sum misses it — the body's inner `let` binders are dropped from
the usage multiset — and a small lambda whose body re-materialises marked
locals would slip under the bound while materialising far more. Walking the term
counts both in one pass. An alias (`x = y`) gets `M(x) = atomSize(y)` by the same
rule — transparency falls out, no special case.

### 2. Two axes — duplication and motion

A single numeric bound is not enough (reviews **P1**). Two things can go wrong
when a mark is re-materialised, and each is its own axis, propagated differently:

- **Duplication** (size / diamond) — re-materialising a mark at *N* sites copies
  it *N* times. Handled by the recursive `M` (a large embedded structure inflates
  its embedder past the bound) plus a `DupPolicy` that, **inherited through
  aliases**, keeps a single-use fixed structure from being re-exposed as multi-use.
- **Motion** (ADR-0096's capture boundary + execution count) — a call sink is
  licensed only single-use and only up to `CapBranch`; re-materialising it (or
  anything **embedding** it) more than once, or inside a **closure**, breaks that.
  Handled by a `motionCap` that **composes through every embedding**.

The first cut had two holes:

1. `v = {a:x,b:x}` (a `CRecord`, marked single-use), `w = v` (alias used twice):
   a numeric "no bound" is inherited, `w` is never stripped, the record
   re-explodes `2^depth` through a chain of aliases. → the **duplication policy**
   must be a first-class `ShareOnly` inherited through aliases, not a sentinel
   number.
2. `x = clean a` (single-use sink), `r = {value:x}`, `l = \u -> r`, `[l,l]`: `r`
   is *not* an alias — it *embeds* the call. Following the constraint only through
   aliases misses it, so `l` (small, multi-use) is copied, carrying the call
   **into the closure** and duplicating it. → the **motion** constraint must ride
   along as a capture-keyed fact and **compose through every embedding** (the
   duplication policy need not — see below).

**Duplication policy.** `data DupPolicy = ShareOnly | Bounded Int | Unbounded`,
from the rhs, alias inheriting its target's:

```
CAtom (AtomVar t) → policy of t (else Unbounded)   CRecord/CCtor/CArray/CUpdate → ShareOnly
CPrim | CAccessor → Bounded 5                        CApp → ShareOnly
CLam              → Unbounded if closed, else Bounded 16
```

`ShareOnly` strips at `total > 1` (no numeric bound applies); `Bounded n` strips
at `total > 1 ∧ M ≥ n` (the recursive `M` caps a graph of survivors); `Unbounded`
(closed lambda, bare-var alias) never strips. It inherits **only through aliases**
and deliberately does *not* compose through other embeddings (a `CPrim`/`CAccessor`
stays `Bounded 5` even when it embeds a `ShareOnly` value): a *large* embedded
structure is already caught by `M` (its size inflates the embedder past the bound),
a *small* one is harmless to copy, and the one embedded thing that is genuinely
dangerous to copy — a call — is caught by the motion axis, which does compose.

**Motion cap.** Each mark also carries `motionCap :: Capture` — the highest
capture it tolerates. A call owns `CapBranch`; everything else owns `CapClosure`
(unconstrained) but **inherits the `min`** of the caps of the marks it embeds
(so `r = {value:x}` gets `x`'s `CapBranch`); a lambda is `CapClosure` (building a
closure is pure — safe to re-materialise anywhere; a motion-constrained ref in its
body is stripped by its own use-capture before it can be embedded). A binder with
`motionCap < CapClosure` — i.e. it is, or embeds, a call — is stripped when it is
**used more than once** (duplicating the call) **or used above its cap** (moving
the call into a closure). This is the ADR-0096 single-use ∧ `≤ CapBranch` licence,
enforced through arbitrary embeddings. For it to see call marks at all, the
bottom-up pass records **every** mark's `Usage` (count *and* capture — the latter
already raised to `CapClosure` inside a lambda by `capAt`), verdict marks and call
marks alike.

**One walk.** Size, `stripped` set, policy, and `motionCap` all fall out of a
single top-down walk threading `kept-mark → { m, policy, motionCap }`; it recurses
into lambda/branch bodies (P2), and outer-first order makes each binder final
before its referrers. Stripping only removes marks (strictly more conservative
than the verdict). On the prim diamond it alternates `M(v1)=3` kept / `M(v2)=7 ≥ 5`
stripped / `M(v3)=3` kept …; on the record-alias diamond every fan-out alias is
`ShareOnly` and shared; and a call embedded anywhere is shared the moment it would
be duplicated or moved into a closure.

### 3. No inner-round growth guard (a defence-in-depth idea, dropped during implementation)

An earlier draft added a *secondary* guard inside `nbeBinding`'s loop: measure
each round's quoted-term size against the outer backstop's `roundGrowthMax`/
`growthFloor` cap (relative to a fixed `e0`), and on breach bail the whole call
to term-identity (`return e0`) — a cheap net meant to downgrade *unknown future*
blow-up classes from a crash to the backstop's kept-input diagnosis.

**It was implemented, measured, and removed.** Two independent reasons:

1. **It cannot catch what it was for.** The size is only measurable *after* a
   round's `quote` has built the whole term; a blow-up that detonates *within a
   single round* — the reported diamond's own failure mode — overflows the stack
   before any size exists to test. So the guard never covers a single-round
   crash; only the §2 audit (a *pre*-materialisation judgement) does.
2. **Its per-round test cannot tell a transient overshoot from a runaway.** The
   NbE fixpoint legitimately **overshoots then settles**: a binding inflates in a
   middle round and a later round (case-of-case distribution + folding) collapses
   it back. The outer backstop measures the *final* term and sees the settled
   size; a per-round guard fires on the transient peak. Measured on self-compile,
   it tripped **162 times across 54 legitimate bindings** (`CST.Parser.parse*`,
   the CLI `fmt*`/`show*` helpers — the compiler's own hot paths) while the outer
   backstop fired **zero** times, regressing the self-compile size ratio
   1.108→1.130 by bailing settling bindings to their un-optimised input.

The audit (§2) closes the known exponential class; the outer round-growth
backstop already downgrades a *non-settling* final term to kept-input; and a
genuine single-round crash needs an Eval/Quote-internal budget (see Alternatives),
which this guard is not. The residual blow-up surface after dropping it is small
and characterised under Consequences.

### 4. Verification

- **The regression fixture this record exists for:** the diamond prim chain
  at depth 20 normalises — terminating, output size within a small constant
  of the input, behaviourally equal (oracle differential on an executable
  variant).
- **Alias-veiled diamond** (`w = v1; v2 = w + w; …`): same bounds (alias
  transparency).
- **Lambda-clause diamond** (multi-use small lambdas referencing marked
  binders): same bounds.
- **P1 — alias laundering closed by policy inheritance:** the KnownSize-record
  alias diamond (`v_i = {a,b}` single-use, `w_i = v_i` used twice) stays linear
  at depth 20; a single-use clean call aliased twice (`x = clean a; w = x;
  T(w,w)`) materialises the call **once** (the alias inherits `ShareOnly` and is
  shared), never duplicated at both uses.
- **P1 (motion, embedding) — call constraint composes through non-aliases:** a
  single-use clean call *embedded* in a value and used twice
  (`x = clean a; p = x.field; [p, p]`) is shared, call once (duplication axis
  through the embedding); and one whose embedder is used inside a lambda
  (`r = {value: x}; l = \u -> r; [l, l]`) is kept **outside** the closure, one
  call, never moved in (motion axis) — the ADR-0096 `CapClosure` boundary, held
  through arbitrary embeddings.
- **P2 — inner-body expansion counted:** a small non-closed lambda whose body
  re-materialises a param-dependent inner small-deref six times (`\a -> let t =
  a+a in [t×6, x]`), used twice, is **shared not copied** — the recursive `M`
  (not the free-var approximation) sees the over-bound materialised body and
  strips the mark, so the optimised term holds one lambda.
- **Positive controls (no regression):** one-level multi-use small prim still
  re-materialises (the clause's intended win); the alternation shape
  (kept/stripped/kept) is asserted structurally; the ADR-0089 §7 fixture
  suite and the Simplify-transfer suite pass unchanged.
- **Gates:** `--opt ≡ --no-opt ≡ oracle` on the effect fixtures; benchmark
  instruction ratios and `size×`/`time×` against the current baseline
  (`benchmarks/out/opt-effect.txt`, 2026-07-13); self-compile completes with
  its size ratio within the ADR-0089 backstop.

## Consequences

- The gate's multi-use clauses keep their wins (one-level re-materialisation)
  and lose their compounding failure mode; the "small" in *small-deref* /
  *small-lambda* becomes true by construction, transitively.
- One more analysis pass per inner round — one top-down `O(term)` walk that
  computes materialised sizes and strip decisions together, threading a
  `kept-mark → { m, policy, motionCap }` environment. It needs from the bottom-up
  pass only each mark's `Usage` (occurrence count + capture — `markUsages ::
  Map String Usage`), not the effect facts; the measured ADR-0095 lesson (keep
  per-round analysis cheap) is respected — the walk visits each node once and
  size / cap lookups are `O(1)`.
- Stripping only ever *removes* marks: strictly more conservative than today
  on the affected shapes, identical everywhere else. Convergence is
  unaffected (the mark-stability check already tolerates mark-set changes
  between rounds).
- `Quote`'s non-tail recursion remains a separate, orthogonal limit (a deep
  *linear* let spine can still overflow on pathological inputs — the
  stack-safety hardening its preamble already defers is not this record).
- **The residual blow-up surface after this record (no inner guard).** The audit
  closes the exponential class — chained multi-use *re-materialising* marks
  (small-deref, small-lambda, alias) compounding `2^depth`. What it deliberately
  does **not** bound:
  * **Closed lambdas** (audit-exempt by design): a large closed lambda used at
    `N` sites duplicates `N`-fold. This is **linear**, not exponential (a closed
    body has no free references, so it cannot pull in other marks and cannot
    chain), and the outer round-growth backstop keeps the input when the *final*
    term breaches `roundGrowthMax`×. This is the common, benign overshoot the
    dropped guard mis-read as a runaway.
  * **A deep *linear* spine** overflowing `Quote` (the orthogonal stack limit
    above) — a size the audit is not even about.
  * **An unknown future reduction rule that compounds** through some clause the
    audit does not model. None is known — the audit covers every current
    multi-use re-materialising clause — but if one arose it would blow up
    *within a single round* and crash, since (per §3) no post-`quote` guard can
    see it coming; the fix then is an Eval-side budget (Alternatives), not a
    size backstop.

## Alternatives considered

- **Deny multi-use marks whose rhs references any local binder.** Simpler
  (no `M`), but loses the alternation recovery (only the first level of a
  chain keeps its win) and — more importantly — strips marks whose references
  stay within the bound (shared locals, or other kept marks whose transitive
  size is small), which are exactly the safe cases the `M` computation is there
  to keep.
- **A numeric bound per mark, inherited only through aliases (no `DupPolicy`, no
  motion fact).** The first cut; it failed review P1 twice. (a) A KnownSize
  construction or a call is marked only when *single-use*, so it has no natural
  numeric bound — stored as "unbounded" (`topBound`) — and an alias inheriting
  that number is never stripped, so `v = {a,b}; w = v; …` launders the exemption
  into `2^depth` fan-out. (b) Even with a first-class `ShareOnly`, following it
  only through *aliases* misses `r = {value:x}` (a value *embedding* a call, not
  aliasing it): `r` dodges the test and a small multi-use lambda over `r`
  duplicates the call and moves it into a closure. The fix needs a first-class
  `DupPolicy` (inherited through aliases) **and** a separate capture-keyed motion
  fact that **composes through every embedding** — the axis that actually has to
  chase a call through a non-alias value.
- **Compute effective size inside the existing bottom-up pass.** The
  dependency direction is wrong: a binder's rhs references *outer* binders,
  whose mark status the bottom-up pass decides *later*. A fixpoint over the
  bottom-up pass would re-walk the term O(depth) times; the top-down audit
  does it once.
- **An Eval-side expansion counter** (abort substitution past a budget). This is
  the only mechanism that could catch a genuine *single-round* crash — it acts
  during materialisation, before `quote` builds the whole term. It is not
  dismissed lightly, but it couples the decision into the evaluator's hot path
  and turns a static judgement into a dynamic one, when the audit already has
  every fact to decide statically. Left as the future option should a real
  single-round blow-up class (one the §2 audit does not cover) ever surface.
- **An inner-round growth guard** (measure each round's size, bail on breach).
  Prototyped and **dropped** (§3): it cannot catch a single-round crash (the size
  exists only post-`quote`), and its per-round test bailed 54 legitimate
  overshoot-then-settle bindings on self-compile where the outer backstop —
  measuring the settled *final* term — correctly fires on none. A guard tuned to
  fire only far above the fixpoint's legitimate transient peaks (a large absolute
  node ceiling, not the outer 4× cap) could be revisited if an unknown class ever
  motivates it, but it buys nothing over the audit for any *known* shape.
- **Do nothing (document the shape as unsupported).** A legal, natural source
  shape (`a = x+x; b = a+a; …`) that crashes the compiler is not a
  documentable limitation.
