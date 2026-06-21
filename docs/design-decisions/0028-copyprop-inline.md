# 0028. Copy-propagation and small-callee inlining

- Status: Accepted
- Date: 2026-06-21

## Context

DictElim (ADR-0027) collapses statically-known dispatch but measured ~0% alone:
PureScript floats a constant dictionary-member extraction (`add semiringInt`) to
a shared binding, so DictElim only turns that one-time binding into an *atom
alias*
`$x = intAdd`. The hot loop is `$x a b`, and `intAdd` is an eta-expanded primop
`\$0 $1 -> Prim(AddInt, [$0, $1])`. The per-call cost — the alias indirection and
the eta-application — is still paid every iteration.

Two standard, local simplifications capture it: **copy-propagate** the alias
(`$x` → `intAdd`) and **inline** the small callee, giving
`$x a b → intAdd a b → Prim(AddInt, a, b)` — the dispatch *and* the call indirection
gone, a real primitive at the site.

This must respect the modular optimisation principle ([[optimizer-modular-not-
whole-program]]): a pass may use what a binding *calls* (its callees /
dependencies), never who calls it, and must not specialise a callee per caller.

## Decision

A pass `Middle_end.Passes.Simplify` (run after DictElim) that, to a local
fixpoint, applies two dependency-directed rewrites:

- **Copy-propagation** — `Let (x, CAtom a, body)` becomes `body` with `x`
  substituted by the atom `a`, and the binding dropped. (`a` is a var/literal/
  foreign, in scope where `x` was; substituting an atom duplicates no work and
  cannot capture.) This removes DictElim's aliases and other trivial lets.
- **Saturated inlining of a small, flat callee** — a call `f a₁…aₙ` where the
  binding for `f` is `\p₁…pₙ -> Ret c` with `c` a *binder-free* computation (no
  nested `if`/`case`/`lambda`, so no capture and no code blow-up) and the call is
  *exactly* saturated, becomes `c` with each `pᵢ` substituted by `aᵢ`. This is the
  caller pulling in a callee's (a dependency's) body; the decision is made by the
  *callee's own shape*, never by how many callers it has.

Guards keep it correct and bounded: substitute atoms only; inline only saturated
calls of binder-free single-computation bodies; iterate to a fixpoint with a
small cap. Over-/under-application, and callees with `let`/`case`/`lambda` bodies,
are left alone (later, general inlining with alpha-renaming can lift the flatness
restriction).

Verified by the ADR-0025 round-trip (same value/effects on every fixture) and
measured by ADR-0026 — a combined `dictelim+simplify` variant whose curve drops
below `anf`/`dictelim` on the arithmetic-heavy benches, with the `direct`/`anf`
baseline columns unchanged (no regression).

## Consequences

- Monomorphic method calls collapse to primitives at the site (`+`/`-`/`==` →
  `Prim …`), capturing the per-call win DictElim set up; expected step/alloc drops
  on `fib`, `map-fold`, etc.
- Eta-expanded primop leaves (ADR-0017) inline away, so the FFI-ladder indirection
  costs nothing once a call is monomorphic and saturated.
- Polymorphic dispatch (dict is a parameter) is untouched — correctness over
  completeness; specialisation is future work.
- Modular by construction: every rewrite uses only a binding's own body and its
  callees, so the pass ports unchanged to per-module batch compilation.

## Alternatives considered

- **A full inliner** (size/usage heuristics, possibly inlining single-use callees
  via an all-callers count). More powerful but needs whole-program use information
  and risks code blow-up; it also violates the modular principle. The flat,
  saturated, callee-size-only rule is the safe, dependency-directed subset.
- **General beta-reduction everywhere** (inline any lambda body with
  alpha-renaming). Fine eventually, but the binder-free restriction avoids
  capture-avoidance machinery for v1 while still catching the eta-primop case that
  matters.
- **Fold DictElim into this pass.** Kept separate so each is independently
  measurable (DictElim's ~0% finding is itself informative) and independently
  reviewable.
