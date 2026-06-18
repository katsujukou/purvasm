# 0013. Guards in `case` alternatives

- Status: Accepted
- Date: 2026-06-18

## Context

[0011](0011-adt-pattern-matching.md) (Accepted) added `case` and deferred
**guards** as its other orthogonal axis, noting they "sit on the alternative, not
the binder" and "need a continuation frame (a guard is a term that is
*evaluated*, and a false guard falls through to the next alternative)". This
record adds them.

CoreFn models an alternative's right-hand side as `Either [(Guard, Expr)] Expr`:

- `Right e` — an **unconditional** result (what [0011](0011-adt-pattern-matching.md)
  has today).
- `Left [(g₁, e₁), (g₂, e₂), …]` — a non-empty list of **guarded** results.
  Guards are tried top to bottom; the first whose guard holds yields its
  expression. If **every** guard in the alternative fails, control **falls
  through to the next alternative** — whose binders are then matched afresh.

A `Guard` in CoreFn is a boolean `Expr`. PureScript's *pattern* guards
(`| Cons x xs' <- xs`), **conjunctive** guards (`| g₁, g₂`), and `let` in guards
are desugared by the compiler **before** CoreFn — into nested `Case` expressions
plus a let-bound **fail-continuation** closure that the match-failure paths call
to fall through to the next alternative — so the machine only ever sees the two
forms above (`Right e` and `Left` of *single boolean-condition* guards). Pattern
guards are therefore **out of scope** here as a machine feature: the frontend
lowers them to constructs the machine already has (`Case`, `Abs`/`App`, `Let`),
so nothing new is needed for them.

> **Empirical confirmation (2026-06-18, `purs` 0.15.16 CoreFn).** A pattern guard
> `case xs of C y | C _ <- y -> y` lowers to all-`isGuarded:false` alternatives:
> the guard becomes a **nested `Case`** (`case y of C _ -> y ; _ -> v true`) and
> the catch-all is hoisted into a let-bound fail-continuation `v = \_ -> …` that
> both the nested failure and the outer `_` call. A conjunctive guard
> (`| g₁, g₂ -> e`) lowers the same way. Only **single boolean-condition** guards
> survive as `isGuarded:true`, and a disjunction of them (`| g₁ -> e₁ | g₂ -> e₂`)
> is exactly a **multi-element** `Left [(g₁,e₁),(g₂,e₂)]` — confirming this
> record's `Guarded of (term * term) list`.

Two properties make guards genuinely different from the binder axis
([0011](0011-adt-pattern-matching.md), [0012](0012-array-record-binders.md)),
both of which are pure structural matching:

1. **A guard is evaluated.** Unlike a binder, evaluating a guard re-enters the
   machine, so it needs a reified continuation frame — the first matching feature
   that does.
2. **A matching alternative can still be rejected.** Because a failed guard falls
   through, dispatch can no longer "commit to the first alternative whose binders
   match"; it must be able to resume the search at the *next* alternative, against
   the same already-evaluated scrutinees. This is exactly the evaluation-order
   observability that [0011](0011-adt-pattern-matching.md) said guards reintroduce
   (constraining the decision-tree freedom).

## Decision

### Generalize the alternative's right-hand side

Replace the unconditional-only result of [0011](0011-adt-pattern-matching.md):

```ocaml
type rhs =
  | Unconditional of term
  | Guarded of (term * term) list   (* (guard, result) pairs, tried in order *)

type alternative = { binders : binder list; result : rhs }
```

[0011](0011-adt-pattern-matching.md)'s alternative is the `Unconditional` case;
its `result : term` becomes `result : rhs`. A `Guarded []` is not emitted by
CoreFn (a `Left` list is non-empty); the machine treats it defensively as
"no guard holds" → fall through.

### Dispatch becomes guard-aware

[0011](0011-adt-pattern-matching.md)'s dispatch took the first alternative whose
binders match and committed to it. It is generalized to a top-to-bottom scan that
*may resume*:

For each alternative in order, match its binders against the scrutinee values
`vs` (binder matching follows [0012](0012-array-record-binders.md)'s contract: a
legitimate value-level non-match yields `None`, a type-impossible shape stucks):

- **binders do not match** (`None`) → try the next alternative (as before);
- **binders match → bindings `bs`**, then by the right-hand side:
  - `Unconditional body` → bind `bs` and evaluate `body` (exactly
    [0011](0011-adt-pattern-matching.md));
  - `Guarded ((g, e) :: more)` → bind `bs` (allocating in the store, extending
    the env to `ρ′`), then **evaluate `g` in `ρ′`** under a new
    `Guard_test` frame remembering `e`, the remaining `more`, `ρ′`, and the
    fall-through state (the alternatives still after this one, `vs`, and the
    case's environment).

When the guard value returns:

- **`true`** → evaluate `e` in `ρ′`;
- **`false`, `more = (g₂, e₂) :: rest`** → evaluate `g₂` in `ρ′` under a
  `Guard_test` frame with `rest` (same bindings — still this alternative);
- **`false`, `more = []`** → **fall through**: dispatch the alternatives *after
  this one* against `vs` in the case's environment;
- not a boolean → stuck (a guard must be `Bool`; well-typed CoreFn guarantees it).

No alternative matches (binders fail for all, or all guards fall through) → stuck,
the same non-exhaustive-case stuck as [0011](0011-adt-pattern-matching.md).

### New continuation frame

```ocaml
Cont.Guard_test of
  { on_true     : Ast.term                 (* result if the guard under test holds *)
  ; rest_guards : (Ast.term * Ast.term) list (* remaining (guard, result) of this alt *)
  ; alt_env     : Env.t                     (* ρ′: this alternative's bindings *)
  ; rest_alts   : Ast.alternative list      (* alternatives to try on fall-through *)
  ; scrutinees  : Value.t list              (* already-evaluated vs, to re-match rest_alts *)
  ; case_env    : Env.t                     (* the case's environment, for fall-through *)
  ; rest        : Cont.t
  }
```

An **inline record** is proposed for this one frame (rather than the tuple shape
the other frames use) purely for readability — it carries seven fields, and naming
them documents the fall-through state. It remains first-order data, so the
keystone property of [0002](0002-cesk-execution-model.md) (the continuation is
inspectable/serializable) is preserved.

### Transition rules

Notation as in [0002](0002-cesk-execution-model.md)/[0011](0011-adt-pattern-matching.md);
`dispatch(alts, vs, ρ, σ, κ)` is the scan above, `ρ′`/`σ′` extend `ρ`/`σ` with
one fresh `α` per binding in `bs`. `dispatch` itself takes no step when binders
fail — it is a host-level scan that bottoms out in one of the rules below or in
*stuck*.

**Dispatch on the first binder-matching alternative:**

| matched alternative | next state |
| - | - |
| `Unconditional body` | `⟨Eval(body, ρ′), σ′, κ⟩` |
| `Guarded ((g,e) :: more)` | `⟨Eval(g, ρ′), σ′, Guard_test{on_true=e; rest_guards=more; alt_env=ρ′; rest_alts; scrutinees=vs; case_env=ρ; rest=κ}⟩` |
| `Guarded []` (defensive) | `dispatch(rest_alts, vs, ρ, σ, κ)` |
| (no alternative's binders match) | **stuck** — non-exhaustive case |

**Return into `Guard_test` with value `v`:**

| condition | next state |
| - | - |
| `v = VBool true` | `⟨Eval(on_true, alt_env), σ, rest⟩` |
| `v = VBool false`, `rest_guards = (g₂,e₂) :: more` | `⟨Eval(g₂, alt_env), σ, Guard_test{on_true=e₂; rest_guards=more; …unchanged…}⟩` |
| `v = VBool false`, `rest_guards = []` | `dispatch(rest_alts, scrutinees, case_env, σ, rest)` |
| `v` not a boolean | **stuck** — guard is not a `Bool` |

The scrutinees are evaluated **once** (by the existing `Case_scrut` frame,
[0011](0011-adt-pattern-matching.md)) and then carried through dispatch and every
`Guard_test`, so fall-through never re-evaluates them.

## Consequences

- **`case` is now complete** (modulo pattern guards, which the frontend desugars):
  multi-scrutinee, all binder forms ([0011](0011-adt-pattern-matching.md)/
  [0012](0012-array-record-binders.md)), and guarded alternatives with
  fall-through. The CoreFn frontend can lower `CaseAlternative` directly:
  `Right e → Unconditional e`, `Left gs → Guarded gs`.
- **The first matching feature that reifies into K.** Binders never needed a
  frame because matching evaluates nothing; a guard does, so `Guard_test` is the
  first pattern-matching continuation. This is precisely the boundary
  [0011](0011-adt-pattern-matching.md) drew when it deferred guards.
- **Dispatch generalizes; `Pmatch` does not.** `Pmatch.match_binders` stays the
  pure structural matcher; only the machine's `dispatch` grows from "first
  binder-match commits" to "first binder-match whose guard holds, else resume".
  The [0011](0011-adt-pattern-matching.md) `dispatch_case` is the `Unconditional`
  special case of this rule.
- **Failed-guard bindings are dead allocations.** A guarded alternative allocates
  its `bs` in the store *before* its guard runs; if the guard fails and control
  falls through, those cells are unreachable. The append-only store
  ([0002](0002-cesk-execution-model.md)) makes this correct, and the future
  per-fiber GC reclaims them — the same "dead allocation" status as any value that
  becomes unreachable. (A later optimization could test guards before binding when
  the binder introduces no variables the guard uses, but that is a codegen
  concern, not the spec.)
- **Evaluation order is observable, and the bytecode must preserve it.** Guards
  run top to bottom, left guard before right, and fall-through re-tries later
  alternatives — a fixed sequence the decision-tree lowering of
  [0011](0011-adt-pattern-matching.md) must honour. Structural tests may still be
  shared/reordered, but guard evaluation is sequenced; this is the constraint
  [0011](0011-adt-pattern-matching.md) anticipated when it said guards remove the
  "reorder at will" freedom.
- **Minimal churn to [0011](0011-adt-pattern-matching.md).** Only the `alt` smart
  constructor changes (it wraps its term in `Unconditional`); every existing
  unconditional alternative — samples and tests — is unchanged at the call site. A
  new `altg` constructor builds guarded alternatives.
- **`K` stays first-order.** Despite the seven-field frame, `Guard_test` is plain
  data; nothing about save/resume/scheduling
  ([0002](0002-cesk-execution-model.md)) is compromised.

## Alternatives considered

- **A single guard per alternative** (not a list). Rejected: CoreFn emits a list
  (`| g₁ = e₁ | g₂ = e₂` over shared binders); a single guard would force the
  frontend to split one alternative into several with duplicated binders, and the
  `Guard_test` frame threads the list at no extra cost.
- **Encode "unconditional" as an always-true guard** (`result : (term * term)
  list`, an empty/`true` guard meaning unconditional). Rejected: CoreFn's
  `Either` distinguishes the two cleanly; a magic always-true guard obscures the
  common unconditional case and invites an extra `VBool true` step per arm.
- **Desugar guards to nested `if`/`case` at lowering, with no machine support.**
  A guard chain *is* an `if`-`else` cascade — but its `else` is "fall through to
  the next alternative", which needs the already-evaluated scrutinees re-matched
  without re-evaluating them. Desugaring that without a machine frame means either
  duplicating scrutinee evaluation (wrong for effects) or synthesising join points
  and a re-match term — more machinery in the lowering than a single
  `Guard_test` frame in the spec. Rejected for the executable spec; the bytecode
  layer may still compile guards to branch cascades over the shared decision tree.
- **Re-evaluate scrutinees on fall-through.** Rejected outright: scrutinees must
  be evaluated exactly once (effects, identity); the values `vs` are carried in
  the frame precisely so fall-through re-matches without re-evaluating.
- **Reify the alternative scan into K as an explicit `Case_alts` frame** (instead
  of a host-level `dispatch` that bottoms out in `Guard_test`). Rejected as
  unnecessary: the binder scan evaluates nothing, so there is nothing to suspend
  between alternatives *except* across a guard — and that one suspension point is
  exactly what `Guard_test` already captures (it carries `rest_alts`). A separate
  frame for the pure scan would duplicate that state for no gain.
