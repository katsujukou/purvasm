# 0024. By-need recursive bindings (refining ADR-0004)

- Status: Accepted
- Date: 2026-06-20

## Context

Implementing `Effect` (ADR-0023) surfaced a blocker that is not about `Effect` at
all: **mutually recursive type-class dictionaries** get stuck under the strict
`letrec` of ADR-0004.

`Effect`'s instances form one recursive binding group — `monadEffect`,
`bindEffect`, `applyEffect`, `applicativeEffect`, `functorEffect` — a genuine
cycle (`map = liftA1`, `apply = ap`, plus superclass back-references). CoreFn
emits the **superclass** fields as deferred thunks (`Applicative0 = \_ ->
applicativeEffect`) and the **method** fields eagerly (`apply = ap monadEffect`).
Our `letrec` evaluates members in source order — `monadEffect`, `bindEffect`,
`applyEffect`, … — and reserves an address per member; reading one before it is
filled is a black-hole. Building `applyEffect` (third) evaluates `ap monadEffect`,
whose CoreFn body is an eager dictionary-projection `let`
(`ap = \dict -> let bind = …; pure = … in \f a -> …`); projecting `pure` forces
`monadEffect`'s `Applicative` superclass thunk → `applicativeEffect`, the *fourth,
not-yet-built* member → `stuck`.

### How other backends handle the same cycle

- **The JS backend uses `$runtime_lazy`** — not, as an earlier draft of this
  record claimed, an `undefined` that is "captured but not used". The compiled
  `Effect/index.js` defines `$runtime_lazy` and wraps exactly the
  forward-referencing members (`functorEffect`, `applyEffect`) in it, leaving
  `monadEffect`/`applicativeEffect` eager. `$runtime_lazy` is a memoised
  init-once thunk with a "needed before initialised" guard — i.e. by-need
  evaluation applied selectively.
- **purs-wasm needs no general lazy mechanism** (its ADR 0008). Its insight:
  split the reference graph by which edges fire at construction. *Eager* edges are
  method fields (they point *up*: `applyEffect → monadEffect`); *lazy* edges are
  superclass fields (they point *down*, all behind thunks). Every cycle-closing
  back-edge is a superclass edge, so the **eager construction graph is a DAG**;
  it orders construction by a topological sort of that DAG and memoises via CAF
  globals or knot-tying, keeping `$runtime_lazy`-style laziness only as a fallback.

### Why purs-wasm's lazy-free route does not transfer to this oracle (yet)

purs-wasm's DAG argument rests on `ap monadEffect` being "a PAP that *captures*
a reference without reading the sibling's contents at construction." That holds
under **arity-aware / uncurried** calling: an under-saturated `ap monadEffect`
does not run `ap`'s body, so the projection `let` never fires until the method is
actually called (post-init). Our core is **curried and strict** (ADR-0002): `ap
monadEffect` is a genuine one-argument application that runs `ap`'s body now,
projecting the superclass and reading `applicativeEffect`. That is precisely why
the (also curried) JS backend cannot use the DAG/topo-sort route either and falls
back to `$runtime_lazy`.

The DAG only re-emerges once an optimiser removes the eager projections — purs-wasm
notes `Effect`'s group is in fact *eliminated* by `DictElim`/`Impurify`, not
constructed. The un-optimised curried oracle has neither uncurried capture nor
`DictElim`, so for it the construction graph genuinely contains the cycle.

## Decision

Make recursive (`letrec`) bindings **by-need**, refining ADR-0004 — the
curried-oracle equivalent of the JS backend's `$runtime_lazy`:

- A `letrec` reserves one store address per binding, each holding a **suspension**
  of its right-hand side closed over the (complete) recursive environment, then
  evaluates the body.
- The first dereference of a binding **forces** its suspension: the slot is marked
  black-hole, the right-hand side is evaluated, and the resulting value is written
  back and memoised (later dereferences are O(1); a CAF's effects run once).
- A binding dereferenced *while it is being forced* is a genuine cycle and stays
  `stuck` (the black-hole marker detects it).
- Non-recursive `Let` stays strict (no forward reference to defer); strict
  application, `Prim`, and `case` are unchanged (ADR-0002).

By-need needs no static analysis of which members forward-reference, and is robust
to the dependency being *dynamic* — hidden, as here, inside a called function
(`ap`) that forces a superclass thunk. Laziness is confined to the recursive knot,
exactly where PureScript's semantics require it.

## Consequences

- Mutually recursive dictionaries resolve: forcing `applicativeEffect` during
  `applyEffect`'s construction builds it on demand (its own fields are
  `pureE` and a superclass thunk, so it completes without forcing back), then
  memoises. `Effect` — and any `ap`/`liftA1`-based instance — runs, unblocking the
  ADR-0023 `Effect` E2E (`Effect.Ref`, `Effect.Console`).
- Genuine cycles still get `stuck`: `letrec x = x`, mutually-referential *values*
  with no intervening thunk — they dereference themselves mid-force. The existing
  black-hole tests keep their meaning.
- Top-level recursive groups become by-need (forced on first use, not at link
  time). For the pure bindings we link this is observationally identical to eager
  initialisation (the ADR-0021 DCE faithfulness argument); an effectful CAF would
  run its effect once, on first demand.
- Cost: one continuation frame and one memoising write per recursive binding on
  first force; it reuses the existing store back-patch seam.
- Forward path: once the lower IR has arity-aware/uncurried calling and/or
  `DictElim`, the eager construction graph becomes a DAG and this can *degrade*
  to purs-wasm's lazy-free topological-sort-plus-knot-tying. By-need at the oracle
  is the always-correct baseline that does not wait for those passes.

## Alternatives considered

- **Selective `$runtime_lazy`** (wrap only forward-referencing members, as the JS
  backend does). Correct, but it needs a per-binding "does this forward-reference"
  analysis; uniform by-need is simpler for the oracle and the analysis buys no
  observable difference here.
- **Topological sort of the eager construction graph** (purs-wasm's route).
  Insufficient for the *curried* oracle: the real eager dependency
  (`applyEffect → applicativeEffect`) is dynamic — it lives inside `ap`'s body
  forcing a superclass thunk — so a static syntactic sort of the group cannot see
  it. It becomes viable only with arity-aware capture or `DictElim`; revisit at
  the lower IR.
- **Knot-tying** (allocate placeholders, back-patch siblings). Same limitation:
  it assumes construction only *captures* sibling references, but our curried
  projection *reads their contents*, so a placeholder would be deconstructed.
- **An `undefined`/bottom placeholder value.** Pollutes the value space with a
  bottom and defers errors unpredictably; and (contrary to an earlier draft) it
  is not what the JS backend does. Rejected.
- **General laziness everywhere.** Unnecessary and contrary to the strict core
  (ADR-0002); by-need *only* for recursive bindings is the targeted change.
