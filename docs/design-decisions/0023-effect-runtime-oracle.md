# 0023. Effect runtime at the CESK oracle: thunks, IO leaves, no reflection

- Status: Accepted
- Date: 2026-06-20

## Context

The next capability is `Effect` — PureScript's type of native, synchronous side
effects. In the `effect` package, `Effect a` is a **nullary thunk** `Unit -> a`:
`pureE a = \_ -> a`, `bindE a f = \_ -> f (a unit) unit`, and the combinators
(`untilE`, `whileE`, `forE`, `foreachE`) are likewise pure thunk-plumbing that run
sub-effects in a loop. `foreign import data Effect` is a type — erased at runtime.
So the whole `Effect` *monad* is ordinary first-order guest code over closures;
nothing in it performs a side effect by itself.

purs-wasm refined this with **effect reflection / impurification / generalized
effect reflection (GER)**: effects reify to explicit `Reflect`/`Perform` nodes
(`pure a ≡ Reflect (() => a)`, `bind e f ≡ Reflect (() => let a = Perform(e) in
Perform(f))`, and even opaque foreign effects via GER, `log s ≡ Reflect (() =>
Perform(log s))`). Those nodes exist so that *impurification* — erasing them when
compiling down — can keep the impure barrier and not let an optimiser break effect
semantics (e.g. inlining `void` to `let _ = eff() in …`, then dead-assignment
elimination dropping `eff()`).

That machinery is an **optimiser** concern, and it does not belong at the CESK
oracle, for two reasons:

- The hazard only arises under optimisation. The CESK machine is strict and
  performs no inlining, DCE, reordering, or duplication, so every `Effect` thunk
  is forced exactly when the program demands it, exactly once, in order — `void`
  still runs its effect. The oracle is sound by construction.
- Before dictionary inlining (which the oracle does not do), `bind`/`pure` flow
  through the `Monad Effect` dictionary, so `bindE`/`pureE` are not even
  syntactically identifiable. Recognising effects to reify them is a post-inlining
  (optimiser) pass.

The CESK oracle therefore *defines* the effect-soundness contract that a future
impurification must preserve, rather than implementing reflection itself.

## Decision

Run `Effect` at the CESK oracle with no new evaluation machinery beyond a single,
scoped admission of real side effects:

- **Effect monad + combinators are structural guest terms** (ADR-0020 rung):
  `pureE`, `bindE`, `untilE`, `whileE`, `forE`, `foreachE` as hand-written
  `Cesk.Ast` over application, `if`, and a guest loop. `Effect a` is just a
  function value; correct sequencing falls out of strict evaluation of `bindE`.
- **Effectful leaves are native rung IO foreigns** (ADR-0022): a leaf such as
  `Effect.Console.log` resolves to a host function that returns the `Effect`
  thunk; forcing that thunk performs the real host effect and returns the result.
  This is the CESK realisation of GER — the thunk's force *is* the `Perform`. The
  `host` registry's `call : Value.t list -> Value.t` already permits this; an
  effectful entry is simply one whose `call` performs IO. No type change.
- **Runtime driver**: a top-level `main :: Effect Unit` is run by evaluating it
  to its thunk and applying it to `unit` (the immediate `0`, ADR-0017). The
  machine's
  ordinary `run` then drives the effects to completion.

Scope of this slice:

- **`Effect.Ref` is the primary, value-observable vehicle** (`new`/`read`/
  `write`/`modify`): a program like `do { r <- new 0; modify (_+1) r; read r }`
  returns `1`, so sequencing and state are asserted as a returned value, with no
  stdout capture. A `Ref` is represented as a **one-cell mutable array** (ADR-0019)
  — no new value form.
- **`Effect.Console.log`** is provided for the canonical demo, with one
  stdout-capturing test. Leaf foreign idents are pinned against the `effect` /
  `console` / `refs` package sources, not guessed.

The single semantic change: `step` performs real IO when an effectful leaf's thunk
saturates — that transition alone is no longer pure. Everything else is unchanged.

The oracle's **effect-soundness contract** (the reference a later optimiser's
impurification/GER must preserve): *effects execute in program order, and each
`Perform` runs exactly once — never eliminated, never duplicated.*

## Consequences

- "Hello, world" and stateful pure-`Effect` programs run end-to-end with no
  changes to the CESK transition rules — only new ladder entries (structural
  Effect ops, native effect leaves) plus a one-line runtime driver.
- The reflection/impurification/GER design is deferred to the lower IR (ANF +
  optimiser); this ADR records the soundness contract it must meet and names
  purs-wasm's design as the inheritance, so that work starts from a defined
  oracle rather than first principles.
- pause/resume is unaffected: already-performed effects stay performed and the
  reified continuation carries the rest. But a snapshot replayed from an earlier
  state **re-runs** the effects between the snapshot and now — a caveat for any
  future time-travel/replay, called out here so it is a known property, not a
  surprise.
- The machine is no longer a pure function of its input once effectful leaves are
  registered. This is the `Effect`-typed boundary the PureScript type system
  guards; effectful host entries are the `unsafe` core wrapped by safe,
  `Effect`-typed APIs (per the project's convention).

## Alternatives considered

- **Explicit `Reflect`/`Perform` nodes in the upper IR.** Make the effect barrier
  syntactic in `Cesk.Ast` even at the oracle. Rejected for now: the oracle does
  not optimise, so the nodes would be identity wrappers earning nothing; and
  effects are not identifiable pre-inlining, so the nodes could not be produced
  by the current (type-erased, un-inlined) lowering anyway. They belong to the
  optimiser, with this oracle as their correctness reference.
- **World-passing / `RealWorld` token threading.** Model effects as pure state
  transitions over an explicit world. Pure, but heavy, and diverges from
  PureScript's `Effect = Unit -> a` representation, so the oracle would no longer
  mirror the source semantics it is meant to certify.
- **Free-monad / reified-command interpretation.** Reify effects as data and
  interpret them in a driver outside the machine. Elegant and keeps `step` pure,
  but PureScript's `Effect` is native thunks, not a free monad; this would be a
  different language, not a faithful oracle.
- **Stdout-only first effect (`Console.log`).** Iconic but awkward to assert in
  unit/E2E tests. `Effect.Ref` gives value-observable effects, so it leads; the
  log demo follows.
