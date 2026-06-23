# 0034. Effect-aware optimisation: structural effect analysis, the force/saturation soundness model, and impurification (GER)

- Status: Accepted
- Date: 2026-06-22

## Context

The optimiser will need to know which expressions **perform** an effect — to do a
sound dead-binding elimination (never drop a `let` whose right-hand side may perform)
and, eventually, **impurification (GER)**: rewriting the thunked `Effect` monad
(ADR-0023) into direct imperative bytecode, removing the per-action closure/force
overhead. `Effect` is erased to thunks, so the question is "where does an effect
fire", not "what is the type".

An earlier sketch assumed effect classification must come from `externs.cbor`
(types), forcing two heavy prerequisites: a CBOR/externs decoder and a `.purs` CST
parser (externs is public-only). The `purs-wasm` author's debrief corrected this: in
`purs-wasm` purity was **always a leaf bit from the host registry**, not type-bearing
— types were load-bearing only for *marshalling* and *arity*, and the externs/CST
walls were the by-product of bundling purity + arity + marshalling into one
foreign-signature plus externs being public-only. So purity needs neither wall:

- **private** bindings have their CoreFn bodies (analyse them directly);
- **imported** bindings' purity is computed when *they* are compiled and travels in
  their `.pvmi` (ADR-0033);
- **body-less foreign** leaves are classified by the host registry's effectful bit,
  or conservatively `may-perform`.

The **real minefield** (per `purs-wasm`'s actual bugs) is not the purity bit but the
**force/saturation model** — exactly where in an `Effect` thunk an effect fires.
This ADR puts that model first-class.

## Decision

Compute effects structurally, codify the force/saturation soundness model as
first-class invariants, and build impurification (GER) on top — all without
`externs.cbor`/CST (deferred to a precision-only follow-up, "ADR B").

### Structural effect analysis

A per-binding `may-perform` classification, **dependency-directed** (a binding's
class is a function of its callees' classes — never its callers,
[[optimizer-modular-not-whole-program]]):

- **Leaf bits.** The host registry (ADR-0022) is the source of truth for which native
  leaves perform (`Effect.Console.log` performs; `Data.Show.*`, arithmetic primops do
  not). A *pure* user foreign (e.g. `Math.sqrt`) can be registered here as a pure leaf,
  so it escapes the conservative `may-perform` default below — a cheap v1 precision win
  that needs no externs (ADR B).
- **Combinator recognition (+ pass order).** Recognise `Effect`'s `do`-desugaring
  combinators by their known names (see **I2**) so the effect structure is read from
  the term, not a type. At CoreFn level these are typeclass methods
  (`Control.Bind.bind dict` / `Control.Bind.discard dict`); the recognition key is
  either the dict-resolved concrete name (`Effect.bindE`, …) or the unresolved
  `Control.Bind.bind`/`discard` with a check that its dictionary is the `Effect`
  instance — which one depends on pass order vs DictElim (either is acceptable, fix it
  in the implementation). **Conservative rule:** an unresolved `bind`/`discard` whose
  dictionary is *unknown* is `may-perform` (it might be `Effect`) — the same
  conservatism as effect polymorphism below, applied to the combinators themselves.
- **Cross-module propagation via `.pvmi`.** Each export carries its `may-perform` bit;
  a module's analysis consumes its imports' bits from their `.pvmi`. Private bodies
  are analysed locally; body-less foreigns use the leaf bit or default `may-perform`.
- **Recursive groups: least fixpoint.** The module graph is acyclic, so purity folds
  bottom-up across modules; but an intra-module self-/mutually-recursive group (an SCC,
  e.g. `go n = log n *> go (n-1)`) has members whose class depends on their own.
  Compute each SCC by a **least fixpoint** (initialise `pure`; bump a member to
  `may-perform` when it reaches a perform-callee; iterate to convergence). Without it
  the analysis loops, or misclassifies a recursive effectful branch as `pure` and DBE
  deletes its effects.
- **Conservative by default, sound over-approximate.** An unknown call, and
  **dictionary-passing effect polymorphism** (`traverse`, `liftEffect`/`MonadEffect`,
  transformer stacks — where `m = Effect` is decided by the incoming dictionary, not
  visible at the definition) are `may-perform`. This is sound; precision is recovered
  once a monomorphisation/specialisation pass exists (run **specialise →
  effect-analyse**, as `purs-wasm`'s caller-homed specialisation did). purvasm has no
  specialiser yet, so those sites stay conservative for now (noted, not blocking).

### The force/saturation soundness model (first-class)

ADR-0023's contract — *effects run in program order; each `Perform` runs exactly
once; never eliminated, duplicated, or reordered* — is realised by these invariants,
which **every** effect-touching transform (DBE, GER) must preserve. Each comes with a
differential test obligation; `purs-wasm`'s real bugs clustered here.

- **I1 — construction ≠ execution.** Building/passing/storing an `Effect` *value* is
  pure; the effect fires only at the **saturating force** (apply-to-unit that reaches
  a perform leaf). `may-perform` is judged at that force point. (`pure (log "x") ::
  Effect (Effect Unit)` is pure outside, inner unforced.) Mis-locating the force point
  breaks soundness immediately.
- **I2 — recognition completeness, incl. `discard`.** The recognised set must cover
  *all* of `do`-desugaring: `bind`/`bindE`, **`discard`** (`Control.Bind.discard` for
  `do { eff; rest }`), `pure`/`pureE`, and the loop combinators
  (`forE`/`foreachE`/`whileE`/`untilE`), plus the perform leaves. Dropping `discard`
  silently deletes effects (a `purs-wasm` bug).
- **I3 — voided / conditional effects preserved.** `void eff`, and `when`/`unless` on
  a runtime `Bool`, *execute* (possibly conditionally) even though the result is
  discarded. Folding must keep the executed-but-discarded and conditional branches —
  never eliminate as dead, never trap.
- **I4 — bare-CAF Effect and combinator arity/saturation match.** A top-level
  `main :: Effect Unit` (a bare `Effect` CAF) must be performed by the driver
  (apply-to-unit, ADR-0032). And — purvasm uncurries (adjacent lambdas merge to a
  multi-arg `CLam`, `Transl.collect_lam`, ADR-0025) — a normalised effect *body*'s
  arity (e.g. Abs-merged to 2) must match the saturation convention of the combinator
  (`forE`/`foreachE`/`whileE`) driving it. The loop combinators are currently
  *structural guest terms* run through the dynamic eval/apply protocol (ADR-0025), so a
  mismatch does **not** trap like `purs-wasm`'s typed `call_ref` (its hardest spot);
  instead eval/apply mis-saturates — a `Vpap` lingers where a value was expected, or
  the body is over-applied — yielding a wrong (silent) result. So GER must still keep
  the reconstructed effect-body arity consistent with how the driving combinator
  saturates it; the symptom here is a wrong result, not a hard cast error.

### Accepted semantic relaxation: partial correctness (a non-goal)

purvasm's IR is **strict (CBV)** — a `let`'s right-hand side is evaluated even when
unused (only recursive-group bindings are by-need, ADR-0024). So besides `Effect`,
two non-`Effect` observables exist at *pure* positions: **divergence** (a pure
non-terminating RHS) and **partiality** (a pure crash — out-of-bounds, ill-typed,
`unsafeCrashWith`-style; in purvasm these are a host `stuck`, *not* guest-catchable).
Dropping a dead, pure, diverging/throwing unused binding turns "hang/crash" into
"complete" — so DBE is **not** full observational equivalence.

**This relaxation is accepted, deliberately.** DBE preserves **partial correctness**:
for runs that terminate and do not crash, the `Effect` order and all values are fully
preserved; a *dead, pure* `diverge`/`throw` at a pure position may be removed.
Rationale (the mainstream choice — GHC, purs-backend-es do the same): termination is
undecidable, dead partials are vanishingly rare in real code, and forgoing the
thunk-elimination / GER opportunity to chase them is not worth it. The preserved
property is stated precisely as *partial correctness*, **not** "soundness", so a later
implementer does not build on a false full-equivalence premise.

**Containment.** The relaxation is confined to the `pure ∧ ¬may-perform` class. An
action that *both* performs and may diverge/throw (`forever (log _)`, a throwing
`Effect`) is `may-perform`, so I1–I4 protect it (never dropped, never reordered).
"Ignore a pure Bottom" must never leak into "ignore an effectful Bottom" — which holds
automatically as long as the `may-perform` classification is correct.

**The one real leak — pure-exception reification.** If a feature lets a pure exception
be *observed as an `Effect` value* (`Effect.Exception.try`/`catchException`), then a
dead pure `may-throw` under a `try` is observably *not* dead. purvasm has **no such
catch primitive today** (partiality is a non-catchable host `stuck`), so the
fully-lenient reading is adopted now; if a `catch`/`Effect.Exception` boundary is ever
added, this must be revisited — treat `may-throw` conservatively under a catch.

### Impurification (GER)

Recognise a `bind`/`discard`/`pure`/loop tree of `Effect` actions and lower it to a
**direct imperative bytecode sequence** (perform in order), eliminating the
per-action thunk/closure. It is **opportunistic and always safe to decline**: when a
pattern is not confidently recognised, leave the thunked encoding — the VM already
runs that correctly (ADR-0032), so GER can never *break* a program, only fail to
optimise it. The first slice targets straight-line `do`-blocks (`bind`/`discard`/
`pure` + `log`/`Ref`); `when`/`unless` precision and the loop-combinator arity edge
cases (I4) start conservative — *keep the thunked form and perform it, never drop the
effect* (conservative here means "do not impurify", not "may eliminate") — and tighten
later.

### Verification

Differential equivalence vs the oracle (value **and** stdout order) on every effect
fixture plus GER-targeted fixtures exercising each invariant: straight-line
do-blocks, `discard` sequencing (I2), `void`/`when`/`unless` (I3), nested
`Effect (Effect _)` (I1), loop combinators and bare-CAF `main` (I4). The analysis is
unit-tested on expected classifications (e.g. `main` may-perform, `eval`/arithmetic
pure).

## Consequences

- A sound effect classification exists with **no `externs.cbor` decoder and no `.purs`
  parser** — both walls dissolve; they return only as a precision upgrade (ADR B).
- The force/saturation invariants become the explicit contract the normaliser and any
  effect-touching pass inherit, concentrating correctness where `purs-wasm` actually
  failed.
- DBE preserves **partial correctness** (drops only `pure ∧ ¬may-perform` bindings;
  divergence/partiality at pure positions is an accepted relaxation, *not* full
  observational equivalence — see "Accepted semantic relaxation"); GER removes
  `Effect`-monad overhead where recognised.
- Effect-polymorphic (dictionary-passing) sites stay conservatively `may-perform`
  until a specialiser exists; the analysis is built to run **after** specialisation
  when one lands.
- The general inliner/normaliser (study-gated) inherits these invariants; GER is the
  effect-aware subset and folds into it later rather than fighting it.

## Alternatives considered

- **Type-sourced purity via `externs.cbor` (+ CST for private).** `purs-wasm`'s route;
  rejected for the first cut — heavy (CBOR + version-locked externs schema; a PureScript
  parser), public-only (hence the CST hack), and unnecessary: purity is a leaf bit
  propagated through bodies + `.pvmi`. Kept as a deferred **precision** companion
  (ADR B) for effect-polymorphic pure-FFI proofs and marshalling/arity, not purity.
- **Skip the analysis; keep running effects via thunks only.** Correct (status quo) but
  forecloses sound DBE and GER; the thunk overhead stays.
- **Treat any higher-order application as `may-perform`.** Too coarse — poisons pure
  combinators (`map`/`foldr`); the force-point model (I1) plus combinator recognition
  is what keeps the approximation useful.
- **Eliminate/reorder effects freely (treat `Effect` as pure data).** Unsound — breaks
  ADR-0023's order/exactly-once contract; the whole point of I1–I4.
