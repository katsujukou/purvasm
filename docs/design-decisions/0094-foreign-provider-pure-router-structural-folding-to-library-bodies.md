# 0094. Foreign provider is a pure router; structural folding moves to library bodies

- Status: ~~Proposed~~ **Accepted** _(2026-07-12: accepted by the maintainer)_
- Date: 2026-07-12

## Context

The compiler's foreign provider (`Purvasm.Compiler.Ffi`, ported from boot's
`Ffi`) resolves a foreign identifier through a ladder: `resolver key =
intrinsicTerm key <|> structural key`. Two of these rungs carry very different
kinds of thing:

- **`intrinsics`** — eta-expanded primops (`Purvasm.Int.add` → `\a b -> Prim(AddInt,[a,b])`),
  compiler-builtin literals, `unsafeCoerce` = identity. These are *representation
  facts*: the compiler owns them because they have no library implementation.

- **`structural`** — **guest terms** for higher-order/first-order-composed
  foreigns: `Data.Ord.{ordInt,ordNumber,ordString,ordChar,ordBoolean}Impl`,
  `Data.Eq.eqArrayImpl`, `Data.Functor.arrayMap`, plus the `Effect.*` / `Ref` /
  `ST.*` thunk-model combinators and the Fn-uncurried / `Record.Builder`
  adapters. These are *implementations* the compiler embeds as `TmApp`/`TmIf`/
  `TmPrim` compositions.

With native FFI now working (ADR-0073/0090/0091/0092), the pure-PS library layer
(ulib) already **shadows** most of the pure `structural` entries: `arrayMap`,
`eqArrayImpl`, and the `Data.Ord.*Impl` family are provided by ulib as ordinary
PureScript definitions over `purvasm-base`. Real programs never emit these
foreigns — the shadow bodies do the work. The `structural` guest terms are, for
those keys, **dead as a link-time provider**.

They are, however, **not dead as an optimiser input**. The NbE optimiser
(`MiddleEnd.Optimizer.Nbe`) imports `Ffi.resolver` and uses a structural guest
term as an on-demand *compiler-global structural rung* to **constant-fold** a
saturated foreign call — e.g. `ordIntImpl lt eq gt 1 2` folds to a constant on
known operands (ADR-0089 §1). Deleting the `structural` entries therefore breaks
the optimiser's folding and its `unfolds ordIntImpl …` test, even though the
entries are otherwise dead. This coupling is the design fork this ADR resolves.

The problem is a **responsibility overlap**: the foreign provider carries a
second job (supplying implementation bodies to the optimiser) on top of its
first job (routing a foreign to its provider). The overlap is why removing dead
routes is not a local change.

## Decision

This ADR sets a **direction** and executes the **first, safe slice** of it. The
two are kept explicitly separate so the slice can land without waiting on the
larger structural migrations.

### Final target (direction, not fully realised by this ADR)

The foreign provider should become a **pure router**: it maps a foreign
identifier to *where its implementation lives*, and returns no implementation
body of its own. In the end state:

`Ffi.resolver` makes exactly one distinction: **`Just term`** — the compiler
owns this foreign as an intrinsic (its remaining, legitimate job) — versus
**`Nothing`** — the compiler does not own it, so it is a **native leaf** whose
`pvf_<key>` symbol some *provider* supplies at link. Which provider is decided
downstream (FSR shape + link plan), not by `resolver`:

```
provider ladder (conceptual, across the whole system):
  compiler intrinsic   Ffi.resolver = Just   (primOp eta / builtin literal /
                                              unsafeCoerce = identity; pv_* runtime API)
  native leaf          Ffi.resolver = Nothing, symbol pvf_<key> supplied by:
    · runtime          (pvf_ in leaf.rs, ADR-0073)
    · ulib             (a ulib package's own native C .c, e.g. showNumberImpl —
                        ADR-0073 §3 + the ulib.json `foreign` map)
    · in-app           (the app's own native leaf, ADR-0091)
  host-resolved        Ffi.resolver = Nothing, no native provider (dropped, JS/host)
```

**A ulib package plays two distinct roles — do not conflate them:**

1. **Pure-PS shadow (source overlay).** It re-implements a registry package's JS
   foreign as ordinary PureScript over `purvasm-base`. The foreign then **is
   never emitted into CoreFn**, so the key never reaches `resolver` — there is no
   rung, and nothing to route. This is the role the sliced keys use.
2. **Native (C) leaf provider.** For things that *genuinely cannot* be pure PS,
   the ulib package still declares a `foreign import` and ships its own native
   `.c` (e.g. `showNumberImpl`). Here the foreign **does** reach `resolver`, which
   returns `Nothing` (a native leaf), and the ulib `.c` supplies `pvf_<key>` at
   link. This *is* the "ulib foreign" rung above.

So "ulib" is not synonymous with "source overlay": a shadow module may itself
contain native leaves. The mental-model correction is narrower than a blanket
"shadow ⇒ no resolver": *a pure-PS re-implementation* removes the foreign
upstream (role 1); *a ulib native leaf* is routed like any other native leaf
(role 2).

Reaching the pure-router end state also requires relocating or reclassifying the
structural entries this ADR does **not** touch (see *Scope* below). Those are
deferred to their own ADRs; the strong invariant is stated as a *target*, not as
something this ADR alone establishes.

### Scope of this ADR (the slice)

This ADR retires only the **pure structural entries already shadowed in ulib**:
`Data.Ord.{ordInt,ordNumber,ordString,ordChar,ordBoolean}Impl`,
`Data.Eq.eqArrayImpl`, `Data.Functor.arrayMap`. For these:

1. **Their bodies already exist as ulib pure-PS shadows** (or gain one where a
   gap remains), e.g.

   ```purescript
   ordIntImpl lt eq gt x y =
     if PI.lt x y then lt else if PI.eq x y then eq else gt
   ```

   which lowers to the same `LtInt`/`EqInt` primops the guest term used.

2. **The optimiser folds those library bodies, not provider terms.** NbE sees the
   ulib shadow's CoreFn body as a normal **dependency / inline candidate**
   (`shouldInlineExternApp`, ADR-0089) and folds it on known operands through the
   ordinary intrinsic-primop path. The `structural` rung's role in NbE for these
   keys is removed.

**Explicitly out of scope of this ADR** — the structural entries that remain in
`Ffi` after this slice, each deferred to its own future decision:

- `Data.Number.fromStringImpl` — ADR-0092 deliberately keeps it a **compiler
  structural intrinsic** (it wires `Fn4`/`isFinite`/`just`/`nothing` around the
  `Purvasm.Number.parseFloat` native leaf); not a pure-body relocation.
- `Record.Builder.unsafe{Modify,Rename}` and the `Fn.Uncurried` / `Partial`
  (`_unsafePartial`) adapters — pure adapters, candidates for the same
  shadow-relocation treatment, but not covered here.
- `Effect.*` / `Ref` / `ST.*` — a control-flow lowering, not a pure-body
  relocation, owned by the optimiser's effect analysis (ADR-0034 GER /
  Impurification).

### Invariants

- **I1 (target) — provider carries no implementation.** In the end state
  `Ffi.resolver` returns only a routing classification and never a guest `Term`
  that composes primops to implement library semantics. (Intrinsic *literals* and
  the identity `unsafeCoerce` remain, as representation facts, not library
  implementations.) *This ADR advances I1 for the sliced keys only; the remaining
  structural entries above keep it from holding globally until their ADRs land.*
- **I2 (established here) — the optimiser's fold source is library bodies.** For
  the sliced keys, constant-folding happens because the ulib shadow body is a
  visible dependency candidate, not because the provider handed the optimiser a
  term.

## Rollout (ordering is load-bearing)

The migration must **prove fold-parity before deleting the old rung**, so that a
program that used to fold a scalar comparison to a constant still does:

1. Add/confirm the ulib pure-PS shadow body for each retired key
   (`Data.Ord.{ordInt,ordNumber,ordString,ordChar,ordBoolean}Impl`,
   `Data.Eq.eqArrayImpl`, `Data.Functor.arrayMap`).
2. Add an **end-to-end fold-parity harness** with **per-key expectations** (a
   whole-pipeline test, not a unit test over a synthetic body), because the keys
   do not all fold the same way:
   - `Data.Ord.{ordInt,ordNumber,ordChar,ordBoolean}Impl` — with the shadow in
     the dependency closure and **known operands**, the call must fold to a
     **constant** (their primops `LtInt`/`EqInt`/`LtNumber`/… fold on literals).
   - `Data.Ord.ordStringImpl` — `LtString` follows the non-folding policy, so a
     constant is **not** the expectation here. Assert instead that the shadow
     **body is visible and reduces to the expected intrinsic shape** (the
     `LtString`/`EqString` structure), i.e. parity with what the guest term
     produced, not a folded literal.
   - `Data.Eq.eqArrayImpl` / `Data.Functor.arrayMap` — array operands are rarely
     compile-time known; assert body visibility / correct lowering rather than a
     folded constant.
3. Only after (2) is green: delete **the sliced keys** from `Ffi.structural`
   (and their now-unused guest-term helpers `arrayMap`/`eqArray`/`ordCmp`/
   `ordBoolean`), and delete/replace the `unfolds ordIntImpl …` structural-rung
   test with the ulib-body-fold positive test.

   `NbeEnv.structural` itself is **not** deleted by this slice: the NbE rung
   consumes `resolver` generically and still serves the out-of-scope structural
   keys (`fromStringImpl`, `Record.Builder`, `Fn.Uncurried`/`Partial`,
   `Effect`/`ST`). It is retired only in the **final target**, once the last
   structural entry is relocated by its own ADR. For the sliced keys the rung
   simply goes quiet — `resolver` returns `Nothing`, so the shadow body takes
   over.

The sliced `structural` entries may live on **briefly as a transitional bridge**
between steps 1 and 3; they must not outlive step 2. Delete-first is prohibited —
it would hide a silent optimisation regression.

## Consequences

### Benefits

- The provider moves toward its single responsibility (route, don't implement),
  matching the "provider is a road sign" principle behind ADR-0073/0091 — for the
  sliced keys; the remaining structural entries follow in their own ADRs.
- For the sliced keys, one implementation per library function (the ulib PS body)
  rather than a guest-term copy in the compiler plus a shadow in ulib. No drift
  between them.
- The optimiser folds *the code that actually runs*, closing the gap where a
  guest term and its shadow could diverge.

### Risks / caveats

- **R1 — "guaranteed fold" becomes "fold if the inline gate passes."** The old
  structural rung was *compiler-global* and folded unconditionally. The new path
  rides NbE's cross-module extern-body inline gate (Complexity/Usage/size,
  ADR-0089). For bodies as small as `ordIntImpl` this is fine, but it is a real
  semantic shift: a body just over the size threshold would emit a runtime call
  where it previously folded. **Mitigation:** keep the shadow bodies minimal; run
  a benchmark regression check (scalar compare/eq folding is a real perf lever,
  per the project's bench-regression discipline). **The existing `--opt-effect`
  gate — the 5 benchmarks + self-compile — already covers this** (optimiser-team
  note, 2026-07-12): `fib`/`quicksort` are live regression detectors for scalar
  compare folding, so reuse that gate rather than standing up a separate harness.
  If a body that *must* fold
  risks missing the gate, mark it for unconditional inlining on the shadow side
  (an `@inline`-always-equivalent) rather than reviving a compiler-global rung —
  subject to the optimiser team's inline-directive design.
- **R2 — reachability of the shadow body.** Folding now requires the shadow
  module's body to reach the optimiser's dependency env. This holds naturally
  (referencing `ordIntImpl` means importing the shadow's `Data.Ord`), but the
  small pure body must be exported / propagated so NbE can see it. The parity
  harness (step 2) is what verifies this per key.
- **R3 — delete-first regression.** Addressed by the rollout ordering above.

### Out of scope — Effect/ST

The `Effect.*` / `Ref` / `ST.*` structural combinators are **not** relocated to
ulib bodies by this ADR. Lowering `whileE`/`forE` to loops at the backend is too
late (it duplicates work across the VM and LLVM backends and misses the effect
context). These belong to the optimiser's effect analysis — nullary-thunk
resolution and control-flow lowering via GER / Impurification (ADR-0034) — and
are tracked on that separate, heavier timeline. This ADR must not become a
prerequisite that holds the small pure-structural cleanup hostage to the large
effect-lowering design.

## Alternatives considered

- **Keep the structural rung solely for the optimiser.** Rejected: it re-mixes
  the provider's responsibilities (route + implement) — the exact overlap that
  produced the coupling — and keeps two copies of each library body.
- **Delete the structural entries now, fix the optimiser later.** Rejected:
  delete-first turns a fold into a silent runtime call with no failing test to
  catch it (R3).
- **Move Effect/ST to ulib bodies too, uniformly.** Rejected: effects are a
  control-flow lowering, not a pure body relocation; conflating them couples a
  small safe cleanup to ADR-0034's large design.

## Affected components

- `compiler/src/Purvasm/Compiler/Ffi.purs` — retire the **sliced** `structural`
  entries (`Data.Ord.*Impl`, `Data.Eq.eqArrayImpl`, `Data.Functor.arrayMap`) and
  their now-unused guest-term helpers (`arrayMap`, `eqArray`, `ordCmp`,
  `ordBoolean`). The out-of-scope structural entries stay.
- `compiler/src/Purvasm/Compiler/MiddleEnd/Optimizer/Nbe.purs` — the
  `structural` rung stops matching the sliced keys (via `resolver` returning
  `Nothing`); `NbeEnv.structural` itself stays until the final target retires the
  last structural entry (optimiser team).
- ulib pure-PS shadow modules — host the relocated bodies (ulib team).
- Tests — replace the structural-rung fold test with an end-to-end
  ulib-body-fold parity test.

## Progress (2026-07-12): the slice is implemented — rollout steps 1–3 complete

- **Step 1 (confirm shadows)**: all three overlay corefn artifacts (`dist/ulib/{Data.Ord,Data.Eq,
  Data.Functor}`) carry **zero foreigns**. One factual correction surfaced: the shadow `Data.Ord`
  does not define `ord*Impl` bindings at all — its instances implement `compare` directly (and
  string compare is a **recursive UTF-8 byte loop**, `compareStringImpl`, byte-exact with the VM
  order by construction, not an `LtString` composition). The overlay world therefore contains
  the sliced `Data.Ord` keys in *no* form; `Data.Eq.eqArrayImpl`/`Data.Functor.arrayMap` remain
  as ordinary pure-PS bindings.
- **Step 2 (fold-parity harness)**: `Test.E2E.Purvasm.Compiler` gained an ADR-0094 parity
  section that loads the **real artifacts** by the build's own overlay-first resolution and
  threads them through the real optimiser seam. Per-key: Int `compare`/`lessThan` on known
  operands fold to the `LT`/`true` constants; string compare survives as a call into the shadow's
  recursive byte-loop body (the `LtString`-shape expectation above was registry-minded — corrected
  to body visibility); `arrayMap` admits two correct lowerings (the shadow body is a
  *non-recursive wrapper* over an internal loop, so it may inline — the residual then carries the
  loop's array primitives — or stay a call); and a **corpus scan** asserts no overlay corefn
  declares any sliced key as a foreign (dead as a provider, mechanically).
- **Step 3 (delete)**: the seven `structural` entries and their four guest-term helpers
  (`arrayMap`, `eqArray`, `ordCmp`, `ordBoolean`) are deleted; the `unfolds ordIntImpl`
  structural-rung unit test is replaced by a negative (a retired key is untouched by any
  compiler-global rung), with the E2E harness as the positive.

**Measured: zero movement.** All five bench instruction counts are byte-identical before and
after the deletion (fib 3.131 / count-state 1.801 / map-fold 1.896 / quicksort 1.866 /
json-parse 2.980), self-compile size ratio 1.111 unchanged, `.pmi` 564/564, all gates green —
I2 had in fact already held: in the overlay pipeline the folds always came from the shadow
bodies, and the sliced rung was dead weight. R1's "fold if the gate passes" shift is therefore
confirmed costless for these keys at the current thresholds, with `--opt-effect` (fib/quicksort)
standing as the live regression gate.
