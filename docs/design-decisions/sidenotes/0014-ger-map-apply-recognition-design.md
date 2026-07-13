# Design proposal: recognising the `Effect` `Functor`/`Apply` instances for GER's `map`/`apply` rewrite (ADR-0099 Slice 3)

- Date: 2026-07-13
- Status: **implemented** (option B, fail-closed, 2026-07-13) — see the ADR-0099 Slice 3 Progress note.
  One finding beyond the proposal: the clean `map functorEffect f m` **dispatch** is only seen by
  `Impurify`'s `early` pass for *direct* `map`/`apply` uses; a `void`-style use is inlined by NbE
  into a runtime `functorEffect.map` **projection** (`CAccessor`) — the group-recursive `map` field
  never folds (ADR-0098) — so `recognize` needed a **second arm** matching the `CAccessor dict field`
  projection on an Effect-family dict (caught at `close`), replacing it with the η-expanded canonical
  `map`/`apply` function. Both arms are gated on `effectFamily`. Measured payoff: `bench-effect-ref`
  `--opt` ratio 1.199 → 1.965.
- Related: [ADR-0099](../0099-generalized-effect-reflection-cperform.md) §5(a) / Slice 3,
  [ADR-0098](../0098-effect-instance-dict-visibility-grouped-projection.md) (the recursive
  `Effect` instance group), the `bench-effect-ref` measurement below.

## 1. What the measurement decided

The new `bench-effect-ref` effect-hot instrument (a `forE` loop whose body is
`when (even i) (void (Ref.modify (_ + i) acc))`) was compiled at `--opt` and its
`Bench.EffectRef.Main.pmo` inspected. The dictionary surface splits cleanly:

- **Collapses already (Slice 2 + NbE), no Slice-3 work needed:** `bind`, `discard`,
  `pure`, `when`, `unless`. The `discard` binding is *byte-identical* to the `bindE`
  canonical `CPerform` tree (`discard = bind`); `when` collapses to `case b of { true →
  act; false → \$u -> unit }` — the `else` branch is `pure unit` = `Effect.pureE unit`
  which GER lowered to the unit thunk `\$u -> unit` (no `Perform` — `pure` does not
  perform; the `Perform`s come from `bind`/`discard`).
- **Does NOT collapse — the Slice-3 headroom:** `map` / `apply` (the `Functor` / `Apply`
  surface). `void` survives as a residual binding

  ```
  Bench.EffectRef.Main.void = \x -> (ld Effect.functorEffect; gf "map") (const unit) x
  ```

  i.e. a per-use **field projection of `map` off the `Effect.functorEffect` instance
  dictionary**, then a call. The `functorEffect` dict CAF is materialised and its `map`
  field projected at runtime every iteration.

So Slice 3's value is concentrated in the `§5(a)` **direct `functorEffect.map` /
`applyEffect.apply` rewrite**. `discard`/`when`/`unless` need no dedicated handling
(maintainer-confirmed scope: option 1 — map/apply only).

## 2. Why this is hard: the impls are generic and the superclass link is a hidden local

`Effect`'s `Functor`/`Apply` instances derive their methods generically:

```
instance functorEffect :: Functor Effect where map   = liftA1
instance applyEffect   :: Apply   Effect where apply = ap
```

So the instance CAF is (post-normalisation) roughly

```
functorEffect = let v = liftA1 applicativeEffect in { map: v }
```

`liftA1` needs the `Applicative` dict; the instance bakes `applicativeEffect` in. Two
consequences for recognition:

1. **The `map` impl is not a GER key.** Unlike `applicativeEffect.pure = Effect.pureE`
   and `bindEffect.bind = Effect.bindE` (direct GER foreigns Slice 2 already recognises),
   `functorEffect.map`'s impl is `liftA1 applicativeEffect`. `Impurify.recognize`
   (descriptor-of-impl) returns `Nothing` for it.

2. **The superclass link is opaque across modules.** `machineryOf` stores
   `instances["Effect.functorEffect"]["map"] = AtomVar "v"` — the *local* binder of the
   `Effect` module's `functorEffect` CAF (its value `liftA1 applicativeEffect` is a
   `let`-rhs, not carried into the instances map). In a **consumer** module
   (`bench-effect-ref`), that `v` is unresolvable — which is exactly why `DictElim`
   declines `map functorEffect` today. **The consumer's machinery cannot, on its own,
   link `functorEffect` to `applicativeEffect`.**

Recognition therefore cannot key on "the impl reaches a GER key" from the consumer side
without new published information. The question is *what* to publish / where to
recognise.

## 3. Options

### A. Effect-family fixpoint over the machinery (rejected as primary)

*Rule:* an instance is Effect-family iff a field impl is a GER key, or a method impl
applies a helper to an Effect-family dict.

*Blocker:* the second clause needs to see `functorEffect.map = liftA1 applicativeEffect`,
but the consumer's machinery only has the opaque local `v` (§2.2). Works **only in the
defining module** (`Effect`), where `v`'s value and `applicativeEffect` are local — see
option C. Not viable consumer-side without publishing the superclass link.

### B. Rec-group anchor, **fail-closed** (recommended)

*Rule:* the `Effect` instances (`functorEffect`, `applyEffect`, `applicativeEffect`,
`bindEffect`, `monadEffect`) are **one mutually-recursive binding group** (ADR-0098's
premise). But shared Rec-group membership is **not by itself** a proof that a dict is an
`Effect` dict — a recursive group could mix instances of unrelated classes, and lowering
*another* type's `map` as `Effect`'s `map` is **wrong code**, not a harmless false
positive. The earlier "holds `pureE` and `bindE` somewhere" test had a hole (review
round 2, P1): a group `{pure: Effect.pureE}, {bind: Effect.bindE}, {map: unrelatedMap}`
passes it, yet admitting it would rewrite `unrelatedMap` as `Effect`'s `map`. So v1
pins a **full structural-ABI match of the whole group's shape** — fail-closed on any
surplus, deficit, or duplicate.

The `Effect` `Monad`-hierarchy group has this exact field-name-set multiset (verified
against the compiled `Effect` corefn — method fields plus `purs`'s superclass-accessor
fields):

```
{ map }                        -- Functor
{ apply, Functor0 }            -- Apply       (Functor superclass)
{ pure,  Apply0 }              -- Applicative (Apply superclass)
{ bind,  Apply0 }              -- Bind        (Apply superclass)
{ Applicative0, Bind1 }        -- Monad       (Applicative + Bind superclasses, no method)
```

A Rec group is admitted as Effect-family **iff all** hold:

- **Size + membership:** the group has **exactly five** members, and every one is a
  dictionary present in `machinery.instances` (no unknown / non-dict member).
- **Shape:** the multiset of the five members' field-name-sets equals the multiset above
  **exactly** — one `{map}`, one `{apply, _}`, two `{_method_, _}` distinguished as
  `{pure, _}` and `{bind, _}`, one `{_, _}` method-free two-superclass dict. A **surplus**
  member (e.g. the stray `{map: unrelatedMap}`), a **missing** shape, or a **duplicated**
  shape all fail.
- **Anchors:** the `{pure, _}` member's `pure` field value is exactly `Effect.pureE`, and
  the `{bind, _}` member's `bind` field value is exactly `Effect.bindE` — the two
  `StructuralGuest` GER descriptors (`unsafePerformEffect` / `EffectEliminator` is **not**
  an admissible anchor).
- **Unambiguous:** at most one Rec group in scope matches; two matches (never expected)
  fail closed.

If any gate fails the group is **not published** (GER falls back to the Slice-2 collapse,
which is correct if slower). Only a fully-matching group is Effect-family; then all five
members — including `functorEffect`/`applyEffect`, whose own fields are generic
`liftA1`/`ap` — are admitted, and GER rewrites `map`/`apply` projected off any member.

(Robustness note: the *method* field names `map`/`apply`/`pure`/`bind` are stable class
methods; the superclass-accessor names `Functor0`/`Apply0`/… are a `purs` convention.
The matcher may key the shape on the method fields + per-dict superclass-field *count*
rather than the exact superclass names, to survive a `purs` renaming without loosening
the fail-closed guarantee. If even this proves brittle, an **explicit dict-name
descriptor** — hardcoding `Effect.functorEffect`/`applyEffect` — is *safer* than a
partial structural check, and is the accepted fallback despite ADR §5's
no-name-hardcoding aspiration, which item-4 generalisation revisits.)

*Honesty about what this proves:* this validates the **structural ABI of the
compiler-owned `Effect` module** (its instance group's shape), **not** a full
type-derived proof that the dict is `Effect`'s. A fully principled check would carry each
instance's type-constructor identity down to ANF — too heavy for Slice 3. The fail-closed
gates keep a mis-shaped or third-party group from ever being admitted.

*What it needs (P2 — publish the classification, not the topology):* a
**`effectFamily :: Set String`** field on `DictMachinery` — the *result* set of
Effect-family dict keys, **not** the raw Rec-group `Map`. `localFactsOf` derives it
**once** from the raw `am.decls` (which still carry `Decl.recursive` + `members`, before
`machineryOf` flattens them) via the validator above, and `mergeMachinery` **unions** it
so a consumer sees dependencies' Effect-family sets. No new `BuildSummary` field is
needed — the summary already carries `dict: lf.dict`. Publishing only the classification
(not the topology) stops another pass from re-deriving the same risky Effect-family
meaning from raw group structure.

*Why it's clean:* it matches how ADR-0098 already reasons about the `Effect` instances
(one Rec group), needs no helper-name (`liftA1`/`ap`) or dict-name (`functorEffect`)
hardcoding, and is anchored on the GER keys (`pureE`/`bindE`) that are *already* the
Slice-2 anchors. A non-`Effect` `liftA1`-based functor is never admitted (its group holds
neither GER key), so it is never mis-rewritten.

*Cost/risk:* a `DictMachinery.effectFamily` set + its `mergeMachinery` union +
`localFactsOf` running the validator over un-flattened decls. Moderate, localised; the
fail-closed gates bound the risk to "misses a rewrite" (never "wrong rewrite").

### C. Recognise at the defining module, rewrite the instance field (alternative)

Rewrite `functorEffect`/`applyEffect`'s `map`/`apply` **field** to the canonical Effect
`map`/`apply` **where the instance is defined** (the `Effect` module), using a *local*
Effect-family fixpoint (option A's rule, which *is* decidable locally: `v = liftA1
applicativeEffect`, `applicativeEffect.pure = Effect.pureE` are all local siblings). The
published dict then carries the canonical `map`/`apply`; consumers projecting
`functorEffect.map` get it directly.

*Trade-off:* rewrites instance **data** (dict fields), not use sites — a different GER
operation than Slices 1–2 (which rewrite computations). Interacts with the recursive dict
group (ADR-0098) and the grouped-projection machinery; needs care that the rewritten
field still projects group-free. Also only helps once the dict is projected (it does not
remove the projection itself, only makes its result canonical).

### D. Hardcode the accessor field + dict name (rejected)

Key the rewrite on `field == "map"`/`"apply"` and `dict == "Effect.functorEffect"`/etc.
The ADR explicitly rejects dictionary-name hardcoding; recorded only to note it is the
thing we are avoiding.

## 4. Recommendation

**Option B (Rec-group anchor, fail-closed).** It is the only option that (a) recognises
the `Effect` `Functor`/`Apply` instances **cross-module** without name/helper hardcoding,
(b) reuses the GER-key anchors Slice 2 already trusts, and (c) rewrites **use sites**
(consistent with Slices 1–2), leaving instance data alone. The cost is a single derived
`DictMachinery.effectFamily :: Set String` classification (P2) computed once by
`localFactsOf` via the fail-closed validator (P1) and unioned through `mergeMachinery`.

The rewrite itself is then §5(a): for `map`/`apply` (identified by the accessor's field
name) applied to an Effect-family dict,

```
map   dict f m    →  \$u -> let a = perform m in f a
apply dict mf ma  →  \$u -> let f = perform mf in let a = perform ma in f a
```

recognised in `Impurify.recognize`'s dispatch arm (extended: if the resolved impl is not
a GER key but the accessor field is `map`/`apply` **and** the dict is Effect-family, emit
the map/apply canonical instead of declining), reusing the existing η-expansion /
over-application / hoisting machinery.

## 5. Open questions to settle before / during implementation

1. **Accessor-field identity.** Is keying on `machinery.accessors[acc] == "map"` /
   `"apply"` acceptable (it is a class-method field name, not a dict name), or must the
   `Functor`/`Apply` classes themselves be identified structurally? The field-name key is
   far weaker hardcoding than a dict name; **it is *not* self-sufficient** — a wrong
   `map` would be wrong code — so it is only ever the *field selector*, gated by the
   fail-closed Effect-family membership (a `map` field on a non-Effect-family dict is left
   alone). (Resolved for **P2**: publish the derived `effectFamily :: Set String`
   classification, not the raw Rec-group topology.)
2. **`apply`'s dict.** `apply = ap` bakes the *Monad* dict (`monadEffect`), whose `bind`
   routes to `bindEffect.bind = Effect.bindE`. Confirm the Rec group spans
   `functorEffect … monadEffect` so the validator's `pureE`+`bindE` anchor admits
   `applyEffect` (it should — all five are one group per ADR-0098).
3. **Validator fixtures.** A positive fixture (the real `Effect` group admits
   `functorEffect`/`applyEffect`), and — the P1 requirement — a **mixed / mis-shaped
   Rec-group negative fixture** (a group holding only one of `pureE`/`bindE`, or with a
   non-dict member, or a second unrelated `map`-bearing dict) that is **not** admitted.
4. **Measurement target.** Re-run `bench-effect-ref` after landing to confirm `void` no
   longer projects `functorEffect.map` and the ratio improves from 1.199.
