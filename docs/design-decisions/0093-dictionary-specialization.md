# 0093. Dictionary specialization on the optimiser seam

- Status: Accepted (2026-07-12)
- Date: 2026-07-12

## Abstract

Add a `Specialize` pass to the ADR-0086 seam that clones a top-level function **once per (callee,
known-dictionary-arguments) pair, homed in the consuming module**, binding the dictionary
parameters to their concrete dictionary references and letting the ordinary NbE rounds fold the
clone. This is the recovery path for the count-state regression the ADR-0089 self-compile
extension accepted (2.163 → 1.801): builders whose dictionary parameter is **only passed through**
(`projected == 0`, the `monadStateStateT` class) qualify for no inline tier — by design, since
widening the tiers is exactly the measured CST.Parser blow-up — but a *shared clone* costs one
copy per distinct instantiation instead of one per call site, which sidesteps the blow-up
economics entirely. Dictionary specialization is the **first of two** planned specializations: the
roadmap's caller-homed **higher-order specialization** (static-argument transformation for
`map`/`fold`/`filter`-class recursive loops — the residual map-fold/quicksort cost the recursion
bar keeps out of inlining's reach) is a separate later slice that reuses this pass's skeleton
(homing, naming, dedup infrastructure) with its own qualifying conditions and termination
argument.

## Context

### The measured motivation

The ADR-0089 self-compile extension restored the strict extern-free 64-tier and added the
scrutinised-known-arg tier. Both deliberately refuse a builder that neither projects nor applies
its dictionary parameter — it only forwards it:

```
Bench.CountState.Main.monadStateStateT = Control.Monad.State.Trans.monadStateStateT(Data.Identity.monadIdentity)
  -- callee body: \dict -> let $q1 = monadStateT(dict) in { state: …, Monad0: \_ -> $q1 }
```

`dict` has `projected == 0, appliedHead == 0`: no tier fires, `Main.get = Main.monadStateStateT.state`
stays a dispatch, and count-state sits at **1.801** (from 2.163 when the unsound S7 tier still
unfolded it). The refusal is correct *for inlining* — per-call-site copying of
sibling-referencing ≤64 bodies is the measured ×167/round CST.Parser explosion — but the cost
model that makes inlining dangerous makes specialization cheap:

- inlining cost ∝ **call sites** × body size (a grammar web has hundreds of sites);
- specialization cost ∝ **distinct (callee, dictionary) instantiations** × body size — for
  dictionaries this is small by nature (count-state instantiates `monadStateStateT` with exactly
  one dictionary; a module rarely uses more than a couple per class).

### Prior art

- **GHC's `Specialise` pass**: the standard, decades-proven answer to this exact shape —
  overloaded functions applied to known dictionaries get monomorphic clones; dictionary dispatch
  is eliminated without inline-threshold gymnastics. purs-backend-optimizer reaches the same
  outcomes through source-level `@inline` directives — a channel purvasm lacks (ADR-0089
  §Context); specialization is the directive-free substitute.
- **purs-wasm's `Optimize/Specialize.purs`** (~400 lines, read 2026-07-12): a working
  *higher-order* specialization whose skeleton decisions transfer directly —
  **caller-homing** (its ADR 0032: the clone lives in the consuming module, so a module's
  optimized output depends only on itself + its dependencies, never its consumers — per-module
  builds and caches survive; cross-consumer duplicate clones are tolerated), **dedup by
  key** within the consuming module, and **fresh clone naming against the whole known ident set**
  (its comment records a name-collision bug that bound references to the wrong clone). Its
  HOS-specific machinery (static-parameter detection through recursion, lambda-shape hashing with
  capture abstraction) is *not* needed here and is owed a close reading when the HOS slice starts.

### Why the dictionary variant goes first

1. The evidence is in hand: the exact orphaned binding class, module, and delta (−0.362 on
   count-state) are already isolated.
2. The keying is trivial where HOS's is hard: dictionary arguments are **stable global
   references** (`Data.Identity.monadIdentity`) — the dedup key is a name vector, no shape
   hashing, no capture abstraction.
3. The skeleton this pass builds (homing, naming, dedup, the clone-emission point in
   `optimizeModule`) is what the HOS slice reuses.

## Decision (sketch, for review)

### 1. Discovery — syntactic, on the optimised decls, marks-style

After the NbE rounds converge (the driver's per-round output), scan the module's decls for
**residual saturated applications** `CApp h args` where:

- `h` resolves to a dependency/sibling **candidate** with `arity == Just (length args)` (the body
  is available and, by `publishBound`, `< 64` nodes — clones are small by construction);
- the application survived every inline tier (it is *in* the residual — no double-judging);
- at least one argument is a **dictionary-valued global**: an `AtomVar`/`AtomForeign` whose
  extern is a value candidate that force-peeks (the existing `known` machinery, grouped entries
  excluded) to an `SRec` — or is itself a *grouped builder alias* (the S8 `group` fact);
- the callee is **neither recursive nor a Rec-group member** (review pin: `group == Set.empty`
  on the callee's candidate — the S8 grouped builders are published candidates too, and a
  "not self-recursive but mutually recursive" callee would otherwise slip in; recursive
  specialization is HOS's territory, out of scope). The split is deliberate: a grouped builder
  alias is welcome as a dictionary **argument** (it peeks to a record through the S8 machinery)
  and refused as a **callee**.

### 2. The clone — caller-homed, dictionary parameters bound, otherwise untouched

For each discovered `(calleeKey, dictArgs)` pair, emit **one** new member in the consuming
module:

```
<mangledCallee>$spec<n> = \<non-dict params> -> let <dict param> = <dict atom> in <callee body>
```

— the candidate body verbatim with each dictionary parameter `let`-bound to its concrete global
reference (an atom — no structural copy), and the call site rewritten to
`CApp <clone> <remaining args>`. No reduction happens at emission time: the **next driver round**
optimizes the clone like any member — the dictionary is now a known reference inside it, so the
peek/fold machinery (and the S8 grouped trigger, and the known-arg tier) fire where they
couldn't before, and forwarded calls (`monadStateT(dict)` with `dict` now concrete) become the
next round's discovery sites.

### 3. Dedup, naming, homing — state-free monotonicity

The seam gives the pass nowhere to keep a mutable seen-set: `optimizeModule` is a pure single
step and the driver threads only the `AnfModule` between rounds (review pin — a per-round empty
map would re-discover the same instantiation every round and sprout `$spec1, $spec2, …`
forever). Both dedup and termination are therefore **derived from the module term itself**:

- **Dedup key, position-included** (review pin): `(calleeKey, Array { index :: Int, key :: String })`
  — the *indices* of the parameters being bound matter, not just the set of dictionary names
  (the same dictionary in a different parameter position is a different instantiation, and the
  clone is defined by *which* parameters it `let`-binds). The call-site rewrite derives from the
  same mask: clone parameters = callee parameters minus the masked indices, `<remaining args>` =
  arguments at unmasked indices, in order.
- **Deterministic clone names — the module is the seen-set**: the clone's member key is a
  **deterministic, injective encoding of the dedup key**
  (`<mangledCallee>$spec$<idx>_<mangledDictKey>[$<idx>_<mangledDictKey>…]` — dictionary vectors
  are 1–3 qualified names, so the full key is embedded, no hashing and no hash-collision
  soundness argument; the escaping may borrow the LLVM mangler's conventions). Discovery emits a
  clone **only if no member with that key exists**; an existing member is reused by reference.
  Injectivity gives the purs-wasm collision lesson for free — same name ⟺ same instantiation —
  and monotonicity across rounds is a consequence of the module carrying its own clones.
  The reserved `$` namespace (source cannot lex it) keeps user bindings out; a fixture still
  pins the no-collision claim against adversarial existing names.
- **Homing**: the consuming module (purs-wasm ADR 0032's argument verbatim: dependents-only
  dataflow, per-module builds and caches survive; cross-consumer duplicates accepted — dict
  instantiation counts are small, and no Binaryen-style late merge exists on the VM/LLVM paths).

### 4. Clones are module-private

Never exported: the `.pmi` export surface is untouched (the existing 564-`.pmi` mode-stability
gate is the verifier); referenced only from this module's bodies (same linkability class as any
private binding). **v1 does not publish clones as inline candidates** — dependents that want the
same specialization discover their own (caller-homed, duplicates accepted); publishing clones is
a later, evidence-driven relaxation.

### 5. Termination and size

- The key space is finite: (candidates in scope) × (dictionary globals in scope) × (parameter
  masks); the deterministic-name reuse caps emissions at one clone per key per module, with no
  cross-round state — a round that discovers only already-present clone names emits nothing and
  rewrites nothing new, so the fixpoint's `module == prev` check terminates it.
- Cascade depth = builder-chain depth (a transformer stack), not call-site count.
- Each clone is `< publishBound` at emission; its subsequent rounds are guarded by the existing
  round-growth backstop and the size/time + self-compile gates — no new size mechanism.
- The driver fixpoint converges when a round discovers no new key and rewrites no site (clone
  emission and site rewriting are part of the module term, so the existing `module == prev`
  convergence check subsumes it).

### 6. Pipeline placement and gates

`optimizeModule` becomes `Specialize ∘ Nbe ∘ DictElim` per round — discovery on the NbE output,
emission into the module handed to the next round. All existing gates apply unchanged
(behavioural equality across the 5 benches, `.pmi` 564, size×/time×, the self-compile leg);
count-state is the target measurement (recover toward ≥ 2.1), the other four benches and the
self-compile ratio are the no-regression watch.

## Alternatives considered

- **A pass-through-aware inline tier** (relax the known-arg tier to `projected == 0` arguments):
  returns to per-call-site copying of sibling-referencing bodies — the measured blow-up's exact
  surface — and was already rejected in the ADR-0089 self-compile extension's framing.
- **Source-level `@inline` directives** (the purs-backend-optimizer channel): no directive
  channel exists in purvasm, and library-annotating is a workflow cost this compiler's
  no-directive stance deliberately avoids.
- **Whole-program specialization** (home clones in the callee's module, driven by all callers):
  violates [optimizer-modular-not-whole-program]; purs-wasm's ADR 0032 records the same
  rejection for the same reason.
- **Doing HOS first**: its keying (lambda shape hashing, capture abstraction) and termination
  (recursive clones) are strictly harder, and its measured target (map-fold/quicksort loop
  dispatch) is not yet traced; the dictionary variant builds the shared skeleton on the easier
  ground. HOS follows as its own ADR, opening with the owed purs-wasm `Specialize.purs`
  close reading (sidenote).

## Owed with implementation

- Fixtures: the pass-through builder chain specializes and folds end-to-end (the
  `monadStateStateT` shape); dedup (two sites, one instantiation → one clone — and stable across
  a second driver round: re-running on the output emits nothing new); **positional** dedup (the
  same dictionary at a different parameter index → a distinct clone); distinct dictionaries →
  distinct clones; a self-recursive callee and a Rec-group member callee are both refused (the
  latter still accepted as an *argument*); a module already containing a member named exactly
  like a would-be clone key is reused, never duplicated or miscollided; `.pmi` surface untouched.
- `--emit-ir` trace of count-state's `Main.get`/`Main.put` showing the re-fused endpoint.
- Measured table incl. self-compile (clone weight must stay inside the 1.5 size gate).

## Progress (2026-07-12): landed as accepted; the target metric did not move — blockers measured

Implemented as pinned: `Optimizer.Specialize` (state-free discovery/emission, deterministic
injective clone keys, positional masks, prepended module-private clones, v1 dependency-global
dictionaries only), wired as `Specialize ∘ Nbe ∘ DictElim` with the summary derived from the
post-specialize decls, `candidatesOf` skipping the `$spec$` namespace. All eight owed fixtures
pass as written (259/259 unit + E2E); the compiler closure emits **6 clones**, self-compile size
ratio **1.111** (gate green), `.pmi` 0-differ, all benches at their pre-specialize values —
the pass is behaviourally sound and cheap at scale.

**But count-state stays at 1.801.** The measured reasons, in the residual IR:

1. The dominant residual application is `monadRecStateT(monadRecIdentity)` — the `tailRecM`
   machinery — and its callee's converged body is ≈ **235 IR lines, far past `publishBound`
   (64)**: it is never a published candidate, so neither the known-arg tier nor this pass can
   see a body for it. The `< 64` bound exists to keep *inline* summaries small and per-site
   duplication bounded; a specialization clone is one copy per instantiation, for which a
   235-node body is a different (and plausibly acceptable) economics — but publishing larger
   bodies for spec-callees only is **outside this record's accepted scope** and needs its own
   decision (a `specBound` channel on the summary, or body access by another route).
2. The `get`/`put` residual chain runs through `monadStateT(monadIdentity)` whose callee is a
   **Rec-group member** — refused by this record's own review pin (grouped callees are HOS/
   recursive-specialization territory). A group-stopped clone variant is likewise a follow-up
   decision, not a v1 patch.

Both blockers are recorded here rather than worked around: relaxing either bound ad hoc would
re-open the size/termination arguments this record and the ADR-0089 self-compile extension just
closed. Measured-and-reported per §6; the recovery follow-up (a spec-callee body channel sized
for clone economics, and/or grouped-callee clones) is its own proposal on this evidence.
