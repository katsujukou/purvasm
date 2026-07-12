# 0096. Memory-effect summaries: `mayTouchMutable` through call summaries; single-use pure clean calls sink

- Status: ~~Proposed~~ **Accepted** _(2026-07-13: promoted — maintainer accepted after two review rounds)_
- Date: 2026-07-13

> **Revision (2026-07-13, review round 1).** Three findings folded in. (P1) The
> **FFI semantic contract** is pinned in §3: a foreign whose shape claims
> purity must be observationally pure — hidden mutation is supported only for
> compiler-known linear-return unsafe leaves (`unsafeSetByte` class); a
> pure-typed foreign mutating host globals (`bumpGlobal :: Unit -> Unit`) is
> outside the supported FFI semantics, because dead-drop (ADR-0095, already
> shipping) consults only the perform bit and would delete the observable
> write. A dated note now marks the same premise on ADR-0095 §4. (P2) The
> summary/predicate **equations** are pinned in §1 (they were previously
> implied): PAP construction is clean but the PAP *summary carries the
> callee's bits*, exact saturation shifts `retMtouch` down one level, and
> over-application is dirty. (P2) Verification gains the **two-level
> propagation fixtures** (PAP residual, reader-closure return, point-free
> alias chain, recursive-group fixpoint). The bit is renamed
> **`mtouch`/`retMtouch` (`mayTouchMutable`)** — at foreign leaves it covers
> hidden writes, not just reads, so the name now says what it means: the
> motion hazard `vsat` cannot express.

> **Revision (2026-07-13, review round 2).** (P1) The FFI contract is
> corrected to bind **per saturation level**: each `vsat = false` in a summary
> is the contract that *that* value's exact saturation is observationally
> pure; `retVsat = true` marks the *next* saturation effectful and never
> licenses performing at the current one — `log "x"` (`{ vsat: false,
> retVsat: true }`) must do nothing but build its thunk, or dead-drop deletes
> a real effect. The explicit-`Effect`-head constraint (FSR cannot see through
> synonyms) is folded into the contract, and ADR-0095's note is corrected
> likewise. (P2) The reader-closure fixture is pinned as a **let-returned
> closure** — the curried spelling uncurries into one `CLam` (ADR-0025) and
> would test a PAP instead of the exact-application shift. (P2) The `@memsafe`
> "no `ulib.json` schema change" claim is retracted: ulib-overlaid consumers
> read shapes from `ulib.json.foreignSigs`, not source, so the directives
> record must resolve a provenance split (source-reach only vs a
> `ulib-tools`-validated manifest field); this record no longer prejudges it.

## Context

ADR-0095 built the effect analysis and drew a hard boundary: `may-perform = false`
licenses *elimination when dead*, never *motion*, because the summary does not
track mutable-memory reads — a callee wrapping `IndexArray` is `vsat = false`,
yet sinking its call across a `SetArray` changes the value read. Its §3 named
the follow-up: a memory-effect extension of the summary ("a `mayReadMutable`
bit propagated through calls the same way `vsat` is"), with the fixture
obligation *a call hiding `IndexArray` must not move across a `SetArray`*.
This record is that follow-up — with the bit renamed **`mayTouchMutable`**
(`mtouch`), since at foreign leaves it must cover hidden writes as well as
reads (see §1/§3).

**What the perform bit already covers, and the one structural gap.** In the
ADR-0095 model the mutation primops (`NewArray`/`SetArray`) make evaluation
`may-perform` — so a callee that *writes* (or allocates, whose identity
duplication would fork) is already excluded from every relaxed class by the
perform bit, and must stay so: a dead binding whose evaluation writes a shared
array is observable through the array, so dead-drop soundness itself depends on
writes staying in `eperf`. The single pinned primop that is *perform-clean* is
**`IndexArray`** — a read. Structurally, the entire gap between "pure" and
"movable" is reads hiding inside call summaries.

**Foreign leaves are a second, different gap.** A foreign's `ForeignShape` is
reconstructed from its *type* (ADR-0080), and a type cannot prove
memory-cleanness: `Purvasm.String.unsafeSetByte` (ADR-0052) is typed
`String -> Int -> Int -> String` — shape-pure — yet mutates in place. (Its
*dead-drop* is still sound: under the leaf's documented linear-thread contract
the write's only observer is the binding's own result, and a dead result
observes nothing. Its *motion* is not.) So at the leaf, the needed bit is not
"reads" but "may touch the mutable store at all, read or hidden write" — and it
cannot be derived; it can only default conservatively.

**Why motion is worth having.** The sink the ADR-0095 gate refuses is the
`capture ≤ Branch` single-use case: a pure call computed unconditionally whose
sole use sits under a branch. Sinking it into the branch makes it execute only
on the path that uses it — a real win, and the same relaxation class as
dead-drop (a skipped pure diverge, ADR-0034's accepted partial correctness).
The `CapNone` case (sink within a straight line) is placement only; the
`CapClosure` case re-executes per closure call and must stay pinned.

**Why publish widening is *not* worth having (analysed, rejected).** ADR-0095's
Context site (3) — publishing a CAF whose body is a pure saturated call — turns
out to be self-defeating: if the callee is small enough that consumers could
fold the call, the home module's *own* NbE round already unfolds it at gate A,
leaving a value body that the existing publish predicate accepts. What remains
un-unfolded is precisely the big/opaque callee — and publishing that call means
every consumer use site *re-executes the CAF's init work*, the exact work
CAF-sharing exists to avoid. A pure clean call CAF is therefore either already
publishable (post-unfold) or a pessimization to publish; no widening clause is
added.

## Decision

Extend the seam's effect summary with a two-level **mutable-store bit**
(`mtouch`/`retMtouch`, "`mayTouchMutable`"), computed structurally like
`vsat`/`retVsat`, defaulted dirty at foreign leaves; consume it in exactly one
place — the ADR-0095 dead-only branch generalises to a **dead-or-single-use**
branch for calls proven pure *and* clean. In-memory only; `ForeignShape` and
every persisted/authored surface stay untouched.

### 1. The extended fact (in-memory only)

- The analysis' summary becomes
  `{ arity, vsat, retVsat, mtouch, retMtouch }`:
  * `mtouch` — may saturating this value **touch the mutable store**?
    Structurally that means a read (`IndexArray` — writes/allocation are
    already `may-perform`); at a foreign leaf it means any store interaction,
    hidden writes included. The bit's one-sentence meaning: **the motion
    hazard `vsat` cannot express**.
  * `retMtouch` — may the saturated result, when itself saturated, do so?
- **The equations** (load-bearing — the two-level shift is where an
  implementation silently goes wrong; each line mirrors its `vsat`/`retVsat`
  analogue in ADR-0095 §1 / boot's `vsum_c`):

  ```
  -- the evaluation predicate (the eperfC analogue)
  mtouchC (partial application)   = false            -- constructing a PAP is clean
  mtouchC (exact application)     = callee.mtouch
  mtouchC (over-application)      = true
  mtouchC (CPrim op)              = pinnedPrim op    -- IndexArray/SetArray/NewArray
  mtouchC (construction/proj/atom/lambda) = false

  -- the value summary
  vsum (partial application)      = { mtouch: callee.mtouch,    retMtouch: callee.retMtouch }
  vsum (exact application)        = { mtouch: callee.retMtouch, retMtouch: callee.retMtouch }
  vsum (over-application)         = unknown (dirty)
  vsum (lambda).mtouch            = mtouchExpr(body)
  vsum (lambda).retMtouch         = vsumExpr(body).mtouch
  vsum (CAccessor)                = unknown (dirty)   -- a field may be any value
  vsum (prim/ctor/array/record/update) = clean value
  branch join                     = field-wise ||     ; SCC least fixpoint as in 0095
  ```

  Note the asymmetry the review pinned: **a PAP's construction is clean
  (`mtouchC = false`) but its *summary* carries the callee's bits verbatim** —
  the residual saturation must not lose the hazard. `mtouchC` covers all three
  `pinnedPrim`s for honesty ("touches the store"), but only `IndexArray` adds
  information — `NewArray`/`SetArray` stay in the **perform** bit, unchanged
  and load-bearing: dead-drop consults `eperf` alone, and observable writes
  must never enter a relaxed class.
- **Foreign leaves lift dirty**: a `ForeignShape` becomes
  `{ …, mtouch: true, retMtouch: true }`. The type cannot prove cleanness
  (`unsafeSetByte` is the counterexample), so the conservative default also
  covers pure-typed hidden writes. The planned precision channel is a
  **directive on the declaration** — a `@memsafe` marker in the
  `foreign import`'s comment, part of the future optimiser-directive family
  (the `@inline`/`@unbox` class). Its reach forks by **provenance**, and this
  record leaves that fork open for the directives record to resolve: for
  *source-reconstructed* foreigns (app/registry modules) FSR already parses
  the declaration's CST (ADR-0080), so the directive rides the same pass and
  the assertion lives next to the declaration it describes; but a
  *ulib-overlaid* module's consumer has no source — its shapes arrive from
  `ulib.json`'s `foreignSigs` — so reaching ulib leaves requires either
  `ulib-tools build` validating the directive and distributing it through the
  manifest (a schema extension after all), or ulib foreigns simply staying
  dirty. Whichever way that lands, the directive's *meaning* must be pinned
  as "touches the mutable store in **no** way — neither writes *nor reads*":
  a read-only leaf (an `unsafeGetByte` class) is mutation-free yet not
  motion-safe, so `@memsafe` must not apply to it. A dedicated directives
  record specifies all of this; not here.
- **Carrier types**: the extended fact is owned by `EffectAnalysis` and rides
  the in-memory channels (`BuildEnv.effects` / `BuildSummary.effects`, the
  lexical env, the oracle). `ForeignShape` — the FSR output, the `ulib.json`
  schema, the JSON codec (ADR-0080/0090) — is **not** extended; it lifts at
  the boundary. Dependencies' published facts therefore carry *real* structural
  `mtouch` (a dependency's big pure arithmetic lambda is provably clean at its
  consumers), while foreign shapes stay conservative.

### 2. The consumer: the call branch generalises to dead-or-single-use

The ADR-0095 §3 branch (a `CApp` never enters `classifyRhs`/`verdict`) gains
one clause. For an exact-saturated `CApp` with `vsat = false`:

- `usage = Nothing` → **drop** mark (ADR-0095, unchanged; needs no `mtouch` —
  elimination was already licensed by purity alone, under the §3 FFI
  contract).
- `usage = Just { total: 1, capture ≤ CapBranch }` **and** `mtouch = false` →
  **sink** mark. The carrier is the same `marks` set; the machinery is the one
  the Deref class already uses — `Eval` binds the marked binder's computation,
  and `quoteAtom`'s compound fallback re-materialises it at its sole use site,
  including inside a branch arm.
- anything else (multi-use: duplication; `CapClosure`: multi-execution;
  over-application / unknown / dirty) → pinned, exactly as today.

This **supersedes ADR-0095's review-round-2 pin** ("at any use count ≥ 1 a pure
call stays pinned") for the strictly stronger fact only: that pin's rationale
was that `vsat = false` alone cannot rule out hidden reads — `mtouch = false`
is precisely the missing half. The 0095-only configuration (facts without
`mtouch`, i.e. every foreign-derived fact) behaves identically to today.

### 3. Soundness

- **The FFI semantic contract (review P1, corrected round 2 — per
  saturation).** Dead-drop consults the perform bit at the dropped binding's
  *own* saturation, so the contract binds **each saturation level
  separately**: every `vsat = false` in a reconstructed shape is the contract
  that **that value's exact saturation is observationally pure** — no IO, no
  observable store mutation. `retVsat = true` says only that the *returned*
  value's own saturation performs; it is never a license to perform at the
  current one. Concretely, `log :: String -> Effect Unit` is
  `{ vsat: false, retVsat: true }`: `log "x"` must do nothing but build the
  effect thunk — the runtime leaf defers the write to the thunk's force
  (ADR-0023/0067) — and a host implementation that printed eagerly at
  construction would be deleted by dead-drop, making it contractually wrong,
  not merely unoptimisable. The declared type *is* the effect contract (the
  ADR-0080 design premise stated as an obligation), which pulls in the
  companion constraint ADR-0080 already documents as a limitation: an
  effectful foreign must carry an **explicit** `Effect`/`ST`/`EffectFn{N}`/
  `STFn{N}` head — a type synonym hiding `Effect` is opaque to FSR and would
  claim a fully pure shape, i.e. silently opt into the pure contract it
  violates. A pure-shaped foreign that mutates observable host state —
  `foreign import bumpGlobal :: Unit -> Unit` updating a host global — is
  **outside the supported FFI semantics**: neither the dirty `mtouch` lift nor
  any conservative default can protect it, because elimination (already
  shipping since ADR-0095) never consults `mtouch`. Such an effect must be
  typed `Effect`. The **only sanctioned exception** is the compiler-known
  linear-return `unsafe` class (`Purvasm.String.unsafeSetByte`, ADR-0052):
  its documented contract — fresh, unshared, observed only through the
  returned value — is precisely what makes a *dead* result unobservable, so
  dead-drop stays sound for it and motion is denied by the dirty lift. Any
  future leaf wanting this treatment must carry the same documented
  linear-return contract; third-party pure-shaped foreigns with hidden global
  mutation are unsupported, not merely unoptimised. (The alternative — a
  separate `mayWrite` bit consulted by dead-drop — is rejected below.)
- Motion is licensed only by `vsat = false ∧ mtouch = false` at exact
  saturation. A clean call interacts with no mutable state and performs no IO,
  so reordering it across *any* computation — including performs and writes —
  preserves every observable; the only behavioural change class is
  conditional/late execution of a pure diverge, which is ADR-0034's accepted
  partial-correctness relaxation (the same class dead-drop already inhabits).
- The dirty foreign default keeps ADR-0095's no-motion guarantees byte-for-byte
  for every foreign-called shape: the 0095 no-motion fixtures must pass
  unchanged.
- Dead-drop's own soundness is untouched: writes stay in `eperf`, the drop
  clause does not consult `mtouch`, and the FFI contract above is the premise
  that makes that sufficient.

### 4. Verification

- **The 0095 §3 obligation, structurally:** a *big* (un-unfoldable) sibling
  whose body hides an `IndexArray` gets `mtouch = true` from the structural
  walk — its live single-use call never sinks across a `SetArray`. (The small
  version unfolds at gate A and its direct `IndexArray` is `pinnedPrim`-pinned
  — also covered, by construction.)
- **Fires:** a big pure arithmetic sibling's single-use call sinks — at
  `CapNone` (placement) and into a `CapBranch` arm (conditional execution) —
  asserted structurally; multi-use and `CapClosure` stay pinned.
- **Two-level propagation (review P2 — each targets one equation, so a broken
  `retMtouch` cannot pass):**
  * *PAP residual:* constructing a PAP of a dirty callee is itself
    droppable-when-dead (clean construction), but the PAP's **residual
    saturation** never sinks (`vsum(PAP).mtouch = callee.mtouch`).
  * *Reader-closure return:* a clean function returning a reader closure —
    pinned as the **let-returned** spelling
    (`outer = \a -> let reader = \u -> …IndexArray a… in reader`), because
    the curried spelling `\a -> \u -> …` normalises to one uncurried
    `CLam [a, u]` (ADR-0025's `collect_lam`), which would make a one-argument
    application a *PAP* and miss the target equation. With arity 1,
    `outer x` is exact: the binding itself is clean
    (`mtouchC = callee.mtouch = false` — droppable when dead), while the
    hazard shifts `retMtouch → mtouch` into the result's summary, so the
    returned closure's saturation never sinks.
  * *Point-free alias chain:* an alias of a reader (`M.alias = M.reader`)
    keeps the dirt through summary lookup — a call through the alias stays
    pinned.
  * *Recursive-group propagation:* a group member calling a reader sibling is
    dirty at the least fixpoint — its calls stay pinned.
- **Foreign conservatism:** the existing 0095 no-motion guards (foreign-shaped
  `M.read`) pass unchanged.
- **Behaviour gates:** `--opt ≡ --no-opt ≡ oracle` on the effect fixtures;
  benchmark ratios against the 2026-07-12 baseline; opt-compile `time×`/`size×`
  within the ADR-0089 gates (the facts stay lazy — the 0095 implementation's
  measured lesson).

### Out of scope

- **Publish widening** — rejected outright this time, not deferred (see
  Context: already-publishable after local unfold, or a work-duplication
  pessimization).
- **The optimiser-directive family** — `@inline` (the purs-backend-optimizer
  reference's one missing piece in the NbE port), `@unbox`, and `@memsafe` on
  foreign leaves (this record's precision channel, spelled as a declaration
  comment read by FSR — see §1). One dedicated record should define the
  directive surface once, so each optimisation consumes rather than invents
  its own annotation syntax.
- **`CapClosure` sink**, escape-based `NewArray` relaxation, and persistence
  of the extended fact (the `.pmi` follow-up inherits the five-field shape
  when it lands).

## Consequences

- The purity/motion boundary drawn in ADR-0095 §4 is completed rather than
  weakened: motion now has a licensing fact, and the license is exactly the
  conjunction the review demanded.
- Pure clean calls conditionally execute (branch sink) and compact let-spines
  (straight-line sink); dead-drop cascades reach one step further. The expected
  instruction-count movement on the current corpus is again modest — the
  benchmark residuals point at recursive-loop dispatch (the HOS track), not at
  movable straight-line calls — and the record says so up front.
- Foreign-heavy code sees no motion until the `memClean` precision record
  lands; structural (sibling/dependency-body) chains see it now.
- One more boolean pair per summary; the lazy-fact discipline (0095) bounds the
  added analysis cost.

## Alternatives considered

- **A separate `mayWrite` bit consulted by dead-drop** (the review's other P1
  option). Rejected: it would make every pure-typed foreign's *dead* call
  undroppable by default (all leaves lift dirty), regressing ADR-0095's shipped
  wins to protect a foreign class — pure-typed hidden global mutation — that
  the FFI contract can simply rule out instead. The contract is also the more
  honest boundary: a `bumpGlobal` whose effect matters is mistyped, not
  mis-analysed; typing it `Effect` restores every guarantee. Revisit only if a
  legitimate leaf appears that needs observable mutation under a pure type and
  cannot carry the linear-return contract.
- **A single `mayTouchMutable` bit (one level).** Rejected: without the
  `retMtouch` level a PAP of a dirty callee would either poison the PAP value
  (over-conservative — constructing it is clean) or lose the dirt at the later
  saturation (unsound). The two-level structure is the same reason
  `vsat`/`retVsat` is a pair.
- **Extend `ForeignShape` itself.** Rejected: it rides authored and persisted
  surfaces (`ulib.json` `foreignSigs`, the ADR-0080 codec) whose schema change
  buys nothing today — every foreign lifts to the same conservative constant.
  The extension is analysis-internal until the precision record gives leaves a
  way to be clean.
- **Derive foreign cleanness from the `unsafe` naming convention.** Rejected:
  the convention governs this repo's sources, not user/third-party foreigns,
  and a naming heuristic silently misclassifying one leaf is exactly the
  silent-default failure mode ADR-0080 exists to retire.
- **Include publish widening.** Rejected with analysis (Context): the
  foldable case is already published after local unfolding; the residual case
  duplicates CAF init work per use site.
- **Sink through `classifyRhs`/`verdict` eligibility.** Rejected (still): the
  verdict's multi-use clauses (`rhsSize < 5` re-materialisation) would
  duplicate calls; the dedicated branch keeps single-use as a structural
  precondition, not a threshold.
