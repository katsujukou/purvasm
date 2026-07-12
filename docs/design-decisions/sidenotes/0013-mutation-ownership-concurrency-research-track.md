# Research track: mutation, ownership, and concurrency analysis for an impure parallel ANF

- Date: 2026-07-13
- Status: background survey + mid/long-term development plan; feeds a future
  memory-effect-system ADR (the GER companion) and the v2 concurrency work.
  **Deliberately off the walls-3/4 critical path** — it becomes load-bearing when the
  M:N runtime (ADR-0062) and impurification (ADR-0034's deferred half) land, not before.
- Scope: why "mutation analysis" outgrows the effect track; what purvasm already has;
  a three-layer plan; the prior art worth borrowing from; the consumers that make the
  combination novel.
- Related: [ADR-0095](../0095-effect-analysis-on-the-optimizer-seam.md) /
  [ADR-0096](../0096-memory-effect-summaries-pure-call-sink.md) (the short-term facts),
  ADR-0034 (GER, deferred), ADR-0052 (the `unsafeSetByte` linear contract), ADR-0019
  (the ST build discipline), ADR-0061/0062 (partition invariant, capture-class rule),
  sidenotes 0006 (runtime case study) and 0009 (cross-capability review).

## 1. Why this is its own theme

The effect track's memory facts (ADR-0095/0096) are deliberately tiny: conservative
booleans that let the NbE gate drop or sink a few more calls. Two design commitments
make the underlying question much larger than that:

- **Impurification makes ANF an impure language.** After GER, `Effect` sequencing is no
  longer carried structurally by thunks — the optimiser faces real computational
  effects and real mutable memory in the IR itself. On impurified code a 1-bit
  may-touch fact collapses to "everything dirty, everything pinned": the optimiser
  would lose on GER'd code exactly what NbE gained on thunked code. A memory effect
  system with *locations* (regions/capabilities) is therefore **a practical
  prerequisite of the GER era, not research decoration**. The natural sequence is
  0096 → GER → region-indexed effects.
- **The runtime is truly parallel.** v2's capability-local heaps and M:N scheduler
  (ADR-0061/0062) mean mutation analysis must eventually answer concurrency questions
  (what may race, what may cross a capability), not just reordering questions.

"Mutation analysis for a parallel impure functional language" is a well-plowed research
field; the point of this note is to avoid reinventing wheels and to name where purvasm's
combination is genuinely its own.

## 2. What purvasm already has: three informal ownership contracts and a backstop

The track does not start from a blank page. Three ownership-shaped invariants are
already pinned in accepted records — informally, each in its own vocabulary:

1. **The `unsafeSetByte` linear-thread contract** (ADR-0052): the buffer is fresh,
   unshared, and observed only through the binding's result — a *uniqueness* assumption,
   stated as a documentation contract. (ADR-0095's dead-drop soundness for pure-typed
   foreigns leans on exactly this.)
2. **The ST build discipline** (ADR-0019): `NewArray` → fill → treat as immutable — a
   *linear-then-frozen* lifecycle, the same shape as Clean's uniqueness-to-shared decay
   or Rust's `&mut` → freeze.
3. **The partition invariant and the fork capture-class rule** (ADR-0061/0062):
   capability-local heaps, copy-on-send, `Ref` never promoted; a fork's placement class
   decided by what its closure captures (Ref-free → promotable, Ref-holding →
   affinity-bound). The cross-capability review (sidenote 0009) already concluded full
   static detection of raw-mutable-identity escape is impossible with the current
   analyses, settling for **a lint plus a runtime header-kind backstop**.

The research track's unifying job: **one permission vocabulary these three contracts are
instances of** — so the soundness story is told once, and the runtime backstop can be
progressively upgraded to static reasoning.

## 3. The three-layer plan

- **Layer 1 — optimizer facts (now; ADR-0095/0096).** Conservative, automatically
  computed, in-memory booleans (`vsat`/`retVsat`/`mread`/`retMread`). Purpose: license a
  little more elimination and motion. Scope guard: no regions, no logic, no proofs —
  and the layer keeps its "memory-effect summaries" name; it does not claim to be
  mutation analysis.
- **Layer 2 — a region/capability-indexed memory effect system (mid-term; the GER
  companion).** Facts shaped like `read ρ / write ρ / alloc ρ / atomic ρ` rows, where
  `ρ` names a store region (per-allocation-site, per-`Ref`, or per-capability — the
  granularity is the central design question). One purvasm-specific constraint shapes
  everything: **CoreFn is type-erased** (sidenote 0006 noted this for effect systems
  generally), so Koka-style *source-level* effect types are unavailable — the system
  must be an **inference on ANF** (the Talpin–Jouvelot/Tofte–Talpin lineage) plus an
  **assertion channel at the leaves** (the `@memsafe` directive family of ADR-0096 §1,
  generalising to region annotations on foreigns). The propagation infrastructure
  already exists: the seam's summary channels and the lazy-fact discipline (ADR-0095's
  measured lesson).
- **Layer 3 — the ownership/permission story (long-term; the v2 companion).**
  Logic-informed *design vocabulary*, never an in-compiler prover: unique ownership
  (may write), shared/fractional permission (may read, not write), affine/linear
  capability (may transfer), region-indexed effects (which store), atomic/fiber
  boundaries (which interference). Concurrent separation logic and Iris enter here as
  the **soundness story told on paper** — "these permission rules are sound because
  they are an instance of CSL-style ownership reasoning" — the RustBelt pattern
  (semantic soundness of a surface discipline, proved once, consumed by everyone), not
  a proof obligation in the compile path.

## 4. Prior art worth borrowing from

- **Region/effect inference:** the classic FX and Talpin–Jouvelot effect-and-region
  inference, and Tofte–Talpin/MLKit region inference — the inference-based lineage fits
  the type-erased setting directly. DDC (Disciple) is the modern
  region-and-effect-typed functional reference.
- **Row-polymorphic effect types:** Koka — effects in types with inference, safe
  encapsulation of local state (`st` heap effect), and Perceus reuse analysis. The
  *source-typed* half is unavailable to purvasm (type erasure), but Koka's effect-row
  algebra and its state-encapsulation soundness argument are the design reference.
- **Uniqueness/linearity:** Clean's uniqueness types (the linear-then-shared decay of
  contract 2), Linear Haskell, Mezzo (permissions as a surface discipline), Alias
  Types/L3 (the calculus-level treatment).
- **Reference capabilities for actor/parallel runtimes:** Pony's `iso/val/ref/...`
  deny-property capabilities — static data-race freedom for a share-by-communication
  runtime — is the closest existing shape to the partition invariant + capture-class
  rule; Verona's region-based concurrent ownership is the successor design. Erlang's
  share-nothing (sidenote 0006) is the degenerate always-copy point purvasm's
  copy-on-send already occupies.
- **Separation logic:** O'Hearn/Brookes CSL (local reasoning for threads owning
  disjoint resources), Iris (the modern higher-order concurrent SL framework), RustBelt
  (Iris-based semantic soundness of Rust's ownership — the usage-mode model for layer
  3), SteelCore (CSL embedded in an effectful type theory — evidence the effect/CSL
  connection is a live research seam).

## 5. The consumers — where the combination is novel

"Mutation analysis" alone is not a novelty claim; Koka/DDC/Mezzo/Verona plow that
field. The purvasm-shaped claim is **one set of inferred permission facts with several
consumers across the compiler/runtime boundary**:

- (a) **Optimizer motion licensing** — layer-1/2 facts feeding NbE and the GER-era
  reorderer (the only consumer that exists today);
- (b) **Static partition-invariant checking** — upgrading the sidenote-0009 lint +
  runtime header-kind backstop toward compile-time capture-class classification;
- (c) **Copy-on-send → move** — proven uniqueness of a sent value elides the copy the
  partition invariant otherwise demands;
- (d) *(speculative)* **in-place reuse** — Perceus/FBIP-style reuse of uniquely-owned
  cells, if layer-2 facts ever become precise enough to feed allocation decisions.

A dictionary-specialising NbE optimiser + impurification + a capability-local parallel
runtime, all consuming the same ownership facts, is a defensible "ownership-aware
effect-directed optimiser" story — and each consumer has a measurable gate in the
existing harnesses.

## 6. Sequencing pins and anti-goals

- Layer 1 ships now (ADR-0096); GER next; layer 2 is designed **with** GER's reorderer
  as its first consumer; layer 3 rides the v2 runtime arc.
- **Anti-goals:** no proof obligations in the compile path; no source-syntax effect
  types (type erasure is a fixed constraint — inference + leaf directives only); the
  track must never stall the effect track or walls 3–4 (it consumes their outputs).
- Each layer lands ADR-first with the usual gates; this note is the map, not a
  decision.

## 7. The horizon of the type-erasure constraint (a far-future note)

The "type erasure is fixed" constraint is fixed **for as long as purvasm consumes
`purs`'s `corefn.json`**. The maintainer's stated super-long-term goal — once the
native toolchain has matured past the current walls — is to implement the PureScript
*type checker* in PureScript and build it with purvasm, making purvasm an independent
compiler rather than an alternative backend. The frontend is already partly in hand:
the embedded CST parser (ADR-0080) self-compiles natively today (purvasm-regex,
ADR-0081, was built for it); the type checker is the last, largest piece. If that
lands, a *typed* CoreFn successor becomes available, layer 2's inference can turn
type-directed, and a row-polymorphic effect system becomes purvasm's own internal
language rather than a constraint workaround. The project's differential-oracle
discipline (golden-reference testing against `purs` itself, the same harness shape as
the boot byte-identity gates) is the de-risking tool for that reimplementation.
Nothing in layers 1–3 depends on this; they are designed for the erased world and
upgrade gracefully if it arrives.
