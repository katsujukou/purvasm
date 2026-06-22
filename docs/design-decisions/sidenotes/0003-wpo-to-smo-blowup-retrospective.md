# Retrospective: why blow-ups reappeared on WPO→SMO, and why they took so long to fix

- Date: 2026-06-22
- Type: process retrospective (not a code change). Companion to 0001 (genericShow blow-up) and 0002 (the
  original fixed-threshold exponential).
- Scope: the move from **WPO** (whole-program optimization, one in-memory process) to **SMO**
  (single-module optimization — the `purwc` worker, ADR 0038) re-surfaced large code-size blow-ups even
  though NbE (ADR 0020/0035) was already the reducer. This analyses **(1) why they reappeared** and
  **(2) why the fix was slow** — to make the next decoupling (self-host) cheaper.

## 0. A correction that frames everything

It is tempting to say "we switched to reduction-aware NbE, it looked fixed, then SMO re-broke it." The
precise history is different and is the key to the whole analysis:

- The NbE **reducer** (fire β / projection / known-case when operands are known) landed (ADR 0020
  stages 1–2). The **sharing** that keeps the reducer itself polynomial landed (ADR 0035 Layers A/B).
- But the **reduction-aware *inline-or-share decision*** (ADR 0020 stage 3 / ADR 0035 **Layer C**) was
  **never implemented — it was explicitly deferred.** What shipped instead was a coarse `normalFormSizeCap`
  (Layer C *lite*).
- So `genericShow`'s dictionary dispatch **did** materialise (projection + β fire), and **nothing
  decided not to inline it** — the blow-up was always latent in the reducer.

Under WPO it was **masked**, not absent. SMO removed the masks. So the real statement is:

> **WPO's global view provided implicit safety nets that hid a deferred problem; SMO removed the nets and
> called the bluff, forcing the reduction-aware work that was always owed.**

## 1. Why the blow-up reappeared on WPO→SMO

Three WPO "safety nets" masked the latent blow-up; SMO removes all three, and adds one new failure mode.

### (a) Whole-program DCE — masked it entirely

In metatheory, `Show/Eq TypedExpr` are **dead** (never used). WPO sees all use sites → DCE drops them
before codegen → the pathological `genericShow` is never lowered, never reaches Binaryen. The SMO worker
**cannot see its dependents**, so it **over-exports** every function (ADR 0038) → it must compile the
dead pathological code. *A pathology that is dead-in-the-whole-program becomes live-for-compilation in a
single module.* This alone turned "no problem" into "1.76 MB module that hangs Binaryen."

### (b) The full inline/reduction context — bounded the per-function size

WPO builds the reduction context over the entire program; SMO approximates it from dependency `.pmi`
summaries. A different context yields a different normal form and a different point at which the size cap
fires. The size cap was tuned (200 K) against *whole-program-reduced* forms; against the SMO-reduced form
it fired at the wrong granularity. (The cap is a global-magnitude heuristic — it implicitly assumed the
WPO context.)

### (c) No persistent intermediate artifacts — no feedback loop

WPO is one in-memory pass: a module's output never becomes its own input. SMO **persists** `.pmi` to a
shared `_build` dir and reads dependencies from the **same** dir (`--deps` == `-O`). The worker's
`loadDepInterfaces` then read the target's **own** stale `.pmi` and optimized the module **against
itself** → re-inlined its own `genericShow` instances → **9× bloat** (self-pollution). This is a
**brand-new failure mode that cannot exist in WPO** — it is created by the artifact-caching architecture
itself, not by the optimizer.

### The unifying cause: decoupling reveals hidden coupling

An optimizing compiler's whole point is **global reasoning**. The monolithic WPO had pervasive *implicit*
dependencies on global information — "everything dead is gone", "the inline context is the whole program",
"there is no persistent state to corrupt". None of these were written down as contracts; they were
ambient. **SMO is a decoupling, and decoupling converts every implicit global assumption into an explicit
obligation that must be re-established — and you discover each one only when it breaks.** The blow-ups did
not "come back" because reduction-awareness failed; they surfaced because the *masks* (DCE, full context,
no shared state) were ambient WPO properties that SMO silently dropped. The amount of such hidden coupling
is large precisely because global optimization is the feature.

Note on magnitude: these SMO blow-ups were **large constant-factor / super-linear** (self-pollution 9×,
genericShow ~33×, over-export aggregate additive) — not the strict **2^depth** of the original fusion
exponential (0002). The danger is the same (Binaryen is super-linear; a 1.76 MB module hangs), but the
mechanism is "unmasked latent bulk", not "multiplicative duplication".

## 2. Why the fix took so long

Split into **irreducible** difficulty (inherent to the problem) and **avoidable** inefficiency (how the
debugging actually went), honestly.

### Irreducible

- **Stacked causes, peeled serially.** One symptom ("Typecheck hangs") had **three independent causes**:
  self-pollution, the genericShow NbE blow-up, and over-export feeding Binaryen. You cannot see cause
  *N+1* until cause *N* is removed (fixing self-pollution exposed that the bulk was still there; fixing
  the bulk's source exposed that Binaryen, not lowering, was the consumer). Serial by nature.
- **Symptom far from cause.** The failure is a **hang in Binaryen's optimizer**, three stages downstream
  of the cause (a 9× MIR bloat in the *optimizer*). Every stage in between (optimize → lower → codegen →
  binaryen) could *plausibly* hang, so each had to be ruled out by measurement.
- **Slow, expensive feedback loop.** A diagnosis cycle = rebuild metatheory corefn (~90 s) + run the
  worker/orchestrate (up to a **200 s timeout** just to observe "still hanging"). Minutes per hypothesis;
  the hang itself consumed the clock.
- **The real fix needs deep semantics, not a patch.** Distinguishing a *reducing* inline from a
  *non-reducing* one, the cyclic-vs-acyclic-DAG distinction, the anonymous-projected-method gap — these
  require a precise model of the NbE value domain. This is the **same wall ADR 0020 hit**; reduction-aware
  inlining is intrinsically subtle.

### Avoidable (lessons)

- **Wrong initial hypotheses, falsified one at a time.** The session (and prior memory) chased
  "summaryInlineKeys locality" (it was self-pollution), "lowering case-of-case blow-up" (it was Binaryen),
  "genericShow lowering is the landmine" (lowering was fine). Each cost cycles. **The cheap discriminating
  test came late:** the one-line `-g` run (skip `B.optimize`) that isolated Binaryen in seconds should
  have been the *first* probe, not a mid-investigation one. *Lesson: bisect the pipeline with the cheapest
  stage-isolating switch before theorising about any one stage.*
- **No fast reproducer; the whole guard suite stayed green.** Unit/e2e/diffPurwc/bench all **passed
  throughout** — because no fixture had a reduction-heavy (genericShow-on-recursive-ADT) module. diffPurwc's
  `.pmi` byte-parity contract held for its small fixtures and **silently broke for Typecheck, undetected**.
  False confidence + only a slow reproducer (full metatheory). *Lesson: when adding an architecture (SMO),
  add a fixture that exercises the property the architecture stresses (here: a recursive-`Generic` module
  in diffPurwc) so the blow-up has a fast, suite-level reproducer.*
- **Heuristic settling needs full-suite validation.** The principled fix (method-tagging) was too
  large/risky, so we used inc1+inc2+cap — each requiring the entire guard suite **plus** the bench
  (CountState stack-safety) to prove it does not regress the prized State-monad collapse. Validating a
  heuristic is inherently more expensive than validating a principled invariant.
- **Architectural-responsibility detour.** The `Data.Number` blocker was first patched in the *compiler*
  (`Build.purs` reachability expansion); the user correctly flagged it as a responsibility violation
  (reachability is `purs-wasm`'s job; ulib soundness is the real fix) and it was reverted, becoming
  ADR 0039/0040. Time was spent putting the fix in the wrong layer first. *Lesson: when a single-module
  failure looks like "missing information", ask first **whose** information it is — the worker's, the
  orchestrator's, or the library's — before patching.*

## 3. Synthesis and prevention

The two questions share one root: **the value of a whole-program optimizer is its global reasoning, and
that reasoning is encoded as ambient, undocumented assumptions. Decoupling it into per-module compilation
is therefore expensive twice over — once to re-establish each hidden global obligation, and once to
*diagnose* failures that now appear far from their cause, in new components, through new interactions
(shared state), with no fast reproducer because the legacy suite still tests the masked path.**

Concrete prevention for the remaining decoupling work (method-tagging, self-host — roadmap in
[[separated-compilation-cli-lib]]):

1. **Make pathologies fast to reproduce at suite level.** Add a recursive-`Generic`/`Eq` fixture to
   diffPurwc (and a `summary`/`finalMod` *size* assertion, not just byte-parity) so any future
   context-sensitivity or blow-up trips a fast guard, not a 200 s metatheory build.
2. **Keep stage-isolating switches first-class.** `-g` (skip Binaryen optimize), `--dump-mir`, and
   per-stage size probes turned the diagnosis; treat "bisect the pipeline" as the default first move on
   any "hang/too-big" report.
3. **Write down the global obligations SMO must re-establish**, as an explicit checklist, instead of
   rediscovering them: (a) dead-code is *not* dropped by the worker → over-export; (b) the inline/reduce
   context is an *approximation* of whole-program; (c) the artifact store is *shared mutable state*
   (self-pollution class); (d) reduction-awareness (Layer C) is *owed*, not optional, once DCE no longer
   masks it.
4. ~~**Finish the principled fix (method-tagging, 0001 §4)**~~ **Finish the principled fix — the
   *per-reference reduction-aware inline gate*, NOT method-tagging** (corrected 2026-06-22, see
   [0005](0005-layer-c-attempt-and-the-size-cap-decision.md)). Method-tagging was implemented and is
   byte-neutral on this blow-up (it targets the wrong shape; the blow-up is generic-helper *composition*
   via a partial application). The invariant ("inline only when it reduces") is realised by a per-reference
   gate combining **complexity + size + usage** decided *before* unfolding (purs-backend-es `shouldInline*`),
   not by tagging projected methods. **Caveat:** a *magnitude* guard (a function-size budget) likely
   **remains** regardless — this project lowers to Wasm and feeds **super-linear Binaryen** `B.optimize`,
   which purs-backend-es (a JS backend) does not have. So the end-state is "per-reference gate in the
   reducer **plus** an explicit codegen-side size budget for Binaryen", and the cap is **accepted** as the
   current stand-in for both (0005 §5–§6).
