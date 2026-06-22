# Portable field notes: inlining blow-ups and separate-compilation decoupling

- Date: 2026-06-22
- Purpose: a **project-agnostic** distillation of investigations 0001–0003, written so the lessons
  transfer to any optimizing compiler (the file:line / framework-specific detail lives in 0001–0003).
- Audience: someone building an optimizing compiler with an inliner and/or moving from whole-program to
  separate (per-module) compilation.

Three roles are used abstractly below: the **reducer** (the pass that inlines/simplifies), the **backend**
(a downstream optimizer/codegen that is super-linear in function size, e.g. an external SSA optimizer),
and the **artifact store** (any persisted intermediate interface/object a separate-compilation build
reads back).

---

## Lesson 1 — A static size/use inlining heuristic cannot be made correct

A threshold like *"inline a binding iff it is small OR used once"* is structurally unable to separate the
good case from the bad, because **the same binding shape sits on both sides**:

- *Small, multi-use, and it **reduces** after inlining* (a monad combinator that collapses to
  straight-line code; a dictionary method that saturates to a primitive) → a **net shrink**. You want it.
- *Small, multi-use, and it **does not reduce*** (CPS / fusion combinators that just produce more CPS) →
  a **net grow**. You don't.

Same size, same use-count, opposite outcome. **The real discriminator is "does inlining lead to a
reduction (β / projection / known-case / saturation)", not how big or how-used the binding is.** Tighten
the threshold and you regress the collapses; loosen it and the non-reducing ones explode. There is no
threshold setting that wins both — it is a wall, not a tuning problem.

## Lesson 2 — Acyclic does not mean bounded (the diamond)

Inliners keep their inline set **acyclic** so the fixpoint terminates. Termination is **not** size-bounding.
The pathological shape is a **diamond** — a binding that references another inlinable binding **more than
once**:

```
b0 = leaf
bᵢ = f b₍ᵢ₋₁₎ b₍ᵢ₋₁₎      -- each level references the previous TWICE
```

Inlining `b_d` duplicates `b_{d-1}` twice, each duplicating `b_{d-2}` twice, … → **2^depth** copies. It
*terminates* (acyclic) but only after building an exponential term. CPS/fusion pipelines are literally this
shape. **Takeaway: "the inline set is a DAG" buys termination, never a size bound.** Keep a depth-`d`
diamond as a permanent regression fixture; its normal form must stay linear in `d`.

## Lesson 3 — Make inlining a *consequence of evaluation*, decided from the residual

The fix to Lessons 1–2 (this is the NbE / `eval`–`quote` architecture, à la modern PS/Haskell backends):
don't feed a pre-selected set to a rewriter. **Evaluate** the program into a semantic domain where
redexes fire **only when operands are known**, leaving *neutrals* (stuck terms) where they are not; then
**reify** (`quote`) and decide per binding, from its **residual** usage and whether it reduced:

- **inline** if it reduced / is used at most once / is trivial;
- **retain a single shared `let`** if it is multi-use and did **not** reduce (compute once, reference —
  never copy).

This turns the diamond into one shared binding instead of `2^depth` copies, and inlines the collapsing
combinators (because they *do* reduce) — the decision the static heuristic could not make.

**Two traps when you build this:**

1. **The reducer can be exponential even when its output isn't.** A naive `eval`/`quote` recomputes a
   shared value once per use site (in `eval`) and re-reifies it once per path (in `quote`) → Θ(2^depth) in
   the **reducer's work** on the same diamond. Fix with **memoization + a sharing tag**: evaluate each
   inline binding once (memo), tag the shared value, and CSE it in `quote` into one hoisted binding.
   Have the diamond fixture assert *reducer time/output*, not just output size.

2. **Firing redexes is not the same as the reduction-aware *decision*.** It is tempting to ship "the
   reducer + a size cap" and call it reduction-aware. It is not. If the reducer eagerly fires every
   projection/β it *can*, it will happily materialise a dispatch whose terminal `case <opaque> of …`
   **stays stuck** — bulk, not reduction. The actual *decision* ("don't inline when it won't reduce")
   is the hard part and is easy to defer. **If you defer it, you have not solved the problem; you have
   hidden it** (see Lesson 5).

## Lesson 4 — The carrier of recursion decides inline-vs-call (why generics/type-classes are the acute case)

A recursion that lives in a **named binding** (a function, or a self-referential value) is **excluded from
the inline set** (self-reference / a "currently-unfolding" guard breaks the cycle) → it stays a **call** →
safe, regardless of size.

A recursion that lives in a **data structure the reducer inlines** — e.g. a tree of **dictionary records**
(a type-class `Generic`/`Show`/`Eq` instance for an N-constructor type) — is **acyclic** (the dictionary
for a compound type points at *different* dictionaries for its parts), so **nothing stops the unfolding**:
projection fires, β fires, and the whole dictionary DAG is materialised in one pass. For an opaque value
the terminal dispatch never reduces, so it is pure bulk — measured at **~30×** a hand-written equivalent.

**Same recursion, different carrier, opposite fate.** The blow-up "sickbed" is therefore not "type-class
methods" as such but **any large acyclic structure encoded in the inline-able domain (records/dictionaries)
that gets fully unfolded against an opaque argument.** Generics are the acute case because they *encode
structural recursion as a dictionary DAG by construction*.

Design implication: when recursion or large structure must survive as runtime dispatch, ensure it is
carried by something the reducer treats as a **call**, not something it treats as **inlinable data** — or
make the reducer able to *back off* a projection-then-apply when it won't reduce (the "method-tagging"
fix in 0001 §4: tag a projected method with its origin so a non-reducing application can be reified back to
a runtime `dict.method(arg)` call instead of inlined bulk).

## Lesson 5 — Whole-program → per-module is a "decoupling reveals hidden coupling" event

A whole-program optimizer's *value is its global reasoning*, and that reasoning is encoded as **ambient,
undocumented assumptions**. Splitting it into per-module (separate) compilation converts each ambient
assumption into an **explicit obligation you discover only when it breaks.** Three recurring classes:

1. **Dead-code masking.** Whole-program DCE deletes dead code before the backend sees it. A per-module
   worker **cannot see its dependents**, so it **over-exports** and must compile everything — *including
   code that is dead in the whole program*. Any latent pathology in dead code (a huge unused `Generic`
   instance) becomes **live for compilation**. A backend that is super-linear in function/module size then
   hangs on the aggregate it never used to see.

2. **Context approximation.** The reducer's inline/reduction context was *the whole program*; per-module
   it is an **approximation from dependency interfaces**. Different context → different normal form → any
   **magnitude heuristic** (a size cap) tuned against whole-program forms now fires at the wrong point.

3. **Shared mutable artifact store (new failure mode).** Whole-program is one in-memory pass — a module's
   output never becomes its own input. Per-module **persists** interfaces and reads dependencies back; if
   the dependency directory and the output directory are the **same**, a module can read its **own** stale
   artifact and optimize **against itself** → self-pollution (here: a 9× bloat). *This failure cannot exist
   in the monolith; the artifact-caching architecture creates it.* Treat the artifact store as shared
   mutable state and exclude self/own-output explicitly.

**The unifying statement:** the cost of decoupling an optimizer is paid twice — once to **re-establish every
global obligation the monolith took for granted**, and once because failures now appear **far from their
cause, in new components, through new interactions**, with **no fast reproducer** (the legacy suite still
tests the masked whole-program path and stays green).

## Lesson 6 — Why such bugs are slow to diagnose, and how to make them fast

Irreducible (accept these):

- **Stacked independent causes, peeled serially.** One symptom ("it hangs") hid three causes
  (self-pollution, the inline blow-up, over-export-feeds-backend). You cannot see cause *N+1* until cause
  *N* is removed.
- **Symptom far from cause.** The hang surfaced in the *backend optimizer*, ~3 stages downstream of the
  *reducer* bloat that caused it. Every stage in between can plausibly hang, so each must be ruled out.
- **Deep-semantics fix.** Distinguishing reducing from non-reducing inlines is intrinsically subtle (it is
  the same wall as Lesson 1).

Avoidable (the actual lessons):

- **Bisect the pipeline with the cheapest stage-isolating switch *first*.** A one-line "skip the backend
  optimizer" flag isolated the hang to the backend in *seconds*; it should be the **first** probe on any
  "hangs / output too big" report, before theorising about any single stage. Keep such switches
  (`skip-backend`, `dump-IR`, per-stage size counters) first-class.
- **A green test suite is not coverage of a new architecture.** All guards passed throughout because **no
  fixture exercised the property the new architecture stresses** (here: a recursive-`Generic` module under
  per-module compilation). The interface byte-parity contract held for small fixtures and **silently broke
  for the real module, undetected.** When you add an architecture, add a fixture that *stresses its new
  property* and assert *sizes*, not just equality — so the blow-up trips a fast guard, not a multi-minute
  full build.
- **Prefer an invariant over a magnitude heuristic.** A size cap "worked" but is the very thing that
  silently re-tuned itself wrong across the context change (Lesson 5.2). An invariant ("inline only when it
  reduces") does not depend on a magnitude and survives the decoupling. A heuristic also costs more to
  *validate* (you must prove it doesn't regress the wins, e.g. a monad-collapse / stack-safety benchmark).
- **Ask whose information is missing before patching.** A per-module "missing callee / missing info"
  failure has an owner — the worker, the orchestrator, or the *library/source*. Patching it in the wrong
  layer (e.g. widening reachability in the compiler to cover a *library packaging* defect) is a detour;
  fix it where the responsibility lives.

## One-paragraph summary

Static inlining heuristics are unfixable because size/use cannot tell a reducing inline from a
non-reducing one; the principled answer is to make inlining a consequence of evaluation and decide from
the residual (inline if it reduced, share if not). But "firing redexes + a size cap" is **not** that
decision — it materialises stuck bulk (acutely for generic/dictionary dispatch on opaque values, because
structural recursion is carried by an acyclic, fully-inlinable dictionary DAG). Moving such a compiler
from whole-program to per-module compilation does not introduce these pathologies; it **un-masks** them, by
removing the ambient global safety nets (dead-code elimination, whole-program context, no shared mutable
state) the monolith relied on without writing them down — and it makes them slow to diagnose because the
failure appears far from its cause, in new components, with the old test suite still green. The durable
fixes are *invariants, not magnitudes*, and *fixtures that stress the new architecture's property*, not the
masked legacy path.
