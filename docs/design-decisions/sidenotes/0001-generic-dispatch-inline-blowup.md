# Investigation: the `genericShow`/`genericEq` inline blow-up (NbE optimizer)

- Date: 2026-06-21
- Status: root cause understood; partial fixes landed (inc1, inc2, heuristic cap); principled fix (method-tagging) deferred
- Scope note: this resolves the **`genericShow` blow-up** (blocker ①) — the worker compiles `Typecheck`
  in isolation (7.97 s). The **full `metatheory` orchestrate build still fails** on a *separate*,
  unfixed blocker ② — `unknown callee: Data.Number.isFinite`, the ulib `Data.Int` foreign-only shadow
  (ADR 0039/0040, designed not implemented), which aborts ~28 s in, before `Typecheck`. See §7.
- Code touched: `MiddleEnd/Optimize/Semantics.purs`, `MiddleEnd/Optimize/DictElim.purs`, `purwc/.../Compile.purs`
- Related: ADR 0020 (reduction-aware inliner), ADR 0035 (NbE sharing + Layer C), ADR 0032 (caller-homed specialization), ADR 0038 (separated compilation / `purwc`)

## 1. What was investigated

Under separated compilation (ADR 0038), the `purwc` worker compiling `examples/metatheory`'s
`Examples.Metatheory.Typecheck` **hung > 200 s**, while the whole-program / `--per-module-codegen`
build finished in ~18 s. The chain of investigation:

1. **Where is the hang?** Not lowering (all 469 functions lower + validate in ~40 s; `assignProgramReps`
   completes). It is **Binaryen's `B.optimize`** pass on the over-exported module (proven: `-g`, which
   skips `B.optimize`, compiles Typecheck in 5.74 s producing a 1.76 MB unoptimized wasm).
2. **Why is the module 1.76 MB?** The over-exported `genericShow` / `genericEq` instance functions are
   huge: `showTypeError` / `genericShow1` = 14 K MIR nodes, `eqTypedExpr` = 54 K.
3. **Is `genericShow` inherently that big?** **No.** A hand-written `show` of the same 7-constructor
   recursive ADT is **2 KB** of MIR; the derived `genericShow` is **66 KB** — a **~33× blow-up**. So it
   is an **optimizer** problem, not an inherent cost of `Generic`.
4. **Is it a separated-compilation (worker vs whole-program) divergence?** A *first* bug was: the worker
   optimized each module against its **own stale `.pmi` summary** (self-pollution — see §3), inflating
   the summary 9× (4.8 MB vs 631 KB). Fixed. *After* that fix the finalized MIR is byte-identical to
   whole-program, yet the worker still hung — because it **over-exports** the (dead-in-metatheory)
   `genericShow`/`genericEq` and feeds them to Binaryen, which the whole-program build DCE-drops.
   But note (§5): a *live* `genericShow` does **not** hang the whole-program build either — so the real
   issue is the per-function bulk, of which over-export is only an amplifier.

## 2. Root cause (the precise invariant)

The blow-up is the optimizer **inlining a non-reducing dispatch chain carried by a large acyclic DAG of
dictionary records**, materialising stuck bulk instead of reducing.

Concretely, after caller-homed specialization (ADR 0032), `showTypeError = genericShow te` (with `te`
an **opaque** parameter) becomes `genericShow$spec = \x -> dict.genericShow'(from x)` with `dict` a
**concrete** instance dictionary. Normalising that declaration:

- `dict` for a Sum type is `genericShowSum subDict₁ subDict₂` — an **inline-built record**
  `{ genericShow': \v -> case v of Inl(a) -> …; Inr(b) -> subDict₂.genericShow'(b) }`.
- The record projection `dict.genericShow'` **fires** (record is known) → returns the method lambda.
- The method is applied to `from x` (= a *stuck* `case x of …`, because `x` is opaque) → β **fires**,
  but the body's `case <stuck> of Inl/Inr` **stays stuck** — *materialised, not reduced*.
- The `Inr` branch `subDict₂.genericShow'(b)` **projects + applies again** → recurses through the whole
  Rep DAG → all N constructors × their fields unfold in one pass → 14 K–66 K nodes.

The distinguishing invariant (validated against `FibAnd`, §6):

> A binding blows up when the **inline set** (records / dictionaries — a **DAG**) encodes a **large
> acyclic structure** that is **fully unfolded**, because every step (projection, β) *fires* yet the
> terminal `case <opaque> of …` never reduces.
>
> - **Cyclic** recursion (a self-referential CAF like `FibAnd`; a named recursive function `go = … go …`)
>   is **excluded from the inline set** (`selfRef` / `visited` cycle-breaking) → stays a call → safe.
> - **Acyclic** recursion through a **dictionary DAG** (type-class `Generic`/`Show`/`Eq`/… Rep dicts) has
>   no cycle to stop it → fully inlined → blows up when the structure is large.

So the "sickbed" is **not type-class methods per se**, but the **large acyclic dictionary DAG** that
recursive type-class instances build. (A non-recursive or small instance is fine; a *live* use against a
known constructor reduces small — it is the **opaque** argument that leaves the dispatch stuck.)

The reason a genuine recursive function never triggers this: its recursion lives in a **named binding**
(kept as a call, `visited`-protected), whereas type-class recursion lives in a **dictionary record**
(inlined). Same recursion, different carrier — and the carrier decides inline-vs-call.

## 3. What was fixed (the landed changes)

All landed; verified by: compiler unit **163/163** (incl. `NbeStress` diamond + cap, cross-module
`DictElim`), e2e **153/153**, `test:bin` all PASS (`showShadow`/`intGenericShadow`/`genericEq`),
**CountState stack-safe to 1 000 000** (State-monad win preserved), `diffPurwc` **5/5**, worker
Typecheck **7.97 s / 236 KB** (was > 200 s), metatheory whole-program **9.96 s / 91.9 KB**. `purs-tidy`
clean.

- **Self-pollution fix** (`purwc/.../Compile.purs`, commit `74ea8ca`). `loadDepInterfaces` reads *all*
  `.pmi` in the shared `_build` dir (`--deps` == `-O`), **including the target's own** stale `.pmi`.
  Optimizing a module against its own previous summary re-inlines its own `genericShow` instances into
  itself → 9× bloat (`.pmi` 4.8 MB vs 631 KB). Fix: filter `e.summary.name /= mn`. With it, the worker's
  finalized MIR byte-matches whole-program. **This is a real correctness/quality bug independent of the
  inline blow-up.**
- **inc1 — Layer C value-use → call** (`Semantics.purs`, committed). An unfolded inline candidate
  (`SShared q`) that reaches `quote` *unconsumed* (used as a value, never fired a redex) is reified as a
  plain **call** to `q` (or inlined if its body is trivial: alias / literal / nullary ctor), instead of
  CSE-ing the materialised body. Reduction is unchanged, so cross-module dict-elim is intact.
- **inc2 — per-application reduction-aware guard** (`Semantics.purs`, commit `bbf76a3`). In `apply`,
  when an inline-set binding `SShared q` applied to args yields a result **exceeding
  `perAppInlineBudget` (2048)** — measured by the fuel-bounded `exceedsBudget`/`sizeFuel` — keep the
  named call `NApp (NTop q) args` rather than inlining the bulk. **Safety:** the guard fires only on
  inline-set bindings; a genuine recursive function (already a call, never in the inline set) is
  untouched. Effect: surgical, made metatheory whole-program **18 → 6 s**.
- **Heuristic cap** (`DictElim.normalFormSizeCap` 200 000 → **8 000**, commit `bbf76a3`). The per-decl
  fallback (re-reduce with an emptied inline context) catches what inc2 cannot (§4). A specialized
  `genericShow$spec` (14 K–54 K) exceeds 8 000 → falls back → `genericShowSum` etc stay calls → small.
  Empirically separated from genuine code (CountState decls are < 50 nodes); all guards green.

### Why inc2 is insufficient alone (the residual)

inc2 can only fall back to a **named** call (`NTop q`). But the blow-up's terminal step applies an
**anonymous projected method** — `record.genericShow'` is a *field value* (a bare `SLam`), not an
`SShared q`. With no name to call, inc2 cannot keep it as a call, so it β-reduces and materialises the
bulk. The cap is the (coarse, per-declaration) stand-in that achieves the same end state.

## 4. The principled fix (deferred): method-tagging — a backtracking inliner

> **CORRECTION (2026-06-22 — method-tagging implemented and DISPROVEN; see [investigation 0005](0005-layer-c-attempt-and-the-size-cap-decision.md)).**
> `SProjMethod` was implemented and is **byte-neutral on the metatheory blow-up** — the over-exported
> `Typecheck` decl sizes are *identical* with and without it. Reason: this blow-up is **not** a single
> anonymous record-method projection (the shape `SProjMethod` targets) but the recursive derived-`Generic`
> `Eq`/`Show` materialised as a nested **`case` tree** — a *composition* of small `Data.Show.Generic.*`
> helpers, carried by a **partial application** (`genericShow` is arity 3; the instance applies 2 args, so
> the per-application `inc2` guard sees only a size-1 partial `SLam` and the bulk materialises later at
> `quote`, unguarded — 0005 §2). The principled fix is therefore **not** method-tagging but a
> **per-reference reduction-aware inline gate** (complexity + size + usage, decided *before* unfolding,
> à la purs-backend-es), and the `normalFormSizeCap` is **accepted** as the per-declaration stand-in plus
> a size budget for the super-linear Binaryen backend (0005 §5–§6). The design below is kept as the
> historical proposal; do not implement it for this blow-up.

Make the projection-then-apply step reduction-aware, mirroring inc2 but for record-method dispatch.

1. **Tag projected methods with their origin.** In `accessor`, return a new
   `Sem` variant `SProjMethod dictSem fieldLabel methodSem` instead of the bare method lambda, so the
   value remembers *which dictionary field* it came from.
2. **Backtrack on blow-up in `apply`.** When applying `SProjMethod dict l method`:
   - compute `r = apply method args` (try the β);
   - if `exceedsBudget r`: return the **neutral** `NApp (NAccessor l dictNeutral) args` — i.e. keep
     `dict.l(args)` as a *runtime* projection + `call_ref`, **not** the inlined bulk;
   - else: return `r` (it reduced — inline it).

This stops the recursion at the call boundary: `subDict.genericShow'(b)` with `b` opaque stays a runtime
dict dispatch, so `genericShow$spec` collapses to small records-with-method-lambdas + runtime dispatch,
**linear** in the constructor count rather than fully unfolded. It is the precise generalization of
inc2 to anonymous methods, so it covers the *entire* residual class and lets the cap return to 200 K (or
be removed).

**Why deferred (cost / risk):**

- New `SProjMethod` variant must thread through every `Sem` consumer (`quote`, `unShared`, `matchSem`,
  `performSem`, `update`, …), each handling it correctly.
- Reconstructing `dictNeutral` from an inline-built `SRecord` means the dictionary record must survive to
  runtime — interacts with `DictElim.summarize` / DCE / codegen (the dicts must be kept + emitted).
- Must not regress dict-elim (`eq → intEq` must still inline) or the State-monad collapse; needs the full
  guard suite + bench.
- Adds runtime dict dispatch (`call_ref` + record allocation) for the affected methods — the same cost
  the cap fallback already pays, but now per-method-site.

## 5. Key measurements (for reuse)

- Hand-written `show` (7-ctor recursive ADT): ~2 KB MIR. Derived `genericShow`: 66 KB. → **~33×**.
- Metatheory `Typecheck` (post self-pollution fix): `showTypeError`/`genericShow1` = 14 K nodes,
  `eqTypedExpr` = 54 K; over-exported module = 1.76 MB unoptimized wasm.
- Worker Typecheck compile: `-g` (no Binaryen optimize) = **5.74 s**; with optimize = **> 200 s hang**
  (before the cap), **7.97 s** (after inc2 + cap 8000).
- A **live** opaque `genericShow`+`genericEq` over 4–7-constructor recursive ADTs, all exported, builds
  **whole-program in 21.5 s / 221 KB — no hang.** So Binaryen is super-linear but does *not* hang on
  reduced live generic code; the worker hung only on the *aggregate of over-exported, unreduced* dead
  generics. (A genuinely generic-heavy program is a separate, latent scaling concern, not a correctness
  blocker, and affects whole-program equally.)
- CountState (State-monad win) decls: all < 50 nodes — far below any sane cap, so the cap is safe for it.

## 6. `FibAnd` is safe (the control case)

`E2E.FibAnd` is a cyclic top-level CAF (`fibAnd = Fib "fib" \n -> … case fibAnd of …`). It *looks* like
recursive dispatch but its optimized MIR is **749 chars**: `fibAnd` is **kept as a named reference**
(`case E2E.FibAnd.fibAnd of Fib(_, f) -> f(…)` stays a runtime case), never inlined — because it is
**self-referential** (a `rec` group, excluded from the inline set by `selfRef`). This is the empirical
confirmation of §2: cyclic recursion is excluded from inlining (safe); the acyclic dictionary DAG is not
(blows up when large).

## 7. Open follow-ups

- ~~**Blocker ② (separate, gates metatheory end-to-end): ulib `Data.Int` foreign-only shadow →
  `unknown callee: Data.Number.isFinite`.**~~ **RESOLVED 2026-06-21 (ADR 0039 §1/§3/§4 implemented).**
  The "foreign-only" module category is abolished: ulib resolution is now **presence-driven** — a
  module is lib-sourced iff the lib ships a corefn for it (`resolveModuleSet`, `Manifest.purs`). A
  wat-only patch (`Data.Int`, `Data.Show.Generic`) keeps the registry corefn with its **real imports**
  intact, so `Data.Number` is staged + compiled like any other dependency and `isFinite` resolves;
  only the missing foreign comes from the lib. The exact-version `shadowSet` gate was removed (§3,
  lenient versioning). The reverted `expandClosure` workaround is no longer needed. **Verified:**
  `metatheory --orchestrate` builds end-to-end (exit 0, 41 s, 253 KB `index.wasm`); compiler 163 /
  cli-lib 29 / purs-wasm 12 / ulib-tooling 37 unit, e2e 153/153, `test:bin` 12/12, `diffPurwc` 6/6
  (incl. a new `examples-intpatch` orchestrate fixture exercising the `Data.Int → Data.Number` reach),
  `diffPerModule` metatheory behaviour-match. §2's full source-overlay form is deferred to ADR 0040.
- Implement method-tagging (§4) to make the fix fully principled and retire / relax the heuristic cap.
- Consider whether lowering the cap to 8000 ever un-inlines a genuine large declaration in practice
  (none observed across unit/e2e/bin/CountState; the metatheory whole-program stayed 91.9 KB).
- Separate, latent: Binaryen `B.optimize` is super-linear on large functions — relevant if a program
  genuinely uses heavy generic instances live; not the metatheory blocker.
