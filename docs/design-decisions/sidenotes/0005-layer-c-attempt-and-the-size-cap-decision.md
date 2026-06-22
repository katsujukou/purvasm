# Investigation: the Layer C attempt — method-tagging disproven, the size cap accepted, and the real path forward

- Date: 2026-06-22
- Status: **decision recorded** — the `DictElim.normalFormSizeCap` is **accepted** as the principled
  per-declaration stand-in for the (still-deferred) per-reference reduction-aware inline decision. No
  optimizer code changed this session; everything attempted was reverted (the repo is at the committed
  ulib-patch state, optimizer clean).
- Companion to 0001 (the genericShow blow-up + the *proposed* method-tagging fix), 0002 (the original
  fixed-threshold exponential), 0003/0004 (the WPO→SMO retrospective + portable lessons).
- Code touched (all **reverted**): `MiddleEnd/Optimize/Semantics.purs`, `.../DictElim.purs`, `MiddleEnd.purs`,
  `compiler/test/NbeStress.purs`. Verified against the **purs-backend-optimizer** source
  (`~/workspace/purescript-backend-optimizer`).

## TL;DR

The goal was to **replace the `normalFormSizeCap` heuristic with principled reduction-awareness** (the
deferred ADR 0035 Layer C). After empirically testing four fixes — all reverted — the conclusion is:

> The metatheory `genericShow`/`genericEq` blow-up is a **size** phenomenon of a specific class (0001),
> not the duplication exponential that reduction-awareness fixes (0002). The cap is the **right tool**
> for it: a per-declaration size budget that is simultaneously (i) the post-hoc stand-in for the
> unimplemented per-reference inline decision, and (ii) a function-size budget for the **super-linear
> Binaryen backend** (which purs-backend-es, a JS backend, does not have — and so does not need a cap).
> **`normalFormSizeCap = 8000` is accepted.** It is not a 0040 blocker (a deterministic constant).

The *principled* fix that would let the cap relax is **not** method-tagging (0001 §4) — that was
disproven here — but a **per-reference reduction-aware inline gate** à la purs-backend-es
(`shouldInline*`: complexity + size + usage, decided *before* unfolding). See §6.

## 1. What was attempted, and why each was reverted (all empirical)

| # | Attempt | Result | Why |
|---|---|---|---|
| 1 | **method-tagging** (`SProjMethod`, 0001 §4): tag a projected dict method so a non-reducing application backtracks to a runtime `dict.l(args)` | **byte-NEUTRAL** on the blow-up (decl sizes *identical* with/without) | The blow-up is **not** a single record-method projection. It is recursive derived-`Generic` `Eq`/`Show` inlined as a nested **`case` tree** — a *composition* of small `Data.Show.Generic.*` helpers (genericShow/genericShowSum/genericShowArgsProduct/from/to, all size ≤ 32). `SProjMethod` targets the wrong shape. |
| 2 | **Exclude `.Generic.` helpers from the inline set** (`coreSelect`) | **false positive** | `--per-module-codegen` was *already* fine at cap=200K with or without the exclusion; only the **orchestrate worker** hangs, and the exclusion did not change the worker. (The worker's blow-up source is not the simplifier inline set the exclusion touched — see §3/§4.) |
| 3 | **Partial-application guard**: re-wrap a partial `apply (SShared q s) args` in a budget-guarded `SLam` so the eventual saturation is bounded | **did not bound it** (still hung) | The guard runs `apply r more` (a full materialisation) at **every** composition level (genericShow → Sum → Constructor → …); the nested guards **compound** — *more* expensive than the per-declaration cap, which materialises once. |
| 4 | **Defer the worker's `B.optimize` to post-merge** (worker emits un-optimized wasm; orchestrator optimizes the merged, DCE'd whole) | **incomplete** | Only helps the **dead** case (whole-program DCE drops dead generics before the post-merge optimize). A **live** `genericShow` on a recursive ADT still materialises the big dispatch; Binaryen is super-linear on it regardless of dead/live. |

## 2. The pinned mechanism (why the committed `inc2` guard misses it)

Traced with temporary size instrumentation in `MiddleEnd.localOpt` (per pipeline stage) and in
`Semantics.apply`'s `SShared` case:

- **Locus = the NbE simplify** (`DictElim.simplifyModule → reduce → normalize`), **not** Specialize.
  `showTypeError` is < 20 nodes *post-specialize* (`{ show: genericShow dict }`) and grows to 1599 →
  4794 *during simplify*. The big siblings (`showTypedExpr` 133 K, `eqTypedExpr` 54 K) exceed the cap
  *inside* `simplifyModule` and are re-reduced small there — which is why a post-simplify probe only
  sees the under-cap ones.
- **The carrier is a partial application.** `Data.Show.Generic.genericShow` has **arity 3**; the
  instance `show = genericShow dict₁ dict₂` applies **2 of 3** args. `apply (SShared genericShow) [d₁,d₂]`
  returns a **partial `SLam/1`**, which `exceedsBudget` counts as **1 node** (an unapplied lambda is
  opaque) → under budget → inlined as the partial, **tag dropped**.
- **The materialisation is deferred to `quote`.** Reifying the partial `SLam` applies it to a fresh
  neutral (the missing value arg) → the whole generic dispatch materialises — but now on a **bare**
  `SLam` (no `SShared`), so **no `inc2` guard fires**. The per-declaration `normalFormSizeCap` is the
  only thing that catches the materialised result.
- **The good/bad discriminator for this class is *size*.** `eq eqInt a b` (a,b opaque) → `intEq(a,b)`
  (small neutral, **good**); `genericShow dict te` (te opaque) → 133 K case tree (big neutral, **bad**).
  Both *fire redexes* and both *produce a neutral*; only size separates them. This is the **0001 class**.
  It is distinct from the **0002 class** (fusion's `2^depth` duplication), which *is* what reduction-aware
  inline-or-share fixes. Reduction-awareness does not, by itself, decide 0001 — a size element is
  intrinsic (see §6, and purs-backend-es's own `size < N` thresholds).

## 3. Why the worker hangs but whole-program / `--per-module-codegen` do not

- **Whole-program and `--per-module-codegen` DCE before codegen.** They have whole-program reachability,
  so `Show/Eq TypedExpr` (dead — never reached from `Main`) are dropped before lowering/Binaryen
  (`Typecheck.wasm` has **3 exports**). They build at **cap = 200 K** (8.5–9 s).
- **The SMO worker cannot see its dependents → it over-exports every function** (ADR 0038), so it must
  compile the dead 133 K, and `B.optimize` (super-linear) hangs on it. `B.optimize` is invoked
  **per-module in the worker** (`Compiler.purs:271`); the orchestrator's post-merge pass
  (`Build.purs:629`) only runs `remove-unused-module-elements` (DCE), **no** `B.optimize`.
- **Therefore `cap = 8000` is load-bearing *only* for the worker's over-export.** It is the dead-code
  masking of 0003/0004 Lesson 5.1 made concrete: *a pathology dead-in-the-whole-program is
  live-for-compilation in a single module.*

## 4. purs-backend-es ground truth (verified against source)

Checked `~/workspace/purescript-backend-optimizer/src/PureScript/Backend/Optimizer/{Semantics,Analysis}.purs`:

- **NbE** (`BackendSemantics`/`eval`/`quote`), **JS-only** codegen (`Codegen/EcmaScript*`); **no
  binaryen/wasm anywhere** — i.e. **no super-linear downstream optimizer**.
- **It does have numeric size thresholds**, but **pre-emptive and per-reference**, integrated into the
  reduction-aware decision — *not* a post-hoc "re-reduce if the result > N" cap. `BackendAnalysis`
  carries `complexity :: Complexity` (`Trivial | Deref | KnownSize | NonTrivial`), `size :: Int`
  (node count via `bump`), and per-binding `Usage { total, captured, call }`. The decisions:

  ```purescript
  shouldInlineLet level a b = … case Map.lookup level s2.usages of
    Just (Usage { captured, total, call }) ->
      (s1.complexity == Trivial)
        || (captured == CaptureNone && total == 1)
        || (captured <= CaptureBranch && s1.complexity <= Deref && s1.size < 5)
        || (s1.complexity == Deref && call == total)
        || (s1.complexity == KnownSize && total == 1)
        || (isAbs a && (total == 1 || Map.isEmpty s1.usages || s1.size < 16))
        || (isKnownEffect a && total == 1)

  shouldInlineExternReference _ (BackendAnalysis s) _ = s.complexity <= Deref && s.size < 16
  shouldInlineExternApp _ (BackendAnalysis s) _ args =
    (s.complexity <= Deref && s.size < 16)
      || (Map.isEmpty s.usages && not s.externs && s.size < 64)
      || …
  ```

- **So purs-backend-es avoids the blow-up by *not inlining* the `NonTrivial` generic dispatch at
  multi-use / non-folding sites** (the `complexity`/`size`/`usage` gate fails) → it **never builds** the
  133 K. It needs no cap because (a) the per-reference gate is pre-emptive, and (b) even if it inlined
  big, its downstream is **linear** (JS text → V8), not a super-linear external optimizer.

**Correction to an earlier claim in this investigation:** "purs-backend-es has no size threshold; the
only difference is Binaryen" is **wrong**. purs-backend-es uses size thresholds; the real differences are
(1) **pre-emptive per-reference gate** vs this project's **unconditional unfold + post-hoc cap**, and
(2) **no super-linear backend**.

## 5. Why the cap is accepted (the decision)

- **It is correct for the 0001 class.** A per-declaration size budget bounds the materialised generic
  dispatch; it is the post-hoc analogue of purs-backend-es's pre-emptive per-reference gate.
- **It is a hard requirement for the Wasm/Binaryen backend.** `B.optimize` is super-linear in function
  size; a 133 K function hangs it (proven: `-g`/optimize-off compiles in seconds, 0001 §5). A backend
  guard bounding the function size handed to Binaryen is needed **regardless** of how good the reducer is
  — purs-backend-es is exempt only because it has no such backend.
- **Saturated, known-constructor `genericShow` still folds.** Verified by `--dump-mir` on a small
  `derive Generic` ADT: `show (Blue n)` reduces to the **resolved** dispatch (`let ctor = "Blue" in …
  showIntImpl(n) …`), small — the cap does not fire there. (Not the literal `"(Blue 2)"`: the leaf
  `showIntImpl` and the `intercalate` array machinery remain as small runtime ops, as in purs-backend-es
  where `showIntImpl` is a foreign.) So the cap does not cost the full-saturated optimisation.
- **It is not a 0040 blocker.** The cap is a fixed constant applied per-declaration; the artifact is a
  deterministic function of `(source, dep `.pmi`, toolchain)`. `cap = 8000` works on every path
  (whole-program, `--per-module-codegen`, **and** the orchestrate worker — metatheory orchestrate builds
  at 8000).

**Decision: keep `normalFormSizeCap = 8000`** and understand it as a *backend size budget + a stand-in
for the deferred per-reference decision*, not a magic number to be removed.

## 6. What is NOT done — the real path forward (for future implementers)

**The per-reference reduction-aware inline *decision* (ADR 0035 Layer C / ADR 0020 stage 3) is still
deferred.** Today `Semantics.evalVar` unfolds an inline-set binding **unconditionally**
(`SShared q (force lz)`); `inc1` (`quote` of an unconsumed `SShared` → call), `inc2` (`apply` budget),
and the `normalFormSizeCap` are all **post-hoc** guards.

To relax/remove the cap (the optimizer half of it; the backend half may remain — see below), implement
the **purs-backend-es-style per-reference gate** — and note this is a **bounded** change (an analysis +
a ~10-line predicate), **not** the large spine-threading rewrite earlier feared:

1. **Add a per-binding analysis** equivalent to `BackendAnalysis`: `complexity` (Trivial/Deref/KnownSize/
   NonTrivial), `size` (node count), and `Usage` (total / captured / call) per reference. (`Analysis.purs`
   already has `exprSize`/`references`; the complexity lattice + usage counting are the new parts.)
2. **Decide inline-vs-call at the reference site, *before* unfolding** (in `evalVar`, or where a `Var`'s
   spine is known): inline only when the binding is `Trivial`/`Deref`, or single-use, or small
   (`size < ~16`) — i.e. mirror `shouldInlineLet`/`shouldInlineExternApp`. A `NonTrivial` multi-use
   binding (the generic dispatch) stays a **call**, so the 133 K is **never built**. This subsumes the
   inline-set `size ≤ 32` pre-selection (which is the right idea but applied at *set construction* and
   then unfolded unconditionally).
3. **Preserve the prized collapses** (the validation surface, more expensive than for an invariant): the
   State-monad collapse (`Test.E2E.StackSafe`, `bench/CountState`), the Effect collapse (`effectPrim`/
   `effectRefMain`), dict-elim (`eq → intEq`, `showShadow`/`intGenericShadow`), the fusion convergence
   (`NbeStress` diamond), and the byte-equal-modulo-rename bench gate (`countEffect`/`curry`/`mapFoldArray`).
4. **Reference:** purs-backend-optimizer `Semantics.purs` `shouldInlineLet` / `shouldInlineExternApp` /
   `shouldInlineExternReference` and `Analysis.purs` `Complexity` / `Usage` / `bump`.

**The backend size guard likely survives even after Layer C.** purs-backend-es needs no cap partly
because its downstream (JS/V8) is linear. This project lowers to Wasm and runs **Binaryen `B.optimize`
(super-linear)**. A *live* large generic dispatch (a program that genuinely `show`s a big recursive ADT)
still produces a big function; whether to inline it (big, fast) or keep a runtime dict call (small) is a
**function-size-vs-Binaryen** trade-off that belongs at the **lowering→codegen boundary**, not the
optimizer. So a size budget — the cap's role (ii) — may remain as an explicit Wasm-backend guard. The
right end-state is probably: per-reference reduction-aware gate in the reducer (removes the *optimizer's*
need for `normalFormSizeCap`) **plus** an explicit, documented codegen-side function-size guard for
Binaryen (replacing role (ii) of the cap, ideally measured against the actual Binaryen scaling).

**SMO amplifier — handle separately.** The worker over-exporting dead code (§3) is the amplifier, not the
cause. Constraints for any mitigation: per-module dead-code judgement is legitimate **only** for
optimizer-*synthesised* non-exported declarations (`…$specN`) — a `corefn`-exported decl may be used by a
dependent and **must not** be dropped by the worker. Whole-program DCE at the *link* (entry-rooted) is the
legitimate place to drop a `corefn`-exported-but-program-unreachable decl. Deferring the worker's
`B.optimize` to post-merge (attempt #4) is sound for the dead case but does not bound a *live* big
generic — so it is at best a complement to the size guard, not a replacement.

## 7. Corrections this session makes to the companion docs

- **0001 §4** ("the principled fix = method-tagging"): **method-tagging does not fix this blow-up** —
  empirically byte-neutral, because the blow-up is generic-helper *composition* (a `case` tree), not the
  anonymous-projected-method shape `SProjMethod` addresses. The principled fix is the per-reference
  reduction-aware gate (§6). (0001 §4 annotated.)
- **0003 recommendation #4** and **0004 Lesson 4** ("finish the principled fix (method-tagging)"):
  superseded — read "the principled fix" as the **per-reference reduction-aware inline gate** (§6), and
  note the **Wasm-backend size guard may remain regardless** (purs-backend-es is exempt only because it
  has no Binaryen). (Both annotated.)
