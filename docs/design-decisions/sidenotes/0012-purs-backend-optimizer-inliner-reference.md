# Reference study: how the purs-backend-optimizer NbE inliner works

- Date: 2026-07-07
- Type: prior-art reference (not a code change). The **mechanism** companion to 0001–0005, which
  record the *lessons*; this records the *reference implementation those lessons point at*.
- Purpose: the ADR-0082 optimiser track builds Level 2's own best-in-class NbE general inliner, and the
  family lessons ([general-inliner-study-first], sidenotes 0004 §Lesson 3 / 0005 §6) all resolve to
  "do what purs-backend-es does." This note is the close reading of that reference so the Level-2 port
  starts from the exact shapes, not a paraphrase.
- Source read: `~/workspace/purescript-backend-optimizer` at commit `4810dc0`
  (`src/PureScript/Backend/Optimizer/{Semantics,Analysis,Convert,Syntax,Directives}.purs`,
  `Semantics/Foreign.purs`, `docs/optimized-pattern-matching.md`).
- Companion result at the end (§10): a **measured** confirmation that LLVM `-O2` is super-linear in
  single-function size on our toolchain — the data behind the "a codegen size budget likely remains"
  caveat of 0005 §6.

## 0. The one-sentence model

Inlining is **a consequence of evaluation**: the program is `eval`'d into a semantic domain where a
redex fires **only when its operands are known**, leaving *neutrals* where they are not; `quote` reifies
back and, **per reference, before unfolding**, decides inline-vs-share from a cached analysis
(`complexity` + `size` + `usage`). The driver iterates `quote ∘ eval` to a fixpoint. There is **no
pre-selected inline set fed to a rewriter** (the design 0002 showed cannot be made correct), and **no
post-hoc output cap** (the purs-wasm `normalFormSizeCap` stand-in of 0001/0005) — instead there are
**size *thresholds* embedded in the per-reference gate**.

## 1. The pipeline: an `eval`/`quote` fixpoint

`optimize` (Semantics.purs:1649) is the driver — iterated normalization-by-evaluation, not a single
pass:

```purescript
optimize traceSteps ctx env (Qualified mn (Ident id)) initN originalExpr =
  go (…) initN originalExpr
  where
  go steps n expr1 = do
    let Tuple rewrite expr2 = goStep n expr1
    if rewrite then go newSteps (n - 1) expr2
    else Tuple (…) expr2
  goStep n expr1
    | n == 0 = unsafeCrashWith (name <> ": Possible infinite optimization loop.")
    | otherwise = do
        let expr2 = quote ctx (eval env expr1)
        let BackendAnalysis { rewrite } = analysisOf expr2
        Tuple rewrite expr2
```

Each step is a full `quote ctx (eval env expr1)` round-trip. The loop repeats while the freshly-quoted
expression's cached `BackendAnalysis.rewrite` flag is set, bounded by `rewriteLimit` (10 000, set in
`Builder.purs`). The final `BackendExpr` is turned into the serializable `NeutralExpr` by `freeze`
(Semantics.purs:1671), which folds all recorded rewrites down into plain syntax.

```
CoreFn ─Convert─▶ BackendExpr ──optimize (iterate quote∘eval to fixpoint)──▶ freeze ─▶ NeutralExpr ─▶ codegen
```

## 2. Three IR representations

1. **`BackendSyntax a`** (Syntax.purs) — the syntax tree. `Var (Qualified Ident)` (top-level),
   `Local (Maybe Ident) Level` (a **de Bruijn *level***), `App`/`Abs` (curried),
   `UncurriedApp`/`UncurriedAbs`, `UncurriedEffectApp`/`Abs`, `Accessor`, `Update`,
   `CtorSaturated`/`CtorDef`, `LetRec`/`Let`, `EffectBind`/`EffectPure`/`EffectDefer`,
   `Branch (NonEmptyArray (Pair a)) a` (a `case` **already lowered** to a cond-chain + default — see
   §9), `PrimOp (BackendOperator a)`, `PrimEffect`, `PrimUndefined`, `Fail`.
2. **`BackendSemantics`** (Semantics.purs:38) — the semantic (NbE "value") domain. Functions are held as
   **host functions** (`SemLam (Maybe Ident) (BackendSemantics -> BackendSemantics)`, i.e. HOAS), so β is
   a meta-language application. The `Sem*` constructors are reducible; the `Neut*` constructors are
   **neutrals** (stuck terms — `NeutVar`, `NeutLocal`, `NeutApp`, `NeutData`, `NeutLit`, `NeutPrimOp`, …).
3. **`BackendExpr = ExprSyntax BackendAnalysis (BackendSyntax BackendExpr) | ExprRewrite BackendAnalysis
   (BackendRewrite BackendExpr)`** (Semantics.purs:68) — the `quote` output. **Every node caches a
   `BackendAnalysis`.** `ExprRewrite` records a *discovered but not-yet-applied* rewrite
   (`RewriteInline`, `RewriteUncurry`, `RewriteUnpackOp`, `RewriteDistBranches*`, `RewriteStop`), applied
   on the next `eval` (the `instance Eval BackendExpr` interprets `ExprRewrite`, Semantics.purs:300).

Separating rewrite **discovery** (in `quote`/`build`) from rewrite **application** (in the next `eval`)
is what makes the fixpoint loop composable.

## 3. `SemRef`: the carrier that defers the inline decision

The single most important type for the port:

```purescript
SemRef EvalRef (Array ExternSpine) (Lazy BackendSemantics)     -- Semantics.purs:39
data ExternSpine = ExternApp (Spine …) | ExternUncurriedApp (Spine …)
                 | ExternAccessor BackendAccessor | ExternPrimOp BackendOperator1
data EvalRef = EvalExtern (Qualified Ident) | EvalLocal (Maybe Ident) Level
```

A reference to a top-level or local binding is evaluated to a `SemRef` that **accumulates the spine of
applications / accessors / prim-ops applied to it** while **deferring the body's unfolding behind a
`Lazy`**. `evalApp`/`evalAccessor`/`evalPrimOp`, on meeting a `SemRef`, just push onto the spine
(`evalRef` → `snocSpine`, Semantics.purs:906/935) — they **do not unfold**. The decision to unfold (or
keep a call) is taken later, when the spine is settled (`evalExternFromImpl`, §5) and at `quote`. This is
the structural fix for the purs-wasm failure mode where `evalVar` unfolded **unconditionally** and only a
post-hoc cap caught the bulk (0005 §6).

## 4. `BackendAnalysis`: the gate's input (Analysis.purs)

`analyze` (Analysis.purs:203) composes, bottom-up, a `BackendAnalysis` per syntax node:

```purescript
newtype BackendAnalysis = BackendAnalysis
  { usages :: Map Level Usage, size :: Int, complexity :: Complexity
  , args :: Array Usage, rewrite :: Boolean, deps :: Set (Qualified Ident)
  , result :: ResultTerm, externs :: Boolean }

data Complexity = Trivial | Deref | KnownSize | NonTrivial   -- a lattice; append = max
newtype Usage = Usage { total, captured :: Capture, arities :: Set Int, call, access, case, update :: Int }
data Capture = CaptureNone | CaptureBranch | CaptureClosure  -- append = max
data ResultTerm = KnownNeutral | Unknown
```

Load-bearing assignments (from `analyze`):
- **`complexity`**: literals → `Trivial`; `Accessor (Local …)` → `Deref`; `Abs`/non-empty
  array/record/`LitString > 128` → `KnownSize`; `Let`/`App`(saturated)/`Branch`/`CtorDef`/`Update` →
  `NonTrivial`.
- **`Usage` per `Level`**: `total` (occurrences), `captured` (None < Branch < Closure — did the use sit
  under a lambda/branch), `arities` + `call` (from `callArity` at `App`/`UncurriedApp` of a `Local`),
  `access`/`case`/`update` (accessor / `OpIsTag` / record-update uses).
- **`size`**: node count via `bump`. Feeds the `< 5` / `< 16` / `< 64` thresholds.
- **`result`**: `KnownNeutral` vs `Unknown` — gates branch distribution (§7).

`bound`/`boundArg` drop a level's usage when leaving its scope; `capture` raises all captured-flags when
entering a lambda/branch; `withArgs` threads a binding's per-argument `Usage` array (`args`) so an
extern's call sites can be judged argument-by-argument (§5).

## 5. The reduction-aware inline gate — the payoff

The gate lives in **two** places, one per reference kind.

**(A) Top-level / cross-module references** — `evalExternFromImpl` (Semantics.purs:947) dispatches on the
settled `ExternSpine` shape and consults `shouldInline*`:

```purescript
shouldInlineExternReference _ (BackendAnalysis s) _ =
  s.complexity <= Deref && s.size < 16

shouldInlineExternApp _ (BackendAnalysis s) _ args =
  (s.complexity <= Deref && s.size < 16)
    || (Map.isEmpty s.usages && not s.externs && s.size < 64)   -- closed & small
    || (delayed && Array.length s.args <= Array.length args && s.size < 16)
    || (delayed && or (Array.zipWith shouldInlineExternAppArg s.args args) && s.size < 16)
  where delayed = Array.length s.args > 0

shouldInlineExternAccessor _ (BackendAnalysis s) _ _ = s.complexity <= Deref && s.size < 16
```

Because these are checked **before** unfolding, a `NonTrivial` large binding (a recursive-`Generic`
dispatch — the 0001 blow-up) fails every clause and **stays a call: the 133 K function is never built.**

**(B) Local `let`** — the smart constructor `build` (§6) calls `shouldInlineLet` (Semantics.purs:1581):

```purescript
shouldInlineLet level a b = case Map.lookup level s2.usages of
  Nothing -> true                                          -- dead binding: drop it
  Just (Usage { captured, total, call }) ->
    (s1.complexity == Trivial)
      || (captured == CaptureNone && total == 1)           -- linear, uncaptured: always
      || (captured <= CaptureBranch && s1.complexity <= Deref && s1.size < 5)
      || (s1.complexity == Deref && call == total)
      || (s1.complexity == KnownSize && total == 1)
      || (isAbs a && (total == 1 || Map.isEmpty s1.usages || s1.size < 16))
      || (isKnownEffect a && total == 1)
```

A **multi-use, non-reducing, non-trivial** binding satisfies none of these → it is **kept as a single
shared `let`, never copied** (the fix for the 0002 diamond: `2^depth` copies collapse to one shared
binding).

`InlineDirective`s (§8) are consulted **first** and override the heuristic: `InlineNever → NeutStop`
(a hard stop, quoted back as a plain call), `InlineAlways → unfold`, `InlineArity n → unfold iff ≥ n
args applied`. `envForGroup`/`addStop` insert an `InlineNever` self-stop before recursively evaluating an
inlined group, so a recursive group cannot unfold forever.

> **On size *thresholds* vs a size *cap*.** The numbers `< 5` / `< 16` / `< 64` / `<= 128` are real, but
> they are **pre-emptive, per-reference** inputs to the gate, *not* purs-wasm's post-hoc per-declaration
> `normalFormSizeCap` (unfold unconditionally, then re-reduce if the result exceeds N). This distinction
> is the correction 0005 §4 makes: purs-backend-es *has* size thresholds and *has no* output cap.

## 6. `build`: algebraic rewrites the fixpoint drives (Semantics.purs:1284)

`quote` calls `build` at every reconstructed node. Besides `shouldInlineLet`, `build` discovers (records
as `ExprRewrite`, sets `rewrite = true`):

- **redundant-test / boolean simplification** on `Branch` — `buildBranchCond` + `simplifyCondIsTag` /
  `simplifyCondBoolean` / `simplifyCondLiftAnd` / `simplifyCondRedundantElse`.
- **case-of-case / branch distribution** — `shouldDistributeBranchApps` / `…Accessor` / `…PrimOp1/2L/2R`
  push an application/accessor/op **into each branch** (guarded by operand `complexity <= Deref` and, for
  the let form, `s2.size <= 128` and `result == KnownNeutral`). This is the reduction the NbE inliner most
  wants a *structured* scrutinee for.
- **unpacking** — `shouldUnpackRecord` / `…Ctor` / `…Array` / `…Update`: `let x = {a,b} in … x.a …`
  becomes per-field lets (fires only when `total == access (+ update/case)`, i.e. the binding is used
  *only* through projection).
- **uncurry / eta** — `shouldUncurryAbs` (a let-bound lambda always applied at one arity), `shouldEtaReduce`.
- **Effect normalisation** — `EffectBind (EffectPure b) k → EffectDefer (Let b k)`, `EffectDefer` collapse,
  `not (not x) → x`, `App (App …) → App …` / `Abs (Abs …) → Abs …` flattening.

## 7. Foreign constant-folding (Semantics/Foreign.purs)

A rewrite table, `coreForeignSemantics :: Map (Qualified Ident) ForeignEval`, wired into evaluation
through `Env.evalExternSpine` (supplied by `Convert.makeExternEvalSpine`):

```purescript
type ForeignEval = Env -> Qualified Ident -> Array ExternSpine -> Maybe BackendSemantics
```

On a `Var qual`, the driver looks `qual` up in the table; if a rule fires it wins, else it falls back to
inlining the real implementation (`evalExternFromImpl`). Rules match on the accumulated spine and are
retried as `evalRef` accretes arguments — so a curried operator folds once **saturated**. A rule
*lowers* a foreign into a primitive, e.g.

```purescript
primBinaryOperator op env _ = case _ of
  [ ExternApp [ a ] ] -> Just $ makeLet Nothing a \a' -> SemLam Nothing \b' ->
                                  evalPrimOp env (Op2 op a' b')   -- 2nd arg kept as SemLam
  _ -> Nothing
-- Data.Semiring.intAdd ↦ Op2 (OpIntNum OpAdd); Data.Ord.* ↦ compare+OpIsTag shape ↦ Op2 (op OpLt) …
```

`evalPrimOp` (Semantics.purs:627) then constant-folds when both operands `deref` to `NeutLit`, applies
algebraic identities on partially-known operands (`x && false → false`, `0 - y → negate y`), or leaves a
`NeutPrimOp` neutral (`floatLet`-ing the operands). Categories covered: Semiring/Ring/EuclideanRing,
Eq/Ord, HeytingAlgebra, Int.Bits, Semigroup, Array `length`/`unsafeIndex`, String.CodePoints, Effect/ST/
Ref, Record.Builder/Unsafe, `unsafeCoerce` (erases itself), `Function.Uncurried mkFn/runFn`. Registration
is a flat array → `Map.fromFoldable`; arity families (`mkFn1..10`, …) via `map builder (Array.range 1 10)`.
New/overriding rules enter through the same `foreignSemantics` option.

## 8. Directives and the cross-module driver (Directives.purs, Convert.purs, Builder.purs)

- **Directives.** `-- @inline Mod.foo arity=2` / `always` / `never` / `default`, and `-- @inline export
  foo.append arity=1`, are lexed (via the CST lexer) into `InlineDirectiveMap = Map EvalRef (Map
  InlineAccessor InlineDirective)`, where `InlineAccessor = InlineRef | InlineProp l | InlineSpineProp l`
  (the last two address instance-dictionary methods `dict.method`). Applied in `evalExternFromImpl` (§5),
  overriding the heuristic.
- **Two environments.** `ConvertEnv` is the reader context (levels, `dataTypes`, `implementations`,
  `directives`, `foreignSemantics`). The eval `Env` (Semantics.purs:178) is built fresh per binding, its
  `evalExternRef`/`evalExternSpine` closing over `ConvertEnv` to resolve cross-module/foreign references
  at eval time.
- **Cross-module.** `Builder.buildModules` walks modules in **topological order**, threading
  `implementations :: Map (Qualified Ident) (Tuple BackendAnalysis ExternImpl)` — each dependency's
  **cached analysis + frozen implementation**. A module *reads* it (via `getCtx.lookupExtern` for
  analysis, `makeExternEvalSpine` for eval-time inlining) and *publishes* its own bindings + exported
  directives for dependents. Dead bindings/imports are pruned by each binding's `deps` set.
- **Transitive directive inference** (`inferTransitiveDirective`): after optimising a binding whose result
  is `App (Var qual) args` with `qual` carrying `arity=n`, it auto-adds `arity=(n − len args)` to the
  current binding — thin wrappers inline through a chain without hand-written directives.

## 9. Where this diverges for purvasm (design, not transcription)

Three points the Level-2 port must *decide*, not copy:

1. **Match-compilation placement.** purs-backend-es compiles patterns to a `Branch` (cond-chain) **early,
   at Convert time** (the Maranget/CPMtGDT decision tree with the `pbaN` column heuristic and the
   "reference problem" *function-call* leaf — `docs/optimized-pattern-matching.md`); its NbE then folds on
   `Branch`. **purvasm ADR-0083 keeps `CCase` structured through the optimiser and decision-trees at
   codegen entry** (the GHC placement — load-bearing precisely so the NbE inliner sees a *structured*
   scrutinee for case-of-known-constructor / case-of-case). So purvasm's `build`/`evalBranches`
   equivalents must reduce over **`CCase`**, not `Branch` — a fresh design, not a port of the `Branch`
   simplifiers. (The purs-backend-es `Branch` simplifiers remain a useful catalogue of *which* reductions
   to implement.)
2. **Variable representation.** The reference uses de Bruijn **`Level`** throughout; purvasm's CESK carries
   a `name → address` indirection. The port needs the level bridge (or a faithful `Level` layer over the
   existing ANF/`Normalize`).
3. **A codegen-side per-function size budget likely remains** (§10) — the reference needs none because its
   backend (JS/V8) is linear; purvasm's is LLVM `-O2`.

Everything else — `Complexity`/`Usage`/`size` analysis, the `Sem*`/`Neut*` domain with the `SemRef`
spine-deferral carrier, `eval`/`quote`/`build`/`optimize`, the two `shouldInline*` gate sites, and the
foreign-folding table (which maps onto purvasm's existing `Primitive`/`ForeignSig` resolution) — is a
close, near-mechanical port. 0005 §6's estimate ("an analysis + a ~10-line predicate, not a large
spine-threading rewrite") matches this source.

## 10. Companion measurement: LLVM `-O2` is super-linear in single-function size

The reference inliner needs no size cap partly because its backend is linear. purvasm's native link runs
**`clang -c -O2`** on every emitted `.ll` (`boot/lib/native_link/native_link.ml:43`), and the emitted IR
is *deliberately naive* (per-step virtual registers + conservative root/reload traffic, ADR-0072 §6) —
so `-O2` is **load-bearing for performance and cannot simply be dropped**. That makes LLVM's scaling on a
single large function a real constraint. Measured 2026-07-07, Apple clang 15.0.0 (arm64), on synthetic
`.ll` reproducing purvasm's exact emission shape — one function of N steps, each step ≈ 20 lines /
4 basic blocks / 2 φ (root-read → `tailcc` extern call → safepoint check block → root block), min-of-k
wall clock:

| steps | `.ll` lines | `-O2` s | `-O0` s | O2 doubling-exponent |
| ----: | ----------: | ------: | ------: | -------------------: |
|   100 |       3 912 |    0.04 |    0.02 |                    — |
|   500 |      19 512 |    0.15 |    0.04 |                    — |
| 1 000 |      39 012 |    0.34 |    0.08 |                 1.18 |
| 2 000 |      78 012 |    0.78 |    0.18 |                 1.20 |
| 4 000 |     156 012 |    2.10 |    0.46 |                 1.43 |
| 8 000 |     312 012 |    6.31 |    1.02 |                 1.59 |
| 16 000 |    624 012 |   20.48 |       — |             **1.70** |

The O2 exponent **climbs monotonically** (1.18 → 1.70 over the range); `-O0` stays ~linear (≈1.1). So the
super-linearity is **entirely the -O2 pipeline**. `-ftime-report` names the drivers: **GVN** and
**InstCombine** in the middle-end, **MachineCSE** (≈22 % of the machine passes) and the **Greedy Register
Allocator** in the backend — all value-numbering / liveness passes, fed by the redundant
`getelementptr %ctx, 8` + `load` traffic the naive IR emits and by the huge live ranges a long single
function creates.

Crucially, this is super-linear in **single-function** size, **not module** size: the largest real module
(`output-purvm/_build/mod_544.ll`, 87 K lines across hundreds of functions) compiles at `-O2` in 0.78 s —
the same as a *single* 78 K-line function — but the single-function curve diverges above that while the
many-small-functions total stays linear. The largest single function purvasm emits today is ≈ 2 280 lines
(≈ 0.1 s); multi-second territory needs ~35–70× that.

**Consequence for the inliner design.** The reduction-aware gate (§5) is the **primary** defence — a big
dispatch is never materialised into one function. A **codegen-boundary per-lifted-function size budget**
(keep the dispatch a runtime call above K instructions / basic blocks) is the **backstop** for whatever
the gate's thresholds miss — exactly the "gate in the reducer **plus** an explicit codegen-side
function-size guard" end-state 0005 §6 predicted, now with data, and bounding the **function**, not the
module. Secondary mitigation: the ADR-0079 inline-rooting fast paths shrink the redundant `%ctx` traffic
GVN/MachineCSE chew on, lowering the constant factor. (Repro generator kept out-of-tree in the working
scratchpad; regenerate against the shipping toolchain before relying on the absolute numbers.)

## Related

- Lessons this reference answers: 0001 (genericShow blow-up), 0002 (fixed-threshold exponential),
  0003 (WPO→SMO retrospective), 0004 (portable lessons), 0005 (Layer C attempt / size-cap decision).
- Consumers: ADR-0082 (the optimiser track that builds this), ADR-0083 (match placement — §9.1).
