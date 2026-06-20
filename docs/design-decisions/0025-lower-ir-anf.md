# 0025. Lower IR: ANF with eval/apply, verified by round-trip against the oracle

- Status: Accepted
- Date: 2026-06-20

## Context

The CESK AST (ADR-0002) is the project's executable **spec / oracle**: small,
faithful, *curried*, and *un-normalised*, interpreted small-step. It is not an
optimisation or codegen substrate. The next layer down is a **lower IR** that is,
so it can carry the optimiser and eventually feed PURVASM stack bytecode
(ADR-0003).

The optimiser's payoff is concrete and already anticipated:

- **DictElim** — collapse type-class method dispatch (`accessor(instance dict)`)
  to the underlying implementation, erasing the dictionary-passing cost
  (ADR-0007).
- **GER / impurification** — lower `Effect` onto ordinary functions (ADR-0023,
  ADR-0024). Once `Effect` is on the normal optimisation playground, inlining,
  uncurrying, and tail-call elimination give *fast* and *stack-safe* `Effect`
  computation for free.
- **Pattern-match → decision tree** — the oracle matches naively (top-to-bottom
  structural walk, ADR-0011/0012/0013) as the *semantic reference*; the lower IR
  compiles `case` to a decision tree of primitive tests (tag / literal test, field
  projection, `if`) that shares tests and never backtracks — the codegen-friendly
  form, with guard fall-through (ADR-0013) preserved.
- Inlining, uncurrying, and DCE beyond the linker's reachability pass (ADR-0021).

The IR form is **ANF**, not CPS: continuations are already reified by the machine
model, so ANF keeps direct style while normalising evaluation order (atomic
arguments, let-named intermediates, syntactically explicit tail position) — the
standard substrate for both an optimiser and a stack-machine codegen.

## Decision

1. **Lower IR = ANF.** Atoms (`Var`/`Lit`/constructor & foreign references);
   every compound subexpression is `let`-named; calls take atomic arguments. Tail
   and non-tail calls are distinguished **at the instruction level**, so a later
   codegen can do tail-call elimination (the CESK oracle does not need this — its
   continuation is heap-allocated — but the native VM will). `case` is kept as an
   ANF node (atomic scrutinees) and carried through faithfully by `transl`;
   compiling it to a decision tree is a dedicated later pass, not part of this
   slice.
2. **Calling convention: eval/apply, uncurried from the start** (not push/enter).
   Known-arity functions (top-level and `let`-bound lambdas) are called by
   *saturated direct calls*; an unknown/first-class function goes through a generic
   apply that dispatches on arity — saturate, over-apply (call then apply the
   rest), or build an explicit partial application. This matches a stack VM and
   the direct dispatch DictElim produces, and is the faster, established choice.
3. **Source: `transl : Cesk.Ast → Anf`.** Reuse the whole existing front-end
   (newtype erasure, `Char = Int`, `unit = 0`, reachability DCE); normalise to ANF
   and recover arities (the uncurrying analysis).
4. **Verify by round-trip against the oracle — no new interpreter or VM.** Provide
   `rev_transl : Anf → Cesk.Ast` that re-curries eval/apply calls and multi-arg
   lambdas back to curried `App`/`Lam` and erases the tail marks (which the
   heap-continuation oracle ignores). A pass `P` on ANF is **sound** iff, for every
   fixture, `eval (rev_transl (P (transl e)))` equals `eval e` — the same value,
   and for `Effect` programs the same observable effects (captured stdout, `Ref`
   result). With `P = id` this validates `transl`/`rev_transl` themselves. The
   single oracle stays the source of truth; the bytecode VM is deferred entirely.
5. **Scope of this slice:** the ANF form, `transl`, `rev_transl`, and the
   round-trip harness over the existing fixtures. **No** optimisation passes, **no**
   bytecode, **no** separate ANF interpreter.

## Consequences

- The optimiser can be built and trusted incrementally: each later pass (DictElim,
  inlining, uncurry refinement, GER/impurification, DCE) is a function on ANF
  gated by the *same* round-trip equivalence — none of them needs a VM to be
  validated. The ADR-0023 effect-soundness contract becomes a mechanical test (the
  round-trip compares effect order, not just the return value).
- The IR carries the tail distinction and explicit arities now, so the future
  `Anf → PURVASM bytecode` codegen and VM can honour proper tail calls and
  saturated calls without re-deriving them.
- What the round-trip does **not** verify: stack-safety and performance. The oracle
  is already heap-stack-safe and `rev_transl` erases tail marks, so those are
  properties of the eventual VM, validated against the oracle when it exists. This
  slice secures *semantic* equivalence (values + effects) only — which is exactly
  what optimisation soundness needs.
- The IR is untyped (CoreFn is type-erased; the upper IR is untyped; DictElim and
  the rest are structural).

## Alternatives considered

- **Build `Anf → bytecode → VM` first, verify by running bytecode against the
  oracle.** More faithful to the eventual runtime, but it forces the whole VM to
  exist before any optimisation can be trusted. The round-trip (suggested in
  review) gets optimisation validation now at a fraction of the cost; the VM still
  comes later and is validated against the same oracle. Chosen: round-trip first.
- **A separate ANF interpreter** for verification. Redundant once `rev_transl` +
  the oracle exist, and a second evaluator is a second thing to keep faithful.
- **CoreFn → ANF** (a parallel front-end). Duplicates the lowering; `Cesk.Ast →
  ANF` reuses it and makes the round-trip a clean same-representation comparison.
- **push/enter calling convention.** Simpler at the call site but slower currying
  and a poor fit for known-arity direct calls and a stack VM; eval/apply is the
  established faster choice and matches DictElim'd dispatch.
- **A CPS lower IR.** Unnecessary — continuations are already modelled — and harder
  to read, optimise, and round-trip.
- **Curried ANF first, uncurry later.** Postpones arity/uncurrying work that
  `Effect` (ADR-0024) and the stack VM need regardless; do it once, up front.

## Appendix: `atom` / `cexpr` / `expr` — which goes where

The ANF type has three levels, and a sub-position of a compound node always uses
exactly one of them. The choice follows one rule, stated three ways.

The levels by role:

- **`atom`** — no evaluation step (`AVar`/`ALit`/`AForeign`): "already a value".
- **`cexpr`** — exactly *one* computation step. It appears only as the thing a
  `Let` binds or a `Ret` returns — the unit of a straight-line let-sequence.
- **`expr`** — a let-sequence (`Let … (Ret cexpr)`): a whole evaluation context.

The decision rule for a sub-position:

1. **Is it an operand consumed in this same step?** → `atom`. (`CApp` head/args,
   `CPrim`/`CCtor`/`CArray`/`CRecord`/`CAccessor`/`CUpdate` operands, `CIf`
   condition, `CCase` scrutinees.) Allowing a compound here would hide a
   sub-evaluation inside the step and defeat ANF's explicit ordering.
2. **Is it one operation whose result then flows on (bound or returned)?** →
   `cexpr` (only under `Let`/`Ret`).
3. **Is it its own block of control / scope?** → `expr`. (`CLam` body, `CIf`
   branches, `CCase` alternative results and guards, `LetRec` right-hand sides,
   the body of `Let`/`LetRec`.)

The unifying principle is **"can a `let` be hoisted across this boundary?"**
Normalisation makes an operand atomic by hoisting its `let`s outward into the
enclosing `expr`. But a `let` cannot cross a control/scope boundary — into an
`if` branch (it might not run), a `λ` body (different time and scope), a `case`
alternative, or a `LetRec` right-hand side (its `let`s may reference the
group). So at each such boundary normalisation *resets* to a fresh `expr` that
carries its own `let`s; everywhere hoisting is legal, the operand collapses to an
`atom`. `cexpr` is just the straight-line statement in between.

This shows up directly in the normaliser ([Transl]): `norm_atom`/`norm_atoms`
serve `atom` positions (emitting a hoisted `Let` when needed), `norm_ce`+`k`
build the `cexpr` sequence, and `anf_tail` is called at exactly the `expr`
positions — the control boundaries listed above.

The cleanest illustration is the `Let` vs `LetRec` asymmetry: `Let` binds a
`cexpr` (a non-recursive right-hand side's inner `let`s were hoisted in front of
it), whereas `LetRec` binds `expr`s (a recursive right-hand side's inner `let`s
may reference the group, so they cannot be hoisted out and must stay inside).
