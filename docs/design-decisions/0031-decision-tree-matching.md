# 0031. Decision-tree pattern compilation in the VM

- Status: Accepted
- Date: 2026-06-22

> **Progress (2026-06-22).** Implemented in `boot/lib/vm/`: unconditional `case`
> compiles to a Maranget decision tree (`Codegen.gen_case_simple`) of explicit
> `Switch_ctor`/`Switch_lit`/`Switch_len` + `Proj`/`Proj_arr`/`Get_field` extraction,
> occurrences bound to fresh locals, assembled via a label-backpatching pass
> (`resolve`). The host-side `Match` is removed; guarded `case` keeps the `Test`
> chain. Differential equivalence holds on every fixture, benchmark, and a new
> decision-tree fixture set (literal/nested-ctor/as-pattern/shared-prefix/default/
> array-length — e2e `vm` group, 23 cases). As predicted, `vm_instrs` *rises* on
> simple cases (matching cost now explicit, not a hidden host step). The naive
> explicit matcher (`gen_case_naive`, toggled by `use_naive_matching`) is wired into
> the bench as the comparison baseline: the tree is byte-for-byte equal on `fib`
> (no shared structure) and saves up to ~8% `vm_instrs` on case-heavy benches
> (bintree-bfs 0.92, quicksort 0.96, map-fold 0.96), all still oracle-agreeing.
> Guards-into-tree remains the fast-follow.

> **Progress (2026-06-22, fast-follow done).** The fast-follow is implemented:
> guarded alternatives are now folded into the decision tree. A guard chain
> (ADR-0013) is evaluated at the matched leaf (top to bottom); if every guard is
> false, control falls through to the rows below this one, recompiled against the
> same occurrences (`emit_rhs … ~on_fail:(fun () -> compile occs rest)`) — Maranget's
> "keep the successors reachable" — so sequencing and fall-through are exactly
> ADR-0013. With one matching path for every `case`, the slice-1 `Test`/`Drop`
> instructions and the `Test`-chain compiler are retired, and the now-unused
> host-side `Pmatch`-style matcher is removed from `Machine`. The compiler module is
> `Match_compile` (`compile ~atom ~body ~tail`), called by `Codegen.gen_case`; the
> naive baseline handles guards too (fall-through to the next alternative). New e2e
> guard cases (multi-guard chain, guard-failure fall-through inside a constructor
> branch, naive+guards) pass; full differential (fixtures + benches) and the no-opt
> baseline are unchanged.

## Context

The slice-1 VM (ADR-0030) matches `case` **host-side**: an unconditional `case`
compiles to a single opaque `Match` instruction, a guarded one to a `Test`/guard
chain, and each runs the naive structural matcher (`Pmatch`-style, try each
alternative's binders in order) *inside one OCaml step*. This was a deliberate stub:
it is correct and reuses the oracle's matcher, but it has two limits.

1. **It is not native-faithful.** A native target cannot call a host matcher;
   pattern matching must be **explicit control flow** (tag tests, field loads,
   branches). ADR-0003's phase-2 plan — recover SSA by abstract stack simulation —
   needs structured branches, and ADR-0011/0013 already anticipated that "the
   bytecode layer may compile guards to branch cascades **over the shared decision
   tree**" and that structural tests "may be shared/reordered" while guard
   evaluation stays sequenced.
2. **It hides matching cost.** A whole structural match (arbitrary nesting, N
   alternatives) is one `vm_instrs` tick, so the cost model is dishonest for
   matching and the decision-tree improvement the benchmark discussion anticipated
   is invisible. The naive matcher also **re-examines** the scrutinees once per
   alternative (O(alts) scans); a decision tree examines each sub-term once and
   shares tests across alternatives (Maranget, *Compiling Pattern Matching to Good
   Decision Trees*).

This is the first deferred **VM-stage optimisation** that the VM unblocks (ADR-0030):
make matching real bytecode, done in the optimal explicit form.

> **Honesty note (cost model).** Because slice 1's `Match` is one instruction,
> making matching explicit will *raise* `vm_instrs` on simple cases (e.g.
> `case xs of Nil/Cons` becomes a tag test + field loads instead of one host
> `Match`). That is the **honest** cost finally appearing, not a regression. The
> decision tree is judged against a **naive explicit matcher** (also compiled to
> bytecode, re-testing per alternative), not against the host-side stub; its win
> grows with shared structure, nested patterns, and alternative count.

## Decision

Compile `case` to an explicit **decision tree** of primitive bytecode tests,
replacing the host-side `Match`. (First slice: **unconditional** `case`; guarded
`case` keeps the slice-1 `Test`/guard chain, which is already correct — see Scope.)

- **Maranget-style construction.** Treat the alternatives as a pattern matrix over
  the scrutinee columns; repeatedly pick a column, switch on the head discriminant
  (constructor tag / scalar literal), and `specialize`/`default` the matrix, until a
  row is selected. Wildcards/variables impose no test (they bind); a column of all
  wildcards is skipped. The result is a tree where **each occurrence is inspected at
  most once** on any path.
- **Occurrences via locals, not the operand stack.** An *occurrence* is a path into
  the scrutinees (e.g. "field 1 of scrutinee 0"). To preserve ADR-0003's invariants
  (operand-stack height statically known, **empty at branch boundaries**), a test
  loads its occurrence, and destructuring **binds sub-occurrences to fresh locals**,
  rather than juggling partial values across branches on the operand stack.
- **New/changed instructions (decision level, not final spelling):** switch on a
  data value's tag to one of several targets (with a default), test a scalar literal
  and branch, bind/extract a constructor field / array element / record field at an
  occurrence into a local, and bind an occurrence to a name (var / as-pattern). The
  array-length and record-label-presence checks of ADR-0012 become explicit tests
  (a different array length is a value-level non-match → default edge; a missing
  record label is type-impossible → stuck, as today).
- **Semantics preserved exactly (ADR-0011/0012/0013), verified differentially.**
  First-match order; value-level non-match (fall to the default/next) vs
  type-impossible shape (stuck); for guards (when folded in later) the sequencing
  ADR-0013 fixes — top-to-bottom, left-to-right, fall-through re-tries the residual
  rows against the already-bound scrutinees — is honoured; only *structural* tests
  are shared/reordered. Validated by ADR-0030's differential check (VM result ==
  `Cesk.Machine.eval`) on every pure fixture and benchmark, with the oracle's
  `Pmatch` as the reference semantics.
- **Measurement.** Report `vm_instrs` as before; additionally compare the decision
  tree against a naive explicit matcher (same bytecode primitives, per-alternative
  re-testing) on the suite, so the tree's quality is measured in VM-instruction
  terms — the comparison the host-side stub could not express.

## Scope

- **In:** unconditional `case` (all binder forms of ADR-0011/0012: ctor, literal,
  array, record, as-pattern, nested) compiled to a decision tree. This covers
  **100% of the benchmarks** (fib/quicksort/n-queens/bintree/map-fold are all
  unconditional), so the measurement target is fully exercised.
- **Deferred to a fast-follow:** folding **guarded** alternatives into the tree
  (Maranget's guard handling keeps a guarded row's successors reachable on guard
  failure). Until then guarded `case` keeps the slice-1 `Test`/guard chain — correct,
  just not yet decision-tree-shared. Effects/native are unrelated (ADR-0030 slice 2).

## Consequences

- Matching becomes **native-ready** explicit control flow (the ADR-0003 SSA path can
  consume it), and the VM's cost model for matching becomes honest.
- The slice-1 host-side `Match` is retired for unconditional `case`; `Pmatch`
  remains the **oracle's** matcher and the differential reference (unchanged).
- `vm_instrs` rises on trivial cases (the now-visible matching cost) and the
  decision tree minimises that cost relative to naive explicit matching; net effect
  per bench is a **measured** question, reported alongside the existing columns.
- A new regression surface: the differential check must keep passing on every binder
  form and on non-exhaustive/stuck paths; a decision-tree-specific fixture set
  (shared prefixes, nested ctors, array/record/as-patterns, default edges) is added.

## Alternatives considered

- **Keep the host-side matcher.** Simplest, but never reaches native (a host call
  cannot be lowered to SSA) and keeps the cost model dishonest; the whole point of
  having the VM is to make this real and measurable.
- **Naive explicit matcher (per-alternative re-testing in bytecode).** Faithful and
  simpler than a decision tree, but re-examines occurrences O(alts) times and shares
  nothing; it is the *baseline* this ADR's tree is measured against, not the choice.
- **Backtracking automaton / matching as a DFA with backtracking.** Smaller code
  than a decision tree but reintroduces re-examination and complicates the SSA
  recovery; decision trees are the standard good-code choice and compose with the
  guard sequencing ADR-0013 requires.
- **Do guards in the first slice too.** More complete, but guard fall-through
  interacts with the residual matrix and is the riskier half; scoping it as a
  fast-follow keeps the first slice reviewable and already covers all benchmarks.
