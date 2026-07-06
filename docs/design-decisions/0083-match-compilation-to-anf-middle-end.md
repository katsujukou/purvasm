# 0083. Match compilation to a shared ANF middle-end pass, and a byte-identity re-baseline

- Status: Accepted
- Date: 2026-07-07

## Context

Match/decision-tree compilation is currently done **per backend, at backend-lowering time**, and
all **three** backends that consume the ANF's high-level `CCase` do it differently:

- the **bytecode** path compiles a `case` to a decision tree in the backend
  (`Vm.Codegen.gen_case` → `Match_compile`, [0031](0031-decision-tree-matching.md); Level 2 mirrors
  it in `Bytecode.Lower.Match`);
- the **OCaml-source** path (`codegen_ml`, [0036](0036-anf-to-ocaml-value-representation.md)) is a **hybrid**:
  `case_decision` emits a decision tree (leaning on `ocamlopt`'s own switch) *iff* the alt has no
  record binder, and otherwise falls back to `emit_match`, a per-binder cascade — decision-tree
  lowering was left a later perf slice there too;
- the **native (LLVM)** path compiles `CCase` to a **CPS cascade** in `codegen_llvm`
  ([0072](0072-anf-to-llvm-lowering.md) §5) — **no decision tree at all** (a re-test-per-alt chain
  that `-O2` cannot even fold into a `switch`, since `pv_read_raw` calls sit between the compares;
  the decision-tree matcher was an explicitly-deferred perf slice, [0072](0072-anf-to-llvm-lowering.md)
  Alternatives).

Two costs follow. **The same language construct is compiled three times, differently** — a
per-backend matcher to maintain in each, and (worse) the OCaml-source and native paths silently
forgo (in whole or in part) the decision tree the bytecode path has. And
[0082](0082-native-codegen-port-to-level-2.md)'s native-codegen port would have to transcribe the
LLVM cascade faithfully (for `.ll` byte-identity), banking the missing-decision-tree wart into a
second codebase.

Match compilation is **backend-agnostic language lowering** — it belongs in the middle end, over
ANF, shared by every backend (the `matchcomp` placement Grain and GHC-family compilers use). Moving
it there dissolves both costs: one Maranget decision tree, lowered uniformly (bytecode `switch` /
LLVM jump table), and the native port then just lowers a decision tree — no matcher to write.

**Why now, and why touch frozen boot.** The refactor is only safely cross-checkable *while boot is
alive*: change match compilation in **both** boot and Level 2 together, and the standing bytecode
**byte-identity** (528/528, [lv2-native-build]) confirms the two still agree — the refactor
verifies itself. Once boot retires (post-port), a Level-2-only change of this shape would have no
byte-identity partner (the [0082](0082-native-codegen-port-to-level-2.md) Option-A/D tension). So the
cleanest reference exists *now*. This does breach [boot-freeze], deliberately: the freeze assumed
boot was a *complete* reference, but boot's native path not decision-tree-compiling was an
unexpected, not-reference-worthy state — a one-time foundation fix before re-freezing is the right
call, not codegen-feature creep.

## Decision

### 1. A shared ANF→ANF match-compilation pass, in both compilers

Introduce one match compiler — a Maranget-style decision tree, generalising/relocating
[0031](0031-decision-tree-matching.md) — as an **ANF→ANF pass**, written in boot (the reference)
and faithfully ported to Level 2. ANF gains a **decision-tree construct**; the pass rewrites `CCase`
into it, and **all three backends lower the decision tree, not `CCase`**:

- `Vm.Codegen` drops its `Match_compile` call and emits the bytecode `switch`/branch ops from the
  tree;
- `codegen_ml` drops the `case_decision`/`emit_match` hybrid and emits from the tree uniformly
  (a tag `match`/`if` chain — `ocamlopt` still switches the leaf integer test);
- `codegen_llvm` drops the CPS cascade and emits an LLVM `switch` (tag/immediate) or equality-test
  branch chain (boxed).

`CCase` becomes a pass-*input* construct only — **no backend compiles it**.

**The decision-tree IR must cover the full current binder vocabulary**, not just a tag switch —
`Match_compile`, `codegen_ml`, and `codegen_llvm` today collectively handle `BCtor`, `BLit`
(Int/Char/**Number/String**), `BArray`, `BRecord`, and **guarded** alternatives, and the new IR is
the union. Pin the node kinds:

- **`SwitchTag` / `SwitchImmediate`** — a multi-way branch on a constructor tag or an Int/Char
  immediate at a scrutinee position (→ bytecode `switch`, LLVM `switch`, OCaml integer `match`).
- **`TestLit` (equality-test tree, *not* a switch)** — for **boxed** `Number`/`String` literals,
  which LLVM cannot `switch` on; lowered to an equality-call (`pv_eq_*`) branch chain. This node is
  explicitly a *tree of eq-tests*, distinct from `SwitchImmediate`, so the boxed case is not forced
  through a switch it cannot express.
- **`TestArrayLength`** — an array length probe with an element-**bind** continuation (`BArray`).
- **`GetField` / `GetElem` bindings** — record-field / array-element / ctor-argument **get-or-fail**
  projections that bind a name for the subtree (the `BRecord`/`BArray`/`BCtor` field binders).
- **`Guard` with a failure continuation** — a boolean guard whose *false* edge threads to the **next
  alternative's** continuation, not to the shared default (preserving guarded fall-through
  semantics).
- **a `Fail`/default edge** — the "no alternative matched" continuation that every non-exhaustive
  switch and every guard-failure ultimately flows to.

The pass is the union of these; the whole-binder (`b @ _` as-pattern) and nested cases fall out of
the standard Maranget construction over this node set.

### 2. Position: late — after the optimiser, before codegen

The pass runs **after the optimiser and immediately before codegen**, so pattern matches stay
**structured (`CCase`) through optimisation**:

```
CESK → Normalize → DictElim → [ Simplify / NbE inliner / Dbe ]  →  MatchCompile → codegen
                              (--opt only)                          (required lowering, always last)
```

This is the GHC placement, and it is load-bearing for the [0082](0082-native-codegen-port-to-level-2.md)
optimiser track: the **NbE general inliner** wants structured matches for `case`-of-known-constructor
and `case`-of-`case` — reductions that are natural on a `CCase` scrutinee and brittle-to-impossible on
an already-lowered tag-`switch`. (Folding match compilation into `CESK → ANF` linearisation, the
Grain placement, is rejected in Alternatives for exactly this reason.) `MatchCompile` is **required
lowering**, so it runs in `--no-opt` too — `--no-opt` = `Normalize + DictElim + MatchCompile`; `--opt`
inserts the optimiser before it.

### 3. Re-baseline byte-identity; behaviour by the differential

Because boot and Level 2 change **together**, the gates stay live and become the refactor's proof:

- **Bytecode `.pmo`/`.pmi` byte-identity** is re-run (Level 2 vs boot). Both sides now decision-tree
  in the ANF pass and lower identically, so they stay byte-identical — at a **new absolute baseline**
  (the bytes change; the old golden reference is replaced). Checked-in fixtures that pin bytecode
  bytes are regenerated (the compiler `refPmo`/`refPmi` tests, any `.pmo` e2e fixtures).
- **The four-way differential** (VM / OCaml-native / LLVM-native / JS, oracle-checked) is the
  behaviour gate — it re-confirms every backend still computes the right values, in particular the
  two boot native paths whose lowering changes most: `codegen_ml` (record-binder cascade → uniform
  tree) and `codegen_llvm` (CPS cascade → decision tree). The OCaml-source path has **no**
  byte-identity partner — the differential is its only gate — so it is exercised across the full
  binder vocabulary (`BLit` Number/String, `BArray`, `BRecord`, guards) deliberately.

### 4. The native-codegen port is deferred to after this

[0082](0082-native-codegen-port-to-level-2.md) is deferred until this record lands, and its native
backend then **lowers the decision-tree ANF** — no per-backend matcher, and none of its Option-A/D
tension (a `.ll`-byte-identity gate against a boot that also decision-trees). The `.ll`-byte-identity
strategy of [0082](0082-native-codegen-port-to-level-2.md) is otherwise unchanged.

### 5. The OCaml-source backend: kept through this refactor, retired at native self-host

`codegen_ml` is no longer the primary native path (self-host runs on LLVM), but through the
boot-reference period it still earns its keep in two ways, and both are strongest *during* this
refactor:

1. **An independent leg of the four-way differential.** Its emit + compiled-runtime path
   (OCaml source → `ocamlopt`-produced native) is independent of `codegen_llvm`'s (LLVM IR + the
   owned Rust runtime). A bug in the shared match compiler surfaces identically on every leg — but a
   bug specific to the **LLVM emit or the owned runtime** shows up as `codegen_ml` *disagreeing*, so
   it isolates "LLVM/runtime-specific fault" from "ANF/optimiser fault."
2. **Boot-period insurance.** While Level 2 has no native backend, boot carrying *two* independent
   native implementations is what makes that isolation possible at all.

So this record **does** refactor `codegen_ml` onto the shared decision tree along with the other two
backends (§1) — a deliberate, one-time investment to keep that last insurance in a *clean* form
(one matcher, three emitters) rather than freezing a diverging legacy matcher through the riskiest
lowering change.

`codegen_ml` is **retired — deprecated, then deleted — only once the native self-host path fully
subsumes it**, i.e. when all of:

- Level-2 LLVM backend is **`.ll` byte-identical to boot LLVM on `--no-opt`**
  ([0082](0082-native-codegen-port-to-level-2.md) §2 backend gate);
- Level-2 **`--opt` ≡ `--no-opt` ≡ oracle** is stable ([0082](0082-native-codegen-port-to-level-2.md) §2 optimiser gate);
- **native self-host passes** (Level 2 compiles itself on the LLVM/owned-runtime path);
- the **VM / LLVM / JS differential + `runtime`/`crates` tests** have, for a settling period,
  demonstrably caught the classes of fault `codegen_ml` was the insurance against.

At that point `codegen_ml` (the `ocaml_backend` library, the CLI `--backend ocaml` enum arm, and its
e2e differential leg) is **deprecated**, then **deleted at the next release boundary or at wall-3
completion**. That deletion is out of scope for this record — it is a follow-up, gated on the
checklist above.

## Consequences

- One match compiler for the whole project (all three backends, both compilers): the decision tree
  that drives a bytecode `switch`, an OCaml `match`, and an LLVM jump table alike, maintained once.
- The native paths *gain* the decision tree they lacked — a correctness-neutral, perf-positive change
  (`switch`/jump-table vs a re-testing or record-binder cascade), verified by the differential.
- The refactor is self-verifying: byte-identity (boot ↔ Level 2, both changed together) plus the
  differential — the safest possible time to move a load-bearing lowering, and the reason to do it
  before boot retires rather than after.
- [0031](0031-decision-tree-matching.md)'s decision tree is **relocated** from bytecode-lowering to
  the shared ANF pass (and generalised to serve native too); its algorithm is unchanged in spirit.
- A one-time, deliberate exception to [boot-freeze], scoped to this foundation fix; boot re-freezes
  after, now a *complete* reference (all three boot backends decision-tree).
- Cost: a large refactor across boot (ANF type, the new pass, all three backends) and Level 2 (the
  pass, bytecode lowering), plus a fixture re-baseline — but incrementally checked by the two live
  gates.

## Alternatives considered

- **Fold match compilation into `CESK → ANF` linearisation** (the Grain placement — no high-level
  `case` ever reaches ANF). Simpler and gives a cleaner ANF type, but the optimiser would then only
  ever see decision trees — foreclosing `case`-of-known-constructor / `case`-of-`case` for the NbE
  inliner ([0082](0082-native-codegen-port-to-level-2.md)). Rejected: keep matches structured through
  optimisation, compile late (§2). (The round-trip `CESK ↔ ANF` verification that once motivated an
  ANF→ANF pass is obsolete; the optimiser-ordering reason is the current one and it is decisive.)
- **Keep per-backend matchers; just add a decision tree to the native paths** (leave bytecode's in
  `Vm.Codegen`, complete `codegen_ml`'s hybrid, add one to `codegen_llvm`). Fixes the native warts
  but keeps three matchers and the triple compilation — the structural problem this record exists to
  end.
- **Do it Level-2-only, diverging from a frozen boot** ([0082](0082-native-codegen-port-to-level-2.md)
  Option A/D). Loses the byte-identity partner for the refactor and forces a partial or dropped gate;
  the whole value of *now* is that boot can move with us and cross-check.
- **Defer to after boot retires.** Then the refactor has no byte-identity cross-check at all (boot is
  gone) — exactly the safety this record buys by acting while the reference is alive. Rejected.
