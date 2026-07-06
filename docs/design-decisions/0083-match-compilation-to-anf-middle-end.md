# 0083. Match compilation to a shared ANF middle end, and a byte-identity cross-check

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

### 1. One shared match compiler — a middle-end `CCase → dtree` builder, in both compilers

Introduce one match compiler — a Maranget-style decision tree, generalising/relocating
[0031](0031-decision-tree-matching.md) — written in boot (the reference) and faithfully ported to
Level 2. It is a **backend-agnostic middle-end function** producing an explicit decision-tree
intermediate:

```
Middle_end.Match_compile.compile : atom list -> alt list -> dtree
```

**Realisation note (implementation choice, 2026-07-07).** `dtree` is a **shared middle-end
intermediate type, not a new `Anf.cexpr` constructor.** The ANF `cexpr`/`CCase` type is *unchanged*;
each backend, at its existing `CCase` site, calls the shared `compile` to get a `dtree` and then
lowers that:

- `Vm.Codegen` splits today's `Match_compile` at the tree boundary — the Maranget tree-build moves
  into `Middle_end.Match_compile`, the `pseudo`/`resolve`/back-patch **emission** stays in
  `Vm.Codegen` as `lower_dtree` → bytecode `switch`/branch ops. A faithful split preserves the
  fresh-occurrence order, so the bytecode is **byte-identical by construction**;
- `codegen_ml` drops the `case_decision`/`case_cascade`/`emit_match` hybrid and lowers the `dtree`
  uniformly (a tag `match`/`if` chain — `ocamlopt` still switches the leaf integer test);
- `codegen_llvm` drops the CPS cascade and lowers the `dtree` to an LLVM `switch` (tag/immediate) or
  equality-call branch chain (boxed), reproducing the cascade's rooting/safepoint discipline and
  boxed-literal hoisting per tree node.

This realises the accepted intent — one shared matcher; the optimiser still sees structured `CCase`
(§2); every backend lowers the *same* tree, none runs its own matcher — while containing the blast
radius: **`Anf.cexpr`, `transl`/`rev_transl`, and the four optimiser passes are untouched** (they
never see a compiled tree, since `compile` is called at codegen entry, after them). `compile` is a
pure `(scruts, alts) -> dtree` function, so the matcher is unit-testable in isolation on its tree
output. (The originally-recorded form — a new `CMatch` `cexpr` node rewritten by an `expr -> expr`
pass — was rejected during implementation for a larger blast radius and higher byte-identity risk
with no present consumer of a post-match ANF.)

**The `dtree` type must cover the full current binder vocabulary**, not just a tag switch —
`Match_compile`, `codegen_ml`, and `codegen_llvm` today collectively handle `BCtor`, `BLit`
(`LInt`/`LBool`/`LNumber`/`LString` — note there is **no `Char` literal**), `BArray`, `BRecord`, and
**guarded** alternatives, and the `dtree` is the union. Node kinds (mirroring the existing bytecode
matcher so the split stays byte-identical):

- **`Switch_ctor`** — a multi-way branch on a constructor tag, each arm extracting its fields
  (bytecode `Proj`, LLVM `pv_read_field`, OCaml `f.(i)`) before its subtree.
- **`Switch_lit`** — a single multi-way branch keyed by literal, covering **all** of
  `LInt`/`LBool`/`LNumber`/`LString` uniformly (exactly as today's bytecode `Switch_lit`). Each
  backend chooses the discriminant: bytecode emits one `Switch_lit` op; OCaml a `match`; LLVM an
  `icmp`-switch for the immediate `LInt`/`LBool` subset and a `pv_prim_eq_number`/`pv_prim_eq_string`
  **equality chain** for the boxed `LNumber`/`LString` subset (which LLVM cannot `switch` on). The
  "boxed literals must not be forced through a switch LLVM can't express" requirement is thus met at
  **lowering** time, keeping the shared node a single `Switch_lit`.
- **`Switch_len`** — an array-length branch, each arm extracting elements (`Proj_arr` /
  `pv_prim_index_array` / `a.(i)`) before its subtree.
- **record expansion** — records impose no discriminant (row-polymorphic; labels present by typing):
  extract each label (`Get_field` / `pv_record_get` / `SMap.find`) into a sub-occurrence and widen
  the column, no switch.
- **guard fall-through** — a `Guarded` leaf tests each guard in order; all-false threads to the
  **next rows' continuation** (recompiled remaining matrix), not the shared default, preserving
  guarded fall-through ([0013](0013-adt-pattern-matching.md)).
- **`Fail`** — the "no alternative matched / every guard fell through" leaf (bytecode `Fail`, LLVM
  `pv_case_fail`, OCaml `stuck`).

Var/as-pattern binders (`BVar`/`BNamed`) and nested cases fall out of the standard Maranget
construction over this node set (leaf variable binds are `name := occurrence`).

### 2. Position: late — at codegen entry, after the optimiser

`compile` runs **at each code generator's entry, after all optimiser passes**, so pattern matches
stay **structured (`CCase`) through optimisation**:

```
CESK → Transl → [ DictElim / Simplify / NbE inliner / Dbe ]  →  { Match_compile → lower } per backend
                (native path only, today)                         (required lowering at codegen entry)
```

This is the GHC placement, and it is load-bearing for the [0082](0082-native-codegen-port-to-level-2.md)
optimiser track: the **NbE general inliner** wants structured matches for `case`-of-known-constructor
and `case`-of-`case` — reductions that are natural on a `CCase` scrutinee and brittle-to-impossible on
an already-lowered tag-`switch`. (Folding match compilation into `CESK → ANF` linearisation, the
Grain placement, is rejected in Alternatives for exactly this reason.)

Two facts pinned by reconnaissance shape *where* `compile` is called. (a) Boot's **bytecode path runs
no optimiser at all** — it is `Transl.transl` → codegen (`pvm/compile.ml`, `pvm/image.ml`,
`pvm/plink.ml`); only the **native** path runs `DictElim → Simplify → Dbe` (`bin/main.ml`). (b) There
is **no `--opt`/`--no-opt` flag yet** — that split is [0082](0082-native-codegen-port-to-level-2.md)'s,
still deferred. So this record adds no flag: `compile` is simply invoked at each backend's `CCase`
site (bytecode `Vm.Codegen`, `codegen_ml`, `codegen_llvm`), which for the native path is already
after the optimiser and for the bytecode path is right after `Transl`. When
[0082](0082-native-codegen-port-to-level-2.md) later adds `--no-opt`, match compilation is unaffected
— it already sits at codegen entry, after whatever optimiser ran. The direct ANF interpreters (the
CESK oracle; boot's `Vm.eval_anf`) keep consuming `CCase` and never match-compile — they stay
independent behavioural references.

### 3. Re-baseline byte-identity; behaviour by the differential

Because boot and Level 2 change **together**, the gates stay live and become the refactor's proof:

- **Bytecode `.pmo`/`.pmi` byte-identity** is re-run (Level 2 vs boot). Both sides build the same
  `dtree` from the same (post-`Transl`) `CCase` and lower it identically, so they stay byte-identical.
  *Should* a faithful split perturb the bytes on both sides, that is a new absolute baseline and the
  golden reference is replaced — the pins are **inline string literals** (`refPmo`/`refPmi`/
  `refPmoDiaA`/`refPmiDiaA`) in the Level-2 test modules (`compiler/test/{Unit,E2E}/...`), regenerated
  by re-running boot's `purvm compile` on the checked-in fixture corefn and pasting the emitted bytes
  (no auto-regen script). In the event the split perturbed nothing and the golden was unchanged (see
  Progress) — but the re-baseline path is kept here because a split *could* have moved the bytes.
- **The differential** is the behaviour gate. Boot's e2e harness is four independent implementations
  — **CESK oracle + VM-bytecode + OCaml-native + LLVM-native** (no JS leg lives in boot; the JS
  backend is Level-2-side). It re-confirms every backend still computes the right values, in
  particular the two boot native paths whose lowering changes most: `codegen_ml` (record-binder
  cascade → uniform tree) and `codegen_llvm` (CPS cascade → decision tree). The OCaml-source path has
  **no** byte-identity partner — the differential is its only gate — so it is exercised across the
  full binder vocabulary (`BLit` Number/String, `BArray`, `BRecord`, guards) deliberately.

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
- Cost: a large refactor across boot (the shared builder + all three backends) and Level 2 (the
  builder + bytecode lowering) — but incrementally checked by the two live gates at every step. (The
  faithful split left the bytecode golden unchanged, so in the event no fixture re-baseline was
  needed; §3 kept it as a possibility only because a split *could* have perturbed the bytes.)

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

## Progress

- **2026-07-07 — Implemented (Option C), all gates green.** Shared matcher landed as a middle-end
  intermediate (§1 realisation note): boot `Middle_end.Match_compile` (`middle_end/match_compile.ml`,
  `compile : atom list -> alt list -> scrut_binds * dtree`) and its faithful Level-2 mirror
  `Purvasm.Compiler.MiddleEnd.MatchCompile`. `Anf.cexpr`/`CCase`, `transl`/`rev_transl`, and the four
  optimiser passes were untouched.
  - **Bytecode (byte-identity):** boot `vm/match_compile.ml` split at the tree boundary — build moved
    out, `lower_dtree` (pseudo/resolve/back-patch) stayed. All **292** fixture `.pvmo`/`.pvmi`
    byte-for-byte identical to pre-refactor boot; Level-2 `.pmo`/`.pmi` still byte-identical to boot
    (`refPmo`/`refPmi`/`refPmoDiaA` unit + e2e). **The faithful split perturbed nothing, so the golden
    reference is unchanged — no fixture re-baseline was needed.**
  - **Native (differential):** `codegen_ml` dropped its `friendly`/`case_decision`/`case_cascade`/
    `emit_match` hybrid; `codegen_llvm` dropped its CPS cascade + `match_binder`. Both now lower the
    shared `dtree`. The LLVM lowering roots **every** occurrence (scrutinees *and* extracted
    sub-values), reading via `get_current` — the tree shares sub-occurrences across rows, and a guard
    body may safepoint before its row's fall-through reuses them (the old per-alt cascade re-derived
    them instead). Boxed `Number`/`String` literals are hoisted+rooted once at case entry; the boxed
    `Switch_lit` subset lowers to a `pv_prim_eq_*` equality chain (LLVM cannot `switch` boxed), the
    immediate subset to an LLVM `switch`. Ctor dispatch splits **by representation first** — one
    `switch` over the immediate (nullary) arms, a separate one over the pointer (field-carrying) arms
    — preserving the old cascade's kind guard (a field arm never `pv_read_field`s an immediate; a
    `ctor_tag` hash collision between a nullary and a field ctor stays disambiguated by kind, as in
    `codegen_ml`'s real-string-tag + arity match).
  - **Gates:** boot e2e differential **218/218** (VM + OCaml-native + LLVM-native vs CESK oracle),
    covering the tricky paths — `case_guard_fallthrough`, `string_lit_binder(_nested)`,
    `number_lit_binder`, `array_binder_fallthrough/nested`, `record_binder`, `case_nested`,
    `case_aspat`. Level-2 **99/99** unit (incl. byte-identity + a new `MatchCompile` `DTree`-structure
    spec) and **3/3** e2e. `dune fmt` / `purs-tidy` clean.
  - **Follow-ups:** ADR-0082 native-codegen port stays deferred (it now lowers the shared tree, no
    per-backend matcher); `--no-opt` is 0082's to add (post-0083 it ends in the required
    `MatchCompile`). `codegen_ml` retirement remains gated on §5's checklist.
