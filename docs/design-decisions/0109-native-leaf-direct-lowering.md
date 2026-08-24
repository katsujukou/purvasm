# 0109. Native-leaf direct lowering: hoist the leaf closure, call the `pvf_` entry

- Status: ~~Proposed~~ **Accepted** _(2026-08-16: explicit maintainer Accept after 2 review rounds)_
  _(revised 2026-08-16 after review round 1 — four P1s and three P2s closed:
  the classifier is now one closed-result classifier (§1), the hoisted cell's linkage / derivation /
  init tier is pinned (§2.2), the direct call is a classified-seam renderer rather than a raw call
  (§4), and the allocation accounting is a three-way identity that subtracts the hoisted inits
  (§5.1). **Revised again 2026-08-16 after review round 2** — three P1s and two P2s closed: the
  symbol's single derivation is now a TYPE (an opaque `ForeignRef` minted by one smart constructor
  and carried in `Ctx.foreigns`, §1.1), the plan/emitter split is corrected (the activation plan
  cannot observe a `CallTarget`, so `CApp` stays on one conservative call-family row and the
  discrimination lives at the seam, §4), and the knob-off leg gets its OWN closed emission decision
  and event classes so no slot changes meaning between legs (§1.2))_
- Date: 2026-08-15
- Deciders: maintainer
- Technical story: ADR-0108 step 4's attribution — 57.8 % of the self-host build's 752.5 M generic
  dispatches are foreign-callee dispatches, 100.00 % of them at an arity the emitter already holds
  and that matches the call, concentrated in 24 symbols of which four carry 91.2 %

## Context

ADR-0108 measured, it did not license. What it established, on one named workload (the compiler
built `--opt`, compiling `Purvasm.CLI.Native` in `--no-opt`):

- `callee-foreign` is **434,445,743 dispatches — 57.8 % of executions at 3.02× its share of the
  code**; 425.2 M in the `apply` form, 9.3 M in the `tail` form;
- **100.00 %** are `known-match`: `Ctx.foreignArity` holds the callee's physical closure arity and
  that arity equals the call's argument count, at every single one;
- the population is **28 keys / 24 symbols**, four of which (`Purvasm.String.byteAt` 45.6 %,
  `unsafeSetByte` 35.3 %, `compareBytes` 6.0 %, `appendBulk` 4.3 %) are 91.2 %. Every top symbol is
  an ADR-0103 string-substrate leaf;
- `callee-literal` is 0, on both axes, measured rather than assumed.

So the question this ADR answers is not "can foreign calls be direct-lowered" — the measurement
already says yes for 100 % of the population. It is the four questions the step-4 close-out named:
**how the leaf ABI connects to the ordinary calling convention (§2), how the TAIL form is handled
(§3), whether dispatch reduction converts to real time (§5), and where the fallback stays (§6).**

### What the current lowering actually emits, per execution

The count above is a count of `pv_apply`/`pv_tailcall` dispatches, and reading it as "the cost is a
dispatch" understates the site. `Emit.atom` materialises an `AtomForeign` callee like any other
operand, so each of those 434 M dispatches is preceded, at the same site, by:

```llvm
  %a = ptrtoint ptr @pvf_Purvasm_2eString_2ebyteAt to i64
  %c = call i64 @pv_make_closure(ptr %ctx, i64 %a, i32 2, i64 1)   ; a fresh no-capture closure
  ; … argv alloca + stores …
  %r = call i64 @pv_apply(ptr %ctx, i64 %c, ptr %argv, i64 2)
```

`pv_make_closure` allocates a `Kind::Closure` object (3 payload words plus header) on the guest
heap, and `new_closure_raw` opens a shadow-stack frame, roots the env, allocates, and pops — per
call. It is also `sp = true` in the ADR-0105 seam, so the site's other operands are rooted across
it. `pv_apply` then re-validates that freshly built closure (`checked_ptr`), reads its header, reads
its arity, compares it to `nargs`, bumps two counters, transmutes the code word and calls it, then
checks the pending-tail slot.

The closure so built carries no information: same code address, same arity, and an env that is
always the unit immediate. No `pvf_` leaf in the runtime reads its closure argument at all (every
`clo`-reading function in `runtime/src/leaf.rs` is an internal *thunk* the leaf itself allocated,
dispatched through `pv_apply` with a real capturing closure).

Three costs therefore hide behind one count, and they are separable:

| cost | per execution | removed by |
| --- | --- | --- |
| a guest-heap `Closure` allocation (+ frame/root/pop, + an `sp = true` site that roots the other operands) | ≥ 434 M | slice A |
| the `pv_apply` boundary: validate, header read, arity read+compare, counters, transmute, pending-tail check | 425.2 M | slice B |
| the `pv_tailcall` stash + trampoline bounce | 9.3 M | slice C |

The allocation figure is **derived, not measured**: it is one materialisation per counted dispatch
(the callee atom is materialised at each site, at each execution), plus an unmeasured number in
value positions. §5.1 makes measuring it a precondition rather than leaving it an inference.

What stays, and is not claimed to go away: the argument buffer (`alloca` + stores), the C boundary
itself, the leaf's `guard` panic shim and its `args_slice`. This ADR replaces a generic dispatch
with a **cheaper direct call**, not with no call.

## Decision

### 1. Three slices, and ONE classifier with a closed result

- **Slice A — hoist the leaf closure.** An `AtomForeign k` materialisation stops allocating: the
  no-capture closure for `k` becomes a root-handle global built once at init, and `atom` reads it. This changes *every* foreign reference (call position and value position alike), removes
  the allocation and the safepoint, and keeps `pv_apply` exactly where it is.
- **Slice B — direct-call the saturated `apply` form.** A `CApp` whose callee classifies as a
  saturated native leaf emits a direct call to `@pvf_<mangle k>` instead of `pv_apply`, passing the
  slice-A closure word.
- **Slice C — the tail form.** The same direct call, then the frame pop and `ret` (§3).

Slice A stands alone and is worth landing alone: it is the allocation lever, it improves the
*fallback* path too, and it is what makes B's ABI question (§2) have a free answer. B and C depend on
A only for the closure operand; they do not depend on each other.

#### 1.1 One leaf reference, one value: the opaque `ForeignRef` (P1, review round 2)

Every party that names a leaf must name it with the SAME derived spelling — and the reference sites
are not all in the classifier: `AtomForeign` in a **value position** never goes through
`directTarget`, and `Ctx.foreigns` is a bare `Set String` today, so `Program.foreignDecls` re-runs
`mangleForeign` on its own. "Derived once inside `directTarget`" (round 1's wording) is therefore not
true and not enforceable; three spellings — the `declare`, the `$fclo` cell, the value-position read
— could drift apart. The fix is a type, not a convention:

```text
-- opaque: the constructor is NOT exported; the accessors are.
newtype ForeignRef  -- { key, fsym, cell, arity }

foreignRef :: String -> Codegen ForeignRef
refKey :: ForeignRef -> String        -- the qualified foreign key
refSym :: ForeignRef -> String        -- `@pvf_…`  (the one `mangleForeign` application)
refCell :: ForeignRef -> String       -- `@pvf_…$fclo` (§2.2)
refArity :: ForeignRef -> Int         -- the PHYSICAL closure arity (ADR-0090 `leafClosureArity`)
```

`foreignRef` is the ONLY way to obtain one, and obtaining one IS registering it:

- it reads `Ctx.foreignArity` and **crashes on a missing entry** — the ADR-0090 wiring-bug crash
  moves here from `Emit.atom`, so there is one such crash site instead of two;
- it derives `fsym`/`cell` (the single `mangleForeign` application in the backend);
- it inserts into `Ctx.foreigns`, which becomes **`Map String ForeignRef`** — the registration the
  `declare` set already depended on, now carrying the derived value instead of re-deriving it.

Consumers then take the value, never the key: `Emit.atom` (value position) reads `refCell`;
`directTarget` returns it inside `ForeignTarget`; the `foreignDirect` renderer emits `refSym`;
`Program.foreignDecls` maps `refSym` over `Map.values`; the fclo init (§2.2c) emits `refSym`/
`refArity`/`refCell`; the census reads the event payload, which carries the same `ForeignRef`. Pinned
as a unit test: `mangleForeign` has exactly one call site in the backend, and it is inside
`foreignRef`.

#### 1.2 One classifier for ELIGIBILITY, one closed decision for EMISSION

Round 1 replaced `directTarget`'s `Either` with a closed result so the saturated-leaf case is a
terminal outcome of the one classifier rather than a re-inspection at the `CApp` arm (the drift
ADR-0108 §1 forbids: "the census consumes the EMITTER's classification, it does not re-run the
classifier"). Round 2 splits that result from what is EMITTED, because the two are not the same
question once a measurement knob exists (P1):

```text
-- what the classifier knows: ELIGIBILITY
data CallTarget
  = GuestTarget FnInfo        -- a direct `tailcc` guest entry (today's `Right`)
  | ForeignTarget ForeignRef  -- a native leaf at its known, MATCHING arity
  | GenericTarget MissReason  -- the generic dispatch, with today's reason

directTarget :: Env -> Atom -> Int -> Codegen CallTarget

-- what the lowering does: a closed function of (target × form × feature knob)
data EmissionDecision
  = EmitGuestDirect FnInfo Form
  | EmitForeignDirect ForeignRef Form   -- eligible AND the slice is enabled
  | EmitForeignDeferred ForeignRef Form -- eligible, slice DISABLED (the A/B's off leg only)
  | EmitGeneric MissReason Form

data Form = FApply | FTail
```

The `AtomForeign k` leaf of the existing decision tree becomes: `ref <- foreignRef k`; if
`refArity ref == nargs` return `ForeignTarget ref`, else `GenericTarget MissCalleeForeign`. No new
control flow, no priority list, no second table of arities.

**Why the fourth constructor exists.** The A/B's off leg lowers an ELIGIBLE call generically, and
round 1 left unsaid which event that records. Recording `GenericApply MissCalleeForeign` would make
the residue slot mean "arity mismatch" in one leg and "arity mismatch + everything the knob turned
off" in the other — the same slot changing meaning between the two legs being compared, which is the
one thing an A/B may not do. `EmitForeignDeferred` gives the off leg its own class, and with it a
two-way gate that is stronger than either leg alone:

```text
knob OFF  ⇒  foreign-direct == 0     ∧  foreign-deferred == the eligible population
knob ON   ⇒  foreign-deferred == 0   ∧  foreign-direct   == the eligible population
both legs ⇒  the CallTarget stream is IDENTICAL (only the decision differs)
```

The last line is the property the measurement needs — the two legs differ in the recipe, not in the
classification — and it is asserted the way ADR-0108 §3 asserts its instrumentation is inert: by
comparing the recorded target stream, not by argument.

The knob is a MEASUREMENT vehicle with the lifetime of the measurement (as `PURVASM_PROFILE_APPLY`
is). When it is retired, `EmitForeignDeferred` and its two event classes are retired with it, and the
census gate that pinned them to zero becomes their absence check.

`MissCalleeForeign` therefore keeps a **narrowed and stable** meaning in BOTH legs: a foreign callee
the classifier declined, which after this ADR can only be an arity disagreement (an unknown arity
crashes in `foreignRef`). That is what makes it the residue counter of §6.

### 2. Q1 — the ABI connection

#### 2.1 The conventions do not match, and this ADR does not make them match

| | guest `CodeFn` | native leaf |
| --- | --- | --- |
| symbol | `@<mangle key>$d` | `@pvf_<mangle key>` |
| convention | `tailcc` | `ccc` (`extern "C"`) |
| signature | `(ptr %ctx, i64 %env, i64 %a0, …)` — args flattened into registers | `(ptr %ctx, i64 %clo, ptr %args, i64 %nargs)` — args behind a buffer |
| `musttail`-able from a guest body | yes | no (neither convention nor signature matches) |

The direct call is a call to the **existing** `AbiCodeFn` entry, using the `declare i64
@pvf_…(ptr, i64, ptr, i64)` the emitter already writes for every referenced key
(`Program.foreignDecls`):

```llvm
  ; slice A: read the object's hoisted leaf closure (a root-handle global, like any global read)
  %h = load i64, ptr @pvf_Purvasm_2eString_2ebyteAt$fclo
  %clo = call i64 @pv_get(ptr %ctx, i64 %h)          ; `--debug`; RELEASE is the ADR-0079 inline load
  ; … argv alloca + stores (unchanged) …
  %r = call i64 @pvf_Purvasm_2eString_2ebyteAt(ptr %ctx, i64 %clo, ptr %argv, i64 2)
```

**The published ABI (`runtime/include/purvasm.h`, ADR-0073 §3 / ADR-0091) is not weakened.** Its
`closure` parameter keeps meaning what it says, because slice A hands over a real closure — **an
ABI-equivalent one**, not the identical object the old path built (that one was freshly allocated at
every call, so "the same object `pv_apply` would have handed over" was never a thing that existed;
P2, review round 1). A user leaf that reads `pv_closure_env(clo)`, or that re-enters itself through
`pv_apply(clo, …)`, behaves identically.

One clause is *added* rather than changed:

> At a `pvf_` entry, `closure` is a no-capture closure over that leaf's own code, of that leaf's
> closure arity, whose env is the unit sentinel.

Deliberately **no identity or stability guarantee** is added (P2, review round 1): different objects
hold different copies, and a moving GC relocates them, so promising stable identity would be both
false and a contract we do not need. Closure identity at a leaf entry is not observable through any
`pv_*` the header exposes, and this ADR keeps it that way.

Rejected alternative, recorded because it is the obvious cheaper one: **pass the unit immediate as
`clo`**. It costs no global and no load. It is rejected because it converts a documented capability
of a published C header into a trap that fires inside third-party code (`pv_closure_env` on an
immediate aborts) — to save a rooted-global read on a path that is losing a heap allocation. Revisit
only behind an ABI version bump, never silently.

#### 2.2 The hoisted cell: linkage, derivation, and the init tier (P1, review round 1)

The current emission has **no per-object init**: there are per-gdef `@<key>$init` functions and one
`pv_init_all` in the entry object that calls the *reachable* ones in spine order. And the same
foreign key is referenced by many objects, so a cell defined under external linkage in each of them
would be a duplicate symbol. Four things are therefore pinned.

> **AMENDED 2026-08-16, during implementation — (a), (c) and (d) below are SUPERSEDED by
> §2.2-amended.** The per-object design links a program the previous tree links: a cell's
> initialiser is the only LIVE reference to `@pvf_<key>`, so initialising every key an object
> *mentions* turns "referenced anywhere" into "must have a provider". The self-host fixture closure
> promptly failed to link on `Control.Extend.arrayExtend` — a leaf mentioned by a dead instance
> dictionary, provided by nobody, and until now removed with its code by `-Wl,-dead_strip`. The
> original text is kept below because the reasoning it rejects (a duplicate symbol per object) is
> still what rules out the naive alternative; what changed is the OWNER.

**(a) The cell is a root-handle global with INTERNAL linkage, one per (object × key).**

```llvm
@pvf_<mangle key>$fclo = internal global i64 0
```

Internal linkage is what makes the per-object copy legal without an object-qualified name; the
symbol never reaches the linker. It holds a **permanent root HANDLE**, not a value — the same
representation as every `@<key>$root` gdef global (which is why a moving GC is not a problem), and it
is planted by the same permanent-root tier that `Root`'s init wrappers use for a `Gcaf` (ADR-0105
§2 / ADR-0106 §3). Reads go through the ordinary rooted-global path, so the reload cache and the
ADR-0079 inline fast path apply unchanged. Cost is therefore a rooted-global read (two loads under
the inline ABI), not "one L1 load" as the first draft said.

**(b) The reference set has ONE derivation: `Codegen.foreigns`, now `Map String ForeignRef`
(§1.1).** That map already drives `Program.foreignDecls`, every entry is minted by `foreignRef`, it
is accumulated during emission, and the module skeleton is rendered *after* the emission run
(`renderChunks ctx.globals`), so the cells, the init body and the `declare`s are all emitted from
the same values at the same point — none of them re-deriving a symbol. Pinned as a per-object gate:

```text
#(@…$fclo globals) == #(declare @pvf_…) == #(stores in this object's fclo init) == |foreigns|
```

Nothing may derive a second set — in particular the census reads the emitted events, not a
re-walk of the ANF.

**(c) Each object's cells are initialised exactly once, before every gdef init.** Each object emits,
unconditionally (empty when it references no leaf), an externally-visible

```llvm
define void @pv_fclo$<mangle objectName>$init(ptr %ctx) { … }
```

and the entry object's `pv_init_all` calls **all** of them, in the driver's deterministic object
order, **before** the reachable `$init` calls it already emits. Emitting it unconditionally is what
removes the cross-phase dependency the review named: the entry needs only the object LIST (which
`entryProgram`'s `input.modules` already carries), never each object's `foreigns` set — which is not
knowable until that object's emission has finished. The symbol is produced by one shared function
(`foreignCloInitSym`) consumed by both the defining object and the entry, so the two spellings cannot
drift. The entry object initialises its own cells the same way, through its own such function called
first.

The "all before all" ordering is not conservatism: object B's gdef init may call a function defined
in object A, and that function reads A's cells. A per-object ordering would leave that read on an
uninitialised cell. `pv_init_all` is called exactly once, from `@main` (`RtInitAll`), which is what
makes "exactly once" hold without a guard word.

The init body is a **fixed shape owned by `Root`**, with no body callback — the ADR-0105 §2 / §0106
capability rule that `emitGfunInit` already follows ("the frameless init is a fixed shape owned by
Root — no body callback exists to misuse"), and the reason the phase order cannot be got wrong here
(P2, review round 2):

```text
emitForeignCloInit :: ObjectName -> Array ForeignRef -> Codegen Unit
```

`Root` owns the sequence per ref — `RtMakeClosure` (a safepoint) **then** the permanent-root plant
and the handle store into `refCell` — so a caller cannot plant a permanent handle inside a transient
frame, or store before the allocation that a GC in a later ref's `RtMakeClosure` would relocate.

**(d) Two layers catch a wiring mistake, and they catch different mistakes** (P2, review round 2):

- a **one-sided** inconsistency — the entry calls an init no object defines, or an object defines
  one nothing calls under a name the entry spells differently — is a **link error**, which is what
  makes `foreignCloInitSym` being shared load-bearing rather than stylistic;
- a **two-sided omission** — an object emitted with neither a definition nor a call — links fine and
  leaves that object's cells at the `0` sentinel. That one is caught only by the **census gate**:
  the number of fclo inits declared in the entry equals the number of emitted objects (module
  objects + the entry's own), and each object's cell count matches (b).

Rejected alternative: **one external-linkage cell per KEY, owned by the object of the key's defining
module** (the `synthForeignGdefs` placement rule). Fewer closures (28 program-wide instead of one per
object×key), and it would reuse the `$root`/`external global` machinery verbatim — but it makes the
existence of that owner object a load-bearing invariant, needs foreign keys threaded into `fvExpr`
reachability (shared middle-end machinery the optimiser also reads) so `reachableGdefs` does not
prune the owner's init, and it introduces a cross-object init-order dependency instead of removing
one. The per-object copies cost a bounded, one-off allocation each and are self-contained in the
backend. Revisit if the object×key count ever stops being small.

#### 2.2-amended (2026-08-16): the ENTRY object owns the cells, and initialises the REACHABLE leaves

What the failure above establishes is that the cell's owner is not a linkage question but a
**reachability** question — and exactly one object knows reachability. So:

- **One cell per KEY, program-wide, DEFINED in the entry object** (`@pvf_<mangle key>$fclo = global
  i64 0`) and declared `external global i64` by every module object that reads one. Still a
  permanent root HANDLE, planted by the same tier (a); reads are unchanged. The duplicate-symbol
  problem (a) solves by internal linkage is solved here by there being one definition site, and the
  "does the owner object exist" objection to the rejected alternative does not arise — the entry
  object always exists.
- **One init, `@pv_fclo_init`, in the entry object**, called as the FIRST line of `pv_init_all`
  (pinned by shape in `tools/seam-audit.sh`). The "all cells before all gdef inits" requirement of
  (c) is then structural rather than a list to get right, and the whole per-object init tier — the
  unconditional empty functions, the object-name list, `foreignCloInitSym`'s object argument — is
  deleted.
- **The initialised set is the leaves the reachable program can execute**: `foldAtoms` over the
  bodies of `reachableGdefs` ∪ the entry expression. `foldAtoms` is added beside `mapAtoms` as its
  read-only sibling, so "which atoms are in here" and "rewrite every atom" cannot disagree about
  what an occurrence is. This is what preserves the pre-existing link contract: a leaf mentioned only
  by code the linker strips is never referenced by a live section, so it still needs no provider.
- **The dead-leaf property is a pinned test**, not a comment — `entryLl` over a reachable gdef that
  calls one leaf and an unreachable gdef that calls another must emit the first symbol and never the
  second. Its discrimination was demonstrated live: the superseded design fails the link.
- (d)'s two layers survive unchanged in spirit: a one-sided inconsistency is a link error, and the
  per-object identity `#cell externs == #leaf declares == |foreigns|` is the census gate.

### 3. Q2 — the TAIL form: a direct call and a return, not a trampoline

A generic tail call today stashes `(f, argv, nargs)` in the ctx, pops the frame and returns a dummy;
the enclosing `pv_apply` loop takes the stash and bounces. That machinery exists to keep *guest*
tail recursion in constant stack. **A native leaf is not guest tail recursion**: it is first-order
host code that returns a real value to its caller. Slice C therefore lowers as

> materialise operands → direct call (a safepoint) → pop the frame → `ret` the result

which adds exactly one host frame, returning before the caller's does. There is no chain to bound.
The frame pop is deliberately **after** the call, not fused before it as `tailcallWith` does: the
leaf may allocate, so the caller's roots must still be live across it. Slice C is literally slice B's
recipe followed by `retWith`; no `musttail` is involved (§2.1: the conventions do not match, so
`musttail` is not available even in principle).

Two clauses are pinned with it, because this is where §3 and §6 interact — if either fails, the tail
form stays on the trampoline or the fallback becomes the common case:

1. **A `pvf_` entry never leaves a pending tail.** `pv_tailcall` is not part of the public leaf
   surface (`purvasm.h` exposes `pv_apply`, not `pv_tailcall`), and no runtime leaf stashes one. So
   the direct path needs no `pv_settle` — and `abiSettle`, which the *guest* direct path must emit
   for exactly this reason, is deliberately absent here.
2. **A leaf that re-enters guest code does so through its own `pv_apply`,** which owns its own
   trampoline loop and clears its own stash. Unchanged by this ADR, and it is what makes clause 1 a
   property of the *entry* rather than of the whole call tree beneath it.

**How clause 1 is held (P2, review round 1).** "Call every `pvf_` entry directly and assert the slot
is clear" is not a runnable test — `Partial._crashWith` aborts, `exit` terminates, several leaves
need well-formed guest values. It decomposes into four checks that are each real:

- a **surface check**: `pv_tailcall` is absent from `runtime/include/purvasm.h` (a header-surface
  assertion in the ABI test, so exposing it later cannot happen silently);
- a **re-entrancy fixture**: a leaf that calls back into guest code through `pv_apply` returns with
  the pending-tail slot empty (this is the case clause 2 describes, and the one where a bug would
  actually live);
- a **normal-return sweep**: the built-in leaves that return normally (the string substrate and the
  scalar leaves — the entire measured population) are called directly and asserted to leave the slot
  clear. Diverging leaves are excluded by name, with the exclusion list in the test;
- for **third-party providers** the clause is ABI text in `purvasm.h`, not a test. A provider that
  violates it violates the documented contract; nothing in-tree can enumerate them.

At 9.3 M of 434 M (2.1 %) the tail form is not where the mass is. It is included so that
`callee-foreign` is not split across two lowerings for no reason — but if clause 1 cannot be held,
dropping slice C costs 2.1 % of the population and nothing else.

### 4. The direct call is a CLASSIFIED SEAM operation (P1, review round 1)

`@pvf_*` may allocate on the guest heap, so it is a **safepoint** by the ADR-0105 §1 ground truth,
and it must not be added as raw call text. Three things are normative:

- **A dedicated renderer in `Backend.LLVM.Safepoint`,** beside `guestDirect` and for the same
  reasons — it is a call whose symbol varies, so it is a renderer rather than an `RtOp` row:

  ```text
  foreignDirect :: { fsym :: String, clo :: Val, argp :: String, nargs :: Int } -> Codegen Val
  ```

  It verifies its operands against the declared roles (`clo` is a guest value and goes through the
  token check; `argp`/`nargs` are raw metadata), which means a rooted `clo` reload emits *before* the
  call line; it emits the call; it bumps the epoch exactly once; it mints the result token
  post-bump. `unsafeEmitRawCall` stays confined to it, and `tools/seam-audit.sh` keeps rejecting raw
  call text elsewhere.
- **The PLAN stays on one conservative call-family row; the discrimination lives at the seam**
  (P1, review round 2). Round 1 said `Liveness`'s `CApp` transfer would read a `foreignCallSafepoint`
  constant "for a `ForeignTarget` call". That is not implementable in the current phase structure and
  the ADR must not pretend otherwise: `activationPlanWith` is a PURE analysis over the raw `CExpr`
  and the by-need `FactMap`. It has no `Codegen` state — no `Env`, no `Ctx.foreignArity`, no
  `CallTarget` — so it cannot tell a foreign callee's saturation from anything else, and making it
  able to would mean threading an opaque call-decision set from the classifier into the plan and
  keeping the two in sync. That is a much larger change than this ADR needs, for no behavioural
  difference: **every call family here is a safepoint**, so the plan is identical either way.

  So: `cexprCanSafepoint`'s `CApp`/`CPerform` arms keep reading ONE call-family classification
  covering guest-direct, generic-apply and foreign-direct alike, and the seam declares the foreign
  renderer's own `sp = true` beside it. The invariant that makes the conservative arm sound is
  stated rather than assumed: *if any call family ever becomes non-safepoint, the plan needs the
  shared call-decision set* — a named, deliberate future dependency, not something to be introduced
  quietly by a row edit.
- **Slice A's de-safepointing goes through the same table, via an ABSTRACT rooted-read row.**
  `Liveness.atomCanSafepoint`'s foreign arm reads `rtSafepoint RtMakeClosure` today precisely because
  that is the call `Emit.atom` renders. After slice A the recipe renders a **rooted read**, whose two
  renderings are ABI variants of one operation — an inline load under the release inline ABI
  (ADR-0079), a `pv_get` call under `--debug`. Naming `RtGet` alone would be wrong about the shipped
  build (P1, review round 2), so the seam gets a named abstract classification for the operation
  covering both renderings (`rootedReadSafepoint = false`), and the foreign arm reads THAT — becoming
  structurally the same arm as `AtomVar`, which is exactly what a hoisted reference now is.
  `forcedAtomCanSafepoint` inherits it unchanged.

  Pinned with a counterfactual fixture, the shape that caught the 2026-08-06 force divergence: with
  the row flipped, the plan must move; with it as shipped, a name that used to cross a foreign
  materialisation no longer does. An analysis that agrees with the lowering only by coincidence is
  what this seam exists to prevent.

### 5. Q3 — does dispatch reduction convert to time? The A/B contract, pinned BEFORE implementation

ADR-0107 is the standing evidence for why this section precedes the code: a −16.79 % force-chain
reduction and a −2.66 % `.ll` produced no run-time claim, and its A/B is still owed. **A 434 M
dispatch reduction is a large COUNT; a count is not a time.**

Unlike ADR-0107, this change has *mechanical* endpoints that are exactly predictable in advance —
that is the difference between the two levers, and it is the argument for pinning the numbers now
and letting the measurement falsify them. **Every endpoint below is per slice**, and no slice's
result may be netted against another's (P1/(c), review round 1).

#### 5.1 Mechanical endpoints — EXACT, per slice, and completion conditions

Measured with the ADR-0108 harness (`tools/apply-profile.sh --selfhost --build-mode opt --work-mode
no-opt`) over one pinned CoreFn snapshot, legs taken pairwise between consecutive slices.

**Slice A (allocation only; dispatch must not move).**

```text
every ADR-0108 slot, pv_apply_entries, pv_tailcall_writes   ==  UNCHANGED, to the unit
AtomForeign materialisation executions(before)              ==  the same, after   (leg invariance)
Kind::Closure allocations(before) − Kind::Closure allocations(after)
  ==  AtomForeign materialisation executions(before)  −  hoisted fclo init executions(after)
hoisted fclo init executions(after)  ==  |reachable leaf keys|             (static, × 1 each)
                                     ==  #entry cell definitions
                                     ==  #permanent-root stores in @pv_fclo_init
```

(AMENDED with §2.2: the hoisted count is the ENTRY object's reachable-leaf set, not `Σ over objects
of |foreigns|` — that was the superseded ownership, and leaving it here would make the completion
test specify the rejected design. Leg invariance of the materialisation counter is checked rather
than assumed: it is what makes the closure delta attributable to this change and not to the two legs
having run different work. `tools/apply-profile.sh --alloc-identity BEFORE AFTER` is the verdict.)

The three-way identity is the review's correction, and it matters twice over. First, **slice A does
not remove all the closures**: it allocates one per reachable key at init, so the reduction is the
old materialisation count MINUS those, not the old count. Second, **a `Kind` total cannot attribute
the leaf closures**: `Kind::Closure` counts every closure the program builds, and the foreign
materialisations include VALUE positions that no ADR-0108 dispatch slot ever counted. So both ends
are instrumented rather than inferred:

- a new opt-in counter at the `AtomForeign` materialisation site (`sp = false`, the ADR-0108 §3
  discipline — the bump observes, it does not participate), which is what makes the "≥ 434 M"
  of the Context a measured number and exposes the value-position share for the first time;
- the same counter at the hoisted init, whose value is ALSO statically predicted (Σ |foreigns|,
  each executed once) — two derivations landing on one integer, the ADR-0107 cross-mechanism rule;
- the `Kind::Closure` counter itself, which is **ADR-0108 step 5's smallest useful slice and a
  PRECONDITION of this ADR** — taken before slice A and again after, on a deterministic program, so
  the equality is exact and not a tolerance.

**Slice B (the apply form only).** Both legs classify identically (§1.2), so the eligible population
is counted in BOTH — as `foreign-deferred/apply` in the off leg and `foreign-direct/apply` in the on
leg. The endpoint is therefore an equality between two counted quantities, not a delta against a slot
whose meaning changed:

```text
foreign-deferred/apply (off leg)  ==  foreign-direct/apply (on leg)      -- one number, two legs
pv_apply_entries(off) − pv_apply_entries(on)  ==  that number, exactly
callee-foreign APPLY slot (both legs)  ==  the §6 arity-mismatch residue, unchanged between legs
pv_tailcall_writes, foreign-*/tail, Kind::Closure  ==  UNCHANGED vs slice A
```

**Slice C (the tail form only).**

```text
foreign-deferred/tail (off leg)  ==  foreign-direct/tail (on leg)
pv_tailcall_writes(off) − pv_tailcall_writes(on)  ==  that number, exactly
callee-foreign TAIL slot (both legs)  ==  the §6 residue, unchanged between legs
pv_apply_entries, Kind::Closure  ==  UNCHANGED vs slice B
```

**Invariant across all three** (ADR-0108 §3/§4, extended by the deferred classes only):

```text
Σ generic-apply + structural + foreign-deferred/apply == pv_apply_entries
Σ generic-tail               + foreign-deferred/tail  == pv_tailcall_writes
Σ drill keys                 == the callee-foreign slots + the foreign-deferred slots
```

(The deferred classes appear on the left because in the off leg they ARE `pv_apply`/`pv_tailcall`
dispatches — that is the point of giving them their own name instead of letting them hide inside
`callee-foreign`.)

#### 5.2 The run-time endpoints

Conditions, inherited from the ADR-0107 close-out and not negotiable downward: a quiet dedicated or
self-hosted Linux box (not a shared runner, not a desktop carrying an editor); cases sized to
**3–10 s**; **≥ 20 order-alternated pairs**; the **median and full distribution of the PAIRED
ratios**, never a min-of-K; `perf stat` instructions / branches / branch-misses beside wall time;
`gc_collections` / `gc_copied_words` / `gc_max_live_words` reported per leg; snapshotted inputs;
every knob owned by the harness, the per-program heap passed explicitly (the single change that
narrowed ADR-0107's spread ~40×); and a refusal to time a program the change does not alter.
`tools/byneed-ab.sh` is the template — the leg pair here is "same compiler sources, slice on / off
via a build knob", the same shape as its lattice knob.

Three legs, run and reported separately: **baseline → A**, **A → A+B**, **A+B → A+B+C**.

The corpus must include at least one case that is **not** string-substrate-dominated, so the result
is not a measurement of `byteAt` alone.

#### 5.3 The noise floor, and the decision rule — both pre-committed

"Beyond the measured noise floor" is not interpretable after the fact, so the floor is DEFINED
before any comparison leg runs (P1/(c), review round 1):

> For each case, an **A/A pair** — the same binary against itself, same protocol, ≥ 20 order-alternated
> pairs — yields a paired-ratio distribution and its **[p5, p95] interval**. A/A is run on **BOTH
> endpoints of the leg** (the baseline binary and the candidate binary), and the case's **noise floor
> is the ENVELOPE — the union — of the two intervals** (P2, review round 2: the candidate's variance
> is not assumed equal to the baseline's, and removing an allocation can change it in either
> direction). Both intervals are recorded with the result, not just the envelope.

Each slice leg on each case is then classified mechanically:

| verdict | condition |
| --- | --- |
| **WIN** | the leg's paired-ratio median lies below the case's A/A [p5, p95] interval |
| **REGRESSION** | the median lies above it |
| **INCONCLUSIVE** | the median lies inside it |

`INCONCLUSIVE` is a distinct verdict, never rounded to either neighbour, and never reported as "no
regression".

The rule, decided now:

- the §5.1 mechanical endpoints are **completion conditions, per slice**. Any inexactness is a
  defect, not a tolerance — the slice does not land while a column is short;
- the run-time result is **reported in both directions for every leg**, and **no run-time claim is
  made in this ADR without it**. If the box is not available, a slice may close on its mechanical
  endpoint alone, explicitly labelled as such — the ADR-0107 precedent;
- a **REGRESSION on a leg blocks that slice**, pending explanation. It is not offset by another
  slice's WIN, and not averaged across cases: slice A's expected large win must not be allowed to
  absorb a slice B regression, which is precisely why the legs are separated;
- an **INCONCLUSIVE** leg keeps slice A on its measured allocation endpoint (an allocation removal
  the `Kind::Closure` counter and `gc_*` measure directly, independent of time), and puts slices B/C
  to the maintainer as a judgement call with the null result stated. It does not get retold as a win.

### 6. Q4 — where the fallback stays

The generic path is not removed anywhere. It remains, unchanged, at:

- **an unsaturated or over-saturated foreign call** (`nargs ≠ foreignArity k`) — PAP construction and
  over-application semantics stay `pv_apply`'s. Measured 0 on the step-4 workload; implemented and
  fixture-tested anyway, because "0 on one workload" is not "unreachable";
- **a foreign atom in value position** — passed as an argument, stored in a dictionary, returned.
  Slice A changes how the closure is *obtained* (a global read instead of an allocation), not what
  happens to it afterwards;
- **a callee that is a variable bound to a leaf closure** — the emitter sees `AtomVar`, not
  `AtomForeign`, and classifies it as it does today (`local-unknown-fn` &c.). Chasing that is a
  different lever, out of scope;
- **a missing arity fact** — still a compile-time crash (ADR-0090: the FSR shape is the single source
  of truth, and a missing entry is a wiring bug, not a default);
- **the VM and OCaml backends** — untouched. This is an LLVM-backend lowering.

Pinned, and this is what makes the fallback safe to have: **`callee-foreign` does not disappear from
the census and the profile — it becomes the residue counter.** A change that silently pushes traffic
back onto the generic path (a regressed saturation test, a lost arity fact) shows up as a non-zero
residue against a pinned expectation, not as a quiet slowdown.

### 7. Accounting: the ADR-0108 harness grows a call class

`CallEvent` gains four constructors, **split by form** rather than carrying a form field — the
project's "make the invalid state unrepresentable" rule and ADR-0108 §1's own reason for a
per-constructor payload (P2, review round 1) — in one-to-one correspondence with §1.2's
`EmissionDecision`, so a decision and its event cannot disagree:

```text
  | ForeignDirectApply   ForeignRef
  | ForeignDirectTail    ForeignRef
  | ForeignDeferredApply ForeignRef   -- the knob-off leg; retired with the knob (§1.2)
  | ForeignDeferredTail  ForeignRef
```

The payload is the `ForeignRef` itself, so the census reports the symbol the emitter emitted rather
than a re-mangled copy of the key (§1.1).

and the ADR-0108 §2 six-column identity gains a column (the deferred events lower to `pv_apply` /
`pv_tailcall`, so they land in the existing two columns rather than a new one):

```text
pv_apply     == generic-apply + structural-apply + foreign-deferred-apply events
pv_tailcall  == generic-tail                    + foreign-deferred-tail  events
musttail     == direct-musttail events                                   (unchanged)
guestDirect  == direct-nontail events + wrapper entries                  (unchanged)
call @pvf_*  == foreign-direct-apply + foreign-direct-tail events        (NEW)
```

with every needle anchored to the two-space instruction indent, per the ADR-0108 §2 gotcha (the
corpus is the compiler; a module that emits LLVM carries emitted syntax as string constants — and
this ADR adds `@pvf_` and `$fclo` to the strings that emitter carries). The slice-A cell gate of
§2.2(b) and the object-count gate of §2.2(d) are two more per-object identities in the same census.
The dynamic side gets one slot per foreign-direct and per foreign-deferred form, so the population is
counted at BOTH ends of every leg and §5.1's equalities are checked rather than inferred from a
disappearance.

### 8. What this does not do

- It does not inline any leaf body into the emitted IR. `byteAt` remains a call across the C
  boundary with a `guard` shim and an `args_slice`. If §5 shows the residual call to be the cost,
  *that* is the next lever and a different ADR (a much more invasive one: it re-implements the
  ADR-0103 string substrate's representation inside the emitter, which today has exactly one owner).
- It does not flatten the argument buffer into registers. A second, register-passing entry per leaf
  (`pvf_flat_…(ctx, a0, …, an)`) would remove the `alloca` and the stores, at the cost of doubling
  the leaf surface and the published header's contract. Rejected for now as unmeasured; §5.2's
  `perf stat` leg is what would justify revisiting it.
- It does not touch `local-unknown-fn` (26.1 % of executions, the higher-order call), which the
  ADR-0108 ranking puts second and which needs its own design.

#### Progress (2026-08-16): slice 0 — the allocation census and the materialisation counter

The §5.1 precondition, implemented and verified end to end before slice A touches a lowering.

**Runtime.** `Heap::alloc` — the single mutator allocation site — bumps a per-`Kind` counter, and the
census prints as `alloc/kind/*` rows on the `purvasm-applyprofile:v1` line (ADR-0108 §5's "all of it
lives in the step-3 profile schema, never in the dispatch counts"). Two properties are tested rather
than asserted in prose: `Kind::ALL` is pinned discriminant-indexed (a kind added to the enum but not
the census would otherwise count under a neighbour's label), and **a collection is not an
allocation** — the collector evacuates through `collect_core`, so a forced collection leaves the
census unchanged while the survivor really moves. That is what makes this allocation VOLUME rather
than a restatement of `gc_copied_words`.

**Compiler.** `Emit.atom`'s `AtomForeign` arm bumps an `alloc/site/foreign-materialise` slot. The
allocation sites share the apply profile's registration and bump ABI — one layout definition, one
blob, one `pv_applyprofile_bump` — but are a separate family in `CallClass` (`AllocSite`), named
under their own prefix and given the HIGH slot indices so no dispatch slot is renumbered by them.

**Harness.** `slots_of`'s row pattern was too tight to read a three-segment name, which would have
dropped every census row SILENTLY; it now parses any `/`-separated name and fails when a token on
the line does not parse at all. The dispatch identities and the reason ranking take the
`dispatch_rows` family only. Five new `--self-test` cases inject the two faults that matter (an
unreadable row; an allocation row reaching a dispatch sum) and assert the verdict.

**First numbers (fixture leg, `--opt`; the self-host leg is a slice-A/B measurement, not this one).**
Both §3 identities still hold exactly and the instrumented output is unperturbed:

```
alloc/site/foreign-materialise   68,176      == the callee-foreign dispatch total, exactly
alloc/kind/closure               94,296      → foreign materialisations are 72.3 % of ALL
                                                Kind::Closure allocations on these fixtures
```

The first line is the Context's derivation ("one materialisation per counted dispatch") measured for
the first time — on THESE fixtures it is an equality, which also says they contain no value-position
foreign reference. The self-host corpus is not expected to be so clean, and the difference is now
observable instead of assumed.

#### Progress (2026-08-16): slice A — the hoisted leaf closure

Implemented per §1.1 (the opaque `ForeignRef`), §2.2-amended (entry-owned cells) and §4 (the
abstract rooted-read row). `Emit.atom`'s `AtomForeign` arm no longer allocates: it reads the cell,
the same lowering as any rooted global.

**The §5.1 identity holds EXACTLY on the fixture corpus**, and it is the first measured statement
about what the foreign path allocates:

| | before slice A | after |
| --- | ---: | ---: |
| `alloc/kind/closure` | 94,296 | **26,143** |
| `alloc/site/foreign-materialise` | 68,176 | 68,176 |
| `alloc/site/foreign-clo-init` | — | 23 |

```
ΔKind::Closure = 94,296 − 26,143 = 68,153
materialisations(before) − hoisted inits(after) = 68,176 − 23 = 68,153      ✓ to the unit
```

**72.3 % of every closure these fixtures allocated was a leaf closure, and slice A removes all but
23 of them.** The dispatch side is untouched, exactly as the slice-A endpoint requires: every
ADR-0108 slot is unchanged (`callee-foreign` still 68,176), both §3 identities still hold exactly,
and the instrumented run's output is unperturbed.

**The rooting moved with it, through the shared table.** `Liveness.atomCanSafepoint`'s foreign arm
now reads the new abstract `rootedReadSafepoint` row (§4) instead of `rtSafepoint RtMakeClosure`,
so a foreign reference stops being a safepoint for the ANALYSIS at the same moment it stops being one
in the LOWERING. The counterfactual is a pinned fixture rather than a claim: an operand list
`[foreign, x]` no longer roots `x`, while `[boxed-string, x]` — the same shape with a still-allocating
atom — still does, so the fixture is discriminating in both directions.

**Gates green**: 571/571 compiler unit (4 new: the cell/extern split, the dead-leaf property, and
the crossing counterfactual), 31/31 census, the e2e/json/regex/ulib-tools suites,
`tools/seam-audit.sh` (self-test + the two new ADR-0109 cages), `tools/l2-native-behavioural.sh`
7 fixtures × VM/no-opt/opt/stress×2 ≡ oracle **under forced GC** — the discriminator that matters
after a rooting reduction — `tools/ffi-e2e.sh` (user C **and** Rust FFI still produce 42, so the ABI
really is untouched), `tools/apply-profile.sh` fixtures + `--self-test`, `cargo fmt`/`purs-tidy`.

**Hardened in review round 3** (three P1s, all of them about a measurement that could pass while
saying nothing):

- **the reachability walk is stack-safe and pinned.** `foldAtoms` was spine recursion and an
  independently-written case tree — on generated ANF that overflows, and a field missed in one tree
  and not the other would leave live module code reading a cell the entry never defined. It is now an
  explicit work stack under `tailRec`, with a per-node **fidelity matrix** (every `CExpr`/`Rhs` form,
  a distinct atom in every atom position, an exact expected list — it caught an ordering error in the
  expectation on first run) and 100k-spine fixtures (bare, and nested under a guard clause). The doc
  no longer claims "the same traversal as `mapAtoms`": they are two trees held in agreement by that
  matrix, which is the honest statement;
- **the allocation census can no longer disappear quietly.** The presence-only check accepted any
  `alloc/*` row, so the compiler-owned site rows alone kept the leg green while the runtime's entire
  kind family could vanish and print `Kind::Closure=0`. `check_alloc_schema` now requires every kind
  row and every required site row exactly once, and refuses an unknown `alloc/*` row — against a
  schema stated INDEPENDENTLY in the harness, because an expectation read off the measurement is not
  a gate. Six self-test injections (family gone, one row gone, a site gone, a duplicate, an unknown
  row) assert the verdict;
- **the §5.1 identity is executable.** `tools/apply-profile.sh --alloc-identity BEFORE AFTER` parses
  two captured runs, requires a complete census in both, checks **leg invariance of the
  materialisation counter** (the property that makes the closure delta attributable to the change
  rather than to two legs doing different work — the fault injection that first slipped through
  proved the identity was blind without it), and exits non-zero on mismatch. Three more self-test
  injections cover it.

#### Progress (2026-08-16): the slice-A counterfactual knob

§5.2 pins the leg pair as "same compiler sources, slice on/off via a build knob", and slice A first
shipped without one — so the paired legs could not be built from one tree at all. The knob:

- **a closed type, not a Boolean**: `ForeignClosureMode = PerUse | Hoisted`, parsed ONCE at the CLI
  edge from `PURVASM_FOREIGN_CLOSURE` and handed as one value to the emitter and to
  `ActivationConfig`. Fail-closed — an unrecognised value is an error, never "the shipped mode",
  because a typo would make the counterfactual leg the shipped leg and report a real change as no
  change (a unit matrix drives eight rejected spellings);
- **the plan switches with the emitter**, which is the load-bearing part rather than the tidy one:
  `PerUse` allocates at every foreign reference, so `Liveness.atomCanSafepoint`'s foreign arm reads
  `RtMakeClosure` again under it. A mode that reached only the emitter would UNDER-ROOT that leg —
  a GC bug, not a slower measurement. Pinned by a crossing fixture in both directions;
- **`PerUse` is the pre-slice-A program, not an approximation**: a closure per reference, no cell, no
  extern, no `@pv_fclo_init`, no call in `pv_init_all` — and the ordinary `@pvf_` declare kept.
  Asserted per object, in both directions;
- **harness-owned**: all nine `tools/*.sh` scrub an ambient value, `apply-profile.sh` grew
  `--foreign-closure hoisted|per-use` and EXPORTS the leg it was told to build, so a leg is what the
  flag says and never what the caller's shell held.

**The paired mechanism works end to end** — two binaries from one tree, on `Gate.DictDispatch`:

```
ADR-0109 §5.1 identity: ΔKind::Closure 21,984 == materialisations 21,992 − hoisted-inits 8  OK
pv_apply_entries       25,537 == 25,537          (dispatch untouched, both legs)
generic-apply/callee-foreign 20,480 == 20,480    (classification leg-invariant)
stdout                 identical
```

#### Progress (2026-08-16): slice A measured on the SELF-HOST corpus, paired

`tools/apply-profile.sh --paired` runs both modes from ONE snapshot and ONE toolchain build — two
separate invocations cannot give that (each snapshots at its own moment, and `output/` carries the
compiler's own JS, so a `spago build` between them changes the program being measured; the default
workdir also overwrites). Each leg is checked against a reference, then the pair is verdicted.

**Which leg the knob applies to — corrected BY the first paired run failing.** The knob is a
BUILD-mode axis in exactly ADR-0108 §3's sense: it decides how the measured binary is LOWERED, not
what work it performs. Setting it for the workload compile too made the two legs emit different `.ll`
for the workload, so the compilers did different work and every dispatch counter legitimately
differed (`callee-foreign` 428,084,567 vs 427,910,970). That is a measurement of two different runs.
Leg 2 (building the compiler) now carries the mode; legs 1 and 3 run in the shipped mode in both,
and "the two legs did the same work" is a VERDICT — byte-identical workload artifacts — rather than
an assumption.

**Result (`--paired --build-mode opt --work-mode no-opt`, 305 objects, every verdict OK).**

```
ADR-0109 §5.1 identity: ΔKind::Closure 437,263,683
                     == materialisations 437,263,715 − hoisted-inits 32          EXACT
dispatch slot vector             IDENTICAL (17 slots)   — slice A moves no dispatch
per-use hoisted-inits            0
hoisted inits == cells == stores == leaf symbols          32   (four derivations, one integer)
workload emission                BYTE-IDENTICAL (305 objects)
per leg: Σ generic-apply + structural == pv_apply_entries == 635,226,491
         Σ generic-tail             == pv_tailcall_writes == 124,592,053
         Σ drill keys               == the callee-foreign slots == 437,263,707
```

**437.3 M guest-heap closure allocations removed from one self-host build**, leaving 32 — the
Context's "≥ 434 M", now measured rather than derived. And the first measurement of the
VALUE-position share the Context called unmeasured: materialisations 437,263,715 − dispatches
437,263,707 = **8** foreign references that are not calls, on the whole corpus.

**The IR delta, over the corpus that changed** (each leg's own compiler objects; the workload
emission is byte-identical by the verdict above, so it is not the corpus here):

Every measure is ANCHORED to its instruction form and to the two-space instruction indent, which is
not pedantry: the corpus IS the compiler, so a module that emits LLVM carries emitted syntax as
string constants — the ADR-0108 §2 trap, hit again here (see below the table).

| measure (anchored needle) | per-use | hoisted | delta |
| --- | ---: | ---: | ---: |
| `.ll` lines | 3,381,958 | 3,311,757 | **−70,201 (−2.08 %)** |
| `.ll` bytes | 113,869,012 | 111,549,178 | **−2,319,834 (−2.04 %)** |
| root chains (`^rchk` label) | 84,665 | 81,273 | **−3,392 (−4.01 %)** |
| `pv_root` (`^␣␣%t = call i64 @pv_root(`) | 84,665 | 81,273 | −3,392 |
| `pv_make_closure` (same form) | 18,797 | 15,104 | −3,693 |
| cell READS (`^␣␣%t = load i64, ptr @pvf_…$fclo`) | **0** | **3,725** | +3,725 |
| `pv_apply` sites | 14,202 | 14,202 | 0 |
| `pv_tailcall` sites | 5,109 | 5,109 | 0 |

The rooting reduction is the §4 de-safepointing arriving in the IR: 3,392 fewer root chains because
a foreign reference stopped being a safepoint. Reload and frame counts are deliberately ABSENT — in
the release inline ABI both are bare load/store on the ctx header with no distinguishing text, so
there is no honest grep for them; the root-chain count is the rooting proxy and it is exact.

**Two counting faults, both caught by a second derivation rather than by reading the script.** The
first: the `$fclo` needle was shell-escaped into a literal `\$fclo` and read 0 in BOTH legs, while
the cells/stores/symbols verdict in the same run said 32. The second, after fixing that: the
unanchored needle read **1 on the per-use leg — which builds no cells at all** — because
`ForeignRef.purs`'s own `c"$fclo"` string constant is in the compiler's object. The anchored recount
decomposes the old 4,108 exactly, which is what makes the 3,725 trustworthy:

```
3,725 read instructions + 318 extern decls + 32 cell definitions
      + 32 permanent-root stores + 1 string constant  ==  4,108
```

**The gates this ADR names, re-run on the tree that carries the knob** — the earlier fixpoint pass
does not carry, because the compiler's own CoreFn changed with the knob:

- `tools/selfhost-fixpoint-diff.sh smoke` — **609/609 stage-3 ≡ stage-4 byte-identical** (and
  C3-link ≡ stage-3, 609/609);
- `examples/run-examples.sh` — **10/10** (fib, helloworld, effect-ref, record-meta,
  recursion-scheme, recursive-value, transformer, rust-ffi, regex-demo, string-case-demo);
- `tools/native-run-diff.sh` — **7/7**, boot ≡ Level-2 ≡ expected;
- `tools/l2-native-behavioural.sh` — 7 fixtures × VM/no-opt/opt/stress×2 ≡ oracle under forced GC;
- `tools/ffi-e2e.sh` — user C **and** Rust FFI both produce 42;
- `tools/seam-audit.sh` + `tools/apply-profile.sh --self-test`, 585/585 compiler unit and the other
  four suites, `cargo fmt` / `purs-tidy`.

**SLICE A CLOSED 2026-08-16 — mechanical and correctness complete, run time OWED, and NO run-time
claim is made.** §5.3 permits exactly this close: the mechanical endpoints are completion conditions,
the run-time result is reported in both directions or not at all, and a slice may close on (a) plus
the allocation/IR argument when the box is unavailable — explicitly labelled, as here.

What slice A establishes: **437,263,683 guest-heap closure allocations removed from one self-host
build** (32 remain), by two independent mechanisms — the runtime per-`Kind` census and the
compiler-side site/init counters — agreeing to the unit, with the dispatch axis pinned unchanged and
the two legs pinned to have done identical work. What it does NOT establish: that this is faster.

**The one OWED measurement**, tracked as its own item and not folded into any later slice: the
base→A A/A intervals on both endpoints and the A/B verdict, recorded even when INCONCLUSIVE. It needs
the quiet Linux box §5.2 requires; everything else above is integer accounting and IR measurement,
which this machine produces.

**Re-baselined**: the `slice1` emission-shape golden (ADR-0104 §4), whose entry object now carries
`@pv_fclo_init` and its call. The nullary-`Effect` leaf-arity test moved its subject from the module
object to the entry's init and kept its discrimination (arity 1 vs the `leafClosureArity` revert's 0).

#### Progress (2026-08-17): slice B — the saturated apply form calls the leaf directly

Implemented per §1.2 (eligibility and emission are separate closed types), §2 (the direct call), §4
(a classified-seam renderer) and §7 (the accounting column).

**Three stages, not two — corrected in review before any measurement.** A two-state knob would have
netted slices B and C together: slice B's mechanical endpoint IS that `pv_tailcall_writes` does not
move, and that is unobservable in a build where the tail form changed too. The mode is therefore

```text
ForeignCallMode = ViaApply | DirectApplyOnly | DirectApplyAndTail
```

with `DirectApplyOnly` the default until slice C has its own checkpoint. Measured on a real build
(`Gate.DictDispatch`, one tree, three legs, identical stdout):

| stage | `@pvf_` sites | `pv_apply` sites | `pv_tailcall` sites |
| --- | ---: | ---: | ---: |
| `ViaApply` | 0 | 1,920 | 767 |
| `DirectApplyOnly` (slice B) | 166 | 1,754 (−166) | **767 (unchanged)** |
| `DirectApplyAndTail` (slice C) | 235 | 1,754 (unchanged) | 698 (−69) |

**A defect the review caught, and what could honestly be pinned.** The form was derived once for
every target from `tail && inDirect` and handed to `decide`, while the recipes branched on the raw
`tail` — so a `tail && not inDirect` site would have emitted a TAIL recipe and recorded an APPLY
event. The derivation is now the pure, exported `CallClass.callForm`, target-aware, and the
DECISION's form drives every recipe (including the drill's key string).

The regression fixture asked for could not be built, and saying so is part of the fix: a `Gcaf` body
is NOT tail position (it must produce a value to root, so it is emitted non-tail — measured, after
the first attempt at such a fixture reported `apply = 2`), and every tail context in the emitter today
is a lifted body, which sets `inDirect`. The state is unreachable, so no fixture can reach it.
`callForm` is pinned instead over its WHOLE input space (3 targets × tail × inDirect), which is where
the wrong answer lived and stays checked if a future activation kind makes the state reachable.

**The paired harness now takes an AXIS** — `--paired closure|apply|tail` — with every knob off the
axis fixed at one value in both legs, and row-level verdicts rather than totals (a total could net a
tail regression against an apply win). Verified at fixture scale before the self-host run:

```
moved:      foreign-deferred-apply 20,480 → 0    foreign-direct-apply 0 → 20,480
INVARIANT:  foreign-deferred-tail 1,512 == 1,512     foreign-direct-tail 0 == 0
            generic-*/callee-foreign 0 == 0          materialisations 21,992 == 21,992
            alloc/kind/closure 5,590 == 5,590        pv_tailcall_writes 2,015 == 2,015   OK
```

That pre-flight also found two harness faults (an IR row label containing `$fclo` was expanded by the
shell and aborted the run under `set -u`; the IR table header named the closure axis whatever axis
ran) and one imprecision now fixed: a MISSING artifact set is reported as missing, not as "the legs
did not do the same work".

**Gates re-run after the B/C split** (the emission changed, so the earlier pass does not carry):
592/592 compiler unit, the other four suites, `tools/seam-audit.sh` (renderer counts re-pinned),
`tools/l2-native-behavioural.sh` under forced GC, `examples/run-examples.sh` 10/10,
`tools/native-run-diff.sh` 7/7, `tools/ffi-e2e.sh`, `purs-tidy`.

**The self-host measurement (`--paired apply --build-mode opt --work-mode no-opt`, 305 objects).**

```
TRANSFER (fail-closed, five conditions):
  before: something to transfer      foreign-deferred-apply = 430,241,659 > 0        OK
  before: no direct calls yet        foreign-direct-apply   = 0                      OK
  after:  nothing left deferred      foreign-deferred-apply = 0                      OK
  transfer is EXACT                  foreign-direct-apply   = 430,241,659            OK
  runtime counter agrees             pv_apply_entries 638,915,068 − 208,673,409
                                                            = 430,241,659            OK
INVARIANT:
  foreign-deferred-tail    9,417,058 == 9,417,058     foreign-direct-tail  0 == 0
  generic-{apply,tail}/callee-foreign  0 == 0         alloc/site/foreign-clo-init 32 == 32
  alloc/site/foreign-materialise 439,658,725 == same  alloc/kind/closure 102,076,450 == same
  pv_tailcall_writes (runtime)  125,420,753 == 125,420,753
  workload emission             BYTE-IDENTICAL (305 objects)
```

**430,241,659 generic dispatches became direct calls in one self-host build, and the trampoline
counters did not move by one.** The transfer is exact in four places the compiler's classification
owns and in a fifth the runtime wrote down its own path — `pv_apply_entries` fell by precisely the
transferred count. The IR side carries the same signature on the changed corpus: `pv_apply` call
SITES 14,245 → 11,020 (−3,225), `pv_tailcall` sites 5,123 unchanged, root chains / `pv_make_closure` /
cell reads unchanged, `.ll` lines unchanged with +81,235 bytes (a direct call line is longer than a
`pv_apply` line — same count, more text).

**The first run reported FAIL, and it was a harness schema defect, not a failed measurement.**
`reconcile()` — the §3 runtime identity — had never been extended to the ADR-0109 classes, so the
via-apply leg summed 208,673,409 against `pv_apply_entries` 638,915,068 and reported a mismatch. The
missing 430,241,659 were exactly the deferred applies: §7's identity was written into this ADR and
into `apply-census.sh`, and not into the runtime reconciliation. **The identity was not relaxed to
make the run pass** — `reconcile()` now includes the deferred rows (and deliberately excludes the
direct ones, which are not dispatches), four self-test injections pin both directions, and the SAVED
artifacts were re-reconciled rather than the four-hour build re-run:

```
via-apply : 208,673,409 + 430,241,659 = 638,915,068 == pv_apply_entries
both legs : 116,003,695 +   9,417,058 = 125,420,753 == pv_tailcall_writes
```

**A second harness defect, found in review:** the transfer rows were PRINTED, not verdicted — an
absent row read as 0, so a partial transfer, or both rows vanishing, would have passed. It is now
`transfer_verdict`, fail-closed on all five conditions above, with a MISSING row a failure rather
than a zero, and six self-test injections (complete transfer; one dispatch left deferred; direct
short by one; rows correct but the runtime delta wrong; a missing row; a vacuous pair with nothing to
transfer). Writing those found a third: a concatenated `case` guard with an empty alternative matches
every string, so the guard had been failing every pair — caught by the "a COMPLETE transfer passes"
row, which is why a passing case belongs in a fault-injection suite.

**SLICE B CLOSED 2026-08-17 — mechanical and correctness complete, run time OWED, and NO run-time
claim is made** (the §5.3 close, as for slice A). What it establishes: the eligible apply-form
population transfers exactly, checked across two mechanisms, with the tail axis, the allocation axis
and the workload emission pinned invariant. What it does not: that this is faster.

**Owed for slice B**, tracked as its own item: the base→B A/A intervals on both endpoints and the A/B
verdict, recorded even when INCONCLUSIVE, on the quiet Linux box §5.2 requires.

#### Progress (2026-08-17): slice C — the tail form, and the default moves to it

The tail lowering shipped with slice B's code but behind `DirectApplyAndTail`, so that slice B's
endpoint (`pv_tailcall_writes` invariant) was observable. Slice C is that stage's own checkpoint.

**The measurement (`--paired tail --build-mode opt --work-mode no-opt`, 305 objects, first run, every
verdict OK).**

```
TRANSFER (fail-closed, five conditions):
  before: something to transfer     foreign-deferred-tail = 9,417,058 > 0         OK
  before: no direct calls yet       foreign-direct-tail   = 0                     OK
  after:  nothing left deferred     foreign-deferred-tail = 0                     OK
  transfer is EXACT                 foreign-direct-tail   = 9,417,058             OK
  runtime counter agrees            pv_tailcall_writes 125,420,753 − 116,003,695
                                                          = 9,417,058             OK
INVARIANT (the MIRROR of slice B's, which is what makes the two separable):
  foreign-deferred-apply  0 == 0          foreign-direct-apply 430,241,659 == same
  generic-{apply,tail}/callee-foreign 0 == 0
  alloc/site/foreign-materialise 439,658,725 == same   alloc/site/foreign-clo-init 32 == 32
  alloc/kind/closure 102,076,450 == same
  pv_apply_entries (runtime) 208,673,409 == 208,673,409
  workload emission  BYTE-IDENTICAL (305 objects)
```

**9,417,058 trampoline dispatches became direct calls, and the apply axis did not move by one.** The
IR is the mirror image of slice B's: `pv_tailcall` call sites 5,123 → 4,653 (−470), `pv_apply` sites
unchanged, root chains / `pv_make_closure` / cell reads unchanged, `.ll` +6 lines and +15,001 bytes.
Both legs' §3 identities also hold on their own, which is the extended `reconcile()` confirmed at
scale rather than on injections alone.

(The tail axis's invariant list gained `alloc/site/foreign-clo-init` in review before the run — the
closure axis is pinned `hoisted` in both legs, so it is an invariant of this pair too, and it was
checked on the apply axis already. Symmetry worth having BEFORE a four-hour measurement, not after.)

**The default is now `DirectApplyAndTail`**, moved on that endpoint plus the correctness gates re-run
on the tree that carries it — not before. A unit row pins the default itself, so a future change to it
is a deliberate edit rather than a side effect of adding a constructor. All three stages remain
selectable: `ViaApply` is still both slices' counterfactual, and `DirectApplyOnly` is still the
`--paired tail` pair's BEFORE leg.

**Gates on the new-default tree**: 595/595 compiler unit (3 new for the stage knob's parse) + the
other four suites, `tools/seam-audit.sh`, `tools/l2-native-behavioural.sh` under forced GC,
`examples/run-examples.sh` 10/10, `tools/native-run-diff.sh` 7/7, `tools/ffi-e2e.sh`,
`tools/apply-profile.sh` fixtures (both identities exact, output unperturbed), `purs-tidy`,
`tools/selfhost-fixpoint-diff.sh smoke` — **609/609 stage-3 ≡ stage-4 byte-identical** (and
C3-link ≡ stage-3, 609/609).

**SLICE C CLOSED 2026-08-17 — mechanical and correctness complete, run time OWED, and NO run-time
claim is made**, the same §5.3 close as slices A and B.

**What the three slices together establish**, all of it counted rather than argued, on one named
workload (the compiler built `--opt`, compiling `Purvasm.CLI.Native` `--no-opt`):

| | removed | checked against |
| --- | ---: | --- |
| slice A — guest-heap closure allocations | **437,263,683** | `Kind::Closure` census vs the site/init counters |
| slice B — generic `pv_apply` dispatches | **430,241,659** | `pv_apply_entries`, to the unit |
| slice C — generic `pv_tailcall` dispatches | **9,417,058** | `pv_tailcall_writes`, to the unit |

**And what none of them establishes: that any of it is faster.** The wall-clock A/A + A/B for each
slice remains OWED, per slice, on the quiet Linux box §5.2 requires — and until it runs, this ADR
makes no run-time claim in either direction. _(A provisional measurement was later taken on a
machine that does not meet §5.2's platform clause; see the post-close section below. It does not
discharge this item, and this paragraph stands as written.)_

#### Post-close provisional measurement (2026-08-18): the run-time endpoints, on a box §5.2 does not sanction

**What this section is.** A record of a measurement, not an amendment. §5.2's conditions and §5.3's
decision rule are unchanged and nothing normative is relaxed here. What this run executed is §5.2's
EXECUTABLE disciplines — snapshotted inputs, harness-owned knobs, the per-program heap passed
explicitly, ≥ 20 order-alternated pairs, the median of paired ratios, the GC counters per leg — and
§5.3's decision rule in full. **It does not satisfy §5.2 as a whole**: the platform clause, the
`perf stat` leg and the corpus clause are all unmet, which is why the result is labelled PROVISIONAL
and does not count as the owed one.

**Why provisional — three reasons, each of them standing on its own:**

1. **`perf stat` did not run.** Darwin has no equivalent at this granularity, so §5.2's
   instructions / branches / branch-misses leg is absent. The harness prints the reason rather than
   omitting the column.
2. **The box is not a quiet dedicated Linux machine.** Apple Silicon additionally offers no CPU
   pinning (P/E cores) and no governor control, so the machine state is neither fixed nor commanded.
3. **§5.2's non-string-substrate case is UNSATISFIED.** The clause is kept, not waived — see below.

**What the design does and does not buy, stated precisely.** §5.3's floor is an A/A envelope measured
on the same machine in the same session, and the regime check below additionally requires the
comparison to have run in the state that floor sampled. Together these SUPPRESS the false-positive
risk; they do not eliminate it. A WIN here is a WIN *under a rule fixed in advance*, over finite
samples, drawn from a non-stationary and autocorrelated process, with three axes judged. It is
evidence, not proof, and it is not the §5.2 result.

**The harness**: `tools/foreign-ab.sh`, built to §5.2/§5.3 and modelled on `tools/byneed-ab.sh`. Its
stage lattice is `apply-profile.sh --paired`'s axes verbatim, so the two harnesses describe one
lattice: `s0` per-use/via-apply → `s1` hoisted/via-apply → `s2` hoisted/direct-apply-only → `s3`
hoisted/direct-apply-and-tail. It owns every knob, snapshots its inputs, pins the heap, requires each
run's exit status, alternates pair order, refuses to start above `--max-load`, and prints a
`NOT THE PROTOCOL` banner below 20 pairs. `--self-test` (17 rows) pins the statistics and the
decision rule on injected data, including the row that fails if §5.3's envelope is ever computed as
anything but the UNION of the two endpoints' intervals.

##### The corpus, measured before anything was timed

§5.2 refuses to time a program the change does not alter, and on this corpus that is not a
formality. Measured s1 → s3 by `pv_apply_entries`:

| program | before | after | moved |
| --- | ---: | ---: | --- |
| `quicksort` 50000 | 6,192,904 | 6,192,881 | 23 (0.0004 %) |
| `map-fold-array` 500000 | 48 | 12 | the run performs no dispatch worth timing |
| `fib` 20000 | 42 | 10 | likewise |
| `json-parse` | 27,813 | 6,842 | 75 %, but its input scales with a fixture, so it has no size knob |

This is the expected consequence of ADR-0108 §4's own drill — four symbols carry 91.2 % of the class,
every one of the top seven is an ADR-0103 string-substrate leaf, `byteAt` alone 45.6 %. On this
corpus the population this ADR lowers IS the string substrate.

**§5.2's non-string-substrate clause is therefore recorded as OWED, and is NOT treated as satisfied
by the table above.** That the existing `mixed` cases came out `NOT-A-PROBE` is a fact about the
corpus; it is not a measurement in a non-string case, and the two must not be conflated. Closing the
clause requires ADDING a case that exercises non-string native leaves heavily enough to be a probe,
sized to §5.2's 3–10 s. Until such a case exists and runs, this leg of §5.2 is unmet.

##### The case that is a probe

`selfhost-fib` — the native compiler, built at each stage of the lattice, compiling
`Bench.Fib.Main --no-opt --emit-llvm`. It reproduces this ADR's whole endpoint structure at roughly a
fifth of the pinned CLI corpus's scale, with every off-axis counter pinned:

| leg | the counter that moves | before | after | pinned INVARIANT |
| --- | --- | ---: | ---: | --- |
| A `s0→s1` | `gc_copied_words` | 11,607,269 | 5,036,929 | `pv_apply_entries`, `pv_tailcall_writes` |
| B `s1→s2` | `pv_apply_entries` | 93,336,993 | 29,545,469 | `pv_tailcall_writes`, `gc_copied_words` |
| C `s2→s3` | `pv_tailcall_writes` | 19,342,702 | 18,023,899 | `pv_apply_entries`, `gc_copied_words` |

and — the precondition without which no timing between stages means anything — **all four stage
binaries emit the workload byte-identically** (`diff -r`, verdicted per stage, not assumed). The knob
is a BUILD-mode axis, so the four compilers must do identical work; this ADR's first paired run
failed precisely because they did not.

The case runs at 7.7–8.1 s, **inside** §5.2's 3–10 s window — and the report prints the wall times so
that is checkable per run rather than assumed from the case table. The distinction is not academic:
the same case measured ~17 s/run on a loaded machine, so the window is a property of the RUN and not
only of the case. `gc_total_ns` is deliberately not a gate: two runs agreeing on `gc_copied_words` to
the word read 0.71 s and 1.85 s.

##### Results

n = 20 paired reps per set, order alternated, ratio = t(after)/t(before); floor = the union of the
two endpoints' A/A [p5, p95] by nearest rank. **All three axes passed the regime check** (the A/B
set's wall median lay inside the span its A/A sets observed).

| leg | A/B median | floor | verdict |
| --- | ---: | --- | --- |
| A — closure `s0→s1` | 0.9454 | `[0.9677, 1.0198]` | **PROVISIONAL WIN** |
| B — apply `s1→s2` | 0.9700 | `[0.9769, 1.0145]` | **PROVISIONAL WIN** |
| C — tail `s2→s3` | 0.9818 | `[0.9722, 1.0195]` | **INCONCLUSIVE** |

**No arithmetic is performed across slices.** The three legs are not summed, not compounded, and one
leg's result never offsets another's — which is why the knob has three call stages rather than two
(§5.2), and the rule is the same one that would have made a slice B regression non-absorbable by
slice A's win.

**Slice C is INCONCLUSIVE, and that is the word it is reported in.** It is not "no regression", it is
not "≈ 1.8 % faster", and its median's proximity to the floor's lower edge is not a partial result.
§5.3 makes INCONCLUSIVE a distinct verdict precisely so it cannot be rounded toward either neighbour.

**Slice B's earlier INCONCLUSIVE was a fact about the box, not about slice B** — and the two runs say
so mechanically rather than by interpretation. The median barely moved (0.9718 → 0.9700); the floor
moved from `[0.8399, 1.0548]` to `[0.9769, 1.0145]`. A floor ±16 % wide cannot resolve a ~3 % effect,
so the first run's null was a statement about resolution. This is recorded because the opposite
reading — "slice B did nothing, then did something" — is the available misreading.

##### The withdrawn run, kept as harness evidence

A first protocol run's tail axis produced a confident INCONCLUSIVE that was **withdrawn, not
reported**. Its A/A sets were taken at ~7.6 s/run; then, from the eighth of the twenty A/B pairs
onward, the machine changed state and every remaining pair ran at ~13.5 s (A/B wall range
7.39–14.55 s against an A/A span
of 7.41–8.66 s). The floor described a machine state the comparison never ran in.

What makes it worth recording is that **nothing in the ratio statistics could have shown it**. Pairs
are measured back-to-back, so a level shift between pairs cancels in the ratio — and it did, to a
degree that is itself the evidence: the withdrawn run's tail median was **0.981783** and the clean
re-run's was **0.981784**, from disjoint samples taken at half the wall-clock speed. The pairing
discipline was working exactly as §5.2 intends.

**The lesson pinned in the harness: paired-ratio validity and floor validity are separate contracts,
and satisfying the first says nothing about the second.** `foreign-ab.sh` now requires the A/B set's
wall median to lie inside the span its A/A sets observed — no constant, only observed data — and
withholds the verdict as `NO-FLOOR` when it does not. The check is exercised by four `--self-test`
rows, including the real drift's shape and a drift in the FASTER direction.

##### Owed, as two separate items

1. **The §5.2 run**: the A/A intervals on both endpoints and the A/B verdict, per slice, with
   `perf stat`, on a quiet dedicated Linux box. The three verdicts above are the input that run
   should expect to confirm or contradict; they do not stand in for it.
2. **A non-string-substrate probe**: a case exercising non-string native leaves heavily enough to
   pass the relevance gate, sized to 3–10 s, so §5.2's corpus clause can be closed by measurement.

Neither item is discharged by the other, and neither is discharged by this section.

## Consequences

- **Emission diverges intentionally**, in all three slices, and the divergence is declared under the
  ADR-0104 §4 golden classes: the affected goldens are re-baselined as an intentional change, the
  L2↔L3 stage fixpoint must still hold (stage 3 ≡ stage 4, byte-identical), and the behavioural gate
  (`tools/l2-native-behavioural.sh`, 8 legs), `native-run-diff`, `ffi-e2e`, the examples sweep and
  the bench oracle are the correctness anchors — not byte identity against the previous emission.
- **The `.ll` shrinks**, and by more than the removed lines: slice A removes a safepoint per foreign
  reference, so the ADR-0105 plan roots less around it. Reported per object, the ADR-0107 way, and
  not confused with a time claim.
- **A new init tier exists** (§2.2-amended): ONE function, `@pv_fclo_init`, in the entry object,
  called from `pv_init_all` before the gdef inits. Its content is the reachable-leaf set, so the
  link contract is exactly the one the tree satisfied before this ADR.
- **Miri is unaffected**: the direct call exists only in emitted LLVM (the address path). The
  index-path runtime (`code_is_address() == false`) never sees it, so the island discipline and its
  Miri tests are untouched.
- **The runtime leaf surface is untouched** — no new `pvf_` symbols, no signature change, no new
  `pv_*` entry point. The runtime-side additions are the step-5 `Kind::Closure` counter, the
  materialisation counter (§5.1), and the §3 clause-1 checks.
- **User C FFI keeps working unchanged** (ADR-0091), which is the whole point of §2.1's rejection of
  the unit sentinel. The `purvasm.h` change is an added guarantee, not a removed one.

## Alternatives considered

- **Pass the unit immediate as the leaf's `closure`.** Cheapest possible. Rejected: it turns a
  documented capability of a published C header into an abort inside third-party code, to save a
  rooted-global read on a path that is losing a heap allocation. Revisit only behind an ABI version
  bump, and only if §5 measures the read.
- **One external-linkage cell per key, owned by the defining module's object.** Rejected in §2.2:
  fewer closures, but it makes the owner object's existence load-bearing, needs foreign keys threaded
  into shared `fvExpr` reachability, and trades one init-order dependency for another.
- **Keep `pv_apply` and only hoist the closure (slice A alone).** Not rejected — it is the first
  slice, and if §5's time legs come back INCONCLUSIVE it may be all that survives on evidence. It is
  not *sufficient* as an answer to ADR-0108, because it leaves 425 M dispatches in place.
- **Re-inspect the callee atom at the `CApp` arm instead of extending the classifier's result type.**
  Rejected in §1: it is a second classifier, and ADR-0108's census is defined as consuming the
  emitter's classification rather than re-deriving it.
- **Cache the leaf closure per activation** (build it once at function entry). Rejected: it
  re-introduces an allocation per activation of every function that calls a leaf — on the hot string
  paths, the same order as per call — and needs a rooting slot for the whole activation.
- **Special-case the four dominant symbols.** Rejected: the classifier generalises at no extra cost
  (100 % of the population is `known-match`), and a four-symbol table would be a second source of
  truth about arity beside `Ctx.foreignArity` — the drift class ADR-0107 §2 forbids.
- **Do nothing and go after `local-unknown-fn` instead** (26.1 %, 48.3 % of sites). Rejected as the
  *first* move: it is the larger design problem (higher-order calls with no function fact at all)
  and the smaller measured share. It stays next in the queue.
- **Infer the allocation saving instead of measuring it.** Rejected explicitly: it is the step this
  project's method does not skip, and ADR-0108 §5 already owns the instrument.
