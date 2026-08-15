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
  no-capture closure for `k` becomes a root-handle global built once per object at init, and `atom`
  reads it. This changes *every* foreign reference (call position and value position alike), removes
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
Kind::Closure allocations(before) − Kind::Closure allocations(after)
  ==  AtomForeign materialisation executions(before)  −  hoisted fclo init executions(after)
hoisted fclo init executions(after)  ==  Σ over objects of |foreigns|      (static, × 1 each)
```

The three-way identity is the review's correction, and it matters twice over. First, **slice A does
not remove all the closures**: it allocates one per (object × key) at init, so the reduction is the
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

## Consequences

- **Emission diverges intentionally**, in all three slices, and the divergence is declared under the
  ADR-0104 §4 golden classes: the affected goldens are re-baselined as an intentional change, the
  L2↔L3 stage fixpoint must still hold (stage 3 ≡ stage 4, byte-identical), and the behavioural gate
  (`tools/l2-native-behavioural.sh`, 8 legs), `native-run-diff`, `ffi-e2e`, the examples sweep and
  the bench oracle are the correctness anchors — not byte identity against the previous emission.
- **The `.ll` shrinks**, and by more than the removed lines: slice A removes a safepoint per foreign
  reference, so the ADR-0105 plan roots less around it. Reported per object, the ADR-0107 way, and
  not confused with a time claim.
- **A new init tier exists** (§2.2c): one unconditional function per object, called from
  `pv_init_all` before the gdef inits. It is emitted even when empty, which is the price of the
  entry not needing to know each object's foreign set.
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
