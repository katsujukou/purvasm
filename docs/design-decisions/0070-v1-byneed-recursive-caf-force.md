# 0070. v1 by-need recursive CAFs

- Status: Accepted
- Date: 2026-07-02

## Abstract

v1 by-need recursive CAFs: the `ByNeed` cell `[state][result]` as a 3-state (`Unforced`/`Building`/`Forced`) memoising suspension, `force` = black-hole-then-`apply(thunk,unit)`-then-memoise (cell rooted across the safepoint), `apply` auto-forces a `ByNeed` callee, `Grec` groups over a shared env; builds on the ADR-0069 record store (a field store does not force), unblocks the recursive `Monad Effect` dictionary (ADR-0067 §2 prerequisite)

## Context

A compiled program that uses the `Effect` monad threads the **recursive `Monad Effect` instance
dictionary**, and [0032](0032-vm-native-foreign-effect.md) established that this group **cannot be
built eagerly**: `ap monadEffect` saturates and forces a *sibling* (`applicativeEffect`) during the
group's own construction, so a strict spine-order build leaves the sibling unbound.
[0024](0024-by-need-recursive-bindings.md) settled the fix at the oracle: recursive (`letrec`) bindings
are **by-need** — each slot holds a suspension of its RHS closed over the whole recursive environment;
the first dereference forces it (black-holing the slot, evaluating, memoising), a dereference *while
forcing* is a genuine cycle and stays stuck.

[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2 already reserves the heap kind —
`ByNeed = [state: raw][result: value]` — and the collector scans its `result` slot. But the **force
operation is unimplemented**: there is no `new_byneed`, no state machine, no black-hole. So the runtime
cannot yet build a recursive CAF group, which [0067](0067-v1-effect-execution-and-native-leaves.md) §2
named as the **prerequisite** for running a full compiled `Effect` program. This record implements the
[0024](0024-by-need-recursive-bindings.md) mechanism concretely on the v1 `ByNeed` cell.

## Decision

### 1. `ByNeed` cell: a three-state memoising suspension

The cell `[state: raw][result: value]` (§2 layout) is a small state machine on the raw `state` word:

- **`Unforced` (0)** — `result` holds the **suspension**: an arity-1 **thunk closure** (`\_ -> rhs`)
  whose env captures the recursive group (ADR-0059 §1 shared env block). Forcing runs the RHS.
- **`Building` (1)** — the black-hole: the suspension is being forced right now.
- **`Forced` (2)** — `result` holds the **memoised value**; `state` never changes again.

`result` is a value slot in **every** state (a `Closure` pointer, then the forced value), so the
collector traces it uniformly and the suspension stays reachable while it runs. `new_byneed(thunk)`
allocates the cell `Unforced` with `result = thunk` (self-rooting the `thunk` input, ADR-0066 §3).

### 2. `force` — the operation

`force` is a safe public API, so it **release-validates its input first** (as the `Str`/record readers
do, ADR-0067/0069): release-`assert` `cell.kind == ByNeed` and `state ∈ {Unforced, Building, Forced}`
before dispatch — the raw `state` word is writable via the public `write_raw`, and an undefined value
must fault here, not fall through to a bogus branch. Then `force(cell)` dispatches on `state`:

- **`Forced`** → return `result` (O(1); the memoised value / the CAF's effect already ran once).
- **`Building`** → **black-hole**: forcing a cell mid-force is a genuine cycle (`letrec x = x`,
  self-referential values with no intervening thunk) — a fatal runtime fault
  ([0024](0024-by-need-recursive-bindings.md); the oracle's *stuck*).
- **`Unforced`** → set `state = Building`, read the suspension from `result`, evaluate it via
  `v = apply(thunk, [unit])` (§4 — forcing a nullary thunk reuses the eval/apply path, exactly like an
  `Effect` thunk, [0067](0067-v1-effect-execution-and-native-leaves.md)), then set `result = v`,
  `state = Forced`, and return `v`.

The `apply` is a **safepoint**, so `force` **roots the cell across it and reloads it before the memoise
write** (ADR-0066 §3) — the suspension itself stays reachable through the rooted cell's `result` slot
while its own code runs, and the members it references are reachable through that suspension's env.

### 3. `apply` auto-forces a `ByNeed` callee

A by-need binding used *as a function* (a dictionary method projected then called) reaches `apply` in
callee position. `apply` therefore, on a `ByNeed` callee, **forces it and re-dispatches** on the
forced value — so a by-need value is transparently callable. (A by-need value read for its *fields* —
a dictionary projected for a method — is forced by a `force` the codegen emits at the projection, since
a generic `read_field` does not know a slot is by-need; §5. `apply`'s auto-force covers the
callee-position case; the projection case is explicit.)

### 4. Recursive-group (`Grec`) construction

**The `env` block holds each member's `ByNeed` *cell*, not its suspension closure.** This is
load-bearing: a sibling dereference must reach the memoising/black-holing **cell** (so it forces once
and detects cycles) — if `env` held the raw suspension closure, a sibling would re-run it and bypass
memoisation. So `env[i] = cell_i`, and `cell_i.result = suspension_i` (a thunk that closes over the
*same* `env`, hence over every sibling cell).

Because every step allocates and `alloc` is a safepoint (ADR-0066 §4), the whole graph must stay rooted
while it is being wired — a cell, an env, or a thunk collected before it is mutually reachable would be
a use-after-free. The knot is inherent — a `suspension_i` captures `env`, which holds the cells, which
hold the suspensions — so a cell must exist *before* its suspension does. The public
`new_byneed(suspension)` (§1) always takes a real suspension and cannot express that; the `Grec`
builder therefore uses a **builder-internal placeholder-cell constructor** — a cell allocated
`Unforced` with a placeholder `result`, backpatched with its real suspension in step 3 — kept separate
from the public API so `new_byneed`'s "born with its suspension" invariant stays clean. The
construction is, explicitly:

1. Open a root frame; allocate the shared `env` `Array` (size = group size) with every slot the `unit`
   sentinel (a placeholder), and **root `env`**.
2. For each member `i`: allocate `cell_i` via the builder-internal placeholder constructor (`Unforced`,
   `result` = `unit` placeholder) and **root `cell_i`**; write `env[i] = cell_i` (a value-slot store, no
   force). (`env` is rooted, so a collection here relocates it and the already-written cells; reload
   from the roots.)
3. For each member `i`: build `suspension_i` — a thunk closure capturing (the now-complete) `env` — and
   backpatch it into `cell_i.result` (a plain value-slot store; `state` stays `Unforced`). Storing the
   suspension does **not** force it.
4. Close the frame (pop all roots); the group is reachable from wherever `env`/its cells were installed.

First force of any member runs its suspension; a forced *sibling* whose own construction merely
**stores** a back-reference into a record field completes without black-holing — this rests on record
construction being a plain value store that does **not** force what it stores
([0069](0069-v1-dynamic-record-operations.md) `new_record`/`record_set`), so `applicativeEffect`'s
fields (`pureE` + a superclass *thunk* — a stored `ByNeed`, not a forced read; the
[0032](0032-vm-native-foreign-effect.md) knot-tying observation) go in without forcing back. It then
memoises. Only a member that dereferences *itself* mid-force black-holes. Non-recursive CAFs stay strict
(built eagerly, no `ByNeed`).

### 5. Scope and validation

- v1 exercises this with **hand-built recursive CAF groups** shaped like the `Monad Effect`
  dictionary (mutually-referential cells whose forcing crosses siblings), asserting: memoisation
  (force twice → one evaluation, one effect), the knot-tying group completes, and a genuine self-cycle
  black-holes. Codegen later emits `new_byneed` for `Grec` groups and `force` at by-need dereference
  points.
- **Forced-GC coverage** (ADR-0066): a group whose forcing collects mid-`force` must still resolve to
  the same values — the cell-rooting contract under real recursion.
- **Miri** (ADR-0063 §4) on the force paths (state transitions, black-hole, memoise).
- This unblocks the [0067](0067-v1-effect-execution-and-native-leaves.md) §2 gate: with `ByNeed`-force
  in place, a compiled program threading the `Monad Effect` dictionary can run (once codegen lands).

## Consequences

- Recursive dictionaries — `Monad Effect` and any `ap`/`liftA1`-based instance — build on demand and
  memoise; the [0067](0067-v1-effect-execution-and-native-leaves.md) §2 "full compiled program"
  prerequisite is met on the runtime side.
- One `apply` (the suspension) + one memoise write per member on first force; O(1) thereafter — the
  [0024](0024-by-need-recursive-bindings.md) cost, on the v1 heap.
- Genuine cycles still fault (black-hole), preserving the oracle's *stuck* semantics.
- By-need is confined to `ByNeed` cells (recursive groups); ordinary evaluation stays strict
  (ADR-0002/0024) — no general laziness.
- Forward path unchanged: once `DictElim` eliminates the `Effect` group (ADR-0024 Consequences,
  ADR-0034), most of these cells never exist; `ByNeed` remains the always-correct baseline for the
  groups that do.

## Alternatives considered

- **Store the suspension outside the cell (a side thunk table).** Needless indirection: the `result`
  slot already holds a value the collector traces, and a `Closure` pointer *is* a value — the cell
  holds its own suspension, then its own result.
- **A dedicated black-hole sentinel value in `result`** (instead of a `state` word). Pollutes the value
  space with a bottom and forces every consumer to test for it; the explicit `state` word keeps the
  black-hole local to `force` (ADR-0024's "an `undefined`/bottom placeholder" rejection).
- **Auto-force `ByNeed` in `read_field`** (make every field read force). Wrong layer: only *by-need
  dereference* sites force, and `read_field` is the generic accessor used for all kinds; forcing there
  would force raw structure reads and couple the collector-facing API to evaluation. Codegen emits
  `force` precisely at by-need dereferences; `apply` auto-forces only the callee position.
- **Eager topological construction / knot-tying without by-need** (purs-wasm's route). Insufficient for
  the curried, un-optimised program the runtime receives before `DictElim`: the eager dependency is
  dynamic (inside `ap`'s body), exactly the [0024](0024-by-need-recursive-bindings.md) analysis — it
  becomes viable only post-`DictElim`, which the runtime does not assume.
