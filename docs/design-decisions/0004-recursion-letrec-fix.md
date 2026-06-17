# 0004. Recursion via `letrec` and store-backpatching

- Status: Accepted
- Date: 2026-06-17

## Context

The CESK core ([0002](0002-cesk-execution-model.md), Accepted) deliberately
omitted recursion. PureScript programs are full of recursive bindings (every
`where`-defined helper, every recursive `let`, mutually recursive groups), so
the core cannot stay non-recursive. CoreFn already delivers recursion as
explicit binding groups (`Rec`/`NonRec`), not as a fixpoint combinator, so the
machine needs a binder that can refer to itself, not a clever encoding.

Two properties of [0002](0002-cesk-execution-model.md) make this the decisive
slice:

1. The `name → address → value` indirection (`ρ` then `σ`) was kept "even though
   it looks redundant for a pure language" *precisely* so a name can be bound to
   an address before the value at that address exists.
2. [0002](0002-cesk-execution-model.md) flagged recursion as "where cycles first
   appear" and named the **Store as the GC seam**. Recursion is the slice that
   turns that abstract claim into a concrete cyclic object graph the future
   collector must handle.

PureScript is strictly evaluated, so this is *strict* recursion: a recursive
binding is well-defined only when its right-hand side is a value form (in
practice a `λ`) that does not force its own value while being constructed. This
matches OCaml's `let rec` value-RHS restriction and rules out `letrec x = x`.

## Decision

Add a single recursive binder to the core and realize it by
**store-backpatching** (Landin's knot): reserve the binding's address first, bind
the name to it, evaluate the right-hand side in the already-extended environment,
then **overwrite** that address with the resulting value. The closure built by
the right-hand side captures an environment that already points at the
address, so backpatching ties the knot.

### Surface additions

- A term `Letrec(x, e₁, e₂)` — bind `x` (recursively visible in `e₁`) to `e₁`,
  then evaluate `e₂`. The n-ary / mutually recursive group is a generalization
  (see Consequences); the core starts with the single-binding form.
- A store sentinel `⊥` — an **uninitialized ("black-hole")** slot, the value an
  address holds between its reservation and its backpatch.
- A continuation frame `Letrec_bind(α, e₂, ρ, κ)` — remembers the reserved
  address `α`, the body `e₂`, the recursive environment `ρ`, and the rest `κ`.

### Transition rules

Notation is as in [0002](0002-cesk-execution-model.md): state `⟨focus, σ, κ⟩`,
`α` a fresh address, `σ[α ↦ v]` updates the store at `α`, `ρ[x ↦ α]` extends the
environment. `⊥` is the uninitialized slot.

**Eval mode** — one new rule:

| `t` | next state |
| - | - |
| `Letrec(x, e₁, e₂)` | `⟨Eval(e₁, ρ[x ↦ α]), σ[α ↦ ⊥], Letrec_bind(α, e₂, ρ[x ↦ α], κ)⟩` (`α` fresh) |

`x` is in scope inside `e₁` (it maps to `α`), so a `λ` right-hand side captures
the recursive environment.

**Return mode** — one new rule:

| `κ` | condition | next state |
| - | - | - |
| `Letrec_bind(α, e₂, ρ, κ′)` | | `⟨Eval(e₂, ρ), σ[α ↦ v], κ′⟩` — **backpatch `α`** |

The `σ[α ↦ v]` here **overwrites** the slot reserved by the `Letrec` rule. This
is the machine's **first in-place store write**: every prior `σ[α ↦ v]`
(`Arg`, `Let_body`) wrote to a *fresh* `α` from `alloc`; this one updates an
*existing* `α`.

**Refinement to the `Var` rule.** Looking up an address may now find `⊥`:

| `t` | condition | next state |
| - | - | - |
| `Var x` | `σ(ρ(x)) = ⊥` | **stuck** — recursive binding used before it is initialized (black-hole) |
| `Var x` | otherwise | `⟨Return σ(ρ(x)), σ, κ⟩` (unchanged) |

### Why the knot ties (single-binding `λ` case)

For `Letrec(f, Lam(n, body), e₂)`:

1. `Letrec` reserves `α`, sets `σ[α ↦ ⊥]`, and evaluates the `λ` in `ρ[f ↦ α]`.
2. `Eval(Lam …)` immediately returns `VClosure{n, body, ρ[f ↦ α]}` — it captures
   the environment but **does not read `σ(α)`**, so the `⊥` is never observed.
3. `Letrec_bind` backpatches `σ[α ↦ VClosure{…, ρ[f ↦ α]}]`.

Now the closure's captured environment maps `f ↦ α` and `σ(α)` *is* that very
closure: `closure → env → α → (σ) → closure`. A later self-call evaluates
`Var f` to `σ(α)` = the closure (no longer `⊥`). The cycle is the point — it is
the first cyclic object in the store.

## Consequences

- **The store is no longer append-only.** It gains an in-place update at an
  existing address (backpatch), distinct from `alloc`. The Store interface
  (`empty`/`alloc`/`find`) grows a fourth operation — `set`/`update` `α ↦ v` on
  an already-reserved address — kept behind the same module so the GC seam stays
  intact.
- **First cycles appear**, validating [0002](0002-cesk-execution-model.md)'s
  GC-seam rationale. For a future copying collector the reserved address must be
  a real heap cell *before* it is filled, so the closure can point at it; the
  backpatch site is where the collector's invariants (and later, write barriers)
  first matter.
- **A `⊥` representation is required.** Either the store slot becomes
  "value-or-uninitialized" (e.g. a dedicated `VBlackhole` / an `option`), or the
  uniform heap word of phase 1b reserves a black-hole tag. The encoding is an
  implementation detail deferred to the value-representation work, but recursion
  forces it onto the agenda.
- **The allocating rules are now three** — `Arg`, `Let_body`, and `Letrec` (the
  placeholder) — and there is exactly **one mutating rule**, `Letrec_bind`. A
  trace's store still grows only at applications, `let`, and `letrec`; only
  `letrec` writes an address twice.
- **Strictness is enforced dynamically** by the `⊥`→stuck rule: a non-value RHS
  that forces its own binding (`letrec x = x`, `letrec x = x + 1`) gets stuck.
  This can later be upgraded to a *static* well-formedness check on binding
  groups (as OCaml does for `let rec`), turning a stuck state into a compile
  error; the dynamic rule is the executable-spec stand-in until then.
- **`fix` needs no separate machinery.** A fixpoint over a function value is
  `letrec f = g f in f`'s value form, i.e. expressible once `letrec` exists, so
  `fix` (if exposed at all) lowers to `letrec` rather than adding a rule.
- **Bytecode impact (forward-looking).** Backpatching compiles to: allocate a
  cell, push its address into the environment frame, build the closure, then
  `STORE`/`SETREC` into the reserved slot — a small, ordinary instruction
  sequence; nothing here complicates the stack-machine lowering of
  [0003](0003-stack-based-bytecode.md).

## Alternatives considered

- **`fix` as a primitive** (`fix : (a → a) → a` with a dedicated rule). Rejected
  as the *primary* mechanism: it is less general than `letrec` (no mutual
  recursion, no multi-binding groups), does not match CoreFn's explicit
  recursive binding groups, and is subsumed by `letrec` anyway. May still be
  *exposed* as a library function lowering to `letrec`.
- **Reference-cell / thunk indirection** — bind the name to a fresh mutable ref,
  have the closure read the ref, fill the ref after building the closure.
  Rejected: it re-introduces, as a separate `ref` value type, exactly the
  `name → address → value` indirection the Store *already* provides; the store
  backpatch is the same idea without a second indirection.
- **Cyclic environments via the host** (OCaml `let rec` / laziness to build a
  self-referential closure directly). Rejected: it hides the cycle inside the
  host's heap, defeating the explicit-heap/GC-seam purpose and producing
  semantics that cannot be carried to the phase-2 native runtime or the phase-3
  self-host, which own their memory.
- **Y/Z-combinator encoding** (no machine support; recursion as a fixpoint
  combinator). Rejected: needs laziness or heavy self-application in a strict
  setting, performs poorly, and does not reflect the recursive-binding-group IR
  the CoreFn frontend will actually emit.
