# 0005. Mutual recursion via recursive binding groups

- Status: Accepted
- Date: 2026-06-17

## Context

[0004](0004-recursion-letrec-fix.md) (Accepted) added a *single-binding*
`letrec` realized by store-backpatching, and explicitly noted that "the n-ary /
mutually recursive group is a generalization … the core starts with the
single-binding form." This record makes that generalization concrete.

The forcing function is the frontend: CoreFn delivers recursion as **binding
groups** — a `Rec` group is a set of bindings that may all refer to one another
(`f` calls `g`, `g` calls `f`), a `NonRec` binding stands alone. Mutual
recursion is therefore not an exotic case to bolt on later; it is the shape the
machine will actually be fed. A single-binding `letrec` cannot express it:
nesting `letrec f = … g … in letrec g = … f …` puts `f` out of scope in `g`'s
right-hand side (and vice versa), so the two simply cannot see each other.
Mutual recursion requires the names to share one scope.

## Decision

Generalize `letrec` from one binding to a **recursive binding group**: a list of
`(name, rhs)` pairs whose names are all in scope in every right-hand side *and*
in the body. The single-binding form of [0004](0004-recursion-letrec-fix.md)
becomes the one-element group; the store-backpatching mechanism is unchanged,
only widened from one address to *n*.

### Surface additions

- The term changes from `Letrec of string * term * term` to
  **`Letrec of (string * term) list * term`** — the binding group and the body.
- The continuation frame generalizes from
  `Letrec_bind of Addr.t * Ast.term * Env.t * t` to
  **`Letrec_bind of Addr.t * (Addr.t * Ast.term) list * Ast.term * Env.t * t`**:
  the address being filled now, the `(address, rhs)` pairs still to evaluate,
  the body, the recursive environment, and the rest `κ`.

The `Blackhole` (⊥) slot and the `Var`→stuck-on-⊥ rule are exactly as in
[0004](0004-recursion-letrec-fix.md); nothing new is needed there.

### Transition rules

Notation as in [0002](0002-cesk-execution-model.md) /
[0004](0004-recursion-letrec-fix.md). A group is `[(x₁,e₁), …, (xₙ,eₙ)]`. Fresh
addresses `α₁ … αₙ` are reserved in one burst; `ρ′ = ρ[x₁ ↦ α₁, …, xₙ ↦ αₙ]` is
the **recursive environment** in which *every* right-hand side and the body are
evaluated.

**Eval mode:**

| `t` | next state |
| - | - |
| `Letrec([(x₁,e₁), …, (xₙ,eₙ)], body)`, `n ≥ 1` | `⟨Eval(e₁, ρ′), σ[α₁ ↦ ⊥]…[αₙ ↦ ⊥], Letrec_bind(α₁, [(α₂,e₂), …, (αₙ,eₙ)], body, ρ′, κ)⟩` |
| `Letrec([], body)` | `⟨Eval(body, ρ), σ, κ⟩` (empty group — body in the unchanged env) |

All *n* addresses are reserved **before** any right-hand side runs, so a forward
reference (`e₁` mentioning `xₙ`) resolves to a reserved address rather than an
unbound name — this is what makes the bindings mutually visible.

**Return mode:**

| `κ` | condition | next state |
| - | - | - |
| `Letrec_bind(αᵢ, [], body, ρ′, κ′)` | | `⟨Eval(body, ρ′), σ[αᵢ ↦ v], κ′⟩` — backpatch the last binding, run the body |
| `Letrec_bind(αᵢ, (αⱼ,eⱼ) :: rest, body, ρ′, κ′)` | | `⟨Eval(eⱼ, ρ′), σ[αᵢ ↦ v], Letrec_bind(αⱼ, rest, body, ρ′, κ′)⟩` — backpatch `αᵢ`, evaluate the next right-hand side |

Each binding is backpatched **as soon as its value is known** (incremental),
then the next right-hand side is evaluated, still in `ρ′`. Because every
right-hand side runs in `ρ′`, a `λ` in any binding captures the whole group and
can call any sibling; the cycles among the closures are tied as the backpatches
land.

## Consequences

- **[0004](0004-recursion-letrec-fix.md) is the `n = 1` special case.** Its
  store-backpatching, `⊥` sentinel, and stuck rule carry over verbatim; this
  record only widens the address count and threads the remaining bindings through
  the `Letrec_bind` frame.
- **One allocation burst of `n`.** The reserve step grows the store by `n`
  black-holes at once (the only multi-allocation transition); each backpatch then
  overwrites in place, so a trace's store jumps by `n` at a `letrec` and never
  again for that group.
- **Forcing `⊥` still gets stuck**, now also catching mutual non-value cycles
  such as `letrec x = y and y = x` (each right-hand side reads the other's
  reserved-but-unfilled address). Groups of `λ`s — the intended, well-formed case
  — never read a `⊥`, exactly as in the single-binding case. This keeps the
  dynamic stand-in for OCaml's static `let rec` value-RHS restriction.
- **Evaluation order is left-to-right and incremental.** For a well-formed
  all-`λ` group this is unobservable (no right-hand side reads a sibling's slot
  while constructing its own closure). It only becomes visible for non-value
  right-hand sides, which are the stuck cases anyway, so the order is not part of
  the observable contract.
- **Duplicate names within a group are assumed absent.** CoreFn emits
  type-checked binding groups, so `letrec x = … and x = …` should not arise; the
  machine does not police it (a later well-formedness check could reject it at
  reserve time, turning a last-writer-wins `ρ′` into an explicit error).
- **The `Letrec_bind` frame grows with the group.** Its pending-bindings list is
  `O(n)`, which is the natural size of "a group still being tied"; it shrinks by
  one per backpatch. No change to the keystone property — the continuation is
  still first-order data.
- **CoreFn lowering is now direct.** A `Rec` group maps to one `Letrec(group,
  body)`; a `NonRec` maps to `Let` (or a singleton group). No dependency analysis
  or SCC ordering is required at this layer — that is an optional optimization for
  the bytecode lowering, not a correctness need here.

## Alternatives considered

- **Nested single-binding `letrec`s.** Rejected because it is not merely uglier
  but *incapable* of mutual recursion: nesting makes the scopes asymmetric, so two
  bindings cannot refer to each other. A shared scope is mandatory.
- **Two-pass: evaluate all right-hand sides, then backpatch them all at once**
  (instead of incremental backpatch). Equivalent for the well-formed all-`λ` case
  (no `⊥` is read either way) and only differs for non-value right-hand sides,
  which are stuck regardless. Incremental backpatching is the more natural
  small-step shape (one backpatch per `Return`) and is chosen for that reason.
- **Dependency analysis into `NonRec` + ordered evaluation** (split a `Rec` group
  into strongly-connected components, evaluate acyclic parts in order). This is a
  compiler optimization that can make more non-value bindings well-defined, but it
  belongs in the CoreFn→bytecode lowering, not in the executable-spec machine;
  CoreFn already hands us the `Rec`/`NonRec` distinction, so the machine stays
  simple.
