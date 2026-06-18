# 0011. Algebraic data types and `case` pattern matching

- Status: Accepted
- Date: 2026-06-18

## Context

The CESK core now has literals, variables, lambda/application, `let`/`letrec`,
`if`, primitives, arrays, and records ([0002](0002-cesk-execution-model.md)–
[0010](0010-record-row-polymorphic.md)). The one feature still missing before a
CoreFn frontend is meaningful is **algebraic data types and pattern matching** —
`Maybe`, `Either`, lists, and every user `data` declaration, plus the `case`
expressions that take them apart. Records gave us *labelled product* values;
ADTs add *tagged sums*, and `case` is how the language discriminates them.

How CoreFn delivers these fixes most of the shape:

- A **data constructor** is `Constructor typeName ctorName [field]` — after type
  erasure it is a **curried function** of a fixed arity (one argument per field)
  that builds a tagged value. `Nothing` is arity 0; `Just` is arity 1; `Cons` is
  arity 2. Constructors are first-class: `map Just xs` passes `Just` as a
  function, so partial application must work.
- A **case** is `Case [scrutinee] [alternative]` — note the scrutinee is a
  *list*: PureScript's multi-way `case e1, e2 of …` matches several values at
  once without building a tuple. Each alternative carries one **binder** per
  scrutinee and a result, plus optional guards.
- A **binder** (CoreFn `Binder`) is one of: `NullBinder` (`_`), `VarBinder`
  (`x`), `LiteralBinder` (a literal pattern, including array/record literals),
  `ConstructorBinder` (`Ctor p₁ … pₙ`, possibly nested), `NamedBinder` (`x@p`).

Type erasure means the machine never needs the *type* name to match — a
constructor's **tag** (its name) plus arity is enough to discriminate and
destructure. This is the sum-type counterpart of [0007](0007-monomorphic-primitives.md):
no runtime type information, only a tag we compare.

This record fixes (a) how constructed data is represented and built, and (b) how
`case` evaluates and how a binder matches a value. Its scope is deliberately the
**scalar core** of matching; two orthogonal axes are split out (see *Scope*).

## Decision

### Scope of this slice

Included: tagged data values, curried constructors with partial application,
multi-scrutinee `case`, and the binders **wildcard / variable / scalar literal /
(nested) constructor / as-pattern**.

Deferred to follow-on records (the same way recursion was split
[0004](0004-recursion-letrec-fix.md)/[0005](0005-mutual-recursion-binding-groups.md)):

- **Guards** (`| cond ->` and pattern guards). Guards are an *orthogonal* axis —
  they sit on the alternative, not the binder — and they need a continuation
  frame (a guard is a term that is *evaluated*, and a false guard falls through
  to the next alternative). Folding that into this slice would mix two concerns.
- **Array- and record-literal binders** (`[a, b]`, `{x, y}`). These are the
  `LiteralBinder` cases over the structured values of
  [0009](0009-array-immutable-host-backed.md)/[0010](0010-record-row-polymorphic.md).
  They are structurally cheap to add to the matcher later but are not needed to
  exercise the sum-type machinery, which is the point of this slice.

### Value representation

1. **A fully-applied constructor is a tagged tuple:**
   `Value.VData of { tag : string; fields : Value.t array }`. The `tag` is the
   constructor name (the only thing matching compares); `fields` are its argument
   values in declaration order. A nullary constructor has `fields = [||]`.

2. **A partial constructor is `Value.VCtor of { tag; arity; args }`.** Because a
   CoreFn constructor is a curried function, an unsaturated application is a
   value in its own right — exactly like a closure that has not yet received all
   its arguments. `args` accumulates in **reverse** (cheap cons); when its length
   reaches `arity`, the next argument completes the data. This is what makes
   constructors first-class (`map Just xs`) without special-casing them at every
   call site.

### Surface additions

- `Ast.term`:
  - `Ctor of string * int` — a constructor referenced as a value (tag, arity).
  - `Case of term list * alternative list` — multi-scrutinee case analysis.
- `Ast.binder` (new type): `BNull` (`_`), `BVar of string`, `BLit of lit`,
  `BCtor of string * binder list`, `BNamed of string * binder`.
- `Ast.alternative` (new type): `{ binders : binder list; result : term }` — one
  binder per scrutinee. (No guard field in this slice.)
- `Cont.t`: `Case_scrut of Value.t list * Ast.term list * alternative list *
  Env.t * t` — the same accumulate-left-to-right-in-reverse frame as `Prim_args`
  / `Array_elems`, now also carrying the alternatives and the case's environment.
- A new module **`Pmatch`** holding the structural matcher (see below). It is
  placed in its own module because matching is a self-contained, value-only
  computation independent of the transition rules (cohesion, per the project's
  single-responsibility guidance).

### Transition rules

Notation as in [0002](0002-cesk-execution-model.md): state `⟨focus, σ, κ⟩`, `α`
a fresh address, `ρ` the environment, `rev` list reversal.

**Eval mode** — constructors and entering a case:

| `t` | next state |
| - | - |
| `Ctor(tag, 0)` | `⟨Return (VData{tag; [\|\|]}), σ, κ⟩` |
| `Ctor(tag, n)`, `n ≥ 1` | `⟨Return (VCtor{tag; n; []}), σ, κ⟩` |
| `Case([], alts)` | **dispatch** `alts` against `[]` (degenerate; see below) |
| `Case(e₁ :: rest, alts)` | `⟨Eval(e₁, ρ), σ, Case_scrut([], rest, alts, ρ, κ)⟩` |

**Return mode** — constructor application *extends the existing `Arg` rule*
([0002](0002-cesk-execution-model.md)); the closure and non-function cases are
unchanged:

| `κ` | condition | next state |
| - | - | - |
| `Arg(v_f, κ′)` | `v_f = VCtor{tag; n; as}`, `\|as\| + 1 < n` | `⟨Return (VCtor{tag; n; v :: as}), σ, κ′⟩` |
| `Arg(v_f, κ′)` | `v_f = VCtor{tag; n; as}`, `\|as\| + 1 = n` | `⟨Return (VData{tag; rev(v :: as)}), σ, κ′⟩` |

Unlike the closure case of `Arg`, **constructor application allocates nothing in
`σ`** — the argument value is stored inside the data, not bound to a name. This
matches array/record literals ([0009](0009-array-immutable-host-backed.md)/
[0010](0010-record-row-polymorphic.md)), which also build a container without
touching the store.

**Return mode** — evaluating the scrutinees left to right:

| `κ` | condition | next state |
| - | - | - |
| `Case_scrut(done, e :: rest, alts, ρ, κ′)` | | `⟨Eval(e, ρ), σ, Case_scrut(v :: done, rest, alts, ρ, κ′)⟩` |
| `Case_scrut(done, [], alts, ρ, κ′)` | | **dispatch** `alts` against `rev(v :: done)` |

**Dispatch** — given the scrutinee values `vs` (in source order), select the
first alternative whose binders all match, bind, and evaluate its result:

| condition | next state |
| - | - |
| `select(alts, vs) = Some(bs, body)` | `⟨Eval(body, ρ′), σ′, κ′⟩` where `ρ′`, `σ′` extend `ρ`, `σ` with one fresh `α` per binding in `bs` |
| `select(alts, vs) = None` | **stuck** — non-exhaustive case |

Each pattern binding `(x ↦ value)` allocates a fresh `α` and threads
`σ[α ↦ value]`, `ρ[x ↦ α]` — the **same** `name → address → value` indirection
every other binder uses ([0002](0002-cesk-execution-model.md)). So pattern
binding grows the store exactly like `let`/application, once per bound name.

**Matching** — `select` tries alternatives in order; for one alternative it
matches each binder against the corresponding value, threading the accumulated
bindings and failing fast. A single binder against a value `v`:

| binder | matches when | binds |
| - | - | - |
| `BNull` | always | nothing |
| `BVar x` | always | `x ↦ v` |
| `BNamed(x, p)` | `p` matches `v` | `x ↦ v`, plus `p`'s bindings |
| `BLit ℓ` | `v` equals `ℓ` | nothing |
| `BCtor(tag, ps)` | `v = VData{vtag; fields}`, `tag = vtag`, `\|ps\| = \|fields\|`, and each `pᵢ` matches `fieldᵢ` | the sub-bindings |
| any other pairing | never | — |

Literal equality follows the value's primitive: `Int`/`Bool`/`String` exact,
`Number` by IEEE `=` (so a `NaN` literal pattern never matches — consistent with
`Prim.eval EqNumber`, [0008](0008-number-ieee754-double.md)). A constructor
binder against data with a **different tag is an ordinary match failure** (fall
through to the next alternative), *not* a stuck state — that is exactly how a
`case` discriminates a sum. ~~"Stuck" is reserved for *no* alternative matching.~~

> **Correction (2026-06-18):** [0012](0012-array-record-binders.md) (Proposed)
> refines the "any other pairing → never" row and the struck sentence above.
> The matcher returns `None` only for a **legitimate, well-typed value-level
> non-match** (different constructor tag, unequal same-typed scalar, different
> array length); a **type-impossible shape mismatch** (a binder against a value
> of the wrong kind — e.g. a constructor binder against a non-`VData`, reachable
> only via `unsafeCoerce` or a lowering bug) is now **stuck**, consistent with
> `Accessor` ([0010](0010-record-row-polymorphic.md)) and ill-typed primitives
> ([0007](0007-monomorphic-primitives.md) §4). So "stuck" covers *both* no
> alternative matching *and* a type-impossible pairing. See
> [0012](0012-array-record-binders.md) for the full rule.

### Why matching may be host recursion

Matching is a **total structural function over already-evaluated values**: a
binder only decomposes an existing `VData`/scalar, it never evaluates a subterm
or forces a store slot. So the host recursion inside `Pmatch` hides no evaluation
step and respects [0002](0002-cesk-execution-model.md)'s "no host recursion
inside the evaluation of a subterm" — it is the same status as `Prim.eval`
computing on its already-computed arguments, or `Store.find` walking the store.
Only the *result* of the chosen alternative re-enters the machine as a normal
`Eval`. (Guards, deferred, are the part that *does* evaluate and therefore *will*
need a continuation frame — another reason to split them out.)

Non-reification is the *interpreter-side symptom* of a deeper property worth
naming, because it is what the phase-2 compiler will rely on:
**matching is side-effect-free structural inspection**. Every test it performs (a
tag comparison, a field projection, a literal equality) has no observable effect,
and there is no state to suspend mid-match; the *only* observable outcomes are
**which alternative is chosen** and **what it binds**. That property is precisely
what licenses a compiler to **reorder, share, and merge** those tests freely —
i.e. to lower a `case` to a **decision tree** (Maranget-style) or a backtracking
automaton — since any scheme that yields the same chosen-alternative-and-bindings
is observably identical. It is also exactly the freedom that guards would remove:
a guard evaluates a term in source order and can fall through, reintroducing an
observable evaluation sequence that constrains how tests may be reordered. The
clean "reorder at will" story therefore holds for *this* guard-free slice, and is
a third reason guards are deferred (see *Alternatives considered*).

## Consequences

- **First sum type; tag-only discrimination.** `VData` is the machine's first
  value that is matched on by *name*. Combined with `VRecord` (labelled product),
  the machine now has both halves of CoreFn's data model. No type information is
  consulted — only the tag — keeping [0007](0007-monomorphic-primitives.md)'s
  type-erasure invariant.
- **Constructors are closures' twin.** `VCtor` is to `VData` what a partially
  applied closure is to a saturated call: both grow through the `Arg` rule. The
  rule now branches on "closure vs constructor vs neither", but the shape is
  uniform. Constructor application is the one `Arg` path that does **not** touch
  the store.
- **A fourth left-to-right frame** (`Case_scrut`) joins `Prim_args`,
  `Array_elems`, `Record_fields`. Its accumulate-in-reverse core is identical;
  it additionally carries the alternatives. [0010](0010-record-row-polymorphic.md)
  already concluded these frames differ enough (labels vs not, now alternatives
  too) that a forced shared abstraction would obscure more than it saves — this
  fourth instance does not change that call.
- **`Pmatch` is the seam for the deferred axes.** Adding array/record literal
  binders is new arms in `Pmatch`; adding guards is a new continuation frame
  plus an alternative field — neither disturbs the rules above. The split keeps
  each follow-on small.
- **Non-exhaustive match is stuck**, like every other partiality in the core
  (unbound variable, non-function application, missing field). Well-typed CoreFn
  is exhaustive (the compiler inserts a failing default arm when a match can fail
  at runtime), so this `stuck` is the executable-spec stand-in until the frontend
  supplies that arm.
- **`Store` is untouched** structurally: pattern bindings use the existing
  `alloc`, and `VData`/`VCtor` are host containers for now (phase 1b moves them
  to heap blocks, like arrays and records).
- **CoreFn lowering will be direct:** a `Constructor` node → `Ctor(name, arity)`;
  a `Case` → `Case(scrutinees, alternatives)`; each CoreFn `Binder` → the
  matching `Ast.binder`. The two deferred axes are the only gaps, and both are
  flagged here.
- **Bytecode impact (forward-looking): `Pmatch` is the spec, a decision tree is
  the realization.** The phase-1a matcher is the *naive sequential* algorithm —
  try alternatives top to bottom, re-inspecting the scrutinee for each, first
  match wins — and that is deliberately the **executable specification** of what a
  `case` means (the chosen alternative and its bindings). It is **not** itself a
  decision tree. Phase-2 lowering compiles the same `case` to a **decision tree**
  that inspects each part of the scrutinee once and shares tests across
  alternatives; its only obligation is to agree with the spec's
  chosen-alternative-and-bindings. This is the **same spec/realization relation**
  as [0003](0003-stack-based-bytecode.md)'s "bytecode is a compiled realization of
  the CESK transitions" — here the linear matcher is the oracle and the decision
  tree is the optimized realization, justified by the purity property above.
  Concretely, `VData` is a tagged heap block and a decision-tree node is a tag
  dispatch (a switch on the tag) plus field loads into fresh locals for the
  bindings — ordinary structured control flow consistent with
  [0003](0003-stack-based-bytecode.md); nested binders are nested dispatch.
  Nothing here complicates the stack-machine lowering.

## Alternatives considered

- **Represent a constructor as a saturated term `Construct of string * term
  list`** (like `Array`/`Record` literals) instead of a curried `VCtor`.
  Rejected as the primary form: CoreFn emits constructors as first-class
  *functions* (`map Just xs`), so a saturated-only term cannot represent a
  partially applied constructor without eta-expanding every use into a lambda at
  lowering time. The `VCtor` accumulator models exactly what CoreFn hands us and
  handles saturated and partial uses uniformly through the `Arg` rule the machine
  already has.
- **Carry the type name in `VData`** (`{ typeName; tag; fields }`). Rejected:
  type-erased matching needs only the tag; storing the type name re-imports
  runtime type information the rest of the machine deliberately lacks
  ([0007](0007-monomorphic-primitives.md)). Tags are unique within a type, which
  is all `case` discriminates on.
- **Single-scrutinee `case` only** (match one value; encode multi-way matching
  by tupling). Rejected: CoreFn's `Case` is already multi-scrutinee, and tupling
  would force allocating a throwaway record/array purely to match it apart. The
  list-of-scrutinees form is the direct lowering and costs only a list in the
  frame.
- **Reify matching as machine transitions** (a `Match` focus that small-steps
  through binders). Rejected as unnecessary: matching evaluates nothing, so there
  is no continuation to save mid-match; a pure structural function is simpler and
  is consistent with how `Prim.eval` already does value-only work. (This would
  change *if* binders could run code — but the one binder form that does, guards,
  is deferred and will get its frame then.)
- **Fold guards into this slice.** Rejected for cohesion: guards evaluate a term
  and fall through on `false`, which needs a continuation frame and touches the
  dispatch loop, whereas binders are pure structure. Splitting mirrors
  [0004](0004-recursion-letrec-fix.md)/[0005](0005-mutual-recursion-binding-groups.md)
  and keeps each record reviewable.
- **One generic structural equality for literal binders** rather than per-type
  literal comparison. Rejected for the same reason as
  [0007](0007-monomorphic-primitives.md): equality is per-type (and `Number` is
  IEEE, not total); the matcher compares each scalar literal with its own
  primitive's semantics, not a blanket deep-equal.
