# 0012. Array and record binders

- Status: Accepted
- Date: 2026-06-18

## Context

[0011](0011-adt-pattern-matching.md) (Accepted) added `case` with the *scalar*
binders — wildcard, variable, scalar literal, (nested) constructor, as-pattern —
and explicitly deferred **array- and record-literal binders** as one of its two
orthogonal axes, predicting that they would be "structurally cheap to add to the
matcher later … new arms in `Pmatch`". This record cashes that prediction.

CoreFn's `Binder` has a `LiteralBinder (Literal (Binder a))` case, and that inner
`Literal` includes `ArrayLiteral [Binder]` and `ObjectLiteral [(PSString,
Binder)]`. So matching `[a, b]` against an array and `{ x, y }` against a record
are exactly the structured-value counterparts of the scalar `BLit` we already
have — over the two structured values the machine already represents:
`Value.VArray` ([0009](0009-array-immutable-host-backed.md)) and `Value.VRecord`
([0010](0010-record-row-polymorphic.md)). After this record, every CoreFn
`Binder` form is covered; only the guard axis
([0013](0013-case-guards.md) — Proposed) remains.

Adding the **record** binder forces a question [0011](0011-adt-pattern-matching.md)
left implicit. A record pattern `{ name }` names a field whose *presence is part
of the scrutinee's row type*, so a well-typed value always has it — but what does
the matcher do if it does **not** (reachable only by `unsafeCoerce` or a lowering
bug)? [0011](0011-adt-pattern-matching.md)'s matcher answered every off-shape
pairing with a silent `None` (its `any other pairing → never` catch-all). This
record settles that contract deliberately, and the answer **refines 0011's
catch-all** as well as defining the new binders.

The matcher is still the only thing that changes — no new continuation frame, no
change to `Case`/dispatch, no evaluation — consistent with 0011's "`Pmatch` is
the seam for the deferred axes".

## Decision

### The two new binders

1. **`BArray of binder list`** — matches a `VArray` of **exactly** the same
   length, element-wise. PureScript array patterns are fixed-length: `[a, b]`
   matches only a two-element array. A length mismatch is an ordinary match
   failure (next alternative), because **array length is a runtime property, not
   part of the type** (`[a,b]` and a three-element array share the type
   `Array t`). There is **no rest/spread binder** (`[x, ...xs]`): PureScript core
   has no such syntax — head/tail decomposition is library code (`Array.uncons`),
   not a binder — so none is added.

2. **`BRecord of (string * binder) list`** — matches a `VRecord` that contains
   **at least** the named labels, matching each named field's sub-binder against
   that field's value. The pattern names a **subset** of fields: row polymorphism
   ([0010](0010-record-row-polymorphic.md)) means the value may carry more fields
   than the pattern mentions, and those extra fields are ignored.

Both compose with every existing binder: `[Just x, _]`, `{ p: Cons h t }`,
`Box x@{ tag: 0 }` — nesting is just the matcher recursing, as for `BCtor`.

### The matcher's `None`-vs-`stuck` contract (refining ADR-0011)

The matcher distinguishes two kinds of "does not match":

- **A legitimate, well-typed value-level non-match** — one a correct program can
  actually produce and that `case` exists to discriminate — yields **`None`**
  (fall through to the next alternative). These are exactly:
  - a constructor binder against `VData` of a **different tag** (sum
    discrimination);
  - a scalar-literal binder against a **same-typed but unequal** scalar
    (including a `NaN` literal vs a `NaN` value — IEEE, [0008](0008-number-ieee754-double.md));
  - an array binder against a `VArray` of a **different length**.

- **A type-impossible shape mismatch** — a binder against a value of the wrong
  *kind*, which the type system rules out and only `unsafeCoerce` or a
  frontend/lowering bug could create — is **`stuck`**. These are:
  - a constructor / scalar-literal / array / record binder against a value that
    is not (respectively) data / that scalar type / an array / a record;
  - a **record binder against a `VRecord` missing one of its named labels**
    (presence is in the row type);
  - (defensively) a constructor binder against same-tag data of a different arity
    (a constructor's arity is fixed).

This makes the matcher consistent with the rest of the machine, where a situation
the type system excludes is `stuck`: `Accessor` on a missing field
([0010](0010-record-row-polymorphic.md)) and ill-typed primitives
([0007](0007-monomorphic-primitives.md) §4). It **refines [0011](0011-adt-pattern-matching.md)**:
that record's `any other pairing → never (None)` catch-all is replaced by this
contract, so a type-impossible pairing it used to swallow as `None` now stucks. A
dated Correction is recorded in [0011](0011-adt-pattern-matching.md).

`Pmatch` therefore returns `Some/None` over well-typed inputs and is `stuck` on
type-impossible shapes — the same status as `Prim.eval`
([0011](0011-adt-pattern-matching.md)'s "Why matching may be host recursion").

### Matching rules

The full single-binder table under this contract (extending
[0011](0011-adt-pattern-matching.md); `match` threads the accumulated bindings
and fails fast):

| binder | value `v` | result |
| - | - | - |
| `BArray [p₁ … pₙ]` | `VArray [\|w₁ … wₙ\|]` (same length), each `pᵢ` matches `wᵢ` | the sub-bindings |
| `BArray [p₁ … pₙ]` | `VArray` of length `≠ n` | `None` (legitimate) |
| `BArray …` | not a `VArray` | **stuck** |
| `BRecord [(l₁,p₁) …]` | `VRecord m`, every `lᵢ ∈ m`, each `pᵢ` matches `m[lᵢ]` | the sub-bindings |
| `BRecord [(l,_) …]` | `VRecord m`, some `lᵢ ∉ m` | **stuck** |
| `BRecord …` | not a `VRecord` | **stuck** |

Array elements are matched **left to right**; record fields in the pattern's
order (the value's field order is irrelevant — a `VRecord` is an unordered map,
[0010](0010-record-row-polymorphic.md)). Both remain pure: no subterm is
evaluated.

### Surface additions

- `Ast.binder`: `BArray of binder list`, `BRecord of (string * binder) list`.
- `Ast.binder_to_string`: render `[p₁, …]` and `{ l₁: p₁, … }`.
- `Pmatch`: the array/record arms above, **and** tightening the existing scalar
  arms so a wrong-*kind* value stucks (a `BLit (LInt _)` against a non-`VInt`, a
  `BCtor` against a non-`VData`) while same-type/different-value and
  different-tag stay `None`.

No change to `Value`, `Cont`, `Ast.term`, `Ast.alternative`, or the `Case`
transition rules — the entire change is in the matcher and the binder type.

## Consequences

- **All CoreFn binders are now covered.** Scalar (0011) + structured (this
  record) = `NullBinder`, `VarBinder`, `LiteralBinder` (scalar / array / object),
  `ConstructorBinder`, `NamedBinder`. The only matching feature still outstanding
  is guards ([0013](0013-case-guards.md) — Proposed), which live on the
  alternative, not the binder.
- **No machinery beyond the matcher.** No continuation frame, no dispatch change,
  no `Store` interaction during matching (bindings still allocate only when the
  chosen alternative's body runs, [0011](0011-adt-pattern-matching.md)). This is
  the concrete payoff of 0011's "`Pmatch` is the seam" design.
- **Well-typed programs are unaffected by the `stuck` tightening.** Every binder
  in a well-typed (non-`unsafeCoerce`) program meets a value of the kind its type
  guarantees, so the new `stuck` branches are unreachable; `None`-vs-`stuck` is
  observable only for `unsafeCoerce`d values or a lowering bug.
- **A wildcard does not rescue an `unsafeCoerce`d value that hits an earlier
  structural binder.** Given `case m of Just b -> b ; _ -> 0`, if `m` is
  `unsafeCoerce 42` (runtime `VInt 42`, not a `VData`), the first arm's
  `BCtor("Just", …)` is tested against a non-`VData` and **stucks** — control
  never reaches `_`, so the call does **not** return `0`. The JS backend, whose
  constructor pattern is a runtime tag test (`v instanceof Just`), would instead
  fall through to `_`. This divergence is **accepted**: it occurs only under
  `unsafeCoerce` (undefined behaviour — the programmer has opted out of the type
  guarantee, [CLAUDE.md] `unsafe` convention) or a real lowering bug, where a loud
  `stuck` is more useful than silently routing a mis-shaped value to the wrong
  arm (the programmer has opted out of the type guarantee — the `unsafe`
  convention in `CLAUDE.md`); and purvasm does not target JS bug-compatibility on
  UB ([0006](0006-string-utf8-char-int.md)).
- **Array patterns are exact-length; record patterns are subset/row-polymorphic.**
  Length is read O(1) from the `VArray` ([0009](0009-array-immutable-host-backed.md));
  field lookup is O(log n) on the map. A `{ name }` pattern matches any record
  with a `name` field regardless of what else it carries.
- **Bytecode impact (forward-looking).** An array binder lowers to a length check
  plus indexed loads into the element sub-binders; a record binder lowers to
  labelled field loads into the field sub-binders — both ordinary decision-tree
  nodes ([0011](0011-adt-pattern-matching.md)), no new control-flow shape. The
  `stuck`-on-type-impossible contract needs no runtime check in well-typed
  code, so it does not add nodes on the hot path.

## Alternatives considered

- **Lenient matcher: type-impossible shape mismatch is `None`, not `stuck`** (the
  original draft of this record, and [0011](0011-adt-pattern-matching.md)'s
  `any other pairing → never` catch-all). Under it the matcher stays a pure total
  `Some/None` function and a wildcard rescues `unsafeCoerce`d values (matching the
  JS backend). Rejected: it silently swallows the type-impossible case — including
  genuine lowering bugs during frontend bring-up — routing a mis-shaped value to
  whatever arm happens to accept it, and it is inconsistent with `Accessor`
  ([0010](0010-record-row-polymorphic.md)) and ill-typed primitives
  ([0007](0007-monomorphic-primitives.md) §4), which already `stuck`. The only
  behaviour it buys back is JS-compatible fall-through for `unsafeCoerce` (UB),
  which is not a goal ([0006](0006-string-utf8-char-int.md)).
- **Array rest/spread binder `[x, ...xs]`.** Rejected: not a PureScript core
  syntactic form. Cons/uncons-style decomposition is `Data.Array` library code
  over `unsafeIndex`/`length` ([0009](0009-array-immutable-host-backed.md)), not a
  binder; adding a rest binder would invent surface syntax the frontend never
  emits.
- **Exact-field record patterns** (the pattern must name *every* field). Rejected:
  it contradicts row polymorphism — a function matching `{ name }` must accept
  records with extra fields ([0010](0010-record-row-polymorphic.md)). The subset
  reading is the only one consistent with the record value's open-row semantics.
- **One shared "literal binder" arm dispatching on value shape** (scalar / array /
  record in one case). Rejected for the same reason as
  [0007](0007-monomorphic-primitives.md): each shape has its own structural rule
  (exact length vs subset labels vs scalar equality, the last IEEE-special for
  `Number`); separate arms are clearer than a shape switch.
- **Defer record binders, ship only array binders now.** Rejected: they are the
  same kind of decision (a structural arm over an existing structured value) and
  0011 deferred them as a single item; splitting them would fragment a cohesive
  slice without reducing risk — and the record arm is what forces the
  `None`-vs-`stuck` contract this record settles.
