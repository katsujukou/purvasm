# 0009. Array as an immutable, host-backed vector

- Status: Accepted
- Date: 2026-06-17

## Context

`Array` is the next CoreFn value to add. PureScript's `Array` is an immutable,
densely-indexed sequence (backed by a JS array); its `Data.Array` API is
purely functional, while in-place mutation lives in a separate world
(`Data.Array.ST` / `Effect` with `STArray`/mutable arrays). Unlike `Record`
(deferred — its row-polymorphic dynamic field set needs its own ADR), `Array` is
structurally simple: an ordered vector of values with O(1) indexing.

This record fixes the representation, how literals are built, and the index/
length primitives. Mutation is explicitly out of scope here.

## Decision

1. **`Array` is `Value.t array` (the host OCaml `array`), treated as
   immutable.** Add `Value.VArray of Value.t array`. The core operations
   (literal construction, indexing, length) never mutate it, so although the
   host array has a mutable backing ~~we never use it; the value is read-only.~~
   In phase 1b's uniform heap, an array becomes a contiguous heap block — the
   `VArray` interface is the seam, like `Store` is for the heap.

   > **Correction (2026-06-19):** [0019](0019-mutable-array-building.md) (Proposed)
   > revises "we never use it." The mutable backing *is* used by two confined,
   > unsafe **array-building** primitives (`NewArray`/`SetArray`) so structural
   > higher-order functions can be written as guest code over first-order
   > primitives ([0020](0020-structural-ffi-guest-code.md)). The immutable value,
   > its literal form, and the read API (this record) are unchanged; mutation is
   > the local build-then-use (ST) discipline, not general mutable state.

2. **Array literals are a dedicated term, not a primitive.** Add
   `Ast.Array of term list` (e.g. `[a, b, c]`). Its elements are evaluated
   **left to right** via a new continuation frame
   `Array_elems of Value.t list * Ast.term list * Env.t * Cont.t` — the same
   accumulate-in-reverse shape as `Prim_args`. An empty literal yields the empty
   array immediately.

3. **Indexing and length are monomorphic primitives** (per
   [0007](0007-monomorphic-primitives.md)): `IndexArray` (`Array → Int → a`) and
   `LengthArray` (`Array → Int`). `IndexArray` is the **unsafe** primitive: an
   out-of-bounds index is `stuck` (it corresponds to `unsafeIndex`, whose JS
   behaviour is "no check"). The total, safe `Data.Array.index :: Array a → Int →
   Maybe a` is **not** a primitive — once ADTs land, it is ordinary library code
   `if 0 <= i && i < length arr then Just (unsafeIndex arr i) else Nothing`,
   which is why `LengthArray` is provided now.

4. **Mutation is out of scope.** No `push`/`modify`/`snoc` primitives here.
   Mutable arrays (`Data.Array.ST`, `Effect` mutable arrays) are a later slice
   with their own representation decision (in-place `VArray` vs a distinct
   mutable tag), made when effects land.

### Transition rules

A literal `Array [e₁, …, eₙ]` (notation as in
[0002](0002-cesk-execution-model.md)):

**Eval mode:**

| `t` | next state |
| - | - |
| `Array []` | `⟨Return (VArray [\|\|]), σ, κ⟩` |
| `Array (e₁ :: rest)` | `⟨Eval(e₁, ρ), σ, Array_elems([], rest, ρ, κ)⟩` |

**Return mode:**

| `κ` | next state |
| - | - |
| `Array_elems(done, [], ρ, κ′)` | `⟨Return (VArray (rev (v :: done) as array)), σ, κ′⟩` |
| `Array_elems(done, e :: rest, ρ, κ′)` | `⟨Eval(e, ρ), σ, Array_elems(v :: done, rest, ρ, κ′)⟩` |

Indexing and length go through the existing `Prim`/`Prim_args` machinery:
`Prim(IndexArray, [arr; i])` and `Prim(LengthArray, [arr])` evaluate their
arguments left to right like any primitive, then `prim.ml` does the bounds-checked
read / length.

## Consequences

- **Array literals allocate nothing in the `Store`.** Like other values, a
  `VArray` lives as a host value for now; element evaluation may allocate (via
  `Arg`/`Let_body`/`letrec`) but the array container itself does not touch the
  store. (Phase 1b revisits this when arrays become heap blocks.)
- **`Array_elems` mirrors `Prim_args`.** Two frames now share the
  evaluate-left-to-right-accumulating-in-reverse pattern; `Record` literals will
  make a third. If a third confirms the shape, a shared "sequence of
  subexpressions" frame could be factored out — noted, not done now.
- **Unsafe-by-default indexing keeps the core small.** The machine has one
  indexing rule; safety (the `Maybe`) is composed in the library once ADTs exist.
  Until then, an out-of-range index is a `stuck` state, consistent with how the
  core treats other type/shape errors.
- **Element order is observable and fixed left-to-right**, matching `Prim_args`
  and the rest of the machine; no surprises for effectful elements later.

## Alternatives considered

- **Represent `Array` as an OCaml list.** Rejected: PureScript `Array` is O(1)
  indexed; a list gives O(n) `index` and the wrong performance model. A vector
  is the faithful representation.
- **Make `IndexArray` safe (return a `Maybe`) as a primitive.** Rejected: it
  needs the `Maybe` ADT, which does not exist yet, and it bakes a library policy
  into the machine. The unsafe primitive + library wrapper keeps the
  primitive/library boundary clean (cf. [0007](0007-monomorphic-primitives.md)).
- **Include mutation now** (`push`, in-place `modify`). Rejected: PureScript
  keeps immutable `Array` and mutable `STArray` distinct; mutation belongs with
  the effects slice and its own representation decision.
- **Make array literals a primitive (`MakeArray` varargs)** instead of a term.
  Rejected: arity is not fixed, and literal construction is a syntactic form like
  `If`/`Let`, not a base-type operation; a dedicated term with its own frame is
  cleaner and matches how CoreFn delivers `ArrayLiteral`.
