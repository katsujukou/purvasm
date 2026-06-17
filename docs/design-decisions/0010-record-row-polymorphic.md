# 0010. Record as an unordered field map

- Status: Accepted
- Date: 2026-06-18

## Context

`Record` is the last value needed before ADTs. It is the one that is genuinely
*not* like an OCaml record: PureScript records are **row-polymorphic**. A type
like `forall r. { name :: String | r } -> String` accepts any record that has at
least a `name` field; the rest of the fields (`r`) are unknown at the function's
definition but **present in the value at runtime**. After type erasure, the row
machinery is gone and a record is simply a **dynamic set of label → value
bindings** — exactly a JS object, which is how PureScript compiles it.

Three consequences shape the design:

1. **No fixed field set.** Unlike an OCaml record (a fixed, statically-known
   struct), a purvasm record's fields are only a runtime set. Row-polymorphic
   code carries fields it cannot name.
2. **Order does not matter.** A row is an *unordered* set of labels: `{a, b}`
   and `{b, a}` are the same type and the same value. JS objects happen to keep
   an insertion order, but PureScript semantics do not depend on it.
3. **Dynamic growth/shrink exist.** `Record.insert`/`delete`/`merge` and
   unknown-field access (`Record.get`/`unsafeGet` with a runtime label) add,
   remove, and read fields whose names are not statically fixed.

This record fixes the representation and the three syntactic operations CoreFn
emits directly (literal, accessor, update). Dynamic field operations are scoped
out but the representation is chosen so they drop in later.

## Decision

1. **A record is an unordered field map: `Value.VRecord of Value.t Map.M(String).t`**
   (a Base `string`-keyed map). The map *is* the row after erasure. Order is not
   represented, matching row semantics; the left-to-right **evaluation** order of
   a literal's fields is observable (effects), but the resulting value's identity
   is order-independent — which a map gives for free. Lookup/update are O(log n).

2. **Three dedicated terms (CoreFn syntactic forms), not primitives:**
   - `Ast.Record of (string * term) list` — a record literal (`ObjectLiteral`).
     Fields evaluate **left to right** via a `Record_fields` frame (the
     label-carrying sibling of `Array_elems`/`Prim_args`).
   - `Ast.Accessor of string * term` — `r.label` (`Accessor`), a **static**
     label fixed at compile time. Evaluates the record, then projects.
   - `Ast.Update of term * (string * term) list` — `r { l = e, … }`
     (`ObjectUpdate`). Evaluates the base record, then each new field value
     left to right, then returns a **new** record (immutable) with those labels
     overwritten.

3. **Dynamic field operations are out of scope here, but the map supports them.**
   `Record.insert`/`delete`/`merge` and runtime-label access
   (`unsafeGet : String -> Record -> a`, taking the label as a *value*) are
   monomorphic primitives ([0007](0007-monomorphic-primitives.md)) added later;
   each is one `Map.add`/`Map.remove`/`Map.find` on the same `VRecord`. They are
   primitives (label is a runtime `String`), distinct from the static `Accessor`
   term.

4. **Missing/duplicate labels.** Accessing a label not in the record, or
   updating into a non-record, is `stuck` (well-typed CoreFn never does this —
   the row type guarantees the field exists). A literal with a duplicate label
   should not arise from CoreFn; the map takes last-writer-wins.

### Transition rules

Notation as in [0002](0002-cesk-execution-model.md); `m` is a field map, `m[l]`
a lookup, `m{l ↦ v}` an update, `∅` the empty record.

**Eval mode:**

| `t` | next state |
| - | - |
| `Record []` | `⟨Return (VRecord ∅), σ, κ⟩` |
| `Record ((l,e) :: rest)` | `⟨Eval(e, ρ), σ, Record_fields(l, [], rest, ρ, κ)⟩` |
| `Accessor(l, e)` | `⟨Eval(e, ρ), σ, Project(l, κ)⟩` |
| `Update(e, ups)` | `⟨Eval(e, ρ), σ, Update_rec(ups, ρ, κ)⟩` |

**Return mode** — record literal:

| `κ` | next state |
| - | - |
| `Record_fields(l, done, [], ρ, κ′)` | `⟨Return (VRecord (mapOf ((l,v) :: done))), σ, κ′⟩` |
| `Record_fields(l, done, (l₂,e₂) :: rest, ρ, κ′)` | `⟨Eval(e₂, ρ), σ, Record_fields(l₂, (l,v) :: done, rest, ρ, κ′)⟩` |

**Return mode** — projection:

| `κ` | condition | next state |
| - | - | - |
| `Project(l, κ′)` | `v = VRecord m`, `l ∈ m` | `⟨Return m[l], σ, κ′⟩` |
| `Project(l, κ′)` | otherwise | **stuck** |

**Return mode** — update (base record first, then new field values):

| `κ` | condition | next state |
| - | - | - |
| `Update_rec([], ρ, κ′)` | `v = VRecord m` | `⟨Return (VRecord m), σ, κ′⟩` (no-op update) |
| `Update_rec((l,e) :: rest, ρ, κ′)` | `v = VRecord m` | `⟨Eval(e, ρ), σ, Update_fields(m, l, [], rest, ρ, κ′)⟩` |
| `Update_rec(_, ρ, κ′)` | `v` not a record | **stuck** |
| `Update_fields(m, l, done, [], ρ, κ′)` | | `⟨Return (VRecord (m with all ((l,v) :: done) set)), σ, κ′⟩` |
| `Update_fields(m, l, done, (l₂,e₂) :: rest, ρ, κ′)` | | `⟨Eval(e₂, ρ), σ, Update_fields(m, l₂, (l,v) :: done, rest, ρ, κ′)⟩` |

## Consequences

- **Order-independence is structural.** Because the value is a map, two literals
  that differ only in field order produce equal maps; no normalization needed.
  (Field *equality* itself, like any `Eq`, comes from a dictionary, not the
  machine — [0007](0007-monomorphic-primitives.md).)
- **A third left-to-right frame appears** (`Record_fields`), alongside
  `Prim_args` and `Array_elems`. But Record's elements are *labelled*
  (`(label, term)`), so its frame shape genuinely differs; [0009](0009-array-immutable-host-backed.md)
  floated factoring out a shared frame — this confirms the shapes are different
  enough that a forced abstraction would obscure more than it saves. Keep three
  frames.
- **Update is immutable and allocates a new map.** `r { x = 1 }` shares nothing
  mutable with `r`; the old record is unchanged. Matches PureScript.
- **The dynamic row world drops in cleanly.** `insert`/`delete`/`merge`/`unsafeGet`
  are later primitives over the same `VRecord` map; nothing about the
  representation needs to change to support row-polymorphic library code.
- **`Store` is untouched**, as with arrays: the record container is a host value
  for now (phase 1b moves it to a heap block).

## Alternatives considered

- **Association list `(string * Value.t) list`** (like `Env`). Rejected as the
  representation: it leaks field order into the value (two orders → two distinct
  lists), needs explicit dedup, and is O(n). A map matches the unordered-row
  semantics directly. (`Env` can be a list because it is a *scope* with
  shadowing, not a value.)
- **Fixed struct / tuple by field index** (resolve labels to positions at compile
  time). Rejected for phase 1a: it cannot represent row-polymorphic values whose
  field set is not statically known, which is the whole point of records here. A
  positional layout is a *later optimization* for monomorphic records, decided at
  the bytecode layer, not the spec.
- **Positional layout for type-class dictionaries.** Type-class dictionaries are
  the one kind of record whose shape *is* fully static — no row variable, no
  runtime grow/shrink — so they look like ideal candidates for a positional tuple.
  They are blocked, but for a **different reason than row polymorphism**. A
  dictionary with a superclass — e.g. `Apply`, whose superclass is `Functor` —
  lowers to `{ apply: <applyImpl>, Functor0: \_ -> <functorDict> }`, where the
  superclass is held as a **thunk**. (The thunk breaks the cycle when mutually
  recursive instance dictionaries are built — the same Landin's-knot construction
  as [0004](0004-recursion-letrec-fix.md) /
  [0005](0005-mutual-recursion-binding-groups.md), which the machine already
  handles; under our strict semantics the thunk is just a `\_ -> dict` closure.)
  Lowering an access like `applyDict.Functor0 ().map` to a tuple index needs the
  `label → index` map, but CoreFn's `Accessor` carries only the *label*; that
  mapping depends on the **class definition** — its members and the order of its
  superclasses — which is global information absent from the local CoreFn node.
  The field-naming convention (`Functor0` = superclass #0) lets one *guess*, but
  it is a convention, not a semantic tag. Resolving it reliably needs the
  **Externs** (which retain class definitions), so dictionary positional layout
  requires an Externs-aware pipeline, not CoreFn alone. Until then — and for
  correctness, always — a dictionary is simply a `VRecord` and runs as ordinary
  record code.
- **Make `Accessor` a primitive taking the label as a value.** Rejected for the
  static case: CoreFn's `Accessor` has a compile-time label, so a dedicated term
  is the direct lowering. The *runtime-label* read is the separate `unsafeGet`
  primitive — the two are genuinely different operations (cf.
  [0009](0009-array-immutable-host-backed.md)'s literal-term vs index-primitive
  split).
- **Mutable records / in-place update.** Rejected: PureScript records are
  immutable; `ObjectUpdate` produces a new value. Any mutable record story
  belongs with the effects slice.
