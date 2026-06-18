# 0015. CoreFn lowering: `Corefn` → `Cesk.Ast`

- Status: Accepted
- Date: 2026-06-18

## Context

[0014](0014-corefn-ingestion.md) (Accepted) decodes `corefn.json` into a faithful
`Corefn` AST. The remaining step of the frontend is **lowering**: translate a
`Corefn.Module.t` into a `Cesk.Ast.term` the machine can run, closing the
pipeline `purs → corefn.json → Decode → Lower → Machine.eval`.

Most of lowering is a one-to-one structural map, because the core was built
slice-by-slice to mirror exactly what CoreFn emits ([0006](0006-string-utf8-char-int.md)–
[0013](0013-case-guards.md)). The decisions worth recording are the few places
where CoreFn and the core do **not** line up one-to-one:

1. **Names.** CoreFn `Var` is `Qualified (Maybe ModuleName) Ident`; the core's
   environment is keyed by a flat `string` ([0002](0002-cesk-execution-model.md)).
   Locals, same-module top-level references, and cross-module references must all
   land in that one namespace without colliding.
2. **A module is a set of bindings, not an expression.** The machine evaluates a
   term; a module is a list of `Bind`s. Lowering must turn the module into a term
   that evaluates a chosen entry binding with the others in scope.
3. **Binding order matters under strict evaluation.** CoreFn dependency-analyses
   top-level (and `let`) bindings into ordered `NonRec`/`Rec` groups; that order
   is what makes strict initialisation of non-function bindings well-defined
   (cf. [0004](0004-recursion-letrec-fix.md)/[0005](0005-mutual-recursion-binding-groups.md)).
4. **A few CoreFn nodes carry information the type-erased core discards** —
   constructor type names and qualifiers, `ObjectUpdate` copy-field hints, `Char`.

This record settles those. It deliberately defers everything to do with linking
the world together — that is its own slice (see *Scope*).

## Decision

### A dedicated `Lower` library

Lowering bridges two subsystems, so it lives in its **own library** (`lower`,
depending on both `corefn` and `cesk`). `corefn` stays a pure decoder (no `cesk`
dependency) and `cesk` stays independent of the frontend; the dependency arrow
points only inward, from `lower` to each. This keeps each subsystem's
responsibility single and the layering acyclic.

### Structural lowering (the one-to-one part)

Each `Corefn` form maps to its core counterpart:

- Literals → `Lit`/`Array`/`Record`, with **`LitChar` code point → `LInt`**
  (Char is Int, [0006](0006-string-utf8-char-int.md)); `LitArray`/`LitObject`
  recurse into `Array`/`Record`.
- `Abs`→`Lam`, `App`→`App`, `Accessor`→`Accessor`.
- `ObjectUpdate`→`Update`, **dropping the `copyFields` hint** (our update is an
  immutable functional update, [0010](0010-record-row-polymorphic.md); the hint is
  a closed-vs-open optimisation the spec does not need).
- `Constructor (typeName, ctorName, fieldNames)` → **`Ctor (ctorName,
  |fieldNames|)`** — tag-only, so `typeName` and the constructor's module
  qualifier are dropped (the machine discriminates on the tag alone,
  [0011](0011-adt-pattern-matching.md)).
- `Case` → `Case`; each binder maps to its core binder
  ([0011](0011-adt-pattern-matching.md)/[0012](0012-array-record-binders.md)), a
  `ConstructorBinder` keeping only the constructor name as the tag; the
  alternative's `Either (guard list) expr` maps to
  `Guarded`/`Unconditional` ([0013](0013-case-guards.md)).

### Name resolution: a flat-key scheme

Every `Var` resolves to one environment key by origin:

- **`Qualified None id` (a local)** → the bare key `id` — it was introduced by an
  enclosing `Abs`/`Let`/binder, which binds the same bare `id`.
- **`Qualified (Some mn) id` (a top-level/imported reference)** → a **qualified
  key** (the module name joined with the ident, e.g. `Data.Maybe.fromMaybe`).

Top-level declarations are bound under their **own-module qualified key**, so a
same-module reference resolves to them. Locals stay bare. Because the two live in
distinct key shapes, a local can shadow a same-named top-level without a
qualified reference to that top-level being captured by the local — the one case a
naive bare-name scheme would get wrong.

### A module becomes an entry-evaluated binding chain

A module lowers, for a chosen entry identifier, to its declarations wrapped around
`Var <entry's qualified key>`. The wrapping folds the decls **in CoreFn's order**,
`NonRec → Let` and `Rec → Letrec` ([0004](0004-recursion-letrec-fix.md)/[0005](0005-mutual-recursion-binding-groups.md)),
binding each under its qualified key. **This preserves the strict initialisation
order CoreFn's dependency analysis computed** — collapsing everything into one
recursive group would black-hole a non-function binding that reads a sibling
defined later (e.g. `a = b` before `b = 2`), which CoreFn's `NonRec` ordering
exists precisely to avoid. The identical fold (binding bare keys) lowers an
expression-level `Let`, whose binds are locals.

### Unresolved references stay total, not fatal

Lowering is **total**: it never fails. A reference to another module, or to a
`foreign` import, lowers to a `Var` under its qualified key that is simply **not
bound** in this single module — so it is `stuck` ("unbound variable", naming the
missing qualified key) only if it is actually forced at runtime. A later linking
slice will bind those keys; until then a module still lowers and runs as far as
its self-contained parts allow.

### Scope of this slice

- **One module**, evaluated at a chosen entry. Deferred: cross-module **linking /
  import resolution**, **FFI / `foreign`**, the **qualified-FFI-ident → primitive
  table** ([0007](0007-monomorphic-primitives.md) §3 — it is coupled to
  dictionary handling and linking, so it waits for them), and any **`Meta`-driven
  optimisation** (newtype erasure, constructor fast paths). Unresolved
  externals/foreigns are the total-lowering case above.

## Consequences

- **The pipeline runs end to end** for self-contained modules:
  `purs → corefn.json → Decode → Lower → Machine.eval`. Programs needing only
  Prim types and their own definitions (data, pattern matching, records, arrays,
  higher-order functions) execute; programs needing Prelude arithmetic/classes do
  not yet, because those route through imports/dictionaries (deferred).
- **CoreFn's `Rec`/`NonRec` ordering is load-bearing**, not cosmetic: the lowering
  relies on it for correct strict initialisation, reusing the same machinery for
  top-level and `let`. This is the concrete payoff of taking binding groups from
  the frontend ([0005](0005-mutual-recursion-binding-groups.md)).
- **Type-erased drops are explicit and safe**: `typeName`, constructor module
  qualifiers, and `copyFields` carry no runtime meaning for a tag-discriminating,
  immutable-record machine; dropping them matches [0007](0007-monomorphic-primitives.md)/
  [0010](0010-record-row-polymorphic.md)/[0011](0011-adt-pattern-matching.md).
- **The flat-key scheme is the seam for linking.** Cross-module references already
  have well-formed qualified keys; the deferred linking slice only has to *bind*
  those keys (from other modules' lowerings or the prim table), not change how
  references are produced.
- **Testable without `purs` at test time** by lowering a committed `corefn.json`
  fixture (as in [0014](0014-corefn-ingestion.md)) at a chosen entry and checking
  the evaluated `Value.t` — an end-to-end check of decode + lower + run.
- **`Char` fully collapses to `Int`** at the frontend boundary, so nothing below
  lowering ever sees a character ([0006](0006-string-utf8-char-int.md)).

## Alternatives considered

- **One big top-level `Letrec` over all decls** (ignore `Rec`/`NonRec`). Rejected:
  under strict evaluation a non-function binding that reads a sibling bound later
  in the group hits a black-hole ([0004](0004-recursion-letrec-fix.md)); CoreFn's
  ordered `NonRec` groups exist to prevent exactly that, so the lowering must
  preserve them.
- **A naive bare-name environment** (drop the module qualifier, key everything by
  ident). Rejected: a local shadowing a same-named top-level would capture a
  qualified reference meant for the top-level. The qualified-key scheme keeps the
  two namespaces distinct at negligible cost.
- **Hard-error at lowering on an external / `foreign` reference.** Rejected for
  the first slice: total lowering lets a module lower and run its self-contained
  parts even when a dead or not-yet-exercised branch mentions an external; the
  runtime `stuck` already names the missing qualified key. (A linking slice may
  add an *opt-in* completeness check later.)
- **Include the primitive table now** ([0007](0007-monomorphic-primitives.md) §3).
  Rejected: a leaf FFI ident is only reachable as a direct call after dictionary
  elimination or cross-module linking; without those the table has nothing to act
  on. It belongs with the linking/dictionary slice.
- **Lower directly during decode (no separate `Lower` library).** Rejected, as in
  [0014](0014-corefn-ingestion.md): decoding (JSON shape) and lowering (language
  semantics) are separate concerns; fusing them buries name resolution and the
  binding-order logic in JSON plumbing and couples `corefn` to `cesk`.
- **Put lowering inside `corefn` or `cesk`.** Rejected: it would force a
  dependency between the decoder and the machine in one direction or the other;
  a dedicated `lower` library keeps both subsystems single-purpose and the
  layering acyclic.
