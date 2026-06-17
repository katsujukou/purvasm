# 0007. Monomorphic primitives; type classes stay dictionary-passing

- Status: Accepted
- Date: 2026-06-17

## Context

[0006](0006-string-utf8-char-int.md) extended `Eq` and `Lt` to accept either two
`VInt`s or two `VString`s, making those primitives **polymorphic** — they inspect
the runtime shape of their arguments and dispatch. That was expedient but does
not match how PureScript or CoreFn actually work, and it was flagged immediately.

In PureScript, `==` is a method of the `Eq` type class, not a built-in. After
type checking, classes are desugared to **dictionary passing**: `x == y` (with
`x, y : Int`) becomes roughly

```purescript
Data.Eq.eq dictEqInt x y      -- dictEqInt = { eq = eqIntImpl, ... }
```

where the leaf `eqIntImpl` is the FFI implementation. So what purvasm is handed
by CoreFn is never a polymorphic `==`; it is a reference to a specific,
**monomorphic** FFI operation (`Data.Eq.eqIntImpl`, `Data.Eq.eqStringImpl`, …)
applied to two arguments. A polymorphic `Eq` primitive that switches on value
shape is therefore both semantically off and a re-introduction of runtime type
information into a type-erased machine.

## Decision

1. **Primitives are monomorphic.** Each primitive corresponds to exactly one
   base-type FFI operation and accepts exactly one argument shape. The
   polymorphic `Eq`/`Lt` of [0006](0006-string-utf8-char-int.md) are split into
   per-type primitives (e.g. `EqInt` / `EqString`, `LtInt` / `LtString`).
   `Add`/`Sub`/`Mul` are already `Int`-monomorphic; `Append` is already
   `String`-monomorphic. (When `Number` is added later, `intAdd` and `numAdd`
   become distinct primitives by the same rule.)

   > **Progress (2026-06-17):** at the user's direction, `Add`/`Sub`/`Mul` are
   > renamed to `AddInt`/`SubInt`/`MulInt` now — type-suffixed from the start
   > rather than waiting for `Number` — so every arithmetic and comparison
   > primitive is uniformly type-tagged and no untyped-looking primop remains.
   > When `Number` lands, `AddNumber` (etc.) join them as distinct primitives.

2. **Type classes remain ordinary dictionary passing.** The machine knows nothing
   about classes: an instance dictionary is just a value (a record of methods),
   threaded as a normal argument. Only the **leaf FFI methods** of the prim
   libraries resolve to purvasm primitives. Composite instances — `Eq` for
   arrays, records, and user-defined ADTs — are ordinary PureScript code that
   *calls* those primitives; they are **not** primitives themselves.

   Dictionary passing is the *baseline*, not a requirement to keep dictionaries
   around at runtime. When the instance is statically known, optimization —
   dictionary elimination / specialization / inlining, whether by
   `purs-backend-es` upstream or a later purvasm pass — may erase the dictionary
   and reduce a method call to a **direct call of the monomorphic leaf method**,
   which is sometimes a primitive. purvasm accepts that reduced form natively: a
   direct application of a primitive is exactly what it already evaluates.
   Correctness is unaffected — dictionary-routed or specialized-direct, both
   reach the same monomorphic primitive.

3. **CoreFn lowering maps qualified FFI idents to primitives** via a table
   (`Data.Eq.eqIntImpl → EqInt`, `Data.Semiring.intAdd → Add`, …). This is the
   single place that knows the correspondence; it is built out when the CoreFn
   frontend lands.

4. **A primitive applied to the wrong shape stays `stuck`.** Well-typed CoreFn
   can never do this (the type checker guarantees the FFI ident matches the
   value), so the `stuck` branch is a defensive internal-error case, not a
   user-facing behavior.

## Consequences

- **No runtime type dispatch.** `prim.ml` no longer asks "is this an int or a
  string?" — each primitive has one argument pattern. Type erasure is respected:
  the machine never needs an argument's type to pick an operation.
- **A clean primitive / type-class boundary.** Base-type leaves (`eqInt`,
  `intAdd`, `concatString`) are primitives; everything composed on top
  (`eqArray`, `eqRecord`, an ADT's derived or hand-written `Eq`) is a normal
  dictionary that bottoms out in primitives. This is the rule that will stop
  Record/Array/ADT work from sprouting ad-hoc `Eq`/`Ord` primops.
- **Naming.** Primitives are type-suffixed (`EqInt`, `EqString`) for readability;
  the lowering table maps the real FFI idents onto them, so the surface names of
  the prim libraries and our internal primop names are decoupled.
- **Samples/tests split by type.** Hand-built terms now use `eq_int` / `eq_string`
  (etc.) smart constructors instead of one overloaded `eq`. Minor churn, and it
  mirrors what CoreFn produces.
- **Scope of this change now:** refactor `Eq → EqInt|EqString` and
  `Lt → LtInt|LtString` in `Ast.primop` and `prim.ml`, and update the
  smart-constructors/samples/tests. The CoreFn→primitive table (point 3) is
  deferred to the frontend; nothing else in the machine changes.

## Alternatives considered

- **Keep polymorphic primitives with runtime dispatch** (the [0006](0006-string-utf8-char-int.md)
  state). Rejected: it needs runtime type info, does not match CoreFn's
  per-instance FFI references, and blurs the line between primitive base
  operations and composable type-class methods.
- **One generic structural equality** (a single deep `==` over all values).
  Rejected: PureScript `Eq` is per-instance and may be *user-defined* (a custom
  `Eq` that is not structural — e.g. case-insensitive, or ignoring a field).
  Baking structural equality into the machine would silently give the wrong
  answer for such instances; equality must come from the dictionary, not the
  machine.
- **Tag primitives with an explicit type argument** (`Eq` taking a type code).
  Rejected: that is just runtime dispatch wearing a hat — the monomorphic split
  is simpler and is what the FFI idents already give us for free.
