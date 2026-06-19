# 0018. Honour `IsNewtype`: erase newtype wrappers in lowering

- Status: Accepted
- Date: 2026-06-19

## Context

Implementing primitive FFI ([0017](0017-primitive-ffi.md)) and trying to run a
real `Prelude` arithmetic program surfaced a blocker: **current PureScript
represents every typeclass dictionary as a newtype.** Verified against
`prelude` 6.0.2 / `purs` 0.15.16, the `Semiring` class compiles to

- a newtype constructor decl `Semiring$Dict = \x -> x` carrying `meta: IsNewtype`
  (the constructor is the **identity function**), and
- a method `add = \dict -> case dict of Semiring$Dict v -> v.add`, whose
  `ConstructorBinder` also carries `meta: IsNewtype`,
- an instance built as `semiringInt = Semiring$Dict { add: intAdd, … }` — i.e.
  `App(Var Semiring$Dict, <record>)`.

[0015](0015-corefn-lowering.md) lowered every `ConstructorBinder` to a real
`BCtor` and deferred `Meta`. The two sides then disagree: construction applies the
**identity** constructor, so `semiringInt` is just the **record** (a `VRecord`);
but the method's match is `BCtor("Semiring$Dict", …)`, which tested against a
`VRecord` is a wrong-shape **stuck** ([0012](0012-array-record-binders.md)).

So newtype handling is **not a deferred optimisation** (as
[0010](0010-record-row-polymorphic.md)/[0015](0015-corefn-lowering.md) framed it)
but a **correctness requirement**: dictionaries are newtypes, so without erasing
them no real PureScript runs at all.

## Decision

**Honour `IsNewtype` in lowering by erasing the newtype wrapper** — the runtime
value of a newtype *is* its underlying value, exactly as PureScript/JS treats it.

- A **`ConstructorBinder` carrying `meta: IsNewtype` is transparent**: lower it to
  its single inner binder, dropping the constructor test. `case x of C$Dict v -> e`
  becomes `case x of v -> e`, so a method `\dict -> case dict of C$Dict v -> v.m`
  is effectively `\dict -> dict.m`.
- **Construction needs no special case in the common path**: PureScript already
  emits the newtype constructor *declaration* as the identity function
  (`C$Dict = \x -> x`), which lowers normally, so `App(Var C$Dict, r)` reduces to
  `r`. Defensively, a `Constructor` *node* carrying `IsNewtype` (should one ever
  appear directly) lowers to the identity function rather than `Ctor(name, 1)`.
- This is the **first load-bearing use of `Ann.meta`**, which
  [0014](0014-corefn-ingestion.md) deliberately retained.

The machine and `Cesk.Ast.binder` are **unchanged**: erasure happens entirely in
lowering, so the interpreter never sees a newtype binder and stays simple.

## Consequences

- **Typeclass dictionaries work**, unblocking the `Prelude` milestone together
  with [0017](0017-primitive-ffi.md)'s leaves: `add dict x y` reduces through the
  record accessor to `intAdd x y` to a primop. The "dictionaries are free
  records" reading ([0007](0007-monomorphic-primitives.md)/[0010](0010-record-row-polymorphic.md))
  holds — once the newtype around the record is erased.
- **Newtypes are zero-cost in general** (user newtypes too), matching PureScript;
  no `VData` box is allocated for them.
- **`Ann.meta` earns its keep** ([0014](0014-corefn-ingestion.md)); this revises
  [0015](0015-corefn-lowering.md)'s "defer Meta-driven optimisation" — newtype
  erasure is correctness, not optimisation (a dated Correction is added there).
- **Faithful runtime representation:** a newtype value is its underlying value, so
  the interpreter and a future native backend agree with PureScript/JS — keeping
  the differential-testing oracle valid.
- **Bounded scope:** only `IsNewtype` is acted on; other `Meta` (e.g.
  `IsTypeClassConstructor`, `IsForeign`) remain informational for now.

## Alternatives considered

- **Treat newtype constructors as real (`VData`-wrapped) constructors,
  consistently.** Rejected: PureScript emits the constructor decl as the identity
  function, so to box consistently we would have to *override* CoreFn's own decl —
  fighting the source — and it would allocate a wrapper for every dictionary and
  user newtype, diverging from PureScript's zero-cost newtypes.
- **Keep deferring newtypes as an optimisation.** Rejected: it is not optional —
  without erasure, dictionary method dispatch is a wrong-shape `stuck`, so no real
  PureScript program runs.
- **Erase at decode (ingestion) instead of lowering.** Rejected: decode stays a
  faithful mirror of CoreFn ([0014](0014-corefn-ingestion.md)); erasure is a
  semantic decision driven by `meta`, so it belongs in lowering
  ([0015](0015-corefn-lowering.md)).
- **A dedicated machine `VNewtype` / transparent value.** Rejected: pointless — a
  newtype's underlying value already *is* its representation; adding a machine
  concept would complicate the interpreter for no semantic gain.
