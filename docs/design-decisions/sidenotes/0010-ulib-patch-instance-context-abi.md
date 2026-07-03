# Investigation: a ulib patch's type-class instance contexts are a frozen ABI

- Date: 2026-07-04
- Status: constraint pinned; the `record-studio` SingletonRecord patch is the worked example.
- Related: [ADR-0038](../0038-base-package-and-ulib-patches.md) (the overlay model),
  [ADR-0040](../0040-ulib-testing-strategy.md)/[ADR-0043](../0043-ulib-tools-build-verify-test.md)
  (`verify` and its known limits), [ADR-0069](../0069-v1-dynamic-record-operations.md)
  (representation opacity, which motivated the patch).

## The failure

Shadowing `Record.Studio.SingletonRecord` (whose `unsafeGetFirstField` foreign reads a record's
only field *without its name* — inexpressible on the opaque id-keyed representation), the obvious
rewrite added `IsSymbol key` to the `SingletonRecord` instance context so `value` could
`unsafeGet (reflectSymbol …)`. It type-checked, passed `verify` (interface-faithful), staged —
and then **every backend failed at runtime** with `accessor: not a record` on a program using
`value`.

## The mechanism

A consumer module's corefn is compiled against the **registry** module, and dictionary
construction is baked into the consumer at its constraint-solve site:

```
-- consumer corefn (compiled against registry: context = RowToList + SingletonRecordFields)
dict = singletonRecord1 Prim.undefined srfDict        -- exactly 2 arguments
```

The ulib overlay then swaps in the patch's *definitions* under those call sites. The patched
instance builder took **3** arguments (`IsSymbol` added), so the consumer's 2-argument
application left the dictionary a **partial application** — and the member accessor
(`dict.value`) projected a field from a closure. The stuck was loud, but nothing before runtime
catches it: `verify` compares export-name sets, and the compile of the staged tree is internally
consistent (the type error only exists *across* the registry-compiled consumer and the patched
definition).

So, for any module a ulib patch shadows:

- **Frozen (consumer-visible ABI):** each instance's **context — count and order of
  non-erased constraints** (each becomes a builder argument; `Prim`-class constraints erase to a
  `Prim.undefined` placeholder argument, so they count too); the **declaration order of
  instances** (the generated `fooBar`/`fooBar1` instance names consumers reference by qualified
  key); every export's **type/arity as consumers apply it** (a class member's dictionary-record
  field is projected by the *shadow's* accessor, but the member accessors and instance builders
  are only consistent because both live in the shadowed module).
- **Free:** definition *bodies*; private declarations; **adding** class members (the dictionary
  record is built by the shadow's instance builders and projected by the shadow's accessors —
  both overlaid together), with the residual that a *downstream hand-written* instance of that
  class would lack the new member; adding new exported declarations (`verify` reports the
  widening and passes).

## The worked fix

Keep every class head, instance head, context, and declaration order **verbatim** from upstream;
route the needed `IsSymbol` evidence through the instance that already had it
(`SingletonRecordFields`' resolving instance), exposed as a **new member** `firstFieldKey`
returning the reflected key string; `value` then reads `unsafeGet (firstFieldKey …)`. Validated
by a scratch consumer compiled against the **registry** module and linked with the overlay
(VM / OCaml-native / LLVM-native all agree with the JS reference), plus the upstream suite via
`ulib-tools test` (18/18).

## Lessons

1. **Test a patch against a registry-compiled consumer, not only the staged tree.** The staged
   compile and the JS-fidelity upstream suite both compile consumers *from the patched source*,
   so they can never see this class of breakage; only the corefn-overlay link path
   (`purvm … --ulib`) exercises the mixed-compilation ABI. A scratch consumer + the three-backend
   run is the cheap probe.
2. **`verify`'s export-name diff is blind to signature- and instance-shape drift** — already
   recorded as its known limit (ADR-0043: purs 0.15.16 lacks `docs --format json`); instance
   contexts are a second, subtler instance of the same gap. The deferred externs-based
   compatibility gate (ADR-0038/0040) would close both, since externs carry instance heads and
   contexts.
