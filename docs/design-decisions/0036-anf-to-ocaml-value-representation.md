# 0036. ANF → OCaml codegen: value representation and calling convention

- Status: Accepted
- Date: 2026-06-25

> **Progress (2026-06-25).** Implemented in `lib/ocaml_backend/codegen_ml.ml`: the `Rt`
> prelude (`value`, `app`, primitives, `to_string` mirroring VM `Value.to_string`) + an
> ANF→OCaml emitter, driven end-to-end (codegen → `ocamlopt` → run) against the oracle
> in the e2e `ocaml` group. Covered: the pure subset; recursive *value* bindings via
> OCaml `lazy` (forced on reference; ADR-0024); all binder kinds — incl. record and
> number — via a CPS cascade matcher (`emit_match`); and the **foreign boundary** + an
> Effect runner (force the entry to unit, perform, print the result).
>
> **Refinement to the foreign decision.** The Decision's foreign bullet proposed an
> `Rt.value` ↔ oracle *bridge* onto `Ffi.host`. In practice the generated program is a
> **self-contained `ocamlopt` executable** (stdlib only; it does *not* link the boot
> libraries), so the native leaves are **re-implemented in the `Rt` prelude over
> `Rt.value`** (`showInt`/`showString`/`showNumber`/`log`, added on demand — the
> minimal-FFI policy), with the differential enforcing parity with `Ffi.host`. This is a
> better fit for the self-contained-native goal than linking the compiler's host.
>
> **Update — the hybrid calling convention (option 2) is implemented.** A known-arity
> function (a directly-lambda binding) emits a native multi-arg `fn_<x>` plus a `VClos`
> wrapper `<x>`; an exactly-saturated call compiles to a *direct* `fn_x a b c` (no `app`,
> no `VClos` — `ocamlopt` optimises it), while partial/over/unknown application falls
> back to `app` (over-application saturates natively then `app`s the rest). Verified: a
> recursive `fact`/`fib` compiles its self-calls as direct native calls (zero `app`),
> preserving uncurrying.
>
> **Update — `case` decision-tree lowering.** A case whose binders are all *friendly*
> (no record binder; a number literal rides a `VNumber v` binding + `when v = f` guard)
> compiles to an OCaml `match`, so `ocamlopt` builds the decision tree (each scrutinee
> examined once). A case containing a record binder falls back to the CPS cascade.
> Verified: `classify`/`pick` compile to OCaml `match`, `viaRecord` to the cascade. The
> remaining perf items (unboxing, arity-tagged native closures) stay deferred.

## Context

ADR-0035 chose **OCaml 5 codegen** for boot's native backend: lower optimised ANF
(ADR-0025) to OCaml source, compiled by `ocamlopt`. This ADR fixes *how ANF values and
calls map to OCaml*, so the codegen and a small hand-written **runtime-support module**
(`Rt`) can be written and validated against the oracle/VM.

Guiding constraints: **correctness first** (optimisation is deferred / on-demand,
ADR-0035); **lean on OCaml** (its closures, GC, strict evaluation, and `lazy`) rather
than rebuild machinery; stay **structurally close to the existing value model**
(`Cesk.Value` / VM `Value.t`) so the differential printer works across all three.

## Decision

### A uniform `value` ADT (runtime-support module `Rt`)

After type erasure (CoreFn → ANF) a PureScript value is dynamically shaped, so OCaml
needs **one** type for "any value". `Rt.value` mirrors VM `Value.t`:

`VInt of int` · `VNumber of float` · `VBool of bool` · `VString of string` ·
`VArray of value array` · `VRecord of value SMap.t` (as in VM `Value`) ·
`VData of string * value array` ·
`VCtor of string * int * value list` (under-applied constructor) ·
`VClos of …` (a function — see below) · `VForeign of …` (native leaf, ADR-0022).

It is *simpler* than VM `Value.t`: **no `Vpap`** (an OCaml closure already is a partial
application) and **no `Vindirect`** (OCaml `lazy` is the by-need cell — see Recursion).
Literals map directly (`LInt`→`VInt` an OCaml `int`, `LNumber`→`VNumber` float,
`LBool`/`LString` likewise; `Char` is an `int`, ADR-0006). `CCtor` → `VData` when
saturated, `VCtor` when partial; `CRecord`/`CAccessor`/`CUpdate` → `VRecord` (a map);
`CArray` and `NewArray`/`SetArray` → `VArray` (a *mutable* OCaml array). Same shapes as
the VM, so one printer compares all three byte-identically.

### Calling convention (the central choice)

ANF is *uncurried* (multi-arg `CLam`/`CApp`, ADR-0025). Two ways to map this:

1. **Uniform curried closures** — `VClos of (value -> value)`; an arity-n lambda becomes
   nested OCaml closures; every `CApp` goes through `app : value -> value -> value`.
   Simplest and obviously correct, but **throws away uncurrying**: every call is curried
   through an arity-1 dispatch (intermediate closures, no `ocamlopt` fast path).
2. **Hybrid (recommended)** — emit a *known-arity* function as a **native OCaml
   multi-arg** function and a *statically-saturated* call as a **direct** `f a b c` (so
   `ocamlopt`'s direct multi-arg path applies — uncurrying preserved). Only when a
   function is used as a *value* (higher-order) is it wrapped in `VClos`, and only
   *unknown / partial / over*-application goes through the uniform `app`.

**Decided: option 2 (the hybrid).** It keeps the point of the uncurried ANF for the
common direct-call case (so the bootstrapped self-host is fast enough to be usable),
while the `VClos`/`app` path stays correct for higher-order code. Three details the
implementation must pin down:

- **Where arity is known.** `CApp(head, args)`'s callee arity is static only when `head`
  is a top-level `Gfun` or a local `CLam` — an `AVar` parameter is unknown. This is the
  *same* arity problem `Codegen.gdef_of_expr` already classifies for the bytecode VM
  (and ADR-0034's effect analysis), so **reuse that classification**; everything else
  takes the `app` path.
- **Both forms are emitted.** A known-arity function needs (a) a native multi-arg
  definition for direct calls and (b) a **`VClos` wrapper** for when it is passed as a
  value. Taking `VClos of (value -> value)` (option 1's arity-1 payload), the wrapper is
  the re-curried `fun a -> VClos (fun b -> … native a b …)`.
- **`app` is a small, honest eval/apply.** `app : value -> value -> value` dispatches on
  the value: a `VClos` rides OCaml's own currying (saturate/PAP/over for *closures* is
  OCaml's), but **`VForeign` and `VCtor` (partial constructor) saturation is bespoke** —
  `app` accumulates their args and fires at full arity. So "lean on OCaml" holds for the
  direct path and the closure case; the indirect path *does* carry a minimal eval/apply
  for foreigns and constructors (see Alternatives).

Option 1 remains the fallback if 2's known-arity bookkeeping proves not worth it. (A
fully unboxed / arity-tagged native-closure representation via `Obj.magic` is a later
perf option, not now.)

### Strict evaluation and recursion

OCaml is CBV, matching ANF — `Let` → OCaml `let` (evaluation order preserved,
ADR-0025/0034). A top-level non-function CAF is just a `Let` in the program spine, built
strictly in dependency order (ADR-0030/0032) — the emitted `let` sequence preserves that
order.

A recursive group (`LetRec`) lowers to a **single** OCaml `let rec … and …`: a function
member keeps its plain RHS, and a *non-function* (by-need) member is `lazy`, forced on
use. OCaml's `let rec` permits this mix (`let rec f = (fun …) and v = lazy (…)`), which
matters because the motivating case for by-need recursion (ADR-0024/0032) is an
**instance-dictionary group that mixes function and value members** (or is all values) —
a naive "function ⇒ `let rec`, value ⇒ `lazy`" split would miss exactly that. The
mutual knot-ties through `let rec` + `lazy`.

`lazy` realises by-need and its `Lazy.Undefined` is exactly our `Building` **cycle
detection** (a value re-forced while already forcing). It is **not** a parallel
black-hole: OCaml `lazy`/`force` is *single-domain* — concurrent `force` from two
domains is a data race, and `Lazy.Undefined` says nothing about another domain
evaluating the same cell. This is safe in **boot** because CAFs/recursive cells are
built at startup on one domain, in dependency order, *before any fiber starts*. The real
cross-domain forcing synchronisation (GHC's `Blackhole` blocking-and-resume, sidenote
0006 §1) is a **bespoke-phase** concern, bucketed with the deferred
`Ref`/`AVar`-across-domains semantics (ADR-0035) — not `lazy`.

### `case`, primops, Effect, foreigns

- **`case`** reuses the decision-tree compiler (`Match_compile`, ADR-0031); the tree is
  emitted as an OCaml `if`/`match` cascade over `VData` tags, literals, and array length.
- **Primops** → `Rt` functions over the unwrapped scalars (`AddInt` = `int` `+`,
  `IndexArray`/`SetArray`/`NewArray` = OCaml array ops, …).
- **`Effect a ≡ Unit -> a`** (ADR-0023) is just a `VClos` thunk; `run_effect` =
  `app main unit`. The scheduler / effect handlers (ADR-0035) build on this seam — a
  fiber is a forced Effect thunk — and are **deferred to that follow-up ADR**. But
  because an Effect body is emitted as a `VClos`, **ADR-0034's invariant I4** (the
  effect body's arity must match the saturation convention of the loop combinator
  `forE`/`foreachE`/`whileE` driving it — a mismatch is a *silent wrong result*) must
  hold across **both** the direct and the re-curried-`VClos` paths. It is a validation
  obligation here even though the scheduler is deferred.
- **Foreign leaves** reuse the host registry (`Ffi.host`, ADR-0022), but "over
  `Rt.value`" means a **third value-conversion layer**: model it on `vm/foreign.ml`'s
  `to_oracle`/`of_oracle` (which already bridges VM `Value.t` ↔ `Cesk.Value`), giving an
  `Rt.value` ↔ oracle bridge that **inherits the same constraints** (e.g. a higher-order
  closure crossing a first-order leaf boundary is unsupported → stuck). Preserves the
  native-leaf seam and the minimal-FFI policy ([[boot-then-selfhost-bootstrap]]).

### Validation

Differential, as always: codegen → OCaml source → `ocamlopt` → run **equals** the CESK
oracle and the bytecode VM on value and `Effect` order for every fixture and bench.
Three cases to list explicitly, since they are where the mapping is subtle:

- **I4 arity consistency** — a loop combinator (`forE`/`foreachE`/`whileE`) driving a
  multi-argument effect body, exercised through both the direct and `VClos` paths.
- **Heterogeneous recursive groups** — an instance-dictionary group mixing function and
  value members must knot-tie correctly through `let rec` + `lazy` (ADR-0024/0032).
- **Over/partial application of `VForeign`/`VCtor`** — the bespoke saturation in `app`.

## Consequences

- A correct native path leaning on OCaml (closures, GC, strict eval, `lazy`), with a
  small bespoke core — `Rt` is mostly the `value` type, `app`, and prim/foreign glue.
- `Rt.value` is a third structural sibling of `Cesk.Value` / VM `Value.t`; the shared
  printer keeps differential testing trivial. Foreigns add a third
  `Rt.value` ↔ oracle bridge alongside `vm/foreign.ml`.
- `lazy` cleanly realises ADR-0024 by-need + cycle detection **for boot's single-domain
  startup**; it is *not* a parallel black-hole — cross-domain forcing safety is deferred
  to the bespoke phase (with `Ref`/`AVar`-across-domains).
- The indirect `app` path is a *minimal* eval/apply (closures ride OCaml; `VForeign` /
  `VCtor` saturation is bespoke), so "lean on OCaml" is true of the direct path, not
  absolutely.
- `CArray` (immutable, ADR-0009) and `NewArray`/`SetArray` (ADR-0019) collapse to one
  *mutable* OCaml array (as in the VM). Then ADR-0035's lever "immutable data is freely
  shareable across cores" is a **convention, not enforced by the type/representation** —
  flag for the bespoke GC/sharing design.
- Boxed scalars and (even in the hybrid) some closure wrapping are not maximally fast;
  acceptable per ADR-0035. **Named follow-ups:** unboxing / arity-tagged native closures,
  and the scheduler/`Aff`/`AVar` ADR that makes `Effect` thunks fibers.

## Alternatives considered

- **Uniform curried closures (option 1).** Simplest, but discards uncurrying and is
  likely too slow to bootstrap comfortably; kept as the fallback.
- **Eval/apply in OCaml over `value list`, mirroring the VM.** Rejected as the *primary*
  convention — it reimplements what OCaml's application already does for closures. But
  honestly, the hybrid's indirect `app` path *is* a minimal eval/apply for `VForeign` /
  `VCtor` saturation; the win of the hybrid is that the *common direct-call path* avoids
  it entirely, not that eval/apply disappears.
- **`Obj.magic` dynamic-arity native closures.** Fast multi-arg without currying, but
  unsafe and fiddly; deferred to the on-demand perf pass.
- **Reuse `Cesk.Value` as the runtime value.** Its closures are over Cesk *terms* (the
  interpreter), not OCaml functions; a native backend needs OCaml-closure-backed
  functions, so a distinct but structurally identical `Rt.value` is cleaner.
- **Unboxed native scalars throughout.** Needs a non-uniform representation or `Obj`
  tricks; the uniform ADT is correct and simple first, unboxing is a perf follow-up.
