# 0022. Native foreign rung: opaque host-provided functions

- Status: Accepted
- Date: 2026-06-19

## Context

The foreign provider ladder (ADR-0017) anticipated three rungs — *intrinsic*
(eta-expanded primops), *structural* (guest terms over first-order primitives,
ADR-0020), and a *native* rung for foreigns the guest cannot express. Only the
first two are built. `show` forces the third.

`Data.Show`'s scalar leaves cannot be written either way:

- `showNumberImpl :: Number -> String` needs floating-point formatting (JS
  `Number.prototype.toString` plus the `"x.0"` rule), which no combination of the
  arithmetic/array primops can produce.
- `showStringImpl`/`showCharImpl` need character-level inspection and escaping —
  expressible only over a *string*-primitive layer (`charCodeAt`, `fromCharCode`,
  substring) we have deliberately not built, and clumsy even then.
- `showIntImpl :: Int -> String` is integer-to-string — again outside the primop
  set.

These are pure, *first-order* host functions: each takes fully-evaluated values
and returns a value, never calling back into guest code. That is exactly the case
a native foreign should serve, and it does not reintroduce the native
re-entrancy that ADR-0020 ruled out for *higher-order* foreigns.

The alternative — minting a new `primop` for each such leaf — would grow the
machine's fixed instruction set with every library foreign. The primop set should
stay the small, closed core; open-ended host functions belong behind an opaque,
extensible boundary.

## Decision

Add the native rung as **opaque host-provided foreign functions**, reached by an
opaque reference and called through a uniform boundary:

- A new leaf term, `Ast.Foreign name`, an opaque reference to a host function by
  its qualified key. It carries no implementation (the AST stays pure, printable
  data) and has no free variables.
- A new value, `Value.VForeign`, a partial application of a host function —
  mirroring `VCtor`: it accumulates arguments one at a time and, when its arity
  is reached, calls the host implementation on the collected (fully-evaluated)
  arguments and returns the result. First-order by construction: the
  implementation receives values and returns a value, so no guest closure is ever
  applied natively.
- A *host registry* supplied to the machine: a lookup from a foreign's name to its
  arity and host implementation (`Value.t list -> Value.t`). Evaluating
  `Ast.Foreign name` consults it to build the initial `VForeign`; an unknown name
  is `stuck`. The registry is threaded into `step`/`run`/`eval` (default empty),
  leaving every existing call site unchanged.
- The FFI module gains the native rung: the resolver maps `Data.Show.show*Impl`
  to `Ast.Foreign` references (so linking and reachability DCE treat them
  uniformly, ADR-0021), and a companion host registry supplies their OCaml
  implementations, faithful to the `prelude` JS FFI. Ladder order: intrinsic →
  structural → native.

Scope of this slice: the four scalar `Show` leaves (`showInt`, `showNumber`,
`showChar`, `showString`) as native functions — escaping and number formatting
are straightforward in the host language. `showArrayImpl` is *higher-order*
(`(a -> String) -> Array a -> String`) and stays a structural guest term
(ADR-0020), since it must apply a guest element-shower via an ordinary `App`.

## Consequences

- `show` on scalars runs end-to-end: a program reaching `show 42`, `show 3.0`, or
  `show "hi"` links (DCE pulls in only the reached leaf) and evaluates to the
  expected `String`.
- The native boundary is established for what comes after `show` — every genuine
  host primitive (eventually, effects) enters through `VForeign`/the registry,
  not through new primops or new machine modes.
- The machine gains exactly one value form and one term form, plus a registry
  parameter; the transition for `VForeign` is the `VCtor` saturation rule with a
  host call at the end. No re-entrancy, no change to existing rules.
- Fidelity caveat: host `Number`→`String` formatting must reproduce JS
  `Number.toString` (e.g. `3.0` → `"3.0"`, integers without a decimal point);
  where the host's native float printing diverges, the implementation normalises
  to match. Documented at the implementation, tested against known cases.

## Alternatives considered

- **A primop per show leaf** — `ShowInt`/`ShowNumber`/… in the `primop` enum.
  Smallest diff, no new value form, but it grows the core instruction set with
  library-specific operations and does not generalise to the open set of host
  functions (or to effects). The native rung pays a one-time mechanism cost and
  then scales by registry entries, not enum cases.
- **Native re-entrancy (`VForeign` applies guest closures)** — would let a single
  mechanism cover higher-order foreigns too, but reintroduces host-recursive
  evaluation that ADR-0020 rejected (it breaks the reified-continuation
  invariant). First-order native + structural guest code already covers the
  cases, so re-entrancy stays out.
- **A string-primitive layer + guest terms** — `charCodeAt`/`fromCharCode`/
  substring primops, with `showString`/`showChar` as guest code. A large
  primitive surface for marginal benefit, and `showNumber` would still need host
  formatting. Rejected for `show`; a string-primitive layer may still be
  warranted later for genuinely structural string algorithms.
