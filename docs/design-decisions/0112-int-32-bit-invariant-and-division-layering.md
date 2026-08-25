# 0112. `Int`'s 32-bit invariant is absolute: normalise every quotient, and let Prelude choose the division

- Status: ~~Proposed~~ **Accepted** _(2026-08-17: accepted by the maintainer after two review rounds — the absolute 32-bit invariant with pinned zero-divisor values, the purvasm-base/Prelude layering with quot/rem derived rather than intrinsic, the NbE constant-folding path, and the precisely scoped divergence from stock `purs`)_
- Date: 2026-08-17

> **Revision (2026-08-17, review round 1):** the leak has a **second** source, and it is not a
> provider: `Nbe.Eval.foldPrim` folds `Int` operations with whatever `Prelude` the compiler was built
> against, so a node-built and a natively-built compiler disagree — §Context and §4 now carry it, with
> the fix scoped to the whole `Int` family rather than `div`/`mod` alone. §1 pins the zero-divisor
> values, which "`ToInt32` of the mathematical result" cannot determine. §4 adds `Purvasm.Int`'s
> module preamble. §Consequences narrows the stock-`purs` agreement claim to builds that actually
> reach `Purvasm.Int`. [0017](0017-primitive-ffi.md) gains a dated correction: it recorded
> `DivInt`/`ModInt` as truncating.
>
> **Revision (2026-08-17, review round 2):** `zshr` is stated as a **confirmed** second leak
> (registry `zshr (-1) 0 == 4294967295`), not a suspected one, and gains its own fold regression.
> §2/§Consequences scope the new divergence precisely — it is the **Euclidean** quotient at one input
> pair, not "the quotient operations", and it is not purvasm's only difference from the registry
> (`zshr`, `Data.Int.rem x 0`), merely the one this record introduces. §4's `mod` range argument is
> conditioned on `b ≠ 0`.

## Context

Building the owned VM's primops ([0110](0110-owned-vm-purescript-native.md) slice 1) surfaced a
disagreement between purvasm's own targets at exactly one input pair:

| producer of `Int` division | `bottom / (-1)` |
| --- | --- |
| boot's VM (`Vm.Machine.eval_prim`, `w32 (ediv …)`) | `-2147483648` |
| the Rust runtime (`runtime/src/prim.rs`, widens to `i64` then `wrap32`) | `-2147483648` |
| `Purvasm.Int`'s **JS** provider (`Math.floor(a / b)`, no `\| 0`) | `2147483648` |
| stock PureScript 6.0.2 on JS | `2147483648` |

`bottom / (-1)` is `2^31`, one past `Int`'s maximum. Two commitments meet here and appear to
conflict:

- **purvasm follows the PureScript ecosystem's semantics.** Prelude 4.x deliberately moved
  `EuclideanRing Int` from truncating to **Euclidean** division, and supplied `quot`/`rem` for the
  truncating pair ([purescript-prelude#168](https://github.com/purescript/purescript-prelude/pull/168)).
  That is a design decision by the PureScript team, not a JS implementation detail, and purvasm
  already honours it: the ulib Prelude's `euclideanRingInt` selects `Purvasm.Int.div`/`mod`.
- **`Int` is a 32-bit signed integer.** PureScript's own `Prim` documentation says so, and purvasm's
  native ABI *fixes* it: a sign-extended `i32` payload in `TaggedWord`, `pv_int(int32_t)`,
  `pv_int_payload() -> int32_t`, `i32` extraction in the LLVM backend.

The conflict is only apparent. Choosing Euclidean division does not entail letting a quotient escape
the type's range; the escape comes from the JS provider computing
`y > 0 ? Math.floor(x / y) : -Math.floor(x / -y)` and never normalising the result. Nothing upstream
documents that as intended, and `2147483648` is not a value the rest of the system can even hold —
`pv_int` takes an `int32_t`. So it reads as an unhandled overflow at the boundary, not as a
specification.

Two further facts, established while investigating, bound how much has to change:

- **The layering is already right.** `purvasm-base` sits *below* Prelude; the ulib Prelude selects
  `Purvasm.Int.div`/`mod` for its `EuclideanRing Int` instance. Nothing needs to be rearranged for
  Prelude to be the place where the *choice* of division lives.
- **The truncating pair already exists and already normalises.** The `integers` ulib defines
  `quot x y = PI.fromNumber (PN.div (PI.toNumber x) (PI.toNumber y))` and `rem x y = x - quot x y * y`.
  Because `quot` passes through `ToInt32` (`Purvasm.Int.fromNumber`), `quot bottom (-1)` is already
  `bottom`, and `rem` inherits `Int`'s wrapping arithmetic. There are no `QuotInt`/`RemInt` primops
  anywhere, and none are needed for correctness.

And one place where the same leak is *not* the provider's fault, found in review:

- **The optimiser folds constants with the host's `Prelude`.** `Nbe.Eval.foldPrim` evaluates
  `DivInt`/`ModInt` with plain `div`/`mod`, i.e. whichever `EuclideanRing Int` the compiler was
  *built* against. A node-built compiler uses the registry's `intDiv` and folds `bottom / (-1)` to
  `2147483648`; the same compiler built natively resolves `Prelude` through the ulib to
  `Purvasm.Int.div` and folds it to `bottom`. So the constant a program gets depends on how its
  compiler was compiled — which is a self-hosting fixpoint hazard, not merely a target difference,
  and it is not closed by fixing a provider.

## Decision

### 1. `Int`'s 32-bit invariant is absolute

Every operation that yields an `Int` yields `ToInt32` of the mathematical result, on every target and
in **both** division families. In particular `bottom / (-1) == bottom` and `quot bottom (-1) == bottom`.

**Where the mathematical result does not exist, the value is pinned here**, since §1's rule cannot
determine it. Division by zero is total in both families, and the two families answer differently:

| | `x / 0` | remainder of `x` and `0` |
| --- | --- | --- |
| Euclidean (`div` / `mod`) | `0` | `0` |
| truncating (`quot` / `rem`) | `0` | `x` |

`rem x 0 == x` is not an oversight to be tidied into `0`: it is what the present derivation yields,
and it is the one that keeps the division identity `x == y * quot x y + rem x y` true at `y == 0`
(`0 * 0 + x == x`), which Euclidean's `mod x 0 == 0` does not (`0 * 0 + 0 ≠ x`). Both are kept as they
are, and both are tested.

The invariant is a property of the *type*, not of a backend: a value outside the range is not an
`Int`, and no purvasm representation can carry one. Every other arithmetic operation in
`Purvasm.Int` already normalises (`add`/`sub` with `| 0`, `mul` via `Math.imul`, `zshr` with an
explicit re-wrap whose comment states the reason); `div` is the one that does not, and that is the
defect.

### 2. The divergence from stock PureScript's JS backend is deliberate and recorded

At `bottom / (-1)` — the Euclidean quotient, one input pair — a purvasm build and a stock-`purs`
build disagree, and purvasm is the one that changes. This is **not** a general licence to diverge —
the rule stays "follow the ecosystem's semantics". What is being declined is the reproduction of an
unhandled boundary case that violates the type's own contract, in the one provider that has it. The
native path needs no change: it already complies.

The same reasoning already applied elsewhere without being written down — the ulib's `zshr` and
`Data.Int.rem` keep `Int` in range where the registry's do not (§Consequences) — so this record
states an invariant those were already honouring rather than opening a new kind of exception.

The divergence is recorded here, tested at the boundary (§4), and documented where the provider is
defined.

### 3. `purvasm-base` provides the mechanism; Prelude chooses the semantics

`purvasm-base` is upstream of Prelude and must not privilege one division family. It carries **both**
pairs as peers:

- Euclidean `div` / `mod` — non-negative remainder, total on a zero divisor;
- truncating `quot` / `rem` — toward zero, total on a zero divisor.

Prelude's `EuclideanRing Int` selects the first (already the case); `Data.Int` exposes the second.
The `integers` ulib's present definitions move down into `purvasm-base`, and `Data.Int.quot`/`rem`
become re-exports, so there is one definition of each rather than a per-library one.

**`quot`/`rem` stay derived, not intrinsic.** Making them machine primops would add `PrimOp`
constructors to the bytecode format, the LLVM backend, the runtime, and boot — which is *frozen*
([0104](0104-retire-boot-byte-identity-gate.md) §1) — for no correctness gain, since the derived
definitions already satisfy §1. They are peers of `div`/`mod` in the **API**, which is what the
layering requires; whether they should also be peers in the **machine** is a performance question,
deferred until a measurement asks for it.

### 4. What changes

- `packages/purvasm-base/src/Purvasm/Int.js`: `div` normalises its result (`| 0`), with a comment
  naming this record and the stock-`purs` divergence. `mod` is unchanged and needs no normalisation —
  for `b ≠ 0` its result lies in `[0, |b|)` with `|b| ≤ 2^31`, so it cannot leave the range, and for
  `b == 0` it is the explicit `0` of §1's table.
- `packages/purvasm-base/src/Purvasm/Int.purs`: gains `quot`/`rem` beside `div`/`mod`, documented as
  the truncating pair, with the two families presented as peers. **The module's own preamble changes
  too**: it currently says every export is an intrinsic the backend resolves to a primop, which stops
  being true the moment a derived pair joins it. It becomes the low-level `Int` API — intrinsics
  where the machine has one, definitions built on them where it does not — and each member says which
  it is.
- `ulib/integers/Data.Int.purs`: `quot`/`rem` re-export the `purvasm-base` definitions.
- `compiler/src/Purvasm/Compiler/MiddleEnd/Optimizer/Nbe/Eval.purs`: `foldPrim` stops folding `Int`
  operations with the host's `Prelude` and delegates to `Purvasm.Int` (the compiler already depends
  on `purvasm-base`). The scope is the **whole `Int` family, not just `div`/`mod`**: the hazard is
  "the fold uses whatever provider the compiler was built against", and `zshr` is a **confirmed**
  second instance: the registry's `Data.Int.Bits.zshr` is `n >>> m`, so `zshr (-1) 0` is
  `4294967295` — outside `Int` — where purvasm answers `-1`. Delegating the family closes the class
  rather than the cases we happened to find, and makes the fold agree with the machine it folds *for*.
- `vm/src/Purvasm/VM/Prim.purs`: the temporary `Int.or (Int.div a b) 0` normalisation is **removed** —
  it exists only because the provider is out of contract, and the VM goes back to delegating.
- Regression tests at each boundary:
  - `purvasm-base`'s own, for the four division operations at `bottom` and at a zero divisor (§1's
    table);
  - the **NbE fold**, that `DivInt bottom (-1)` folds to `bottom` and `ZshrInt (-1) 0` folds to `-1` —
    the two known cases that must hold identically whether the compiler was built on node or
    natively;
  - the VM's existing `Purvasm.VM.Prim` case, which stays: it pins the contract the VM depends on,
    wherever the implementation lives.

Not changed: boot, the runtime's `ediv`/`emod`, the primop set, the C ABI, the value representation,
and the compiler's **backends** — the only compiler change is the NbE constant folder above, which
alters no lowering, only which implementation evaluates a fold.

## Consequences

- The behavioural difference from stock `purs` that **this record introduces** is one input pair of
  the **Euclidean quotient**: `bottom / (-1)`. That scoping is deliberate on both sides. It is not
  purvasm's only difference from the registry — the ulib already answers `-1` where the registry's
  `zshr (-1) 0` gives `4294967295`, and `x` where the registry's `Data.Int.rem x 0` gives JS's
  `x % 0`, i.e. `NaN`. Those are pre-existing, and this record merely writes down the invariant they
  were already honouring. Nor is it "the quotient operations" in general: `quot bottom (-1)` already
  wraps on stock, whose `quot` passes through `| 0`.

  What the change buys is agreement among purvasm's **own** targets: the same program compiled
  natively, run under the owned VM, and constant-folded by the optimiser now give the same answer.
  The claim stops there, deliberately. An ordinary program built by stock `purs` keeps using the
  *registry* Prelude's `intDiv` and still answers `2147483648`; only a build that reaches
  `Purvasm.Int.div` — through the ulib, or by importing it directly — picks this up. Closing that gap
  for arbitrary registry code is not a goal, and could not be done from here.
- `purvasm-base` stops encoding a preference between the two division families, so a future library
  that wants truncating division has a peer to select rather than a re-derivation to write.
- The owned VM's local normalisation disappears, restoring "the intrinsic is the single semantics
  source" ([0110](0110-owned-vm-purescript-native.md) §3's spirit for arithmetic).
- `quot`/`rem` remain a `Number` round trip on every target. That is a real cost on the native path
  (two conversions and a float divide for an integer operation) and is the thing a future measurement
  would use to justify promoting them to primops.
- Anything that relied on `bottom / (-1) == 2147483648` changes answer. Nothing can reasonably rely on
  it: the value is not an `Int`, and on the native path it was never produced.

## Alternatives considered

- **Leave the provider and normalise in each consumer** (what the VM does today, as a stopgap).
  Rejected: every consumer would re-implement the invariant, and — worse — ordinary PureScript code
  compiled by stock `purs` would still disagree with the same code compiled natively. The defect is
  in one place and belongs fixed there.
- **Widen `Int`'s payload so `2^31` is representable.** Raised while diagnosing, and withdrawn: it
  would cross the value representation ([0059](0059-native-abi-value-representation.md)), the C ABI
  (`pv_int`/`pv_int_payload`), the LLVM backend's `i32` extraction, comparisons, `IntToNumber`,
  display and FFI marshalling — all to preserve a value PureScript itself says is not an `Int`.
- **Match stock PureScript exactly, leak included.** Rejected: "follow the ecosystem" is about
  semantics the PureScript team *chose* — Euclidean division is such a choice and purvasm keeps it —
  not about reproducing an unhandled boundary case that the language's own definition of `Int`
  contradicts and that the native representation cannot hold.
- **Promote `quot`/`rem` to machine primops now.** Rejected for this record: it touches frozen boot,
  the bytecode format, the runtime and the backend without changing any answer. Left as a measured
  follow-up (§3).
