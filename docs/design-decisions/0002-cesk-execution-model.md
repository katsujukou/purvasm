# 0002. Start phase 1 with a CESK machine over a minimal strict core

- Status: Proposed
- Date: 2026-06-16

## Context

Phase 1 (see [0001](0001-phase-1-host-language-ocaml.md)) needs a first
execution model. Two questions had to be settled before any code:

1. **What altitude to start at.** Jump straight to a bytecode VM, or first build
   a higher-level interpreter that pins down evaluation, effect, and scheduling
   *semantics* as an executable specification?
2. **Which machine shape.** The project is literally an "abstract machine", the
   flagship feature is true parallel async, and a locked decision calls for
   per-fiber heaps with a local GC. Whatever we start with must make the
   continuation and the heap *first-class*, because async/await, effect
   handlers, and lightweight fibers are all built by saving and resuming
   continuations, and the GC needs an explicit heap to manage.

PureScript is strictly evaluated, so the core needs no thunk/laziness
machinery — a conventional strict functional core suffices.

## Decision

Start phase 1 with a **small-step CESK machine** over a minimal strict core
(literals, variables, lambda, application, `let`, `if`, primitives),
implemented as an *executable specification* (phase 1a) before any bytecode.

- **C**ontrol: a `focus` that is either `Eval (term, env)` or `Return value`.
- **E**nvironment: lexical bindings, `name → address`.
- **S**tore: a heap, `address → value`, with the `name → address → value`
  indirection kept even though it looks redundant for a pure language.
- **K**ontinuation: an explicit, **defunctionalized** stack of frames
  (`Fun`/`Arg`/`Let_body`/`If_branch`/`Prim_args`/`Halt`) — first-order data,
  not host closures.

Every transition is total and explicit; no host-language recursion hides inside
the evaluation of a subterm.

### Transition rules

A state is `⟨focus, σ, κ⟩` where `σ` is the store and `κ` the continuation; `ρ`
is the environment carried by an `Eval` focus. `σ(ρ(x))` looks `x` up to an
address in `ρ`, then that address to a value in `σ`. `α` denotes a fresh
address; `σ[α ↦ v]` extends the store; `ρ[x ↦ α]` extends the environment.
`δ(op, vs)` is the primitive operation. `rev` reverses a list.

**Eval mode** — `⟨Eval(t, ρ), σ, κ⟩` steps by the shape of `t`:

| `t` | next state |
| - | - |
| `Lit (LInt n)` | `⟨Return (VInt n), σ, κ⟩` |
| `Lit (LBool b)` | `⟨Return (VBool b), σ, κ⟩` |
| `Var x` | `⟨Return σ(ρ(x)), σ, κ⟩` |
| `Lam(x, e)` | `⟨Return (VClosure{x, e, ρ}), σ, κ⟩` |
| `App(f, a)` | `⟨Eval(f, ρ), σ, Fun(a, ρ, κ)⟩` |
| `Let(x, e₁, e₂)` | `⟨Eval(e₁, ρ), σ, Let_body(x, e₂, ρ, κ)⟩` |
| `If(c, t, e)` | `⟨Eval(c, ρ), σ, If_branch(t, e, ρ, κ)⟩` |
| `Prim(op, a :: rest)` | `⟨Eval(a, ρ), σ, Prim_args(op, [], rest, ρ, κ)⟩` |
| `Prim(op, [])` | `⟨Return δ(op, []), σ, κ⟩` (stuck in practice: no nullary ops) |

**Return mode** — `⟨Return(v), σ, κ⟩` steps by the shape of `κ`:

| `κ` | condition | next state |
| - | - | - |
| `Halt` | | **done** — `v` is the final result |
| `Fun(a, ρ, κ′)` | | `⟨Eval(a, ρ), σ, Arg(v, κ′)⟩` |
| `Arg(v_f, κ′)` | `v_f = VClosure{x, e, ρ_c}` | `⟨Eval(e, ρ_c[x ↦ α]), σ[α ↦ v], κ′⟩` |
| `Arg(v_f, κ′)` | otherwise | **stuck** — application of a non-function |
| `Let_body(x, e₂, ρ, κ′)` | | `⟨Eval(e₂, ρ[x ↦ α]), σ[α ↦ v], κ′⟩` |
| `If_branch(t, e, ρ, κ′)` | `v = VBool true` | `⟨Eval(t, ρ), σ, κ′⟩` |
| `If_branch(t, e, ρ, κ′)` | `v = VBool false` | `⟨Eval(e, ρ), σ, κ′⟩` |
| `If_branch(…)` | `v` not a boolean | **stuck** |
| `Prim_args(op, done, [], ρ, κ′)` | | `⟨Return δ(op, rev(v :: done)), σ, κ′⟩` |
| `Prim_args(op, done, a :: rest, ρ, κ′)` | | `⟨Eval(a, ρ), σ, Prim_args(op, v :: done, rest, ρ, κ′)⟩` |

The only rules that allocate (`σ[α ↦ v]`) are `Arg` (function application) and
`Let_body`; everything else threads `σ` unchanged. This is why, in a trace, the
store grows exactly at applications and `let` bindings.

## Consequences

- The reified **K is the keystone**: because the continuation is data, it can be
  saved, resumed, and scheduled — the substrate for effect handlers, async, and
  fibers. This is the same structure as `purescript-aff`'s `bhead/btail` +
  `attempts` and Cats Effect 3's `conts` stack, realized at the machine level
  rather than the monadic-bind level.
- The **Store is the GC seam**: it is isolated behind a small interface
  (`empty`/`alloc`/`find`), so replacing the ever-growing map with a per-fiber
  copying collector later need not change the transition rules. (Caveat: a
  moving collector will additionally need the machine's roots — env, kont,
  focus — so a roots interface will be added; the *allocation/lookup call sites*
  in the transition rules stay put.)
- **Bytecode becomes a compiled realization** of these transitions (control → an
  instruction pointer; transition rules → the interpreter loop), so starting
  with CESK does not discard the "abstract machine + bytecode" plan — it
  sequences it.
- Choosing defunctionalized continuations (vs higher-order closures) keeps the K
  inspectable and serializable, which suits later scheduling and a bytecode
  encoding.
- Not yet covered, deliberately: recursion (`letrec`/`fix`), algebraic data
  types and pattern matching, effect handlers, a real GC, a CoreFn frontend, and
  bytecode. These are follow-on slices.

## Alternatives considered

- **Tree-walking interpreter using the host call stack.** Simplest, but the
  continuation is *not* reified — there is nothing to save/resume — so it cannot
  grow into effect handlers or fibers without a rewrite, and it is a poor fit
  for a project whose subject is the machine itself.
- **CEK (no store).** Drops the `name → address → value` indirection. Cleaner
  for a pure language, but discards the explicit heap that the per-fiber GC will
  manage; the store is wanted from day one precisely as that seam.
- **Straight to a bytecode VM.** The eventual target, but premature before the
  evaluation/effect/scheduling semantics are stable; harder to read and to test
  as a specification while the language is still moving.
