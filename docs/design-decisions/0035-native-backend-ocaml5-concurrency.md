# 0035. Native backend via OCaml 5 codegen; an M:N effect-handler concurrency runtime

- Status: Accepted
- Date: 2026-06-25

## Context

The grand goal is a *true concurrent native runtime* for PureScript. `boot` (the
OCaml-hosted implementation — CESK oracle ADR-0002, bytecode VM ADR-0030..0032,
separate compilation ADR-0033, effect analysis + DBE ADR-0034) is a **bootstrap means,
not the final artifact**: the plan ([[boot-then-selfhost-bootstrap]]) is to finish
`boot` to native fast, reimplement purvasm in PureScript (self-host) on it, then build
the bespoke native runtime once the ABI is fixed.

Two consequences drive this ADR:

- **The optimiser is paused.** Its OCaml code is throwaway (only its *design* ports to
  the self-host); polishing it on a stepping-stone VM has low return. The remaining arc
  (study → specialisation → NbE inliner, [[optimizer-roadmap]]) is shelved; passes are
  added **on-demand**, only when bootstrap performance forces it. The current
  DictElim + Simplify + DBE + effect-analysis is the "moderate" baseline.
- **Go native now.** The hardest, most novel part of the goal is the *parallel runtime*
  (scheduler + continuations + GC + ABI). Tackle it early to de-risk — but cheaply, by
  standing on existing technology rather than building bespoke first.

**Prior art surveyed** (full notes:
[sidenote 0006](sidenotes/0006-concurrent-runtime-case-study.md)):

- **Go** — M:N work-stealing scheduler, cheap goroutines, channels, growable stacks.
- **OCaml 5** — domains (parallelism), effect handlers (one-shot delimited continuations
  → direct-style fibers), multicore GC. The substrate boot stands on.
- **GHC RTS** — a *Capability* is a floating "permission to run Haskell" (a run queue of
  green threads + a nursery) that OS threads (`Task`s) acquire/release, so the
  green-thread scheduler is **RTS-native** and a blocking FFI call **hands the Capability
  off** to another OS thread while the rest keep running. Also Spark/work-stealing,
  `Blackhole` (a thunk under evaluation by one core blocks others — the parallel form of
  our by-need `Building` cell, ADR-0024), native `MVar`/STM. The closest functional
  template; we are *strict*, so Sparks matter less, but the structure transfers.
- **Cats Effect 3** — a CESK-like fiber state machine (continuations as heap objects in
  a trampolined loop), a fiber-dedicated work-stealing scheduler, and **cooperative
  preemption** by auto-yielding every N binds. The best "`Aff` compiled to a native M:N
  runtime" reference.
- **Erlang/BEAM** — share-nothing, one-process-one-heap (no global stop-the-world GC),
  **reduction-count preemption** (forcibly deschedule at ~4000 reductions).
- **Rust/Tokio** — async I/O (`io_uring`/`epoll`/`kqueue`) wired into the work-stealing
  loop via a **Waker**; compile-time future state machines.
- **Flix** — channels/processes; a *typed* effect system we **cannot** use (CoreFn is
  type-erased, ADR-0034), so concurrency stays a runtime/library concern.

## Decision

### boot's native backend = OCaml 5 codegen

Lower optimised ANF (ADR-0025) to **OCaml source**, compiled by `ocamlopt` to a native
executable. `boot` stands on **OCaml 5's runtime** instead of a bespoke one, reusing its
**domains** (parallelism), **effect handlers** (concurrency), and **multicore GC**.

### Target concurrency/runtime model

The model both boot and the eventual bespoke runtime aim at, synthesised from the prior
art. Each dimension gives the *target*, then what **boot** gets free from OCaml 5 vs
what the **bespoke** phase must build.

- **Thread model** [Go, GHC] — *target:* M:N work-stealing, one execution slot per core
  (a *Capability*). *boot:* domains + a WST scheduler (Domainslib/Eio, or a thin own
  layer). *bespoke:* own Capability scheduler. Note the key difference: a GHC Capability
  **floats** across OS threads, whereas an OCaml **domain is bound 1:1 to its OS
  thread** — so on boot a "Capability" is really *domain + scheduler*, and a domain
  cannot be handed off (see the blocking gap below).
- **Fiber representation** [OCaml 5] — *target:* stack-based one-shot delimited
  continuation. *boot:* OCaml effect handlers. *bespoke:* own continuation repr.
- **Fiber communication** [GHC `MVar`, CE3] — *target:* `AVar` (MVar-like async cell) as
  the core blocking primitive, with channels/queues built on it. *boot:* `AVar` over
  effect-handler blocking. *bespoke:* own `AVar`/channels.
- **Preemption** [BEAM, CE3] — *target:* cooperative **plus** reduction-count auto-yield
  so a pure loop cannot starve a domain. *boot:* cooperative only (yields at `await`) —
  a known **gap**. *bespoke:* reduction-count yields injected at codegen.
- **Memory / GC** [Erlang, GHC] — *target:* capability-local heap + a shared *immutable*
  heap (purity makes immutable data freely shareable; avoid a global stop-the-world).
  *boot:* OCaml's shared multicore GC. *bespoke:* per-capability + shared-immutable GC.
- **Async I/O** [Rust/Tokio] — *target:* an `io_uring`/`epoll` driver + Waker wired into
  the WST loop. *boot:* Eio (`io_uring`). *bespoke:* own I/O driver.

A fiber blocks by *performing* an effect (`take` an empty `AVar`, `await` I/O); the
scheduler is the handler that captures the continuation and reschedules it, woken by the
corresponding `put`/completion — direct-style, no callback/CPS hell. Purity is the lever
throughout: immutable data is freely shared across domains; only *mutable* state
(`Ref`, `AVar`) needs care, and the model steers toward "share by communicating".

For boot this is **OCaml 5 directly** — the table's middle column. Two honest gaps, both
rooted in OCaml domains being **bound 1:1 to their OS thread** (unlike GHC's floating
Capabilities), so a domain that stops takes its fibers with it:

- **Preemption.** OCaml does not auto-yield, so a long *pure* PureScript loop (no
  `await`) never cooperatively yields and can hog a domain. The bespoke phase closes it
  with reduction-count yields injected at codegen (BEAM/CE3).
- **Blocking handoff.** A blocking syscall freezes the whole domain (all its fibers) —
  there is no GHC-style Capability release. boot mitigates by doing I/O through Eio's
  async layer (never truly blocking); the bespoke Capability could add real handoff.

boot accepts both — adequate for bootstrap; a single runaway pure loop or an
un-Eio'd blocking call is the failure mode to keep in mind.

### PureScript concurrency = `Aff` on fibers

`Effect` runs on the current fiber; **`Aff`** — already a fiber monad
(`forkAff`/`killFiber`) — maps onto runtime fibers. The mutable primitives split by
scope: **`Ref`** is the *synchronous, intra-fiber* cell (get/set, never blocks);
**`AVar`** is the *asynchronous, inter-fiber* cell (MVar-like: `take` parks the fiber
when empty, `put` when full) — PureScript's existing fiber-communication primitive, on
which channels/queues are built. So fork/`AVar`/`await` are **runtime functions** that
`Aff` (or a new library) consumes. Effects are **not type-tracked**: CoreFn is
type-erased (ADR-0034), so — unlike Koka/Flix — the primitives are ordinary runtime
functions, not type-enforced (as `Aff` already is in PureScript).

### Explicitly deferred to the self-host / native-ABI phase

- The **bespoke native runtime** — own codegen (C/LLVM/…), own GC, own M:N scheduler,
  own continuation representation — built later in PureScript, informed by the OCaml-5
  path. Its ABI is where the **native-C FFI**, the foreign-vs-guest **arity-duality
  unification**, and **EffectFnN** land ([[ffi-foreign-provider-ladder]],
  [[effect-force-saturation-model]]).
- **`Ref`/`AVar`-across-domains semantics** (domain-local vs atomic/synchronised) and the
  exact `AVar`/channel/`Aff` API — design alongside the codegen.
- **Reduction-count preemption** (BEAM/CE3 style, injected at codegen) — the bespoke
  fairness fix for pure loops; boot ships cooperative-only.
- **Floating Capabilities with blocking handoff** (GHC-style) — so a blocked OS thread
  releases its execution slot; boot's domains are bound 1:1, mitigated by Eio.
- **Share-nothing / per-capability heaps** (Erlang/GHC) — a later GC-contention win;
  boot uses OCaml's shared multicore GC.

### Validation

The OCaml-codegen native backend is held to the **same differential discipline**: it
must agree with the CESK oracle (and the bytecode VM, which stays as the reference) on
value and `Effect` order for every fixture/benchmark. Concurrency adds non-determinism,
so concurrent tests assert on the *set* of allowed outcomes / linearisable results,
not a single interleaving.

### Follow-up ADRs (not decided here)

The **ANF → OCaml value/representation mapping** (closures, the `Effect` thunk, ADTs,
records/arrays, how `Ffi.host` leaves bind) and the **channel/Aff/scheduler API** are
their own ADRs, drafted next.

## Consequences

- Fastest path to a real parallel native PureScript runtime — OCaml 5 *is* one, with
  the exact effect-handler mechanism targeted; boot reaches native via `ocamlopt`.
- The concurrency model and the `Effect`/`Aff` mapping get validated end-to-end early,
  de-risking the grand goal cheaply.
- boot's optimiser stops at "moderate"; correct (if not maximally fast) native output is
  fine for bootstrap, and perf work returns on-demand or in the self-host.
- boot couples to OCaml's runtime/value model and FFI; **boot's ABI ≈ OCaml's**, and the
  bespoke ABI is a later, separate effort (explicitly deferred).
- Two backends to keep differentially-equal (bytecode VM + OCaml-codegen native) — but
  that is the project's existing oracle discipline, not new overhead.

## Alternatives considered

- **Bespoke native runtime now** (C/LLVM + own GC/scheduler/continuations). The ultimate
  goal, but the largest and riskiest effort — it reimplements what OCaml 5 gives for
  free. Premature for boot; chosen instead for the self-host/native phase.
- **Keep optimising the bytecode VM; defer native.** The VM is a throwaway stepping
  stone; polishing its optimiser delays the real native risk and produces throwaway
  code. Rejected — pause the optimiser, go native.
- **1:1 OS threads / N:1 event loop.** 1:1 cannot scale to massive concurrency; N:1 has
  no real parallelism. M:N gets both (the standard choice).
- **Callback/CPS async (no effect handlers).** Works but composes poorly (callback
  hell); effect handlers give direct style and OCaml 5 already provides them.
- **Type-tracked effects (Koka/Flix-style) to drive the runtime/optimiser.** Unavailable
  — CoreFn is type-erased (ADR-0034); effect information is structural only. Concurrency
  stays a library/runtime concern, not a type-enforced one.
