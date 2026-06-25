# Case study: concurrent / parallel runtimes (prior art for a native PureScript runtime)

- Date: 2026-06-25
- Status: background survey; feeds ADR-0035 (native backend + concurrency runtime)
- Scope: prior-art notes for a *true parallel* native runtime for PureScript — surveying
  the core architectures worth borrowing from, and what each implies for us.
- Related: [ADR-0035](../0035-native-backend-ocaml5-concurrency.md) (the decisions this
  informs), ADR-0024 (by-need recursive bindings — cf. GHC `Blackhole`), ADR-0034
  (effect analysis; CoreFn is type-erased, so typed effect systems are unavailable).

PureScript's strengths — purity and a strong type system, especially *controlled
effects* — make "a non-blocking runtime that enables true parallelism" a powerful target
for a native backend. Beyond the starting pair already chosen — **OCaml 5** (algebraic
effects for lightweight continuations) and **Go** (M:N work-stealing) — the following are
the modern concurrent/parallel runtimes whose core architecture is worth studying.

## 1. GHC Haskell (RTS: execution model & Capability)

The runtime of PureScript's "elder sibling" is the deepest reference.

- **Capability as an abstraction layer.** Over OS threads (`Task`s), the RTS maps
  `Capability`s (virtual processors), one per physical core (`-N`). A Capability is a
  *floating* "permission to run Haskell" carrying a run queue of green threads (`TSO`s)
  plus a nursery; OS threads **acquire/release** it. Hence the green-thread scheduler is
  **RTS-native**, and a blocking (safe) FFI call **hands the Capability off** to another
  OS thread so the rest keep running.
- **Spark & work-stealing.** Parallel evaluation uses lightweight `Spark` pointers to
  thunks, work-stolen from per-Capability FIFO/LIFO queues.
- **Blackhole & blocking.** When one core hits a thunk another core is already
  evaluating, the thread is suspended (semantic blocking) and woken on completion.
- **Insight for us.** We are *strict* (so Sparks matter less), but the memory layout of a
  pure functional language, a scheduler tightly coupled with a parallel GC, and native
  `MVar`/STM are direct templates. Blackhole is the **parallel form of our by-need
  `Building` cell** (ADR-0024): a CAF/thunk forced concurrently by two domains needs the
  same "one builds, others block" discipline, now with real synchronisation.

### Capability vs OCaml domain (the key distinction)

Both are per-core execution units, but: a GHC **Capability floats** across OS threads
(acquire/release; #OS-threads can exceed #Capabilities; blocking FFI hands it off, and
the green-thread scheduler is built into the RTS). An OCaml **domain is bound 1:1 to its
OS thread** (a domain *is* a parallel OS thread with its own minor heap; it does not
float), and lightweight fibers are *not* built in — they are a library concern (Eio /
Domainslib) layered on domains via effect handlers. Consequence: on a domain, a blocking
syscall or a long pure loop freezes the domain *and all its fibers*, with no automatic
handoff — the two gaps ADR-0035 records for boot.

## 2. Scala / Cats Effect 3 (Fiber & work-stealing executor)

- **Relation to CESK.** Conceptually close to a CESK machine: the `IO` runtime is a large
  state machine (a trampolined / bounce-pointer loop) over fiber continuations kept as
  heap objects.
- **Compute pool & work-stealing.** A work-stealing scheduler **dedicated to fibers** is
  built in-house, separate from the JVM's general thread pool (ForkJoinPool).
- **Automatic (cooperative) preemption.** To stop a fiber monopolising a thread on, e.g.,
  an infinite loop, an automatic `yield` is inserted every N effects (flatMaps) —
  built in at the codegen/runtime level.
- **Insight for us.** CE3 is the best textbook for "compile a monadic effect (`Effect` /
  `Aff`) to a native M:N runtime": how to drive **the cost of heap-ifying a continuation
  (fiber-isation) toward zero**, and how to **inject cooperative preemption**.

## 3. Erlang / Elixir (BEAM VM)

The ancestor of the actor model; the extreme of concurrency.

- **Share-nothing, one heap per process.** Processes share no memory; they communicate by
  message passing only. This **eliminates a global stop-the-world GC** — each process GCs
  its own tiny heap independently.
- **Reduction-based preemption.** Function calls / loop iterations are counted as
  "reductions"; at a threshold (~4000) the process is forcibly descheduled mid-work
  (genuinely preemptive).
- **Insight for us.** For *true* parallelism the biggest bottlenecks are **cross-core
  data sharing (mutable refs, thread safety) and GC contention**. A share-nothing or
  one-heap-per-core design lifts parallel throughput dramatically. PureScript's
  immutability helps: immutable data can be shared by reference across cores without
  copying — only mutable state (`Ref`, `AVar`) needs care.

## 4. Rust (Tokio / mio) & modern async I/O

Unavoidable when building the native backend in C/Rust/LLVM directly.

- **epoll / kqueue / io_uring integration.** The work-stealing loop is wired directly to
  the OS's event notification (`io_uring` / `epoll`).
- **Zero-cost futures.** Rust generates the state machine statically at compile time,
  minimising runtime allocation.
- **Insight for us.** OCaml 5's algebraic effects handle stack-based one-shot delimited
  continuations cheaply, but the open problem is **how that cooperates with the event
  loop when an OS I/O wait (socket, etc.) occurs under multicore parallelism**. Studying
  how Tokio packs a **Waker** together with work-stealing clarifies the native-I/O
  picture.

## 5. Flix

Channels / processes (Go-inspired: `spawn`, channels, `select`) plus a **typed effect
system**. The effect system is the part we **cannot** reuse: we ingest type-erased CoreFn
(ADR-0034), so effects are not type-tracked. Concurrency stays a runtime/library concern
(as PureScript's `Aff` already is), recovered structurally rather than from types.

## Synthesis: a design matrix for a PureScript-strength runtime

| Component | Approach to adopt | From |
| - | - | - |
| Thread model | M:N work-stealing; one OS thread per core (a Capability) | Go, GHC |
| Lightweight green thread | algebraic-effects stack-based continuation (fiber) | OCaml 5 |
| Preemption | compile-time reduction-count cooperative yield | Erlang, Cats Effect |
| Fiber communication | `AVar` (MVar-like async cell) as the core blocking primitive; channels built on it | GHC `MVar`, CE3 |
| Memory & GC | capability-local heap + shared *immutable* heap (avoid parallel-GC contention) | Erlang, GHC |
| Async I/O | an `io_uring` / `epoll` driver thread + Waker mechanism | Rust (Tokio) |

The decisive engineering question is how to **compile-time transform** PureScript's
existing `Aff` (asynchronous effects) onto such a native M:N runtime. `Aff` is already a
fiber monad (`forkAff` / `killFiber`), and the mutable primitives split by scope —
`Ref` is the synchronous intra-fiber cell, `AVar` the asynchronous inter-fiber one — so
the mapping onto fibers + the `AVar`/scheduler primitives is direct.
