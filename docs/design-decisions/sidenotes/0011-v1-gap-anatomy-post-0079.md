# Investigation: the v1 `llvm/ml` gap after ADR-0079 — anatomy, remaining levers, and where each one now lives

- Date: 2026-07-05
- Status: measured state + agreed sequencing; the working map for the next performance session.
- Related: [ADR-0076](../0076-direct-known-arity-calls-musttail.md) /
  [ADR-0077](../0077-cross-module-direct-calls-pmi-arity.md) /
  [ADR-0079](../0079-ctx-header-abi-inline-rooting-fast-paths.md) (the lever history, each with a
  measured Progress note), [ADR-0072](../0072-anf-to-llvm-lowering.md) §6/§7 (the reservations the
  levers cash in), [ADR-0034](../0034-effect-optimization.md) (why the effect bit has no safe
  default), [sidenote 0008](0008-self-compile-profiling.md) (the profile-first discipline that
  found the earlier cliffs).

## Where the gap stands

The v1 boundary-cost arc is essentially complete. A `fib` iteration crossed the runtime
`extern` boundary **~30 times** before the arc and **~2–3 times** after it (one `pv_apply` for a
PAP-CAF callee, one–two `pv_read_field` for closure envs); everything else — frame open/close,
root/get, settle, the immediate-force check, the scalar primops — is now inline IR under `-O2`.

Measured per lever (paired interleaved runs, min-of-5 — see "measurement discipline" below):

| lever | result |
|---|---|
| 0076 module-local direct calls + `musttail` | 1.4–1.7× |
| 0077 cross-module direct calls | ≈1.0× (hot loops were already local-direct; sites are cold — the honest negative that redirected the arc) |
| `clang -O2` alone | ≈1.0× (extern walls blocked all optimisation) |
| inline scalar primops (0072 §7) | ≈1.0× (2 of ~30 crossings) |
| 0072 §6 slice 1 (need-driven `eval_atoms`, uncaptured globals) | 1.06–1.16× |
| inline `force` immediate check | 1.01–1.05× |
| 0079 ctx-header inline (root/get/frame/pop/settle) | **1.10–1.33×** |

The lesson the ≈1.0× rows teach: individual boundary removals are invisible while ~30 crossings
remain; the payoff only lands when a whole *class* of crossings dies (0079). What remains is no
longer boundary **count** — it is the items below.

## The remaining v1 levers, and where each lives after the boot freeze

Boot (Level 1) is **frozen** as of 2026-07-05: no codegen/optimization investment except
Level-2+-blocking fixes. That splits the remaining levers by home:

**Codegen-side — wait for the native-codegen port to Level 2, then implement in PureScript:**

1. **Liveness-based rooting (0072 §6 endgame; biggest expected win).** `Let`-bound values and
   parameters still root unconditionally (root-on-create); the correct criterion is *root only a
   value live across a safepoint*. In `fib`, almost nothing qualifies (a `sub` result consumed as
   the very next call's argument needs no root). A small ANF liveness analysis; no ABI change;
   the standing differential (forced-GC fixtures included) is the gate. Note the division of
   labour with LLVM: `-O2`'s CSE can merge redundant header *loads* between calls, but a root
   *store* is an observable side effect LLVM must preserve (the GC really reads it) — only the
   codegen, which knows where safepoints are, can delete the root itself.
2. **`pv_read_field` inline fast path.** Every closure-env/field read is an extern call plus the
   runtime's checked-deref (object-start bitmap). Inlining needs object-header/slot layout
   exposure — a 0079-shaped ABI extension → measurement-gated ADR after (1).
3. **Allocation inline (bump-pointer exposure).** The big one; moves the safepoint discipline
   into codegen — a full ADR, judged on the residual profile after (1)/(2).
4. **Effect/GER on native, codegen half** ([0067](../0067-v1-effect-execution-and-native-leaves.md)
   §7). Effect-chain-shaped code (`count-state` was consistently the worst ratio) allocates a
   closure per step.

**Runtime-side (Rust) — NOT frozen, can land anytime, independent of the port:**

5. **`pv_apply`-internal fast path**: front-load the "saturated plain closure" case inside the
   entry (no ABI change). Palliative for the PAP-CAF traffic until the optimizer cures it.
6. **Heap growth on overflow** ([0066](../0066-v1-heap-and-gc.md) §4's reserved increment) —
   retires the `--heap-words` friction; robustness, not speed.
7. **Effect/GER runtime half**, and Aff/fiber groundwork (the B-runtime track).

**Optimizer-side (the real cure for the biggest residual):**

8. **PAP CAFs** — `lessThan = Data.Ord.lessThan ordInt` evaluated at init into a PAP whose call
   is irreducibly generic *in codegen* (the arity is a runtime fact). The fix is caller-homed
   specialization / eta-expansion of CAF-of-known-PAP — the optimizer roadmap's next application,
   behind the standing study-first gate (read `purs-backend-optimizer` before building).

## Measurement discipline (hard-won, do not relearn)

- **Paired interleaved runs only** (alternate binaries in one loop, min-of-K). This machine
  drifts between ≥3 absolute-time states for the *same binary* (observed 3.5 s / 6.0 s / 31 s on
  one workload): session-to-session and solo-vs-full-sweep comparisons produced phantom
  1.1–1.7× readings and one phantom 5×, all of which evaporated under pairing.
- The bench per-leg cache rebuilds when artifacts are older than `purvm` (a stale-toolchain
  reuse once produced a whole table of dead numbers); `benchmarks.yaml` (workflow_dispatch +
  weekly cron) is the thermally boring trend line — within-run ratios only, never absolute
  times across runs.

## Agreed sequencing (2026-07-05)

Rust FFI (maintainer, in flight) → [0080](../0080-foreign-signature-reconstruction-cst.md)
foreign-signature reconstruction → binary `.pmo`/`.pmi` record (carrying foreign-shape `.pmi`
publication on the same `format_version` bump) → **native-codegen port to Level 2** (wall 3;
the frozen boot codegen is the port's stable byte-identity golden reference) → levers (1)–(4)
and the optimizer track (8), written once, in PureScript. Levers (5)–(7) may interleave at any
point — they live in the runtime.
