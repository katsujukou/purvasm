# 0052. Native `Purvasm.String.unsafeSetByte` in-place mutation — eliminate the O(output²) string building behind `blit`/`joinWith` (the link cold-build bottleneck)

- Status: Accepted
- Date: 2026-06-29

> **Progress (2026-06-29).** Implemented. Native `Purvasm.String.unsafeSetByte` is now an in-place
> byte write (`Bytes.unsafe_set` over `Bytes.unsafe_of_string`, returning the same `VString`) in
> both hosts — `boot/lib/ocaml_backend/codegen_ml.ml` and `boot/lib/ffi/ffi.ml`; the JS `String.js`
> is unchanged. The linear-unsafe-build contract was consolidated as a doc-comment on the
> `unsafeSetByte` declaration (`packages/purvasm-base/src/Purvasm/String.purs`) — fresh-buffer /
> linear-thread / no-shared-literal / no-`src=dst` — and all current consumers were audited and
> conform. `dune fmt` applied.
>
> **Validation — all three gates met.**
> - **(a) Byte-identity:** boot suite 143/143; compiler unit 59/59 + E2E 3/3 (incl. `.pmo`/`.pmi`-
>   equal-boot); plus a direct native differential — the new Level-3's `.pmo` for `Data.Map.Internal`
>   is byte-identical to boot's `.pvmo`. No `format_version` bump.
> - **(b) Serialize scaling:** per-module native compile (which serializes `.pmo`/`.pmi`) drops from
>   28–43 s to **0.5–0.8 s** on the heavy modules (~50×; O(n²) → O(n)).
> - **(c) Headline — the cold build completes:** a cold native build of the full 227-module self-host
>   closure now **finishes in ~23 s and emits `app.pvm`** (704 KB), versus the prior state where it
>   never finished (compile ~207 s, then the link step stalled >213 s with no `app.pvm`).
>
> One primitive fix sped up **both** the compile stage (per-module serialize) and the link stage
> (`imageToString`), as predicted.

## Context

After ADR-0049 (linear construction) and ADR-0051 (flat serialization), a cold native build
compiles all 227 self-host modules in ~207 s and then **does not finish the link step** (>213 s,
no `app.pvm`). ADR-0050 assumed the remedy was a streaming / link-from-disk redesign; profiling
(the decode→serialize method, "measure, don't guess") refuted that and located a single native
primitive bug instead:

1. **The JS full build completes in 3.68 s and `link` never stalls** — so `link` is not
   algorithmically heavy (a quadratic would be slow on JS too).
2. **JS phase split:** link-build (reachability + `Data.Map` merge) = 24–30 ms; `imageToString`
   (723 KB) = 156 ms. The user's hypothesis that the `Data.Map` *merge* was the cost is rejected;
   the link-stage weight is localised to **serialization**.
3. **The native-specific O(n²):** `Data.String.Common.blit` (and `joinWith`, `fromCharArray`,
   `putCp`) fill a `unsafeNew` buffer by calling `Purvasm.String.unsafeSetByte` **once per byte**.
   The native `unsafeSetByte` (`boot/lib/ocaml_backend/codegen_ml.ml:181-185`, and the VM/oracle
   host `boot/lib/ffi/ffi.ml:758-767`) does `Bytes.of_string … Bytes.to_string` — it **copies the
   entire string on every byte write**, so building a string of length `T` is **O(T²)**. On JS
   `unsafeSetByte` is also O(n), but the stock-`purs` build resolves `joinWith` to the *registry*
   implementation (`Array.prototype.join`, O(n)), so the ulib byte-builders never run there — which
   is why JS is fast and only native is quadratic.
4. **Direct micro-benchmark of the native OCaml op** (build a string by repeated `setByte`):

   | size  | current (copy) | fixed (in-place) |
   |-------|----------------|------------------|
   | 100 KB | 4.56 s        | 0.6 ms           |
   | 400 KB | 84.1 s        | 2.1 ms           |

   Doubling size ~4.2–4.9× the time — clean O(n²). The 100 KB = 4.56 s point **quantitatively
   matches ADR-0051's "big module 3.5–5.5 s"**, confirming that ADR-0051 removed only the nested-
   `<>` *depth* factor while the O(T²) blit remained — and is what the reported link "stall" is.

So the real bottleneck is one primitive, not the orchestration; plan (A) of ADR-0050
(streaming / link-from-disk) is unnecessary.

## Decision

Make the native `Purvasm.String.unsafeSetByte` an **in-place byte mutation (O(1))** in both hosts
that implement it — the OCaml-codegen backend (`codegen_ml.ml`) and the VM/oracle host
(`ffi.ml`) — writing the byte directly into the buffer and returning the same `String`, instead of
copying the whole string in and out. (Essence: `Bytes.unsafe_set` over the buffer shared with the
string via `Bytes.unsafe_of_string`, returning the original `VString`; no `of_string`/`to_string`
copy.) The JS implementation (`String.js`) is left unchanged — JS strings are immutable and the
JS build uses the registry `joinWith` anyway.

**Soundness — the linear unsafe-build protocol.** This is safe for exactly the same reason native
`SetArray` is an in-place mutation (ADR-0009/0019): the only producer of a mutable `String` buffer
is `unsafeNew`, and every consumer **threads that fresh buffer linearly** — each `unsafeSetByte`
result is the sole reference passed to the next write, no alias of the buffer is observed between
writes, and the buffer is published as an immutable `String` only once filling is complete.

**The contract must be made explicit on the primitive, not left to the `unsafe` prefix.** The
in-place semantics are sharper than "unsafe" in general, and a future naive caller would silently
corrupt memory. So this record also requires a doc-comment on the `unsafeSetByte` *declaration*
(`Purvasm.String.purs`) consolidating the invariant the implementation now depends on:

> Mutates the byte buffer in place and returns the same `String`. The caller MUST pass a fresh
> buffer from `unsafeNew`, thread it linearly (never reuse a reference to the buffer taken before a
> write), and MUST NOT pass a shared or literal `String`, nor use it for an overlapping copy
> (`src = dst`).

The no-overlap (`src ≠ dst`) clause matters because the byte-copy builders (`blit` and friends)
take separate `src`/`dst` and the signature alone would permit aliasing them.

All current consumers were audited against this contract and conform (each takes a fresh
`unsafeNew` buffer, threads it linearly, copies from a distinct source): `Data.String.Common.blit`;
`Data.String.Internal.Utf8` (`putCp`, and the slice/copy loop); `Data.Semigroup` string `append`;
`Data.Int.byteString`; `Data.Show` `put1`…`put4`. The primitive is reached only through these safe
builders and is never exported for direct use.

**Output is unchanged**, so `.pmo`/`.pmi`/`app.pvm` stay byte-identical and `Image.format_version`
is **not** bumped (the version stamp tracks output meaning, ADR-0033).

## Scope

- **In:** the in-place rewrite of `unsafeSetByte` in `codegen_ml.ml` and `ffi.ml` (both must change
  so the native binary and the oracle/VM agree). `dune fmt` per the boot convention.
- **Out:**
  - The JS `String.js` (correct and O(n)-relevant only via the unused ulib path).
  - The **deferred single-buffer two-pass serialiser** floated in ADR-0051: this fix **closes**
    that item — a two-pass serialiser that still wrote via the copying `unsafeSetByte` would remain
    O(n²); the correct fix is the primitive, after which the existing flat `stringify` is linear.
  - ADR-0050's incremental reuse + per-module streaming, which remain a separate, lower-priority
    *warm-rebuild* concern (not the cold-build remedy).
  - (Optional, not required) lowering `blit`'s byte loop onto a native `Bytes.blit` leaf to get a
    `memcpy`; in-place `setByte` already makes the builders linear, so this is only a constant-factor
    extra.

## Validation

The acceptance gate, mirroring ADR-0049/0051 (byte-identity + a perf yardstick), plus the
maintainer's headline requirement:

- **(a) Byte-identity (correctness).** The unit (59) and E2E (3) suites stay green, including the
  `.pmo`/`.pmi`/`app.pvm`-equal-boot tests — output is unchanged, so they must remain green with no
  edit and **no `format_version` bump**.
- **(b) Serialize scaling (the fix).** The native string-builder cost goes from O(n²) to O(n):
  re-run the `setByte`-build micro-bench (the table above: ~4.2–4.9× per doubling → ~constant
  per-byte; 400 KB: 84.1 s → ~ms) and/or the per-module `moduleToString` probe (a big module's
  serialize, ~3.5–5.5 s today, becomes sub-second).
- **(c) Headline acceptance — the cold build completes.** A cold native build of the 227-module
  self-host closure must finish the **link step** and emit `app.pvm` (today: >213 s in `link` with
  no `app.pvm`). Record the after-numbers (total cold build, and the link-stage time) once
  measured; reaching "produces `app.pvm`" is the maintainer's actual requirement and the primary
  acceptance signal.

## Consequences

- The link-stage `imageToString` (whole-program, ~723 KB) and **every per-module `.pmo`/`.pmi`
  serialization** drop from O(T²) to O(T): **one primitive fix speeds up both the compile stage
  (the ~207 s) and the link stage** of the cold build. This is the cold-build remedy the maintainer
  asked for, and it is a few lines, not a redesign.
- Confirms — a third time after ADR-0049/0051 — the discipline of profiling before designing: the
  "streaming/link" hypothesis (ADR-0050 plan A) would have been wasted effort.
- Reinforces the in-place native-builder model (ADR-0009/0019) and extends it from `Array` to
  `String`; the linear-thread invariant is the shared soundness basis.

## Alternatives considered

- **Streaming / link-from-disk redesign (ADR-0050 plan A).** Refuted by measurement: `link` is
  linear and light on JS; the cost is native string-building, not orchestration. Would not have
  fixed the quadratic.
- **Single-buffer two-pass serialiser (ADR-0051 deferred).** Does not help while `unsafeSetByte`
  copies; superseded by fixing the primitive.
- **Keep `unsafeSetByte` copying; special-case `joinWith` with a bespoke native leaf.** Narrower
  and leaves the same O(n²) trap in `blit`/`fromCharArray`/`putCp`; fixing the shared primitive is
  the root cause and covers every builder.
- **A persistent/rope `String` representation.** A far larger change to the value model (ADR-0006)
  for no benefit the linear in-place build does not already give.
