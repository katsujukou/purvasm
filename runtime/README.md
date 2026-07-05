# Purvasm Runtime

The owned v1 native runtime (ADR-0063/0064): allocator, copying GC, shadow-stack rooting, `apply`,
and the `pv_*` C ABI that LLVM-generated code and native foreigns link against (ADR-0071/0073).

Built as both a `staticlib` (the `extern "C"` surface linked into compiled programs) and a `lib`
(rlib — the Rust-unit-test + Miri front door). Same code, two front doors (ADR-0071 §1).

## The C ABI surface and its lockstep consumers

`include/purvasm.h` declares the foreign-facing `pv_*` surface and is its **source of truth**
(ADR-0073 §2). **Changing the C ABI — a signature, a type spelling, an object layout an accessor
implies — requires following updates in the `crates/` workspace (ADR-0078), in the same change:**

- **`crates/purvasm-sys`** mirrors `purvasm.h` 1:1 as `extern "C"` declarations. A declaration must
  match the `src/abi.rs` definition **textually, including integer signedness**: the rlib link
  (tests/Miri) puts declaration and definition in one Rust graph, and Miri rejects an
  `i32`-vs-`u32` mismatch as UB even though the machine ABI is identical. (This is why the header
  spells `uint32_t` where `abi.rs` says `u32`.)
- **`crates/purvasm-foreign`** (`__rt` shims, `Ctx`/`PvValue`, conversions) consumes the surface
  through `purvasm-sys`; rooting-contract or accessor changes surface here.
- Validation: `cargo test` in `runtime/` and `crates/`, plus Miri for both
  (`nix develop .#miri`). The `crates/purvasm-foreign` tests link this runtime's rlib and drive
  the ABI end-to-end — they are the fastest signal that a surface change broke a consumer.

`codegen_llvm.ml` (boot) also emits calls against parts of this ABI — including direct closure
env-slot reads that deliberately do **not** go through `pv_closure_env` (codegen is the runtime's
internal lowering ABI; ADR-0078 Progress note). A change to the closure/object layout is
coordinated lockstep with codegen, not just with `crates/`.

Eventually the `purvasm-sys` ↔ `purvasm.h` version agreement will be checked by the build driver
at bundle time (ADR-0078 §5 Progress note); until then this lockstep rule is enforced by review
and the test suites above.
