//! app-Rust FFI fixture crate (ADR-0091 §Addendum): a `#[pv_foreign]` leaf over `crates/purvasm-foreign`.
//! `tools/ffi-e2e.sh` writes this crate's `Cargo.toml` (with the repo-absolute `purvasm-foreign` path) at
//! run time, so the checked-in fixture carries only the source.
use purvasm_foreign::pv_foreign;

#[pv_foreign(module = "AppRust", name = "addSeven")]
fn add_seven(x: i32) -> i32 {
    x + 7
}
