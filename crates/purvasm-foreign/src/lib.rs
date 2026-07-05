//! Safe, idiomatic authoring layer for purvasm native foreigns (ADR-0078).
//!
//! A foreign written with this crate compiles to exactly what a C foreign is — one `AbiCodeFn`
//! under a link-time `pvf_*` symbol, speaking only the `pv_*` C ABI (`purvasm.h`) — but the
//! rooting contract that C leaves carry as a comment-level discipline is enforced structurally
//! here (ADR-0078 §3):
//!
//! - every guest value is held as a [`PvValue`] — a **rooted handle** whose every read reloads
//!   through `pv_get`, so reload-after-safepoint is automatic;
//! - [`Ctx`] and [`PvValue`] are **lifetime-branded to the shadow-stack frame** the generated
//!   wrapper opens, so a handle cannot outlive the `pv_pop_frame` that invalidates its slot, and
//!   both are `!Send`/`!Sync` — the single-capability contract (ADR-0064) is unbreakable from
//!   safe code;
//! - scalar/string conversions are owned **copy-out/copy-in** ([`FromPv`]/[`IntoPv`]), so no
//!   interior pointer into the moving heap ever exists.
//!
//! The paved path is the [`pv_foreign`] attribute macro; the safe API underneath is usable
//! without it (the escape hatch of ADR-0078 Alternatives).

mod convert;
mod ctx;

#[doc(hidden)]
pub mod __rt;

/// Re-exported so foreign crates depend on this one crate only.
pub use purvasm_foreign_macros::pv_foreign;
#[doc(hidden)]
pub use purvasm_sys as sys;

pub use convert::{FromPv, IntoPv};
pub use ctx::{Ctx, PvValue};
