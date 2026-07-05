//! `FromPv`/`IntoPv` — the wasm-bindgen-style boundary conversions (ADR-0078 §3).
//!
//! Scalars and strings convert by owned copy (never a borrow into the moving heap); `PvValue`
//! passes through untyped for leaves that work on opaque guest values via [`Ctx`].

use crate::ctx::{Ctx, PvValue};

/// Convert a guest argument into an owned Rust value at leaf entry.
pub trait FromPv<'f>: Sized {
    fn from_pv(cx: &Ctx<'f>, v: PvValue<'f>) -> Self;
}

/// Convert a Rust result into a guest value at leaf exit.
pub trait IntoPv<'f> {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f>;
}

impl<'f> FromPv<'f> for i32 {
    fn from_pv(cx: &Ctx<'f>, v: PvValue<'f>) -> Self {
        cx.int(v)
    }
}
impl<'f> FromPv<'f> for bool {
    fn from_pv(cx: &Ctx<'f>, v: PvValue<'f>) -> Self {
        cx.bool(v)
    }
}
impl<'f> FromPv<'f> for f64 {
    fn from_pv(cx: &Ctx<'f>, v: PvValue<'f>) -> Self {
        cx.number(v)
    }
}
impl<'f> FromPv<'f> for String {
    fn from_pv(cx: &Ctx<'f>, v: PvValue<'f>) -> Self {
        cx.string(v)
    }
}
impl<'f> FromPv<'f> for PvValue<'f> {
    fn from_pv(_cx: &Ctx<'f>, v: PvValue<'f>) -> Self {
        v
    }
}

impl<'f> IntoPv<'f> for i32 {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.new_int(self)
    }
}
impl<'f> IntoPv<'f> for bool {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.new_bool(self)
    }
}
impl<'f> IntoPv<'f> for f64 {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.new_number(self)
    }
}
impl<'f> IntoPv<'f> for String {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.new_str(&self)
    }
}
impl<'f> IntoPv<'f> for &str {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.new_str(self)
    }
}
impl<'f> IntoPv<'f> for () {
    fn into_pv(self, cx: &Ctx<'f>) -> PvValue<'f> {
        cx.unit()
    }
}
impl<'f> IntoPv<'f> for PvValue<'f> {
    fn into_pv(self, _cx: &Ctx<'f>) -> PvValue<'f> {
        self
    }
}
