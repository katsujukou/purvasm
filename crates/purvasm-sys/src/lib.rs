//! Raw `extern "C"` declarations of the purvasm `pv_*` C ABI (ADR-0078 §3).
//!
//! This crate is the `unsafe` floor of the Rust foreign layer: a **1:1, hand-maintained mirror of
//! `runtime/include/purvasm.h`** — the ONLY surface a native foreign may use (ADR-0073 §2) — and
//! nothing else. It deliberately declares neither the runtime lifecycle (`pv_runtime_new`/`_free`)
//! nor the `pv_prim_*` codegen helpers: those are not part of the foreign surface, exactly as the
//! header's preamble excludes them. The header stays the single source of truth; a signature here
//! drifting from it is a bug in this crate.
//!
//! A Rust foreign must NOT depend on `purvasm-rt`'s crate internals (ADR-0078 §2): these
//! declarations resolve at the final link against the runtime that defines the symbols, exactly as
//! a C foreign's calls do — representation opacity does not depend on the foreign's language.
#![no_std]

/// The runtime context (`PVContext` in `purvasm.h`). Opaque: never allocate or dereference one.
#[repr(C)]
pub struct PVContext {
    _opaque: [u8; 0],
}

/// A purvasm value — an opaque tagged word (immediate scalar OR moving heap pointer). Do not
/// inspect; only pass to `pv_*` functions.
pub type PVWord = u64;

/// A native leaf's code, exported as `pvf_<mangle(key)>` (ADR-0073 §3): `(ctx, closure, args,
/// nargs)`. `closure` is the leaf's own closure value — captured env slots are read from it via
/// [`pv_read_field`] when the leaf captures.
pub type PVCodeFn = unsafe extern "C" fn(*mut PVContext, PVWord, *const PVWord, usize) -> PVWord;

extern "C" {
    /* ── Shadow-stack rooting (the purvasm.h rooting contract) ─────────────────────────────── */

    /// Open a shadow-stack frame; returns a mark to pass to [`pv_pop_frame`].
    pub fn pv_frame(ctx: *mut PVContext) -> PVWord;
    /// Protect `v` for the current frame; returns a stable handle to reload it after a safepoint.
    pub fn pv_root(ctx: *mut PVContext, v: PVWord) -> PVWord;
    /// Reload the current (possibly moved) value behind a root `handle`.
    pub fn pv_get(ctx: *mut PVContext, handle: PVWord) -> PVWord;
    /// Close the frame opened by the matching [`pv_frame`], discarding its roots.
    pub fn pv_pop_frame(ctx: *mut PVContext, mark: PVWord);

    /* ── Scalar accessors (ctx-taking, encoding stays the runtime's — ADR-0073 §2) ─────────── */

    /// The IEEE-754 bit pattern of a boxed `Number`.
    pub fn pv_number_bits(ctx: *mut PVContext, number: PVWord) -> u64;
    /// An immediate `Int`'s payload.
    pub fn pv_int_payload(ctx: *mut PVContext, i: PVWord) -> i32;
    /// An immediate `Boolean`'s payload as 0/1.
    pub fn pv_bool_payload(ctx: *mut PVContext, b: PVWord) -> i32;
    /// A `String`'s UTF-8 byte length (pairs with [`pv_str_copy`]).
    pub fn pv_str_len(ctx: *mut PVContext, s: PVWord) -> usize;
    /// Copy a `String`'s UTF-8 bytes into a caller-owned buffer of capacity `cap`; returns the
    /// count copied. The two-call copy-out shape never hands out an interior heap pointer.
    pub fn pv_str_copy(ctx: *mut PVContext, s: PVWord, dst: *mut u8, cap: usize) -> usize;

    /* ── Immediate constructors (no allocation, hence no ctx) ──────────────────────────────── */

    /// An immediate `Int` from an `i32`.
    pub fn pv_int(v: i32) -> PVWord;
    /// An immediate `Boolean` from a C truth value (0 = false, non-0 = true).
    pub fn pv_bool(v: i32) -> PVWord;
    /// The immediate `Unit` value.
    pub fn pv_unit() -> PVWord;

    /* ── Value constructors (each self-roots its arguments across its own allocation) ──────── */

    /// A `String` from `len` UTF-8 bytes at `bytes`.
    pub fn pv_new_str(ctx: *mut PVContext, bytes: *const u8, len: usize) -> PVWord;
    /// A boxed `Number` from an IEEE-754 bit pattern (pair with [`pv_number_bits`]).
    pub fn pv_new_number(ctx: *mut PVContext, bits: u64) -> PVWord;
    /// An immutable `Array` from `n >= 1` value words (use [`pv_empty_array`] for empty).
    pub fn pv_new_array(ctx: *mut PVContext, elems: *const PVWord, n: usize) -> PVWord;
    /// The canonical empty `Array` (no allocation).
    pub fn pv_empty_array() -> PVWord;
    /// An algebraic-data value: constructor `tag`, then `n` field words.
    pub fn pv_new_adt(ctx: *mut PVContext, tag: u32, fields: *const PVWord, n: usize) -> PVWord;
    /// A record from parallel sorted FNV-1a-64 `ids` and `values` (ADR-0069).
    pub fn pv_new_record(
        ctx: *mut PVContext,
        ids: *const PVWord,
        values: *const PVWord,
        n: usize,
    ) -> PVWord;
    /// A mutable one-cell `Ref` initialised to `init`.
    pub fn pv_new_ref(ctx: *mut PVContext, init: PVWord) -> PVWord;
    /// A closure over `code` (an `AbiCodeFn` address) of `arity`, capturing `env`.
    pub fn pv_make_closure(ctx: *mut PVContext, code: u64, arity: u32, env: PVWord) -> PVWord;

    /* ── Field / record access ─────────────────────────────────────────────────────────────── */

    /// A `Closure`'s captured `env` value — how an effect-thunk leaf reaches its captures without
    /// knowing the closure layout.
    pub fn pv_closure_env(ctx: *mut PVContext, closure: PVWord) -> PVWord;
    /// Read record field for label id `id` (ADR-0069).
    pub fn pv_record_get(ctx: *mut PVContext, record: PVWord, id: PVWord) -> PVWord;
    /// A record with field `id` set to `value` (immutable copy-on-update, ADR-0069).
    pub fn pv_record_set(ctx: *mut PVContext, record: PVWord, id: PVWord, value: PVWord) -> PVWord;
    /// Read value-slot `i` of a heap object (e.g. a closure's captured env).
    pub fn pv_read_field(ctx: *mut PVContext, obj: PVWord, i: u64) -> PVWord;
    /// Write value-slot `i` of a heap object.
    pub fn pv_write_field(ctx: *mut PVContext, obj: PVWord, i: u64, v: PVWord);

    /* ── Application / forcing ─────────────────────────────────────────────────────────────── */

    /// Apply `f` to `nargs` argument words (curried; over-/under-application handled).
    pub fn pv_apply(ctx: *mut PVContext, f: PVWord, args: *const PVWord, nargs: usize) -> PVWord;
    /// Force a by-need cell to its value; passes any non-cell through unchanged (ADR-0070).
    pub fn pv_force_if_byneed(ctx: *mut PVContext, v: PVWord) -> PVWord;
}

/* ══════════════════════════════════════════════════════════════════════════════════════════════
 * GENERATED-CODE ABI (ADR-0079) — mirrored for layout verification, NOT for foreign use.
 * ══════════════════════════════════════════════════════════════════════════════════════════════ */

/// The generated-code ABI context header (`pv_ctx_header` in `purvasm.h`, ADR-0079 §1): the first
/// bytes of every `PVContext`. Mirrored here so the sys side carries the same compile-time layout
/// assertions as the runtime's definition — a drift is layout, which Miri cannot see, so the two
/// `const` nets below are the detection mechanism (ADR-0079 §1).
///
/// **A foreign provider must NOT read or write this** — the supported foreign surface is the
/// `pv_*` functions above (ADR-0073 §2); exactly two consumers may rely on the prefix guarantee
/// (generated code and the runtime/sys mirrors). It is `pub` only so a future privileged consumer
/// (the safe-wrapper's internals, never its users) can name the type.
#[repr(C)]
pub struct PvCtxHeader {
    /// The shadow stack's storage base (moves only on slow-path growth).
    pub roots_base: *mut u64,
    /// One past the top root = the next handle = the frame mark.
    pub roots_len: u64,
    /// Fast-path bound: `roots_len == roots_cap` → the slow-path `pv_root` grows.
    pub roots_cap: u64,
    /// `0` = no stashed generic tail (`pv_settle`'s fast path).
    pub pending_tail: u64,
}

/// `PV_CTX_HEADER_VERSION` in `purvasm.h` (ADR-0079 §1). Bumped in lockstep with any
/// [`PvCtxHeader`] layout change (alongside the `pv_ctx_abi_v<N>` link-time stamp).
pub const PV_CTX_HEADER_VERSION: u32 = 1;

// The sys half of the ADR-0079 §1 compile-time layout net (the runtime's definition carries the
// identical assertions beside `CtxHeader` in `gc.rs`).
const _: () = {
    assert!(core::mem::size_of::<PvCtxHeader>() == 32);
    assert!(core::mem::offset_of!(PvCtxHeader, roots_base) == 0);
    assert!(core::mem::offset_of!(PvCtxHeader, roots_len) == 8);
    assert!(core::mem::offset_of!(PvCtxHeader, roots_cap) == 16);
    assert!(core::mem::offset_of!(PvCtxHeader, pending_tail) == 24);
};

extern "C" {
    /// Run-time ABI-version backstop (ADR-0079 §1): the generated entry stub calls this once at
    /// startup; a mismatch aborts loudly. Also the mechanism the ADR-0078 §5 driver-side ABI
    /// check consumes. Not part of the foreign surface.
    pub fn pv_abi_check(version: u32);
}
