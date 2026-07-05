//! Hidden shims the `#[pv_foreign]` expansions call (ADR-0078 §3/§4). The frame, rooting, arity
//! check, and panic guard live HERE — one checked implementation — so a macro expansion is a thin
//! call, not a re-statement of the contract.

use std::panic::{catch_unwind, AssertUnwindSafe};

use crate::ctx::{Ctx, PvValue};
use purvasm_sys as sys;

/// A fatal foreign fault: report on stderr, then abort — the same observable class as a C leaf's
/// runtime shape fault. Never unwinds into the runtime (ADR-0078 §4).
fn fatal(key: &str, what: &str) -> ! {
    eprintln!("purvasm: fatal in native foreign `{key}`: {what}");
    std::process::abort()
}

/// Run `body` under the leaf contract: arity check, `pv_frame` open, arguments rooted, result
/// word extracted (reloaded) BEFORE the frame pop, panic guard around the lot.
///
/// # Safety
/// `ctx`/`args`/`nargs` must be the untouched parameters the runtime passed to the `AbiCodeFn` —
/// upheld by the macro-generated wrapper, the only caller.
pub unsafe fn leaf_shim(
    ctx: *mut sys::PVContext,
    args: *const sys::PVWord,
    nargs: usize,
    key: &str,
    arity: usize,
    body: impl for<'f> FnOnce(&Ctx<'f>, &[PvValue<'f>]) -> PvValue<'f>,
) -> sys::PVWord {
    // The panic strategy note (ADR-0078 §4): under `panic = "unwind"` (the pinned default) this
    // catches and reports; under `panic = "abort"` the process aborts before reaching here, which
    // is sound but loses the named diagnostic.
    let out = catch_unwind(AssertUnwindSafe(|| {
        if nargs != arity {
            fatal(
                key,
                &format!("arity mismatch: expected {arity} argument(s), got {nargs}"),
            );
        }
        let mark = sys::pv_frame(ctx);
        let cx = Ctx::new(ctx);
        let vals: Vec<PvValue<'_>> = (0..nargs).map(|i| cx.root_word(*args.add(i))).collect();
        let result = body(&cx, &vals);
        // Reload the result's current word while its root is still live, THEN pop. Between the
        // pop and the return there is no safepoint, so the word stays valid for the caller
        // (which roots it per its own contract).
        let w = cx.word_of(result);
        sys::pv_pop_frame(ctx, mark);
        w
    }));
    match out {
        Ok(w) => w,
        Err(payload) => {
            let msg = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "panic (non-string payload)".to_string());
            fatal(key, &format!("panic: {msg}"))
        }
    }
}

/// Build the ADR-0067 effect thunk: capture `caps` into an env `Array` and close the given
/// `AbiCodeFn` over it with arity 1 (the thunk takes the forced `unit`).
pub fn make_thunk<'f>(cx: &Ctx<'f>, code: sys::PVCodeFn, caps: &[PvValue<'f>]) -> PvValue<'f> {
    let env = cx.new_array(caps);
    let env_w = cx.word_of(env);
    // A code address is a u64 in the value ABI (ADR-0071 §3); `pv_make_closure` self-roots `env`
    // across its own allocation.
    let w = unsafe { sys::pv_make_closure(cx.raw(), code as usize as u64, 1, env_w) };
    cx.root_word(w)
}

/// Run an effect thunk's `body` with the captures read back from the closure env: frame open,
/// closure env slots rooted, result reloaded before the pop — the perform-time half of
/// [`make_thunk`].
///
/// # Safety
/// `ctx`/`closure` must be the untouched `AbiCodeFn` parameters — upheld by the macro-generated
/// thunk, the only caller.
pub unsafe fn thunk_shim(
    ctx: *mut sys::PVContext,
    closure: sys::PVWord,
    ncaps: usize,
    key: &str,
    body: impl for<'f> FnOnce(&Ctx<'f>, &[PvValue<'f>]) -> PvValue<'f>,
) -> sys::PVWord {
    let out = catch_unwind(AssertUnwindSafe(|| {
        let mark = sys::pv_frame(ctx);
        let cx = Ctx::new(ctx);
        // No safepoint has occurred since the runtime passed `closure`, so reading its env slot
        // before rooting is within the "valid until the next allocating call" window.
        let env = cx.root_word(sys::pv_closure_env(ctx, closure));
        let caps: Vec<PvValue<'_>> = (0..ncaps).map(|i| cx.read_field(env, i as u64)).collect();
        let result = body(&cx, &caps);
        let w = cx.word_of(result);
        sys::pv_pop_frame(ctx, mark);
        w
    }));
    match out {
        Ok(w) => w,
        Err(_) => fatal(key, "panic in effect thunk"),
    }
}
