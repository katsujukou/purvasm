//! End-to-end tests of the ADR-0078 layer: leaves authored with `#[pv_foreign]` are invoked
//! through their generated `AbiCodeFn`s against a real runtime (`purvasm-rt`'s rlib front door),
//! exercising conversion, rooting-across-allocation, the effect thunk shape, and GC pressure.
//! These are the tests a C foreign can never have in-process — and they run under Miri.

use purvasm_foreign::{pv_foreign, Ctx, PvValue};
use purvasm_rt::abi;
use purvasm_sys::{PVContext, PVWord};

/* ── leaves under test ─────────────────────────────────────────────────────────────────────── */

#[pv_foreign(module = "Test.Scratch", name = "addImpl")]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[pv_foreign(module = "Test.Scratch", name = "greetImpl")]
fn greet(name: String) -> String {
    format!("Hello, {name}!")
}

/// A `&str` parameter borrows the shim-owned copy for the duration of the call (ADR-0078 §3).
#[pv_foreign(module = "Test.Scratch", name = "shoutImpl")]
fn shout(s: &str) -> String {
    s.to_uppercase()
}

/// A leaf that allocates several times while holding earlier values — the rooted-handle path:
/// `s` and `left` must survive (and reload after) the later allocations.
#[pv_foreign(module = "Test.Scratch", name = "sandwichImpl")]
fn sandwich<'f>(cx: &Ctx<'f>, s: PvValue<'f>) -> PvValue<'f> {
    let left = cx.new_str("(");
    let right = cx.new_str(")");
    cx.new_array(&[left, s, right])
}

#[pv_foreign(module = "Test.Scratch", name = "mulLaterImpl", effect)]
fn mul_later(a: i32, b: i32) -> i32 {
    a * b
}

#[pv_foreign(module = "Test.Scratch", name = "answerImpl", effect)]
fn answer() -> i32 {
    42
}

/* ── harness ───────────────────────────────────────────────────────────────────────────────── */

/// Run `f` against a fresh runtime of `words` local heap words, then free it. The `*mut Heap`
/// the runtime hands out is the `PVContext` the C ABI names — same object, opaque over there.
fn with_rt<R>(words: usize, f: impl FnOnce(*mut PVContext) -> R) -> R {
    let heap = abi::pv_runtime_new(words);
    let ctx = heap.cast::<PVContext>();
    let r = f(ctx);
    unsafe { abi::pv_runtime_free(heap) };
    r
}

fn heap(ctx: *mut PVContext) -> *mut purvasm_rt::Heap {
    ctx.cast()
}

fn read_string(ctx: *mut PVContext, w: PVWord) -> String {
    unsafe {
        let len = abi::pv_str_len(heap(ctx), w);
        let mut buf = vec![0u8; len];
        let n = abi::pv_str_copy(heap(ctx), w, buf.as_mut_ptr(), len);
        buf.truncate(n);
        String::from_utf8(buf).unwrap()
    }
}

/* ── tests ─────────────────────────────────────────────────────────────────────────────────── */

#[test]
fn pure_scalar_leaf_roundtrips() {
    with_rt(1 << 16, |ctx| unsafe {
        let args = [abi::pv_int(2), abi::pv_int(3)];
        let r = __pvf_add(ctx, abi::pv_unit(), args.as_ptr(), 2);
        assert_eq!(abi::pv_int_payload(heap(ctx), r), 5);
    });
}

#[test]
fn string_leaf_copies_out_and_in() {
    with_rt(1 << 16, |ctx| unsafe {
        let name = abi::pv_new_str(heap(ctx), "purvasm".as_ptr(), 7);
        let r = __pvf_greet(ctx, abi::pv_unit(), [name].as_ptr(), 1);
        assert_eq!(read_string(ctx, r), "Hello, purvasm!");
    });
}

#[test]
fn str_ref_leaf_borrows_the_shim_copy() {
    with_rt(1 << 16, |ctx| unsafe {
        let s = abi::pv_new_str(heap(ctx), "loud".as_ptr(), 4);
        let r = __pvf_shout(ctx, abi::pv_unit(), [s].as_ptr(), 1);
        assert_eq!(read_string(ctx, r), "LOUD");
    });
}

#[test]
fn ctx_leaf_holds_values_across_allocations() {
    with_rt(1 << 16, |ctx| unsafe {
        let s = abi::pv_new_str(heap(ctx), "mid".as_ptr(), 3);
        let arr = __pvf_sandwich(ctx, abi::pv_unit(), [s].as_ptr(), 1);
        assert_eq!(read_string(ctx, abi::pv_read_field(heap(ctx), arr, 0)), "(");
        assert_eq!(
            read_string(ctx, abi::pv_read_field(heap(ctx), arr, 1)),
            "mid"
        );
        assert_eq!(read_string(ctx, abi::pv_read_field(heap(ctx), arr, 2)), ")");
    });
}

#[test]
// Applying the thunk goes through the address-path `call_code` transmute, which is the
// supported-target ABI reconstruction and documented as never-run-under-Miri (ADR-0071 §3);
// `effect_leaf_captures_into_the_thunk` below covers the Miri-checkable half.
#[cfg_attr(
    miri,
    ignore = "address-path code call is not representable under Miri (ADR-0071 §3)"
)]
fn effect_leaf_defers_to_the_thunk() {
    with_rt(1 << 16, |ctx| unsafe {
        // Outer leaf: capture (6, 7) into the thunk — no multiplication happens yet.
        let args = [abi::pv_int(6), abi::pv_int(7)];
        let thunk = __pvf_mul_later(ctx, abi::pv_unit(), args.as_ptr(), 2);
        // Perform: apply the thunk to unit, as `run_effect`/a bind would.
        let unit = [abi::pv_unit()];
        let r = abi::pv_apply(heap(ctx), thunk, unit.as_ptr(), 1);
        assert_eq!(abi::pv_int_payload(heap(ctx), r), 42);
    });
}

/// The Miri-checkable half of the effect shape: the outer leaf must build an arity-1 closure
/// whose env `Array` holds the captured arguments in order (the `make_thunk` layout the thunk
/// shim reads back).
#[test]
fn effect_leaf_captures_into_the_thunk() {
    with_rt(1 << 16, |ctx| unsafe {
        let args = [abi::pv_int(6), abi::pv_int(7)];
        let thunk = __pvf_mul_later(ctx, abi::pv_unit(), args.as_ptr(), 2);
        let env = abi::pv_closure_env(heap(ctx), thunk);
        assert_eq!(
            abi::pv_int_payload(heap(ctx), abi::pv_read_field(heap(ctx), env, 0)),
            6
        );
        assert_eq!(
            abi::pv_int_payload(heap(ctx), abi::pv_read_field(heap(ctx), env, 1)),
            7
        );
    });
}

#[test]
fn arity_zero_effect_leaf_is_the_thunk() {
    with_rt(1 << 16, |ctx| unsafe {
        // An arity-0 effect's leaf IS the thunk: the closure the compiler builds over it has
        // arity 1 and gets applied to unit — simulate that call shape directly.
        let unit = [abi::pv_unit()];
        let r = __pvf_answer(ctx, abi::pv_unit(), unit.as_ptr(), 1);
        assert_eq!(abi::pv_int_payload(heap(ctx), r), 42);
    });
}

/// Repeated leaf calls on a small heap: each call's garbage (arguments, results, per-call roots)
/// must become collectable once its frame pops, and live values must survive the collections the
/// churn forces. A leaked root or a stale word turns this into an OOM abort or a wrong string.
#[test]
fn gc_pressure_across_many_calls() {
    with_rt(1 << 12, |ctx| unsafe {
        for i in 0..2_000 {
            let s = format!("call-{i}");
            let name = abi::pv_new_str(heap(ctx), s.as_ptr(), s.len());
            let r = __pvf_greet(ctx, abi::pv_unit(), [name].as_ptr(), 1);
            assert_eq!(read_string(ctx, r), format!("Hello, call-{i}!"));
        }
    });
}
