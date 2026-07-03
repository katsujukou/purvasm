//! Native foreign leaves and the `pv_foreign` resolver (ADR-0071 §6 / ADR-0072 §9).
//!
//! A native leaf is a first-order host function over the value representation (ADR-0022). Codegen lowers
//! an unresolved `foreign import` to `pv_foreign(key)`, which returns the leaf as a closure value —
//! mirroring `boot`'s `codegen_ml`'s `foreign`, held at parity by the differential (ADR-0072 §10). Only
//! the genuine **host** leaves live here; the *structural* foreigns (`Effect.Ref`, `Effect.pureE`/`bindE`,
//! `ord_cmp`, …) are guest terms the linker's `Ffi.resolver` substitutes before codegen (they lower to
//! ordinary primops/`apply`), so they are **not** leaves.
//!
//! v1 provides the leaves on demand (the minimal-FFI policy): the CORE set that effect demos need, plus
//! the `Purvasm.String` byte primitives the `show`/`Semigroup` `ulib` builds strings from (ADR-0052).
//! The `Number`-math / `FS` / `System` families and the float-formatting `show`s are added when the
//! self-compiler differential reaches them. An unknown key is a clear fault.
//!
//! An **effectful** leaf follows the ADR-0067 "foreign returns the thunk; forcing performs" shape: the
//! outer `CodeFn` returns a thunk closure whose body performs the effect when forced.

use crate::abi::{args_slice, guard, heap};
use crate::gc::Heap;
use crate::heap::HeapPtr;
use crate::word::TaggedWord;

/// `Data.Show.showIntImpl :: Int -> String` — the decimal spelling (matches OCaml `string_of_int`).
extern "C" fn leaf_show_int(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: the `AbiCodeFn` contract — `ctx` is live and `args`/`nargs` a valid buffer (ADR-0071 §3).
    guard(|| unsafe {
        let h = heap(ctx);
        let n = args_slice(args, nargs)[0].as_int();
        h.new_str(n.to_string().as_bytes()).as_word().to_bits()
    })
}

/// `Purvasm.Stdio.writeLineImpl :: String -> Effect Unit` (ADR-0068) — the outer `\s -> thunk`: it
/// captures `s` in a thunk closure whose force writes the line (ADR-0067 §3/§5).
extern "C" fn leaf_write_line(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `new_closure_raw`'s only unsafe input is the raw code address.
    guard(|| unsafe {
        let h = heap(ctx);
        let s = args_slice(args, nargs)[0];
        h.trace_value("writeLine arg", s);
        let env = h.new_array(&[s]); // capture `s`; `new_array` self-roots it across the alloc
        h.new_closure_raw(leaf_write_line_thunk as usize as u64, 1, env.as_word())
            .as_word()
            .to_bits()
    })
}

/// The `writeLine` thunk: `\_u -> (write s; unit)`. Reads `s` from its env and appends a line to the
/// output sink (ADR-0067 §5). No allocation after reading `s`, so no rooting.
extern "C" fn leaf_write_line_thunk(ctx: *mut Heap, clo: u64, _args: *const u64, _n: usize) -> u64 {
    // SAFETY: `clo` is this thunk closure (env slot 2 → the captured `[s]` array), `ctx` live.
    guard(|| unsafe {
        let h = heap(ctx);
        let cp = HeapPtr::from_word(TaggedWord::from_bits(clo));
        let env = HeapPtr::from_word(h.read_field(cp, 2));
        let s = h.read_field(env, 0);
        h.stdio_write_line(s);
        TaggedWord::unit().to_bits()
    })
}

/// `Partial._crashWith :: String -> a` — a pure partial crash: forcing it halts with the message
/// (matches `codegen_ml`'s `stuck`). The panic is contained at the FFI boundary (ADR-0071 §7) → abort.
extern "C" fn leaf_crash_with(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` is a `String`.
    guard(|| unsafe {
        let h = heap(ctx);
        let msg = h.str_read(HeapPtr::from_word(args_slice(args, nargs)[0]));
        panic!("Partial.crashWith: {msg}");
    })
}

/// `Purvasm.String.byteLength :: String -> Int` — the UTF-8 byte length (ADR-0052).
extern "C" fn leaf_byte_length(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` is a `String`.
    guard(|| unsafe {
        let h = heap(ctx);
        let s = h.checked_ptr(args_slice(args, nargs)[0]);
        TaggedWord::int(h.str_len(s) as i32).to_bits()
    })
}

/// `Purvasm.String.byteAt :: String -> Int -> Int` — the byte at index `i` (0-based), bounds-checked.
extern "C" fn leaf_byte_at(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` a `String`, `args[1]` an `Int`.
    guard(|| unsafe {
        let h = heap(ctx);
        let a = args_slice(args, nargs);
        let s = h.checked_ptr(a[0]);
        TaggedWord::int(h.str_byte_get(s, a[1].as_int() as usize) as i32).to_bits()
    })
}

/// `Purvasm.String.unsafeNew :: Int -> String` — a fresh zero-filled buffer for the linear byte-build
/// protocol (ADR-0052); [`leaf_unsafe_set_byte`] fills it in place.
extern "C" fn leaf_unsafe_new(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` is an `Int`.
    guard(|| unsafe {
        let h = heap(ctx);
        let n = args_slice(args, nargs)[0].as_int();
        h.new_str_uninit(n as usize).as_word().to_bits()
    })
}

/// `Purvasm.String.unsafeSetByte :: String -> Int -> Int -> String` — write byte `i` in place (low 8
/// bits of the value) and return the **same** buffer (ADR-0052 linear build; no allocation).
extern "C" fn leaf_unsafe_set_byte(
    ctx: *mut Heap,
    _clo: u64,
    args: *const u64,
    nargs: usize,
) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args` = `(String, Int, Int)`.
    guard(|| unsafe {
        let h = heap(ctx);
        let a = args_slice(args, nargs);
        let s = a[0];
        let sp = h.checked_ptr(s);
        h.str_byte_set(sp, a[1].as_int() as usize, (a[2].as_int() & 0xff) as u8);
        s.to_bits()
    })
}

/// Resolve a native foreign `key` to its leaf as a closure value (ADR-0072 §9). Each leaf is a
/// no-capture closure over its `AbiCodeFn`; an unknown key faults (grow the set on demand).
///
/// # Safety
/// `ctx` live; `key_ptr`/`key_len` a valid UTF-8 buffer.
#[no_mangle]
pub unsafe extern "C" fn pv_foreign(ctx: *mut Heap, key_ptr: *const u8, key_len: usize) -> u64 {
    guard(|| {
        let key = std::str::from_utf8(std::slice::from_raw_parts(key_ptr, key_len))
            .expect("pv_foreign: key not UTF-8");
        // (code address, arity) of the leaf's outer `AbiCodeFn`.
        let (code, arity): (u64, u32) = match key {
            "Data.Show.showIntImpl" => (leaf_show_int as usize as u64, 1),
            "Purvasm.Stdio.writeLineImpl" => (leaf_write_line as usize as u64, 1),
            "Partial._crashWith" => (leaf_crash_with as usize as u64, 1),
            // `Purvasm.String` byte primitives (ADR-0052 linear string build).
            "Purvasm.String.byteLength" => (leaf_byte_length as usize as u64, 1),
            "Purvasm.String.byteAt" => (leaf_byte_at as usize as u64, 2),
            "Purvasm.String.unsafeNew" => (leaf_unsafe_new as usize as u64, 1),
            "Purvasm.String.unsafeSetByte" => (leaf_unsafe_set_byte as usize as u64, 3),
            other => panic!("pv_foreign: unbound foreign leaf {other:?}"),
        };
        let h = heap(ctx);
        h.new_closure_raw(code, arity, TaggedWord::unit())
            .as_word()
            .to_bits()
    })
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::abi::{pv_apply, pv_runtime_free, pv_runtime_new};

    /// Look a leaf up by key (as codegen's `pv_foreign(key)` would).
    unsafe fn foreign(ctx: *mut Heap, key: &str) -> u64 {
        pv_foreign(ctx, key.as_ptr(), key.len())
    }

    #[test]
    fn show_int_leaf_returns_the_decimal_string() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let show = foreign(ctx, "Data.Show.showIntImpl");
            let arg = [TaggedWord::int(-42).to_bits()];
            let r = pv_apply(ctx, show, arg.as_ptr(), arg.len());
            let h = heap(ctx);
            assert_eq!(
                h.str_read(HeapPtr::from_word(TaggedWord::from_bits(r))),
                "-42"
            );
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn write_line_leaf_appends_to_the_sink_when_forced() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            // `writeLine "hi"` returns the thunk (nothing written yet); forcing it (apply to unit) writes.
            let wl = foreign(ctx, "Purvasm.Stdio.writeLineImpl");
            let s = heap(ctx).new_str(b"hi").as_word().to_bits();
            let sarg = [s];
            let thunk = pv_apply(ctx, wl, sarg.as_ptr(), sarg.len());
            assert!(
                heap(ctx).output().is_empty(),
                "no write before forcing the thunk"
            );
            let unit = [TaggedWord::unit().to_bits()];
            let _ = pv_apply(ctx, thunk, unit.as_ptr(), unit.len());
            assert_eq!(heap(ctx).output(), ["hi".to_string()]);
            pv_runtime_free(ctx);
        }
    }

    /// The linear byte-build protocol end to end (ADR-0052): `unsafeNew` a buffer, `unsafeSetByte` each
    /// byte (returning the same buffer), then read it back via `byteLength` / `byteAt` and `str_read`.
    #[test]
    fn string_byte_build_round_trip() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let new = foreign(ctx, "Purvasm.String.unsafeNew");
            let set = foreign(ctx, "Purvasm.String.unsafeSetByte");
            let at = foreign(ctx, "Purvasm.String.byteAt");
            let len = foreign(ctx, "Purvasm.String.byteLength");
            let call = |f: u64, a: &[u64]| pv_apply(ctx, f, a.as_ptr(), a.len());
            let int = |n: i32| TaggedWord::int(n).to_bits();
            // unsafeNew 2; unsafeSetByte 0 'H'(72); unsafeSetByte 1 'i'(105) — same buffer threaded through
            let s = call(new, &[int(2)]);
            let s = call(set, &[s, int(0), int(72)]);
            let s = call(set, &[s, int(1), int(105)]);
            // byteLength = 2; byteAt 0 = 72; byteAt 1 = 105
            assert_eq!(TaggedWord::from_bits(call(len, &[s])).as_int(), 2);
            assert_eq!(TaggedWord::from_bits(call(at, &[s, int(0)])).as_int(), 72);
            assert_eq!(TaggedWord::from_bits(call(at, &[s, int(1)])).as_int(), 105);
            // the completed buffer is valid UTF-8 "Hi"
            assert_eq!(
                heap(ctx).str_read(HeapPtr::from_word(TaggedWord::from_bits(s))),
                "Hi"
            );
            pv_runtime_free(ctx);
        }
    }
}
