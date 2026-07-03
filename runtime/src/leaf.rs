//! Native foreign leaves, resolved by **link-time `pvf_*` symbols** (ADR-0071 §6 / ADR-0073 §3).
//!
//! A native leaf is a first-order host function over the value representation (ADR-0022). Each leaf is an
//! `AbiCodeFn` (ADR-0071 §3) exported under the name `pvf_<mangle key>` — the injective mangling of the
//! foreign's qualified key (ADR-0072 §2 / ADR-0073 §3). Codegen references that symbol directly and wraps
//! it in a no-capture closure of the leaf's arity (`pv_make_closure(@pvf_…, arity, unit)`), so foreign
//! resolution is the linker's job, not a runtime string dispatch — mirroring `boot`'s `codegen_ml`'s
//! `foreign`, held at parity by the differential (ADR-0072 §10). Only the genuine **host** leaves live
//! here (the intrinsic floor, ADR-0073 §1/§4); the *structural* foreigns (`Effect.Ref`, `Effect.pureE`/
//! `bindE`, `ord_cmp`, …) are guest terms the linker's `Ffi.resolver` substitutes before codegen (they
//! lower to ordinary primops/`apply`), so they are **not** leaves. A missing symbol is a **link error**.
//!
//! v1 provides the leaves on demand (the minimal-FFI policy): the CORE set that effect demos need, plus
//! the `Purvasm.String` byte primitives the `show`/`Semigroup` `ulib` builds strings from (ADR-0052).
//! The `Number`-math / `FS` / `System` families and the float-formatting `show`s are added when the
//! self-compiler differential reaches them (or, per ADR-0073, ship from a `ulib`'s compiled `.c`).
//!
//! An **effectful** leaf follows the ADR-0067 "foreign returns the thunk; forcing performs" shape: the
//! outer `CodeFn` returns a thunk closure whose body performs the effect when forced.

use crate::abi::{args_slice, guard, heap};
use crate::gc::Heap;
use crate::heap::HeapPtr;
use crate::word::TaggedWord;

/// `Data.Show.showIntImpl :: Int -> String` — the decimal spelling (matches OCaml `string_of_int`).
#[export_name = "pvf_Data_2eShow_2eshowIntImpl"]
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
#[export_name = "pvf_Purvasm_2eStdio_2ewriteLineImpl"]
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

/// `Purvasm.Stdio.writeErrLineImpl :: String -> Effect Unit` (ADR-0068) — the stderr sibling of
/// [`leaf_write_line`], same `\s -> thunk` shape.
#[export_name = "pvf_Purvasm_2eStdio_2ewriteErrLineImpl"]
extern "C" fn leaf_write_err_line(
    ctx: *mut Heap,
    _clo: u64,
    args: *const u64,
    nargs: usize,
) -> u64 {
    // SAFETY: as [`leaf_write_line`].
    guard(|| unsafe {
        let h = heap(ctx);
        let s = args_slice(args, nargs)[0];
        h.trace_value("writeErrLine arg", s);
        let env = h.new_array(&[s]);
        h.new_closure_raw(leaf_write_err_line_thunk as usize as u64, 1, env.as_word())
            .as_word()
            .to_bits()
    })
}

/// The `writeErrLine` thunk: `\_u -> (write stderr; flush; unit)`. Unlike [`leaf_write_line_thunk`]
/// this **writes `stderr` directly and flushes per call — never the drained capture sink** (ADR-0074
/// §5 write-before-exit): `exit` skips the sink drain of anything still buffered there, and stderr is
/// not part of the differential's captured-stdout comparison, so nothing may buffer it.
extern "C" fn leaf_write_err_line_thunk(
    ctx: *mut Heap,
    clo: u64,
    _args: *const u64,
    _n: usize,
) -> u64 {
    // SAFETY: as [`leaf_write_line_thunk`].
    guard(|| unsafe {
        use std::io::Write;
        let h = heap(ctx);
        let cp = HeapPtr::from_word(TaggedWord::from_bits(clo));
        let env = HeapPtr::from_word(h.read_field(cp, 2));
        let s = h.str_read(HeapPtr::from_word(h.read_field(env, 0)));
        let err = std::io::stderr();
        let mut lock = err.lock();
        // A stderr write/flush failure is a fatal boundary fault (ADR-0071 §7), as in `pv_drain_output`.
        writeln!(lock, "{s}").expect("writeErrLine: stderr write failed");
        lock.flush().expect("writeErrLine: stderr flush failed");
        TaggedWord::unit().to_bits()
    })
}

/// `Purvasm.System.Process.exitImpl :: Int -> Effect Unit` (ADR-0056) — `\code -> thunk`; the force
/// terminates the process (ADR-0074 §5).
#[export_name = "pvf_Purvasm_2eSystem_2eProcess_2eexitImpl"]
extern "C" fn leaf_exit(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_write_line`]; `args[0]` is an `Int` immediate, captured as an ordinary value.
    guard(|| unsafe {
        let h = heap(ctx);
        let code = args_slice(args, nargs)[0];
        h.trace_value("exit arg", code);
        let env = h.new_array(&[code]);
        h.new_closure_raw(leaf_exit_thunk as usize as u64, 1, env.as_word())
            .as_word()
            .to_bits()
    })
}

/// The `exit` thunk: `\_u -> (drain the stdout sink; terminate)`. **Draining before terminating is the
/// ADR-0074 §5 write-before-exit contract**: the v1 stdout is a captured sink flushed at exit
/// (`pv_drain_output`), so terminating without the drain would silently drop every line the program
/// already wrote. Exit is the one leaf allowed to run the drain mid-program — nothing runs after it.
extern "C" fn leaf_exit_thunk(ctx: *mut Heap, clo: u64, _args: *const u64, _n: usize) -> u64 {
    // SAFETY: as [`leaf_write_line_thunk`]; `pv_drain_output` is called after the last `h` use (fresh
    // exclusive borrow), and `exit` never returns.
    guard(|| unsafe {
        let h = heap(ctx);
        let cp = HeapPtr::from_word(TaggedWord::from_bits(clo));
        let env = HeapPtr::from_word(h.read_field(cp, 2));
        let code = h.read_field(env, 0).as_int();
        crate::abi::pv_drain_output(ctx);
        std::process::exit(code)
    })
}

/// `Partial._crashWith :: String -> a` — a pure partial crash: forcing it halts with the message
/// (matches `codegen_ml`'s `stuck`). The panic is contained at the FFI boundary (ADR-0071 §7) → abort.
#[export_name = "pvf_Partial_2e_5fcrashWith"]
extern "C" fn leaf_crash_with(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` is a `String`.
    guard(|| unsafe {
        let h = heap(ctx);
        let msg = h.str_read(HeapPtr::from_word(args_slice(args, nargs)[0]));
        panic!("Partial.crashWith: {msg}");
    })
}

/// `Purvasm.String.byteLength :: String -> Int` — the UTF-8 byte length (ADR-0052).
#[export_name = "pvf_Purvasm_2eString_2ebyteLength"]
extern "C" fn leaf_byte_length(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
    // SAFETY: as [`leaf_show_int`]; `args[0]` is a `String`.
    guard(|| unsafe {
        let h = heap(ctx);
        let s = h.checked_ptr(args_slice(args, nargs)[0]);
        TaggedWord::int(h.str_len(s) as i32).to_bits()
    })
}

/// `Purvasm.String.byteAt :: String -> Int -> Int` — the byte at index `i` (0-based), bounds-checked.
#[export_name = "pvf_Purvasm_2eString_2ebyteAt"]
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
#[export_name = "pvf_Purvasm_2eString_2eunsafeNew"]
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
#[export_name = "pvf_Purvasm_2eString_2eunsafeSetByte"]
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

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::abi::{pv_apply, pv_make_closure, pv_runtime_free, pv_runtime_new};

    type AbiCodeFn = extern "C" fn(*mut Heap, u64, *const u64, usize) -> u64;

    /// Build the closure codegen would for a leaf reference (ADR-0073 §3): a no-capture closure over the
    /// leaf's `AbiCodeFn` symbol and its arity — i.e. `pv_make_closure(@pvf_…, arity, unit)`.
    unsafe fn leaf(ctx: *mut Heap, code: AbiCodeFn, arity: u32) -> u64 {
        pv_make_closure(
            ctx,
            code as usize as u64,
            arity,
            TaggedWord::unit().to_bits(),
        )
    }

    #[test]
    fn show_int_leaf_returns_the_decimal_string() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let show = leaf(ctx, leaf_show_int, 1);
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
            let wl = leaf(ctx, leaf_write_line, 1);
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

    /// The testable halves of the ADR-0074 §5 contract: `writeErrLine`'s force must **not** touch the
    /// captured stdout sink (it writes stderr directly), and `exitImpl` applied to its argument returns
    /// the thunk **without** terminating (the exit fires only on force — which a test cannot run).
    #[test]
    fn write_err_line_bypasses_the_sink_and_exit_is_thunked() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let wel = leaf(ctx, leaf_write_err_line, 1);
            let s = heap(ctx).new_str(b"boom").as_word().to_bits();
            let sarg = [s];
            let thunk = pv_apply(ctx, wel, sarg.as_ptr(), sarg.len());
            let unit = [TaggedWord::unit().to_bits()];
            let r = pv_apply(ctx, thunk, unit.as_ptr(), unit.len());
            assert_eq!(TaggedWord::from_bits(r), TaggedWord::unit());
            assert!(
                heap(ctx).output().is_empty(),
                "stderr write must never ride the stdout sink"
            );
            // exitImpl 0 returns the thunk un-forced: the test process is still alive to assert it.
            let ex = leaf(ctx, leaf_exit, 1);
            let zero = [TaggedWord::int(0).to_bits()];
            let exit_thunk = pv_apply(ctx, ex, zero.as_ptr(), zero.len());
            assert!(!TaggedWord::from_bits(exit_thunk).is_immediate());
            pv_runtime_free(ctx);
        }
    }

    /// The linear byte-build protocol end to end (ADR-0052): `unsafeNew` a buffer, `unsafeSetByte` each
    /// byte (returning the same buffer), then read it back via `byteLength` / `byteAt` and `str_read`.
    #[test]
    fn string_byte_build_round_trip() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let new = leaf(ctx, leaf_unsafe_new, 1);
            let set = leaf(ctx, leaf_unsafe_set_byte, 3);
            let at = leaf(ctx, leaf_byte_at, 2);
            let len = leaf(ctx, leaf_byte_length, 1);
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
