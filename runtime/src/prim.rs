//! The primop helpers (ADR-0071 §6 / ADR-0072 §7): one `extern "C"` `pv_prim_*` per
//! [`Cesk.Ast.primop`], the **single tested source of truth** for the tricky scalar semantics — 32-bit
//! wrapping, Euclidean `div`/`mod`, ECMAScript `ToInt32` (ADR-0041), byte-lexicographic `String` order —
//! so codegen emits a call rather than re-deriving them in IR. Kept byte-for-byte in step with `boot`'s
//! `codegen_ml`'s `prim_fn`, which the differential enforces (ADR-0072 §10).
//!
//! **Two shapes.** *Pure* primops over immediates (`Int` / `Boolean`) touch no heap and cannot panic, so
//! they are plain `extern "C" fn(u64, …) -> u64` with no context and no [`guard`]. *Heap* primops
//! (`Number` boxes, `String`, `Array`, `Record`) take the `ctx` and run under [`guard`] (a bad pointer /
//! absent label is a release fault, ADR-0071 §1/§7), reading their inputs into locals/vectors **before**
//! any allocation (so the constructor's self-rooting covers the survivors — no extra rooting here).

use crate::abi::{empty_array, guard, heap};
use crate::gc::Heap;
use crate::heap::{HeapPtr, Kind};
use crate::word::TaggedWord;

// --- word helpers -----------------------------------------------------------------------------------

#[inline]
fn ii(w: u64) -> i32 {
    TaggedWord::from_bits(w).as_int()
}
#[inline]
fn bb(w: u64) -> bool {
    TaggedWord::from_bits(w).as_bool()
}
#[inline]
fn mk_int(v: i32) -> u64 {
    TaggedWord::int(v).to_bits()
}
#[inline]
fn mk_bool(b: bool) -> u64 {
    TaggedWord::bool(b).to_bits()
}

/// Read a boxed `Number`'s `f64` (raw word 0 of a `NumberBox`).
///
/// # Safety
/// `w` is a `NumberBox` pointer word; `read_raw` validates the object header.
#[inline]
unsafe fn num(h: &Heap, w: u64) -> f64 {
    f64::from_bits(h.read_raw(HeapPtr::from_word(TaggedWord::from_bits(w)), 0))
}

/// Read a `Str`'s bytes as an owned `String`.
///
/// # Safety
/// `w` is a `Str` pointer word; `str_read` validates the object header / kind.
#[inline]
unsafe fn strv(h: &Heap, w: u64) -> String {
    h.str_read(HeapPtr::from_word(TaggedWord::from_bits(w)))
}

// --- scalar semantic core (the tested invariants) ---------------------------------------------------

/// Truncate to a signed 32-bit `Int` (`w32` / ECMAScript `ToInt32` last step, ADR-0041): the low 32
/// bits, sign-extended — PureScript `Int` is 32-bit wrapping (ADR-0006).
#[inline]
fn wrap32(n: i64) -> i32 {
    n as i32
}

/// Euclidean remainder (ADR-0041): non-negative, `0` on a zero divisor. Matches `codegen_ml`'s `emod`.
#[inline]
fn emod(a: i32, b: i32) -> i32 {
    if b == 0 {
        return 0;
    }
    let m = (b as i64).abs();
    let r = (a as i64) % m;
    (if r < 0 { r + m } else { r }) as i32
}

/// Euclidean quotient (ADR-0041): `(a - emod a b) / b`, `0` on a zero divisor, wrapped to 32 bits (so
/// `i32::MIN / -1` wraps rather than overflows). Matches `codegen_ml`'s `ediv`.
#[inline]
fn ediv(a: i32, b: i32) -> i32 {
    if b == 0 {
        return 0;
    }
    let m = emod(a, b) as i64;
    wrap32(((a as i64) - m) / (b as i64))
}

/// ECMAScript `ToInt32` on a `Number` (the JS `n | 0`, ADR-0041): NaN/±∞ → 0, else truncate toward zero,
/// reduce mod 2³², signed. Matches `codegen_ml`'s `p_number_to_int`.
#[inline]
fn number_to_int(f: f64) -> i32 {
    if !f.is_finite() {
        0
    } else {
        // `trunc` toward zero, `%` = fmod, then `as i64` truncates the (in-range) result, `wrap32` signs.
        wrap32((f.trunc() % 4_294_967_296.0) as i64)
    }
}

// --- pure Int primops (no ctx, no guard) ------------------------------------------------------------

#[no_mangle]
pub extern "C" fn pv_prim_add_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a).wrapping_add(ii(b)))
}
#[no_mangle]
pub extern "C" fn pv_prim_sub_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a).wrapping_sub(ii(b)))
}
#[no_mangle]
pub extern "C" fn pv_prim_mul_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a).wrapping_mul(ii(b)))
}
#[no_mangle]
pub extern "C" fn pv_prim_div_int(a: u64, b: u64) -> u64 {
    mk_int(ediv(ii(a), ii(b)))
}
#[no_mangle]
pub extern "C" fn pv_prim_mod_int(a: u64, b: u64) -> u64 {
    mk_int(emod(ii(a), ii(b)))
}
#[no_mangle]
pub extern "C" fn pv_prim_and_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a) & ii(b))
}
#[no_mangle]
pub extern "C" fn pv_prim_or_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a) | ii(b))
}
#[no_mangle]
pub extern "C" fn pv_prim_xor_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a) ^ ii(b))
}
/// Left shift, shift amount masked to `[0, 31]`, result wrapped to 32 bits (ADR-0041).
#[no_mangle]
pub extern "C" fn pv_prim_shl_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a).wrapping_shl((ii(b) & 31) as u32))
}
/// Arithmetic (sign-propagating) right shift, amount masked to `[0, 31]`.
#[no_mangle]
pub extern "C" fn pv_prim_shr_int(a: u64, b: u64) -> u64 {
    mk_int(ii(a).wrapping_shr((ii(b) & 31) as u32))
}
/// Logical (zero-fill) right shift, amount masked to `[0, 31]`.
#[no_mangle]
pub extern "C" fn pv_prim_zshr_int(a: u64, b: u64) -> u64 {
    mk_int(((ii(a) as u32) >> (ii(b) & 31)) as i32)
}
#[no_mangle]
pub extern "C" fn pv_prim_complement_int(a: u64) -> u64 {
    mk_int(!ii(a))
}
#[no_mangle]
pub extern "C" fn pv_prim_eq_int(a: u64, b: u64) -> u64 {
    mk_bool(ii(a) == ii(b))
}
#[no_mangle]
pub extern "C" fn pv_prim_lt_int(a: u64, b: u64) -> u64 {
    mk_bool(ii(a) < ii(b))
}

// --- pure Bool primops ------------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn pv_prim_and_bool(a: u64, b: u64) -> u64 {
    mk_bool(bb(a) && bb(b))
}
#[no_mangle]
pub extern "C" fn pv_prim_or_bool(a: u64, b: u64) -> u64 {
    mk_bool(bb(a) || bb(b))
}
#[no_mangle]
pub extern "C" fn pv_prim_not_bool(a: u64) -> u64 {
    mk_bool(!bb(a))
}
#[no_mangle]
pub extern "C" fn pv_prim_eq_bool(a: u64, b: u64) -> u64 {
    mk_bool(bb(a) == bb(b))
}

// --- Number primops (ctx: read boxes, box results) --------------------------------------------------

/// # Safety
/// `ctx` live; `a`/`b` `NumberBox` pointer words.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_add_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let (x, y) = (num(h, a), num(h, b));
        h.new_number(x + y).as_word().to_bits()
    })
}
/// # Safety
/// As [`pv_prim_add_number`].
#[no_mangle]
pub unsafe extern "C" fn pv_prim_sub_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let (x, y) = (num(h, a), num(h, b));
        h.new_number(x - y).as_word().to_bits()
    })
}
/// # Safety
/// As [`pv_prim_add_number`].
#[no_mangle]
pub unsafe extern "C" fn pv_prim_mul_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let (x, y) = (num(h, a), num(h, b));
        h.new_number(x * y).as_word().to_bits()
    })
}
/// # Safety
/// As [`pv_prim_add_number`]. Division by zero yields IEEE ±∞/NaN (not a fault).
#[no_mangle]
pub unsafe extern "C" fn pv_prim_div_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let (x, y) = (num(h, a), num(h, b));
        h.new_number(x / y).as_word().to_bits()
    })
}
/// # Safety
/// As [`pv_prim_add_number`].
#[no_mangle]
pub unsafe extern "C" fn pv_prim_eq_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        mk_bool(num(h, a) == num(h, b)) // IEEE: NaN != NaN
    })
}
/// # Safety
/// As [`pv_prim_add_number`].
#[no_mangle]
pub unsafe extern "C" fn pv_prim_lt_number(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        mk_bool(num(h, a) < num(h, b))
    })
}
/// `Int -> Number` widening (ADR-0041).
///
/// # Safety
/// `ctx` live; `a` an `Int` immediate.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_int_to_number(ctx: *mut Heap, a: u64) -> u64 {
    guard(|| heap(ctx).new_number(ii(a) as f64).as_word().to_bits())
}
/// `Number -> Int` via `ToInt32` (ADR-0041).
///
/// # Safety
/// `ctx` live; `a` a `NumberBox` pointer.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_number_to_int(ctx: *mut Heap, a: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        mk_int(number_to_int(num(h, a)))
    })
}

// --- String primops (ctx: read Str bytes) -----------------------------------------------------------

/// # Safety
/// `ctx` live; `a`/`b` `Str` pointer words.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_eq_string(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        mk_bool(strv(h, a) == strv(h, b))
    })
}
/// Byte-lexicographic order (matches `codegen_ml` / OCaml `String` compare).
///
/// # Safety
/// As [`pv_prim_eq_string`].
#[no_mangle]
pub unsafe extern "C" fn pv_prim_lt_string(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        mk_bool(strv(h, a) < strv(h, b))
    })
}

// --- Append / Array primops (ctx) -------------------------------------------------------------------

/// `Append`: `String ++ String` or `Array ++ Array` (ADR-0072 §5). Dispatches on `a`'s kind — a `Str`
/// pointer → string concat; otherwise array concat (either operand may be the empty-array sentinel).
///
/// # Safety
/// `ctx` live; `a`/`b` `Str` pointers, or `Array` pointers / the empty-array sentinel.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_append(ctx: *mut Heap, a: u64, b: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let av = TaggedWord::from_bits(a);
        if av.is_pointer() && h.header(HeapPtr::from_word(av)).kind() == Kind::Str {
            let mut s = strv(h, a);
            s.push_str(&strv(h, b));
            h.new_str(s.as_bytes()).as_word().to_bits()
        } else {
            array_append(h, a, b)
        }
    })
}

/// `Array ++ Array` honouring the empty-array sentinel (an immediate operand is the empty array).
///
/// # Safety
/// `a`/`b` are `Array` pointers or the empty-array sentinel.
unsafe fn array_append(h: &mut Heap, a: u64, b: u64) -> u64 {
    let (av, bv) = (TaggedWord::from_bits(a), TaggedWord::from_bits(b));
    if av.is_immediate() {
        return b; // [] ++ b = b
    }
    if bv.is_immediate() {
        return a; // a ++ [] = a
    }
    let (pa, pb) = (HeapPtr::from_word(av), HeapPtr::from_word(bv));
    let (na, nb) = (h.header(pa).size_words(), h.header(pb).size_words());
    // Snapshot all elements first (no allocation between), then build — `new_array` self-roots them.
    let mut elems: Vec<TaggedWord> = Vec::with_capacity((na + nb) as usize);
    for i in 0..na {
        elems.push(h.read_field(pa, i));
    }
    for i in 0..nb {
        elems.push(h.read_field(pb, i));
    }
    h.new_array(&elems).as_word().to_bits()
}

/// `IndexArray`: `arr[i]`, with a bounds fault (an empty-sentinel array is always out of bounds).
///
/// # Safety
/// `ctx` live; `arr` an `Array` pointer or the empty sentinel; `idx` an `Int`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_index_array(ctx: *mut Heap, arr: u64, idx: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let av = TaggedWord::from_bits(arr);
        let i = ii(idx);
        assert!(av.is_pointer(), "index: out of bounds (empty array)");
        let p = HeapPtr::from_word(av);
        let n = h.header(p).size_words() as i64;
        assert!(i >= 0 && (i as i64) < n, "index: out of bounds");
        h.read_field(p, i as u64).to_bits()
    })
}

/// `LengthArray`: `0` for the empty sentinel, else the `Array`'s element count.
///
/// # Safety
/// `ctx` live; `arr` an `Array` pointer or the empty sentinel.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_length_array(ctx: *mut Heap, arr: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let av = TaggedWord::from_bits(arr);
        if av.is_immediate() {
            mk_int(0)
        } else {
            mk_int(h.header(HeapPtr::from_word(av)).size_words() as i32)
        }
    })
}

/// `NewArray n`: a fresh array of `n` `Int 0`s; `n == 0` → the empty-array sentinel; `n < 0` faults.
///
/// # Safety
/// `ctx` live; `nw` an `Int`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_new_array(ctx: *mut Heap, nw: u64) -> u64 {
    guard(|| {
        let n = ii(nw);
        assert!(n >= 0, "newArray: negative length");
        if n == 0 {
            empty_array().to_bits()
        } else {
            let elems = vec![TaggedWord::int(0); n as usize];
            heap(ctx).new_array(&elems).as_word().to_bits()
        }
    })
}

/// `SetArray`: in-place `arr[i] := v` (the linear unsafe-build protocol, ADR-0009/0052), returning the
/// same array. A bounds fault (empty sentinel is always out of bounds).
///
/// # Safety
/// `ctx` live; `arr` an `Array` pointer or the empty sentinel; `idx` an `Int`; `v` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_set_array(ctx: *mut Heap, arr: u64, idx: u64, v: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let av = TaggedWord::from_bits(arr);
        let i = ii(idx);
        assert!(av.is_pointer(), "set: out of bounds (empty array)");
        let p = HeapPtr::from_word(av);
        let n = h.header(p).size_words() as i64;
        assert!(i >= 0 && (i as i64) < n, "set: out of bounds");
        h.write_field(p, i as u64, TaggedWord::from_bits(v));
        arr
    })
}

// --- dynamic Record primops (ctx; String label) ----------------------------------------------------

/// `RecordGet label rec` (ADR-0069).
///
/// # Safety
/// `ctx` live; `label` a `Str`, `rec` a `Record`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_record_get(ctx: *mut Heap, label: u64, rec: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .record_unsafe_get(TaggedWord::from_bits(label), TaggedWord::from_bits(rec))
            .to_bits()
    })
}
/// `RecordSet label value rec` (functional update).
///
/// # Safety
/// `ctx` live; `label` a `Str`, `rec` a `Record`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_record_set(
    ctx: *mut Heap,
    label: u64,
    value: u64,
    rec: u64,
) -> u64 {
    guard(|| {
        heap(ctx)
            .record_unsafe_set(
                TaggedWord::from_bits(label),
                TaggedWord::from_bits(value),
                TaggedWord::from_bits(rec),
            )
            .to_bits()
    })
}
/// `RecordHas label rec` → `Boolean`.
///
/// # Safety
/// `ctx` live; `label` a `Str`, `rec` a `Record`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_record_has(ctx: *mut Heap, label: u64, rec: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .record_unsafe_has(TaggedWord::from_bits(label), TaggedWord::from_bits(rec))
            .to_bits()
    })
}
/// `RecordDelete label rec`.
///
/// # Safety
/// `ctx` live; `label` a `Str`, `rec` a `Record`.
#[no_mangle]
pub unsafe extern "C" fn pv_prim_record_delete(ctx: *mut Heap, label: u64, rec: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .record_unsafe_delete(TaggedWord::from_bits(label), TaggedWord::from_bits(rec))
            .to_bits()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // The scalar semantic core is the part the type system cannot guarantee (32-bit wrap / Euclidean /
    // ToInt32), so it is unit-tested against the `codegen_ml` reference behaviour.

    #[test]
    fn int_arithmetic_wraps_at_32_bits() {
        assert_eq!(ii(pv_prim_add_int(mk_int(i32::MAX), mk_int(1))), i32::MIN);
        assert_eq!(ii(pv_prim_sub_int(mk_int(i32::MIN), mk_int(1))), i32::MAX);
        // 2^16 * 2^16 = 2^32 ≡ 0 (mod 2^32)
        assert_eq!(ii(pv_prim_mul_int(mk_int(1 << 16), mk_int(1 << 16))), 0);
    }

    #[test]
    fn euclidean_div_mod() {
        // Non-negative remainder regardless of sign.
        assert_eq!((ediv(7, 3), emod(7, 3)), (2, 1));
        assert_eq!((ediv(-7, 3), emod(-7, 3)), (-3, 2));
        assert_eq!((ediv(7, -3), emod(7, -3)), (-2, 1));
        assert_eq!((ediv(-7, -3), emod(-7, -3)), (3, 2));
        // Zero divisor → 0 (total, no trap).
        assert_eq!((ediv(5, 0), emod(5, 0)), (0, 0));
        // i32::MIN / -1 wraps (would overflow) rather than trapping.
        assert_eq!(ediv(i32::MIN, -1), i32::MIN);
        assert_eq!(emod(i32::MIN, -1), 0);
    }

    #[test]
    fn shifts_mask_amount_and_pick_the_right_fill() {
        // Left shift wraps to 32 bits; amount masked mod 32 (so `<< 32` == `<< 0`).
        assert_eq!(ii(pv_prim_shl_int(mk_int(1), mk_int(31))), i32::MIN);
        assert_eq!(ii(pv_prim_shl_int(mk_int(1), mk_int(32))), 1);
        // Arithmetic vs logical right shift on a negative value.
        assert_eq!(ii(pv_prim_shr_int(mk_int(-8), mk_int(1))), -4); // sign-propagating
        assert_eq!(ii(pv_prim_zshr_int(mk_int(-1), mk_int(28))), 0xF); // zero-fill
        assert_eq!(ii(pv_prim_complement_int(mk_int(0))), -1);
    }

    #[test]
    fn to_int32_coercion() {
        assert_eq!(number_to_int(0.0), 0);
        assert_eq!(number_to_int(42.9), 42); // truncate toward zero
        assert_eq!(number_to_int(-42.9), -42);
        assert_eq!(number_to_int(f64::NAN), 0);
        assert_eq!(number_to_int(f64::INFINITY), 0);
        assert_eq!(number_to_int(f64::NEG_INFINITY), 0);
        // 2^32 + 1 ≡ 1 ; 3e9 wraps into the negative half.
        assert_eq!(number_to_int(4_294_967_297.0), 1);
        assert_eq!(number_to_int(3_000_000_000.0), 3_000_000_000i64 as i32);
    }

    #[test]
    fn bool_ops() {
        let (t, f) = (mk_bool(true), mk_bool(false));
        assert!(bb(pv_prim_and_bool(t, t)) && !bb(pv_prim_and_bool(t, f)));
        assert!(bb(pv_prim_or_bool(f, t)) && !bb(pv_prim_or_bool(f, f)));
        assert!(bb(pv_prim_not_bool(f)) && !bb(pv_prim_not_bool(t)));
        assert!(bb(pv_prim_eq_bool(t, t)) && !bb(pv_prim_eq_bool(t, f)));
    }

    #[test]
    fn int_compares() {
        assert!(bb(pv_prim_eq_int(mk_int(5), mk_int(5))));
        assert!(!bb(pv_prim_eq_int(mk_int(5), mk_int(6))));
        assert!(bb(pv_prim_lt_int(mk_int(-1), mk_int(0))));
        assert!(!bb(pv_prim_lt_int(mk_int(0), mk_int(0))));
    }
}
