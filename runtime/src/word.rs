//! The v1 tagged-word value representation (ADR-0064 §1).
//!
//! A [`TaggedWord`] is a 64-bit machine word whose **least-significant bit** is the tag:
//! `LSB = 1` → an *immediate* carrying a 63-bit arithmetic-shifted payload; `LSB = 0` → an aligned
//! heap pointer. `Int` / `Char` / `Boolean` / `Unit` / nullary-constructor indices are immediates
//! (unboxed, 64-bit only — ADR-0064 §1); `Number` and every field-carrying object are heap
//! pointers.
//!
//! The word is **type-erased**: the same immediate `0` is `false`, `Int 0`, `Unit`, and nullary
//! constructor `#0`. Nothing here disambiguates them — the code that consumes a value already knows
//! its (erased) type and picks the accessor (ADR-0011). The `int` / `bool` / `char` / `nullary_ctor`
//! constructors are therefore just *interpretations* of the immediate encoding, not distinct tags.

/// A 64-bit tagged guest word. See the module docs.
///
/// **`==` is physical (bit) equality, *not* PureScript `Eq`.** For immediates it coincides with
/// guest equality; for pointers it is *reference identity* (like `unsafeRefEq`), never structural
/// equality — structural `Eq` is the codegen's job via dictionaries. The derive is kept because bit
/// equality is exactly what the collector and the tests need; runtime code must not mistake
/// `a == b` for guest `Eq`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TaggedWord(u64);

/// The low tag bit: `1` = immediate, `0` = pointer.
const TAG_BIT: u64 = 1;

impl TaggedWord {
    // --- raw encoding -------------------------------------------------------

    /// An immediate carrying `payload` in its 63 high bits (`(payload << 1) | 1`).
    ///
    /// `payload` must fit in 63 bits (`[-2^62, 2^62)`); every guest immediate (`Int` 32-bit,
    /// `Char` ≤ 21-bit, `Boolean`, small ctor tags) fits comfortably.
    #[inline]
    pub const fn immediate(payload: i64) -> Self {
        // Symmetric with `from_addr`'s alignment check: a payload ≥ 2^62 would silently lose its top
        // bit to `<< 1` (a shift does not trip `overflow-checks`).
        debug_assert!(
            payload >= -(1 << 62) && payload < (1 << 62),
            "immediate payload must fit in 63 bits"
        );
        TaggedWord(((payload as u64) << 1) | TAG_BIT)
    }

    /// A heap pointer to a 2-aligned address (every heap object is at least word-aligned, so
    /// `LSB = 0` holds by construction).
    ///
    /// **Crate-internal.** The only public way to mint a *pointer* word is
    /// [`HeapPtr::as_word`](crate::HeapPtr::as_word) from a real allocated object, so safe callers
    /// cannot forge a pointer to an arbitrary address (which `apply` / `collect` would then deref as
    /// a header — UB). This is the "a pointer word is heap-owned" invariant.
    #[inline]
    pub(crate) fn from_addr(addr: usize) -> Self {
        debug_assert_eq!(
            addr & (TAG_BIT as usize),
            0,
            "heap pointer must be 2-aligned"
        );
        TaggedWord(addr as u64)
    }

    /// `true` when this word is an immediate (`LSB = 1`).
    #[inline]
    pub const fn is_immediate(self) -> bool {
        self.0 & TAG_BIT == TAG_BIT
    }

    /// `true` when this word is a heap pointer (`LSB = 0`).
    #[inline]
    pub const fn is_pointer(self) -> bool {
        self.0 & TAG_BIT == 0
    }

    /// The 63-bit immediate payload, sign-extended (arithmetic shift). Only meaningful when
    /// [`is_immediate`](Self::is_immediate).
    #[inline]
    pub const fn payload(self) -> i64 {
        (self.0 as i64) >> 1
    }

    /// The heap address. Only meaningful when [`is_pointer`](Self::is_pointer).
    #[inline]
    pub const fn addr(self) -> usize {
        self.0 as usize
    }

    /// The raw bit pattern — the value that crosses the codegen / C-ABI boundary as a plain machine
    /// word (ADR-0063 §2: never a Rust reference).
    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0
    }

    /// Reconstruct from a raw bit pattern. **Crate-internal** for the same reason as
    /// [`from_addr`](Self::from_addr): an arbitrary `bits` with `LSB = 0` is a forged pointer word.
    /// The inbound codegen / C-ABI boundary will re-expose a dedicated `unsafe` entry when it lands;
    /// until then only the runtime reconstructs already-stored words (e.g. `read_field`).
    #[inline]
    pub(crate) const fn from_bits(bits: u64) -> Self {
        TaggedWord(bits)
    }

    // --- guest scalar interpretations (all immediates) ----------------------

    /// An `Int` (32-bit wrapping, ADR-0006). The payload is the **sign-extended `i32`** — the
    /// canonical form `EqInt` / `OrdInt` / `show` / the FFI boundary read directly (ADR-0064 §1).
    #[inline]
    pub const fn int(v: i32) -> Self {
        Self::immediate(v as i64)
    }

    /// Read as an `Int`, taking the low 32 bits (the canonical payload is already a sign-extended
    /// `i32`, so this round-trips [`int`](Self::int) exactly).
    #[inline]
    pub const fn as_int(self) -> i32 {
        self.payload() as i32
    }

    /// A `Boolean`.
    #[inline]
    pub const fn bool(b: bool) -> Self {
        Self::immediate(b as i64)
    }

    /// Read as a `Boolean` (`payload != 0`).
    #[inline]
    pub const fn as_bool(self) -> bool {
        self.payload() != 0
    }

    /// A `Char` — a Unicode code point (ADR-0059; code-point, not UTF-16 code-unit).
    #[inline]
    pub const fn char(code_point: u32) -> Self {
        debug_assert!(
            code_point <= 0x10FFFF && (code_point < 0xD800 || code_point > 0xDFFF),
            "invalid Unicode code point (surrogate or > U+10FFFF)"
        );
        Self::immediate(code_point as i64)
    }

    /// Read as a `Char` code point.
    #[inline]
    pub const fn as_char(self) -> u32 {
        self.payload() as u32
    }

    /// `Unit` — the immediate `0` (ADR-0059).
    #[inline]
    pub const fn unit() -> Self {
        Self::immediate(0)
    }

    /// A nullary constructor index (per-constructor `i31`, ADR-0059 §1).
    #[inline]
    pub const fn nullary_ctor(tag: u32) -> Self {
        Self::immediate(tag as i64)
    }

    /// Read a nullary constructor's index.
    #[inline]
    pub const fn as_ctor_tag(self) -> u32 {
        self.payload() as u32
    }
}

impl core::fmt::Debug for TaggedWord {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_immediate() {
            write!(f, "Imm({})", self.payload())
        } else {
            write!(f, "Ptr(0x{:x})", self.addr())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn immediate_and_pointer_are_disjoint() {
        assert!(TaggedWord::int(7).is_immediate());
        assert!(!TaggedWord::int(7).is_pointer());
        // A 2-aligned address decodes as a pointer.
        let p = TaggedWord::from_addr(0x1000);
        assert!(p.is_pointer());
        assert!(!p.is_immediate());
        assert_eq!(p.addr(), 0x1000);
    }

    #[test]
    fn int_round_trips_including_bounds_and_negatives() {
        for v in [
            0i32,
            1,
            -1,
            42,
            -42,
            i32::MIN,
            i32::MAX,
            i32::MIN + 1,
            i32::MAX - 1,
        ] {
            let w = TaggedWord::int(v);
            assert!(w.is_immediate());
            assert_eq!(w.as_int(), v, "int round-trip failed for {v}");
        }
    }

    #[test]
    fn int_payload_is_sign_extended_canonical() {
        // The canonical payload equals the mathematical signed value (ADR-0064 §1).
        assert_eq!(TaggedWord::int(-1).payload(), -1);
        assert_eq!(TaggedWord::int(i32::MIN).payload(), i32::MIN as i64);
        assert_eq!(TaggedWord::int(i32::MAX).payload(), i32::MAX as i64);
    }

    #[test]
    fn bool_round_trips() {
        assert!(!TaggedWord::bool(false).as_bool());
        assert!(TaggedWord::bool(true).as_bool());
    }

    #[test]
    fn char_round_trips_including_astral() {
        for cp in [0u32, 0x41, 0x3042, 0x10FFFF] {
            assert_eq!(TaggedWord::char(cp).as_char(), cp);
        }
    }

    #[test]
    fn nullary_ctor_round_trips() {
        for tag in [0u32, 1, 2, 255, 1000] {
            assert_eq!(TaggedWord::nullary_ctor(tag).as_ctor_tag(), tag);
        }
    }

    #[test]
    fn type_erasure_immediate_zero_is_shared() {
        // false / Int 0 / Unit / nullary ctor #0 are the *same* word — intentional, disambiguated
        // by the (erased) type of the consumer, not by the representation (ADR-0011).
        let zero = TaggedWord::immediate(0);
        assert_eq!(TaggedWord::bool(false), zero);
        assert_eq!(TaggedWord::int(0), zero);
        assert_eq!(TaggedWord::unit(), zero);
        assert_eq!(TaggedWord::nullary_ctor(0), zero);
    }

    #[test]
    fn bits_round_trip() {
        let w = TaggedWord::int(-12345);
        assert_eq!(TaggedWord::from_bits(w.to_bits()), w);
        let p = TaggedWord::from_addr(0xdead_beef0);
        assert_eq!(TaggedWord::from_bits(p.to_bits()), p);
    }

    #[test]
    #[should_panic]
    #[cfg(debug_assertions)]
    fn char_rejects_surrogate() {
        let _ = TaggedWord::char(0xD800);
    }

    #[test]
    #[should_panic]
    #[cfg(debug_assertions)]
    fn immediate_rejects_63bit_overflow() {
        let _ = TaggedWord::immediate(1 << 62);
    }
}
