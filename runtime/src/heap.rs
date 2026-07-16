//! Heap object header, kinds, and pointer type (ADR-0064 §2).
//!
//! This module fixes the **layout contract** the codegen and the (forthcoming) Cheney collector
//! agree on. The allocator, the moving collector, and the explicit field accessors
//! (`read_field` / `write_field`) land in the next increment; here we pin the header encoding and
//! the per-kind layout so both sides can be written against it.
//!
//! Every field-carrying object begins with one [`Header`] word, followed by `size_words` payload
//! words. The header `kind` is the **layout descriptor**: it says which payload words are *value
//! slots* (a [`crate::TaggedWord`], scanned by the tag rule) and which are *raw* (a code pointer,
//! an `f64`, bytes, ids — never scanned). See [`Kind`] for each shape.

use core::ptr::NonNull;

/// The kind of a heap object = its layout descriptor (ADR-0064 §2).
///
/// Value-slot vs raw layout per kind (payload words after the header):
/// - [`Adt`](Kind::Adt): `[ctor_tag: raw] ++ [field: value; n]`.
/// - [`Record`](Kind::Record): `[label_ids: value ptr → raw-id array] [values: value ptr → value
///   array]` — *both fields are pointers* (value slots the collector traces); the label-id array they
///   point to is all-raw. Dictionaries use this kind. (The raw-id-array kind and `new_record` land
///   with records; the value array uses [`Array`](Kind::Array).)
/// - [`Closure`](Kind::Closure): `[code: raw fn ptr] [arity: raw] [env: value → shared env block]`.
///   The env is a **pointer to a separate value-slot block**, *not* inline: a mutually-recursive
///   group shares **one** env block, knot-tied by back-patching each member into it (function
///   members are `(static code ptr, shared env)` — a pure back-patch; a heterogeneous value member
///   that forces a sibling during construction is a [`ByNeed`](Kind::ByNeed) cell — ADR-0059 §1 /
///   ADR-0024). Free variables, including sibling functions, are read positionally from the block.
///   (The value-slot-block kind is defined with the allocator.)
/// - [`Pap`](Kind::Pap): `[function: value] [remaining_arity: raw] [captured: value; n]`.
/// - [`Str`](Kind::Str): `[len: raw] [utf8 bytes: raw]`.
/// - [`NumberBox`](Kind::NumberBox): `[f64: raw]`.
/// - [`Ref`](Kind::Ref): `[cell: value (mutable)]`.
/// - [`ByNeed`](Kind::ByNeed): `[state: raw] [result: value]`.
/// - [`Array`](Kind::Array): `[slot: value; n]` — a value-slot array (`size == n`). PureScript
///   `Array`, a closure's shared env block, and a record's value array all use this kind. (The
///   empty-array singleton is deferred.)
/// - [`RawIds`](Kind::RawIds): `[count: raw] [id: raw; count]` — a `Record`'s sorted FNV-1a-64 label
///   ids (ADR-0069). **All-raw** (no value slots): the collector moves the object but never
///   interprets its words. The `count` word lets it carry its length; an empty record stores no
///   `RawIds` (an immediate sentinel in the `Record` slots instead).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Kind {
    Adt = 0,
    Record = 1,
    Closure = 2,
    Pap = 3,
    Str = 4,
    NumberBox = 5,
    Ref = 6,
    ByNeed = 7,
    Array = 8,
    RawIds = 9,
    /// A borrowed **view** into a packed [`Str`](Kind::Str)'s bytes (ADR-0103 §1):
    /// `[base: value → Str] [byte_off: raw] [byte_len: raw] [cp_len: raw]` — always 4 payload
    /// words. Built only by the bounded-retention slice builders (never nested — slicing a slice
    /// re-bases onto the underlying `Str`); `cp_len` is the code-point count when known, the
    /// `CP_UNKNOWN` sentinel otherwise (memoised on first demand, ADR-0103 §2). The discriminant
    /// is ABI (ADR-0064 §2), fixed here, not to drift.
    StrSlice = 10,
}

impl Kind {
    /// Decode a header kind byte. Returns `None` for an unknown tag (a corrupt header).
    #[inline]
    pub const fn from_u8(b: u8) -> Option<Kind> {
        match b {
            0 => Some(Kind::Adt),
            1 => Some(Kind::Record),
            2 => Some(Kind::Closure),
            3 => Some(Kind::Pap),
            4 => Some(Kind::Str),
            5 => Some(Kind::NumberBox),
            6 => Some(Kind::Ref),
            7 => Some(Kind::ByNeed),
            8 => Some(Kind::Array),
            9 => Some(Kind::RawIds),
            10 => Some(Kind::StrSlice),
            _ => None,
        }
    }
}

/// GC colour. **Reserved for v2** (mark-sweep of the shared heap, generational collection). The v1
/// Cheney collector does not mark; it relocates by forwarding (see [`Header::is_forwarded`]).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Color {
    White = 0,
    Grey = 1,
    Black = 2,
}

/// One object header word: `{ kind: 8, forwarded: 1, colour: 2, size_words: 53 }`.
///
/// **Invariant:** every heap object has `size_words >= 1`. v1 has no size-0 objects (nullary
/// constructors are immediates; every field-carrying kind has at least one payload word), and
/// forwarding needs that first word to hold the address — so [`Header::new`] rejects size 0
/// (else a forwarded size-0 object would write its forwarding address over the *next* object's
/// header).
///
/// **Forwarding contract (ADR-0064 §2):** the collector reads `kind`/`size` when it evacuates a
/// *live* object; once an object is forwarded, following a pointer to it reads **only the forwarding
/// address in the first payload word** — `kind`/`size` are not re-read and no re-copy happens.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Header(u64);

const KIND_MASK: u64 = 0xFF;
const FWD_BIT: u64 = 1 << 8;
const COLOR_SHIFT: u64 = 9;
const COLOR_MASK: u64 = 0b11 << COLOR_SHIFT;
const SIZE_SHIFT: u64 = 11;

impl Header {
    /// A live object header. `size_words` is the payload length (words after this header).
    #[inline]
    pub const fn new(kind: Kind, size_words: u64, colour: Color) -> Header {
        // Release-on: a size-0 object would corrupt the *next* object when forwarded (the forwarding
        // address goes in the first payload word), and an over-53-bit size would overflow the field.
        // These are memory-safety invariants the collector relies on, so `assert!`, not `debug_assert!`.
        assert!(
            size_words >= 1,
            "every heap object needs >= 1 payload word (forwarding stores the address there)"
        );
        assert!(size_words < (1 << 53), "size_words must fit in 53 bits");
        Header((kind as u64) | ((colour as u64) << COLOR_SHIFT) | (size_words << SIZE_SHIFT))
    }

    /// The object kind (layout descriptor). Panics only on a corrupt header (unknown kind).
    #[inline]
    pub fn kind(self) -> Kind {
        Kind::from_u8((self.0 & KIND_MASK) as u8).expect("corrupt header: unknown kind")
    }

    /// The payload length in words (words after the header).
    #[inline]
    pub const fn size_words(self) -> u64 {
        self.0 >> SIZE_SHIFT
    }

    /// The GC colour (reserved for v2).
    #[inline]
    pub const fn colour(self) -> Color {
        match (self.0 & COLOR_MASK) >> COLOR_SHIFT {
            0 => Color::White,
            1 => Color::Grey,
            _ => Color::Black,
        }
    }

    /// `true` when the object has been evacuated (its first payload word is the forwarding address).
    #[inline]
    pub const fn is_forwarded(self) -> bool {
        self.0 & FWD_BIT != 0
    }

    /// Mark this header forwarded (set by the collector after the object is fully copied).
    #[inline]
    pub const fn with_forwarded(self) -> Header {
        Header(self.0 | FWD_BIT)
    }

    /// Raw bits (for storing the header word / debugging).
    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0
    }

    /// Reconstruct a header from its stored word.
    #[inline]
    pub const fn from_bits(bits: u64) -> Header {
        Header(bits)
    }
}

impl core::fmt::Debug for Header {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_forwarded() {
            write!(f, "Header(FORWARDED, size={})", self.size_words())
        } else {
            write!(
                f,
                "Header({:?}, size={}, {:?})",
                self.kind(),
                self.size_words(),
                self.colour()
            )
        }
    }
}

/// A pointer to a heap object (its [`Header`]). A *typed handle*, never a Rust reference into the
/// moving heap (ADR-0063 §2). The unsafe field accessors arrive with the allocator/collector; this
/// is the type + its word conversions.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct HeapPtr(NonNull<Header>);

impl HeapPtr {
    /// # Safety
    /// `ptr` must point to a live, header-prefixed object in a capability-local heap.
    #[inline]
    pub const unsafe fn from_raw(ptr: NonNull<Header>) -> HeapPtr {
        HeapPtr(ptr)
    }

    /// Reconstruct the [`HeapPtr`] a *pointer* value word denotes (ADR-0064 §1).
    ///
    /// # Safety
    /// `w` must be a pointer word (`w.is_pointer()`) to a live, header-prefixed object.
    #[inline]
    pub unsafe fn from_word(w: crate::TaggedWord) -> HeapPtr {
        debug_assert!(w.is_pointer(), "from_word: not a pointer word");
        HeapPtr::from_raw(NonNull::new_unchecked(w.addr() as *mut Header))
    }

    /// The raw header pointer.
    #[inline]
    pub const fn as_ptr(self) -> *mut Header {
        self.0.as_ptr()
    }

    /// This pointer as a [`crate::TaggedWord`] (`LSB = 0`, ADR-0064 §1).
    #[inline]
    pub fn as_word(self) -> crate::TaggedWord {
        crate::TaggedWord::from_addr(self.0.as_ptr() as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_round_trips() {
        for kind in [
            Kind::Adt,
            Kind::Record,
            Kind::Closure,
            Kind::Pap,
            Kind::Str,
            Kind::NumberBox,
            Kind::Ref,
            Kind::ByNeed,
            Kind::Array,
        ] {
            for size in [1u64, 2, 42, 1 << 20, (1 << 53) - 1] {
                for colour in [Color::White, Color::Grey, Color::Black] {
                    let h = Header::new(kind, size, colour);
                    assert_eq!(h.kind(), kind);
                    assert_eq!(h.size_words(), size, "size round-trip failed");
                    assert_eq!(h.colour(), colour);
                    assert!(!h.is_forwarded());
                }
            }
        }
    }

    #[test]
    fn with_forwarded_sets_only_the_flag() {
        // `with_forwarded` sets the flag bit and leaves the other bits physically intact. Following a
        // forwarded pointer reads the forwarding address from the first payload word (§2 contract) —
        // the collector does not re-read kind/size, so no "size after forwarding" property is needed.
        let live = Header::new(Kind::Adt, 7, Color::White);
        assert!(!live.is_forwarded());
        let fwd = live.with_forwarded();
        assert!(fwd.is_forwarded());
        assert_eq!(fwd.to_bits(), live.to_bits() | (1 << 8));
    }

    #[test]
    #[should_panic]
    fn header_rejects_size_zero() {
        // `Header::new` now `assert!`s (release-on), so this holds regardless of build profile.
        let _ = Header::new(Kind::Adt, 0, Color::White);
    }

    #[test]
    fn kind_from_u8_rejects_unknown() {
        assert_eq!(Kind::from_u8(0), Some(Kind::Adt));
        assert_eq!(Kind::from_u8(7), Some(Kind::ByNeed));
        assert_eq!(Kind::from_u8(8), Some(Kind::Array));
        assert_eq!(Kind::from_u8(9), Some(Kind::RawIds));
        assert_eq!(Kind::from_u8(10), Some(Kind::StrSlice));
        assert_eq!(Kind::from_u8(11), None);
        assert_eq!(Kind::from_u8(255), None);
    }
}
