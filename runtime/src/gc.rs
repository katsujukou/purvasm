//! The v1 local heap: a semi-space (Cheney) copying collector (ADR-0064 §5).
//!
//! This is the **unsafe GC island** (ADR-0063): raw memory, tagged words, object relocation. It
//! exposes a small typed API (the `new_*` constructors, `read_field`/`write_field`, `collect`) and
//! never hands out a Rust reference into the moving heap. The low-level `alloc` is crate-internal —
//! the public surface fully initialises objects via `new_*`.
//!
//! Layout matches [`crate::heap`]: an object is `[Header][payload word; size]`, a [`HeapPtr`] points
//! at the header. Collection is **layout-directed** (ADR-0064 §2): for each live object the header
//! `kind` says which payload words are *value slots* (a [`TaggedWord`] — scanned/relocated) and
//! which are *raw* (skipped). Forwarding stores the new address in the object's first payload word,
//! after a full copy (the [`Header`] contract — every object has `size >= 1`).
//!
//! Scope (ADR-0064 §0): single-capability, sequential; the shared arena, promotion, generational
//! collection, `gc.statepoint`, and precise shadow-stack integration are v2 / later. Here the caller
//! passes its roots to [`Heap::collect`] explicitly (the shadow-stack API lands with codegen).

use crate::heap::{Color, Header, HeapPtr, Kind};
use crate::word::TaggedWord;
use core::ptr::NonNull;

/// Bytes per heap word.
const WORD: usize = core::mem::size_of::<u64>();

/// A single-capability local heap with two equal semi-spaces (ADR-0064 §5).
pub struct Heap {
    /// The active allocation space.
    space: Box<[u64]>,
    /// The idle space — the target of the next collection.
    reserve: Box<[u64]>,
    /// Bump cursor: words used in `space`.
    top: usize,
}

impl Heap {
    /// A heap whose each semi-space holds `words_per_space` words. (Total reservation is therefore
    /// `2 * words_per_space` — the semi-space memory factor, ADR-0064 §2 / Consequences.)
    pub fn new(words_per_space: usize) -> Heap {
        Heap {
            space: vec![0u64; words_per_space].into_boxed_slice(),
            reserve: vec![0u64; words_per_space].into_boxed_slice(),
            top: 0,
        }
    }

    /// Words currently allocated in the active space.
    #[inline]
    pub fn used(&self) -> usize {
        self.top
    }

    // --- allocation ---------------------------------------------------------

    /// Bump-allocate an object of `kind` with `size` payload words, returning a pointer to its
    /// header. The payload is **uninitialised**; the caller must write every field before the object
    /// becomes reachable / a collection scans it (v1: codegen writes all fields with no safepoint in
    /// between — ADR-0064 §4). Panics on overflow (GC-on-overflow needs the roots; deferred).
    fn alloc(&mut self, kind: Kind, size: u64, colour: Color) -> HeapPtr {
        let words = 1 + size as usize;
        assert!(
            self.top + words <= self.space.len(),
            "heap overflow (GC-on-alloc lands with the shadow-stack roots)"
        );
        let base = self.top;
        self.top += words;
        self.space[base] = Header::new(kind, size, colour).to_bits();
        // SAFETY: `base` is in bounds (checked) and the buffer outlives the returned pointer for the
        // heap's lifetime; the header word was just written.
        let hdr = unsafe { self.space.as_mut_ptr().add(base) } as *mut Header;
        // SAFETY: `hdr` is derived from a live boxed-slice element, hence non-null.
        unsafe { HeapPtr::from_raw(NonNull::new_unchecked(hdr)) }
    }

    /// An `Adt` object: payload `[tag: raw] ++ fields`.
    pub fn new_adt(&mut self, tag: u32, fields: &[TaggedWord]) -> HeapPtr {
        let p = self.alloc(Kind::Adt, 1 + fields.len() as u64, Color::White);
        self.write_raw(p, 0, tag as u64);
        for (i, f) in fields.iter().enumerate() {
            self.write_field(p, 1 + i as u64, *f);
        }
        p
    }

    /// A boxed `Number` (`f64`).
    pub fn new_number(&mut self, x: f64) -> HeapPtr {
        let p = self.alloc(Kind::NumberBox, 1, Color::White);
        self.write_raw(p, 0, x.to_bits());
        p
    }

    /// A `Ref` cell holding `init`.
    pub fn new_ref(&mut self, init: TaggedWord) -> HeapPtr {
        let p = self.alloc(Kind::Ref, 1, Color::White);
        self.write_field(p, 0, init);
        p
    }

    /// An `Array` (value-slot array) of `elems`. Non-empty for now — the empty-array singleton is
    /// deferred (ADR-0064 §5).
    pub fn new_array(&mut self, elems: &[TaggedWord]) -> HeapPtr {
        let p = self.alloc(Kind::Array, elems.len() as u64, Color::White);
        for (i, e) in elems.iter().enumerate() {
            self.write_field(p, i as u64, *e);
        }
        p
    }

    /// A `Closure` `[code: raw][arity: raw][env: value slot]` (ADR-0059 §1). `env` is either the
    /// pointer word of a shared env block — an [`Array`](Kind::Array), via [`HeapPtr::as_word`],
    /// back-patched for a mutually-recursive group — or, for a closure with **no captures**, an
    /// **immediate sentinel** (e.g. [`TaggedWord::unit`]). The immediate case is scanned but
    /// pass-through in `collect`, so a no-capture closure needs no (deferred) empty-array object.
    pub fn new_closure(&mut self, code: u64, arity: u32, env: TaggedWord) -> HeapPtr {
        let p = self.alloc(Kind::Closure, 3, Color::White);
        self.write_raw(p, 0, code);
        self.write_raw(p, 1, arity as u64);
        self.write_field(p, 2, env);
        p
    }

    // --- field access -------------------------------------------------------

    /// The header of `p`.
    #[inline]
    pub fn header(&self, p: HeapPtr) -> Header {
        // SAFETY: `p` points to a live header word in this heap.
        Header::from_bits(unsafe { *(p.as_ptr() as *const u64) })
    }

    /// Read payload word `i` as a value slot (a [`TaggedWord`]).
    #[inline]
    pub fn read_field(&self, p: HeapPtr, i: u64) -> TaggedWord {
        TaggedWord::from_bits(self.read_word(p, i))
    }

    /// Write a [`TaggedWord`] into value-slot `i`.
    #[inline]
    pub fn write_field(&mut self, p: HeapPtr, i: u64, w: TaggedWord) {
        self.write_word(p, i, w.to_bits());
    }

    /// Read raw payload word `i` (a code pointer, an `f64` bit pattern, a ctor tag — never a value).
    #[inline]
    pub fn read_raw(&self, p: HeapPtr, i: u64) -> u64 {
        self.read_word(p, i)
    }

    /// Write a raw payload word `i`.
    #[inline]
    pub fn write_raw(&mut self, p: HeapPtr, i: u64, bits: u64) {
        self.write_word(p, i, bits);
    }

    #[inline]
    fn read_word(&self, p: HeapPtr, i: u64) -> u64 {
        // Release-on: a bad index behind this safe API is an out-of-bounds raw read/write.
        assert!(i < self.header(p).size_words(), "field index out of range");
        // SAFETY: `i < size` (checked above); the payload word is within the live object.
        unsafe { *(p.as_ptr() as *const u64).add(1 + i as usize) }
    }

    #[inline]
    fn write_word(&mut self, p: HeapPtr, i: u64, bits: u64) {
        // Release-on: a bad index behind this safe API is an out-of-bounds raw read/write.
        assert!(i < self.header(p).size_words(), "field index out of range");
        // SAFETY: `i < size` (checked above); the payload word is within the live object.
        unsafe { *(p.as_ptr() as *mut u64).add(1 + i as usize) = bits };
    }

    // --- collection (Cheney) ------------------------------------------------

    /// Collect the local heap, keeping only what is reachable from `roots`. The reachable graph is
    /// copied into the reserve space; `roots` are **rewritten in place** to the survivors' new
    /// addresses (any [`HeapPtr`] / pointer `TaggedWord` held elsewhere across a `collect` is stale —
    /// this models the shadow-stack root-reload contract, ADR-0064 §4). Sharing and cycles are
    /// preserved by forwarding.
    pub fn collect(&mut self, roots: &mut [TaggedWord]) {
        // Split-borrow the two semi-spaces: `from` is scanned, `to` receives survivors.
        let from: &mut [u64] = self.space.as_mut();
        let to: &mut [u64] = self.reserve.as_mut();
        let mut free = 0usize;

        for r in roots.iter_mut() {
            *r = evacuate(from, to, &mut free, *r);
        }

        // Cheney scavenge: walk the objects already in `to`, evacuating their value slots. New
        // survivors are appended past `free`, so the loop terminates when `scan` catches up.
        let mut scan = 0usize;
        while scan < free {
            let hdr = Header::from_bits(to[scan]);
            let size = hdr.size_words() as usize;
            let [(a0, a1), (b0, b1)] = value_slot_ranges(hdr.kind(), hdr.size_words());
            for i in (a0..a1).chain(b0..b1) {
                let idx = scan + 1 + i as usize;
                let w = TaggedWord::from_bits(to[idx]);
                let moved = evacuate(from, to, &mut free, w);
                to[idx] = moved.to_bits();
            }
            scan += 1 + size;
        }

        self.top = free;
        core::mem::swap(&mut self.space, &mut self.reserve);
    }
}

/// Evacuate a word: an immediate is returned unchanged; a pointer to a from-space object is copied
/// to `to` (once — a forwarded object returns its recorded new address) and the updated pointer is
/// returned.
fn evacuate(from: &mut [u64], to: &mut [u64], free: &mut usize, w: TaggedWord) -> TaggedWord {
    if w.is_immediate() {
        return w;
    }
    let from_base = from.as_ptr() as usize;
    // Defensive: a pointer must be word-aligned and inside the from-space. A shared/foreign pointer
    // (v2 S1 arena) needs the ownership dispatch — evacuate-iff-local — added then; here we trap it.
    debug_assert!(
        w.addr().is_multiple_of(WORD)
            && w.addr() >= from_base
            && w.addr() < from_base + from.len() * WORD,
        "evacuate: pointer outside the from-space"
    );
    let obj = (w.addr() - from_base) / WORD; // header word index in `from`
    let hdr = Header::from_bits(from[obj]);
    if hdr.is_forwarded() {
        // The forwarding address is a raw byte address in the first payload word.
        return TaggedWord::from_addr(from[obj + 1] as usize);
    }
    let words = 1 + hdr.size_words() as usize;
    let dst = *free;
    to[dst..dst + words].copy_from_slice(&from[obj..obj + words]);
    let new_addr = to.as_ptr() as usize + dst * WORD;
    // Install forwarding in the from-space object (after the copy, so no live data is lost).
    from[obj] = hdr.with_forwarded().to_bits();
    from[obj + 1] = new_addr as u64;
    *free += words;
    TaggedWord::from_addr(new_addr)
}

/// The payload-index ranges that are **value slots** for a given kind and size (ADR-0064 §2). Up to
/// two `[start, end)` ranges; everything outside them is a raw word the collector never interprets.
fn value_slot_ranges(kind: Kind, size: u64) -> [(u64, u64); 2] {
    match kind {
        // [tag: raw] ++ [field: value; ..]
        Kind::Adt => [(1, size), (0, 0)],
        // [label_ids: value ptr → raw-id array] [values: value ptr → value array] — both fields are
        // pointers the collector traces (the pointed-to arrays are separate objects; `new_record`
        // and the raw-id-array kind are deferred).
        Kind::Record => [(0, 2), (0, 0)],
        // [code: raw] [arity: raw] [env: value ptr → shared env block] — env is ONE value slot (the
        // pointer); captures live in the separate env `Array`, scanned there (ADR-0059 §1).
        Kind::Closure => [(2, 3), (0, 0)],
        // [function: value] [remaining_arity: raw] [captured: value; ..]
        Kind::Pap => [(0, 1), (2, size)],
        // [len: raw] [utf8 bytes: raw]
        Kind::Str => [(0, 0), (0, 0)],
        // [f64: raw]
        Kind::NumberBox => [(0, 0), (0, 0)],
        // [cell: value]
        Kind::Ref => [(0, 1), (0, 0)],
        // [state: raw] [result: value]
        Kind::ByNeed => [(1, 2), (0, 0)],
        // [slot: value; n] — every slot is a value slot.
        Kind::Array => [(0, size), (0, 0)],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_and_field_access() {
        let mut h = Heap::new(64);
        let n = h.new_number(3.5);
        assert_eq!(h.header(n).kind(), Kind::NumberBox);
        assert_eq!(f64::from_bits(h.read_raw(n, 0)), 3.5);

        let adt = h.new_adt(2, &[TaggedWord::int(7), n.as_word()]);
        assert_eq!(h.header(adt).kind(), Kind::Adt);
        assert_eq!(h.read_raw(adt, 0), 2); // ctor tag
        assert_eq!(h.read_field(adt, 1).as_int(), 7);
        assert_eq!(h.read_field(adt, 2), n.as_word());
    }

    #[test]
    fn collect_keeps_reachable_drops_dead() {
        let mut h = Heap::new(64);
        let _dead = h.new_number(9.9); // unreachable
        let num = h.new_number(1.25);
        let live = h.new_adt(1, &[TaggedWord::int(42), num.as_word()]);
        let before = h.used();
        assert!(before > 0);

        let mut roots = [live.as_word()];
        h.collect(&mut roots);

        // ADT (1 header + 3 payload) + NumberBox (1 header + 1 payload) survive; the dead one is gone.
        assert_eq!(h.used(), (1 + 3) + (1 + 1));
        assert!(h.used() < before);

        // The root was rewritten; the survivor's fields are intact.
        let live2 = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.read_field(live2, 1).as_int(), 42);
        let num = unsafe { HeapPtr::from_word(h.read_field(live2, 2)) };
        assert_eq!(f64::from_bits(h.read_raw(num, 0)), 1.25);
    }

    #[test]
    fn collect_preserves_sharing() {
        let mut h = Heap::new(64);
        let shared = h.new_number(2.0);
        // Two roots referencing the *same* object.
        let mut roots = [shared.as_word(), shared.as_word()];
        h.collect(&mut roots);
        // Copied exactly once (forwarding dedup): both roots now point at the same new address.
        assert_eq!(roots[0], roots[1]);
        assert_eq!(h.used(), 1 + 1); // one NumberBox
    }

    #[test]
    fn collect_preserves_cycles() {
        let mut h = Heap::new(64);
        // Build a knot A <-> B (as mutual recursion would, ADR-0059 §1).
        let a = h.new_adt(0, &[TaggedWord::unit()]); // field back-patched below
        let b = h.new_adt(1, &[a.as_word()]);
        h.write_field(a, 1, b.as_word()); // A.field0 = B  (payload index 1)

        let mut roots = [a.as_word()];
        h.collect(&mut roots); // must terminate despite the cycle

        let a2 = unsafe { HeapPtr::from_word(roots[0]) };
        let b2 = unsafe { HeapPtr::from_word(h.read_field(a2, 1)) };
        // The cycle is intact: B.field0 points back at A.
        assert_eq!(h.read_field(b2, 1), a2.as_word());
        assert_eq!(h.header(a2).kind(), Kind::Adt);
        assert_eq!(h.read_raw(b2, 0), 1); // B's ctor tag preserved
        assert_eq!(h.used(), (1 + 2) + (1 + 2)); // A and B, once each
    }

    #[test]
    fn collect_traces_all_closure_captures() {
        // A closure over a shared env block with *two* heap captures — the multi-capture case
        // (ADR-0059 §1). Both must survive; regression against tracing only the first env slot.
        let mut h = Heap::new(64);
        let c1 = h.new_number(1.0);
        let c2 = h.new_number(2.0);
        let env = h.new_array(&[c1.as_word(), c2.as_word()]);
        let clo = h.new_closure(0xC0DE, 1, env.as_word());

        let mut roots = [clo.as_word()];
        h.collect(&mut roots);

        let clo2 = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.read_raw(clo2, 0), 0xC0DE); // code pointer preserved (raw)
        let env2 = unsafe { HeapPtr::from_word(h.read_field(clo2, 2)) };
        assert_eq!(h.header(env2).kind(), Kind::Array);
        assert_eq!(h.header(env2).size_words(), 2);
        let cap1 = unsafe { HeapPtr::from_word(h.read_field(env2, 0)) };
        let cap2 = unsafe { HeapPtr::from_word(h.read_field(env2, 1)) };
        assert_eq!(f64::from_bits(h.read_raw(cap1, 0)), 1.0);
        assert_eq!(f64::from_bits(h.read_raw(cap2, 0)), 2.0); // the *second* capture survived
    }

    #[test]
    fn collect_closure_with_no_captures() {
        // A top-level function with no free variables is still a closure (all calls go through
        // `apply`, §3); its env is an immediate sentinel, not an empty array (which stays deferred).
        let mut h = Heap::new(64);
        let clo = h.new_closure(0xF00D, 0, TaggedWord::unit());
        let mut roots = [clo.as_word()];
        h.collect(&mut roots);

        let clo2 = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.read_raw(clo2, 0), 0xF00D);
        assert!(h.read_field(clo2, 2).is_immediate()); // env sentinel scanned but passed through
        assert_eq!(h.used(), 1 + 3); // just the closure — no env object
    }
}
