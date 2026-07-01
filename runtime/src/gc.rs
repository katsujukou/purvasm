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

use crate::apply::CodeFn;
use crate::heap::{Color, Header, HeapPtr, Kind};
use crate::word::TaggedWord;
use core::ptr::NonNull;

/// Bytes per heap word.
const WORD: usize = core::mem::size_of::<u64>();

/// A single-capability local heap with two equal semi-spaces (ADR-0064 §5).
///
/// Each semi-space is a raw allocation reached through a **single stable base pointer** whose
/// provenance spans the whole buffer; every [`HeapPtr`] is derived from that base and never through a
/// fresh `&mut`/`&` re-borrow of the buffer. This is load-bearing (ADR-0063 §2/§8): re-borrowing the
/// buffer as `&mut` (`Box::as_mut_ptr`, slice indexing, `as_mut()`) would issue a Unique retag that
/// **invalidates every outstanding `HeapPtr`** — a use-after-invalidation the borrow model rejects
/// (and `noalias`-based codegen could miscompile). Only raw-pointer arithmetic off the stable base
/// touches the heap after construction.
pub struct Heap {
    /// Stable base of the active allocation space (`cap` words; owns the allocation, freed in `Drop`).
    space: NonNull<u64>,
    /// Stable base of the idle space — the target of the next collection.
    reserve: NonNull<u64>,
    /// Words per semi-space (both are equal).
    cap: usize,
    /// Bump cursor: words used in `space`.
    top: usize,
    /// v1 bring-up code registry: real [`CodeFn`] pointers (with provenance) keyed by the index a
    /// closure stores in its `code` word. `apply` calls by index — no integer→fn transmute, which
    /// would be a provenance-free (UB) call. The native ABI stores a real code address here instead
    /// (ADR-0064 §3 Correction). Never relocated by GC.
    code_table: Vec<CodeFn>,
}

impl Heap {
    /// A heap whose each semi-space holds `words_per_space` words. (Total reservation is therefore
    /// `2 * words_per_space` — the semi-space memory factor, ADR-0064 §2 / Consequences.)
    pub fn new(words_per_space: usize) -> Heap {
        Heap {
            space: alloc_space(words_per_space),
            reserve: alloc_space(words_per_space),
            cap: words_per_space,
            top: 0,
            code_table: Vec::new(),
        }
    }

    /// Words currently allocated in the active space.
    #[inline]
    pub fn used(&self) -> usize {
        self.top
    }

    /// Intern a v1 code entry, returning the index a closure stores in its `code` word (see
    /// [`code_table`](Heap::code_table)).
    #[inline]
    pub(crate) fn intern_code(&mut self, code: CodeFn) -> u64 {
        let idx = self.code_table.len() as u64;
        self.code_table.push(code);
        idx
    }

    /// The [`CodeFn`] a closure's `code` word (an index) denotes. Bounds-checked: a bad index panics
    /// (safe) rather than reaching a provenance-free call (UB).
    #[inline]
    pub(crate) fn code_at(&self, idx: u64) -> CodeFn {
        self.code_table[idx as usize]
    }

    // --- allocation ---------------------------------------------------------

    /// Bump-allocate an object of `kind` with `size` payload words, returning a pointer to its
    /// header. The payload is **uninitialised**; the caller must write every field before the object
    /// becomes reachable / a collection scans it (v1: codegen writes all fields with no safepoint in
    /// between — ADR-0064 §4). Panics on overflow (GC-on-overflow needs the roots; deferred).
    fn alloc(&mut self, kind: Kind, size: u64, colour: Color) -> HeapPtr {
        let words = 1 + size as usize;
        assert!(
            self.top + words <= self.cap,
            "heap overflow (GC-on-alloc lands with the shadow-stack roots)"
        );
        let base = self.top;
        self.top += words;
        // Derive the header pointer from the stable base (never a fresh buffer re-borrow — see the
        // struct docs). SAFETY: `base < cap` (checked), so the word is within the space allocation.
        let hdr = unsafe { self.space.as_ptr().add(base) };
        unsafe { *hdr = Header::new(kind, size, colour).to_bits() };
        // SAFETY: `hdr` is a live element of the space allocation, hence non-null; it is the header.
        unsafe { HeapPtr::from_raw(NonNull::new_unchecked(hdr as *mut Header)) }
    }

    /// An `Adt` object: payload `[tag: raw] ++ fields`.
    pub fn new_adt(&mut self, tag: u32, fields: &[TaggedWord]) -> HeapPtr {
        let p = self.alloc(Kind::Adt, 1 + fields.len() as u64, Color::White);
        self.write_raw_unchecked(p, 0, tag as u64);
        for (i, f) in fields.iter().enumerate() {
            self.write_field_unchecked(p, 1 + i as u64, *f);
        }
        p
    }

    /// A boxed `Number` (`f64`).
    pub fn new_number(&mut self, x: f64) -> HeapPtr {
        let p = self.alloc(Kind::NumberBox, 1, Color::White);
        self.write_raw_unchecked(p, 0, x.to_bits());
        p
    }

    /// A `Ref` cell holding `init`.
    pub fn new_ref(&mut self, init: TaggedWord) -> HeapPtr {
        let p = self.alloc(Kind::Ref, 1, Color::White);
        self.write_field_unchecked(p, 0, init);
        p
    }

    /// An `Array` (value-slot array) of `elems`. **Non-empty**: the empty-array singleton is deferred
    /// (ADR-0064 §5), so a zero-length array would otherwise trip the `size >= 1` header invariant.
    pub fn new_array(&mut self, elems: &[TaggedWord]) -> HeapPtr {
        assert!(
            !elems.is_empty(),
            "empty array is deferred (needs the immortal singleton, ADR-0064 §5)"
        );
        let p = self.alloc(Kind::Array, elems.len() as u64, Color::White);
        for (i, e) in elems.iter().enumerate() {
            self.write_field_unchecked(p, i as u64, *e);
        }
        p
    }

    /// A `Closure` from a **raw** code word `[code: raw][arity: raw][env: value slot]` (ADR-0059 §1).
    /// For `env` see [`Heap::new_closure`]. Safe Rust callers should use the typed
    /// [`Heap::new_closure`], which interns a [`crate::CodeFn`] and passes its index here.
    ///
    /// In v1 `code` is an index into the heap's code table (via [`Heap::intern_code`]); the native
    /// ABI stores a real code address instead (ADR-0064 §3 Correction). `apply` looks the index up
    /// with a bounds check, so a bad `code` word panics rather than reaching UB.
    ///
    /// # Safety
    /// Currently sound for any `code` (an out-of-range index panics in `apply`). Kept `unsafe` because
    /// under the native ABI `code` becomes a real code address `apply` calls directly — a bogus one is
    /// then UB — and because the caller must still initialise a well-formed closure (valid `env`).
    pub unsafe fn new_closure_raw(&mut self, code: u64, arity: u32, env: TaggedWord) -> HeapPtr {
        let p = self.alloc(Kind::Closure, 3, Color::White);
        self.write_raw_unchecked(p, 0, code);
        self.write_raw_unchecked(p, 1, arity as u64);
        self.write_field_unchecked(p, 2, env);
        p
    }

    /// A `PAP` `[function: value][remaining_arity: raw][captured: value; k]` — an under-applied
    /// callable (ADR-0064 §2/§3). `function` is the underlying `Closure`; `captured` are the args
    /// supplied so far; `remaining` is how many more the closure's arity still needs.
    pub fn new_pap(
        &mut self,
        function: TaggedWord,
        remaining: u32,
        captured: &[TaggedWord],
    ) -> HeapPtr {
        let p = self.alloc(Kind::Pap, 2 + captured.len() as u64, Color::White);
        self.write_field_unchecked(p, 0, function);
        self.write_raw_unchecked(p, 1, remaining as u64);
        for (i, c) in captured.iter().enumerate() {
            self.write_field_unchecked(p, 2 + i as u64, *c);
        }
        p
    }

    // --- field access -------------------------------------------------------
    //
    // Two tiers. The **checked** public accessors (`header` / `read_field` / `write_field` /
    // `read_raw` / `write_raw`) validate that `p` is a live object *header* in the active space
    // (`checked_object`) before any raw read/write, so a safe caller that retained a [`HeapPtr`]
    // across a `collect` — now stale, or aliasing the *interior* of a newer object — is rejected
    // instead of reading a payload word as a header ([P1]: the object-boundary hole). The
    // **unchecked** `*_unchecked` tier (`pub(crate)`) trusts `p` and is the hot path: `new_*` (a
    // freshly allocated object) and `apply` (a callee already validated by `checked_ptr`) use it,
    // since an object-start walk on every field would make construction and `apply` O(n²). Codegen
    // will likewise call the unchecked tier, establishing liveness via shadow-stack roots instead.

    /// The header of `p` (checked — see the tier note above).
    #[inline]
    pub fn header(&self, p: HeapPtr) -> Header {
        self.header_unchecked(self.checked_object(p))
    }

    /// Read payload word `i` as a value slot (a [`TaggedWord`]) — checked.
    #[inline]
    pub fn read_field(&self, p: HeapPtr, i: u64) -> TaggedWord {
        self.read_field_unchecked(self.checked_object(p), i)
    }

    /// Write a [`TaggedWord`] into value-slot `i` — checked.
    #[inline]
    pub fn write_field(&mut self, p: HeapPtr, i: u64, w: TaggedWord) {
        self.write_field_unchecked(self.checked_object(p), i, w);
    }

    /// Read raw payload word `i` (a code pointer, an `f64` bit pattern, a ctor tag — never a value) —
    /// checked.
    #[inline]
    pub fn read_raw(&self, p: HeapPtr, i: u64) -> u64 {
        self.read_raw_unchecked(self.checked_object(p), i)
    }

    /// Write a raw payload word `i` — checked.
    #[inline]
    pub fn write_raw(&mut self, p: HeapPtr, i: u64, bits: u64) {
        self.write_raw_unchecked(self.checked_object(p), i, bits);
    }

    /// The header of `p`, **without** the object-start check. `pub(crate)`: the caller guarantees `p`
    /// is a live object header (freshly allocated in `new_*`, or already validated by `checked_ptr`).
    #[inline]
    pub(crate) fn header_unchecked(&self, p: HeapPtr) -> Header {
        // SAFETY: `p` points to a live header word in this heap (caller's contract).
        Header::from_bits(unsafe { *(p.as_ptr() as *const u64) })
    }

    /// Unchecked [`read_field`](Self::read_field) — see [`header_unchecked`](Self::header_unchecked).
    #[inline]
    pub(crate) fn read_field_unchecked(&self, p: HeapPtr, i: u64) -> TaggedWord {
        TaggedWord::from_bits(self.read_word(p, i))
    }

    /// Unchecked [`write_field`](Self::write_field).
    #[inline]
    pub(crate) fn write_field_unchecked(&mut self, p: HeapPtr, i: u64, w: TaggedWord) {
        self.write_word(p, i, w.to_bits());
    }

    /// Unchecked [`read_raw`](Self::read_raw).
    #[inline]
    pub(crate) fn read_raw_unchecked(&self, p: HeapPtr, i: u64) -> u64 {
        self.read_word(p, i)
    }

    /// Unchecked [`write_raw`](Self::write_raw).
    #[inline]
    pub(crate) fn write_raw_unchecked(&mut self, p: HeapPtr, i: u64, bits: u64) {
        self.write_word(p, i, bits);
    }

    #[inline]
    fn read_word(&self, p: HeapPtr, i: u64) -> u64 {
        // Release-on: a bad index behind this API is an out-of-bounds raw read/write.
        assert!(
            i < self.header_unchecked(p).size_words(),
            "field index out of range"
        );
        // SAFETY: `i < size` (checked above); the payload word is within the live object.
        unsafe { *(p.as_ptr() as *const u64).add(1 + i as usize) }
    }

    #[inline]
    fn write_word(&mut self, p: HeapPtr, i: u64, bits: u64) {
        // Release-on: a bad index behind this API is an out-of-bounds raw read/write.
        assert!(
            i < self.header_unchecked(p).size_words(),
            "field index out of range"
        );
        // SAFETY: `i < size` (checked above); the payload word is within the live object.
        unsafe { *(p.as_ptr() as *mut u64).add(1 + i as usize) = bits };
    }

    // --- pointer validation -------------------------------------------------

    /// Whether `p` addresses the **header word of a live object** in the active space — not merely an
    /// in-bounds word. The active space is a gap-free run of `[Header][payload; size]` records from
    /// offset 0 to `top`; this walks that run and returns `true` iff `p` is exactly one of the record
    /// starts. An *interior* payload word — which a stale pointer can come to alias after a couple of
    /// collect/swap cycles — is therefore rejected ([P1]): without this, a safe caller could read a
    /// payload word as a header (or, in `apply`, transmute-and-call a payload-derived word) —
    /// reachable UB that the earlier in-bounds-only check let through. Aliasing another *header*
    /// start remains an accepted logic bug (a real live object, memory-safe). O(objects): a v1
    /// backstop for the hand-written safe API; codegen proves liveness via shadow-stack roots instead
    /// (ADR-0064 §4).
    #[inline]
    fn is_object_start(&self, p: HeapPtr) -> bool {
        let base = self.space.as_ptr() as usize;
        let addr = p.as_ptr() as usize;
        if addr < base || addr >= base + self.top * WORD || !(addr - base).is_multiple_of(WORD) {
            return false;
        }
        let target = (addr - base) / WORD;
        // Each record start carries a valid live header (written by `alloc`, never forwarded in the
        // active space), so striding by `1 + size` visits exactly the record starts in `[0, top)`.
        let mut off = 0usize;
        while off < target {
            // SAFETY: `off < target < top <= cap`; read the header through the stable space base.
            let hdr = Header::from_bits(unsafe { *self.space.as_ptr().add(off) });
            off += 1 + hdr.size_words() as usize;
        }
        off == target
    }

    /// Release-check that a [`HeapPtr`] reaching the safe API is a live object *header* in this heap,
    /// and return it. Rejects stale (post-`collect`), forged, foreign, and interior-aliasing pointers
    /// via [`is_object_start`](Self::is_object_start) — the object-boundary check the safe field API
    /// and `apply` rely on to avoid reading a non-object as a header (UB).
    #[inline]
    fn checked_object(&self, p: HeapPtr) -> HeapPtr {
        assert!(
            self.is_object_start(p),
            "pointer is not a live object in this heap (stale after collect, forged, or interior alias)"
        );
        // A live active-space object is never forwarded (forwarded headers live only in the idle
        // reserve after a collect); `is_object_start` already implies this, so a debug backstop.
        debug_assert!(
            !self.header_unchecked(p).is_forwarded(),
            "active-space object unexpectedly forwarded"
        );
        p
    }

    /// Release-checked decode of a callee / root pointer *word* to a live object in this heap.
    /// Rejects immediates, then defers to [`checked_object`](Self::checked_object) for the
    /// object-boundary check. This is what keeps the *safe* API from reaching a header deref (or, in
    /// `apply`, a transmute-and-call of a code word) on a non-object — UB. A backstop for the
    /// hand-written safe API; codegen instead enforces liveness via shadow-stack roots.
    #[inline]
    pub(crate) fn checked_ptr(&self, w: TaggedWord) -> HeapPtr {
        assert!(w.is_pointer(), "expected a pointer value, got an immediate");
        // SAFETY: `is_pointer` holds; `from_word` only builds the handle (no deref yet).
        let p = unsafe { HeapPtr::from_word(w) };
        self.checked_object(p)
    }

    // --- collection (Cheney) ------------------------------------------------

    /// Collect the local heap, keeping only what is reachable from `roots`. The reachable graph is
    /// copied into the reserve space; `roots` are **rewritten in place** to the survivors' new
    /// addresses (any [`HeapPtr`] / pointer `TaggedWord` held elsewhere across a `collect` is stale —
    /// this models the shadow-stack root-reload contract, ADR-0064 §4). Sharing and cycles are
    /// preserved by forwarding.
    pub fn collect(&mut self, roots: &mut [TaggedWord]) {
        // The from-space object-start oracle, built once (O(top)) before any forwarding rewrites a
        // header. Both the root check and `evacuate` consult it so that *every* pointer the collector
        // is about to read as a header — an externally supplied root **and** a value slot GC traces
        // internally — is proven to address a live object start first. Without the value-slot arm, a
        // stale/interior pointer stored into a live slot via the safe API would be read as a header
        // and copied as a forged object ([P1]).
        let starts = FromStarts::build(self.space, self.top);

        // Release-on: each pointer root must be a live object *header* in the active (soon-`from`)
        // space; a stale, foreign, or interior-aliasing root is rejected before any dereference.
        for r in roots.iter() {
            if r.is_pointer() {
                assert!(
                    starts.is_start(r.addr()),
                    "collect: a root is not a live object in this heap (stale, foreign, or interior alias)"
                );
            }
        }

        // Both semi-spaces are addressed through their stable raw bases (never `&mut [u64]`, which
        // would invalidate outstanding `HeapPtr`s — see the struct docs). `from` is scanned, `to`
        // receives survivors; they are disjoint allocations.
        let from = self.space;
        let to = self.reserve;
        let mut free = 0usize;

        for r in roots.iter_mut() {
            *r = evacuate(from, to, &mut free, &starts, *r);
        }

        // Cheney scavenge: walk the objects already in `to`, evacuating their value slots. New
        // survivors are appended past `free`, so the loop terminates when `scan` catches up.
        let mut scan = 0usize;
        while scan < free {
            // SAFETY: `scan < free <= cap`; read the header just written into `to`.
            let hdr = Header::from_bits(unsafe { *to.as_ptr().add(scan) });
            let size = hdr.size_words() as usize;
            let [(a0, a1), (b0, b1)] = value_slot_ranges(hdr.kind(), hdr.size_words());
            for i in (a0..a1).chain(b0..b1) {
                let idx = scan + 1 + i as usize;
                // SAFETY: `idx` is a value slot of the object at `scan`, within `to`'s allocation.
                let w = TaggedWord::from_bits(unsafe { *to.as_ptr().add(idx) });
                let moved = evacuate(from, to, &mut free, &starts, w);
                unsafe { *to.as_ptr().add(idx) = moved.to_bits() };
            }
            scan += 1 + size;
        }

        self.top = free;
        // Swap the two stable base pointers (a `Copy` of the raw handles — no buffer re-borrow).
        core::mem::swap(&mut self.space, &mut self.reserve);
    }
}

/// Allocate one zeroed semi-space of `words` words as a raw, single-provenance region. Ownership is
/// held raw (freed by [`Heap`]'s `Drop`), so the buffer is never re-borrowed as `&mut` after this.
fn alloc_space(words: usize) -> NonNull<u64> {
    let boxed: Box<[u64]> = vec![0u64; words].into_boxed_slice();
    // `Box::into_raw` yields a live, non-null `*mut [u64]`; take its element base.
    let raw = Box::into_raw(boxed) as *mut u64;
    // SAFETY: `Box::into_raw` never returns null.
    unsafe { NonNull::new_unchecked(raw) }
}

impl Drop for Heap {
    fn drop(&mut self) {
        // SAFETY: `space`/`reserve` each came from `Box::into_raw` of a `[u64; cap]` boxed slice;
        // rebuild the exact-length boxed slice to run its deallocation. Done once, on drop.
        unsafe {
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.space.as_ptr(),
                self.cap,
            )));
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.reserve.as_ptr(),
                self.cap,
            )));
        }
    }
}

/// A from-space object-start oracle for one collection: which word offsets in the from-space hold an
/// object header. Built once per `collect` (O(top)) so [`evacuate`] can O(1)-reject a value-slot /
/// root pointer that aliases a from-space *interior* word, or lies outside the from-space, before it
/// is read as a header ([P1]). Built *before* any forwarding, so a later-forwarded object is still
/// recorded as a start (its offset does not move; only its header bit / payload word 0 change).
struct FromStarts {
    /// Base address of the from-space buffer.
    base: usize,
    /// One past the last in-use from-space word (`base + top * WORD`).
    end: usize,
    /// `starts[i]` is `true` iff a from-space object header begins at word `i` (`i < top`).
    starts: Vec<bool>,
}

impl FromStarts {
    fn build(space: NonNull<u64>, top: usize) -> FromStarts {
        let base = space.as_ptr() as usize;
        let mut starts = vec![false; top];
        // Stride the gap-free `[Header][payload; size]` records; each header is pristine here.
        let mut off = 0usize;
        while off < top {
            starts[off] = true;
            // SAFETY: `off < top <= cap`; read the header through the stable from-space base.
            let hdr = Header::from_bits(unsafe { *space.as_ptr().add(off) });
            off += 1 + hdr.size_words() as usize;
        }
        FromStarts {
            base,
            end: base + top * WORD,
            starts,
        }
    }

    /// Whether `addr` is the header start of a live from-space object.
    #[inline]
    fn is_start(&self, addr: usize) -> bool {
        addr >= self.base
            && addr < self.end
            && (addr - self.base).is_multiple_of(WORD)
            && self.starts[(addr - self.base) / WORD]
    }
}

/// Evacuate a word: an immediate is returned unchanged; a pointer to a from-space object is copied
/// to `to` (once — a forwarded object returns its recorded new address) and the updated pointer is
/// returned. `starts` gates the pointer path: only a genuine from-space object start is followed.
/// `from` / `to` are the disjoint semi-space bases, addressed by raw offset (never `&mut [u64]`, to
/// keep outstanding `HeapPtr`s valid — see the [`Heap`] struct docs).
fn evacuate(
    from: NonNull<u64>,
    to: NonNull<u64>,
    free: &mut usize,
    starts: &FromStarts,
    w: TaggedWord,
) -> TaggedWord {
    if w.is_immediate() {
        return w;
    }
    // Release-on: the pointer must address a from-space object *header* start. A stale pointer stored
    // into a live value slot via the safe API (interior alias, or pointing outside the from-space) —
    // or a foreign/shared pointer (v2 S1 arena, whose evacuate-iff-local dispatch lands then) — is
    // rejected here, before it is read as a header and copied as a forged object ([P1]).
    assert!(
        starts.is_start(w.addr()),
        "evacuate: a traced pointer is not a live object in the from-space (stale, foreign, or interior alias)"
    );
    let obj = (w.addr() - starts.base) / WORD; // header word index in `from`
                                               // SAFETY: `starts.is_start` proved `obj` is a live object header within the from-space.
    let hdr = Header::from_bits(unsafe { *from.as_ptr().add(obj) });
    if hdr.is_forwarded() {
        // The forwarding address is a raw byte address in the first payload word.
        // SAFETY: a forwarded object still has its payload word 0 in bounds (`size >= 1`).
        return TaggedWord::from_addr(unsafe { *from.as_ptr().add(obj + 1) } as usize);
    }
    let words = 1 + hdr.size_words() as usize;
    let dst = *free;
    // Copy the whole object into `to` at `dst`. SAFETY: `from`/`to` are disjoint allocations; the
    // source `[obj, obj+words)` is in bounds (live object) and the destination `[dst, dst+words)` is
    // in bounds (bump reserve of the same capacity, never overfilled since survivors ⊆ from-space).
    unsafe {
        core::ptr::copy_nonoverlapping(from.as_ptr().add(obj), to.as_ptr().add(dst), words);
    }
    let new_addr = to.as_ptr() as usize + dst * WORD;
    // Install forwarding in the from-space object (after the copy, so no live data is lost).
    // SAFETY: `obj` and `obj + 1` are in bounds (`size >= 1`, header contract).
    unsafe {
        *from.as_ptr().add(obj) = hdr.with_forwarded().to_bits();
        *from.as_ptr().add(obj + 1) = new_addr as u64;
    }
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
    #[should_panic(expected = "not a live object")]
    fn collect_rejects_stale_root() {
        // A root left over from a *prior* collect addresses the now-idle reserve; feeding it back in
        // must be rejected before `evacuate` reads a bogus header from it.
        let mut h = Heap::new(64);
        let n = h.new_number(1.0);
        let stale = n.as_word();
        let mut live = [stale];
        h.collect(&mut live); // relocates `n`; `live[0]` is rewritten, `stale` keeps the old address
        let mut roots = [stale];
        h.collect(&mut roots);
    }

    #[test]
    #[should_panic(expected = "empty array")]
    fn new_array_rejects_empty() {
        // The empty-array singleton is deferred (ADR-0064 §5); a zero-length array is rejected near
        // the API, not deep in the `size >= 1` header invariant.
        let mut h = Heap::new(16);
        let _ = h.new_array(&[]);
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
        // SAFETY: this test only traces layout; the raw code word is never called.
        let clo = unsafe { h.new_closure_raw(0xC0DE, 1, env.as_word()) };

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
        // SAFETY: this test only traces layout; the raw code word is never called.
        let clo = unsafe { h.new_closure_raw(0xF00D, 0, TaggedWord::unit()) };
        let mut roots = [clo.as_word()];
        h.collect(&mut roots);

        let clo2 = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.read_raw(clo2, 0), 0xF00D);
        assert!(h.read_field(clo2, 2).is_immediate()); // env sentinel scanned but passed through
        assert_eq!(h.used(), 1 + 3); // just the closure — no env object
    }

    #[test]
    fn is_object_start_accepts_headers_rejects_interior() {
        // The [P1] object-boundary check: only header starts are accepted; in-bounds *interior*
        // payload words (the alias a stale pointer can acquire after collect cycles) are not.
        let mut h = Heap::new(64);
        let a = h.new_number(1.0); //           words [hdr@0, payload@1]
        let b = h.new_adt(3, &[TaggedWord::int(9)]); // words [hdr@2, tag@3, field@4]
        assert!(h.is_object_start(a));
        assert!(h.is_object_start(b));

        // Interior words: in-bounds and word-aligned, but not object starts.
        let a_interior =
            unsafe { HeapPtr::from_word(TaggedWord::from_addr(a.as_ptr() as usize + WORD)) };
        let b_interior =
            unsafe { HeapPtr::from_word(TaggedWord::from_addr(b.as_ptr() as usize + WORD)) };
        assert!(!h.is_object_start(a_interior));
        assert!(!h.is_object_start(b_interior));

        // Past the bump cursor is out of the active region.
        let past =
            unsafe { HeapPtr::from_word(TaggedWord::from_addr(a.as_ptr() as usize + 32 * WORD)) };
        assert!(!h.is_object_start(past));
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn read_field_rejects_interior_pointer() {
        // The safe field API must reject an interior (non-header) pointer rather than read a payload
        // word as a header — the [P1] hole that reached UB in `apply`.
        let mut h = Heap::new(64);
        let adt = h.new_adt(1, &[TaggedWord::int(7), TaggedWord::int(8)]);
        let interior =
            unsafe { HeapPtr::from_word(TaggedWord::from_addr(adt.as_ptr() as usize + WORD)) };
        let _ = h.read_field(interior, 0);
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn read_field_rejects_stale_pointer_that_became_interior() {
        // The realistic [P1] scenario end-to-end: a HeapPtr obtained safely and retained across two
        // collect/swap cycles, whose (unchanged) address comes to alias the *interior* of a newer
        // live object in the active space. Reachable via the safe API; must be rejected.
        let mut h = Heap::new(16);
        let p = h.new_number(1.0); // active buffer S: header @ word 0
        let q = h.new_number(2.0); // active buffer S: header @ word 2
        let stale = q.as_word(); //   addr = S + 2 words

        // Drop q, keep p. After the swap the active buffer is the other space; `stale` addresses the
        // now-idle S.
        let mut roots = [p.as_word()];
        h.collect(&mut roots);
        let p1 = roots[0];

        // In the new active space, build a size-3 ADT that (when copied back into S) lands at word 0
        // and spans words 0..=3 — so S + 2 words becomes its interior.
        let r = h.new_adt(7, &[TaggedWord::int(1), TaggedWord::int(2)]);

        // Second collect evacuates R (first root → word 0) then P back into S.
        let mut roots2 = [r.as_word(), p1];
        h.collect(&mut roots2);

        // `stale` (still S + 2 words) is now interior to the live R; the safe API must reject it.
        let ghost = unsafe { HeapPtr::from_word(stale) };
        let _ = h.read_field(ghost, 0);
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn collect_rejects_interior_pointer_in_value_slot() {
        // [P1] value-slot boundary: a pointer that aliases a from-space *interior* word, stored into
        // a live object's value slot via the safe API, must be rejected when GC *traces* that slot —
        // not read as a header and copied as a forged object.
        let mut h = Heap::new(64);
        let obj = h.new_adt(9, &[TaggedWord::int(1), TaggedWord::unit()]); // [tag@0, f1@1, f2@2]
                                                                           // Forge a pointer to obj's own tag word (payload index 0): in-bounds and aligned, but not an
                                                                           // object start — exactly the shape a stale pointer takes after collect cycles.
        let interior = TaggedWord::from_addr(obj.as_ptr() as usize + WORD);
        h.write_field(obj, 2, interior); // container `obj` is valid, so the store is accepted

        let mut roots = [obj.as_word()];
        h.collect(&mut roots); // scavenge reaches value slot 2 → evacuate(interior) → reject
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn collect_rejects_stale_pointer_in_value_slot() {
        // The release-mode gap the old debug-only bound check left open: a pointer dropped by an
        // earlier collect (now addressing the idle reserve, outside the from-space) stored into a
        // live slot must also be rejected when traced.
        let mut h = Heap::new(64);
        let doomed = h.new_number(7.0);
        let stale = doomed.as_word();
        let keep = h.new_adt(1, &[TaggedWord::unit()]); // [tag@0, f1@1]

        // Drop `doomed`, keep `keep`; after the swap `stale` addresses the now-idle reserve.
        let mut roots = [keep.as_word()];
        h.collect(&mut roots);
        let keep2 = unsafe { HeapPtr::from_word(roots[0]) };

        // Store the stale pointer into keep's value slot, then collect: tracing it must reject.
        h.write_field(keep2, 1, stale);
        let mut roots2 = [keep2.as_word()];
        h.collect(&mut roots2);
    }
}
