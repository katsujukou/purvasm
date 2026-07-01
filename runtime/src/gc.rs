//! The v1 local heap: a semi-space (Cheney) copying collector (ADR-0064 §5).
//!
//! This is the **unsafe GC island** (ADR-0063): raw memory, tagged words, object relocation. It
//! exposes a small typed API (the self-rooting `new_*` constructors, `read_field`/`write_field`, the
//! shadow-stack root API) and never hands out a Rust reference into the moving heap. `alloc` is
//! `pub(crate)` (a safepoint); the public surface fully initialises objects via `new_*`.
//!
//! Layout matches [`crate::heap`]: an object is `[Header][payload word; size]`, a [`HeapPtr`] points
//! at the header. Collection is **layout-directed** (ADR-0064 §2): for each live object the header
//! `kind` says which payload words are *value slots* (a [`TaggedWord`] — scanned/relocated) and
//! which are *raw* (skipped). Forwarding stores the new address in the object's first payload word,
//! after a full copy (the [`Header`] contract — every object has `size >= 1`).
//!
//! Roots come from a `Heap`-owned **shadow stack** (ADR-0066): [`Heap::root`]/[`Heap::get`] with
//! frame marks, fed to the collector by [`Heap::alloc`] on overflow (GC-on-alloc). The value-storing
//! constructors self-root their inputs across the allocation.
//!
//! Scope (ADR-0064 §0): single-capability, sequential; the shared arena, promotion, generational
//! collection, `gc.statepoint`, and codegen-emitted stack maps are v2 / later.

use crate::apply::CodeFn;
use crate::heap::{Color, Header, HeapPtr, Kind};
use crate::word::TaggedWord;
use core::ptr::NonNull;

/// Bytes per heap word.
const WORD: usize = core::mem::size_of::<u64>();

/// `ByNeed` cell `state` word (ADR-0070 §1). `pub(crate)`: shared with the `force` operation.
pub(crate) const BYNEED_UNFORCED: u64 = 0;
pub(crate) const BYNEED_BUILDING: u64 = 1;
pub(crate) const BYNEED_FORCED: u64 = 2;

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
    /// The **shadow stack** (ADR-0066 §1): the authoritative root set for a collection. Each entry is
    /// a value slot the collector rewrites in place on relocation; a [`Root`] indexes it. `alloc`
    /// feeds this to the collector on overflow, so a value rooted here survives (and is reloaded via
    /// [`Heap::get`]). A `Value`/`TaggedWord`, never a Rust reference into the heap (ADR-0063 §2).
    roots: Vec<TaggedWord>,
    /// Debug-only per-slot generation stamps (ADR-0066 §2): let [`Heap::get`] catch a stale `Root`
    /// whose slot index was popped and reused (an ABA the bare bounds check misses). No release cost.
    #[cfg(debug_assertions)]
    root_gens: Vec<u64>,
    /// Debug-only monotonic generation source for [`root_gens`](Heap::root_gens).
    #[cfg(debug_assertions)]
    next_gen: u64,
    /// v1 bring-up code registry: real [`CodeFn`] pointers (with provenance) keyed by the index a
    /// closure stores in its `code` word. `apply` calls by index — no integer→fn transmute, which
    /// would be a provenance-free (UB) call. The native ABI stores a real code address here instead
    /// (ADR-0064 §3 Correction). Never relocated by GC.
    code_table: Vec<CodeFn>,
    /// The **output sink** for the native stdio write-line leaf (ADR-0067 §5): captured lines, one per
    /// write. A *labelled layering bleed* — safe-shell IO state co-located on the `Heap` in v1 because
    /// the `Heap` already is the universal `CodeFn` runtime context; it moves to a dedicated driver
    /// with the scheduler (the ADR-0063 safe shell). Production drains it to real `stdout`; tests read
    /// [`output`](Heap::output). (`Effect.Console.log` is a `ulib` shadow over the leaf, not a runtime
    /// concern.)
    output: Vec<String>,
}

/// A handle to a value rooted on the shadow stack (ADR-0066 §2). `Copy`, borrows nothing, and is only
/// a slot index (plus a debug generation stamp) — so it survives shadow-stack growth and never locks
/// the [`Heap`]. Valid only within its [`RootFrame`]; the API is `pub(crate)`, a trusted discipline.
#[derive(Clone, Copy)]
pub(crate) struct Root {
    index: usize,
    /// Debug-only: the slot's generation at `root` time — checked by [`Heap::get`] to reject a stale
    /// handle whose index was popped and reused.
    #[cfg(debug_assertions)]
    generation: u64,
}

/// A shadow-stack frame mark (ADR-0066 §2): the stack length captured on frame open. `Copy`, borrows
/// nothing. [`Heap::pop_frame`] truncates back to it, releasing every root pushed since.
#[derive(Clone, Copy)]
pub(crate) struct RootFrame {
    len: usize,
}

/// A validated view of a `Record` (ADR-0069 §3): its field count and, when non-empty, the `RawIds`
/// and value-`Array` pointers. Valid only until the next allocation — the pointers are not rooted, so
/// a caller reads what it needs *before* allocating.
pub(crate) struct RecordView {
    pub count: usize,
    pub ids: Option<HeapPtr>,
    pub vals: Option<HeapPtr>,
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
            roots: Vec::new(),
            #[cfg(debug_assertions)]
            root_gens: Vec::new(),
            #[cfg(debug_assertions)]
            next_gen: 0,
            code_table: Vec::new(),
            output: Vec::new(),
        }
    }

    /// The captured stdio write-line output (ADR-0067 §5), in program order — the test/differential
    /// view of the output sink.
    #[inline]
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Append a line to the output sink (the stdio write-line leaf's write). `pub(crate)`.
    #[inline]
    pub(crate) fn push_output(&mut self, line: String) {
        self.output.push(line);
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

    // --- shadow-stack rooting (ADR-0066 §2) ---------------------------------
    //
    // A trusted `pub(crate)` discipline used by the value-storing constructors, `apply`, and (later)
    // codegen — all of which emit statically balanced, stack-ordered frames. The debug build stamps
    // each slot with a generation so a stale/reused-index `Root` is caught (§2 misuse boundary).

    /// Open a shadow-stack frame — capture the current stack length as a [`RootFrame`] mark.
    #[inline]
    pub(crate) fn frame(&self) -> RootFrame {
        RootFrame {
            len: self.roots.len(),
        }
    }

    /// Push `v` onto the shadow stack; return a [`Root`] handle to its slot. The collector rewrites
    /// the slot on relocation, so [`get`](Heap::get) reads the current (post-collection) value.
    #[inline]
    pub(crate) fn root(&mut self, v: TaggedWord) -> Root {
        let index = self.roots.len();
        self.roots.push(v);
        #[cfg(debug_assertions)]
        let generation = {
            let g = self.next_gen;
            self.next_gen += 1;
            self.root_gens.push(g);
            g
        };
        Root {
            index,
            #[cfg(debug_assertions)]
            generation,
        }
    }

    /// Read the **current** value of a root — the reload-after-safepoint step (ADR-0066 §2). In debug,
    /// asserts the slot has not been popped and reused (generation match); in release, a stale handle
    /// whose index is past the top panics on the bounds check (safe — never out-of-bounds UB).
    #[inline]
    pub(crate) fn get(&self, r: Root) -> TaggedWord {
        #[cfg(debug_assertions)]
        debug_assert_eq!(
            self.root_gens[r.index], r.generation,
            "stale Root: shadow-stack slot was popped and reused"
        );
        self.roots[r.index]
    }

    /// Close a frame: truncate the shadow stack back to the mark, releasing every root pushed since.
    #[inline]
    pub(crate) fn pop_frame(&mut self, f: RootFrame) {
        debug_assert!(
            f.len <= self.roots.len(),
            "pop_frame: mark exceeds shadow-stack top (double-pop / stale frame)"
        );
        self.roots.truncate(f.len);
        #[cfg(debug_assertions)]
        self.root_gens.truncate(f.len);
    }

    /// Read the raw shadow-stack slot at absolute index `i` (crate-internal; used by the self-rooting
    /// constructors that just pushed a contiguous run and know their base). No generation check —
    /// the caller pushed the slot within the current frame.
    #[inline]
    fn root_slot(&self, i: usize) -> TaggedWord {
        self.roots[i]
    }

    // --- allocation ---------------------------------------------------------

    /// Bump-allocate an object of `kind` with `size` payload words, returning a pointer to its
    /// header. The payload is **uninitialised**; the caller must write every field before the object
    /// becomes reachable / a collection scans it (v1: codegen writes all fields with no safepoint in
    /// between — ADR-0064 §4).
    ///
    /// **This is a safepoint** (ADR-0066 §4): on overflow it collects the live set reachable from the
    /// shadow stack and retries once. Any `Value` that must survive the call must therefore be rooted
    /// (ADR-0066 §3) — the safe `new_*` constructors do this for their inputs; `pub(crate)` for
    /// `apply`'s closure constructor. A still-full heap after collection is a fatal OOM.
    pub(crate) fn alloc(&mut self, kind: Kind, size: u64, colour: Color) -> HeapPtr {
        let words = 1 + size as usize;
        // A single object cannot exceed a semi-space (no large-object region in v1).
        assert!(
            words <= self.cap,
            "object of {words} words exceeds the semi-space capacity ({} words)",
            self.cap
        );
        // GC-on-alloc (ADR-0066 §4): collect on overflow (roots = the shadow stack), then retry once.
        if self.top + words > self.cap {
            self.gc();
            assert!(
                self.top + words <= self.cap,
                "heap OOM: live set + {words} words exceed the semi-space ({} words)",
                self.cap
            );
        }
        let base = self.top;
        self.top += words;
        // Derive the header pointer from the stable base (never a fresh buffer re-borrow — see the
        // struct docs). SAFETY: `base < cap` (checked), so the word is within the space allocation.
        let hdr = unsafe { self.space.as_ptr().add(base) };
        unsafe { *hdr = Header::new(kind, size, colour).to_bits() };
        // SAFETY: `hdr` is a live element of the space allocation, hence non-null; it is the header.
        unsafe { HeapPtr::from_raw(NonNull::new_unchecked(hdr as *mut Header)) }
    }

    /// An `Adt` object: payload `[tag: raw] ++ fields`. Self-rooting (ADR-0066 §3): the incoming
    /// field `Value`s are rooted across `alloc` (a safepoint) and re-read from their slots when
    /// written, so a collection during construction cannot leave a stale pointer in the object.
    pub fn new_adt(&mut self, tag: u32, fields: &[TaggedWord]) -> HeapPtr {
        let frame = self.frame();
        let base = frame.len;
        for f in fields {
            self.root(*f);
        }
        let p = self.alloc(Kind::Adt, 1 + fields.len() as u64, Color::White);
        self.write_raw_unchecked(p, 0, tag as u64);
        for i in 0..fields.len() {
            let v = self.root_slot(base + i);
            self.write_field_unchecked(p, 1 + i as u64, v);
        }
        self.pop_frame(frame);
        p
    }

    /// A boxed `Number` (`f64`). No `Value` input, so nothing to root across `alloc`.
    pub fn new_number(&mut self, x: f64) -> HeapPtr {
        let p = self.alloc(Kind::NumberBox, 1, Color::White);
        self.write_raw_unchecked(p, 0, x.to_bits());
        p
    }

    /// A `Ref` cell holding `init`. Self-rooting (ADR-0066 §3).
    pub fn new_ref(&mut self, init: TaggedWord) -> HeapPtr {
        let frame = self.frame();
        let base = frame.len;
        self.root(init);
        let p = self.alloc(Kind::Ref, 1, Color::White);
        let v = self.root_slot(base);
        self.write_field_unchecked(p, 0, v);
        self.pop_frame(frame);
        p
    }

    /// An `Array` (value-slot array) of `elems`. **Non-empty**: the empty-array singleton is deferred
    /// (ADR-0064 §5), so a zero-length array would otherwise trip the `size >= 1` header invariant.
    /// Self-rooting (ADR-0066 §3).
    pub fn new_array(&mut self, elems: &[TaggedWord]) -> HeapPtr {
        assert!(
            !elems.is_empty(),
            "empty array is deferred (needs the immortal singleton, ADR-0064 §5)"
        );
        let frame = self.frame();
        let base = frame.len;
        for e in elems {
            self.root(*e);
        }
        let p = self.alloc(Kind::Array, elems.len() as u64, Color::White);
        for i in 0..elems.len() {
            let v = self.root_slot(base + i);
            self.write_field_unchecked(p, i as u64, v);
        }
        self.pop_frame(frame);
        p
    }

    /// A `Closure` from a **raw** code word `[code: raw][arity: raw][env: value slot]` (ADR-0059 §1).
    /// Safe Rust callers should use the typed [`Heap::new_closure`], which interns a [`crate::CodeFn`]
    /// and passes its index here.
    ///
    /// In v1 `code` is an index into the heap's code table (via [`Heap::intern_code`]); the native
    /// ABI stores a real code address instead (ADR-0064 §3 Correction). `apply` looks the index up
    /// with a bounds check, so a bad `code` word panics rather than reaching UB.
    ///
    /// `env` **is self-rooted** across the allocation (ADR-0066 §3), exactly like the other value-slot
    /// constructors — `alloc` is a safepoint, so a heap-pointer `env` would otherwise be left dangling
    /// by a mid-call collection. This is *not* a caller precondition.
    ///
    /// # Safety
    /// Only the `code` word is unchecked. In v1 it must be a valid code-table index (an out-of-range
    /// index merely panics in `apply`, so this is currently sound for any value); the method stays
    /// `unsafe` because under the native ABI `code` becomes a real code address `apply` calls
    /// directly, where a bogus word is UB.
    pub unsafe fn new_closure_raw(&mut self, code: u64, arity: u32, env: TaggedWord) -> HeapPtr {
        // Self-rooting (ADR-0066 §3): `env` may be a heap pointer stored after `alloc` (a safepoint).
        let frame = self.frame();
        let env_root = self.root(env);
        let p = self.alloc(Kind::Closure, 3, Color::White);
        self.write_raw_unchecked(p, 0, code);
        self.write_raw_unchecked(p, 1, arity as u64);
        let env = self.get(env_root);
        self.write_field_unchecked(p, 2, env);
        self.pop_frame(frame);
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
        // Self-rooting (ADR-0066 §3): `function` then the `captured` values, in that slot order.
        let frame = self.frame();
        let base = frame.len;
        self.root(function);
        for c in captured {
            self.root(*c);
        }
        let p = self.alloc(Kind::Pap, 2 + captured.len() as u64, Color::White);
        let f = self.root_slot(base);
        self.write_field_unchecked(p, 0, f);
        self.write_raw_unchecked(p, 1, remaining as u64);
        for i in 0..captured.len() {
            let v = self.root_slot(base + 1 + i);
            self.write_field_unchecked(p, 2 + i as u64, v);
        }
        self.pop_frame(frame);
        p
    }

    /// A `Str` from UTF-8 `bytes` (ADR-0067 §5). Layout: `[len: raw][utf8 bytes: raw…]` — `len` is the
    /// byte length (payload word 0); the bytes occupy the following words **in ascending memory order**
    /// (byte `i` at payload byte offset `i`), the trailing partial word **zero-filled**, so
    /// `size_words = 1 + ceil(len / 8)`. The **empty string** is valid (`len = 0`, `size_words = 1`).
    /// `new_str` **owns the UTF-8 invariant** — it asserts validity so every reader may assume it. No
    /// `Value` input, so nothing to root across the allocation.
    pub fn new_str(&mut self, bytes: &[u8]) -> HeapPtr {
        assert!(
            core::str::from_utf8(bytes).is_ok(),
            "new_str: bytes are not valid UTF-8 (the Str invariant, ADR-0067 §5)"
        );
        let len = bytes.len();
        let byte_words = len.div_ceil(WORD); // ceil(len / 8); 0 for the empty string
        let p = self.alloc(Kind::Str, 1 + byte_words as u64, Color::White);
        self.write_raw_unchecked(p, 0, len as u64);
        // Zero the byte words first (so the trailing partial word's padding is zero), then copy the
        // bytes over the front in memory order.
        for i in 0..byte_words {
            self.write_raw_unchecked(p, 1 + i as u64, 0);
        }
        // SAFETY: the byte region is payload word 1.. — `WORD` (header) + `WORD` (len) bytes into the
        // object; `byte_words` words (>= ceil(len/8)) were just allocated and zeroed, covering `len`
        // bytes. `bytes` is a disjoint host slice.
        unsafe {
            let dst = (p.as_ptr() as *mut u8).add(2 * WORD);
            core::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, len);
        }
        p
    }

    /// The byte length of a `Str`. **Release-checked** (this is reachable from the safe public API, so
    /// the checks are not `debug`-only): asserts the object is a `Str` and that its stored `len` fits
    /// the allocated byte words, so a non-`Str` object or a `len` word corrupted via the public
    /// `write_raw` cannot drive [`str_read`](Self::str_read) into an out-of-bounds slice (UB).
    #[inline]
    pub fn str_len(&self, s: HeapPtr) -> usize {
        let hdr = self.header(s); // checked: validates `s` is a live object header
        assert_eq!(hdr.kind(), Kind::Str, "str_len on a non-Str object");
        let len = self.read_raw(s, 0) as usize;
        // The bytes occupy `size_words - 1` payload words after the `len` word; `len` must fit them.
        let byte_capacity = (hdr.size_words() as usize - 1) * WORD;
        assert!(
            len <= byte_capacity,
            "Str length {len} exceeds its allocated byte capacity {byte_capacity} (corrupt Str)"
        );
        len
    }

    /// Copy a `Str`'s bytes out to an owned `String`. Copies (never borrows into the moving heap,
    /// ADR-0063 §2); valid UTF-8 by the `new_str` invariant.
    pub fn str_read(&self, s: HeapPtr) -> String {
        // Release-checked: `str_len` asserts `s` is a `Str` and `len <= byte_capacity`, so the slice
        // below is within the object's allocation.
        let len = self.str_len(s);
        // SAFETY: `len` bytes live at payload word 1 (`2 * WORD` bytes into the object) and, by the
        // `str_len` bound, stay within the object's allocated byte words; `s` is a validated header.
        let bytes =
            unsafe { core::slice::from_raw_parts((s.as_ptr() as *const u8).add(2 * WORD), len) };
        String::from_utf8(bytes.to_vec()).expect("Str invariant: valid UTF-8")
    }

    // --- records (ADR-0069) -------------------------------------------------

    /// A `RawIds` object of the sorted label `ids` — `[count][id; count]` (ADR-0069 §1). Non-empty
    /// here (`count >= 1`; the empty record stores no `RawIds`). No `Value` input (ids are raw
    /// `u64`s), so nothing to root across the allocation.
    fn new_rawids(&mut self, ids: &[u64]) -> HeapPtr {
        let p = self.alloc(Kind::RawIds, 1 + ids.len() as u64, Color::White);
        self.write_raw_unchecked(p, 0, ids.len() as u64);
        for (i, id) in ids.iter().enumerate() {
            self.write_raw_unchecked(p, 1 + i as u64, *id);
        }
        p
    }

    /// The `i`-th label id of a **validated** `RawIds` (`i < count`). `pub(crate)`; the caller
    /// validated the `RawIds` (e.g. via [`checked_record`](Heap::checked_record)).
    #[inline]
    pub(crate) fn rawids_id(&self, p: HeapPtr, i: usize) -> u64 {
        self.read_raw_unchecked(p, 1 + i as u64)
    }

    /// A `Record` from parallel sorted `ids` and `values` (ADR-0069 §1). `ids` must be **strictly
    /// ascending** and equal in length to `values`. The **empty** record stores the `unit` sentinel
    /// in both slots (no zero-length object). Self-rooting (ADR-0066 §3).
    pub fn new_record(&mut self, ids: &[u64], values: &[TaggedWord]) -> HeapPtr {
        assert_eq!(
            ids.len(),
            values.len(),
            "new_record: ids/values length mismatch"
        );
        assert!(
            ids.windows(2).all(|w| w[0] < w[1]),
            "new_record: ids must be strictly ascending"
        );
        if ids.is_empty() {
            let p = self.alloc(Kind::Record, 2, Color::White);
            self.write_field_unchecked(p, 0, TaggedWord::unit());
            self.write_field_unchecked(p, 1, TaggedWord::unit());
            return p;
        }
        let frame = self.frame();
        let varr = self.new_array(values); // self-roots `values` across its own allocation
        let vr = self.root(varr.as_word());
        let rawids = self.new_rawids(ids); // `varr` is rooted, so it survives a collection here
        let ir = self.root(rawids.as_word());
        let p = self.alloc(Kind::Record, 2, Color::White);
        let ids_w = self.get(ir);
        let vals_w = self.get(vr);
        self.write_field_unchecked(p, 0, ids_w);
        self.write_field_unchecked(p, 1, vals_w);
        self.pop_frame(frame);
        p
    }

    /// Release-validate a `Record`'s shape and return a [`RecordView`] (ADR-0069 §3, the `str_read`
    /// safety pattern). Rejects a non-`Record`, a mixed empty/non-empty slot pair, a wrong `RawIds`
    /// or value kind, a `count`/length mismatch, and non-strictly-ascending ids — so a record
    /// corrupted through the public `write_*` API faults here, never driving an out-of-bounds read.
    pub(crate) fn checked_record(&self, rec: TaggedWord) -> RecordView {
        let rp = self.checked_ptr(rec);
        assert_eq!(
            self.header_unchecked(rp).kind(),
            Kind::Record,
            "checked_record: not a Record"
        );
        let ids_slot = self.read_field_unchecked(rp, 0);
        let vals_slot = self.read_field_unchecked(rp, 1);
        if ids_slot.is_immediate() {
            // Empty record: both slots must be *exactly* the `unit` sentinel (ADR-0069 §1), not any
            // immediate — so a record corrupted via the public `write_field` (a non-`unit` immediate,
            // or a mixed sentinel/pointer pair) faults rather than being read as empty.
            assert!(
                ids_slot == TaggedWord::unit() && vals_slot == TaggedWord::unit(),
                "corrupt Record: empty-record slots must both be the unit sentinel"
            );
            return RecordView {
                count: 0,
                ids: None,
                vals: None,
            };
        }
        let ip = self.checked_ptr(ids_slot);
        assert_eq!(
            self.header_unchecked(ip).kind(),
            Kind::RawIds,
            "corrupt Record: label_ids is not RawIds"
        );
        let vp = self.checked_ptr(vals_slot);
        assert_eq!(
            self.header_unchecked(vp).kind(),
            Kind::Array,
            "corrupt Record: values is not an Array"
        );
        let count = self.read_raw_unchecked(ip, 0) as usize;
        assert_eq!(
            count,
            self.header_unchecked(ip).size_words() as usize - 1,
            "corrupt RawIds: count word disagrees with size"
        );
        assert_eq!(
            self.header_unchecked(vp).size_words() as usize,
            count,
            "corrupt Record: values length != id count"
        );
        assert!(
            count >= 1,
            "corrupt Record: non-sentinel record with count 0"
        );
        for i in 1..count {
            assert!(
                self.rawids_id(ip, i) > self.rawids_id(ip, i - 1),
                "corrupt Record: ids not strictly ascending"
            );
        }
        RecordView {
            count,
            ids: Some(ip),
            vals: Some(vp),
        }
    }

    // --- by-need cells (ADR-0070) -------------------------------------------

    /// A by-need cell `[state][result]` holding `suspension` (a nullary thunk closure) in the
    /// `Unforced` state (ADR-0070 §1). The **public** entry: the cell is born with its real
    /// suspension. Self-rooting (ADR-0066 §3).
    pub fn new_byneed(&mut self, suspension: TaggedWord) -> HeapPtr {
        let frame = self.frame();
        let sr = self.root(suspension);
        let p = self.alloc(Kind::ByNeed, 2, Color::White);
        self.write_raw_unchecked(p, 0, BYNEED_UNFORCED);
        let s = self.get(sr);
        self.write_field_unchecked(p, 1, s);
        self.pop_frame(frame);
        p
    }

    /// A **placeholder** by-need cell (`Unforced`, `result` = `unit`) for the `Grec` builder
    /// (ADR-0070 §4). Its real suspension — which must capture the group's `env`, which holds the
    /// cells — is backpatched by [`byneed_set_suspension`](Heap::byneed_set_suspension) once `env` is
    /// complete. `pub(crate)`: builder-internal, kept off the public API so `new_byneed`'s
    /// born-with-its-suspension invariant stays clean. No `Value` input.
    ///
    /// `#[allow(dead_code)]`: the `Grec` builder that calls this is emitted by codegen (not yet
    /// present); v1 exercises it only from the by-need recursive-group tests.
    #[allow(dead_code)]
    pub(crate) fn new_byneed_placeholder(&mut self) -> HeapPtr {
        let p = self.alloc(Kind::ByNeed, 2, Color::White);
        self.write_raw_unchecked(p, 0, BYNEED_UNFORCED);
        self.write_field_unchecked(p, 1, TaggedWord::unit());
        p
    }

    /// Backpatch a placeholder cell's suspension (`Grec` builder only; the cell must still be
    /// `Unforced`). A plain value-slot store — it does **not** force the suspension (ADR-0070 §4).
    /// `#[allow(dead_code)]` for the same reason as [`new_byneed_placeholder`](Heap::new_byneed_placeholder).
    #[allow(dead_code)]
    pub(crate) fn byneed_set_suspension(&mut self, cell: HeapPtr, suspension: TaggedWord) {
        debug_assert_eq!(
            self.read_raw_unchecked(cell, 0),
            BYNEED_UNFORCED,
            "byneed_set_suspension: cell is not Unforced"
        );
        self.write_field_unchecked(cell, 1, suspension);
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
        self.write_barrier(p, i, w);
        self.write_word(p, i, w.to_bits());
    }

    /// Write-barrier hook (ADR-0066 §5) — **no-op in v1**. Every value-slot pointer store routes
    /// through here, so the generational remembered-set barrier and the `local → shared` partition
    /// check drop in at one site later without touching call sites. Its firing condition (a store into
    /// an already-live/old or shared object) is future logic; v1 has neither generations nor a shared
    /// partition, so this does nothing.
    #[inline]
    fn write_barrier(&mut self, _obj: HeapPtr, _slot: u64, _new: TaggedWord) {}

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

    /// Collect on allocation overflow (ADR-0066 §4): the local heap is collected with the **shadow
    /// stack** ([`roots`](Heap::roots)) as the root set, then the caller (`alloc`) retries the bump.
    /// Fed by disjoint field borrows — `self.space`/`self.reserve`/`self.top` are `Copy`, `self.roots`
    /// is borrowed mutably — so there is no `mem::take` window in which the root set is empty.
    fn gc(&mut self) {
        self.top = collect_core(self.space, self.reserve, self.top, &mut self.roots);
        // Swap the two stable base pointers (a `Copy` of the raw handles — no buffer re-borrow). The
        // shadow-stack slots were rewritten in place by `collect_core`; `root_gens` is unaffected
        // (same slots, new values).
        core::mem::swap(&mut self.space, &mut self.reserve);
    }

    /// Collect with an **explicit** root slice — **test-only** (`#[cfg(test)]`, ADR-0066 §4). This is
    /// *not* a public alternate root path: the runtime always collects via [`gc`](Heap::gc) rooted by
    /// the shadow stack. A unit test using this form supplies the *complete* root set and keeps the
    /// shadow stack empty, so the two root sources never coexist and diverge. `roots` are rewritten in
    /// place.
    #[cfg(test)]
    pub(crate) fn collect(&mut self, roots: &mut [TaggedWord]) {
        debug_assert!(
            self.roots.is_empty(),
            "explicit-slice collect must not run with a non-empty shadow stack (ADR-0066 §4)"
        );
        self.top = collect_core(self.space, self.reserve, self.top, roots);
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

/// The Cheney collection core (ADR-0066 §4): a function of the raw semi-space bases and the root
/// slice — *not* `&mut self` — so a caller can invoke it with disjoint field borrows (`gc` passes
/// `&mut self.roots` alongside the `Copy` space handles) without a self-alias, and with no `mem::take`
/// window where the root set is empty. Evacuates everything reachable from `roots` into `to`, rewrites
/// `roots` in place to the survivors' new addresses, and returns the new `top` (used words in `to`,
/// which the caller swaps in as the active space). `from` and `to` are disjoint allocations.
fn collect_core(
    from: NonNull<u64>,
    to: NonNull<u64>,
    top: usize,
    roots: &mut [TaggedWord],
) -> usize {
    // From-space object-start oracle, built once (O(top)) before any forwarding rewrites a header;
    // both the root check and `evacuate` consult it so every pointer read as a header is proven to be
    // a live object start first ([P1]).
    let starts = FromStarts::build(from, top);

    // Release-on: each pointer root must be a live object *header* in the from-space; a stale, foreign,
    // or interior-aliasing root is rejected before any dereference.
    for r in roots.iter() {
        if r.is_pointer() {
            assert!(
                starts.is_start(r.addr()),
                "collect: a root is not a live object in this heap (stale, foreign, or interior alias)"
            );
        }
    }

    let mut free = 0usize;
    for r in roots.iter_mut() {
        *r = evacuate(from, to, &mut free, &starts, *r);
    }

    // Cheney scavenge: walk the objects already in `to`, evacuating their value slots. New survivors
    // are appended past `free`, so the loop terminates when `scan` catches up.
    let mut scan = 0usize;
    while scan < free {
        // SAFETY: `scan < free`; read the header just written into `to`.
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

    free
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
        // [label_ids: value → RawIds | unit sentinel] [values: value → Array | unit sentinel] — both
        // slots are traced (a pointer to a separate object, or an immediate sentinel for the empty
        // record; ADR-0064 §2 / ADR-0069).
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
        // [count: raw] [id: raw; count] — a Record's sorted label ids (ADR-0069); all raw.
        Kind::RawIds => [(0, 0), (0, 0)],
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

    // --- GC-on-alloc + shadow-stack rooting (ADR-0066) ----------------------

    #[test]
    fn gc_on_alloc_relocates_rooted_survivor() {
        // A rooted object survives (and is relocated by) a collection that `alloc` triggers on
        // overflow — the mid-evaluation GC the old "alloc panics" note could not exercise.
        let mut h = Heap::new(8); // small semi-space
        let frame = h.frame();
        let keep = h.new_number(3.5); // 2 words
        let kr = h.root(keep.as_word());

        // Unrooted garbage; each `new_number` is 2 words, so this overflows and forces collections
        // that keep only `keep`.
        for _ in 0..20 {
            let _ = h.new_number(1.0);
        }

        let keep2 = unsafe { HeapPtr::from_word(h.get(kr)) };
        assert_eq!(h.header(keep2).kind(), Kind::NumberBox);
        assert_eq!(f64::from_bits(h.read_raw(keep2, 0)), 3.5);
        h.pop_frame(frame);
    }

    #[test]
    fn gc_during_constructor_preserves_rooted_fields() {
        // A collection fired by the *constructor's own* `alloc` must not lose the incoming field
        // values — `new_adt` self-roots them (ADR-0066 §3), so they survive and the object stores the
        // relocated addresses.
        let mut h = Heap::new(8);
        let a = h.new_number(1.0); // top 2
        let b = h.new_number(2.0); // top 4
        let _garbage = h.new_number(9.0); // top 6 — unrooted, collected during the adt's alloc

        // adt needs 1 header + 1 tag + 2 fields = 4 words; top 6 + 4 > 8 → GC mid-construction.
        let adt = h.new_adt(0, &[a.as_word(), b.as_word()]);

        let f1 = unsafe { HeapPtr::from_word(h.read_field(adt, 1)) };
        let f2 = unsafe { HeapPtr::from_word(h.read_field(adt, 2)) };
        assert_eq!(f64::from_bits(h.read_raw(f1, 0)), 1.0);
        assert_eq!(f64::from_bits(h.read_raw(f2, 0)), 2.0);
        // a(2) + b(2) + adt(4) live; the garbage number is gone.
        assert_eq!(h.used(), 8);
    }

    #[test]
    fn gc_collects_unrooted_between_allocs() {
        // 20 unrooted 2-word allocations = 40 words pushed through an 8-word semi-space: only possible
        // because each GC-on-alloc reclaims the unrooted garbage. Reaching the end without OOM *is*
        // the reclamation proof; `used` never exceeds the semi-space and the rooted `keep` is intact.
        let mut h = Heap::new(8);
        let frame = h.frame();
        let keep = h.new_number(3.5);
        let kr = h.root(keep.as_word());
        for _ in 0..20 {
            let _ = h.new_number(1.0);
        }
        assert!(h.used() <= 8, "used exceeded the semi-space: {}", h.used());
        let keep2 = unsafe { HeapPtr::from_word(h.get(kr)) };
        assert_eq!(f64::from_bits(h.read_raw(keep2, 0)), 3.5);
        h.pop_frame(frame);
    }

    #[test]
    fn new_str_round_trips_including_empty_and_padding() {
        let mut h = Heap::new(64);
        for text in ["", "a", "hello", "日本語", "12345678", "123456789"] {
            let s = h.new_str(text.as_bytes());
            assert_eq!(h.header(s).kind(), Kind::Str);
            assert_eq!(h.str_len(s), text.len());
            assert_eq!(h.str_read(s), text);
            // size_words = 1 (len word) + ceil(byte_len / 8); the empty string is size 1.
            let expect_words = 1 + text.len().div_ceil(WORD) as u64;
            assert_eq!(h.header(s).size_words(), expect_words);
        }
    }

    #[test]
    fn new_str_survives_collection() {
        // A `Str`'s packed bytes must relocate intact (they are raw words the collector skips, but the
        // object still moves as a unit).
        let mut h = Heap::new(64);
        let s = h.new_str(b"purvasm");
        let mut roots = [s.as_word()];
        h.collect(&mut roots);
        let s2 = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.str_read(s2), "purvasm");
    }

    #[test]
    #[should_panic(expected = "valid UTF-8")]
    fn new_str_rejects_invalid_utf8() {
        let mut h = Heap::new(16);
        let _ = h.new_str(&[0xff, 0xfe]); // not valid UTF-8
    }

    #[test]
    fn new_record_with_boxed_values_survives_collection() {
        // A `Record` traces `[label_ids → RawIds (raw, skipped)][values → Array]`; the boxed `Number`
        // values must relocate intact while the `RawIds` bytes are moved but never interpreted.
        let mut h = Heap::new(64);
        let a = h.new_number(1.5);
        let b = h.new_number(2.5);
        let r = h.new_record(&[10, 20], &[a.as_word(), b.as_word()]);
        let mut roots = [r.as_word()];
        h.collect(&mut roots);
        let r2 = unsafe { HeapPtr::from_word(roots[0]) }.as_word();
        assert_eq!(h.checked_record(r2).count, 2);
        let v10 = unsafe { HeapPtr::from_word(h.record_get(r2, 10)) };
        assert_eq!(f64::from_bits(h.read_raw(v10, 0)), 1.5);
        let v20 = unsafe { HeapPtr::from_word(h.record_get(r2, 20)) };
        assert_eq!(f64::from_bits(h.read_raw(v20, 0)), 2.5);
    }

    #[test]
    fn new_record_empty_uses_sentinels() {
        let mut h = Heap::new(16);
        let e = h.new_record(&[], &[]);
        assert_eq!(h.header(e).kind(), Kind::Record);
        assert!(h.read_field(e, 0).is_immediate()); // label_ids sentinel
        assert!(h.read_field(e, 1).is_immediate()); // values sentinel
        assert_eq!(h.checked_record(e.as_word()).count, 0);
    }

    #[test]
    #[should_panic(expected = "unit sentinel")]
    fn checked_record_rejects_non_unit_empty_slot() {
        // A record corrupted via the public `write_field` to hold a non-`unit` immediate must not be
        // read as empty (ADR-0069 §1: the empty record is *exactly* the `unit` sentinel in both slots).
        let mut h = Heap::new(16);
        let e = h.new_record(&[], &[]);
        h.write_field(e, 0, TaggedWord::int(1)); // corrupt the label_ids slot
        let _ = h.checked_record(e.as_word());
    }

    #[test]
    #[should_panic(expected = "non-Str")]
    fn str_read_rejects_non_str_object() {
        // Release-on: `str_read` on a non-`Str` (here a `NumberBox`) must reject the kind, not trust
        // payload word 0 as a length and build an out-of-bounds slice.
        let mut h = Heap::new(16);
        let n = h.new_number(1.0);
        let _ = h.str_read(n);
    }

    #[test]
    #[should_panic(expected = "exceeds its allocated byte capacity")]
    fn str_read_rejects_corrupt_length() {
        // The public `write_raw` can overwrite a `Str`'s `len` word; `str_read` must bound the length
        // against the allocated byte words rather than read out of bounds (UB).
        let mut h = Heap::new(16);
        let s = h.new_str(b"hi"); // byte capacity = 8
        h.write_raw(s, 0, 9999); // corrupt the len word
        let _ = h.str_read(s);
    }

    #[test]
    #[should_panic(expected = "OOM")]
    fn alloc_oom_when_live_set_exceeds_semispace() {
        // A live (rooted) set that alone exceeds a semi-space is a fatal OOM after collect-and-retry —
        // v1 has no heap growth (ADR-0066 §4).
        let mut h = Heap::new(6);
        let frame = h.frame();
        let a = h.new_number(1.0);
        let _ra = h.root(a.as_word()); // 2 words
        let b = h.new_number(2.0);
        let _rb = h.root(b.as_word()); // 4
        let c = h.new_number(3.0);
        let _rc = h.root(c.as_word()); // 6 — full, all rooted
        let _d = h.new_number(4.0); // overflow → GC keeps all 6 → retry still overflows → OOM
        h.pop_frame(frame); // unreached
    }
}
