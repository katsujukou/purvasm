//! The frame-branded context and rooted value handle (ADR-0078 §3).

use core::marker::PhantomData;
use purvasm_sys as sys;

/// The brand: `*mut u8` removes `Send`/`Sync` (a guest value must never cross a thread —
/// single-capability, ADR-0064), and the `fn(&'f ()) -> &'f ()` component makes `'f` invariant so
/// the borrow checker cannot shrink/extend a handle's frame lifetime through variance.
type Brand<'f> = PhantomData<(*mut u8, fn(&'f ()) -> &'f ())>;

/// The runtime context for one shadow-stack frame, branded with the frame lifetime `'f`.
///
/// Only the hidden `__rt` shims construct one (from the `ctx` the runtime passed to the leaf), so
/// every `Ctx` a safe leaf body sees is backed by a live runtime and an open frame — the
/// invariant all the safe methods below rely on. Shape errors (passing a `String` handle to
/// [`Ctx::int`], …) are release-mode runtime faults by the `pv_*` validation contract
/// (ADR-0071 §2), never UB, which is what makes these methods safe to expose.
pub struct Ctx<'f> {
    raw: *mut sys::PVContext,
    _brand: Brand<'f>,
}

/// A guest value, held as a **rooted handle**: the wrapped word is the `pv_root` slot handle,
/// never the value word itself, and every read reloads via `pv_get` — so a collection moving the
/// underlying object between two uses is invisible to the leaf. `Copy` is sound because a handle
/// stays valid for the whole frame (`pv_pop_frame` — which invalidates it — is fenced off by the
/// `'f` brand).
#[derive(Clone, Copy)]
pub struct PvValue<'f> {
    handle: sys::PVWord,
    _brand: Brand<'f>,
}

impl<'f> Ctx<'f> {
    /// # Safety
    /// `raw` must be the live context of the current leaf invocation, with a frame open that
    /// outlives `'f`. Upheld by the `__rt` shims — the only callers.
    pub(crate) unsafe fn new(raw: *mut sys::PVContext) -> Self {
        Ctx {
            raw,
            _brand: PhantomData,
        }
    }

    pub(crate) fn raw(&self) -> *mut sys::PVContext {
        self.raw
    }

    /// Root a raw word into the current frame. Crate-internal: a raw unrooted `PVWord` is never
    /// part of the safe surface (ADR-0078 §3).
    pub(crate) fn root_word(&self, w: sys::PVWord) -> PvValue<'f> {
        // SAFETY: ctx live + frame open (the `Ctx::new` invariant).
        let handle = unsafe { sys::pv_root(self.raw, w) };
        PvValue {
            handle,
            _brand: PhantomData,
        }
    }

    /// The current word behind a handle — the reload-after-safepoint step, done on every read.
    pub(crate) fn word_of(&self, v: PvValue<'f>) -> sys::PVWord {
        // SAFETY: the handle came from `root_word` in this frame (the `'f` brand).
        unsafe { sys::pv_get(self.raw, v.handle) }
    }

    /* ── scalar reads (copy-out; shape errors are runtime faults, not UB) ──────────────────── */

    /// Read an `Int`.
    pub fn int(&self, v: PvValue<'f>) -> i32 {
        unsafe { sys::pv_int_payload(self.raw, self.word_of(v)) }
    }

    /// Read a `Boolean`.
    pub fn bool(&self, v: PvValue<'f>) -> bool {
        unsafe { sys::pv_bool_payload(self.raw, self.word_of(v)) != 0 }
    }

    /// Read a `Number`.
    pub fn number(&self, v: PvValue<'f>) -> f64 {
        f64::from_bits(unsafe { sys::pv_number_bits(self.raw, self.word_of(v)) })
    }

    /// Read a `String` as an owned copy (the two-call `pv_str_len`/`pv_str_copy` shape — the
    /// bytes are caller-owned, valid regardless of later collections).
    pub fn string(&self, v: PvValue<'f>) -> String {
        let w = self.word_of(v);
        // SAFETY: ctx live; `buf` sized by the paired `pv_str_len` call on the same word (no
        // safepoint between the two calls — neither allocates).
        unsafe {
            let len = sys::pv_str_len(self.raw, w);
            let mut buf = vec![0u8; len];
            let copied = sys::pv_str_copy(self.raw, w, buf.as_mut_ptr(), len);
            buf.truncate(copied);
            // The runtime asserts strings are valid UTF-8 at construction (ADR-0067 §5).
            String::from_utf8(buf).expect("purvasm String is valid UTF-8 by runtime contract")
        }
    }

    /* ── constructors (results come back rooted) ───────────────────────────────────────────── */

    /// The `Unit` value.
    pub fn unit(&self) -> PvValue<'f> {
        self.root_word(unsafe { sys::pv_unit() })
    }

    /// An `Int`.
    pub fn new_int(&self, v: i32) -> PvValue<'f> {
        self.root_word(unsafe { sys::pv_int(v) })
    }

    /// A `Boolean`.
    pub fn new_bool(&self, v: bool) -> PvValue<'f> {
        self.root_word(unsafe { sys::pv_bool(v as i32) })
    }

    /// A boxed `Number`.
    pub fn new_number(&self, v: f64) -> PvValue<'f> {
        self.root_word(unsafe { sys::pv_new_number(self.raw, v.to_bits()) })
    }

    /// A `String` (copy-in of UTF-8 bytes).
    pub fn new_str(&self, s: &str) -> PvValue<'f> {
        self.root_word(unsafe { sys::pv_new_str(self.raw, s.as_ptr(), s.len()) })
    }

    /// An immutable `Array`.
    pub fn new_array(&self, elems: &[PvValue<'f>]) -> PvValue<'f> {
        if elems.is_empty() {
            return self.root_word(unsafe { sys::pv_empty_array() });
        }
        // Collecting current words immediately before the single allocating call is safe: the
        // constructor self-roots its arguments across its own allocation (purvasm.h contract).
        let words: Vec<sys::PVWord> = elems.iter().map(|e| self.word_of(*e)).collect();
        self.root_word(unsafe { sys::pv_new_array(self.raw, words.as_ptr(), words.len()) })
    }

    /// An algebraic-data value.
    ///
    /// An empty `fields` is the NULLARY constructor, which the runtime represents as an immediate
    /// rather than a heap object — the representation a generated `case` matches, since it splits on
    /// representation before comparing tags. The two go to different ABI entries, and choosing
    /// between them here is the point: a leaf writes `new_adt(tag, &[])` for `Nothing` and never
    /// learns that the encoding differs.
    pub fn new_adt(&self, tag: u32, fields: &[PvValue<'f>]) -> PvValue<'f> {
        if fields.is_empty() {
            return self.root_word(unsafe { sys::pv_new_nullary_adt(tag) });
        }
        let words: Vec<sys::PVWord> = fields.iter().map(|e| self.word_of(*e)).collect();
        self.root_word(unsafe { sys::pv_new_adt(self.raw, tag, words.as_ptr(), words.len()) })
    }

    /// A mutable one-cell `Ref`.
    pub fn new_ref(&self, init: PvValue<'f>) -> PvValue<'f> {
        let w = self.word_of(init);
        self.root_word(unsafe { sys::pv_new_ref(self.raw, w) })
    }

    /* ── access / application ──────────────────────────────────────────────────────────────── */

    /// An `Array`'s element count (the empty array reads as 0).
    pub fn array_len(&self, v: PvValue<'f>) -> usize {
        unsafe { sys::pv_array_len(self.raw, self.word_of(v)) }
    }

    /// Read value-slot `i` of a heap object.
    ///
    /// For an **ADT use [`Self::adt_field`]**, not this: an `Adt`'s payload is `[tag] ++ fields`, so
    /// slot 0 is the raw tag rather than a value — reading it here would hand back a word that is not
    /// a value at all, and the layout would have leaked into the leaf besides.
    pub fn read_field(&self, obj: PvValue<'f>, i: u64) -> PvValue<'f> {
        let w = self.word_of(obj);
        self.root_word(unsafe { sys::pv_read_field(self.raw, w, i) })
    }

    /// An algebraic-data value's constructor tag — the number [`Self::new_adt`] was given, which is
    /// `fnv1a64(name).lo & 0x7fffffff` over the constructor's fully qualified NAME.
    ///
    /// This is how a leaf inspects a `Maybe`/`Either` it was handed, and it answers for a nullary
    /// constructor too (which has no heap object at all). Without it the safe layer could receive a
    /// data value and do nothing with it, which would leave ADR-0111's "one authoring surface" true
    /// only for C.
    pub fn adt_tag(&self, adt: PvValue<'f>) -> u32 {
        unsafe { sys::pv_adt_tag(self.raw, self.word_of(adt)) }
    }

    /// Field `i` of an algebraic-data value (0-based over the constructor's FIELDS).
    ///
    /// Distinct from [`Self::read_field`] because the tag occupies payload word 0, so field `i` is
    /// slot `i + 1`. Keeping that offset here means a leaf never encodes the ADT layout itself — and
    /// it is why both guards below exist rather than being hygiene:
    ///
    /// - `i + 1` would WRAP at `u64::MAX` in a release profile (overflow checks off), landing on slot
    ///   0, which is the raw tag: a word that is not a value at all. `checked_add` refuses instead,
    ///   in every profile.
    /// - the shape is checked with [`Self::adt_tag`] FIRST, so handing this an `Array` faults in the
    ///   runtime the way [`Ctx`]'s contract says a shape error does, rather than quietly returning
    ///   that array's element `i + 1`.
    pub fn adt_field(&self, adt: PvValue<'f>, i: u64) -> PvValue<'f> {
        let slot = i
            .checked_add(1)
            .expect("adt_field: field index overflows (the tag occupies slot 0)");
        let _ = self.adt_tag(adt); // shape check: faults unless this really is an ADT
        let w = self.word_of(adt);
        self.root_word(unsafe { sys::pv_read_field(self.raw, w, slot) })
    }

    /// Write value-slot `i` of a heap object.
    pub fn write_field(&self, obj: PvValue<'f>, i: u64, v: PvValue<'f>) {
        let (ow, vw) = (self.word_of(obj), self.word_of(v));
        unsafe { sys::pv_write_field(self.raw, ow, i, vw) }
    }

    /// Apply `f` to `args` (curried; over-/under-application handled by the runtime), with the
    /// same contract the C header gives `pv_apply`.
    pub fn apply(&self, f: PvValue<'f>, args: &[PvValue<'f>]) -> PvValue<'f> {
        let fw = self.word_of(f);
        let words: Vec<sys::PVWord> = args.iter().map(|a| self.word_of(*a)).collect();
        self.root_word(unsafe { sys::pv_apply(self.raw, fw, words.as_ptr(), words.len()) })
    }

    /// Force a by-need cell to its value; passes any non-cell through unchanged.
    pub fn force_if_byneed(&self, v: PvValue<'f>) -> PvValue<'f> {
        let w = self.word_of(v);
        self.root_word(unsafe { sys::pv_force_if_byneed(self.raw, w) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use purvasm_rt::abi;

    /// Run `body` against a live runtime, under an open shadow-stack frame.
    ///
    /// The frame is not decoration: `Ctx::new`'s safety contract requires one that outlives the
    /// borrow, because every `PvValue` this hands out is a ROOT in that frame. `leaf_shim` opens it
    /// in production; a test that skipped it would be rooting into a frame that does not exist.
    ///
    /// The unwind dance exists for the same reason. One of these tests panics on purpose, and a
    /// panic must not skip `pv_pop_frame`/`pv_runtime_free` — so the panic is caught, the runtime is
    /// torn down in order, and only then is the unwind resumed for `#[should_panic]` to observe.
    ///
    /// `Ctx::new` is `pub(crate)`, which is why these tests live here rather than in `tests/`: a
    /// leaf-level test could only reach these paths through `leaf_shim`, whose panic guard turns
    /// them into an abort.
    fn with_ctx<R>(body: impl FnOnce(&Ctx<'_>) -> R) -> R {
        let heap = abi::pv_runtime_new(1 << 16);
        let raw = heap.cast::<sys::PVContext>();
        let mark = unsafe { sys::pv_frame(raw) };
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let cx = unsafe { Ctx::new(raw) };
            body(&cx)
        }));
        unsafe { sys::pv_pop_frame(raw, mark) };
        unsafe { abi::pv_runtime_free(heap) };
        match outcome {
            Ok(r) => r,
            Err(payload) => std::panic::resume_unwind(payload),
        }
    }

    /// The legacy entry still answers with a POINTER for `n == 0`, and the new one with an immediate
    /// — both carrying the same tag.
    ///
    /// This contrast is the whole basis for calling the change additive (ADR-0111 §5). If the old
    /// entry's answer ever changed shape, a provider built before the nullary entry existed would
    /// start behaving differently against a runtime that still says it speaks version 1 — so the
    /// wrong-but-unchanged behaviour is pinned here deliberately, beside the right one.
    #[test]
    fn the_legacy_entry_keeps_its_v1_representation() {
        with_ctx(|cx| {
            let tag = 4242;
            let legacy = unsafe { sys::pv_new_adt(cx.raw(), tag, std::ptr::null(), 0) };
            let canonical = unsafe { sys::pv_new_nullary_adt(tag) };

            assert_eq!(
                legacy & 1,
                0,
                "the v1 entry must still return a heap pointer"
            );
            assert_eq!(
                canonical & 1,
                1,
                "the nullary entry must return an immediate"
            );
            assert_ne!(
                legacy, canonical,
                "the two representations must stay distinguishable"
            );

            // Both carry the tag, which is why the wrong one is wrong *silently* — and why the
            // owned VM could accept it while a native `case` could not.
            assert_eq!(unsafe { sys::pv_adt_tag(cx.raw(), legacy) }, tag);
            assert_eq!(unsafe { sys::pv_adt_tag(cx.raw(), canonical) }, tag);
        });
    }

    /// The field index is offset past the tag, so an index at the type's maximum WRAPS to slot 0 —
    /// the raw tag — in any profile with overflow checks off, which is every ordinary release build.
    /// `checked_add` is what makes the refusal profile-independent, and this pins it.
    #[test]
    #[should_panic(expected = "field index overflows")]
    fn adt_field_refuses_an_index_that_would_wrap_onto_the_tag() {
        with_ctx(|cx| {
            let inner = cx.new_int(1);
            let adt = cx.new_adt(7, &[inner]);
            let _ = cx.adt_field(adt, u64::MAX);
        });
    }

    /// A field read is only meaningful on an ADT. Without the shape check this answered with the
    /// *array's* element `i + 1`, silently and with the wrong value — against `Ctx`'s contract that a
    /// shape error is a runtime fault.
    ///
    /// The fault is an abort rather than a panic (the runtime's `guard` catches and aborts), so it
    /// cannot be observed in-process: the test re-runs itself as a child and asserts the child died.
    #[test]
    // The child re-runs this test binary, which needs `current_exe` — an isolated operation Miri
    // refuses, and one this test cannot avoid: the fault under test is a process ABORT (the
    // runtime catches the panic and calls `abort`), so it is only observable from outside the
    // process. Miri checks for UB, which is a different axis; the sibling tests still run there.
    #[cfg_attr(
        miri,
        ignore = "re-runs the test binary; current_exe is isolated under Miri"
    )]
    fn adt_field_on_an_array_faults() {
        const CHILD: &str = "PVF_ADT_FIELD_ARRAY_CHILD";
        if std::env::var(CHILD).is_ok() {
            with_ctx(|cx| {
                let elem = cx.new_int(1);
                let array = cx.new_array(&[elem, elem]);
                let _ = cx.adt_field(array, 0);
            });
            return; // reaching here means no fault, and the parent's assertion fails
        }
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "ctx::tests::adt_field_on_an_array_faults"])
            .env(CHILD, "1")
            .output()
            .expect("re-running the test binary");
        assert!(
            !status.status.success(),
            "adt_field on an Array must fault; the child exited successfully"
        );
    }
}
