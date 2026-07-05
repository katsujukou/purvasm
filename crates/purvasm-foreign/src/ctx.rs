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
    pub fn new_adt(&self, tag: u32, fields: &[PvValue<'f>]) -> PvValue<'f> {
        let words: Vec<sys::PVWord> = fields.iter().map(|e| self.word_of(*e)).collect();
        self.root_word(unsafe { sys::pv_new_adt(self.raw, tag, words.as_ptr(), words.len()) })
    }

    /// A mutable one-cell `Ref`.
    pub fn new_ref(&self, init: PvValue<'f>) -> PvValue<'f> {
        let w = self.word_of(init);
        self.root_word(unsafe { sys::pv_new_ref(self.raw, w) })
    }

    /* ── access / application ──────────────────────────────────────────────────────────────── */

    /// Read value-slot `i` of a heap object.
    pub fn read_field(&self, obj: PvValue<'f>, i: u64) -> PvValue<'f> {
        let w = self.word_of(obj);
        self.root_word(unsafe { sys::pv_read_field(self.raw, w, i) })
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
