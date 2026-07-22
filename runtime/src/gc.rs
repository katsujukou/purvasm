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
use crate::heap_words::parse_heap_words_env;
use crate::stats::{parse_stats_env, Stats};
use crate::word::TaggedWord;
use core::ptr::NonNull;
use std::time::Instant;

/// Bytes per heap word.
const WORD: usize = core::mem::size_of::<u64>();

/// The `StrSlice.cp_len` "not yet counted" sentinel (ADR-0103 §2). A real count never reaches it
/// (`cp_len <= byte_len`, and a byte length near `u64::MAX` cannot be allocated).
pub(crate) const CP_UNKNOWN: u64 = u64::MAX;

/// A normalised view of a string value (ADR-0103 §1): the packed backing `Str`, the byte range
/// within it, and the code-point count when the object carries one (`None` for a bare `Str` and
/// for an un-memoised slice). Produced by [`Heap::str_view`], which release-validates the shape.
/// Holds the backing *pointer* — stale after any allocation; re-derive, never store.
#[derive(Clone, Copy)]
pub(crate) struct StrView {
    pub base: HeapPtr,
    pub off: usize,
    pub len: usize,
    pub cp: Option<usize>,
}

/// `ByNeed` cell `state` word (ADR-0070 §1). `pub(crate)`: shared with the `force` operation.
pub(crate) const BYNEED_UNFORCED: u64 = 0;
pub(crate) const BYNEED_BUILDING: u64 = 1;
pub(crate) const BYNEED_FORCED: u64 = 2;

/// The ABI context header (ADR-0079 §1): the **first bytes of every [`Heap`]**, mirrored by
/// `pv_ctx_header` in `include/purvasm.h`. Release-mode codegen bakes these offsets into its
/// inline rooting/trampoline fast paths, and the ADR-0078 sys layer mirrors the struct — the
/// layout is CONTRACT. Any change bumps `PV_CTX_HEADER_VERSION` (and the `pv_ctx_abi_v<N>`
/// link symbol) in lockstep with the header file; the `const` assertions below pin the layout
/// at compile time on this side of the mirror (a drift is layout, not aliasing — Miri cannot
/// see it, ADR-0079 §1).
///
/// The shadow-stack storage it points at follows the semi-space discipline (stable raw base,
/// no whole-buffer `&mut` re-borrows, ADR-0063 §2/§8); the base moves only on growth, inside
/// the slow-path [`Heap::root`].
#[repr(C)]
pub(crate) struct CtxHeader {
    /// The shadow stack's storage base (owns the allocation; freed in [`Heap`]'s `Drop`).
    roots_base: *mut u64,
    /// One past the top root = the next handle = the frame mark.
    roots_len: u64,
    /// Fast-path bound: `roots_len == roots_cap` → the slow-path entry grows.
    roots_cap: u64,
    /// `0` = no stashed generic tail (`pv_settle`'s fast path); non-zero = stashed.
    pending_tail: u64,
}

// The two compile-time halves of the ADR-0079 §1 layout net (the run/link-time halves are
// `pv_abi_check` and the `pv_ctx_abi_v<N>` symbol, in `abi.rs`).
const _: () = {
    assert!(core::mem::size_of::<CtxHeader>() == 32);
    assert!(core::mem::offset_of!(CtxHeader, roots_base) == 0);
    assert!(core::mem::offset_of!(CtxHeader, roots_len) == 8);
    assert!(core::mem::offset_of!(CtxHeader, roots_cap) == 16);
    assert!(core::mem::offset_of!(CtxHeader, pending_tail) == 24);
};

/// Initial shadow-stack capacity in words. Small; growth doubles inside the slow-path root.
const ROOTS_INITIAL_CAP: usize = 256;

/// A single-capability local heap with two equal semi-spaces (ADR-0064 §5).
///
/// Each semi-space is a raw allocation reached through a **single stable base pointer** whose
/// provenance spans the whole buffer; every [`HeapPtr`] is derived from that base and never through a
/// fresh `&mut`/`&` re-borrow of the buffer. This is load-bearing (ADR-0063 §2/§8): re-borrowing the
/// buffer as `&mut` (`Box::as_mut_ptr`, slice indexing, `as_mut()`) would issue a Unique retag that
/// **invalidates every outstanding `HeapPtr`** — a use-after-invalidation the borrow model rejects
/// (and `noalias`-based codegen could miscompile). Only raw-pointer arithmetic off the stable base
/// touches the heap after construction.
///
/// `#[repr(C)]` with [`CtxHeader`] first: offset 0 of the context pointer IS the ABI header
/// (ADR-0079 §1) — the `const` assertion below pins it.
#[repr(C)]
pub struct Heap {
    /// The ABI context header (ADR-0079): shadow-stack storage + the pending-tail flag, at
    /// offset 0 so generated code and the sys mirror can reach it by fixed offsets.
    header: CtxHeader,
    /// Stable base of the active allocation space (`cap` words; owns the allocation, freed in `Drop`).
    space: NonNull<u64>,
    /// Stable base of the idle space — the target of the next collection.
    reserve: NonNull<u64>,
    /// Words per semi-space (both are equal).
    cap: usize,
    /// Bump cursor: words used in `space`.
    top: usize,
    /// Object-start bitmap for the **active** space: bit `i` is set iff a live object header begins
    /// at word `i` (`i < top`). The O(1) oracle behind [`is_object_start`](Heap::is_object_start) —
    /// the per-dereference liveness/shape check (ADR-0071 §1) was a full record-walk (O(objects))
    /// before this, which made every `pv_read_field`/`apply` linear in the heap and the compiled
    /// self-compiler quadratic-plus. Maintained by `alloc` (set the new base's bit) and rebuilt
    /// after each collection ([`rebuild_starts`](Heap::rebuild_starts)); packed 64 words per `u64`
    /// (~0.2 % of the space).
    starts_bits: Vec<u64>,
    // The **shadow stack** (ADR-0066 §1) lives in `header` (ADR-0079): `roots_base[0..roots_len]`
    // are value slots the collector rewrites in place on relocation; a [`Root`] indexes them.
    // `alloc` feeds them to the collector on overflow, so a value rooted here survives (and is
    // reloaded via [`Heap::get`]). Stored as raw `u64` bits (`TaggedWord` is `repr(transparent)`),
    // never a Rust reference into the heap (ADR-0063 §2).
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
    /// Interpretation of a closure's `code` word (ADR-0071 §3). `false` (the [`Heap::new`] bring-up /
    /// Miri path): an index into [`code_table`](Heap::code_table). `true` (the [`Heap::new_native`]
    /// codegen path): a **real `extern "C"` fn address** [`apply`](Heap::apply) calls directly. A single
    /// heap is one or the other, never both — Miri only ever runs the index path, so the address
    /// transmute is never *executed* under Miri (ADR-0071 §3 / ADR-0063 §4).
    code_is_address: bool,
    /// The trampoline **pending-tail slot** (ADR-0071 §4): `(f, args)` a `CodeFn` body stashed via
    /// `pv_tailcall` as its final action, taken by the enclosing [`apply`](Heap::apply) after the body
    /// returns. `Some` *is* the status flag — no reserved sentinel value. Single slot: written then read
    /// with no intervening `code` call, so it never crosses `apply` activations (only the innermost
    /// active loop reads it). `None` on every index-path heap (no `pv_tailcall`).
    pending_tail: Option<(TaggedWord, Vec<TaggedWord>)>,
    /// Debug tracing (ADR-0072 §11): when set, `apply`/`force`/leaf entry points log a compact
    /// value-flow trace to `stderr`. Enabled by the `PURVASM_TRACE` environment variable on a native
    /// (`new_native`) heap only — the compiled binary reads it, so any program can be traced with
    /// `PURVASM_TRACE=1 ./app` without recompiling. Always `false` on a `lib`/Miri heap.
    trace: bool,
    /// Opt-in dynamic-apply/GC counters (ADR-0102 §3). `Some` iff `PURVASM_STATS=1` on a native
    /// (`new_native`) heap, or a test enabled it directly ([`enable_stats_for_test`](Heap::enable_stats_for_test));
    /// `None` (the default) means every instrumentation point below is a single flag check and
    /// nothing else — no formatting, clock reads, or counter writes on the hot path.
    stats: Option<Stats>,
    /// GC-stress mode (ADR-0105 §5, test-only instrumentation): when set, [`alloc`](Heap::alloc)
    /// collects at **every** entry — the safepoint superset — so every missing-root window
    /// deterministically contains a collection (what it guarantees is the window being *exercised*;
    /// observability of a corruption is the fixtures' readback/checksum concern). Enabled by
    /// `PURVASM_GC_STRESS=1` on a native (`new_native`) heap, or directly by a test; production
    /// heap/ABI semantics are unchanged when off (a single flag check on the alloc path).
    gc_stress: bool,
}

// ADR-0079 §1: the header IS offset 0 of the context pointer.
const _: () = assert!(core::mem::offset_of!(Heap, header) == 0);

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
            header: CtxHeader {
                roots_base: alloc_space(ROOTS_INITIAL_CAP).as_ptr(),
                roots_len: 0,
                roots_cap: ROOTS_INITIAL_CAP as u64,
                pending_tail: 0,
            },
            space: alloc_space(words_per_space),
            reserve: alloc_space(words_per_space),
            cap: words_per_space,
            top: 0,
            starts_bits: vec![0u64; words_per_space.div_ceil(64)],
            #[cfg(debug_assertions)]
            root_gens: Vec::new(),
            #[cfg(debug_assertions)]
            next_gen: 0,
            code_table: Vec::new(),
            output: Vec::new(),
            code_is_address: false,
            pending_tail: None,
            trace: false,
            stats: None,
            gc_stress: false,
        }
    }

    /// A heap for the **codegen / native-ABI path** (ADR-0071 §3): identical to [`Heap::new`] except a
    /// closure's `code` word is a **real `extern "C"` fn address** (`apply` calls it directly) rather
    /// than a [`code_table`](Heap::code_table) index. The `extern "C"` boundary (`pv_runtime_new`) builds
    /// heaps this way; hand-written `CodeFn` tests / Miri use [`Heap::new`].
    ///
    /// # Panics
    /// If `PURVASM_STATS` (ADR-0102 §3) or `PURVASM_GC_STRESS` (ADR-0105 §5) is present and not
    /// exactly `"1"`, or `PURVASM_HEAP_WORDS` is present and not a non-empty positive decimal
    /// `usize` (ADR-0102 §4) — a present-but-malformed value is a diagnosable configuration error,
    /// not silently ignored. Across the `pv_runtime_new` ABI entry this becomes a process abort
    /// (`guard`'s panic containment); a direct Rust-level call (tests) just unwinds.
    pub fn new_native(words_per_space: usize) -> Heap {
        // Heap-size override (ADR-0102 §4): absent means "use the codegen-provided default"
        // (`words_per_space`, unchanged); a present value replaces it outright. This is scoped to
        // measurement/robustness — it removes the need to rebuild or relink a native compiler just to
        // test whether an OOM or GC cliff is heap-size-related; it is not heap growth or a GC design
        // change. Read here (not in `new`) so `lib`/Miri heaps never touch the environment, matching
        // the `PURVASM_TRACE`/`PURVASM_STATS` precedent below.
        let heap_words_raw = std::env::var_os("PURVASM_HEAP_WORDS");
        let heap_words_parsed = match &heap_words_raw {
            None => parse_heap_words_env(None),
            Some(v) => match v.to_str() {
                Some(s) => parse_heap_words_env(Some(s)),
                None => Err(format!(
                    "PURVASM_HEAP_WORDS: expected a non-empty positive decimal integer, got non-UTF-8 \
                     value {v:?}"
                )),
            },
        };
        let words_per_space = match heap_words_parsed {
            Ok(None) => words_per_space,
            Ok(Some(override_words)) => override_words,
            Err(msg) => panic!("{msg}"),
        };
        let mut h = Heap::new(words_per_space);
        h.code_is_address = true;
        // Opt-in value-flow tracing for the compiled binary; read here (not in `new`) so `lib`/Miri
        // heaps never touch the environment (ADR-0072 §11).
        h.trace = std::env::var_os("PURVASM_TRACE").is_some();
        // Opt-in apply/GC counters (ADR-0102 §3); same rationale — read only on the native path. A
        // present non-Unicode value cannot be `"1"`, so it folds into the same "other present value"
        // rejection as a present-but-wrong string.
        let stats_raw = std::env::var_os("PURVASM_STATS");
        let parsed = match &stats_raw {
            None => parse_stats_env(None),
            Some(v) => match v.to_str() {
                Some(s) => parse_stats_env(Some(s)),
                None => Err(format!(
                    "PURVASM_STATS: expected absent or \"1\", got non-UTF-8 value {v:?}"
                )),
            },
        };
        h.stats = match parsed {
            Ok(true) => Some(Stats::default()),
            Ok(false) => None,
            Err(msg) => panic!("{msg}"),
        };
        // Opt-in GC-stress mode (ADR-0105 §5); same accept-"1"-or-absent contract and native-only
        // read as PURVASM_STATS above.
        h.gc_stress = match std::env::var_os("PURVASM_GC_STRESS") {
            None => false,
            Some(v) => match v.to_str() {
                Some("1") => true,
                Some(s) => panic!("PURVASM_GC_STRESS: expected absent or \"1\", got {s:?}"),
                None => {
                    panic!("PURVASM_GC_STRESS: expected absent or \"1\", got non-UTF-8 value {v:?}")
                }
            },
        };
        h
    }

    /// This heap's semi-space word count — **test-only** (ADR-0102 §4 Verification): lets a
    /// subprocess fixture confirm `PURVASM_HEAP_WORDS` actually replaced (or left unchanged) the
    /// codegen-provided default, without a public capacity accessor on the real API. Its only callers
    /// are `abi.rs`'s address-path subprocess tests, which are themselves `not(miri)`-gated (Miri
    /// cannot spawn subprocesses) — matching that gate here, not just `#[cfg(test)]`, keeps a Miri
    /// build from seeing this as dead code.
    #[cfg(all(test, not(miri)))]
    pub(crate) fn cap_for_test(&self) -> usize {
        self.cap
    }

    /// Force apply/GC counters on for this heap without touching the environment — **test-only**
    /// (ADR-0102 §3 Verification): lets an in-process fixture assert deterministic counter values on
    /// a hand-built [`Heap::new`]/[`Heap::new_native`] heap with no `PURVASM_STATS` env-mutation race
    /// against parallel `cargo test` threads.
    #[cfg(test)]
    pub(crate) fn enable_stats_for_test(&mut self) {
        self.stats = Some(Stats::default());
    }

    /// Force GC-stress mode on for this heap without touching the environment — **test-only**
    /// (ADR-0105 §5 slice 0): the in-process analogue of `PURVASM_GC_STRESS=1`, with the same
    /// no-env-mutation rationale as [`enable_stats_for_test`](Heap::enable_stats_for_test).
    #[cfg(test)]
    pub(crate) fn enable_gc_stress_for_test(&mut self) {
        self.gc_stress = true;
    }

    /// The current apply/GC counters (ADR-0102 §3), or `None` if stats are disabled for this
    /// context. Read by `pv_runtime_free` to decide whether to emit the summary line.
    #[inline]
    pub(crate) fn stats(&self) -> Option<&Stats> {
        self.stats.as_ref()
    }

    /// Mutable access to the apply/GC counters (ADR-0102 §3) for the instrumentation points in
    /// sibling modules (`apply`, `abi`) — `stats` itself stays private, matching the
    /// [`trace_on`](Heap::trace_on)/[`code_is_address`](Heap::code_is_address) accessor convention.
    #[inline]
    pub(crate) fn stats_mut(&mut self) -> Option<&mut Stats> {
        self.stats.as_mut()
    }

    /// Whether debug tracing is on (the `PURVASM_TRACE` env var, native heaps only).
    #[inline]
    pub(crate) fn trace_on(&self) -> bool {
        self.trace
    }

    /// Log a labelled value's runtime kind/arity when tracing is on (ADR-0072 §11) — the compact form
    /// used to follow a value through `apply`/`force`/leaves. A pointer prints its `Kind` and the header's
    /// arity/tag word; an immediate prints its bits.
    pub(crate) fn trace_value(&self, label: &str, v: TaggedWord) {
        if !self.trace {
            return;
        }
        if v.is_pointer() {
            let p = self.checked_ptr(v);
            let k = self.header_unchecked(p).kind();
            let w0 = self.read_raw_unchecked(p, 0);
            let w1 = self.read_raw_unchecked(p, 1);
            eprintln!("[trace] {label}: {k:?} w0=0x{w0:x} w1={w1}");
        } else {
            eprintln!("[trace] {label}: imm=0x{:x}", v.to_bits());
        }
    }

    /// Whether the `code` word is a real address (ADR-0071 §3) — read by [`apply`](Heap::apply).
    #[inline]
    pub(crate) fn code_is_address(&self) -> bool {
        self.code_is_address
    }

    /// Stash the trampoline pending-tail `(f, args)` (ADR-0071 §4). Called by `pv_tailcall` as a body's
    /// final action; the enclosing `apply` loop takes it after the body returns.
    #[inline]
    pub(crate) fn set_pending_tail(&mut self, f: TaggedWord, args: Vec<TaggedWord>) {
        // The slot is read-and-cleared before any further stash, so a live `Some` here would be a
        // protocol violation (two `pv_tailcall`s with no intervening `apply` take).
        debug_assert!(
            self.pending_tail.is_none(),
            "pv_tailcall: pending-tail slot already set (protocol violation)"
        );
        self.pending_tail = Some((f, args));
        self.header.pending_tail = 1; // the ADR-0079 fast-path flag mirrors the Option
        if let Some(s) = self.stats.as_mut() {
            s.pv_tailcall_writes = s.pv_tailcall_writes.saturating_add(1);
        }
    }

    /// Take the pending-tail `(f, args)`, clearing the slot (ADR-0071 §4). `None` = the body returned a
    /// real value (index-path heaps never set it).
    #[inline]
    pub(crate) fn take_pending_tail(&mut self) -> Option<(TaggedWord, Vec<TaggedWord>)> {
        self.header.pending_tail = 0; // cleared with the Option (ADR-0079 fast-path flag)
        self.pending_tail.take()
    }

    /// The captured stdio write-line output (ADR-0067 §5), in program order — the test/differential
    /// view of the output sink.
    #[inline]
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Take and **clear** the output sink, returning the captured lines (ADR-0067 §5). Used by
    /// `pv_drain_output` to flush to real `stdout` exactly once — draining, so a second flush emits
    /// nothing rather than re-printing.
    #[inline]
    pub(crate) fn take_output(&mut self) -> Vec<String> {
        core::mem::take(&mut self.output)
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

    /// The current shadow-stack length (root count) — the header is authoritative (ADR-0079).
    #[inline]
    fn roots_len(&self) -> usize {
        self.header.roots_len as usize
    }

    /// Open a shadow-stack frame — capture the current stack length as a [`RootFrame`] mark.
    #[inline]
    pub(crate) fn frame(&self) -> RootFrame {
        RootFrame {
            len: self.roots_len(),
        }
    }

    /// Push `v` onto the shadow stack; return a [`Root`] handle to its slot. The collector rewrites
    /// the slot on relocation, so [`get`](Heap::get) reads the current (post-collection) value.
    /// This is ADR-0079's slow path: release codegen inlines the in-capacity store and reaches
    /// this entry only to grow (which is the one action that moves `roots_base`).
    #[inline]
    pub(crate) fn root(&mut self, v: TaggedWord) -> Root {
        if self.header.roots_len == self.header.roots_cap {
            self.grow_roots();
        }
        let index = self.roots_len();
        // SAFETY: `index < roots_cap` after the growth check; `roots_base` is the region's stable
        // base (same single-provenance discipline as the semi-spaces).
        unsafe {
            *self.header.roots_base.add(index) = v.to_bits();
        }
        self.header.roots_len += 1;
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

    /// Double the shadow-stack region: allocate, copy the live prefix, free the old region, update
    /// the header base/cap. The ONLY place `roots_base` moves (ADR-0079 §2).
    #[cold]
    fn grow_roots(&mut self) {
        let old_cap = self.header.roots_cap as usize;
        let new_cap = old_cap * 2;
        let new_base = alloc_space(new_cap).as_ptr();
        // SAFETY: disjoint regions; the live prefix is `roots_len <= old_cap` words.
        unsafe {
            core::ptr::copy_nonoverlapping(self.header.roots_base, new_base, self.roots_len());
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.header.roots_base,
                old_cap,
            )));
        }
        self.header.roots_base = new_base;
        self.header.roots_cap = new_cap as u64;
    }

    /// Read the **current** value of a root — the reload-after-safepoint step (ADR-0066 §2). In debug,
    /// asserts the slot has not been popped and reused (generation match); in release, a stale handle
    /// whose index is past the top is caught by the assert below (safe — never out-of-bounds UB).
    #[inline]
    pub(crate) fn get(&self, r: Root) -> TaggedWord {
        #[cfg(debug_assertions)]
        debug_assert_eq!(
            self.root_gens[r.index], r.generation,
            "stale Root: shadow-stack slot was popped and reused"
        );
        assert!(
            r.index < self.roots_len(),
            "Root index past the shadow-stack top"
        );
        // SAFETY: bounds-checked against `roots_len` just above; stable base.
        unsafe { TaggedWord::from_bits(*self.header.roots_base.add(r.index)) }
    }

    /// Close a frame: truncate the shadow stack back to the mark, releasing every root pushed since.
    #[inline]
    pub(crate) fn pop_frame(&mut self, f: RootFrame) {
        debug_assert!(
            f.len <= self.roots_len(),
            "pop_frame: mark exceeds shadow-stack top (double-pop / stale frame)"
        );
        self.header.roots_len = f.len as u64;
        #[cfg(debug_assertions)]
        self.root_gens.truncate(f.len);
    }

    /// Read the raw shadow-stack slot at absolute index `i` (crate-internal; used by the self-rooting
    /// constructors that just pushed a contiguous run and know their base). No generation check —
    /// the caller pushed the slot within the current frame.
    #[inline]
    fn root_slot(&self, i: usize) -> TaggedWord {
        assert!(
            i < self.roots_len(),
            "root_slot index past the shadow-stack top"
        );
        // SAFETY: bounds-checked just above; stable base.
        unsafe { TaggedWord::from_bits(*self.header.roots_base.add(i)) }
    }

    // --- C-ABI rooting (ADR-0071 §5): opaque `u64` handles codegen round-trips verbatim -----------
    //
    // The `Root` / `RootFrame` structs cannot cross the C-ABI, and `Root`'s debug generation stamp
    // must not make the ABI vary by build profile. So the boundary passes plain `u64`s codegen never
    // interprets, into which (debug only) the generation is packed so the stale-reuse check survives.

    /// `pv_frame` (ADR-0071 §5): a frame mark as an opaque `u64` (the shadow-stack length).
    #[inline]
    pub(crate) fn abi_frame(&self) -> u64 {
        self.frame().len as u64
    }

    /// `pv_pop_frame` (ADR-0071 §5): truncate back to an `abi_frame` mark.
    #[inline]
    pub(crate) fn abi_pop_frame(&mut self, mark: u64) {
        self.pop_frame(RootFrame { len: mark as usize });
    }

    /// `pv_root` (ADR-0071 §5): root `v` (a raw value word) and return an opaque handle. Debug packs
    /// the slot's generation into the high 32 bits so [`abi_get`](Heap::abi_get) can reject a stale
    /// reused index; release returns the bare index. Codegen treats the handle as opaque either way.
    #[inline]
    pub(crate) fn abi_root(&mut self, v: u64) -> u64 {
        let r = self.root(TaggedWord::from_bits(v));
        #[cfg(debug_assertions)]
        {
            debug_assert!(r.index < (1 << 32), "shadow stack deeper than 2^32 slots");
            ((r.generation & 0xFFFF_FFFF) << 32) | (r.index as u64)
        }
        #[cfg(not(debug_assertions))]
        {
            r.index as u64
        }
    }

    /// `pv_get` (ADR-0071 §5): the current value of an `abi_root` handle — the reload-after-safepoint
    /// step. Debug reconstructs the generation-stamped [`Root`] so [`get`](Heap::get)'s stale-handle
    /// assert fires; release reads by bare index.
    #[inline]
    pub(crate) fn abi_get(&self, handle: u64) -> u64 {
        #[cfg(debug_assertions)]
        let r = Root {
            index: (handle & 0xFFFF_FFFF) as usize,
            generation: handle >> 32,
        };
        #[cfg(not(debug_assertions))]
        let r = Root {
            index: handle as usize,
        };
        self.get(r).to_bits()
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
        // GC-stress (ADR-0105 §5): collect at EVERY alloc entry, so any missing-root window is
        // exercised deterministically. Before the overflow check — the stressed collection usually
        // makes the overflow branch moot, and a still-full heap falls through to the normal path.
        if self.gc_stress {
            self.gc();
        }
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
        self.starts_bits[base >> 6] |= 1u64 << (base & 63);
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

    /// The byte length of a **packed** `Str` (the raw-layout read; string *values* go through
    /// [`str_view`](Self::str_view)/[`str_len`](Self::str_len), which also accept a `StrSlice`).
    /// **Release-checked** (this is reachable from the safe public API, so the checks are not
    /// `debug`-only): asserts the object is a `Str` and that its stored `len` fits the allocated
    /// byte words, so a non-`Str` object or a `len` word corrupted via the public `write_raw`
    /// cannot drive a reader into an out-of-bounds slice (UB).
    #[inline]
    pub(crate) fn packed_str_len(&self, s: HeapPtr) -> usize {
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

    /// The normalised view of a string value — a packed `Str` or a `StrSlice` (ADR-0103 §1), as
    /// `{ base, off, len }` over the packed backing. **Release-checked** (every string reader
    /// funnels through here): the kind must be `Str`/`StrSlice`; a slice's `base` must be a packed
    /// `Str` (never nested), its byte range must lie within the backing, and its `cp_len` must be
    /// the `CP_UNKNOWN` sentinel or `<= byte_len`. Any violation is a fault, never UB.
    ///
    /// The view carries the backing *pointer*: a caller that allocates re-derives the view after
    /// (the moving-GC discipline, ADR-0063 §2 — no borrowed bytes across a safepoint).
    pub(crate) fn str_view(&self, s: HeapPtr) -> StrView {
        let hdr = self.header(s); // checked: validates `s` is a live object header
        match hdr.kind() {
            Kind::Str => StrView {
                base: s,
                off: 0,
                len: self.packed_str_len(s),
                cp: None,
            },
            Kind::StrSlice => {
                let base_w = self.read_field(s, 0);
                assert!(
                    base_w.is_pointer(),
                    "StrSlice base is not a pointer (corrupt slice)"
                );
                // SAFETY: `base_w` is a pointer-tagged word out of a validated slice's value slot;
                // `header`/`packed_str_len` re-validate the target object.
                let base = unsafe { HeapPtr::from_word(base_w) };
                let base_len = self.packed_str_len(base); // asserts the backing is a packed Str
                let off = self.read_raw(s, 1) as usize;
                let len = self.read_raw(s, 2) as usize;
                assert!(
                    off <= base_len && len <= base_len - off,
                    "StrSlice range [{off}, {off}+{len}) exceeds its backing (len {base_len})"
                );
                let cp_raw = self.read_raw(s, 3);
                let cp = if cp_raw == CP_UNKNOWN {
                    None
                } else {
                    assert!(
                        cp_raw as usize <= len,
                        "StrSlice cp_len {cp_raw} exceeds its byte length {len} (corrupt slice)"
                    );
                    Some(cp_raw as usize)
                };
                StrView { base, off, len, cp }
            }
            k => panic!("string view of a non-string object (kind {k:?})"),
        }
    }

    /// The logical byte length of a string value (`Str` or `StrSlice`).
    #[inline]
    pub fn str_len(&self, s: HeapPtr) -> usize {
        self.str_view(s).len
    }

    /// The raw IEEE-754 bit pattern of a boxed `Number` — the read side of `pv_number_bits` (ADR-0073 §2),
    /// so a `.c` foreign can format a `Number` without knowing the encoding. **Release-checked**: asserts
    /// the object is a `NumberBox`, so a non-`Number` word cannot be mis-read as a float bit pattern.
    #[inline]
    pub fn number_bits(&self, n: HeapPtr) -> u64 {
        let hdr = self.header(n); // checked: validates `n` is a live object header
        assert_eq!(
            hdr.kind(),
            Kind::NumberBox,
            "number_bits on a non-Number object"
        );
        self.read_raw(n, 0)
    }

    /// A `Closure`'s captured `env` word — the read side of `pv_closure_env` (ADR-0073 §2's
    /// grow-on-demand accessor policy, prompted by ADR-0078): an effect-thunk foreign reaches its
    /// captures through this, so the closure layout (`code`/`arity` raw words, env at value slot 2)
    /// stays the runtime's. **Release-checked**: asserts the object is a `Closure`.
    #[inline]
    pub fn closure_env(&self, c: HeapPtr) -> TaggedWord {
        let hdr = self.header(c); // checked: validates `c` is a live object header
        assert_eq!(
            hdr.kind(),
            Kind::Closure,
            "closure_env on a non-Closure object"
        );
        self.read_field(c, 2)
    }

    /// An `Array`'s element count — the read side of `pv_array_len` (ADR-0073 §2's grow-on-demand
    /// accessor policy, prompted by ADR-0078's `Vec` conversions). **Release-checked**: asserts the
    /// object is an `Array` (the empty-array immediate sentinel is handled at the ABI layer).
    #[inline]
    pub fn array_len(&self, a: HeapPtr) -> u64 {
        let hdr = self.header(a); // checked: validates `a` is a live object header
        assert_eq!(hdr.kind(), Kind::Array, "array_len on a non-Array object");
        hdr.size_words()
    }

    /// Copy a string value's bytes out to an owned `String` (`Str` or `StrSlice`). Copies (never
    /// hands a borrow into the moving heap outward, ADR-0063 §2); valid UTF-8 by the `new_str`
    /// invariant plus the slice builders' boundary validation (ADR-0103 §1).
    pub fn str_read(&self, s: HeapPtr) -> String {
        let v = self.str_view(s); // release-checked: kind, range, backing
                                  // SAFETY: `str_view` bounds `[off, off+len)` within the backing's validated byte region.
        let bytes = unsafe { core::slice::from_raw_parts(self.view_ptr(&v), v.len) };
        String::from_utf8(bytes.to_vec()).expect("Str invariant: valid UTF-8")
    }

    /// The first byte's address of a view — a **borrow into the moving heap**: the caller must not
    /// allocate while any pointer/slice derived from it is live (ADR-0063 §2), and must re-derive
    /// the view after any safepoint. `pub(crate)`, island-internal by design.
    #[inline]
    pub(crate) fn view_ptr(&self, v: &StrView) -> *const u8 {
        // SAFETY (of the arithmetic): `str_view` validated `off + len` within the backing's bytes,
        // which start at payload word 1 (`2 * WORD` into the object).
        unsafe { (v.base.as_ptr() as *const u8).add(2 * WORD + v.off) }
    }

    /// A fresh **zero-filled** `len`-byte `Str` — `Purvasm.String.unsafeNew` (ADR-0052). The linear
    /// byte-build protocol allocates the buffer here and fills it with [`str_byte_set`], so — unlike
    /// [`new_str`] — the UTF-8 invariant is **not** asserted at allocation (zeros are valid anyway; an
    /// intermediate fill may be transiently invalid, but the protocol reads only the completed string).
    /// No `Value` input, so nothing to root across the allocation.
    pub(crate) fn new_str_uninit(&mut self, len: usize) -> HeapPtr {
        let byte_words = len.div_ceil(WORD);
        let p = self.alloc(Kind::Str, 1 + byte_words as u64, Color::White);
        self.write_raw_unchecked(p, 0, len as u64);
        for i in 0..byte_words {
            self.write_raw_unchecked(p, 1 + i as u64, 0);
        }
        p
    }

    /// Byte `i` of a string value (`Str` or `StrSlice`, view-relative) — `Purvasm.String.byteAt`.
    /// Release-checked (reachable from the safe leaf API): [`str_view`](Self::str_view) validates
    /// the object and its range, and `i` is bounds-checked against the view length.
    #[inline]
    pub(crate) fn str_byte_get(&self, s: HeapPtr, i: usize) -> u8 {
        let v = self.str_view(s);
        assert!(
            i < v.len,
            "byteAt: index {i} out of bounds (Str length {len})",
            len = v.len
        );
        // SAFETY: byte `i < v.len` lies in the view's validated byte range.
        unsafe { *self.view_ptr(&v).add(i) }
    }

    /// Write byte `i` of a `Str` in place — `Purvasm.String.unsafeSetByte` (ADR-0052 linear build).
    /// Release-checked bounds. No **write barrier**: a `Str` holds no GC-tracked pointers (its payload is
    /// raw bytes, ADR-0064 §4), so an in-place byte write is invisible to the collector. No UTF-8 re-check
    /// (an intermediate build may be transiently invalid); soundness rests on the linear-build protocol
    /// (the buffer comes from [`new_str_uninit`], is threaded without aliasing, and is read only once
    /// complete — the same discipline as the `boot` reference, ADR-0052).
    #[inline]
    pub(crate) fn str_byte_set(&mut self, s: HeapPtr, i: usize, b: u8) {
        // ADR-0103 §1: in-place byte writes stay confined to freshly built packed `Str`s — a
        // `StrSlice` (or any other kind) is rejected before the length read.
        let len = self.packed_str_len(s);
        assert!(
            i < len,
            "unsafeSetByte: index {i} out of bounds (Str length {len})"
        );
        // SAFETY: as [`str_byte_get`]; `&mut self` is exclusive, and the byte carries no provenance the
        // collector tracks, so no barrier is required.
        unsafe {
            *(s.as_ptr() as *mut u8).add(2 * WORD + i) = b;
        }
    }

    // --- StrSlice views and the bulk string operations (ADR-0103) -----------

    /// The bytes of a view, as a slice whose lifetime ties to `&self` — allocation needs
    /// `&mut self`, so the borrow checker itself enforces "no allocation while borrowed"
    /// (ADR-0063 §2). The remaining discipline is the caller's: a `StrView` is `Copy`, so never
    /// reuse one derived *before* an allocation — re-derive via [`str_view`](Self::str_view).
    #[inline]
    pub(crate) fn view_bytes<'a>(&'a self, v: &StrView) -> &'a [u8] {
        // SAFETY: `str_view` bounded `[off, off+len)` within the backing's validated byte region.
        unsafe { core::slice::from_raw_parts(self.view_ptr(v), v.len) }
    }

    /// Interpret an inbound string *value word* (the ABI boundary trust model: the tag bit is
    /// taken on faith, shape/liveness release-validated at the deref — `lib.rs`' pointer-word
    /// invariant).
    #[inline]
    fn str_ptr(w: TaggedWord) -> HeapPtr {
        assert!(w.is_pointer(), "string operation on a non-pointer word");
        // SAFETY: a pointer-tagged word; every consumer re-validates via `header`/`str_view`.
        unsafe { HeapPtr::from_word(w) }
    }

    /// Build a string value over the **view-relative** byte range `[off, off+len)` of `s` — the
    /// ADR-0103 §1 bounded-retention rule, in one place for every slice builder:
    ///
    ///   * `len = 0` → the empty packed `Str`;
    ///   * the full backing → the backing itself (no wrapper; a full-range slice of a slice
    ///     re-bases the same way);
    ///   * `B <= 4·len` (B = the packed backing's byte length) → a `StrSlice`;
    ///   * otherwise → materialise a packed `Str`.
    ///
    /// `cp` is the result's code-point count when the caller *genuinely* knows it (ADR-0103 §2's
    /// per-operation rules); `None` stores the `CP_UNKNOWN` sentinel. Constructing either result
    /// allocates, so the backing is **rooted across the allocation and re-read** before it is
    /// stored/copied (the ADR-0066 §3 self-rooting-constructor obligation — a pre-allocation
    /// `HeapPtr` must never survive the `alloc`).
    pub(crate) fn build_str_view(
        &mut self,
        s: TaggedWord,
        off: usize,
        len: usize,
        cp: Option<usize>,
    ) -> TaggedWord {
        let sv = self.str_view(Self::str_ptr(s));
        assert!(
            off <= sv.len && len <= sv.len - off,
            "string slice range [{off}, {off}+{len}) exceeds the value's length {vlen}",
            vlen = sv.len
        );
        let abs_off = sv.off + off;
        let base_len = self.packed_str_len(sv.base);
        if len == 0 {
            return self.new_str(b"").as_word();
        }
        if abs_off == 0 && len == base_len {
            return sv.base.as_word();
        }
        let frame = self.frame();
        let bslot = frame.len;
        self.root(sv.base.as_word());
        if base_len <= 4 * len {
            let p = self.alloc(Kind::StrSlice, 4, Color::White);
            let b = self.root_slot(bslot);
            self.write_field_unchecked(p, 0, b);
            self.write_raw_unchecked(p, 1, abs_off as u64);
            self.write_raw_unchecked(p, 2, len as u64);
            self.write_raw_unchecked(p, 3, cp.map_or(CP_UNKNOWN, |c| c as u64));
            self.pop_frame(frame);
            p.as_word()
        } else {
            let p = self.new_str_uninit(len);
            let b = self.root_slot(bslot);
            // SAFETY: `b` is the *current* (post-allocation) backing pointer; `abs_off + len` was
            // validated within its bytes, and `p` was just allocated with `len` bytes' capacity.
            // The regions are distinct objects, so non-overlapping.
            unsafe {
                let src = (HeapPtr::from_word(b).as_ptr() as *const u8).add(2 * WORD + abs_off);
                let dst = (p.as_ptr() as *mut u8).add(2 * WORD);
                core::ptr::copy_nonoverlapping(src, dst, len);
            }
            self.pop_frame(frame);
            p.as_word()
        }
    }

    /// `Purvasm.String.byteSlice from to s` — the `[from, to)` **view-relative** byte range,
    /// release-validated (ADR-0103 §1/§5): in range, no wraparound (the signed inputs are checked
    /// non-negative before widening), and both endpoints on UTF-8 boundaries, extending `new_str`'s
    /// valid-UTF-8 ownership to every view. The result's code-point count is unknown (§2).
    pub fn str_slice_bytes(&mut self, s: TaggedWord, from: i64, to: i64) -> TaggedWord {
        let v = self.str_view(Self::str_ptr(s));
        assert!(
            0 <= from && from <= to && (to as usize) <= v.len,
            "byteSlice: range [{from}, {to}) invalid for a string of length {len}",
            len = v.len
        );
        let (f, t) = (from as usize, to as usize);
        {
            let bytes = self.view_bytes(&v);
            assert!(
                utf8_is_boundary(bytes, f) && utf8_is_boundary(bytes, t),
                "byteSlice: endpoint not on a UTF-8 code-point boundary"
            );
        }
        self.build_str_view(s, f, t - f, None)
    }

    /// `Purvasm.String.dropCodePoints k s` — drop the first `k` code points (`k` clamped to
    /// `[0, count]`). The result's `cp_len` follows ADR-0103 §2's rule: parent count known ⇒
    /// `parent − consumed`; traversal reached the end ⇒ `0`; otherwise unknown.
    pub fn str_drop_cp(&mut self, s: TaggedWord, k: i64) -> TaggedWord {
        let v = self.str_view(Self::str_ptr(s));
        let w = cp_walk(self.view_bytes(&v), k.max(0) as usize);
        let cp = if let Some(n) = v.cp {
            Some(n - w.consumed) // consumed <= n: the walk stops at the end, and cp_len is validated
        } else if w.off == v.len {
            Some(0)
        } else {
            None
        };
        self.build_str_view(s, w.off, v.len - w.off, cp)
    }

    /// `Purvasm.String.takeCodePoints k s` — keep the first `k` code points (`k` clamped). The
    /// retained count is what the walk actually consumed, so the result's `cp_len` is always known.
    pub fn str_take_cp(&mut self, s: TaggedWord, k: i64) -> TaggedWord {
        let v = self.str_view(Self::str_ptr(s));
        let w = cp_walk(self.view_bytes(&v), k.max(0) as usize);
        self.build_str_view(s, 0, w.off, Some(w.consumed))
    }

    /// `Purvasm.String.codePointLength s` — ADR-0103 §2's contract: O(1) when known or memoised;
    /// the first demand on an `unknown` slice counts **the slice's own bytes** and memoises into
    /// the raw `cp_len` word (a v1-only idempotent write into an immutable object — the §2 sharing
    /// precondition guards v2); a bare `Str` count is a walk, never memoised.
    pub fn str_cp_length(&mut self, s: TaggedWord) -> usize {
        let p = Self::str_ptr(s);
        let v = self.str_view(p);
        if let Some(n) = v.cp {
            return n;
        }
        let n = cp_walk(self.view_bytes(&v), usize::MAX).consumed;
        if self.header(p).kind() == Kind::StrSlice {
            self.write_raw_unchecked(p, 3, n as u64);
        }
        n
    }

    /// `Purvasm.String.codePointAt i s` — the `i`-th code point, or `-1` out of range.
    pub fn str_cp_at(&self, s: TaggedWord, i: i64) -> i64 {
        if i < 0 {
            return -1;
        }
        let v = self.str_view(Self::str_ptr(s));
        let bytes = self.view_bytes(&v);
        let w = cp_walk(bytes, i as usize);
        if w.consumed < i as usize || w.off >= bytes.len() {
            return -1;
        }
        utf8_decode(bytes, w.off) as i64
    }

    /// `Purvasm.String.byteIndexOf hay needle from` — the first **view-relative** byte offset
    /// `>= from` at which `needle`'s bytes occur in `hay`, or `-1`. Byte semantics mirror the ulib
    /// `Utf8.byteIndexOf` this replaces (an empty needle matches at any in-range `from`); UTF-8 is
    /// self-synchronising, so a valid needle's match always lands on a code-point boundary.
    pub fn str_byte_index_of(&self, hay: TaggedWord, needle: TaggedWord, from: i64) -> i64 {
        let hv = self.str_view(Self::str_ptr(hay));
        let nv = self.str_view(Self::str_ptr(needle));
        let (h, n) = (self.view_bytes(&hv), self.view_bytes(&nv));
        let mut i = from.max(0) as usize;
        while i + n.len() <= h.len() {
            if &h[i..i + n.len()] == n {
                return i as i64;
            }
            i += 1;
        }
        -1
    }

    /// `Purvasm.String.byteLastIndexOf hay needle from` — the last view-relative byte offset
    /// `<= from` at which `needle` occurs, or `-1` (mirrors the ulib `Utf8.byteLastIndexOf`).
    pub fn str_byte_last_index_of(&self, hay: TaggedWord, needle: TaggedWord, from: i64) -> i64 {
        let hv = self.str_view(Self::str_ptr(hay));
        let nv = self.str_view(Self::str_ptr(needle));
        let (h, n) = (self.view_bytes(&hv), self.view_bytes(&nv));
        if n.len() > h.len() {
            return -1;
        }
        let mut i = (from.max(0) as usize).min(h.len() - n.len()) as i64;
        while i >= 0 {
            let u = i as usize;
            if &h[u..u + n.len()] == n {
                return i;
            }
            i -= 1;
        }
        -1
    }

    /// `Purvasm.String.compareBytes a b` — byte-lexicographic order over the two views (`-1`/`0`/
    /// `1`; equals code-point-lexicographic under UTF-8), borrowed in place — no copy-out
    /// (ADR-0103 §4).
    pub fn str_compare(&self, a: TaggedWord, b: TaggedWord) -> i32 {
        let (va, vb) = (
            self.str_view(Self::str_ptr(a)),
            self.str_view(Self::str_ptr(b)),
        );
        match self.view_bytes(&va).cmp(self.view_bytes(&vb)) {
            core::cmp::Ordering::Less => -1,
            core::cmp::Ordering::Equal => 0,
            core::cmp::Ordering::Greater => 1,
        }
    }

    /// Borrowed-byte string equality (the `EqString` primop's core, ADR-0103 §4).
    pub fn str_eq(&self, a: TaggedWord, b: TaggedWord) -> bool {
        let (va, vb) = (
            self.str_view(Self::str_ptr(a)),
            self.str_view(Self::str_ptr(b)),
        );
        self.view_bytes(&va) == self.view_bytes(&vb)
    }

    /// `Purvasm.String.appendBulk a b` (and the `Append` primop's string arm): concatenate into a
    /// fresh packed `Str`, ADR-0103 §4's island discipline — snapshot the lengths, **root both
    /// operands, allocate, re-derive, then copy**; never a borrow held across the allocation.
    pub fn str_append2(&mut self, a: TaggedWord, b: TaggedWord) -> TaggedWord {
        let va = self.str_view(Self::str_ptr(a));
        let vb = self.str_view(Self::str_ptr(b));
        let (la, lb) = (va.len, vb.len);
        let frame = self.frame();
        let base = frame.len;
        self.root(a);
        self.root(b);
        let p = self.new_str_uninit(la + lb);
        let a1 = self.root_slot(base);
        let b1 = self.root_slot(base + 1);
        // Re-derive both views from the reloaded roots (the pre-allocation views are stale).
        let va1 = self.str_view(Self::str_ptr(a1));
        let vb1 = self.str_view(Self::str_ptr(b1));
        // SAFETY: source ranges were validated by `str_view` on the current objects; `p` was just
        // allocated with `la + lb` bytes' capacity; distinct objects, non-overlapping.
        unsafe {
            let dst = (p.as_ptr() as *mut u8).add(2 * WORD);
            core::ptr::copy_nonoverlapping(self.view_ptr(&va1), dst, la);
            core::ptr::copy_nonoverlapping(self.view_ptr(&vb1), dst.add(la), lb);
        }
        self.pop_frame(frame);
        p.as_word()
    }

    /// `Purvasm.String.materialize s` — sever ownership below the §1 threshold: identity on a
    /// packed `Str`, a compacting copy of a `StrSlice`.
    pub fn str_materialize(&mut self, s: TaggedWord) -> TaggedWord {
        let p = Self::str_ptr(s);
        if self.header(p).kind() == Kind::Str {
            return s;
        }
        let v = self.str_view(p);
        let len = v.len;
        let frame = self.frame();
        let bslot = frame.len;
        self.root(v.base.as_word());
        let out = self.new_str_uninit(len);
        let b = self.root_slot(bslot);
        // SAFETY: as `build_str_view`'s materialise arm — current backing, validated range, fresh
        // destination.
        unsafe {
            let src = (HeapPtr::from_word(b).as_ptr() as *const u8).add(2 * WORD + v.off);
            core::ptr::copy_nonoverlapping(src, (out.as_ptr() as *mut u8).add(2 * WORD), len);
        }
        self.pop_frame(frame);
        out.as_word()
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
    pub(crate) fn new_byneed_placeholder(&mut self) -> HeapPtr {
        let p = self.alloc(Kind::ByNeed, 2, Color::White);
        self.write_raw_unchecked(p, 0, BYNEED_UNFORCED);
        self.write_field_unchecked(p, 1, TaggedWord::unit());
        p
    }

    /// Backpatch a placeholder cell's suspension (`Grec` builder only; the cell must still be
    /// `Unforced`). A plain value-slot store — it does **not** force the suspension (ADR-0070 §4).
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
    /// in-bounds word. An *interior* payload word — which a stale pointer can come to alias after a
    /// couple of collect/swap cycles — is rejected ([P1]): without this, a safe caller could read a
    /// payload word as a header (or, in `apply`, transmute-and-call a payload-derived word) —
    /// reachable UB that an in-bounds-only check would let through. Aliasing another *header* start
    /// remains an accepted logic bug (a real live object, memory-safe). O(1): a bit test against
    /// [`starts_bits`](Heap::starts_bits) — the earlier full record-walk made every dereference
    /// O(objects) and the compiled self-compiler quadratic-plus.
    #[inline]
    fn is_object_start(&self, p: HeapPtr) -> bool {
        let base = self.space.as_ptr() as usize;
        let addr = p.as_ptr() as usize;
        if addr < base || addr >= base + self.top * WORD || !(addr - base).is_multiple_of(WORD) {
            return false;
        }
        let target = (addr - base) / WORD;
        self.starts_bits[target >> 6] & (1u64 << (target & 63)) != 0
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
        // ADR-0102 §3: clocks are read only around a collection, and only when stats are enabled —
        // a disabled heap performs zero `Instant` calls here. `Instant::now` itself is the entire
        // per-collection cost; `elapsed()` below is read after the whole invocation (collection, swap,
        // and the object-start rebuild), matching "total GC time" as the ADR defines it.
        let start = self.stats.is_some().then(Instant::now);
        // A transient `&mut [TaggedWord]` view of the shadow-stack region for the collector
        // (`TaggedWord` is `repr(transparent)` over `u64`). Raw-parent -> temporary unique view ->
        // back to raw-parent is the sanctioned borrow shape: the view dies before any further
        // raw-pointer access to the region.
        // SAFETY: `roots_base[0..roots_len]` is initialised and in-bounds; no other view exists.
        let roots_view = unsafe {
            core::slice::from_raw_parts_mut(
                self.header.roots_base as *mut TaggedWord,
                self.roots_len(),
            )
        };
        self.top = collect_core(self.space, self.reserve, self.top, roots_view);
        // Swap the two stable base pointers (a `Copy` of the raw handles — no buffer re-borrow). The
        // shadow-stack slots were rewritten in place by `collect_core`; `root_gens` is unaffected
        // (same slots, new values).
        core::mem::swap(&mut self.space, &mut self.reserve);
        self.rebuild_starts();
        if let Some(start) = start {
            // ADR-0102 §3 / `Stats`' doc comment: every counter is unsigned *saturating* — a GC pause
            // long enough to overflow `u64` nanoseconds (~584 years) is not realistic, but `as` would
            // silently wrap rather than saturate, breaking that contract. `try_from` + `unwrap_or(MAX)`
            // saturates instead.
            let ns = u64::try_from(start.elapsed().as_nanos()).unwrap_or(u64::MAX);
            let live_words = self.top as u64;
            if let Some(s) = self.stats.as_mut() {
                s.record_gc(ns, live_words);
            }
        }
    }

    /// Rebuild [`starts_bits`](Heap::starts_bits) for the (post-swap) active space: zero the bitmap,
    /// then stride the gap-free `[Header][payload; size]` records `0..top` marking each start. O(top)
    /// once per collection — the amortised price of the O(1) per-dereference check.
    fn rebuild_starts(&mut self) {
        self.starts_bits.fill(0);
        let mut off = 0usize;
        while off < self.top {
            self.starts_bits[off >> 6] |= 1u64 << (off & 63);
            // SAFETY: `off < top <= cap`; read the freshly-evacuated header via the stable base.
            let hdr = Header::from_bits(unsafe { *self.space.as_ptr().add(off) });
            off += 1 + hdr.size_words() as usize;
        }
    }

    /// Collect with an **explicit** root slice — **test-only** (`#[cfg(test)]`, ADR-0066 §4). This is
    /// *not* a public alternate root path: the runtime always collects via [`gc`](Heap::gc) rooted by
    /// the shadow stack. A unit test using this form supplies the *complete* root set and keeps the
    /// shadow stack empty, so the two root sources never coexist and diverge. `roots` are rewritten in
    /// place.
    #[cfg(test)]
    pub(crate) fn collect(&mut self, roots: &mut [TaggedWord]) {
        debug_assert!(
            self.roots_len() == 0,
            "explicit-slice collect must not run with a non-empty shadow stack (ADR-0066 §4)"
        );
        self.top = collect_core(self.space, self.reserve, self.top, roots);
        core::mem::swap(&mut self.space, &mut self.reserve);
        self.rebuild_starts();
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
        // SAFETY: `space`/`reserve`/`roots_base` each came from `Box::into_raw` of a boxed `[u64]`
        // slice; rebuild the exact-length boxed slice to run its deallocation. Done once, on drop.
        unsafe {
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.space.as_ptr(),
                self.cap,
            )));
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.reserve.as_ptr(),
                self.cap,
            )));
            drop(Box::from_raw(core::ptr::slice_from_raw_parts_mut(
                self.header.roots_base,
                self.header.roots_cap as usize,
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

/// The byte length of the UTF-8 sequence starting with `b` (valid input by the `Str` invariant).
#[inline]
fn utf8_seq_len(b: u8) -> usize {
    match b {
        0x00..=0x7F => 1,
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        _ => 4,
    }
}

/// Is byte offset `i` a code-point boundary of valid UTF-8 (`i == len` counts as one)?
#[inline]
fn utf8_is_boundary(bytes: &[u8], i: usize) -> bool {
    i == bytes.len() || (bytes[i] & 0xC0) != 0x80
}

/// Decode the code point starting at boundary `off` (valid UTF-8 by the `Str` invariant).
fn utf8_decode(bytes: &[u8], off: usize) -> u32 {
    let b0 = bytes[off];
    match utf8_seq_len(b0) {
        1 => b0 as u32,
        2 => ((b0 as u32 & 0x1F) << 6) | (bytes[off + 1] as u32 & 0x3F),
        3 => {
            ((b0 as u32 & 0x0F) << 12)
                | ((bytes[off + 1] as u32 & 0x3F) << 6)
                | (bytes[off + 2] as u32 & 0x3F)
        }
        _ => {
            ((b0 as u32 & 0x07) << 18)
                | ((bytes[off + 1] as u32 & 0x3F) << 12)
                | ((bytes[off + 2] as u32 & 0x3F) << 6)
                | (bytes[off + 3] as u32 & 0x3F)
        }
    }
}

/// The result of a bounded code-point walk: what was actually consumed, and where it stopped.
struct CpWalk {
    consumed: usize,
    off: usize,
}

/// Walk `bytes` from the front over at most `k` code points (UTF-8-valid by the `Str` invariant).
fn cp_walk(bytes: &[u8], k: usize) -> CpWalk {
    let mut consumed = 0usize;
    let mut off = 0usize;
    while consumed < k && off < bytes.len() {
        off += utf8_seq_len(bytes[off]);
        consumed += 1;
    }
    CpWalk { consumed, off }
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
        // [base: value → Str] [byte_off: raw] [byte_len: raw] [cp_len: raw] — the view keeps its
        // packed backing alive through the one traced slot (ADR-0103 §1).
        Kind::StrSlice => [(0, 1), (0, 0)],
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
    fn gc_stats_count_real_overflow_collections_not_test_only_collect() {
        // ADR-0102 §3 Verification: a tiny-heap fixture that forces real `alloc`-on-overflow
        // collections (not the `#[cfg(test)]`-only explicit-root `collect`, which must stay
        // uninstrumented) and asserts the GC counters' shape rather than exact values.
        let mut h = Heap::new(8);
        h.enable_stats_for_test();
        let frame = h.frame();
        let keep = h.new_number(3.5);
        let kr = h.root(keep.as_word());
        for _ in 0..20 {
            let _ = h.new_number(1.0); // unrooted; overflows the 8-word semi-space repeatedly
        }
        let keep2 = unsafe { HeapPtr::from_word(h.get(kr)) };
        assert_eq!(
            f64::from_bits(h.read_raw(keep2, 0)),
            3.5,
            "the rooted survivor must relocate intact"
        );
        h.pop_frame(frame);

        let s = h.stats().expect("stats were enabled");
        assert!(s.gc_collections >= 1, "expected at least one collection");
        assert!(
            s.gc_max_live_words > 0,
            "a live collection must report nonzero max live words"
        );
        assert!(
            s.gc_copied_words >= s.gc_max_live_words,
            "accumulated copied words must be at least the single-collection max"
        );
        // `gc_total_ns` is a plain `u64` — any value it can hold is valid; no exact-time assertion.
        let _ = s.gc_total_ns;

        // The explicit-root test-only `collect` path must never touch the counters.
        let before = *h.stats().unwrap();
        let mut roots: [TaggedWord; 0] = [];
        h.collect(&mut roots);
        assert_eq!(
            *h.stats().unwrap(),
            before,
            "test-only `collect` must not bump GC counters"
        );
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
    #[should_panic(expected = "non-string")]
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

    #[test]
    fn str_byte_build_and_read() {
        // `new_str_uninit` allocates a zero-filled buffer; `str_byte_set` fills it; `str_byte_get`/
        // `str_len` read it back; `str_read` sees the completed UTF-8 (ADR-0052 linear build).
        let mut h = Heap::new(64);
        let s = h.new_str_uninit(3);
        assert_eq!(h.str_len(s), 3);
        assert_eq!(h.str_byte_get(s, 0), 0); // zero-filled
        h.str_byte_set(s, 0, b'a');
        h.str_byte_set(s, 1, b'b');
        h.str_byte_set(s, 2, b'c');
        assert_eq!(h.str_byte_get(s, 1), b'b');
        assert_eq!(h.str_read(s), "abc");
        // a multi-byte word boundary: 9 bytes span two payload words
        let t = h.new_str_uninit(9);
        for i in 0..9 {
            h.str_byte_set(t, i, b'0' + i as u8);
        }
        assert_eq!(h.str_read(t), "012345678");
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn str_byte_set_rejects_out_of_bounds() {
        let mut h = Heap::new(16);
        let s = h.new_str_uninit(2);
        h.str_byte_set(s, 2, b'x'); // index == len → out of bounds
    }

    // --- StrSlice (ADR-0103) --------------------------------------------------------------------

    #[test]
    fn byte_slice_builds_a_view_and_reads_through_it() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"hello, world").as_word();
        // B = 12, L = 5: 12 <= 4*5 → a StrSlice view.
        let w = h.str_slice_bytes(s, 7, 12);
        let p = unsafe { HeapPtr::from_word(w) };
        assert_eq!(h.header(p).kind(), Kind::StrSlice);
        assert_eq!(h.str_read(p), "world");
        assert_eq!(h.str_len(p), 5);
        assert_eq!(h.str_byte_get(p, 0), b'w');
    }

    #[test]
    fn bounded_retention_materialises_a_tiny_extraction() {
        let mut h = Heap::new(128);
        let s = h.new_str("x".repeat(100).as_bytes()).as_word();
        // B = 100 > 4·10 → a packed copy, not a view.
        let w = h.str_slice_bytes(s, 0, 10);
        let p = unsafe { HeapPtr::from_word(w) };
        assert_eq!(h.header(p).kind(), Kind::Str);
        assert_eq!(h.str_read(p), "x".repeat(10));
    }

    #[test]
    fn empty_and_full_range_take_the_rule_exits() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"abcdef").as_word();
        let empty = h.str_slice_bytes(s, 3, 3);
        assert_eq!(h.str_read(unsafe { HeapPtr::from_word(empty) }), "");
        assert_eq!(
            h.header(unsafe { HeapPtr::from_word(empty) }).kind(),
            Kind::Str
        );
        // the full range is the backing itself — no wrapper.
        let full = h.str_slice_bytes(s, 0, 6);
        assert_eq!(full.to_bits(), s.to_bits());
    }

    #[test]
    fn slicing_a_slice_rebases_onto_the_packed_backing() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"hello, world").as_word();
        let outer = h.str_slice_bytes(s, 7, 12); // "world" (a view)
        let inner = h.str_slice_bytes(outer, 1, 4); // "orl" — B=12 <= 4*3 → still a view
        let ip = unsafe { HeapPtr::from_word(inner) };
        assert_eq!(h.header(ip).kind(), Kind::StrSlice);
        // never nested: the inner view's base is the packed Str, not the outer slice.
        let base = unsafe { HeapPtr::from_word(h.read_field(ip, 0)) };
        assert_eq!(h.header(base).kind(), Kind::Str);
        assert_eq!(h.str_read(ip), "orl");
    }

    #[test]
    fn drop_take_clamp_and_derive_cp_len() {
        let mut h = Heap::new(64);
        let s = h.new_str("aβ🐱d".as_bytes()).as_word(); // 1+2+4+1 = 8 bytes, 4 code points
        let dropped = h.str_drop_cp(s, 2); // "🐱d" — 5 bytes; parent count unknown, end not reached
        let dp = unsafe { HeapPtr::from_word(dropped) };
        assert_eq!(h.str_read(dp), "🐱d");
        // unknown at build; the first demand counts the slice's own bytes and memoises.
        assert_eq!(h.read_raw(dp, 3), CP_UNKNOWN);
        assert_eq!(h.str_cp_length(dropped), 2);
        assert_eq!(h.read_raw(dp, 3), 2);
        // take records the retained count as its result cp_len.
        let taken = h.str_take_cp(s, 2); // "aβ" — 3 bytes; B=8 <= 12 → view with cp_len = 2
        let tp = unsafe { HeapPtr::from_word(taken) };
        assert_eq!(h.str_read(tp), "aβ");
        assert_eq!(h.read_raw(tp, 3), 2);
        // clamping: dropping everything (or more) is the empty packed Str; a negative k is 0.
        let all = h.str_drop_cp(s, 99);
        assert_eq!(h.str_read(unsafe { HeapPtr::from_word(all) }), "");
        let none = h.str_drop_cp(s, -1);
        assert_eq!(none.to_bits(), s.to_bits()); // full range → the backing itself
    }

    #[test]
    fn drop_on_a_known_slice_derives_parent_minus_consumed() {
        let mut h = Heap::new(64);
        let s = h.new_str("aβ🐱d".as_bytes()).as_word();
        let taken = h.str_take_cp(s, 3); // cp_len = Some(3)
        let dropped = h.str_drop_cp(taken, 1); // known parent: 3 − 1 = 2, no walk of the result
        let dp = unsafe { HeapPtr::from_word(dropped) };
        assert_eq!(h.read_raw(dp, 3), 2);
        assert_eq!(h.str_read(dp), "β🐱");
    }

    #[test]
    fn cp_at_decodes_and_signals_out_of_range() {
        let mut h = Heap::new(64);
        let s = h.new_str("aβ🐱d".as_bytes()).as_word();
        assert_eq!(h.str_cp_at(s, 0), 'a' as i64);
        assert_eq!(h.str_cp_at(s, 1), 'β' as i64);
        assert_eq!(h.str_cp_at(s, 2), '🐱' as i64);
        assert_eq!(h.str_cp_at(s, 3), 'd' as i64);
        assert_eq!(h.str_cp_at(s, 4), -1);
        assert_eq!(h.str_cp_at(s, -1), -1);
    }

    #[test]
    #[should_panic(expected = "byteSlice: range")]
    fn byte_slice_rejects_an_out_of_range_end() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"abc").as_word();
        let _ = h.str_slice_bytes(s, 0, 4);
    }

    #[test]
    #[should_panic(expected = "UTF-8 code-point boundary")]
    fn byte_slice_rejects_a_mid_code_point_boundary() {
        let mut h = Heap::new(64);
        let s = h.new_str("aβc".as_bytes()).as_word();
        let _ = h.str_slice_bytes(s, 0, 2); // splits β
    }

    #[test]
    #[should_panic(expected = "str_len on a non-Str object")]
    fn unsafe_set_byte_rejects_a_slice() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"hello, world").as_word();
        let w = h.str_slice_bytes(s, 7, 12);
        h.str_byte_set(unsafe { HeapPtr::from_word(w) }, 0, b'!');
    }

    #[test]
    fn borrowed_ops_accept_the_str_by_slice_matrix() {
        let mut h = Heap::new(128);
        let packed = h.new_str(b"world").as_word();
        let big = h.new_str(b"hello, world").as_word();
        let sliced = h.str_slice_bytes(big, 7, 12); // "world" as a view
        assert!(h.str_eq(packed, sliced));
        assert!(h.str_eq(sliced, packed));
        assert_eq!(h.str_compare(sliced, packed), 0);
        let smaller = h.new_str(b"worl").as_word();
        assert_eq!(h.str_compare(smaller, sliced), -1);
        assert_eq!(h.str_compare(sliced, smaller), 1);
        // search over a view, with a view needle.
        let hay = h.str_slice_bytes(big, 0, 12);
        let needle = h.str_slice_bytes(big, 4, 8); // "o, w"
        assert_eq!(h.str_byte_index_of(hay, needle, 0), 4);
        assert_eq!(h.str_byte_index_of(hay, needle, 5), -1);
        assert_eq!(h.str_byte_last_index_of(hay, packed, 11), 7);
        // record hashing borrows both kinds identically.
        assert_eq!(h.str_label_id(packed), h.str_label_id(sliced));
    }

    #[test]
    fn append_concatenates_across_the_kind_matrix() {
        // All four operand-kind combinations: Packed×Packed, Packed×Slice, Slice×Packed,
        // Slice×Slice — the result is always a fresh packed Str.
        let mut h = Heap::new(256);
        let big = h.new_str(b"hello, world").as_word();
        let hello_p = h.new_str(b"hello").as_word();
        let world_p = h.new_str(b"world").as_word();
        let hello_s = h.str_slice_bytes(big, 0, 5);
        let world_s = h.str_slice_bytes(big, 7, 12);
        for (a, b) in [
            (hello_p, world_p),
            (hello_p, world_s),
            (hello_s, world_p),
            (hello_s, world_s),
        ] {
            let joined = h.str_append2(a, b);
            let jp = unsafe { HeapPtr::from_word(joined) };
            assert_eq!(h.str_read(jp), "helloworld");
            assert_eq!(h.header(jp).kind(), Kind::Str);
        }
    }

    #[test]
    fn materialize_severs_ownership_and_is_identity_on_packed() {
        let mut h = Heap::new(64);
        let s = h.new_str(b"hello, world").as_word();
        assert_eq!(h.str_materialize(s).to_bits(), s.to_bits());
        let view = h.str_slice_bytes(s, 7, 12);
        let owned = h.str_materialize(view);
        let op = unsafe { HeapPtr::from_word(owned) };
        assert_eq!(h.header(op).kind(), Kind::Str);
        assert_eq!(h.str_read(op), "world");
    }

    #[test]
    fn a_live_slice_survives_the_backing_moving() {
        // forced-GC fixture 1 (ADR-0103 §Verification): a collection moves the base under an
        // already-live StrSlice; the view's traced base slot must follow it.
        let mut h = Heap::new(64);
        let s = h.new_str(b"hello, world").as_word();
        let view = h.str_slice_bytes(s, 7, 12);
        let mut roots = [view];
        h.collect(&mut roots);
        let vp = unsafe { HeapPtr::from_word(roots[0]) };
        assert_eq!(h.str_read(vp), "world");
        assert_eq!(h.str_cp_length(roots[0]), 5);
    }

    #[test]
    fn slice_construction_survives_its_own_collection() {
        // forced-GC fixture 2 (ADR-0103 §1's rooting obligation): the slice construction itself
        // fires the collection that moves the input backing, between normalisation and the
        // base-slot store. Fill the heap so the 5-word StrSlice allocation overflows: backing
        // 4 words + junk 32 words = 36 occupied words (the junk is unrooted), and 36 + 5 > 40.
        // The stats counter pins that the overflow collection actually ran (an earlier sizing
        // left 36 <= 40 and never collected).
        let mut h = Heap::new(40);
        h.enable_stats_for_test();
        let s = h.new_str(b"0123456789abcdef").as_word(); // 1 + 1 + 2 words
        let junk = h.new_str("y".repeat(240).as_bytes()); // 1 + 1 + 30 words, unrooted garbage
        let _ = junk;
        // keep `s` reachable through the shadow stack while the builder allocates.
        let sr = h.root(s);
        let cur = h.get(sr);
        let view = h.str_slice_bytes(cur, 6, 16);
        assert_eq!(
            h.stats().expect("stats were enabled").gc_collections,
            1,
            "the slice construction must have fired exactly one overflow collection"
        );
        assert_eq!(
            h.str_read(unsafe { HeapPtr::from_word(view) }),
            "6789abcdef"
        );
        // the backing moved under the builder; the re-rooted slot must still reach it.
        assert_eq!(
            h.str_read(unsafe { HeapPtr::from_word(h.get(sr)) }),
            "0123456789abcdef"
        );
    }

    #[test]
    fn gc_stress_collects_at_every_alloc_and_rooted_values_survive() {
        // ADR-0105 §5 slice 0: under stress, EVERY alloc entry collects — no overflow needed —
        // so a missing-root window deterministically contains a collection. The safe constructors'
        // self-rooting must keep their inputs correct through the per-alloc collections, and the
        // copied-words counter must show live relocation actually happened (the stress leg's
        // vacuity guard, `gc_copied_words > 0`).
        let mut h = Heap::new(4096);
        h.enable_stats_for_test();
        h.enable_gc_stress_for_test();
        let s = h.new_str(b"stress-survivor").as_word(); // allocs (collects) then roots internally
        let sr = h.root(s);
        // Each new_adt: roots its fields, then its alloc collects — the just-rooted `sr` string
        // and every prior ADT must relocate correctly on every round.
        let mut prev = h.get(sr);
        for i in 0..8u32 {
            let p = h.new_adt(i, &[prev, h.get(sr)]);
            prev = p.as_word();
            let pr = h.root(prev);
            prev = h.get(pr);
        }
        let stats = h.stats().expect("stats were enabled");
        assert!(
            stats.gc_collections >= 9,
            "stress mode must collect at every alloc (got {} collections)",
            stats.gc_collections
        );
        assert!(
            stats.gc_copied_words > 0,
            "stressed collections with a live set must have copied words"
        );
        // the string survived every collection and reads back intact through its slot.
        assert_eq!(
            h.str_read(unsafe { HeapPtr::from_word(h.get(sr)) }),
            "stress-survivor"
        );
        // the ADT chain survived: walk it back down checking tags.
        let mut cur = prev;
        for i in (0..8u32).rev() {
            let p = unsafe { HeapPtr::from_word(cur) };
            assert_eq!(h.read_raw(p, 0), i as u64, "tag at chain depth {i}");
            cur = h.read_field(p, 1);
        }
    }
}
