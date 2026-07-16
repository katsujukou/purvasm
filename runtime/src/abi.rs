//! The codegen↔runtime **`extern "C"` boundary** (ADR-0071).
//!
//! LLVM-generated code (ADR-0072) links the runtime `staticlib` and calls it across this surface. Every
//! guest value crosses as a raw `u64` — a [`TaggedWord`] bit pattern (ADR-0071 §1) — and the runtime
//! context as an opaque `*mut Heap` (ADR-0071 §2); never a Rust reference into the heap nor an
//! `addrspace(1)` pointer (ADR-0063 §2). The boundary **trusts codegen for a word's tag but validates
//! liveness/shape on every dereference** (the `checked_*` field tier / `apply`), so a codegen bug is a
//! release fault, not UB (ADR-0071 §1).
//!
//! **Panic containment (ADR-0071 §7 / ADR-0063 §3).** Every entry runs under [`guard`], which turns a
//! Rust `panic!` (a tripped invariant, or OOM — ADR-0066 §4) into an `abort` rather than letting it
//! unwind into LLVM frames (UB). It is `catch_unwind`, not crate-wide `panic = "abort"`, so the `lib`'s
//! `#[should_panic]` tests keep unwinding.
//!
//! This module is the **address path** (ADR-0071 §3): heaps are built with [`Heap::new_native`], so a
//! closure's `code` word is a real `extern "C"` fn address ([`AbiCodeFn`]). It is never run under Miri
//! (which exercises the index path via the `lib` API only, ADR-0063 §4).

use crate::gc::Heap;
use crate::heap::HeapPtr;
use crate::word::TaggedWord;
use std::panic::{catch_unwind, AssertUnwindSafe};

/// Run an FFI-entry body with panic containment (ADR-0071 §7). A caught unwind aborts — no post-panic
/// runtime state is ever observed across the boundary. `AssertUnwindSafe` is sound because we abort
/// (never resume) on `Err`. Shared with [`crate::prim`] (the primop entries).
#[inline]
pub(crate) fn guard<R>(body: impl FnOnce() -> R) -> R {
    match catch_unwind(AssertUnwindSafe(body)) {
        Ok(r) => r,
        Err(_) => std::process::abort(),
    }
}

/// Reborrow the opaque context as `&mut Heap`. `ctx` must be a live `Heap` from [`pv_runtime_new`].
///
/// # Safety
/// `ctx` must be a live [`Heap`] from [`pv_runtime_new`], with no other live borrow of it.
#[inline]
pub(crate) unsafe fn heap<'a>(ctx: *mut Heap) -> &'a mut Heap {
    debug_assert!(!ctx.is_null(), "pv_* called with a null context");
    &mut *ctx
}

/// Rebuild an args slice from a codegen-supplied `(ptr, len)`. A zero-length call may pass a null/dangling
/// pointer, so length 0 yields an empty slice rather than an unsound `from_raw_parts(null, 0)`. Shared
/// with [`crate::leaf`] (the native leaves are ordinary `AbiCodeFn`s).
///
/// # Safety
/// `args`/`nargs` describe a valid buffer of `nargs` value words (or `nargs == 0`).
#[inline]
pub(crate) unsafe fn args_slice<'a>(args: *const u64, nargs: usize) -> &'a [TaggedWord] {
    if nargs == 0 {
        &[]
    } else {
        // SAFETY: codegen passes a valid `(ptr, len)` over `nargs` value words; `TaggedWord` is
        // `#[repr(transparent)]` over `u64`.
        std::slice::from_raw_parts(args as *const TaggedWord, nargs)
    }
}

// --- runtime context (ADR-0071 §2) ------------------------------------------------------------------

/// Create an **address-path** runtime context (ADR-0071 §2/§3) with `local_words` per semi-space.
/// Returned as an opaque `*mut Heap`; free with [`pv_runtime_free`].
#[no_mangle]
pub extern "C" fn pv_runtime_new(local_words: usize) -> *mut Heap {
    guard(|| Box::into_raw(Box::new(Heap::new_native(local_words))))
}

/// Destroy a context from [`pv_runtime_new`], freeing both semi-spaces.
///
/// # Safety
/// `ctx` must be a pointer returned by [`pv_runtime_new`] and not already freed.
#[no_mangle]
pub unsafe extern "C" fn pv_runtime_free(ctx: *mut Heap) {
    guard(|| {
        if !ctx.is_null() {
            // ADR-0102 §3: the one-shot summary line, emitted only when stats are enabled — read
            // before the drop below frees the context. A leaked context (never freed) loses its
            // summary; that is acceptable for this diagnostic slice.
            if let Some(stats) = heap(ctx).stats() {
                eprintln!("{}", stats.format());
            }
            drop(Box::from_raw(ctx));
        }
    })
}

// --- calling convention (ADR-0071 §3/§4) ------------------------------------------------------------

/// Apply callable `f` to `nargs` argument words (ADR-0071 §3): the generic entry all v1 calls route
/// through, running the trampoline (ADR-0071 §4). Returns the result word.
///
/// # Safety
/// `ctx` is a live context; `f` is a value word; `args`/`nargs` describe a valid argument buffer.
#[no_mangle]
pub unsafe extern "C" fn pv_apply(ctx: *mut Heap, f: u64, args: *const u64, nargs: usize) -> u64 {
    guard(|| {
        let h = heap(ctx);
        // ADR-0102 §3: the *only* site that counts `pv_apply_entries` — not every `Heap::apply`
        // activation, since `pv_settle`'s slow path and internal helpers reach `apply` directly.
        if let Some(s) = h.stats_mut() {
            s.pv_apply_entries = s.pv_apply_entries.saturating_add(1);
        }
        let argv = args_slice(args, nargs);
        h.apply(TaggedWord::from_bits(f), argv).to_bits()
    })
}

/// Request a **tail call** (ADR-0071 §4): stash `(f, args)` into the context's pending-tail slot as the
/// calling body's final action, then return; the enclosing [`pv_apply`] loop takes it and bounces. The
/// args are copied into runtime-owned storage here, *before* the body pops its shadow-stack frame, so
/// the body may then `pv_pop_frame` and `ret`. The body's own return word is ignored.
///
/// # Safety
/// As [`pv_apply`]; must be a body's final action (exactly one per body, no intervening `pv_apply`).
#[no_mangle]
pub unsafe extern "C" fn pv_tailcall(ctx: *mut Heap, f: u64, args: *const u64, nargs: usize) {
    guard(|| {
        let h = heap(ctx);
        let owned: Vec<TaggedWord> = args_slice(args, nargs).to_vec();
        h.set_pending_tail(TaggedWord::from_bits(f), owned);
    })
}

/// Resolve a pending trampoline bounce for a **direct** caller (ADR-0076 §3): a directly-entered
/// body's generic tail call stashes `(f, args)` ([`pv_tailcall`]) and returns a dummy — and a direct
/// call has no enclosing [`pv_apply`] loop to take the stash. Every non-`musttail` direct call site
/// therefore settles its result: a stashed tail is run to a real value here (`apply` resolves the
/// whole chain flat, ADR-0071 §4), and a real value passes through untouched. A `musttail` edge
/// propagates the dummy+stash to *its* caller's settle. Wrappers do not settle — under the
/// [`pv_apply`] loop the stash belongs to the loop, exactly as before.
///
/// # Safety
/// As [`pv_apply`].
#[no_mangle]
pub unsafe extern "C" fn pv_settle(ctx: *mut Heap, r: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        match h.take_pending_tail() {
            None => {
                if let Some(s) = h.stats_mut() {
                    s.pv_settle_fast = s.pv_settle_fast.saturating_add(1);
                }
                r
            }
            Some((f, args)) => {
                // ADR-0102 §3: `pending_tail_settle_takes` and `pv_settle_slow` are tautologically
                // identical here — `pv_settle` has exactly one pending-tail take site — but land as
                // two schema fields per the ADR (one narrative bucket is "who took it", the other is
                // "what pv_settle did"). This is an intentional fusion, not a bug to deduplicate.
                if let Some(s) = h.stats_mut() {
                    s.pending_tail_settle_takes = s.pending_tail_settle_takes.saturating_add(1);
                    s.pv_settle_slow = s.pv_settle_slow.saturating_add(1);
                }
                h.apply(f, &args).to_bits()
            }
        }
    })
}

/// Build a [`Closure`](crate::heap::Kind::Closure) whose `code` word is the real `extern "C"` fn address
/// `code_addr` (ADR-0071 §3), with `arity` and captured env word `env` (a shared env-block pointer, or
/// an immediate sentinel for a no-capture closure). Returns the closure value word.
///
/// # Safety
/// `code_addr` must be the address of a real [`AbiCodeFn`]; `env` a valid value word.
#[no_mangle]
pub unsafe extern "C" fn pv_make_closure(
    ctx: *mut Heap,
    code_addr: u64,
    arity: u32,
    env: u64,
) -> u64 {
    guard(|| {
        let h = heap(ctx);
        // SAFETY: the ABI contract is that `code_addr` is a real code address (ADR-0071 §3); `env` is
        // self-rooted across the allocation by `new_closure_raw` (ADR-0066 §3).
        h.new_closure_raw(code_addr, arity, TaggedWord::from_bits(env))
            .as_word()
            .to_bits()
    })
}

// --- effect execution + by-need force (ADR-0071 §6 / ADR-0067 / ADR-0070) ---------------------------

/// Run an `Effect` program: `run_effect(main) = apply(main, unit)` (ADR-0067 §2). Returns the final
/// value (a `Unit` for `Effect Unit`); effects fire in program order via strict `apply`.
///
/// # Safety
/// `ctx` live; `main` an `Effect` thunk (an arity-1 closure).
#[no_mangle]
pub unsafe extern "C" fn pv_run_effect(ctx: *mut Heap, main: u64) -> u64 {
    guard(|| heap(ctx).run_effect(TaggedWord::from_bits(main)).to_bits())
}

/// Force a by-need cell (ADR-0070): `Unforced` → evaluate + memoise, `Forced` → the memoised value,
/// `Building` → a black-hole fault. Codegen emits this at a by-need dereference.
///
/// # Safety
/// `ctx` live; `cell` a `ByNeed` pointer word.
#[no_mangle]
pub unsafe extern "C" fn pv_force(ctx: *mut Heap, cell: u64) -> u64 {
    guard(|| heap(ctx).force(TaggedWord::from_bits(cell)).to_bits())
}

/// Force `v` **iff it is a `ByNeed` cell** (ADR-0070 §3), passing any other value through. Codegen emits
/// this at a value-dereference site to force a by-need cell that reached it through an argument or data
/// field — robustly, without static by-need tracking.
///
/// # Safety
/// `ctx` live; `v` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_force_if_byneed(ctx: *mut Heap, v: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .force_if_byneed(TaggedWord::from_bits(v))
            .to_bits()
    })
}

/// **Drain** the captured stdio sink (ADR-0067 §5) to real `stdout`, one line each, **clearing** it. The
/// compiled entry stub calls this at exit so the process's stdout matches the differential (production
/// wiring of the sink; tests instead read [`Heap::output`]). Draining (not just reading) makes a second
/// call a no-op rather than re-printing. A `stdout` write/flush failure is a fatal boundary fault —
/// `expect` → [`guard`] → abort (ADR-0071 §7), never a silently swallowed error.
///
/// # Safety
/// `ctx` live.
#[no_mangle]
pub unsafe extern "C" fn pv_drain_output(ctx: *mut Heap) {
    guard(|| {
        use std::io::Write;
        let lines = heap(ctx).take_output();
        let out = std::io::stdout();
        let mut lock = out.lock();
        for line in &lines {
            // `writeln!`'s `\n` is the line separator; the differential compares the normalised line
            // sequence, not raw bytes (ADR-0067 §5).
            writeln!(lock, "{line}").expect("pv_drain_output: stdout write failed");
        }
        lock.flush().expect("pv_drain_output: stdout flush failed");
    })
}

/// A `case` with no matching alternative (a partial match falling through every arm, ADR-0072 §5) —
/// a fatal runtime fault, mirroring the oracle's *stuck* "no match". Codegen emits this at a matcher's
/// exhausted tail; for a total match it is unreachable. The panic is contained at the boundary → abort
/// (ADR-0071 §7).
#[no_mangle]
pub extern "C" fn pv_case_fail() {
    guard(|| panic!("purvasm: no matching case alternative"));
}

/// Print a **pure `Int` entry**'s value to `stdout` (no trailing newline), matching the oracle's
/// `Value.to_string` for `Int` (OCaml `string_of_int` == Rust `i32` `Display` over all `i32`). The
/// codegen entry stub emits this for a pure `Int` program (ADR-0072 §8); type-directed printing for
/// other entry types is added with the slices that introduce them. A write failure aborts (ADR-0071 §7).
#[no_mangle]
pub extern "C" fn pv_print_int(v: u64) {
    guard(|| {
        use std::io::Write;
        let n = TaggedWord::from_bits(v).as_int();
        let out = std::io::stdout();
        let mut lock = out.lock();
        write!(lock, "{n}").expect("pv_print_int: stdout write failed");
        lock.flush().expect("pv_print_int: stdout flush failed");
    })
}

// --- the ctx-header ABI (ADR-0079 §1) ---------------------------------------------------------------

/// The `pv_ctx_header` ABI version this runtime implements (mirrors `PV_CTX_HEADER_VERSION` in
/// `include/purvasm.h`; the compile-time layout assertions live beside `CtxHeader` in `gc.rs`).
pub const PV_CTX_HEADER_VERSION: u32 = 1;

/// The per-object link-time version stamp (ADR-0079 §1): every generated object that emits
/// header-offset fast paths references this symbol, so linking an object compiled against any
/// other header version fails loudly with an undefined `pv_ctx_abi_v<N>` — per object, at zero
/// runtime cost. Only the symbol for THIS runtime's version exists — and only in the RELEASE
/// profile: the debug runtime packs a generation into root handles and keeps `root_gens`
/// bookkeeping the inline fast paths would neither produce nor update (ADR-0079 §2's
/// mode-switched contract), so an inline-emitting object linked against a debug staticlib must
/// fail at link, not corrupt the generation net at run time. The debug profile exports the
/// `_debug`-suffixed sibling instead (nothing references it yet; it exists so an `nm` audit can
/// tell the profiles apart).
#[cfg(not(debug_assertions))]
#[no_mangle]
pub static pv_ctx_abi_v1: u8 = 0;

/// The debug-profile sibling of [`pv_ctx_abi_v1`] (see there).
#[cfg(debug_assertions)]
#[no_mangle]
pub static pv_ctx_abi_v1_debug: u8 = 0;

/// The run-time backstop of the ADR-0079 §1 net, called once from the generated entry stub:
/// aborts loudly on a header-version mismatch the linker did not mediate. Also the mechanism
/// the ADR-0078 §5 driver-side ABI check consumes.
#[no_mangle]
pub extern "C" fn pv_abi_check(version: u32) {
    if version != PV_CTX_HEADER_VERSION {
        eprintln!(
            "purvasm: ABI mismatch: object expects pv_ctx_header v{version}, runtime provides v{PV_CTX_HEADER_VERSION}"
        );
        std::process::abort();
    }
}

// --- shadow-stack rooting (ADR-0071 §5) -------------------------------------------------------------

/// Open a shadow-stack frame; returns an opaque mark for [`pv_pop_frame`].
///
/// # Safety
/// `ctx` is a live context.
#[no_mangle]
pub unsafe extern "C" fn pv_frame(ctx: *mut Heap) -> u64 {
    guard(|| heap(ctx).abi_frame())
}

/// Root value word `v` across a safepoint; returns an opaque handle for [`pv_get`] (ADR-0071 §5).
///
/// # Safety
/// `ctx` is a live context; `v` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_root(ctx: *mut Heap, v: u64) -> u64 {
    guard(|| heap(ctx).abi_root(v))
}

/// The current value of a root handle — the reload-after-safepoint step (ADR-0071 §5).
///
/// # Safety
/// `ctx` is a live context; `handle` from [`pv_root`], still within its frame.
#[no_mangle]
pub unsafe extern "C" fn pv_get(ctx: *mut Heap, handle: u64) -> u64 {
    guard(|| heap(ctx).abi_get(handle))
}

/// Close a frame back to `mark`, releasing every root pushed since (ADR-0071 §5).
///
/// # Safety
/// `ctx` is a live context; `mark` from [`pv_frame`], balanced (LIFO).
#[no_mangle]
pub unsafe extern "C" fn pv_pop_frame(ctx: *mut Heap, mark: u64) {
    guard(|| heap(ctx).abi_pop_frame(mark))
}

// --- field access (ADR-0071 §6) ---------------------------------------------------------------------

/// Read value-slot field `i` of heap object `obj` (ADR-0071 §6).
///
/// # Safety
/// `ctx` is a live context; `obj` a pointer value word to a live object with `> i` value slots.
#[no_mangle]
pub unsafe extern "C" fn pv_read_field(ctx: *mut Heap, obj: u64, i: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let p = HeapPtr::from_word(TaggedWord::from_bits(obj));
        h.read_field(p, i).to_bits()
    })
}

/// Write value word `v` to value-slot field `i` of `obj` (ADR-0071 §6). Routes through the
/// write-barrier choke point (a no-op in v1, ADR-0066 §5).
///
/// # Safety
/// As [`pv_read_field`]; `v` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_write_field(ctx: *mut Heap, obj: u64, i: u64, v: u64) {
    guard(|| {
        let h = heap(ctx);
        let p = HeapPtr::from_word(TaggedWord::from_bits(obj));
        h.write_field(p, i, TaggedWord::from_bits(v));
    })
}

/// Read raw (non-value) word `i` of `obj` — a code pointer, arity, `f64` bits, id, etc. (ADR-0071 §6).
///
/// # Safety
/// As [`pv_read_field`].
#[no_mangle]
pub unsafe extern "C" fn pv_read_raw(ctx: *mut Heap, obj: u64, i: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let p = HeapPtr::from_word(TaggedWord::from_bits(obj));
        h.read_raw(p, i)
    })
}

/// Write raw word `bits` to raw slot `i` of `obj` (ADR-0071 §6).
///
/// # Safety
/// As [`pv_read_field`].
#[no_mangle]
pub unsafe extern "C" fn pv_write_raw(ctx: *mut Heap, obj: u64, i: u64, bits: u64) {
    guard(|| {
        let h = heap(ctx);
        let p = HeapPtr::from_word(TaggedWord::from_bits(obj));
        h.write_raw(p, i, bits);
    })
}

// --- allocation / constructors (ADR-0071 §6) --------------------------------------------------------

/// Allocate a field-carrying [`Adt`](crate::heap::Kind::Adt) `tag(fields…)` (ADR-0071 §6). A *nullary*
/// constructor is an immediate (codegen emits it inline), so this is only for `arity >= 1`. Self-rooting.
///
/// # Safety
/// `ctx` live; `fields`/`nfields` a valid value-word buffer.
#[no_mangle]
pub unsafe extern "C" fn pv_new_adt(
    ctx: *mut Heap,
    tag: u32,
    fields: *const u64,
    nfields: usize,
) -> u64 {
    guard(|| {
        let h = heap(ctx);
        h.new_adt(tag, args_slice(fields, nfields))
            .as_word()
            .to_bits()
    })
}

/// Box a `Number` (`f64`) (ADR-0071 §6). `bits` is the IEEE-754 bit pattern (codegen passes `f64` bits).
///
/// # Safety
/// `ctx` live.
#[no_mangle]
pub unsafe extern "C" fn pv_new_number(ctx: *mut Heap, bits: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .new_number(f64::from_bits(bits))
            .as_word()
            .to_bits()
    })
}

/// Read a boxed `Number`'s IEEE-754 bit pattern — the FFI read side of ADR-0064's `Number` rep (ADR-0073
/// §2), so a `.c` foreign (e.g. `showNumberImpl`) can format it without knowing the encoding. A `Number`
/// is **boxed** (ADR-0064 §1), so this takes `ctx` to reach — and shape-validate — the heap object; a C
/// leaf can never deref the word itself without breaking representation-opacity (ADR-0069).
///
/// # Safety
/// `ctx` live; `n` a value word denoting a `Number`.
#[no_mangle]
pub unsafe extern "C" fn pv_number_bits(ctx: *mut Heap, n: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let p = h.checked_ptr(TaggedWord::from_bits(n));
        h.number_bits(p)
    })
}

/// Read an immediate `Int`'s payload (ADR-0073 §2). `Int` is an immediate (ADR-0064 §1), so `ctx` is
/// unused — but every scalar accessor takes `PVContext*` for ABI uniformity (the whole `pv_*` surface is
/// `ctx`-first; an immediate accessor simply ignores it).
///
/// # Safety
/// `w` a value word denoting an `Int`.
#[no_mangle]
pub unsafe extern "C" fn pv_int_payload(_ctx: *mut Heap, w: u64) -> i32 {
    guard(|| TaggedWord::from_bits(w).as_int())
}

/// Read an immediate `Boolean`'s payload as `0`/`1` (ADR-0073 §2). Immediate like [`pv_int_payload`]; `ctx`
/// is ignored (uniformity). Returns a C `int`.
///
/// # Safety
/// `w` a value word denoting a `Boolean`.
#[no_mangle]
pub unsafe extern "C" fn pv_bool_payload(_ctx: *mut Heap, w: u64) -> i32 {
    guard(|| TaggedWord::from_bits(w).as_bool() as i32)
}

/// Read a `Closure`'s captured `env` value (ADR-0073 §2's grow-on-demand accessor policy, prompted by
/// ADR-0078): the read side an effect-thunk foreign uses to reach its captures without knowing the
/// closure layout — which stays the runtime's, like every rep behind the accessor surface.
///
/// # Safety
/// `ctx` live; `c` a value word denoting a `Closure`.
#[no_mangle]
pub unsafe extern "C" fn pv_closure_env(ctx: *mut Heap, c: u64) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let p = h.checked_ptr(TaggedWord::from_bits(c));
        h.closure_env(p).to_bits()
    })
}

/// Make an immediate `Int` value word from a C `int32_t` (ADR-0073 §2 write side, the on-demand immediate
/// constructors). Immediates allocate nothing, so no `ctx`; the *encoding* stays the runtime's — the leaf
/// computes nothing itself.
#[no_mangle]
pub extern "C" fn pv_int(v: i32) -> u64 {
    TaggedWord::int(v).to_bits()
}

/// Make an immediate `Boolean` value word from a C truth value (`0` = false, non-`0` = true).
#[no_mangle]
pub extern "C" fn pv_bool(v: i32) -> u64 {
    TaggedWord::bool(v != 0).to_bits()
}

/// The immediate `Unit` value word.
#[no_mangle]
pub extern "C" fn pv_unit() -> u64 {
    TaggedWord::unit().to_bits()
}

/// A `String`'s UTF-8 byte length (ADR-0073 §2 read side). Pairs with [`pv_str_copy`]: the two-call
/// copy-out shape deliberately never hands C an interior pointer into the moving heap.
///
/// # Safety
/// `ctx` live; `s` a value word denoting a `String`.
#[no_mangle]
pub unsafe extern "C" fn pv_str_len(ctx: *mut Heap, s: u64) -> usize {
    guard(|| {
        let h = heap(ctx);
        let p = h.checked_ptr(TaggedWord::from_bits(s));
        h.str_len(p)
    })
}

/// Copy a `String`'s UTF-8 bytes into a caller-owned buffer of capacity `cap`, returning the byte count
/// copied (`min(len, cap)` — size the buffer with [`pv_str_len`]). Copy-out keeps the moving heap opaque:
/// no pointer into it ever escapes to C, so the bytes stay valid regardless of later `pv_*` calls.
///
/// # Safety
/// `ctx` live; `s` a value word denoting a `String`; `dst` writable for `cap` bytes (or `cap == 0`).
#[no_mangle]
pub unsafe extern "C" fn pv_str_copy(ctx: *mut Heap, s: u64, dst: *mut u8, cap: usize) -> usize {
    guard(|| {
        let h = heap(ctx);
        let p = h.checked_ptr(TaggedWord::from_bits(s));
        let src = h.str_read(p);
        let n = src.len().min(cap);
        if n > 0 {
            // SAFETY: `dst` is caller-guaranteed writable for `cap >= n` bytes; `src` is a fresh owned
            // copy of the heap bytes, so the ranges cannot overlap.
            unsafe { core::ptr::copy_nonoverlapping(src.as_ptr(), dst, n) };
        }
        n
    })
}

/// Allocate a mutable [`Ref`](crate::heap::Kind::Ref) cell holding `init` (ADR-0071 §6).
///
/// # Safety
/// `ctx` live; `init` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_new_ref(ctx: *mut Heap, init: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .new_ref(TaggedWord::from_bits(init))
            .as_word()
            .to_bits()
    })
}

/// Allocate a non-empty [`Array`](crate::heap::Kind::Array) of `elems` (ADR-0071 §6). Empty arrays use
/// [`pv_empty_array`] (the runtime rejects a zero-length heap array). Self-rooting.
///
/// # Safety
/// `ctx` live; `elems`/`n` a valid value-word buffer with `n >= 1`.
#[no_mangle]
pub unsafe extern "C" fn pv_new_array(ctx: *mut Heap, elems: *const u64, n: usize) -> u64 {
    guard(|| {
        heap(ctx)
            .new_array(args_slice(elems, n))
            .as_word()
            .to_bits()
    })
}

/// The **empty-array sentinel** (ADR-0071 §6): an immediate, since a zero-length heap `Array` would trip
/// the `size >= 1` header invariant. The array primops treat an immediate array value as empty (mirrors
/// the empty-record `unit` sentinel, ADR-0069 §1). `[]` and `newArray 0` lower here.
#[no_mangle]
pub extern "C" fn pv_empty_array() -> u64 {
    empty_array().to_bits()
}

/// The empty-array sentinel value (crate-internal; the array primops in [`crate::prim`] compare against
/// it). An immediate — a well-typed array value is a heap `Array` pointer or this sentinel.
#[inline]
pub(crate) fn empty_array() -> TaggedWord {
    TaggedWord::unit()
}

/// An `Array`'s element count (ADR-0073 §2 accessor growth, prompted by ADR-0078's `Vec`
/// conversions): the empty-array sentinel reads as 0, a heap `Array` as its element count.
///
/// # Safety
/// `ctx` live; `a` a value word denoting an `Array` (heap object or the empty sentinel).
#[no_mangle]
pub unsafe extern "C" fn pv_array_len(ctx: *mut Heap, a: u64) -> usize {
    guard(|| {
        let w = TaggedWord::from_bits(a);
        if w.to_bits() == empty_array().to_bits() {
            return 0;
        }
        let h = heap(ctx);
        let p = h.checked_ptr(w);
        h.array_len(p) as usize
    })
}

/// Allocate a [`Str`](crate::heap::Kind::Str) from UTF-8 `bytes` (ADR-0071 §6). Asserts valid UTF-8
/// (ADR-0067 §5); the empty string is a valid `Str`. Self-rooting is trivial (bytes are raw).
///
/// # Safety
/// `ctx` live; `bytes`/`len` a valid byte buffer.
#[no_mangle]
pub unsafe extern "C" fn pv_new_str(ctx: *mut Heap, bytes: *const u8, len: usize) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let b: &[u8] = if len == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(bytes, len)
        };
        h.new_str(b).as_word().to_bits()
    })
}

/// Allocate a [`Record`](crate::heap::Kind::Record) from parallel `ids` (FNV-1a-64 label ids, strictly
/// ascending) and `values` (ADR-0071 §6 / ADR-0069). `n == 0` builds the empty record. Self-rooting.
///
/// # Safety
/// `ctx` live; `ids`/`values` valid buffers of `n` words each.
#[no_mangle]
pub unsafe extern "C" fn pv_new_record(
    ctx: *mut Heap,
    ids: *const u64,
    values: *const u64,
    n: usize,
) -> u64 {
    guard(|| {
        let h = heap(ctx);
        let id_slice: &[u64] = if n == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(ids, n)
        };
        h.new_record(id_slice, args_slice(values, n))
            .as_word()
            .to_bits()
    })
}

/// Allocate an [`Unforced`](crate::heap::Kind::ByNeed) by-need cell holding `suspension` (a thunk
/// closure) (ADR-0071 §6 / ADR-0070). Self-rooting.
///
/// # Safety
/// `ctx` live; `suspension` a value word (an arity-1 closure).
#[no_mangle]
pub unsafe extern "C" fn pv_new_byneed(ctx: *mut Heap, suspension: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .new_byneed(TaggedWord::from_bits(suspension))
            .as_word()
            .to_bits()
    })
}

/// A **placeholder** `ByNeed` cell for the `Grec` recursive-group builder (ADR-0070 §4): `Unforced` with a
/// `unit` result, its real suspension backpatched by [`pv_byneed_set_suspension`] once the shared env is
/// complete. Codegen emits the §4 sequence (env array → placeholder cells → backpatch).
///
/// # Safety
/// `ctx` live.
#[no_mangle]
pub unsafe extern "C" fn pv_new_byneed_placeholder(ctx: *mut Heap) -> u64 {
    guard(|| heap(ctx).new_byneed_placeholder().as_word().to_bits())
}

/// Backpatch a placeholder cell's suspension (`Grec` builder, ADR-0070 §4) — a plain value-slot store that
/// does **not** force it. The cell must still be `Unforced`.
///
/// # Safety
/// `ctx` live; `cell` a placeholder `ByNeed` pointer; `susp` an arity-1 thunk closure value.
#[no_mangle]
pub unsafe extern "C" fn pv_byneed_set_suspension(ctx: *mut Heap, cell: u64, susp: u64) {
    guard(|| {
        let h = heap(ctx);
        let cp = HeapPtr::from_word(TaggedWord::from_bits(cell));
        h.byneed_set_suspension(cp, TaggedWord::from_bits(susp));
    })
}

/// **Static** record field read (ADR-0069): `rec.label` where the codegen already hashed `label` to its
/// FNV-1a-64 `id`. Faults if `id` is absent (the typed row constraint guarantees presence). This is the
/// id-keyed core the `Accessor` node lowers to (distinct from the `String`-keyed `RecordGet` primop).
///
/// # Safety
/// `ctx` live; `rec` a `Record` value word.
#[no_mangle]
pub unsafe extern "C" fn pv_record_get(ctx: *mut Heap, rec: u64, id: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .record_get(TaggedWord::from_bits(rec), id)
            .to_bits()
    })
}

/// **Static** record functional update (ADR-0069): `rec { label = value }` by the compiler-hashed `id`
/// (the label must be **present**), returning a new record. The `Update` node folds this over its fields.
///
/// # Safety
/// `ctx` live; `rec` a `Record` value word; `value` a value word.
#[no_mangle]
pub unsafe extern "C" fn pv_record_set(ctx: *mut Heap, rec: u64, id: u64, value: u64) -> u64 {
    guard(|| {
        heap(ctx)
            .record_set(TaggedWord::from_bits(rec), id, TaggedWord::from_bits(value))
            .to_bits()
    })
}

// The address path uses `usize→fn` reconstruction (ADR-0071 §3), which Miri's abstract machine rejects,
// so these end-to-end tests — which actually *call* an `AbiCodeFn` through `pv_apply` — are excluded
// from Miri. The index path (the `lib` API) carries the Miri coverage (ADR-0063 §4).
#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    /// A hand-authored codegen stand-in: `\x y -> x + y` — a leaf `AbiCodeFn` (no env, no allocation,
    /// so no rooting). Reads two `Int` args from the raw buffer and returns their sum.
    extern "C" fn add2(_ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
        assert_eq!(nargs, 2);
        let a = unsafe { args_slice(args, nargs) };
        TaggedWord::int(a[0].as_int() + a[1].as_int()).to_bits()
    }

    #[test]
    fn apply_calls_an_address_codefn() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let clo = pv_make_closure(ctx, add2 as usize as u64, 2, TaggedWord::unit().to_bits());
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let r = pv_apply(ctx, clo, argv.as_ptr(), argv.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 7);
            pv_runtime_free(ctx);
        }
    }

    /// The FFI scalar read side (ADR-0073 §2): `pv_number_bits` round-trips a boxed `Number`'s IEEE-754
    /// bits (the value a `.c` `showNumberImpl` would format); the immediate `Int`/`Boolean` accessors read
    /// their payloads and ignore `ctx`.
    #[test]
    fn scalar_accessors_read_payloads() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let bits = 3.5_f64.to_bits();
            let n = pv_new_number(ctx, bits);
            assert_eq!(pv_number_bits(ctx, n), bits);
            assert_eq!(pv_int_payload(ctx, TaggedWord::int(-42).to_bits()), -42);
            assert_eq!(pv_bool_payload(ctx, TaggedWord::bool(true).to_bits()), 1);
            assert_eq!(pv_bool_payload(ctx, TaggedWord::bool(false).to_bits()), 0);
            pv_runtime_free(ctx);
        }
    }

    /// The FFI string read side accepts BOTH string kinds (ADR-0103 §4: `pv_str_len`/`pv_str_copy`
    /// are kind-transparent via the view normalisation) — a `.c` foreign must see identical
    /// length/bytes whether the value word is a packed `Str` or a `StrSlice`.
    #[test]
    fn string_accessors_read_both_kinds() {
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let bytes = b"hello, world";
            let packed = pv_new_str(ctx, bytes.as_ptr(), bytes.len());
            let sliced = heap(ctx)
                .str_slice_bytes(TaggedWord::from_bits(packed), 7, 12)
                .to_bits();
            assert_eq!(pv_str_len(ctx, packed), 12);
            assert_eq!(pv_str_len(ctx, sliced), 5);
            let mut buf = [0u8; 16];
            let n = pv_str_copy(ctx, packed, buf.as_mut_ptr(), buf.len());
            assert_eq!(&buf[..n], b"hello, world");
            let n = pv_str_copy(ctx, sliced, buf.as_mut_ptr(), buf.len());
            assert_eq!(&buf[..n], b"world");
            // capped copy-out still truncates identically on a slice.
            let n = pv_str_copy(ctx, sliced, buf.as_mut_ptr(), 3);
            assert_eq!(&buf[..n], b"wor");
            pv_runtime_free(ctx);
        }
    }

    /// `\y -> env[0] + y` — an env-capturing leaf (arity 1); reads its captured `Int` from the closure.
    extern "C" fn adder(ctx: *mut Heap, clo: u64, args: *const u64, nargs: usize) -> u64 {
        let h = unsafe { heap(ctx) };
        let cp = unsafe { HeapPtr::from_word(TaggedWord::from_bits(clo)) };
        let env = unsafe { HeapPtr::from_word(h.read_field(cp, 2)) };
        let x = h.read_field(env, 0).as_int();
        let a = unsafe { args_slice(args, nargs) };
        TaggedWord::int(x + a[0].as_int()).to_bits()
    }

    /// `\x -> adder{env=[x]}` — allocates an env array + an `adder` closure capturing `x` (arity 1).
    extern "C" fn mk(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
        let h = unsafe { heap(ctx) };
        let x = unsafe { args_slice(args, nargs) }[0]; // immediate Int → no rooting needed
        let env = h.new_array(&[x]);
        unsafe { h.new_closure_raw(adder as usize as u64, 1, env.as_word()) }
            .as_word()
            .to_bits()
    }

    /// `\x -> mk x` in **tail position** — the body stashes a tail call to `mk` and returns.
    extern "C" fn f_tail(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
        let mk_clo =
            unsafe { pv_make_closure(ctx, mk as usize as u64, 1, TaggedWord::unit().to_bits()) };
        unsafe { pv_tailcall(ctx, mk_clo, args, nargs) };
        0 // ignored — the pending-tail status drives the loop
    }

    /// A body that runs a **nested, itself-over-applying** `pv_apply` (arity 1) and returns a closure
    /// `adder{env=[nested]}`. Used to prove a nested `apply` cannot consume *this* activation's leftover.
    extern "C" fn f_nested(ctx: *mut Heap, _clo: u64, args: *const u64, nargs: usize) -> u64 {
        let a = unsafe { args_slice(args, nargs) }[0]; // immediate Int
        let mk_clo =
            unsafe { pv_make_closure(ctx, mk as usize as u64, 1, TaggedWord::unit().to_bits()) };
        // NESTED non-tail over-application: `mk a` yields `adder{a}`, then leftover [7] applies → a + 7.
        // This nested `pv_apply` has its OWN `conts` (transiently holding [7]).
        let nested_args = [a.to_bits(), TaggedWord::int(7).to_bits()];
        let nested = unsafe { pv_apply(ctx, mk_clo, nested_args.as_ptr(), nested_args.len()) };
        let h = unsafe { heap(ctx) };
        let env = h.new_array(&[TaggedWord::from_bits(nested)]); // immediate → no rooting
        unsafe { h.new_closure_raw(adder as usize as u64, 1, env.as_word()) }
            .as_word()
            .to_bits()
    }

    #[test]
    fn nested_over_application_does_not_consume_outer_leftover() {
        // The ADR-0071 §4 reentrancy guarantee, made executable: `conts` is per-`apply`-activation-local
        // (a Rust local, not a `Heap` field). The outer over-applies `f_nested` to [3, 4] (arity 1 →
        // leftover [4]); inside, `f_nested` runs a nested over-applying `pv_apply` (its own conts, [7]).
        // The outer's [4] must survive and apply to `f_nested`'s returned `adder{env=[3+7]}` → 10 + 4 =
        // 14. A ctx-global `conts` would let the inner activation pop the outer's [4] — a wrong result.
        let ctx = pv_runtime_new(1 << 14);
        unsafe {
            let f = pv_make_closure(
                ctx,
                f_nested as usize as u64,
                1,
                TaggedWord::unit().to_bits(),
            );
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let r = pv_apply(ctx, f, argv.as_ptr(), argv.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 14);
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn tailcall_composes_with_over_application() {
        // The ADR-0071 §4 composition: `f` has arity 1 but is applied to TWO args. `f` tail-calls `mk`
        // (→ an `adder` capturing 3); the leftover `[4]` must stay deferred on `conts` across the tail
        // bounce and then apply to that adder → 3 + 4 = 7. A single-pending-slot design would drop `4`.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let f = pv_make_closure(ctx, f_tail as usize as u64, 1, TaggedWord::unit().to_bits());
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let r = pv_apply(ctx, f, argv.as_ptr(), argv.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 7);
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn apply_under_then_saturates_a_pap_on_the_address_path() {
        // Under-application builds a PAP; supplying the rest saturates through it — the trampoline's
        // conts/PAP path on the address calling convention.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            let clo = pv_make_closure(ctx, add2 as usize as u64, 2, TaggedWord::unit().to_bits());
            let one = [TaggedWord::int(10).to_bits()];
            let pap = pv_apply(ctx, clo, one.as_ptr(), one.len());
            let two = [TaggedWord::int(32).to_bits()];
            let r = pv_apply(ctx, pap, two.as_ptr(), two.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 42);
            pv_runtime_free(ctx);
        }
    }

    // --- ADR-0102 §3 stats fixtures (address-path-only: `pv_tailcall`/`pv_settle`/env parsing are
    // never exercised on the index/Miri path — hence this module's `not(miri)` gate) -------------

    #[test]
    fn stats_count_tailcall_write_and_apply_loop_take() {
        // Reuses `tailcall_composes_with_over_application`'s scenario, adding counter assertions:
        // `f` (arity 1) over-applied to [3,4] tail-calls `mk`; the enclosing `pv_apply` loop (not
        // `pv_settle`) takes the stash.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            heap(ctx).enable_stats_for_test();
            let f = pv_make_closure(ctx, f_tail as usize as u64, 1, TaggedWord::unit().to_bits());
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let r = pv_apply(ctx, f, argv.as_ptr(), argv.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 7);

            let s = *heap(ctx).stats().unwrap();
            assert_eq!(s.pv_apply_entries, 1);
            assert_eq!(s.over_apply, 1, "f (arity 1) is over-applied to [3, 4]");
            assert_eq!(s.pv_tailcall_writes, 1);
            assert_eq!(
                s.pending_tail_apply_takes, 1,
                "the enclosing pv_apply loop takes the stash"
            );
            assert_eq!(
                s.pending_tail_settle_takes, 0,
                "pv_settle is never called on this path"
            );
            assert_eq!(
                s.closure_exact_dispatches, 2,
                "the mk dispatch and the resolved adder call"
            );
            assert_eq!(
                s.entry_exact_fast_hits, 0,
                "f is over-applied (2 args vs arity 1) at entry, so the fast path always misses here \
                 — both exact dispatches are reached via apply_loop's own tail-bounce/leftover-resolve, \
                 never at apply()'s entry"
            );
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn fast_path_hit_then_tailcall_falls_into_trampoline() {
        // ADR-0102 §2 Verification: a callee whose FIRST call is exact-saturated (hits the new
        // pre-loop fast path) and stashes a pending tail must still resolve correctly through the
        // existing trampoline (`apply_loop`) — unlike `stats_count_tailcall_write_and_apply_loop_take`
        // above, which over-applies at entry and therefore never reaches the fast path at all.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            heap(ctx).enable_stats_for_test();
            let f = pv_make_closure(ctx, f_tail as usize as u64, 1, TaggedWord::unit().to_bits());
            let argv = [TaggedWord::int(3).to_bits()]; // exact: arity 1, 1 arg
            let r = pv_apply(ctx, f, argv.as_ptr(), argv.len()); // fast-path hit -> tailcall -> apply_loop
                                                                 // r is `mk(3)` = adder{env=[3]}, a closure; apply it once more (also exact) to check the
                                                                 // captured value survived the fast-path-to-trampoline handoff.
            let argv2 = [TaggedWord::int(39).to_bits()];
            let r2 = pv_apply(ctx, r, argv2.as_ptr(), argv2.len());
            assert_eq!(TaggedWord::from_bits(r2).as_int(), 42);

            let s = *heap(ctx).stats().unwrap();
            assert_eq!(
                s.entry_exact_fast_hits, 2,
                "both pv_apply calls are exact-saturated at entry"
            );
            assert_eq!(s.pv_tailcall_writes, 1);
            assert_eq!(
                s.pending_tail_apply_takes, 1,
                "taken by the fast path's own take, which falls into apply_loop with the stash"
            );
            assert_eq!(
                s.pending_tail_settle_takes, 0,
                "pv_settle is never called on this path"
            );
            assert_eq!(
                s.closure_exact_dispatches, 3,
                "the fast-path mk dispatch, apply_loop's resolved adder dispatch, and the second \
                 fast-path call"
            );
            assert_eq!(s.under_apply, 0);
            assert_eq!(s.over_apply, 0);
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn stats_pv_settle_fast_path_when_no_tail_was_stashed() {
        // A direct (non-`pv_apply`) call to a body that returns a real value with no `pv_tailcall` —
        // the settle call site every non-`musttail` direct call site makes (ADR-0076 §3).
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            heap(ctx).enable_stats_for_test();
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let raw = add2(ctx, TaggedWord::unit().to_bits(), argv.as_ptr(), argv.len());
            let settled = pv_settle(ctx, raw);
            assert_eq!(TaggedWord::from_bits(settled).as_int(), 7);

            let s = *heap(ctx).stats().unwrap();
            assert_eq!(s.pv_settle_fast, 1);
            assert_eq!(s.pv_settle_slow, 0);
            assert_eq!(s.pending_tail_settle_takes, 0);
            pv_runtime_free(ctx);
        }
    }

    #[test]
    fn stats_pv_settle_slow_path_resolves_a_stashed_tail() {
        // A direct call to `f_tail`'s body (bypassing `pv_apply`): it stashes a tail call to `mk` and
        // returns a dummy; `pv_settle` must take and resolve it.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            heap(ctx).enable_stats_for_test();
            let argv = [TaggedWord::int(3).to_bits()];
            let raw = f_tail(ctx, TaggedWord::unit().to_bits(), argv.as_ptr(), argv.len());
            let settled = pv_settle(ctx, raw);
            assert!(
                TaggedWord::from_bits(settled).is_pointer(),
                "mk(3) returns an adder closure value"
            );

            let s = *heap(ctx).stats().unwrap();
            assert_eq!(s.pv_settle_slow, 1);
            assert_eq!(s.pending_tail_settle_takes, 1);
            assert_eq!(s.pv_settle_fast, 0);
            pv_runtime_free(ctx);
        }
    }

    /// `\_ -> 42` — an arity-1 leaf ignoring its arg, for the by-need-suspension divergence fixture
    /// below (`force` always calls a suspension with exactly one `unit` arg, ADR-0070's convention).
    extern "C" fn const_leaf(_ctx: *mut Heap, _clo: u64, _args: *const u64, _nargs: usize) -> u64 {
        TaggedWord::int(42).to_bits()
    }

    #[test]
    fn stats_pv_apply_entries_excludes_internal_apply_activations() {
        // `pv_force` forces a `ByNeed` cell via `Heap::force`, which internally calls `Heap::apply`
        // on the suspension WITHOUT going through `pv_apply` — this must count as a
        // `heap_apply_activations` increment but NOT a `pv_apply_entries` increment.
        let ctx = pv_runtime_new(1 << 12);
        unsafe {
            heap(ctx).enable_stats_for_test();
            let susp = pv_make_closure(
                ctx,
                const_leaf as usize as u64,
                1,
                TaggedWord::unit().to_bits(),
            );
            let cell = pv_new_byneed(ctx, susp);
            let forced = pv_force(ctx, cell);
            assert_eq!(TaggedWord::from_bits(forced).as_int(), 42);

            // One real `pv_apply` call too, for contrast.
            let clo = pv_make_closure(ctx, add2 as usize as u64, 2, TaggedWord::unit().to_bits());
            let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
            let r = pv_apply(ctx, clo, argv.as_ptr(), argv.len());
            assert_eq!(TaggedWord::from_bits(r).as_int(), 7);

            let s = *heap(ctx).stats().unwrap();
            assert_eq!(
                s.pv_apply_entries, 1,
                "only the direct pv_apply call counts"
            );
            assert_eq!(
                s.heap_apply_activations, 2,
                "force's internal apply on the suspension, plus the real pv_apply's apply"
            );
            // ADR-0102 §2: both activations are exact matches at their own entry (the nested
            // force-triggered call on the arity-1 suspension, and the real pv_apply's arity-2 add2
            // call), so both fast-path hit — independent of which one is a `pv_apply_entries`.
            assert_eq!(s.entry_exact_fast_hits, 2);
            pv_runtime_free(ctx);
        }
    }

    // --- `PURVASM_STATS` process-level behavior (subprocess-isolated: env mutation must not race
    // parallel `cargo test` threads) -------------------------------------------------------------

    /// Re-invoke this same test binary running exactly one test (by fully-qualified name), with the
    /// given environment overrides (`None` removes a var that may be set in the ambient environment)
    /// plus a private marker var, capturing its stdout/stderr and exit status. The named test checks
    /// the marker at its own top and runs its real body instead of re-exec'ing — the standard
    /// hand-rolled env-isolation trick (ADR-0102 §3 Verification), since this crate has no `[[bin]]`
    /// and adds no new dev-dependency for it.
    fn run_isolated(test_name: &str, env: &[(&str, Option<&str>)]) -> std::process::Output {
        let exe = std::env::current_exe().expect("current_exe");
        let mut cmd = std::process::Command::new(exe);
        cmd.arg(test_name).arg("--exact").arg("--nocapture");
        cmd.env("__PURVASM_ABI_TEST_CHILD", "1");
        for (k, v) in env {
            match v {
                Some(v) => {
                    cmd.env(k, v);
                }
                None => {
                    cmd.env_remove(k);
                }
            }
        }
        cmd.output().expect("spawn current_exe as a child")
    }

    /// The shared child body every `PURVASM_STATS` subprocess test re-execs into: build a context,
    /// run one real `pv_apply`, free it. What differs is the parent's `PURVASM_STATS` and what it
    /// asserts about the child's stderr/exit status afterward.
    unsafe fn stats_smoke_child() {
        let ctx = pv_runtime_new(1 << 12);
        let clo = pv_make_closure(ctx, add2 as usize as u64, 2, TaggedWord::unit().to_bits());
        let argv = [TaggedWord::int(3).to_bits(), TaggedWord::int(4).to_bits()];
        let r = pv_apply(ctx, clo, argv.as_ptr(), argv.len());
        assert_eq!(TaggedWord::from_bits(r).as_int(), 7);
        pv_runtime_free(ctx);
    }

    #[test]
    fn purvasm_stats_absent_emits_no_line() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { stats_smoke_child() };
            return;
        }
        let out = run_isolated(
            "abi::tests::purvasm_stats_absent_emits_no_line",
            &[("PURVASM_STATS", None)],
        );
        assert!(out.status.success(), "child failed: {out:?}");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            !stderr.contains("purvasm-stats:"),
            "unexpected stats line with PURVASM_STATS absent: {stderr}"
        );
    }

    #[test]
    fn purvasm_stats_one_emits_exactly_one_well_formed_line() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { stats_smoke_child() };
            return;
        }
        let out = run_isolated(
            "abi::tests::purvasm_stats_one_emits_exactly_one_well_formed_line",
            &[("PURVASM_STATS", Some("1"))],
        );
        assert!(out.status.success(), "child failed: {out:?}");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let lines: Vec<&str> = stderr
            .lines()
            .filter(|l| l.starts_with("purvasm-stats:v1 "))
            .collect();
        assert_eq!(
            lines.len(),
            1,
            "expected exactly one stats line, got stderr: {stderr}"
        );
        let mut seen = std::collections::HashSet::new();
        for pair in lines[0].trim_start_matches("purvasm-stats:v1 ").split(' ') {
            let (key, _) = pair
                .split_once('=')
                .unwrap_or_else(|| panic!("malformed key=value pair: {pair}"));
            assert!(seen.insert(key), "duplicate key in schema line: {key}");
        }
        let expected: std::collections::HashSet<&str> =
            crate::stats::SCHEMA_KEYS.iter().copied().collect();
        assert_eq!(seen, expected, "schema key set mismatch");
    }

    #[test]
    fn purvasm_stats_other_value_aborts_runtime_creation() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            let _ctx = pv_runtime_new(1 << 12); // must abort before returning
            unreachable!("pv_runtime_new should have aborted on a malformed PURVASM_STATS");
        }
        let out = run_isolated(
            "abi::tests::purvasm_stats_other_value_aborts_runtime_creation",
            &[("PURVASM_STATS", Some("0"))],
        );
        assert!(
            !out.status.success(),
            "child should have aborted, got: {out:?}"
        );
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains("PURVASM_STATS"),
            "expected the parse_stats_env diagnostic on stderr, got: {stderr}"
        );
    }

    // --- `PURVASM_HEAP_WORDS` process-level behavior (subprocess-isolated, reusing `run_isolated`
    // above: env mutation must not race parallel `cargo test` threads) ------------------------------

    unsafe fn heap_words_absent_child() {
        let ctx = pv_runtime_new(4096);
        assert_eq!(
            heap(ctx).cap_for_test(),
            4096,
            "absent must leave the codegen-provided default unchanged"
        );
        pv_runtime_free(ctx);
    }

    unsafe fn heap_words_override_child() {
        let ctx = pv_runtime_new(4096); // the codegen default this override must replace
        assert_eq!(
            heap(ctx).cap_for_test(),
            1024,
            "a valid override must replace the codegen-provided default"
        );
        pv_runtime_free(ctx);
    }

    /// Shared child body for every malformed-`PURVASM_HEAP_WORDS` abort test: creation must abort
    /// (via `guard`'s panic containment) before ever returning a context.
    unsafe fn heap_words_abort_child() {
        let _ctx = pv_runtime_new(4096);
        unreachable!("pv_runtime_new should have aborted on a malformed PURVASM_HEAP_WORDS");
    }

    /// Shared parent-side assertion for every malformed-`PURVASM_HEAP_WORDS` abort test.
    fn assert_heap_words_aborts(test_name: &str, bad_value: &str) {
        let out = run_isolated(test_name, &[("PURVASM_HEAP_WORDS", Some(bad_value))]);
        assert!(
            !out.status.success(),
            "child should have aborted, got: {out:?}"
        );
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains("PURVASM_HEAP_WORDS"),
            "expected the parse_heap_words_env diagnostic on stderr, got: {stderr}"
        );
    }

    #[test]
    fn purvasm_heap_words_absent_uses_codegen_default() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_absent_child() };
            return;
        }
        let out = run_isolated(
            "abi::tests::purvasm_heap_words_absent_uses_codegen_default",
            &[("PURVASM_HEAP_WORDS", None)],
        );
        assert!(out.status.success(), "child failed: {out:?}");
    }

    #[test]
    fn purvasm_heap_words_valid_override_replaces_default() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_override_child() };
            return;
        }
        let out = run_isolated(
            "abi::tests::purvasm_heap_words_valid_override_replaces_default",
            &[("PURVASM_HEAP_WORDS", Some("1024"))],
        );
        assert!(out.status.success(), "child failed: {out:?}");
    }

    #[test]
    fn purvasm_heap_words_empty_aborts_runtime_creation() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_abort_child() };
            return;
        }
        assert_heap_words_aborts(
            "abi::tests::purvasm_heap_words_empty_aborts_runtime_creation",
            "",
        );
    }

    #[test]
    fn purvasm_heap_words_zero_aborts_runtime_creation() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_abort_child() };
            return;
        }
        assert_heap_words_aborts(
            "abi::tests::purvasm_heap_words_zero_aborts_runtime_creation",
            "0",
        );
    }

    #[test]
    fn purvasm_heap_words_malformed_aborts_runtime_creation() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_abort_child() };
            return;
        }
        assert_heap_words_aborts(
            "abi::tests::purvasm_heap_words_malformed_aborts_runtime_creation",
            "12a",
        );
    }

    #[test]
    fn purvasm_heap_words_overflow_aborts_runtime_creation() {
        if std::env::var_os("__PURVASM_ABI_TEST_CHILD").is_some() {
            unsafe { heap_words_abort_child() };
            return;
        }
        let too_big = format!("9{}", usize::MAX);
        assert_heap_words_aborts(
            "abi::tests::purvasm_heap_words_overflow_aborts_runtime_creation",
            &too_big,
        );
    }
}
