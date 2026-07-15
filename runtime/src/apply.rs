//! The v1 eval/apply calling convention (ADR-0064 §3).
//!
//! [`Heap::apply`] is the single generic entry all v1 calls go through (the *direct*, statically
//! known-arity call-site fast path is a separate, deferred mechanism — ADR-0076's `musttail`). It
//! dispatches on the callee's arity: **saturate** (exactly enough args → call the code, ADR-0102 §2's
//! allocation-free fast path handles the common case inline), **over-apply** (too many → call, then
//! `apply` the result to the rest), or **under-apply** (too few → build a `PAP`). Tail calls and
//! over-application chains run as a **trampoline** (v1 TCE, ADR-0064 §4) in
//! [`apply_loop`](Heap::apply_loop).
//!
//! **Rooting (ADR-0066 §3), per-function.** `alloc` is a safepoint, so each function roots the
//! values *it* holds across a safepoint — the same discipline codegen emits per generated function,
//! not a caller-roots-callee scheme. In `apply` the only such value is the **over-apply leftover
//! args** (used after the `code` call); they are rooted across the call and reloaded. `apply` does
//! *not* root `f` (unused after `code` — over-apply replaces it) nor the saturated call args (handed
//! to `code` at entry with no intervening allocation), because those do not outlive a safepoint
//! *within `apply`*. The `code` callee owns rooting *its* view — see [`CodeFn`].
//!
//! v1 boundaries (lifted with later increments):
//! - `apply` materialises `&mut Heap` and threads it into each [`CodeFn`] by reborrow — sound, one
//!   `&mut` active per level. The C-ABI (`extern "C"`, a raw heap-context pointer, args as ptr+len)
//!   arrives with codegen.
//! - The working argument list is a `Vec` for simplicity; codegen lowers it to a stack array.

use crate::gc::{Heap, Root, RootFrame};
use crate::heap::{HeapPtr, Kind};
use crate::Value;

/// The generated-code entry a [`Closure`](Kind::Closure) points to. It receives the runtime, the
/// closure itself (for its env / free variables), and exactly `arity` arguments, and returns the
/// result. (v1 uses a Rust fn pointer over `&mut Heap`; codegen adopts the `extern "C"` + raw-pointer
/// ABI.)
///
/// **Rooting contract (ADR-0066 §3).** The `closure` and `args` are valid **on entry**. Because the
/// body may allocate — and `alloc` is a safepoint that can relocate the heap — a `CodeFn` that holds
/// `closure` or any `arg` (or any other heap `Value`) *across an allocation it performs* must root it
/// on the shadow stack ([`Heap::root`]/[`Heap::get`]) and reload it after, exactly as the constructors
/// and `apply` do. A body that touches its inputs only *before* it allocates (a leaf primop, or one
/// that reads its env into locals first) needs no rooting. The caller (`apply`) does **not** root the
/// callee's inputs on its behalf; this is the per-function rooting codegen emits.
pub type CodeFn = fn(heap: &mut Heap, closure: Value, args: &[Value]) -> Value;

/// The **codegen / native-ABI** entry a [`Closure`](Kind::Closure) points to when the heap is in
/// address mode ([`Heap::new_native`], ADR-0071 §3). The `extern "C"` C-ABI form of [`CodeFn`]: the
/// runtime context as a raw `*mut Heap`, the closure and each argument as raw `u64` value words
/// (ptr+len for the args), returning the result word. LLVM-generated functions have exactly this
/// signature; the runtime's own leaves expose it too. On the address path [`Heap::apply`] reconstructs
/// this pointer from the closure's `code` word — a linker-provided symbol address, sound on the
/// supported 64-bit targets (ADR-0071 §3), never exercised under Miri (which runs the index path only).
pub type AbiCodeFn =
    extern "C" fn(ctx: *mut Heap, closure: u64, args: *const u64, nargs: usize) -> u64;

impl Heap {
    /// A `Closure` over the type-safe [`CodeFn`] entry. `env` is a shared env block's pointer word
    /// (via [`HeapPtr::as_word`]) or an immediate sentinel for a no-capture closure. The safe wrapper
    /// over [`Heap::new_closure_raw`]: it interns `code` into the heap's code table and stores the
    /// resulting index, so `apply` recovers the real fn pointer (with provenance) by a bounds-checked
    /// lookup — never an integer→fn transmute, which would be a provenance-free (UB) call.
    pub fn new_closure(&mut self, code: CodeFn, arity: u32, env: Value) -> HeapPtr {
        let idx = self.intern_code(code);
        // SAFETY: `idx` is a valid code-table index by construction. `new_closure_raw` self-roots
        // `env` across its allocation (ADR-0066 §3).
        unsafe { self.new_closure_raw(idx, arity, env) }
    }

    /// Apply callable `f` to `args` (ADR-0064 §3 / the ADR-0071 §4 trampoline). `f` must be a
    /// [`Closure`](Kind::Closure) or a [`Pap`](Kind::Pap); applying anything else is a codegen bug and
    /// panics.
    ///
    /// **Fast path then trampoline (ADR-0102 §2 / ADR-0071 §4).** An exact-saturated `Closure` call
    /// (`args.len == arity`) is handled inline, with zero host allocations, before falling into
    /// [`apply_loop`](Heap::apply_loop) — the owned trampoline — only if it stashes a pending tail.
    /// Every other case (under/over-application, `Pap`, `ByNeed`) falls into `apply_loop` directly.
    /// `apply_loop`'s continuation stack is a local, never shared through the `Heap`, so a callee's
    /// nested `apply` cannot pop *this* activation's leftovers. A tail call (address path) is a body's
    /// `pv_tailcall` stashing the pending-tail slot; [`take_pending_tail`](Heap::take_pending_tail)
    /// drives the bounce. On the index path the slot is always `None`.
    pub fn apply(&mut self, f: Value, args: &[Value]) -> Value {
        // ADR-0102 §3: once per activation, regardless of caller (`pv_apply`, `pv_settle`, `force`,
        // or any other internal helper) — the counter this function's own doc comment on
        // `heap_apply_activations` in `stats.rs` describes.
        if let Some(s) = self.stats_mut() {
            s.heap_apply_activations = s.heap_apply_activations.saturating_add(1);
        }
        if self.trace_on() {
            self.trace_value(&format!("apply f (nargs={})", args.len()), f);
            for (i, a) in args.iter().enumerate() {
                self.trace_value(&format!("  arg{i}"), *a);
            }
        }

        // ADR-0102 §2: the allocation-free exact-saturated closure fast path, checked *before*
        // materialising the owned `Vec`/`conts` the generic trampoline needs. Same release-validation
        // tier as the loop below — `checked_ptr` still rejects a stale, foreign, forwarded,
        // non-pointer, or non-callable callee before any unchecked header/code-word access, so this
        // is a shortcut for a case the existing loop already accepts, not a weaker entry. Nothing
        // allocates between the checks below and `call_code`, so handing `code` the original
        // *borrowed* `args` slice (not a copy) is sound under the same per-function rooting
        // discipline `CodeFn`'s doc comment describes: `apply` still doesn't root `f` or the
        // saturated args on the callee's behalf, and a stashed pending tail already carries its own
        // owned arguments through the existing `take_pending_tail` protocol.
        let p = self.checked_ptr(f);
        if self.header_unchecked(p).kind() == Kind::Closure {
            let arity = self.read_raw_unchecked(p, 1) as usize;
            if args.len() == arity {
                if let Some(s) = self.stats_mut() {
                    s.entry_exact_fast_hits = s.entry_exact_fast_hits.saturating_add(1);
                    s.record_closure_exact(arity as u32);
                }
                let code_word = self.read_raw_unchecked(p, 0);
                let result = self.call_code(code_word, f, args);
                if self.trace_on() {
                    self.trace_value(&format!("  = code 0x{code_word:x} returned"), result);
                }
                return match self.take_pending_tail() {
                    // The common case: a real value, zero `Vec` allocations end to end.
                    None => result,
                    // A tail bounce: fall into the existing owned trampoline with the stashed callee.
                    // `conts` starts empty — this was the first dispatch, so there is no over-apply
                    // leftover to carry.
                    Some((nf, nargs)) => {
                        if let Some(s) = self.stats_mut() {
                            s.pending_tail_apply_takes =
                                s.pending_tail_apply_takes.saturating_add(1);
                        }
                        self.apply_loop(nf, nargs, Vec::new())
                    }
                };
            }
        }
        // Fast-path miss (not a `Closure`, or an under/over-application): the existing generic loop,
        // unchanged.
        self.apply_loop(f, args.to_vec(), Vec::new())
    }

    /// The owned generic trampoline (ADR-0071 §4): under-application/PAP-flattening/by-need-forcing/
    /// over-application/tail-bounce handling. Unchanged by ADR-0102 §2 except for the ADR-0102 §3
    /// counters it already carried — entered either directly (an [`apply`](Heap::apply) fast-path
    /// miss) or after the fast path's own exact call stashes a pending tail.
    ///
    /// `conts` is this activation's continuation stack — leftover-arg groups still owed application
    /// (from over-application), innermost-last, applied once the current `(f, args)` chain yields a
    /// *real* value. Each group is rooted on the shadow stack (LIFO with the frame mark), so it
    /// survives every subsequent `code` safepoint until consumed. Callers start it empty: [`apply`]
    /// on both its own paths (a fast-path miss has no leftover yet; a fast-path tail-bounce is still
    /// the first dispatch of this activation, so it has none either).
    fn apply_loop(
        &mut self,
        mut f: Value,
        mut args: Vec<Value>,
        mut conts: Vec<(RootFrame, Vec<Root>)>,
    ) -> Value {
        loop {
            // `apply` is safe public API. A non-pointer, foreign/forged, stale, or forwarded callee
            // would make the header deref below — and the code-word call — UB. `checked_ptr`
            // release-rejects all of those before any dereference (ADR-0064 §4).
            let p = self.checked_ptr(f);
            // `p` is a validated live object header, so the unchecked field tier is sound here (see the
            // tier note in `gc`).
            match self.header_unchecked(p).kind() {
                // A by-need callee (a projected dictionary member used as a function) is forced, then
                // re-dispatched on the forced value (ADR-0070 §3). `force` is a safepoint, so the
                // in-flight `args` are rooted across it and reloaded (`f` is replaced by the result;
                // `conts` roots are independent and untouched).
                Kind::ByNeed => {
                    // `force` (below) internally re-enters `apply` on the suspension, so this single
                    // by-need dispatch also produces a *nested* `heap_apply_activations` increment in
                    // addition to this one and to the outer activation's — intentional per ADR-0102
                    // §3 ("regardless of whether the caller was … another runtime helper/internal
                    // operation"), not a double-count bug.
                    if let Some(s) = self.stats_mut() {
                        s.byneed_dispatch = s.byneed_dispatch.saturating_add(1);
                    }
                    let frame = self.frame();
                    let mut arg_roots: Vec<Root> = Vec::with_capacity(args.len());
                    for a in &args {
                        arg_roots.push(self.root(*a));
                    }
                    let forced = self.force(f);
                    args = arg_roots.iter().map(|r| self.get(*r)).collect();
                    self.pop_frame(frame);
                    f = forced;
                }
                // A PAP flattens to its underlying function applied to (captured ++ args); re-dispatch
                // that as a closure. `remaining_arity` (payload word 1) is redundant here — the
                // closure's own arity drives the split — so it is not read.
                Kind::Pap => {
                    if let Some(s) = self.stats_mut() {
                        s.pap_dispatch = s.pap_dispatch.saturating_add(1);
                    }
                    let function = self.read_field_unchecked(p, 0);
                    let size = self.header_unchecked(p).size_words();
                    let mut combined: Vec<Value> =
                        (2..size).map(|i| self.read_field_unchecked(p, i)).collect();
                    combined.extend_from_slice(&args);
                    f = function;
                    args = combined;
                }
                Kind::Closure => {
                    let arity = self.read_raw_unchecked(p, 1) as usize;
                    if args.len() < arity {
                        // Under-apply: a PAP capturing the args supplied so far — the current call's
                        // value. `new_pap` self-roots `f`/`args` across its own allocation (ADR-0066 §3).
                        if let Some(s) = self.stats_mut() {
                            s.under_apply = s.under_apply.saturating_add(1);
                        }
                        let remaining = (arity - args.len()) as u32;
                        let pap = self.new_pap(f, remaining, &args).as_word();
                        match self.resolve_cont(&mut conts) {
                            None => return pap,
                            Some(next) => {
                                f = pap;
                                args = next;
                            }
                        }
                        continue;
                    }
                    // Saturate / over-apply. `code(…)` is a safepoint (ADR-0066 §3). Between reading
                    // `f`/`args` and this call nothing allocates (rooting a leftover group is host
                    // allocation only), so the call slice snapshot is current. `f` is not read after
                    // `code`. The **leftover** is pushed onto `conts` (rooted) *before* the call, so it
                    // is deferred correctly whether the call returns a value or tail-bounces.
                    let call_args: Vec<Value> = args[..arity].to_vec();
                    if args.len() > arity {
                        if let Some(s) = self.stats_mut() {
                            s.over_apply = s.over_apply.saturating_add(1);
                        }
                        let frame = self.frame();
                        let roots: Vec<Root> =
                            args[arity..].iter().map(|a| self.root(*a)).collect();
                        conts.push((frame, roots));
                    } else if let Some(s) = self.stats_mut() {
                        s.record_closure_exact(arity as u32);
                    }
                    let code_word = self.read_raw_unchecked(p, 0);
                    let result = self.call_code(code_word, f, &call_args);
                    if self.trace_on() {
                        self.trace_value(&format!("  = code 0x{code_word:x} returned"), result);
                    }
                    match self.take_pending_tail() {
                        // Tail bounce (ADR-0071 §4): continue with the stashed callee; the just-pushed
                        // leftover stays on `conts`, deferred behind this new chain.
                        Some((nf, nargs)) => {
                            if let Some(s) = self.stats_mut() {
                                s.pending_tail_apply_takes =
                                    s.pending_tail_apply_takes.saturating_add(1);
                            }
                            f = nf;
                            args = nargs;
                        }
                        // A real value: resolve it against the continuation stack.
                        None => match self.resolve_cont(&mut conts) {
                            None => return result,
                            Some(next) => {
                                f = result;
                                args = next;
                            }
                        },
                    }
                }
                other => panic!("apply: not a callable value (kind {other:?})"),
            }
        }
    }

    /// Call a closure's `code` word on `call_args` (ADR-0071 §3). Index path
    /// ([`Heap::new`]): [`code_at`](Heap::code_at) recovers the real [`CodeFn`] by a bounds-checked
    /// lookup — no integer→fn transmute (a provenance-free call, UB), so it stays Miri-clean. Address
    /// path ([`Heap::new_native`]): the word is a real [`AbiCodeFn`] address, called via the C-ABI —
    /// the supported-target reconstruction (ADR-0071 §3), never run under Miri.
    #[inline]
    fn call_code(&mut self, code_word: u64, f: Value, call_args: &[Value]) -> Value {
        if self.code_is_address() {
            // SAFETY (ADR-0071 §3): on the address path `code_word` is a linker-provided `extern "C"`
            // symbol address with provenance; transmuting a `usize` to the fn pointer and calling it is
            // ordinary indirect dispatch on the supported 64-bit targets. It is *not* valid under Miri's
            // abstract machine, but this branch is unreachable on an index-path (Miri) heap. The raw
            // `self` pointer is re-borrowed as `&mut Heap` inside the callee via `pv_*` — the standard
            // FFI "pass the context by raw pointer" pattern, with no other access to `self` across the
            // opaque call.
            let code: AbiCodeFn =
                unsafe { core::mem::transmute::<usize, AbiCodeFn>(code_word as usize) };
            let ret = code(
                self as *mut Heap,
                f.to_bits(),
                call_args.as_ptr() as *const u64,
                call_args.len(),
            );
            Value::from_bits(ret)
        } else {
            let code = self.code_at(code_word);
            code(self, f, call_args)
        }
    }

    /// Resolve a produced value against this activation's continuation stack (ADR-0071 §4): pop the
    /// next owed leftover group, reload it (post-safepoint), release its roots, and return it as the
    /// next call's args — or `None` when the stack is empty (the caller returns the value).
    #[inline]
    fn resolve_cont(&mut self, conts: &mut Vec<(RootFrame, Vec<Root>)>) -> Option<Vec<Value>> {
        let (frame, roots) = conts.pop()?;
        let vals: Vec<Value> = roots.iter().map(|r| self.get(*r)).collect();
        self.pop_frame(frame);
        Some(vals)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A saturated leaf: `\x y -> x + y` (no env).
    fn add2(_h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        Value::int(args[0].as_int() + args[1].as_int())
    }

    /// The inner closure of `\x -> \y -> x + y`: reads `x` from its env, adds `y`.
    fn adder_body(h: &mut Heap, clo: Value, args: &[Value]) -> Value {
        let clo = unsafe { HeapPtr::from_word(clo) };
        let env = unsafe { HeapPtr::from_word(h.read_field(clo, 2)) };
        let x = h.read_field(env, 0).as_int();
        Value::int(x + args[0].as_int())
    }

    /// The outer closure of `\x -> \y -> x + y`: captures `x` and returns the inner closure.
    fn make_adder(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        let env = h.new_array(&[args[0]]);
        h.new_closure(adder_body, 1, env.as_word()).as_word()
    }

    /// A saturated arity-3 leaf: `\a b c -> a + b + c` (no env).
    fn add3(_h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        Value::int(args[0].as_int() + args[1].as_int() + args[2].as_int())
    }

    /// The inner arity-2 closure of `\x -> \a b -> x + a + b`: reads `x` from its env.
    fn env_add_body(h: &mut Heap, clo: Value, args: &[Value]) -> Value {
        let clo = unsafe { HeapPtr::from_word(clo) };
        let env = unsafe { HeapPtr::from_word(h.read_field(clo, 2)) };
        let x = h.read_field(env, 0).as_int();
        Value::int(x + args[0].as_int() + args[1].as_int())
    }

    /// The outer closure of `\x -> \a b -> x + a + b`: captures `x`, returns the arity-2 inner.
    fn make_env_add(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        let env = h.new_array(&[args[0]]);
        h.new_closure(env_add_body, 2, env.as_word()).as_word()
    }

    #[test]
    fn apply_saturates_exactly() {
        let mut h = Heap::new(64);
        let clo = h.new_closure(add2, 2, Value::unit());
        let r = h.apply(clo.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r.as_int(), 7);
    }

    #[test]
    fn apply_under_builds_a_pap_then_saturates() {
        let mut h = Heap::new(64);
        let clo = h.new_closure(add2, 2, Value::unit());

        // Only one of two args → a PAP, not a result.
        let pap = h.apply(clo.as_word(), &[Value::int(3)]);
        let pp = unsafe { HeapPtr::from_word(pap) };
        assert_eq!(h.header(pp).kind(), Kind::Pap);
        assert_eq!(h.read_raw(pp, 1), 1); // remaining_arity = 1

        // Supplying the last arg saturates through the PAP.
        let r = h.apply(pap, &[Value::int(4)]);
        assert_eq!(r.as_int(), 7);
    }

    #[test]
    fn apply_over_applies_through_the_result() {
        let mut h = Heap::new(64);
        // `make_adder` has arity 1 but is handed two args: it is called with `3` (yielding the inner
        // adder closure), and the trampoline then applies that to `4`.
        let f = h.new_closure(make_adder, 1, Value::unit());
        let r = h.apply(f.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r.as_int(), 7);
    }

    #[test]
    fn apply_curried_one_arg_at_a_time() {
        let mut h = Heap::new(64);
        let f = h.new_closure(make_adder, 1, Value::unit());
        let add1 = h.apply(f.as_word(), &[Value::int(3)]);
        let r = h.apply(add1, &[Value::int(39)]);
        assert_eq!(r.as_int(), 42);
    }

    #[test]
    fn apply_pap_re_under_applied_across_three_args() {
        // Feeding an arity-3 closure one arg at a time exercises the flatten → *re*-`new_pap` path:
        // the second `apply` under-applies a PAP (not a bare closure), so the PAP is rebuilt.
        let mut h = Heap::new(64);
        let f = h.new_closure(add3, 3, Value::unit());

        let p1 = h.apply(f.as_word(), &[Value::int(1)]);
        let pp1 = unsafe { HeapPtr::from_word(p1) };
        assert_eq!(h.header(pp1).kind(), Kind::Pap);
        assert_eq!(h.read_raw(pp1, 1), 2); // remaining 2

        let p2 = h.apply(p1, &[Value::int(2)]); // flatten PAP → still short → rebuild PAP
        let pp2 = unsafe { HeapPtr::from_word(p2) };
        assert_eq!(h.header(pp2).kind(), Kind::Pap);
        assert_eq!(h.read_raw(pp2, 1), 1); // remaining 1

        let r = h.apply(p2, &[Value::int(3)]);
        assert_eq!(r.as_int(), 6);
    }

    #[test]
    fn apply_pap_over_a_closure_with_env_preserves_captures() {
        // A PAP whose function is an env-carrying closure must keep that env reachable and correct
        // through the PAP → saturation path.
        let mut h = Heap::new(64);
        let mk = h.new_closure(make_env_add, 1, Value::unit());
        let inner = h.apply(mk.as_word(), &[Value::int(10)]); // arity-2 closure, env [10]

        let pap = h.apply(inner, &[Value::int(20)]); // under-apply → PAP over the env closure
        let pp = unsafe { HeapPtr::from_word(pap) };
        assert_eq!(h.header(pp).kind(), Kind::Pap);

        let r = h.apply(pap, &[Value::int(12)]); // 10 (env) + 20 + 12
        assert_eq!(r.as_int(), 42);
    }

    #[test]
    fn apply_survives_forced_gc_mid_evaluation() {
        // An over-application whose inner `code` (`make_adder`) allocates enough to force a collection
        // mid-`apply`. The leftover arg (`4`) is rooted across the `code` call and the captured env
        // (`[3]`) is kept live by `new_closure`'s self-rooting, so the result is still correct
        // (ADR-0066 §3/§4). The small heap guarantees the GC actually fires.
        let mut h = Heap::new(8);
        let mk = h.new_closure(make_adder, 1, Value::unit()); // 4 words
        let r = h.apply(mk.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r.as_int(), 7);
    }

    /// A `CodeFn` that holds a **heap-pointer arg** across an allocation *it* performs — the case the
    /// [`CodeFn`] rooting contract governs (ADR-0066 §3). It roots the arg, forces a collection, then
    /// reloads and reads it: `\n -> (force GC); Number (unbox n + 1)`.
    fn add_one_across_gc(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        let frame = h.frame();
        let n = h.root(args[0]); // a NumberBox pointer — must survive the collection below
        for _ in 0..8 {
            let _ = h.new_number(0.0); // unrooted garbage; overflows the small heap → forces GC
        }
        let np = unsafe { HeapPtr::from_word(h.get(n)) }; // reload the relocated arg
        let v = f64::from_bits(h.read_raw(np, 0));
        h.pop_frame(frame);
        h.new_number(v + 1.0).as_word()
    }

    #[test]
    fn codefn_roots_its_arg_across_its_own_gc() {
        // The `CodeFn` rooting contract end-to-end: the callee (not `apply`) roots its own heap-pointer
        // input across a collection it triggers, and the result is correct. A missed root here would be
        // a use-after-move Miri catches.
        let mut h = Heap::new(8);
        h.enable_stats_for_test();
        let clo = h.new_closure(add_one_across_gc, 1, Value::unit());
        let n = h.new_number(41.0);
        let r = h.apply(clo.as_word(), &[n.as_word()]);
        let rp = unsafe { HeapPtr::from_word(r) };
        assert_eq!(f64::from_bits(h.read_raw(rp, 0)), 42.0);
        // ADR-0102 §2 Verification: this exact-saturated call (arity 1, 1 arg) goes through the new
        // fast path, and a callee that roots its own heap-pointer arg across a GC it triggers still
        // works correctly from there.
        assert_eq!(h.stats().unwrap().entry_exact_fast_hits, 1);
    }

    #[test]
    #[should_panic(expected = "expected a pointer value")]
    fn apply_rejects_non_callable_immediate() {
        // Release-on: applying an immediate (here `unit`) must panic, not deref a bogus header.
        let mut h = Heap::new(16);
        h.apply(Value::unit(), &[]);
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn apply_rejects_interior_alias_pointer() {
        // Applying an in-bounds *interior* word (not an object header) must be rejected before the
        // header deref / transmute-and-call — the [P1] hole. Here we point one word into a live
        // closure's payload; a stale callee word can acquire exactly this shape after collect cycles.
        let mut h = Heap::new(64);
        let clo = h.new_closure(add2, 2, Value::unit());
        let word = core::mem::size_of::<u64>();
        let interior = crate::TaggedWord::from_addr(clo.as_ptr() as usize + word);
        h.apply(interior, &[Value::int(1), Value::int(2)]);
    }

    #[test]
    #[should_panic(expected = "not a live object")]
    fn apply_rejects_stale_pointer_after_collect() {
        // Holding a callee across a `collect` that relocates/drops it must be rejected, not deref a
        // forwarded header and transmute-and-call its forwarding address (UB). Root-reload contract.
        let mut h = Heap::new(64);
        let clo = h.new_closure(add2, 2, Value::unit());
        let stale = clo.as_word();
        let mut roots: [Value; 0] = []; // drop everything — `stale` now addresses the idle reserve
        h.collect(&mut roots);
        h.apply(stale, &[Value::int(1), Value::int(2)]);
    }

    // --- ADR-0102 §3 stats fixtures (deterministic counter values, index-path, Miri-safe) ----------

    /// A nullary leaf, for the zero-arity exact-dispatch fixture.
    fn nullary_leaf(_h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        Value::int(99)
    }

    /// A by-need suspension (arity-1, the ADR-0070 convention `force` always calls with one `unit`
    /// arg) that returns the arity-2 `add2` leaf when forced.
    fn suspend_add2(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        h.new_closure(add2, 2, Value::unit()).as_word()
    }

    #[test]
    fn fast_path_hits_on_a_plain_exact_call() {
        // ADR-0102 §2 Verification: the primary "the fast path actually fires" proof — a plain
        // exact-saturated call with no tail bounce goes through the new allocation-free entry, not
        // the generic loop.
        let mut h = Heap::new(64);
        h.enable_stats_for_test();
        let clo = h.new_closure(add2, 2, Value::unit());
        let r = h.apply(clo.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r.as_int(), 7);

        let s = h.stats().unwrap();
        assert_eq!(s.entry_exact_fast_hits, 1);
        assert_eq!(s.closure_exact_dispatches, 1);
        assert_eq!(s.closure_exact_positive, 1);
        assert_eq!(s.heap_apply_activations, 1);
        assert_eq!(
            s.pending_tail_apply_takes, 0,
            "no tail was stashed (index path)"
        );
    }

    #[test]
    fn stats_count_under_over_pap_and_exact_positive_dispatch() {
        let mut h = Heap::new(64);
        h.enable_stats_for_test();

        // Under-apply then saturate through the PAP (reuses `apply_under_builds_a_pap_then_saturates`'s
        // shape): one `under_apply`, then a PAP-flatten exact-positive dispatch.
        let clo = h.new_closure(add2, 2, Value::unit());
        let pap = h.apply(clo.as_word(), &[Value::int(3)]);
        let r1 = h.apply(pap, &[Value::int(4)]);
        assert_eq!(r1.as_int(), 7);

        // Over-apply (reuses `apply_over_applies_through_the_result`'s shape): one `over_apply`, then
        // an exact-positive dispatch on the resolved continuation.
        let f = h.new_closure(make_adder, 1, Value::unit());
        let r2 = h.apply(f.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r2.as_int(), 7);

        let s = h.stats().unwrap();
        assert_eq!(s.under_apply, 1);
        assert_eq!(s.over_apply, 1);
        assert_eq!(
            s.pap_dispatch, 1,
            "the PAP-flatten re-dispatch in the first scenario"
        );
        assert_eq!(
            s.closure_exact_dispatches, 2,
            "the PAP-flattened add2 call and the over-apply's resolved inner-adder call"
        );
        assert_eq!(s.closure_exact_positive, 2);
        assert_eq!(s.closure_exact_zero, 0);
        assert_eq!(
            s.entry_exact_fast_hits, 0,
            "neither top-level call is an exact match at entry: the first is under-applied (1 arg vs \
             arity 2), the second over-applied (2 args vs arity 1) — both exact dispatches here are \
             reached only after PAP-flatten/over-apply-resolve inside apply_loop, not at apply()'s entry"
        );
    }

    #[test]
    fn stats_count_pap_reflattening_when_still_under_applied() {
        // Reuses `apply_pap_re_under_applied_across_three_args`'s shape: feeding an arity-3 closure
        // one arg at a time exercises the flatten → *re*-under-apply → re-Pap path.
        let mut h = Heap::new(64);
        h.enable_stats_for_test();
        let f = h.new_closure(add3, 3, Value::unit());

        let p1 = h.apply(f.as_word(), &[Value::int(1)]); // under_apply → Pap
        let p2 = h.apply(p1, &[Value::int(2)]); // Pap dispatch → flatten → still short → re-Pap
        let r = h.apply(p2, &[Value::int(3)]); // Pap dispatch → flatten → exact
        assert_eq!(r.as_int(), 6);

        let s = h.stats().unwrap();
        assert_eq!(
            s.pap_dispatch, 2,
            "p1->p2 and p2->result each flatten a Pap"
        );
        assert_eq!(
            s.under_apply, 2,
            "the initial apply and the p1->p2 re-under-apply"
        );
        assert_eq!(
            s.closure_exact_dispatches, 1,
            "only the final call saturates"
        );
        assert_eq!(
            s.entry_exact_fast_hits, 0,
            "every call here starts from a Pap (kind mismatch) or an under-applied arity, so the \
             entry check always misses; the one exact dispatch is reached via PAP-flatten re-loop \
             inside apply_loop, never at apply()'s entry"
        );
    }

    #[test]
    fn stats_count_zero_arity_exact_dispatch() {
        let mut h = Heap::new(64);
        h.enable_stats_for_test();
        let clo = h.new_closure(nullary_leaf, 0, Value::unit());
        let r = h.apply(clo.as_word(), &[]);
        assert_eq!(r.as_int(), 99);
        let s = h.stats().unwrap();
        assert_eq!(s.closure_exact_zero, 1);
        assert_eq!(s.closure_exact_positive, 0);
        assert_eq!(s.closure_exact_dispatches, 1);
        // ADR-0102 §2: a zero-arity exact call (`args == &[]`) is still an exact match at entry.
        assert_eq!(s.entry_exact_fast_hits, 1);
    }

    #[test]
    fn stats_count_byneed_dispatch_and_nested_activation() {
        let mut h = Heap::new(64);
        h.enable_stats_for_test();
        let suspension = h.new_closure(suspend_add2, 1, Value::unit());
        let cell = h.new_byneed(suspension.as_word());
        let r = h.apply(cell.as_word(), &[Value::int(3), Value::int(4)]);
        assert_eq!(r.as_int(), 7);

        let s = h.stats().unwrap();
        assert_eq!(s.byneed_dispatch, 1);
        // `force` re-enters `apply` on the suspension as a distinct call: this top-level activation,
        // plus `force`'s own nested `apply(suspension, [unit])` — 2 activations total (both within
        // the *same* top-level call still count as one activation each; `force`'s call is the only
        // nested one here, per the ADR-0102 §3 note on the `Kind::ByNeed` arm).
        assert_eq!(s.heap_apply_activations, 2);
        // both the nested arity-1 suspension dispatch and the re-dispatched arity-2 `add2` call are
        // exact-saturated positive-arity dispatches.
        assert_eq!(s.closure_exact_dispatches, 2);
        assert_eq!(s.closure_exact_positive, 2);
        // ADR-0102 §2: the *nested* `force`-triggered `apply(suspension, [unit])` call is itself an
        // exact match at its own entry (arity 1, 1 arg) and fast-path hits; the outer top-level call
        // does not (its callee is `Kind::ByNeed`, not `Closure`), so the total is 1, not 2.
        assert_eq!(s.entry_exact_fast_hits, 1);
    }
}
