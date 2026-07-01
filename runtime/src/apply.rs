//! The v1 eval/apply calling convention (ADR-0064 §3).
//!
//! [`Heap::apply`] is the single generic entry all v1 calls go through (the direct known-arity fast
//! path is deferred). It dispatches on the callee's arity: **saturate** (exactly enough args → call
//! the code), **over-apply** (too many → call, then `apply` the result to the rest), or
//! **under-apply** (too few → build a `PAP`). Tail calls and over-application chains run as a
//! **trampoline** (v1 TCE, ADR-0064 §4; `musttail` is deferred with the direct fast path).
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

use crate::gc::{Heap, Root};
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

    /// Apply callable `f` to `args` (ADR-0064 §3). `f` must be a [`Closure`](Kind::Closure) or a
    /// [`Pap`](Kind::Pap); applying anything else is a codegen bug and panics.
    pub fn apply(&mut self, f: Value, args: &[Value]) -> Value {
        let mut f = f;
        let mut args: Vec<Value> = args.to_vec();
        loop {
            // `apply` is safe public API. A non-pointer, foreign/forged, stale, or forwarded callee
            // would make the header deref below — and the transmute-and-call of the code word — UB.
            // `checked_ptr` release-rejects all of those before any dereference (ADR-0064 §4).
            let p = self.checked_ptr(f);
            // `p` is now a validated live object header, so the unchecked field tier is sound here
            // (and avoids re-walking the object-start check on every field — see the tier note in
            // `gc`). The `header_unchecked` read is what `checked_ptr` licensed.
            match self.header_unchecked(p).kind() {
                // A by-need callee (a projected dictionary member used as a function) is forced, then
                // re-dispatched on the forced value (ADR-0070 §3). `force` is a safepoint, so the
                // in-flight `args` are rooted across it and reloaded (`f` is replaced by the result).
                Kind::ByNeed => {
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
                        // Under-apply: a PAP capturing the args supplied so far. `new_pap` self-roots
                        // `f` and `args` across its own allocation (ADR-0066 §3), so nothing to root here.
                        let remaining = (arity - args.len()) as u32;
                        return self.new_pap(f, remaining, &args).as_word();
                    }
                    // Saturate / over-apply. `code(…)` is a safepoint (ADR-0066 §3). Between reading
                    // `f`/`args` (at `checked_ptr` above) and this call nothing allocates, so the call
                    // slice snapshot is current. The only values that must survive `code` are the
                    // **leftover** args (read after the call on over-application) — `f` is not read
                    // after `code`, and `result` is `code`'s own (post-collection) return. Root the
                    // leftovers, call, then reload them.
                    let call_args: Vec<Value> = args[..arity].to_vec();
                    let frame = self.frame();
                    let mut leftover: Vec<Root> = Vec::with_capacity(args.len() - arity);
                    for a in &args[arity..] {
                        leftover.push(self.root(*a));
                    }
                    // The `code` word is a code-table index (v1); `code_at` recovers the real fn
                    // pointer with a bounds check — no integer→fn transmute (a provenance-free call, UB).
                    let code = self.code_at(self.read_raw_unchecked(p, 0));
                    let result = code(self, f, &call_args);
                    if leftover.is_empty() {
                        self.pop_frame(frame);
                        return result;
                    }
                    // Over-apply: reload the (possibly relocated) leftovers, continue with `result`.
                    let mut next_args: Vec<Value> = Vec::with_capacity(leftover.len());
                    for r in &leftover {
                        next_args.push(self.get(*r));
                    }
                    self.pop_frame(frame);
                    f = result;
                    args = next_args;
                }
                other => panic!("apply: not a callable value (kind {other:?})"),
            }
        }
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
        let clo = h.new_closure(add_one_across_gc, 1, Value::unit());
        let n = h.new_number(41.0);
        let r = h.apply(clo.as_word(), &[n.as_word()]);
        let rp = unsafe { HeapPtr::from_word(r) };
        assert_eq!(f64::from_bits(h.read_raw(rp, 0)), 42.0);
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
}
