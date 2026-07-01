//! The v1 eval/apply calling convention (ADR-0064 §3).
//!
//! [`Heap::apply`] is the single generic entry all v1 calls go through (the direct known-arity fast
//! path is deferred). It dispatches on the callee's arity: **saturate** (exactly enough args → call
//! the code), **over-apply** (too many → call, then `apply` the result to the rest), or
//! **under-apply** (too few → build a `PAP`). Tail calls and over-application chains run as a
//! **trampoline** (v1 TCE, ADR-0064 §4; `musttail` is deferred with the direct fast path).
//!
//! v1 boundaries (lifted with later increments):
//! - `apply` materialises `&mut Heap` and threads it into each [`CodeFn`] by reborrow — sound, one
//!   `&mut` active per level. The C-ABI (`extern "C"`, a raw heap-context pointer, args as ptr+len)
//!   arrives with codegen.
//! - The in-flight argument [`Value`]s are **not GC-rooted**. This is safe *only* because no
//!   collection can occur mid-`apply` in v1 (`alloc` panics on overflow rather than collecting);
//!   shadow-stack rooting of these lands with GC-on-alloc.
//! - The working argument list is a `Vec` for simplicity; codegen lowers it to a stack array.

use crate::gc::Heap;
use crate::heap::{HeapPtr, Kind};
use crate::Value;

/// The generated-code entry a [`Closure`](Kind::Closure) points to. It receives the runtime, the
/// closure itself (for its env / free variables), and exactly `arity` arguments, and returns the
/// result. (v1 uses a Rust fn pointer over `&mut Heap`; codegen adopts the `extern "C"` + raw-pointer
/// ABI.)
pub type CodeFn = fn(heap: &mut Heap, closure: Value, args: &[Value]) -> Value;

impl Heap {
    /// A `Closure` over the type-safe [`CodeFn`] entry. `env` is a shared env block's pointer word
    /// (via [`HeapPtr::as_word`]) or an immediate sentinel for a no-capture closure. The safe wrapper
    /// over [`Heap::new_closure_raw`]: it interns `code` into the heap's code table and stores the
    /// resulting index, so `apply` recovers the real fn pointer (with provenance) by a bounds-checked
    /// lookup — never an integer→fn transmute, which would be a provenance-free (UB) call.
    pub fn new_closure(&mut self, code: CodeFn, arity: u32, env: Value) -> HeapPtr {
        let idx = self.intern_code(code);
        // SAFETY: `idx` is a valid code-table index by construction.
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
                        // Under-apply: a PAP capturing the args supplied so far.
                        let remaining = (arity - args.len()) as u32;
                        return self.new_pap(f, remaining, &args).as_word();
                    }
                    // Saturate: call the code with exactly `arity` args (reborrowing `&mut *self`).
                    // The `code` word is a code-table index (v1); `code_at` recovers the real fn
                    // pointer with a bounds check — no integer→fn transmute (which would call a
                    // provenance-free pointer, UB). Copy the `CodeFn` out before the `&mut self` call.
                    let code = self.code_at(self.read_raw_unchecked(p, 0));
                    let result = code(self, f, &args[..arity]);
                    if args.len() == arity {
                        return result;
                    }
                    // Over-apply: `result` must itself be callable; continue with the leftover args.
                    f = result;
                    args = args[arity..].to_vec();
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
