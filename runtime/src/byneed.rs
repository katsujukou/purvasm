//! v1 by-need recursive CAFs (ADR-0070): [`Heap::force`] — the black-hole / memoise operation on a
//! `ByNeed` cell (ADR-0070 §2) — and the recursive-group (`Grec`) construction it supports (§4).
//!
//! `apply` auto-forces a `ByNeed` callee (see `apply`); a by-need dereference at a record projection
//! is an explicit `force` the codegen emits. A cell is a 3-state memoising suspension: `Unforced`
//! holds a nullary thunk closure in `result`; `force` runs it once, memoises the value, and marks
//! `Forced`; a re-entrant force (`Building`) is a genuine cycle and black-holes.

use crate::gc::{Heap, BYNEED_BUILDING, BYNEED_FORCED, BYNEED_UNFORCED};
use crate::heap::{HeapPtr, Kind};
use crate::Value;

impl Heap {
    /// Force a by-need cell to its value (ADR-0070 §2). Memoises on first force; a force while the
    /// cell is already being forced is a genuine cycle and black-holes. Safe public API — it
    /// **release-validates** its input first: `cell` is a live `ByNeed` object and its `state` is one
    /// of the three legal values (a `state` word corrupted via the public `write_raw` faults here).
    pub fn force(&mut self, cell: Value) -> Value {
        self.trace_value("force cell", cell);
        let cp = self.checked_ptr(cell);
        assert_eq!(
            self.header_unchecked(cp).kind(),
            Kind::ByNeed,
            "force: not a ByNeed cell"
        );
        match self.read_raw_unchecked(cp, 0) {
            BYNEED_FORCED => self.read_field_unchecked(cp, 1),
            BYNEED_BUILDING => panic!(
                "force: black-hole — a by-need cell forced during its own construction (a genuine cycle)"
            ),
            BYNEED_UNFORCED => {
                // Mark Building (the black-hole guard), then run the suspension via the eval/apply
                // path (a nullary thunk, exactly like an Effect thunk). `apply` is a safepoint, so the
                // cell is rooted across it and reloaded before the memoise write (ADR-0066 §3).
                self.write_raw_unchecked(cp, 0, BYNEED_BUILDING);
                let suspension = self.read_field_unchecked(cp, 1);
                let frame = self.frame();
                let cr = self.root(cell);
                let v = self.apply(suspension, &[Value::unit()]);
                let cp2 = unsafe { HeapPtr::from_word(self.get(cr)) };
                self.write_field_unchecked(cp2, 1, v);
                self.write_raw_unchecked(cp2, 0, BYNEED_FORCED);
                self.pop_frame(frame);
                v
            }
            other => panic!("force: corrupt ByNeed state {other}"),
        }
    }

    /// Force `v` **iff it is a `ByNeed` cell**, looping through a chain of cells to the underlying value;
    /// any non-`ByNeed` value passes through unchanged (ADR-0070 §3). Codegen emits this at a
    /// value-dereference site (a record projection, a `case` scrutinee, a primop operand) because a
    /// by-need cell can reach such a site through a function argument or a data field, where static
    /// by-need tracking cannot see it — the same robustness `apply` gives a by-need callee.
    pub fn force_if_byneed(&mut self, v: Value) -> Value {
        let mut v = v;
        while v.is_pointer() {
            let p = self.checked_ptr(v);
            if self.header_unchecked(p).kind() != Kind::ByNeed {
                break;
            }
            v = self.force(v); // a self-cycle black-holes inside `force`, so this terminates
        }
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CodeFn;

    /// Build a recursive group of `codes.len()` by-need cells over a shared `env` `Array` that holds
    /// every cell (each suspension is `codes[i]`, capturing that env) — the ADR-0070 §4 procedure:
    /// placeholder cells first (rooted), backpatched with their suspensions once `env` is complete.
    /// Returns the cell `Value`s. (Codegen emits this; hand-built here.)
    fn build_group(h: &mut Heap, codes: &[CodeFn]) -> Vec<Value> {
        let n = codes.len();
        let frame = h.frame();
        let env = h.new_array(&vec![Value::unit(); n]);
        let er = h.root(env.as_word());
        let mut cell_roots = Vec::with_capacity(n);
        for i in 0..n {
            let cell = h.new_byneed_placeholder();
            let cr = h.root(cell.as_word());
            let envp = unsafe { HeapPtr::from_word(h.get(er)) };
            let cell_w = h.get(cr);
            h.write_field(envp, i as u64, cell_w); // env[i] = cell_i (a plain store)
            cell_roots.push(cr);
        }
        for i in 0..n {
            let env_now = h.get(er);
            let susp = h.new_closure(codes[i], 1, env_now).as_word();
            let cellp = unsafe { HeapPtr::from_word(h.get(cell_roots[i])) };
            h.byneed_set_suspension(cellp, susp);
        }
        let cells = cell_roots.iter().map(|r| h.get(*r)).collect();
        h.pop_frame(frame);
        cells
    }

    /// A suspension that allocates a fresh `Number 3.5` each time it runs (so identity distinguishes a
    /// re-run from a memoised read).
    fn make_num(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        h.new_number(3.5).as_word()
    }

    #[test]
    fn force_memoises() {
        let mut h = Heap::new(64);
        let susp = h.new_closure(make_num, 1, Value::unit()).as_word();
        let cell = h.new_byneed(susp).as_word();
        let v1 = h.force(cell);
        let v2 = h.force(cell);
        assert_eq!(v1, v2); // physical identity — the suspension ran once, memoised
        let np = unsafe { HeapPtr::from_word(v1) };
        assert_eq!(f64::from_bits(h.read_raw(np, 0)), 3.5);
    }

    /// `A` forces its sibling `B` (env[1]) during its own construction and returns `B + 1` — the
    /// knot-tying shape (ADR-0032): `B` completes without forcing `A` back.
    fn a_forces_b(h: &mut Heap, clo: Value, _args: &[Value]) -> Value {
        let clo = unsafe { HeapPtr::from_word(clo) };
        let env = unsafe { HeapPtr::from_word(h.read_field(clo, 2)) };
        let cell_b = h.read_field(env, 1);
        Value::int(h.force(cell_b).as_int() + 1)
    }
    fn b_returns_42(_h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        Value::int(42)
    }

    #[test]
    fn force_knot_tying_group_completes_and_memoises() {
        let mut h = Heap::new(64);
        let cells = build_group(&mut h, &[a_forces_b, b_returns_42]);
        let (a, b) = (cells[0], cells[1]);
        assert_eq!(h.force(a).as_int(), 43); // A forces B (42) → 43; the group closes, no black-hole
        assert_eq!(h.force(b).as_int(), 42); // B already forced (memoised)
        assert_eq!(h.force(a).as_int(), 43); // A memoised — no re-run
    }

    /// A suspension that forces its own cell (env[0]) — a genuine self-cycle.
    fn forces_self(h: &mut Heap, clo: Value, _args: &[Value]) -> Value {
        let clo = unsafe { HeapPtr::from_word(clo) };
        let env = unsafe { HeapPtr::from_word(h.read_field(clo, 2)) };
        let self_cell = h.read_field(env, 0);
        h.force(self_cell)
    }

    #[test]
    #[should_panic(expected = "black-hole")]
    fn force_self_cycle_black_holes() {
        let mut h = Heap::new(64);
        let cells = build_group(&mut h, &[forces_self]);
        let _ = h.force(cells[0]);
    }

    /// An arity-1 closure `\x -> x + 1`.
    fn add1(_h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        Value::int(args[0].as_int() + 1)
    }
    /// A suspension whose forced value is the `add1` closure.
    fn make_add1(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        h.new_closure(add1, 1, Value::unit()).as_word()
    }

    #[test]
    fn apply_auto_forces_byneed_callee() {
        // A by-need value used as a function: `apply` forces it to the `add1` closure, then applies.
        let mut h = Heap::new(64);
        let susp = h.new_closure(make_add1, 1, Value::unit()).as_word();
        let cell = h.new_byneed(susp).as_word();
        assert_eq!(h.apply(cell, &[Value::int(41)]).as_int(), 42);
    }

    /// A suspension that forces a collection (garbage) then returns a boxed `Number 7.0`.
    fn boxed_forcing_gc(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        for _ in 0..8 {
            let _ = h.new_number(0.0);
        }
        h.new_number(7.0).as_word()
    }

    #[test]
    fn force_survives_forced_gc() {
        // The suspension collects mid-force; the cell is rooted across `apply` and reloaded before the
        // memoise write, and the boxed result survives (ADR-0070 §2 / ADR-0066).
        let mut h = Heap::new(24);
        let susp = h.new_closure(boxed_forcing_gc, 1, Value::unit()).as_word();
        let cell = h.new_byneed(susp).as_word();
        let v = h.force(cell);
        let np = unsafe { HeapPtr::from_word(v) };
        assert_eq!(f64::from_bits(h.read_raw(np, 0)), 7.0);
    }

    #[test]
    #[should_panic(expected = "not a ByNeed")]
    fn force_rejects_non_byneed() {
        let mut h = Heap::new(16);
        let n = h.new_number(1.0);
        let _ = h.force(n.as_word());
    }
}
