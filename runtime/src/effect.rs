//! v1 `Effect` execution (ADR-0067): the `run_effect` driver and the native `Effect.Ref` leaf
//! performers.
//!
//! `Effect a` is an ordinary nullary thunk — an arity-1 closure over `unit` (ADR-0067 §1). The
//! `Effect` monad (`pureE`/`bindE`) is the compiler's structural guest code, so this module
//! implements only the **driver** and the effectful leaves' **performers** (the actual IO). Forcing a
//! thunk is `apply(thunk, [unit])`. Each performer obeys the ADR-0066 §3 per-function rooting
//! contract — see [`Heap::ref_modify`], whose `apply f` is a safepoint.
//!
//! `Effect.Ref` is the core bring-up vehicle (runtime state + evaluation order). The one IO leaf,
//! [`Heap::stdio_write_line`], is a **generic write-line scaffold for the FFI-absent period**, not a
//! permanent runtime feature — the runtime does not know `Effect.Console.log` (that is a `ulib`
//! shadow over a `Purvasm.Stdio.writeLine`-style API; ADR-0067 §5).
//!
//! Scope (ADR-0067 §2): `run_effect` + the `Ref` leaves + the write-line leaf + thunk forcing. The
//! recursive `Monad Effect` dictionary (needing by-need `ByNeed`-force, ADR-0024/0032) is a separate
//! increment; v1's first run targets are hand-built / leaf-driven.

use crate::gc::Heap;
use crate::heap::HeapPtr;
use crate::Value;

impl Heap {
    /// Run a top-level `Effect a` to completion (ADR-0067 §2): force the entry thunk by applying it to
    /// `unit`, returning the final value. Direct-style — the structural `bindE` chain (or, in an
    /// impurified body, straight-line performer calls) sequences sub-effects under strict `apply`.
    pub fn run_effect(&mut self, main: Value) -> Value {
        self.apply(main, &[Value::unit()])
    }

    /// `Effect.Ref.new`'s performer: a fresh `Ref` cell holding `init` (the ADR-0019 one-cell vehicle).
    #[inline]
    pub fn ref_new(&mut self, init: Value) -> Value {
        self.new_ref(init).as_word()
    }

    /// `Effect.Ref.read`'s performer: the cell's current value. No allocation, no rooting needed.
    #[inline]
    pub fn ref_read(&self, r: Value) -> Value {
        // SAFETY: `r` is a `Ref` pointer; `read_field` validates it is a live object header first.
        self.read_field(unsafe { HeapPtr::from_word(r) }, 0)
    }

    /// `Effect.Ref.write`'s performer: store `x` into the cell. No allocation.
    #[inline]
    pub fn ref_write(&mut self, r: Value, x: Value) {
        // SAFETY: as `ref_read`.
        self.write_field(unsafe { HeapPtr::from_word(r) }, 0, x);
    }

    /// `Effect.Ref.modify`'s performer: `r := f (read r)`, returning `unit`.
    ///
    /// `apply f` is a **safepoint** (ADR-0067 §4): it can relocate the `Ref` cell. So the cell is
    /// **rooted across the `apply` and reloaded before the write** (ADR-0066 §3) — writing through a
    /// pointer captured *before* `apply f` would target a stale cell. The read value (`old`) needs no
    /// rooting *here*: `ref_modify` does not use it after `apply f`, and if `f` holds it across a
    /// safepoint *inside* `f`, rooting it is `f`'s own per-function responsibility, not the caller's
    /// (ADR-0066 §3). This is the earliest place a real effect program trips the safepoint rule.
    pub fn ref_modify(&mut self, r: Value, f: Value) -> Value {
        let frame = self.frame();
        let rr = self.root(r);
        let old = self.ref_read(r);
        let new = self.apply(f, &[old]); // safepoint: may relocate the cell
        let r = self.get(rr); // reload the (possibly moved) Ref before writing
        self.ref_write(r, new);
        self.pop_frame(frame);
        Value::unit()
    }

    /// The **native stdio write-line leaf** (ADR-0067 §5): append `s`'s text to the output sink as one
    /// line, return `unit`. This is the runtime's one *generic* IO leaf and a **temporary
    /// boot-parity / smoke-test scaffold for the FFI-absent period** — *not* a permanent first-class
    /// runtime primitive. The runtime deliberately does **not** know `Effect.Console.log` (a JS-derived
    /// name and shape); that is realised as a `ulib` **shadow** over a `Purvasm.Stdio.writeLine`-style
    /// API which calls this leaf. When native (user-defined C) FFI lands, stdio drops to an ordinary
    /// library leaf and the runtime forgets this name (the eventual `extern "C"` entry is
    /// `purvasm_stdio_write_line`).
    ///
    /// Reads the `Str` into an owned line (host memory, no GC allocation), so no rooting is needed.
    ///
    /// **Two destinations by heap kind.** On a **native** heap (`new_native`, the compiled binary)
    /// the line is written to real `stdout` and flushed **per call** — progressive output, correct
    /// interleaving with the live-flushing `stderr` leaf, and no loss of buffered stdout when `exit`
    /// skips the drain (ADR-0074 §5); this mirrors `writeErrLine`. On a **lib/test** heap
    /// (`new`, the Miri/differential path) the line is appended to the capture sink the Rust unit
    /// tests read via [`output`](Heap::output) — testability unchanged. (The native binary's stdout
    /// is captured by the shell in the boot e2e differential, not by the sink, so that gate is
    /// unaffected either way.) A write failure is a fatal abort, as for `pv_drain_output`.
    pub fn stdio_write_line(&mut self, s: Value) -> Value {
        // SAFETY: `s` is a string (`Str | StrSlice`) pointer; `str_read` normalises through the
        // view and validates the object header / kind (ADR-0103).
        let line = self.str_read(unsafe { HeapPtr::from_word(s) });
        if self.code_is_address() {
            use std::io::Write;
            let out = std::io::stdout();
            let mut lock = out.lock();
            writeln!(lock, "{line}").expect("stdio_write_line: stdout write failed");
            lock.flush().expect("stdio_write_line: stdout flush failed");
        } else {
            self.push_output(line);
        }
        Value::unit()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CodeFn;

    /// A `+1` leaf that does not allocate.
    fn inc_body(_h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        Value::int(args[0].as_int() + 1)
    }

    /// A `+1` leaf that **forces a collection** before returning — exercises `ref_modify`'s rooting of
    /// the `Ref` cell across `apply f`. `args[0]` is an `Int` immediate, so this callee holds no heap
    /// value across its own allocation (nothing to root on this side).
    fn inc_body_forcing_gc(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        for _ in 0..8 {
            let _ = h.new_number(0.0); // unrooted garbage → overflows a small heap → GC
        }
        Value::int(args[0].as_int() + 1)
    }

    /// The shared body of an impurified `main :: Effect Int`:
    /// `do r <- new 0; modify (+1) r; modify (+1) r; read r` = `2`, direct-style (as impurified codegen
    /// emits). It self-roots `r` and the `+1` closure across the `modify` safepoints (the `CodeFn`
    /// rooting contract, ADR-0066 §3), reloading them after each.
    fn run_ref_main(h: &mut Heap, inc: CodeFn) -> Value {
        let frame = h.frame();
        let r0 = h.ref_new(Value::int(0));
        let r = h.root(r0);
        let c0 = h.new_closure(inc, 1, Value::unit()).as_word();
        let f = h.root(c0);

        let (rv, fv) = (h.get(r), h.get(f));
        h.ref_modify(rv, fv);
        let (rv, fv) = (h.get(r), h.get(f));
        h.ref_modify(rv, fv);

        let result = h.ref_read(h.get(r));
        h.pop_frame(frame);
        result
    }

    fn ref_main(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        run_ref_main(h, inc_body)
    }

    fn ref_main_forcing_gc(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        run_ref_main(h, inc_body_forcing_gc)
    }

    fn pure_seven(_h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        Value::int(7)
    }

    #[test]
    fn run_effect_forces_the_entry_thunk() {
        // `run_effect(pure 7)` = force `\_ -> 7` → 7. The minimal driver check.
        let mut h = Heap::new(16);
        let main = h.new_closure(pure_seven, 1, Value::unit());
        assert_eq!(h.run_effect(main.as_word()).as_int(), 7);
    }

    #[test]
    fn run_effect_ref_sequence_returns_value() {
        // A stateful pure-`Effect` program returns its final value — sequencing + state observed as a
        // value, no stdout (ADR-0023 / ADR-0067 §4).
        let mut h = Heap::new(64);
        let main = h.new_closure(ref_main, 1, Value::unit());
        assert_eq!(h.run_effect(main.as_word()).as_int(), 2);
    }

    #[test]
    fn ref_modify_roots_cell_across_forced_gc() {
        // `modify`'s `apply f` forces a collection; the rooted `Ref` cell survives and is reloaded
        // before the write, so the result is still correct (ADR-0067 §4 / ADR-0066 §3). The small heap
        // guarantees the GC fires inside `apply f`.
        let mut h = Heap::new(16);
        let main = h.new_closure(ref_main_forcing_gc, 1, Value::unit());
        assert_eq!(h.run_effect(main.as_word()).as_int(), 2);
    }

    // --- stdio write-line leaf (ADR-0067 §5): the curried "leaf returns thunk; force performs" shape

    /// The performing thunk of `writeLine`: `\_ -> perform (write_line s)`, reading `s` from its env.
    fn write_line_thunk(h: &mut Heap, clo: Value, _args: &[Value]) -> Value {
        let clo = unsafe { HeapPtr::from_word(clo) };
        let env = unsafe { HeapPtr::from_word(h.read_field(clo, 2)) };
        let s = h.read_field(env, 0);
        h.stdio_write_line(s)
    }

    /// The outer leaf `writeLine :: String -> Effect Unit`: `\s -> thunk` capturing `s` in the env.
    fn write_line_outer(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        let env = h.new_array(&[args[0]]); // self-roots `s` across the allocation
        h.new_closure(write_line_thunk, 1, env.as_word()).as_word()
    }

    #[test]
    fn write_line_leaf_forces_and_captures() {
        // `run_effect (writeLine "hello")` — `writeLine "hello"` returns the thunk; `run_effect` forces
        // it, which performs the write. The captured sink holds one line.
        let mut h = Heap::new(64);
        let write_line = h.new_closure(write_line_outer, 1, Value::unit());
        let s = h.new_str(b"hello");
        let eff = h.apply(write_line.as_word(), &[s.as_word()]); // the `Effect Unit` thunk
        let r = h.run_effect(eff);
        assert!(r.is_immediate()); // unit
        assert_eq!(h.output(), &["hello".to_string()]);
    }

    /// An impurified `main :: Effect Unit` writing two lines in order (direct performer calls). `sa` is
    /// used before `sb` is allocated, so it never spans a safepoint — no rooting needed.
    fn write_twice_main(h: &mut Heap, _clo: Value, _args: &[Value]) -> Value {
        let sa = h.new_str(b"first");
        h.stdio_write_line(sa.as_word());
        let sb = h.new_str(b"second");
        h.stdio_write_line(sb.as_word());
        Value::unit()
    }

    #[test]
    fn stdio_write_line_preserves_program_order() {
        let mut h = Heap::new(64);
        let main = h.new_closure(write_twice_main, 1, Value::unit());
        h.run_effect(main.as_word());
        assert_eq!(h.output(), &["first".to_string(), "second".to_string()]);
    }

    #[test]
    #[should_panic(expected = "non-string")]
    fn stdio_write_line_rejects_non_str_argument() {
        // The leaf is safe public API: handed a non-`Str` value it must reject the kind (release-on),
        // not read a bogus length and slice out of bounds.
        let mut h = Heap::new(16);
        let n = h.new_number(1.0);
        h.stdio_write_line(n.as_word());
    }

    #[test]
    fn write_line_leaf_survives_forced_gc() {
        // `writeLine`'s outer leaf allocates the thunk (env + closure); a small heap forces a
        // collection there. The captured string survives (in the self-rooted env), and forcing the
        // thunk still writes it correctly.
        let mut h = Heap::new(12);
        let write_line = h.new_closure(write_line_outer, 1, Value::unit());
        let s = h.new_str(b"hi");
        let eff = h.apply(write_line.as_word(), &[s.as_word()]); // GC fires inside `write_line_outer`
        h.run_effect(eff);
        assert_eq!(h.output(), &["hi".to_string()]);
    }
}
