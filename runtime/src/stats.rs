//! Opt-in dynamic-apply/GC counters (ADR-0102 §3): a fixed, machine-readable baseline so the
//! exact-saturated closure fast path (ADR-0102 §2, a later increment) can be judged by measured
//! event-mix deltas rather than guessed. Disabled by default; enabled per [`crate::gc::Heap`]
//! context via the `PURVASM_STATS` environment variable ([`parse_stats_env`]).
//!
//! This module is pure (no `Heap`/environment access) so [`Stats`] and its formatting are unit-tested
//! directly; [`crate::gc::Heap`] owns the actual counter storage and the env-var read.

/// Per-runtime-context dynamic-apply/GC counters (ADR-0102 §3). Every field is an unsigned
/// saturating counter; `gc_max_live_words` is the one running-max exception (see
/// [`record_gc`](Stats::record_gc)). Field order matches the emitted schema order exactly.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Stats {
    /// `pv_apply` ABI entries — increments **only** in the exported entry, not every
    /// [`Heap::apply`](crate::gc::Heap::apply) activation (`pv_settle`'s slow path and internal
    /// runtime helpers reach `apply` without going through `pv_apply`).
    pub pv_apply_entries: u64,
    /// [`Heap::apply`](crate::gc::Heap::apply) activations — once per call, regardless of caller
    /// (`pv_apply`, `pv_settle`, `force`, or any other internal helper).
    pub heap_apply_activations: u64,
    /// `Kind::Closure` dispatches with `args.len == arity` exactly (zero + positive, below).
    pub closure_exact_dispatches: u64,
    /// The zero-arity subset of [`closure_exact_dispatches`](Stats::closure_exact_dispatches).
    pub closure_exact_zero: u64,
    /// The positive-arity subset of [`closure_exact_dispatches`](Stats::closure_exact_dispatches).
    pub closure_exact_positive: u64,
    /// Hits of the pre-loop borrowed-slice exact-saturated fast path (ADR-0102 §2). Always `0` in
    /// this slice — the fast path itself is a later increment; the field exists now so the schema
    /// is stable across it.
    pub entry_exact_fast_hits: u64,
    /// `Kind::Closure` dispatches with `args.len < arity` (under-application, builds a `Pap`).
    pub under_apply: u64,
    /// `Kind::Closure` dispatches with `args.len > arity` (over-application).
    pub over_apply: u64,
    /// `Kind::Pap` dispatches (flatten to the underlying function).
    pub pap_dispatch: u64,
    /// `Kind::ByNeed` dispatches (force, then re-dispatch on the forced value).
    pub byneed_dispatch: u64,
    /// `pv_tailcall` writes to the pending-tail slot.
    pub pv_tailcall_writes: u64,
    /// Pending-tail takes by [`Heap::apply`](crate::gc::Heap::apply)'s own trampoline loop.
    pub pending_tail_apply_takes: u64,
    /// Pending-tail takes by `pv_settle`.
    pub pending_tail_settle_takes: u64,
    /// `pv_settle` fast path (`pending_tail == None`).
    pub pv_settle_fast: u64,
    /// `pv_settle` slow path (`pending_tail == Some`, enters
    /// [`Heap::apply`](crate::gc::Heap::apply)). Numerically identical to
    /// [`pending_tail_settle_takes`](Stats::pending_tail_settle_takes) by construction — `pv_settle`
    /// has exactly one pending-tail take site — landed as two fields per the ADR schema; this is an
    /// intentional fusion, not a bug to deduplicate.
    pub pv_settle_slow: u64,
    /// `Heap::gc` invocations (the real overflow-triggered collector entry; the `#[cfg(test)]`-only
    /// explicit-root `Heap::collect` escape hatch is not counted).
    pub gc_collections: u64,
    /// Total time spent inside `Heap::gc` (collection, semi-space swap, post-collection live-word
    /// accounting, object-start rebuild) — not stats summary formatting.
    pub gc_total_ns: u64,
    /// Accumulated post-collection live words across every collection.
    pub gc_copied_words: u64,
    /// The maximum post-collection live words observed across collections (a running max, not a
    /// sum — see [`record_gc`](Stats::record_gc)).
    pub gc_max_live_words: u64,
}

impl Stats {
    /// Record one `Kind::Closure` exact-saturated dispatch (`args.len == arity`): bump the total and
    /// its zero/positive-arity split together, so a call site cannot update one and forget the other.
    pub(crate) fn record_closure_exact(&mut self, arity: u32) {
        self.closure_exact_dispatches = self.closure_exact_dispatches.saturating_add(1);
        if arity == 0 {
            self.closure_exact_zero = self.closure_exact_zero.saturating_add(1);
        } else {
            self.closure_exact_positive = self.closure_exact_positive.saturating_add(1);
        }
    }

    /// Record one `Heap::gc` invocation: bump the collection count and accumulated time/words, and
    /// max in this collection's live-word count — bundled so a call site cannot update the sum and
    /// forget the max (or vice versa).
    pub(crate) fn record_gc(&mut self, ns: u64, live_words: u64) {
        self.gc_collections = self.gc_collections.saturating_add(1);
        self.gc_total_ns = self.gc_total_ns.saturating_add(ns);
        self.gc_copied_words = self.gc_copied_words.saturating_add(live_words);
        self.gc_max_live_words = self.gc_max_live_words.max(live_words);
    }

    /// The `purvasm-stats:v1` schema line (ADR-0102 §3), **without** a trailing newline — the caller
    /// (`pv_runtime_free`) adds exactly one via `eprintln!`. Keys are append-only within `v1`; a
    /// breaking schema change bumps the prefix version.
    pub(crate) fn format(&self) -> String {
        format!(
            "purvasm-stats:v1 pv_apply_entries={} heap_apply_activations={} closure_exact_dispatches={} closure_exact_zero={} closure_exact_positive={} entry_exact_fast_hits={} under_apply={} over_apply={} pap_dispatch={} byneed_dispatch={} pv_tailcall_writes={} pending_tail_apply_takes={} pending_tail_settle_takes={} pv_settle_fast={} pv_settle_slow={} gc_collections={} gc_total_ns={} gc_copied_words={} gc_max_live_words={}",
            self.pv_apply_entries,
            self.heap_apply_activations,
            self.closure_exact_dispatches,
            self.closure_exact_zero,
            self.closure_exact_positive,
            self.entry_exact_fast_hits,
            self.under_apply,
            self.over_apply,
            self.pap_dispatch,
            self.byneed_dispatch,
            self.pv_tailcall_writes,
            self.pending_tail_apply_takes,
            self.pending_tail_settle_takes,
            self.pv_settle_fast,
            self.pv_settle_slow,
            self.gc_collections,
            self.gc_total_ns,
            self.gc_copied_words,
            self.gc_max_live_words,
        )
    }
}

/// The exact, ordered schema keys `format` emits — shared with tests so the key-completeness check
/// has one source of truth.
#[cfg(test)]
pub(crate) const SCHEMA_KEYS: &[&str] = &[
    "pv_apply_entries",
    "heap_apply_activations",
    "closure_exact_dispatches",
    "closure_exact_zero",
    "closure_exact_positive",
    "entry_exact_fast_hits",
    "under_apply",
    "over_apply",
    "pap_dispatch",
    "byneed_dispatch",
    "pv_tailcall_writes",
    "pending_tail_apply_takes",
    "pending_tail_settle_takes",
    "pv_settle_fast",
    "pv_settle_slow",
    "gc_collections",
    "gc_total_ns",
    "gc_copied_words",
    "gc_max_live_words",
];

/// Parse the `PURVASM_STATS` environment variable's value (ADR-0102 §3): absent means disabled,
/// exactly `"1"` means enabled, and any other present value is an error carrying a diagnostic
/// message — the caller ([`Heap::new_native`](crate::gc::Heap::new_native)) turns that into a panic
/// (which the `pv_*` ABI's `guard` escalates to `process::abort()`). Pure: takes the already-decoded
/// value so it is unit-testable without touching the real environment.
pub(crate) fn parse_stats_env(raw: Option<&str>) -> Result<bool, String> {
    match raw {
        None => Ok(false),
        Some("1") => Ok(true),
        Some(other) => Err(format!(
            "PURVASM_STATS: expected absent or \"1\", got {other:?}"
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_stats_env_absent_is_disabled() {
        assert_eq!(parse_stats_env(None), Ok(false));
    }

    #[test]
    fn parse_stats_env_one_is_enabled() {
        assert_eq!(parse_stats_env(Some("1")), Ok(true));
    }

    #[test]
    fn parse_stats_env_rejects_other_values() {
        for bad in ["0", "true", "", "2", " 1", "1 "] {
            let err =
                parse_stats_env(Some(bad)).expect_err("non-\"1\" present value must be rejected");
            assert!(
                err.contains("PURVASM_STATS"),
                "diagnostic should name the variable: {err}"
            );
        }
    }

    #[test]
    fn format_default_has_every_key_exactly_once_at_zero() {
        let line = Stats::default().format();
        assert!(line.starts_with("purvasm-stats:v1 "));
        assert!(
            !line.ends_with('\n'),
            "format() must not add its own newline"
        );
        let mut seen = std::collections::HashSet::new();
        for pair in line.trim_start_matches("purvasm-stats:v1 ").split(' ') {
            let (key, value) = pair.split_once('=').expect("every field is key=value");
            assert!(seen.insert(key), "duplicate key in schema line: {key}");
            assert_eq!(
                value, "0",
                "default Stats must format all-zero, got {key}={value}"
            );
        }
        let keys: std::collections::HashSet<&str> = seen.into_iter().collect();
        let expected: std::collections::HashSet<&str> = SCHEMA_KEYS.iter().copied().collect();
        assert_eq!(keys, expected, "schema key set mismatch");
    }

    #[test]
    fn format_reports_hand_set_nonzero_values() {
        let s = Stats {
            pv_apply_entries: 7,
            gc_max_live_words: 42,
            ..Stats::default()
        };
        let line = s.format();
        assert!(line.contains("pv_apply_entries=7"));
        assert!(line.contains("gc_max_live_words=42"));
        assert!(line.contains("heap_apply_activations=0"));
    }

    #[test]
    fn record_closure_exact_splits_zero_and_positive() {
        let mut s = Stats::default();
        s.record_closure_exact(0);
        s.record_closure_exact(2);
        s.record_closure_exact(0);
        assert_eq!(s.closure_exact_dispatches, 3);
        assert_eq!(s.closure_exact_zero, 2);
        assert_eq!(s.closure_exact_positive, 1);
    }

    #[test]
    fn record_gc_sums_and_maxes_together() {
        let mut s = Stats::default();
        s.record_gc(100, 10);
        s.record_gc(50, 30);
        s.record_gc(10, 5);
        assert_eq!(s.gc_collections, 3);
        assert_eq!(s.gc_total_ns, 160);
        assert_eq!(s.gc_copied_words, 45);
        assert_eq!(s.gc_max_live_words, 30);
    }
}
