//! ADR-0108 §3: the apply profile — per-`(call form × miss reason)` EXECUTION counters, plus the
//! structural-apply counter, produced only by an INSTRUMENTED build.
//!
//! Two properties are deliberate:
//!
//! * **It is not `purvasm-stats:v1`.** That schema is the behavioural gate's contract
//!   (`gc_collections >= 1`, the schema check in `tools/l2-native-behavioural.sh`); folding a
//!   measurement-only, build-profile-dependent row into it would make the gate's schema depend on
//!   how the binary was built. The two blocks are read together and versioned apart.
//! * **The runtime does not name the slots.** The compiler owns the layout
//!   (`Backend.LLVM.CallClass.profileSlotNames`) and hands it over at start-up via
//!   [`register`](ApplyProfile::register), so the printed schema is labelled from the one
//!   definition of that layout. A mirrored name table here would be a second definition, free to
//!   drift the moment a `MissReason` is added.

use crate::heap::Kind;
use std::collections::BTreeMap;

/// The counters and their compiler-supplied labels. Absent (`None` on the heap) in a normal build:
/// nothing registers, nothing counts, nothing prints.
#[derive(Debug, Default, Clone)]
pub(crate) struct ApplyProfile {
    /// Slot labels, in slot order, exactly as the compiler emitted them.
    names: Vec<String>,
    /// Execution counts, parallel to `names`.
    counters: Vec<u64>,
    /// ADR-0108 §5 / ADR-0109 §5.1: guest-heap allocations by [`Kind`], indexed by discriminant.
    ///
    /// It lives on THIS line, not in `purvasm-stats:v1`, because ADR-0108 §5 pins the allocation
    /// census into the step-3 profile schema — and it is deliberately kept out of the dispatch
    /// identities (its rows carry the `alloc/` prefix, which the harness's `generic-*` sums do not
    /// match). MUTATOR allocations only: the collector evacuates through `collect_core`, not
    /// `Heap::alloc`, so a copied object is not counted again — this is allocation VOLUME, which is
    /// what `gc_copied_words` cannot tell you.
    alloc_kinds: [u64; Kind::ALL.len()],
    /// ADR-0108 §4: the drill's KEYED counters, self-registering by the emitted key string.
    ///
    /// Slots cannot express this axis. A slot index is fixed at emission time and shared by every
    /// object in the program, but the thing being attributed here — which foreign symbol, at what
    /// arity status — is per-module knowledge, and no module can know the program-wide index of a
    /// symbol another module also calls. So the key IS the identity, allocated on first sight.
    /// The fixed slots stay the backbone: `Σ keys == the callee-foreign slots` is then a check
    /// ACROSS two independent mechanisms, not a restatement of one.
    keys: BTreeMap<String, u64>,
}

impl ApplyProfile {
    /// Register a slot layout: `names` is the compiler's `\n`-separated label blob, `slots` its
    /// element count. Returns `None` when the blob does not describe exactly `slots` labels — a
    /// compiler/runtime disagreement about the layout, which the caller turns into an abort rather
    /// than counting into mislabelled slots.
    pub(crate) fn register(names_blob: &str, slots: usize) -> Option<Self> {
        let names: Vec<String> = names_blob.split('\n').map(str::to_owned).collect();
        if names.len() != slots || names.iter().any(String::is_empty) {
            return None;
        }
        Some(ApplyProfile {
            counters: vec![0; slots],
            names,
            keys: BTreeMap::new(),
            alloc_kinds: [0; Kind::ALL.len()],
        })
    }

    /// Count one guest-heap allocation of `kind` (ADR-0108 §5). Infallible and indexed by
    /// discriminant — `Kind::ALL` is pinned to be discriminant-indexed by its own unit test, so
    /// there is no out-of-range case to report.
    pub(crate) fn record_alloc(&mut self, kind: Kind) {
        let c = &mut self.alloc_kinds[kind as usize];
        *c = c.saturating_add(1);
    }

    /// Count one execution of `slot`. Returns `false` when the slot is out of range — again a
    /// layout disagreement, and again fail-closed at the call site rather than silently dropped.
    pub(crate) fn bump(&mut self, slot: usize) -> bool {
        match self.counters.get_mut(slot) {
            Some(c) => {
                *c = c.saturating_add(1);
                true
            }
            None => false,
        }
    }

    /// Count one execution against `key` (ADR-0108 §4). Unknown keys are CREATED, not refused:
    /// unlike a slot, a key carries its own label, so a key this run has not seen before is a new
    /// call site being exercised — not a layout disagreement.
    ///
    /// Looked up BORROWED first: `entry()` would allocate and copy the key string on every call,
    /// including the overwhelming majority that hit an existing counter. On the self-host workload
    /// that is ~434 M allocations for 28 distinct keys. The measurement vehicle has to survive
    /// workloads bigger than the one that motivated it, so the allocation happens once per distinct
    /// key, on the miss.
    pub(crate) fn bump_key(&mut self, key: &str) {
        if let Some(c) = self.keys.get_mut(key) {
            *c = c.saturating_add(1);
            return;
        }
        self.keys.insert(key.to_owned(), 1);
    }

    /// The `purvasm-applyprofile-keys:v1` line, or `None` when the drill recorded nothing (an
    /// uninstrumented drill, or a run that executed no drilled dispatch). A `BTreeMap` keeps the
    /// order stable across runs of the same binary, so the line diffs.
    pub(crate) fn format_keys(&self) -> Option<String> {
        if self.keys.is_empty() {
            return None;
        }
        let mut out = String::from("purvasm-applyprofile-keys:v1");
        for (key, count) in self.keys.iter() {
            out.push(' ');
            out.push_str(key);
            out.push('=');
            out.push_str(&count.to_string());
        }
        Some(out)
    }

    /// The `purvasm-applyprofile:v1` line, without a trailing newline. Slots print in slot order, so
    /// the line is diffable across runs of the same binary.
    ///
    /// The `alloc/kind/*` rows (ADR-0108 §5) come AFTER every compiler slot and carry their own
    /// prefix, so the dispatch identities — which sum `generic-apply/`, `generic-tail/` and
    /// `structural-apply` by name — are unaffected by their presence.
    pub(crate) fn format(&self) -> String {
        let mut out = String::from("purvasm-applyprofile:v1");
        for (name, count) in self.names.iter().zip(self.counters.iter()) {
            out.push(' ');
            out.push_str(name);
            out.push('=');
            out.push_str(&count.to_string());
        }
        for (kind, count) in Kind::ALL.iter().zip(self.alloc_kinds.iter()) {
            out.push_str(" alloc/kind/");
            out.push_str(kind.census_name());
            out.push('=');
            out.push_str(&count.to_string());
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The COMPILER-slot section of the profile line — everything before the runtime-owned
    /// `alloc/kind/*` census rows (ADR-0108 §5). The slot assertions below are exact equalities on
    /// purpose (a slot silently gained or lost is the failure they exist to catch), so they compare
    /// this section rather than the whole line.
    fn slots_of(line: &str) -> &str {
        match line.find(" alloc/kind/") {
            Some(i) => &line[..i],
            None => panic!("the census rows are always emitted: {line}"),
        }
    }

    #[test]
    fn register_sizes_the_counters_from_the_compiler_blob() {
        let p = ApplyProfile::register("a/x\na/y\nstructural-apply", 3).expect("registers");
        assert_eq!(p.counters.len(), 3);
        assert_eq!(
            slots_of(&p.format()),
            "purvasm-applyprofile:v1 a/x=0 a/y=0 structural-apply=0"
        );
    }

    #[test]
    fn register_rejects_a_blob_that_disagrees_with_the_slot_count() {
        // A compiler that added a reason without the count following it (or vice versa) must not be
        // able to count into mislabelled slots: the layout is rejected, not repaired.
        assert!(ApplyProfile::register("a/x\na/y", 3).is_none());
        assert!(ApplyProfile::register("a/x\na/y\na/z", 2).is_none());
        assert!(
            ApplyProfile::register("a/x\n\na/z", 3).is_none(),
            "an empty label is not a slot"
        );
    }

    #[test]
    fn bump_counts_in_slot_order_and_refuses_an_out_of_range_slot() {
        let mut p = ApplyProfile::register("a\nb", 2).expect("registers");
        assert!(p.bump(1));
        assert!(p.bump(1));
        assert!(p.bump(0));
        assert!(
            !p.bump(2),
            "out-of-range slot is refused, not wrapped or ignored"
        );
        assert_eq!(slots_of(&p.format()), "purvasm-applyprofile:v1 a=1 b=2");
    }

    #[test]
    fn keys_self_register_and_print_in_a_stable_order() {
        let mut p = ApplyProfile::register("a\nb", 2).expect("registers");
        assert_eq!(
            p.format_keys(),
            None,
            "a drill that ran nothing prints nothing"
        );
        p.bump_key("z.sym|apply|known-match");
        p.bump_key("a.sym|tail|unknown");
        p.bump_key("z.sym|apply|known-match");
        assert_eq!(
            p.format_keys().expect("has keys"),
            "purvasm-applyprofile-keys:v1 a.sym|tail|unknown=1 z.sym|apply|known-match=2",
            "sorted by key, so two runs of one binary diff cleanly"
        );
    }

    #[test]
    fn alloc_census_prints_every_kind_after_the_compiler_slots() {
        let mut p = ApplyProfile::register("a\nb", 2).expect("registers");
        p.record_alloc(Kind::Closure);
        p.record_alloc(Kind::Closure);
        p.record_alloc(Kind::Str);
        let line = p.format();
        // the compiler's slots keep slot order and come first, so the harness's positional/prefix
        // sums are unaffected by the census riding the same line (ADR-0108 §5).
        assert!(
            line.starts_with("purvasm-applyprofile:v1 a=0 b=0 alloc/kind/"),
            "unexpected layout: {line}"
        );
        assert!(line.contains(" alloc/kind/closure=2"), "{line}");
        assert!(line.contains(" alloc/kind/str=1"), "{line}");
        // every kind gets a row even at zero — a kind that stops being allocated must read 0, not
        // vanish (a missing row and a zero row are different findings).
        for k in Kind::ALL {
            assert!(
                line.contains(&format!(" alloc/kind/{}=", k.census_name())),
                "missing census row for {}: {line}",
                k.census_name()
            );
        }
    }

    #[test]
    fn the_alloc_census_does_not_disturb_the_dispatch_axes() {
        // The identities this profile carries sum `generic-*`/`structural-apply` slots and the keyed
        // drill. A census row must be invisible to both, in both directions.
        let mut p = ApplyProfile::register("generic-apply/callee-foreign\nstructural-apply", 2)
            .expect("registers");
        p.record_alloc(Kind::Closure);
        assert!(p.format().contains("generic-apply/callee-foreign=0"));
        assert!(p.format().contains("structural-apply=0"));
        assert_eq!(p.format_keys(), None);
        assert!(p.bump(0));
        p.bump_key("x|apply|known-match");
        assert!(p.format().contains(" alloc/kind/closure=1"));
    }

    #[test]
    fn the_keyed_axis_does_not_disturb_the_slot_axis() {
        // The two mechanisms are independent by construction; the harness gate compares them, which
        // is only meaningful if neither writes to the other.
        let mut p = ApplyProfile::register("a\nb", 2).expect("registers");
        p.bump_key("x|apply|unknown");
        assert_eq!(slots_of(&p.format()), "purvasm-applyprofile:v1 a=0 b=0");
        assert!(p.bump(0));
        assert_eq!(
            p.format_keys().expect("has keys"),
            "purvasm-applyprofile-keys:v1 x|apply|unknown=1"
        );
    }
}
