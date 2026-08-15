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

use std::collections::BTreeMap;

/// The counters and their compiler-supplied labels. Absent (`None` on the heap) in a normal build:
/// nothing registers, nothing counts, nothing prints.
#[derive(Debug, Default, Clone)]
pub(crate) struct ApplyProfile {
    /// Slot labels, in slot order, exactly as the compiler emitted them.
    names: Vec<String>,
    /// Execution counts, parallel to `names`.
    counters: Vec<u64>,
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
        })
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
    pub(crate) fn format(&self) -> String {
        let mut out = String::from("purvasm-applyprofile:v1");
        for (name, count) in self.names.iter().zip(self.counters.iter()) {
            out.push(' ');
            out.push_str(name);
            out.push('=');
            out.push_str(&count.to_string());
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_sizes_the_counters_from_the_compiler_blob() {
        let p = ApplyProfile::register("a/x\na/y\nstructural-apply", 3).expect("registers");
        assert_eq!(p.counters.len(), 3);
        assert_eq!(
            p.format(),
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
        assert_eq!(p.format(), "purvasm-applyprofile:v1 a=1 b=2");
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
    fn the_keyed_axis_does_not_disturb_the_slot_axis() {
        // The two mechanisms are independent by construction; the harness gate compares them, which
        // is only meaningful if neither writes to the other.
        let mut p = ApplyProfile::register("a\nb", 2).expect("registers");
        p.bump_key("x|apply|unknown");
        assert_eq!(p.format(), "purvasm-applyprofile:v1 a=0 b=0");
        assert!(p.bump(0));
        assert_eq!(
            p.format_keys().expect("has keys"),
            "purvasm-applyprofile-keys:v1 x|apply|unknown=1"
        );
    }
}
