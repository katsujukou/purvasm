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

/// The counters and their compiler-supplied labels. Absent (`None` on the heap) in a normal build:
/// nothing registers, nothing counts, nothing prints.
#[derive(Debug, Default, Clone)]
pub(crate) struct ApplyProfile {
    /// Slot labels, in slot order, exactly as the compiler emitted them.
    names: Vec<String>,
    /// Execution counts, parallel to `names`.
    counters: Vec<u64>,
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
}
