//! purvasm v1 native runtime (ADR-0063 / ADR-0064).
//!
//! Structure follows ADR-0063: an **unsafe GC island** (raw pointers, tagged words, the moving
//! collector) exposing a low-level typed API, under a **safe runtime shell**. GC memory is never
//! treated as a Rust object: there is no `Gc<T>` that derefs to `&T`; the guest heap is reached
//! only through [`TaggedWord`] / [`HeapPtr`] and explicit field accessors (ADR-0063 §2).
//!
//! This crate currently implements the **value representation and object-header layout** of the v1
//! ABI (ADR-0064 §1–§2). The Cheney collector, allocator, `apply`, and the C-ABI boundary land in
//! following increments.
//!
//! Scope reminder (ADR-0064 §0): v1 is **single-capability, sequential, 64-bit, boot-parity**; the
//! whole cross-capability side is v2.

pub mod heap;
pub mod word;

pub use heap::{Header, HeapPtr, Kind};
pub use word::TaggedWord;

/// A guest value: a [`TaggedWord`] that the machine treats as a PureScript value. The word carries
/// no type — an immediate `0` is `false` / `Int 0` / `Unit` / nullary ctor `#0` at once, and the
/// consumer chooses the interpretation (type-erased but well-typed, ADR-0011).
pub type Value = TaggedWord;
