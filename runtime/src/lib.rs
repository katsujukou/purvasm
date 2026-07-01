//! purvasm v1 native runtime (ADR-0063 / ADR-0064).
//!
//! Structure follows ADR-0063: an **unsafe GC island** (raw pointers, tagged words, the moving
//! collector) exposing a low-level typed API, under a **safe runtime shell**. GC memory is never
//! treated as a Rust object: there is no `Gc<T>` that derefs to `&T`; the guest heap is reached
//! only through [`TaggedWord`] / [`HeapPtr`] and explicit field accessors (ADR-0063 §2).
//!
//! **Pointer-word invariant.** A *pointer* [`TaggedWord`] can be minted only from a real [`HeapPtr`]
//! ([`HeapPtr::as_word`]) — the raw address / bit constructors are crate-internal — so safe callers
//! cannot forge a pointer to an arbitrary address that `apply` / `collect` would deref as a header.
//! A [`HeapPtr`] can still be *retained across a `collect`* (a value, not a borrow), so it may go
//! stale or come to alias a newer object's interior; the safe field API and `apply` therefore
//! validate that the pointer is a live **object-header start** (not merely in-bounds) before any
//! deref — see [`Heap::header`] and the `checked`/`unchecked` field tiers in [`gc`].
//!
//! This crate currently implements the v1 **value representation** and **object-header layout**
//! (ADR-0064 §1–§2), the **local Cheney copying collector + allocator** ([`gc::Heap`], ADR-0064 §5),
//! the **eval/apply calling convention** ([`Heap::apply`], ADR-0064 §3), **precise rooting**
//! (ADR-0066): a `Heap`-owned shadow stack ([`Heap::root`]/[`Heap::get`]/[`Heap::frame`]) with
//! self-rooting constructors, so `alloc` **collects on overflow and retries** (GC-on-alloc), and
//! **`Effect` execution** ([`Heap::run_effect`], the `Ref` leaves, `Str` + a generic stdio write-line
//! leaf, ADR-0067). The C-ABI / native FFI boundary, the direct known-arity fast path, and the by-need
//! recursive-CAF (`ByNeed`) force needed by full compiled programs land in following increments.
//!
//! Scope reminder (ADR-0064 §0): v1 is **single-capability, sequential, 64-bit, boot-parity**; the
//! whole cross-capability side is v2.

pub mod apply;
pub mod effect;
pub mod gc;
pub mod heap;
pub mod word;

pub use apply::CodeFn;
pub use gc::Heap;
pub use heap::{Color, Header, HeapPtr, Kind};
pub use word::TaggedWord;

/// A guest value: a [`TaggedWord`] that the machine treats as a PureScript value. The word carries
/// no type — an immediate `0` is `false` / `Int 0` / `Unit` / nullary ctor `#0` at once, and the
/// consumer chooses the interpretation (type-erased but well-typed, ADR-0011).
pub type Value = TaggedWord;
