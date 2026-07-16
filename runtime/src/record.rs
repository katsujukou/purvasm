//! v1 dynamic Record operations (ADR-0069): the id-keyed `get` / `insert` / `set` / `delete` /
//! `modify` over the hash-id record layout (`gc::Heap::new_record`), plus the `fnv1a_64` label hash
//! and the `Record.Unsafe`-shaped `String`-keyed leaves that hash a runtime key then call the core.
//!
//! Records are immutable, so every update **returns a new record**. Each op release-validates its
//! record via `checked_record` first (ADR-0069 §3), self-roots its inputs across the allocations
//! (ADR-0066 §3), and assembles the new id/value vectors with **no allocation in between** so the
//! heap-pointer values it reads stay valid until `new_record` roots them.

use crate::gc::{Heap, RecordView};
use crate::heap::HeapPtr;
use crate::Value;

/// FNV-1a-64 over bytes — the label-id hash (ADR-0069 §2 / ADR-0059 §1).
pub fn fnv1a_64(bytes: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    h
}

impl Heap {
    /// The label id of a dynamic `String` key: `fnv1a_64` over its bytes, borrowed in place —
    /// no copy-out, both string kinds accepted (ADR-0069 §2 / ADR-0103 §4). Pure reads only, so
    /// the borrow never spans an allocation.
    pub fn str_label_id(&self, key: Value) -> u64 {
        // SAFETY: `key` is a string pointer word; `str_view` validates the object / kind / range.
        let v = self.str_view(unsafe { HeapPtr::from_word(key) });
        fnv1a_64(self.view_bytes(&v))
    }

    /// Binary-search a validated record for `id`: `(index, present)`. On absent, `index` is the sorted
    /// insert position.
    fn record_search(&self, view: &RecordView, id: u64) -> (usize, bool) {
        match view.ids {
            None => (0, false), // empty record
            Some(ip) => {
                let (mut lo, mut hi) = (0usize, view.count);
                while lo < hi {
                    let mid = lo + (hi - lo) / 2;
                    let mid_id = self.rawids_id(ip, mid);
                    if mid_id == id {
                        return (mid, true);
                    } else if mid_id < id {
                        lo = mid + 1;
                    } else {
                        hi = mid;
                    }
                }
                (lo, false)
            }
        }
    }

    /// `Record.get`: the value of label `id`. Read-only; a missing id is a fault (ADR-0069 §3).
    pub fn record_get(&self, rec: Value, id: u64) -> Value {
        let view = self.checked_record(rec);
        match self.record_search(&view, id) {
            (pos, true) => self.read_field_unchecked(view.vals.unwrap(), pos as u64),
            (_, false) => panic!("record_get: label {id:#018x} is absent"),
        }
    }

    /// `Record.has`: whether label `id` is present. Read-only, never faults (ADR-0069 §3 / the
    /// `RecordHas` primop).
    pub fn record_has(&self, rec: Value, id: u64) -> bool {
        let view = self.checked_record(rec);
        self.record_search(&view, id).1
    }

    /// `Record.insert`: a new record with `id -> value` added (the label must be **absent**).
    pub fn record_insert(&mut self, rec: Value, id: u64, value: Value) -> Value {
        let frame = self.frame();
        let rr = self.root(rec);
        let vr = self.root(value);
        let view = self.checked_record(self.get(rr));
        let (pos, present) = self.record_search(&view, id);
        assert!(!present, "record_insert: label {id:#018x} already present");

        let count = view.count;
        let mut new_ids: Vec<u64> = Vec::with_capacity(count + 1);
        let mut new_vals: Vec<Value> = Vec::with_capacity(count + 1);
        for i in 0..pos {
            new_ids.push(self.rawids_id(view.ids.unwrap(), i));
            new_vals.push(self.read_field_unchecked(view.vals.unwrap(), i as u64));
        }
        new_ids.push(id);
        new_vals.push(self.get(vr));
        for i in pos..count {
            new_ids.push(self.rawids_id(view.ids.unwrap(), i));
            new_vals.push(self.read_field_unchecked(view.vals.unwrap(), i as u64));
        }
        let out = self.new_record(&new_ids, &new_vals);
        self.pop_frame(frame);
        out.as_word()
    }

    /// `Record.set`: a new record with label `id`'s value replaced (the label must be **present**).
    /// v1 rebuilds both arrays; sharing the unchanged `RawIds` (ADR-0069 §3) is a deferred perf tweak.
    pub fn record_set(&mut self, rec: Value, id: u64, value: Value) -> Value {
        let frame = self.frame();
        let rr = self.root(rec);
        let vr = self.root(value);
        let view = self.checked_record(self.get(rr));
        let (pos, present) = self.record_search(&view, id);
        assert!(present, "record_set: label {id:#018x} is absent");

        let count = view.count;
        let mut new_ids: Vec<u64> = Vec::with_capacity(count);
        let mut new_vals: Vec<Value> = Vec::with_capacity(count);
        for i in 0..count {
            new_ids.push(self.rawids_id(view.ids.unwrap(), i));
            new_vals.push(if i == pos {
                self.get(vr)
            } else {
                self.read_field_unchecked(view.vals.unwrap(), i as u64)
            });
        }
        let out = self.new_record(&new_ids, &new_vals);
        self.pop_frame(frame);
        out.as_word()
    }

    /// `Record.delete`: a new (smaller) record without label `id` (the label must be **present**).
    /// Deleting the last field yields the empty record (both slots the `unit` sentinel).
    pub fn record_delete(&mut self, rec: Value, id: u64) -> Value {
        let frame = self.frame();
        let rr = self.root(rec);
        let view = self.checked_record(self.get(rr));
        let (pos, present) = self.record_search(&view, id);
        assert!(present, "record_delete: label {id:#018x} is absent");

        let count = view.count;
        let mut new_ids: Vec<u64> = Vec::with_capacity(count - 1);
        let mut new_vals: Vec<Value> = Vec::with_capacity(count - 1);
        for i in 0..count {
            if i == pos {
                continue;
            }
            new_ids.push(self.rawids_id(view.ids.unwrap(), i));
            new_vals.push(self.read_field_unchecked(view.vals.unwrap(), i as u64));
        }
        let out = self.new_record(&new_ids, &new_vals);
        self.pop_frame(frame);
        out.as_word()
    }

    /// `Record.union` (ADR-0069 revision): a new record with all of `r2`'s fields, **overwritten by
    /// `r1`'s** on a shared id (left-biased — `Object.assign({}, r2, r1)`). A two-pointer merge of the two
    /// already-ascending id runs into strictly-ascending merged ids, then one `new_record`. Both records
    /// are rooted across that allocation; the reads happen with no intervening allocation, so their field
    /// pointers stay valid, and `new_record` roots the collected values (ADR-0066 §3).
    pub fn record_union(&mut self, r1: Value, r2: Value) -> Value {
        let frame = self.frame();
        let rr1 = self.root(r1);
        let rr2 = self.root(r2);
        let v1 = self.checked_record(self.get(rr1));
        let v2 = self.checked_record(self.get(rr2));
        let cap = v1.count + v2.count;
        let mut ids: Vec<u64> = Vec::with_capacity(cap);
        let mut vals: Vec<Value> = Vec::with_capacity(cap);
        let (mut i, mut j) = (0usize, 0usize);
        // No allocation in this loop, so `v1`/`v2`'s field pointers stay live (ADR-0069 §3).
        while i < v1.count && j < v2.count {
            let id1 = self.rawids_id(v1.ids.unwrap(), i);
            let id2 = self.rawids_id(v2.ids.unwrap(), j);
            if id1 < id2 {
                ids.push(id1);
                vals.push(self.read_field_unchecked(v1.vals.unwrap(), i as u64));
                i += 1;
            } else if id2 < id1 {
                ids.push(id2);
                vals.push(self.read_field_unchecked(v2.vals.unwrap(), j as u64));
                j += 1;
            } else {
                // shared id → `r1` wins
                ids.push(id1);
                vals.push(self.read_field_unchecked(v1.vals.unwrap(), i as u64));
                i += 1;
                j += 1;
            }
        }
        while i < v1.count {
            ids.push(self.rawids_id(v1.ids.unwrap(), i));
            vals.push(self.read_field_unchecked(v1.vals.unwrap(), i as u64));
            i += 1;
        }
        while j < v2.count {
            ids.push(self.rawids_id(v2.ids.unwrap(), j));
            vals.push(self.read_field_unchecked(v2.vals.unwrap(), j as u64));
            j += 1;
        }
        let out = self.new_record(&ids, &vals);
        self.pop_frame(frame);
        out.as_word()
    }

    /// `Record.modify`: `set(rec, id, f (get rec id))`. `apply f` is a safepoint, so `rec` is rooted
    /// across it and reloaded before the `set` (ADR-0066 §3).
    pub fn record_modify(&mut self, rec: Value, id: u64, f: Value) -> Value {
        let frame = self.frame();
        let rr = self.root(rec);
        let old = self.record_get(self.get(rr), id);
        let new = self.apply(f, &[old]); // safepoint
        let rec2 = self.get(rr); // reload the (possibly relocated) record
        let out = self.record_set(rec2, id, new);
        self.pop_frame(frame);
        out
    }

    // --- Record.Unsafe string-keyed leaves (hash the key, then the core op) -------------------------

    /// `Record.Unsafe.unsafeGet` performer: `record_get` keyed by the hash of the `String` `key`.
    pub fn record_unsafe_get(&self, key: Value, rec: Value) -> Value {
        let id = self.str_label_id(key);
        self.record_get(rec, id)
    }

    /// `Record.Unsafe.unsafeSet` performer (insert-or-replace): sets `key -> value`, adding the label
    /// if absent (the untyped `unsafeSet` does not require presence).
    pub fn record_unsafe_set(&mut self, key: Value, value: Value, rec: Value) -> Value {
        let id = self.str_label_id(key);
        let view = self.checked_record(rec);
        let (_, present) = self.record_search(&view, id);
        if present {
            self.record_set(rec, id, value)
        } else {
            self.record_insert(rec, id, value)
        }
    }

    /// `Record.Unsafe.unsafeDelete` performer: `record_delete` keyed by the hash of `key`.
    pub fn record_unsafe_delete(&mut self, key: Value, rec: Value) -> Value {
        let id = self.str_label_id(key);
        self.record_delete(rec, id)
    }

    /// `RecordHas` performer: `record_has` keyed by the hash of the `String` `key`; returns a `Boolean`.
    pub fn record_unsafe_has(&self, key: Value, rec: Value) -> Value {
        let id = self.str_label_id(key);
        Value::bool(self.record_has(rec, id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A record `{ ids[i] -> Int(vals[i]) }`.
    fn rec_of(h: &mut Heap, ids: &[u64], vals: &[i32]) -> Value {
        let vs: Vec<Value> = vals.iter().map(|v| Value::int(*v)).collect();
        h.new_record(ids, &vs).as_word()
    }

    fn get_int(h: &Heap, rec: Value, id: u64) -> i32 {
        h.record_get(rec, id).as_int()
    }

    #[test]
    fn fnv1a_is_deterministic_and_distinguishes() {
        assert_eq!(fnv1a_64(b""), 0xcbf2_9ce4_8422_2325); // FNV-1a-64 offset basis
        assert_eq!(fnv1a_64(b"foo"), fnv1a_64(b"foo"));
        assert_ne!(fnv1a_64(b"foo"), fnv1a_64(b"bar"));
    }

    #[test]
    fn record_get_finds_each_value() {
        let mut h = Heap::new(64);
        let r = rec_of(&mut h, &[10, 20, 30], &[1, 2, 3]);
        assert_eq!(get_int(&h, r, 10), 1);
        assert_eq!(get_int(&h, r, 20), 2);
        assert_eq!(get_int(&h, r, 30), 3);
    }

    #[test]
    #[should_panic(expected = "absent")]
    fn record_get_absent_faults() {
        let mut h = Heap::new(64);
        let r = rec_of(&mut h, &[10, 30], &[1, 3]);
        let _ = h.record_get(r, 20);
    }

    #[test]
    fn record_insert_middle_front_back_keeps_sorted() {
        let mut h = Heap::new(256);
        let r = rec_of(&mut h, &[10, 30], &[1, 3]);
        let mid = h.record_insert(r, 20, Value::int(2));
        assert_eq!(
            (
                get_int(&h, mid, 10),
                get_int(&h, mid, 20),
                get_int(&h, mid, 30)
            ),
            (1, 2, 3)
        );
        let front = h.record_insert(r, 5, Value::int(0));
        assert_eq!((get_int(&h, front, 5), get_int(&h, front, 10)), (0, 1));
        let back = h.record_insert(r, 40, Value::int(4));
        assert_eq!((get_int(&h, back, 30), get_int(&h, back, 40)), (3, 4));
        // original is unchanged (immutable): it has no label 20.
        assert_eq!(h.checked_record(r).count, 2);
    }

    #[test]
    #[should_panic(expected = "already present")]
    fn record_insert_duplicate_faults() {
        let mut h = Heap::new(64);
        let r = rec_of(&mut h, &[10, 20], &[1, 2]);
        let _ = h.record_insert(r, 20, Value::int(9));
    }

    #[test]
    fn record_set_replaces_only_the_target() {
        let mut h = Heap::new(256);
        let r = rec_of(&mut h, &[10, 20, 30], &[1, 2, 3]);
        let r2 = h.record_set(r, 20, Value::int(99));
        assert_eq!(
            (
                get_int(&h, r2, 10),
                get_int(&h, r2, 20),
                get_int(&h, r2, 30)
            ),
            (1, 99, 3)
        );
        assert_eq!(get_int(&h, r, 20), 2); // original unchanged
    }

    #[test]
    #[should_panic(expected = "absent")]
    fn record_set_absent_faults() {
        let mut h = Heap::new(64);
        let r = rec_of(&mut h, &[10], &[1]);
        let _ = h.record_set(r, 20, Value::int(2));
    }

    #[test]
    fn record_delete_removes_and_can_empty() {
        let mut h = Heap::new(256);
        let r = rec_of(&mut h, &[10, 20], &[1, 2]);
        let r1 = h.record_delete(r, 10);
        assert_eq!(get_int(&h, r1, 20), 2);
        assert_eq!(h.checked_record(r1).count, 1);
        // delete the last field → the empty record (count 0, sentinel slots).
        let empty = h.record_delete(r1, 20);
        assert_eq!(h.checked_record(empty).count, 0);
    }

    #[test]
    fn empty_record_get_faults_and_insert_grows() {
        let mut h = Heap::new(64);
        let e = h.new_record(&[], &[]).as_word();
        assert_eq!(h.checked_record(e).count, 0);
        let one = h.record_insert(e, 7, Value::int(42));
        assert_eq!(h.checked_record(one).count, 1);
        assert_eq!(get_int(&h, one, 7), 42);
    }

    fn inc10(_h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        Value::int(args[0].as_int() + 10)
    }

    #[test]
    fn record_modify_applies_f() {
        let mut h = Heap::new(256);
        let r = rec_of(&mut h, &[10, 20], &[1, 2]);
        let f = h.new_closure(inc10, 1, Value::unit()).as_word();
        let r2 = h.record_modify(r, 20, f);
        assert_eq!((get_int(&h, r2, 10), get_int(&h, r2, 20)), (1, 12));
    }

    #[test]
    fn record_unsafe_string_keyed_round_trip() {
        let mut h = Heap::new(512);
        let e = h.new_record(&[], &[]).as_word();
        let ka = h.new_str(b"alpha").as_word();
        let r1 = h.record_unsafe_set(ka, Value::int(1), e);
        let kb = h.new_str(b"beta").as_word();
        let r2 = h.record_unsafe_set(kb, Value::int(2), r1);
        // re-hash fresh key strings to look them up
        let ka2 = h.new_str(b"alpha").as_word();
        assert_eq!(h.record_unsafe_get(ka2, r2).as_int(), 1);
        let kb2 = h.new_str(b"beta").as_word();
        assert_eq!(h.record_unsafe_get(kb2, r2).as_int(), 2);
        // unsafeSet on a present key replaces
        let ka3 = h.new_str(b"alpha").as_word();
        let r3 = h.record_unsafe_set(ka3, Value::int(9), r2);
        let ka4 = h.new_str(b"alpha").as_word();
        assert_eq!(h.record_unsafe_get(ka4, r3).as_int(), 9);
        // unsafeDelete
        let kb3 = h.new_str(b"beta").as_word();
        let r4 = h.record_unsafe_delete(kb3, r3);
        assert_eq!(h.checked_record(r4).count, 1);
    }

    #[test]
    fn record_union_is_left_biased() {
        let mut h = Heap::new(256);
        let r1 = rec_of(&mut h, &[10, 20], &[1, 2]);
        let r2 = rec_of(&mut h, &[20, 30], &[99, 3]);
        let u = h.record_union(r1, r2);
        assert_eq!(h.checked_record(u).count, 3);
        assert_eq!(get_int(&h, u, 10), 1); // only in r1
        assert_eq!(get_int(&h, u, 20), 2); // shared id → r1 wins (not r2's 99)
        assert_eq!(get_int(&h, u, 30), 3); // only in r2
                                           // union with the empty record is identity (both directions)
        let e = h.new_record(&[], &[]).as_word();
        let u2 = h.record_union(r1, e);
        assert_eq!((get_int(&h, u2, 10), get_int(&h, u2, 20)), (1, 2));
        let u3 = h.record_union(e, r1);
        assert_eq!((get_int(&h, u3, 10), get_int(&h, u3, 20)), (1, 2));
    }

    /// A `+1` leaf that forces a collection before returning (garbage → GC in a small heap).
    fn inc1_forcing_gc(h: &mut Heap, _clo: Value, args: &[Value]) -> Value {
        for _ in 0..8 {
            let _ = h.new_number(0.0);
        }
        Value::int(args[0].as_int() + 1)
    }

    #[test]
    fn record_modify_roots_record_across_forced_gc() {
        // `modify`'s `apply f` forces a collection; the record is rooted across it and reloaded before
        // the `set`, so the result is still correct (ADR-0069 §3 / ADR-0066). The small heap plus the
        // garbage-allocating `f` guarantees the GC fires mid-op.
        let mut h = Heap::new(24);
        let r = rec_of(&mut h, &[10, 20], &[1, 2]);
        let f = h.new_closure(inc1_forcing_gc, 1, Value::unit()).as_word();
        let r2 = h.record_modify(r, 20, f);
        assert_eq!((get_int(&h, r2, 10), get_int(&h, r2, 20)), (1, 3));
    }

    #[test]
    #[should_panic(expected = "not a Record")]
    fn record_get_rejects_non_record() {
        let mut h = Heap::new(16);
        let n = h.new_number(1.0);
        let _ = h.record_get(n.as_word(), 10);
    }
}
