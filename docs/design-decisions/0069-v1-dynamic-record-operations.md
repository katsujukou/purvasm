# 0069. v1 dynamic Record operations: hash-id layout, construction, and get / insert / set / delete / modify

- Status: Accepted
- Date: 2026-07-02

## Context

PureScript records are first-class and support **dynamic** field operations — `Record.get` / `set` /
`insert` / `delete` / `modify` (and the untyped `Record.Unsafe.unsafeGet` / `unsafeSet` /
`unsafeDelete` foreigns they build on), plus row-polymorphic literals. A Record-metaprogramming example
is planned and will exercise these. The runtime has the **kind** but none of the operations:
[0064](0064-v1-single-capability-native-abi-codegen-contract.md) §2 fixes `Record = [label_ids: value
ptr][values: value ptr]` (both fields traced; the pointed-to id array is all-raw) but marks `new_record`
and the raw-id-array kind **deferred**; [0059](0059-native-abi-value-representation.md) §1 fixes the
keying: a record is a **uniform label map keyed by 64-bit FNV-1a-hashed label ids, sorted, with a
parallel value array**, and the linker **hard-errors on any hash collision** among a program's static
labels (an interface-time id→name table in the `.pmi`). This record implements construction and the
dynamic operations on that fixed representation.

The design tension this record must settle is **dynamic keys**: a static field access (`r.foo`) hashes
`foo` at compile time and is covered by the link-time collision check, but a *computed* key
(`unsafeGet k r` with `k` a runtime `String`) is hashed at run time and is outside that check.

## Decision

### 1. Layout (as fixed by ADR-0059/0064) + a raw-id-array kind

`Record = [label_ids: value slot][values: value slot]` (the ADR-0064 §2 two-pointer shape;
`value_slot_ranges(Record) = [(0,2)]`, both slots traced). For a **non-empty** record, `label_ids`
points to a new **`Kind::RawIds` (discriminant `9`)** object `[count: raw][id: raw; count]` — the
FNV-1a-64 label ids, **sorted strictly ascending** (no duplicates), all-raw
(`value_slot_ranges(RawIds) = [(0,0)]`: GC scans the object but never interprets its words); `values`
points to a `Kind::Array` whose slot `i` is the value of label `label_ids[i]` — the two arrays are
**parallel** and equal length (`count`). Records of the same *shape* may share one interned `RawIds`
array (the dictionary / static-literal case); a dynamic op that changes the shape allocates a fresh
one.

**The empty record (`{}`, or `delete` of the last field) has `count = 0`.** A zero-length `Array` /
`RawIds` value block is impossible under the `size_words >= 1` header invariant (and the empty-array
singleton is deferred, ADR-0064 §5), so **an empty record stores the immediate `unit` sentinel in
*both* slots** (an immediate, passed through by the collector) rather than pointers to empty objects.
`count(rec)` is `0` when the `label_ids` slot is an immediate, else `RawIds.count`. This keeps every
`RawIds`/`Array` in the heap non-empty (`count >= 1`) and needs no new singleton. (The `RawIds.count`
word lets a *non-empty* record's id array carry its length; the empty case never allocates one.)

`new_record(ids, values)` builds the record: `ids` empty → both slots the `unit` sentinel; else a
`RawIds` + value `Array`, asserting `ids` strictly ascending and `len(ids) == len(values)`,
self-rooting its inputs across the allocations (ADR-0066 §3). `Kind::RawIds = 9` is **ABI** (the kind
discriminant is a codegen/runtime contract, ADR-0064 §2); it is fixed here, not to drift.

### 2. Label ids: static from the compiler, dynamic hashed at runtime

`label_id(name) = fnv1a_64(utf8 bytes of name)`. A **static** access uses the id the compiler emitted
(same hash, so it agrees with the stored ids). A **dynamic** access hashes the runtime `String` key
with the same `fnv1a_64` over the `Str` bytes (ADR-0067 §5). So a single id-keyed core serves both;
`Record.Unsafe.unsafe{Get,Set,Delete}` are runtime leaves that hash their `String` key then call the
core op, and the typed `Record.*` are ordinary PureScript over those.

### 3. The operations — id-keyed, immutable (functional update → a new record)

**Every operation is a safe public API, so it release-validates the record shape first** (as `str_read`
does, ADR-0067 §5 — the public `write_raw`/`write_field` can corrupt a record, and an unchecked
`count`/pointer would drive the binary search or an array copy out of bounds → UB). A `checked_record`
release-`assert`s: `rec.kind == Record`; and either **(empty)** both slots are the immediate `unit`
sentinel, or **(non-empty)** `label_ids.kind == RawIds`, `values.kind == Array`,
`RawIds.count == RawIds.size_words - 1`, `values.size_words == count`, `count >= 1`, and the ids are
**strictly ascending** (which also rejects duplicates — checked once on entry, O(n)). Any violation is
a fault, never UB.

Each op then takes a record + an id (+ value / function) and, because records are immutable, **returns a
new record**; each self-roots its inputs across the allocations (ADR-0066 §3):

- **`record_get(rec, id) -> value`** — binary-search `label_ids` for `id`, return the parallel value.
  A missing id is a fault for `get`/`set`/`delete` (the typed API's row constraint guarantees presence;
  `unsafeGet` on an absent key is a program bug — a release fault, not UB).
- **`record_insert(rec, id, value) -> rec'`** — the label must be **absent** (per `insert`'s type);
  build new `RawIds` with `id` spliced at its sorted position and a new value `Array` with `value` at
  the same index. A larger record.
- **`record_set(rec, id, value) -> rec'`** — the label is **present**; reuse the `label_ids` array,
  build a new value `Array` with slot `i` replaced. Same shape.
- **`record_delete(rec, id) -> rec'`** — remove `id` and its parallel value; new (smaller) `RawIds` +
  `Array`.
- **`record_modify(rec, id, f) -> rec'`** — `record_set(rec, id, apply(f, [record_get(rec, id)]))`,
  rooting `rec` (and `id`) across the `apply` safepoint.

The **empty boundary** falls out of §1: `insert` into an empty record (both slots sentinel) builds the
first `RawIds`/`Array` (`count 1`); `delete` of a record's last field produces the empty record (both
slots the `unit` sentinel again, no zero-length object); `get`/`set`/`delete` on an empty record hit
the missing-id fault. Binary search over the sorted `RawIds` is O(log n); construction is O(n). Static
literals lower to one `new_record` with compiler-sorted ids (no per-field insert).

### 4. Hash collisions

Every **static** label and every **static string-literal** dynamic key in a program is known at link
time, so ADR-0059's **link-time collision check covers them** (a program with two distinct such labels
that FNV-collide is rejected at link). The residual case is a **computed** dynamic key that collides
with a *different* stored label — with 64-bit FNV-1a this is ~1 in 2⁶⁴ per pair (a program would need
~2³² distinct labels for even a 50 % birthday chance), so v1 **accepts it as a documented, negligible
risk** rather than pay to store label strings. (Storing the label `String`s for an exact compare on
hash-match is the robust alternative if a workload ever needs adversarial-collision safety — see
Alternatives.)

### 5. Scope and validation

- Implemented: `Kind::RawIds`, `new_record`, `record_get/insert/set/delete/modify`, and the
  `fnv1a_64`-over-`Str` hash used by the `Record.Unsafe` leaves. The typed `Record.*` and any
  `Foreign.Object`-style string map are PureScript/`ulib` over these (out of scope here).
- Validation: hand-built record ops + the **Record-metaprogramming example** (build a literal, then
  `insert`/`set`/`delete`/`modify`/`get` dynamically, asserting values and shape); **forced-GC**
  coverage (an op that collects mid-construction keeps `rec`/values live, ADR-0066); **Miri** on the
  binary search + array splicing raw-pointer paths.

## Consequences

- Dynamic records work on the ADR-0059/0064 hash-id representation — no new value form beyond the
  `RawIds` array kind, and static and dynamic access share one id-keyed core.
- Immutability is preserved by copy-on-update (a new record per `insert`/`set`/`delete`); the value
  arrays of unchanged-shape updates (`set`) reuse the interned `RawIds`, so only the value array copies.
- Collision safety is exactly ADR-0059's for static/literal labels; computed keys carry a negligible,
  documented residual. A future exact-compare variant is additive.
- `RawIds` is reusable for any sorted-u64 index (a future interned symbol table), not record-specific.

## Alternatives considered

- **String-keyed records (store the label `String`s, sort lexicographically, exact compare).** No hash,
  no collision, and closest to JS `o[k]` semantics — but it discards ADR-0059's committed compact hash-id
  keying and its interned-shape sharing (dictionaries would carry `String` pointers, not `u64`s),
  heavier in both memory and compare cost. Reserved as the exact-compare fallback if adversarial
  collisions ever matter.
- **Ids inline in the record** (`[n][id;n][v;n]`, one variable-size object). Saves an indirection, but
  loses the ADR-0064 §2 shape-sharing of the `RawIds` array across same-row records/dictionaries (the
  common static case), so it trades the dictionary hot path for a rare dynamic one. Rejected.
- **Runtime collision check on every dynamic key** (store strings, compare on hash-match — §4's robust
  option, done always). Pays the string storage and compare unconditionally for a ~2⁻⁶⁴ event; deferred
  behind the accept-and-document choice.
- **Mutable records (in-place `set`/`insert`/`delete`).** PureScript records are immutable values;
  in-place mutation would need the write barrier and break value semantics (`Ref` is the mutable
  vehicle, ADR-0023). Functional update (copy) is the faithful model.
