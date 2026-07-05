//! The native half of `Lib.purs` (ADR-0078): idiomatic Rust — including a crates.io
//! dependency (`rand`) — adapted to the `pvf_*` C ABI by `#[pv_foreign]`. The `effect`
//! marker generates the ADR-0067 thunk pair; the slice arrives as a borrow of a shim-owned
//! copy, and the `Vec` result copies back into the guest heap.

use purvasm_foreign::pv_foreign;
use rand::seq::SliceRandom;

#[pv_foreign(module = "Lib", name = "rand", effect)]
fn rand_upto(max: i32) -> i32 {
    rand::random_range(1..=max)
}

#[pv_foreign(module = "Lib", name = "shuffle", effect)]
fn shuffle(arr: &[i32]) -> Vec<i32> {
    let mut rng = rand::rng();
    let mut vec = arr.to_vec();
    vec.shuffle(&mut rng);
    vec
}
