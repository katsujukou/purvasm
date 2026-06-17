open Base

(* The S of CESK: a heap mapping addresses to slots. A slot is either a filled
   value or an uninitialized "black-hole". The black-hole exists only between a
   recursive binding reserving its address and backpatching it (see
   `Cont.Letrec_bind` and ADR-0004); keeping it inside the store, rather than in
   `Value.t`, means the rest of the machine never has to consider it a value.

   `alloc` and `reserve` only grow the store; `set` is the single in-place update
   that ties a recursive knot. This module is the seam where a per-fiber copying
   collector, which *moves* allocated values, will later replace the ever-growing
   map without touching the machine's transition rules. *)

type slot =
  | Filled of Value.t
  | Blackhole

type t =
  { map : (Addr.t, slot, Addr.comparator_witness) Map.t
  ; next : Addr.t
  }

let empty : t = { map = Map.empty (module Addr); next = Addr.zero }

let alloc (store : t) (value : Value.t) : Addr.t * t =
  let addr = store.next in
  addr, { map = Map.set store.map ~key:addr ~data:(Filled value); next = Addr.next addr }

(* Reserve a fresh address holding a black-hole, to be filled later by `set`.
   This lets `letrec` bind a name to an address before the value exists. *)
let reserve (store : t) : Addr.t * t =
  let addr = store.next in
  addr, { map = Map.set store.map ~key:addr ~data:Blackhole; next = Addr.next addr }

(* Overwrite an already-reserved address with its value — the recursive knot.
   Callers only `set` an address previously handed out by `reserve`. *)
let set (store : t) (addr : Addr.t) (value : Value.t) : t =
  { store with map = Map.set store.map ~key:addr ~data:(Filled value) }

let find (store : t) (addr : Addr.t) : Value.t =
  match Map.find store.map addr with
  | Some (Filled value) -> value
  | Some Blackhole ->
    Errors.stuck
      ("recursive binding used before initialization: address " ^ Addr.to_string addr)
  | None -> Errors.stuck ("dangling store address: " ^ Addr.to_string addr)

let size (store : t) : int = Map.length store.map
