open Base

(* The S of CESK: a heap mapping addresses to slots. A slot is a filled value, an
   uninitialized "black-hole", or a suspended (not-yet-forced) recursive binding.
   Recursive bindings are by-need (ADR-0024): a `letrec` reserves an address per
   member holding a `Suspended` right-hand side, and the first dereference forces
   it — marking the slot a black-hole, evaluating the right-hand side, then filling
   the slot with (and memoizing) the value. A binding dereferenced while it is
   being forced reads the black-hole and gets stuck — the genuine cycle. Keeping
   black-hole and suspension inside the store, rather than in `Value.t`, means the
   rest of the machine never has to consider them values.

   `alloc`/`reserve` only grow the store; `set`/`set_suspended`/`blackhole` update
   a slot in place. This module is the seam where a per-fiber copying collector,
   which *moves* allocated values, will later replace the ever-growing map without
   touching the machine's transition rules. *)

type slot =
  | Filled of Value.t
  | Blackhole
  | Suspended of Ast.term * Env.t

type t =
  { map : (Addr.t, slot, Addr.comparator_witness) Map.t
  ; next : Addr.t
  }

let empty : t = { map = Map.empty (module Addr); next = Addr.zero }

let alloc (store : t) (value : Value.t) : Addr.t * t =
  let addr = store.next in
  addr, { map = Map.set store.map ~key:addr ~data:(Filled value); next = Addr.next addr }

(* Reserve a fresh address holding a black-hole, to be filled later by `set` or
   `set_suspended`. This lets `letrec` bind every name to an address before any
   right-hand side exists. *)
let reserve (store : t) : Addr.t * t =
  let addr = store.next in
  addr, { map = Map.set store.map ~key:addr ~data:Blackhole; next = Addr.next addr }

(* Overwrite an already-reserved address with its value — the recursive knot, and
   the memoizing write that ends a by-need force. *)
let set (store : t) (addr : Addr.t) (value : Value.t) : t =
  { store with map = Map.set store.map ~key:addr ~data:(Filled value) }

(* Park a recursive binding's right-hand side (closed over the recursive
   environment) at its reserved address, to be forced on first dereference. *)
let set_suspended (store : t) (addr : Addr.t) (term : Ast.term) (env : Env.t) : t =
  { store with map = Map.set store.map ~key:addr ~data:(Suspended (term, env)) }

(* Mark a slot a black-hole for the duration of its own forcing, so a binding that
   refers to itself before it is initialized is detected as a cycle. *)
let blackhole (store : t) (addr : Addr.t) : t =
  { store with map = Map.set store.map ~key:addr ~data:Blackhole }

let find_slot (store : t) (addr : Addr.t) : slot =
  match Map.find store.map addr with
  | Some slot -> slot
  | None -> Errors.stuck ("dangling store address: " ^ Addr.to_string addr)

let size (store : t) : int = Map.length store.map
