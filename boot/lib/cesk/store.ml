open Base

(* The S of CESK: a heap mapping addresses to values. Allocation only grows the
   store here; this module is the seam where a per-fiber copying collector, which
   *moves* allocated values, will later replace the ever-growing map. Keeping it 
   isolated now means that change need not touch the machine's transition rules. *)

type t =
  { map : (int, Value.t, Int.comparator_witness) Map.t
  ; next : int
  }

let empty : t = { map = Map.empty (module Int); next = 0 }

let alloc (store : t) (value : Value.t) : int * t =
  let addr = store.next in
  addr, { map = Map.set store.map ~key:addr ~data:value; next = addr + 1 }
;;

let find (store : t) (addr : int) : Value.t =
  match Map.find store.map addr with
  | Some value -> value
  | None -> Errors.stuck ("dangling store address: " ^ Int.to_string addr)
;;

let size (store : t) : int = Map.length store.map
