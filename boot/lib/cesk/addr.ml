open Base

(* Backed by int, but the .mli keeps that hidden so the representation can change
   (generations, forwarding pointers) when the real collector lands. *)
module T = struct
  type t = int

  let compare = Int.compare
  let sexp_of_t = Int.sexp_of_t
end

include T
include Comparator.Make (T)

let zero : t = 0
let next (a : t) : t = a + 1
let equal : t -> t -> bool = Int.equal
let to_string : t -> string = Int.to_string
