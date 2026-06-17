(* An opaque store address: a handle the Store hands out and resolves. Abstract
   on purpose — addresses can only be produced by the store's allocator (through
   [zero]/[next]), never by arithmetic, and never confused with a plain int. The
   indirection name -> addr -> value (Env then Store) is what lets a recursive
   binding name an address before its value exists (see ADR-0004). *)

type t

include Base.Comparator.S with type t := t

(* The first address; the store's allocator starts here. *)
val zero : t

(* The next address in allocation order. *)
val next : t -> t
val equal : t -> t -> bool
val to_string : t -> string
