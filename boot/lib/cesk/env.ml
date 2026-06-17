open Base

(* The E of CESK: lexical bindings mapping names to store addresses. The extra
   level of indirection (name -> address -> value, rather than name -> value)
   is what distinguishes CESK from CEK. It looks redundant for a pure language,
   but the store is where the heap and its garbage collector will live, so we
   thread it explicitly from the very first transition. *)

type t = (string * Addr.t) list

let empty : t = []
let extend (env : t) (name : string) (addr : Addr.t) : t = (name, addr) :: env

let lookup (env : t) (name : string) : Addr.t =
  match List.Assoc.find env ~equal:String.equal name with
  | Some addr -> addr
  | None -> Errors.stuck ("unbound variable: " ^ name)
;;
