open Base

(* Runtime values. A closure captures its defining environment (the E), which
   is the whole point of lexical scoping: free variables resolve where the
   lambda was written, not where it is applied. *)

type t =
  | VInt of int
  | VBool of bool
  | VClosure of closure

and closure =
  { param : string
  ; body : Ast.term
  ; env : Env.t
  }

let to_string : t -> string = function
  | VInt n -> Int.to_string n
  | VBool b -> Bool.to_string b
  | VClosure _ -> "<closure>"
;;
