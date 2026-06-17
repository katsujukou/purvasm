open Base

(* Runtime values. A closure captures its defining environment (the E), which
   is the whole point of lexical scoping: free variables resolve where the
   lambda was written, not where it is applied. *)

type t =
  | VInt of int
  | VNumber of float
  | VBool of bool
  | VString of string
  | VArray of t array
  | VClosure of closure

and closure =
  { param : string
  ; body : Ast.term
  ; env : Env.t
  }

let rec to_string : t -> string = function
  | VInt n -> Int.to_string n
  | VNumber f -> Float.to_string f
  | VBool b -> Bool.to_string b
  | VString s -> "\"" ^ s ^ "\""
  | VArray a ->
    "[" ^ String.concat ~sep:", " (List.map (Array.to_list a) ~f:to_string) ^ "]"
  | VClosure _ -> "<closure>"
