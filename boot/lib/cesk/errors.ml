(* Raised when the machine reaches a stuck state: an ill-formed or ill-typed
   program that no transition rule applies to. Until a type checker sits in
   front of the core language, these stand in for the guarantees the types
   will eventually provide.

   Named `Errors` (not `Error`) so it is not shadowed by `Base.Error` under the
   `open Base` used throughout this library. *)
exception Machine_error of string

let stuck (message : string) : 'a = raise (Machine_error message)
