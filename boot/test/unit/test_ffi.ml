(* Unit tests for the FFI provider ladder (ADR-0017): a foreign leaf resolves
   through the intrinsic rung to an eta-expanded primop term that evaluates
   correctly. (Running real Prelude programs end-to-end is a separate E2E once
   newtype dictionaries are handled.) *)

module C = Cesk.Ast
module V = Cesk.Value

(* Evaluate a resolved leaf applied to argument terms left to right. *)
let apply (key : string) (args : C.term list) : V.t =
  match Ffi.resolver key with
  | None -> Alcotest.failf "no resolution for %s" key
  | Some term -> Cesk.Machine.eval (List.fold_left (fun f a -> C.App (f, a)) term args)

let int n = C.Lit (C.LInt n)
let bool b = C.Lit (C.LBool b)
let str s = C.Lit (C.LString s)

let as_int = function
  | V.VInt n -> n
  | _ -> Alcotest.fail "expected VInt"

let as_bool = function
  | V.VBool b -> b
  | _ -> Alcotest.fail "expected VBool"

let as_string = function
  | V.VString s -> s
  | _ -> Alcotest.fail "expected VString"

(* arithmetic *)
let test_int_add () =
  Alcotest.(check int)
    "intAdd 2 3"
    5
    (as_int (apply "Data.Semiring.intAdd" [ int 2; int 3 ]))

let test_int_mul () =
  Alcotest.(check int)
    "intMul 4 5"
    20
    (as_int (apply "Data.Semiring.intMul" [ int 4; int 5 ]))

let test_int_sub () =
  Alcotest.(check int)
    "intSub 10 3"
    7
    (as_int (apply "Data.Ring.intSub" [ int 10; int 3 ]))

let test_int_div () =
  Alcotest.(check int)
    "intDiv 10 3"
    3
    (as_int (apply "Data.EuclideanRing.intDiv" [ int 10; int 3 ]))

(* div/mod by zero is 0, matching the JS FFI (ADR-0017). *)
let test_int_div_zero () =
  Alcotest.(check int)
    "intDiv 5 0"
    0
    (as_int (apply "Data.EuclideanRing.intDiv" [ int 5; int 0 ]))

let test_int_mod () =
  Alcotest.(check int)
    "intMod 10 3"
    1
    (as_int (apply "Data.EuclideanRing.intMod" [ int 10; int 3 ]))

(* intDegree is abs over existing primops. *)
let test_int_degree () =
  Alcotest.(check int)
    "intDegree (-5)"
    5
    (as_int (apply "Data.EuclideanRing.intDegree" [ int (-5) ]))

(* equality *)
let test_eq_int () =
  Alcotest.(check bool)
    "eqInt 3 3"
    true
    (as_bool (apply "Data.Eq.eqIntImpl" [ int 3; int 3 ]))

let test_eq_string () =
  Alcotest.(check bool)
    "eqString a a"
    true
    (as_bool (apply "Data.Eq.eqStringImpl" [ str "a"; str "a" ]))

(* Char reuses the Int primop (Char is Int, ADR-0006). *)
let test_eq_char () =
  Alcotest.(check bool)
    "eqChar 120 120"
    true
    (as_bool (apply "Data.Eq.eqCharImpl" [ int 120; int 120 ]))

let test_eq_bool () =
  Alcotest.(check bool)
    "eqBool true true"
    true
    (as_bool (apply "Data.Eq.eqBooleanImpl" [ bool true; bool true ]))

(* boolean ops *)
let test_bool_conj () =
  Alcotest.(check bool)
    "conj true false"
    false
    (as_bool (apply "Data.HeytingAlgebra.boolConj" [ bool true; bool false ]))

let test_bool_disj () =
  Alcotest.(check bool)
    "disj false true"
    true
    (as_bool (apply "Data.HeytingAlgebra.boolDisj" [ bool false; bool true ]))

let test_bool_not () =
  Alcotest.(check bool)
    "not true"
    false
    (as_bool (apply "Data.HeytingAlgebra.boolNot" [ bool true ]))

(* string semigroup *)
let test_concat_string () =
  Alcotest.(check string)
    "concatString a b"
    "ab"
    (as_string (apply "Data.Semigroup.concatString" [ str "a"; str "b" ]))

(* unit is a foreign *constant*; Unit is opaque, so we represent it as the
   immediate 0 (cheapest, future-ABI-aligned), not JS's empty record. *)
let test_unit_constant () =
  Alcotest.(check int) "unit = 0" 0 (as_int (apply "Data.Unit.unit" []))

(* A partially applied leaf is an ordinary closure, not stuck. *)
let test_partial_application () =
  match Ffi.resolver "Data.Semiring.intAdd" with
  | Some term ->
    (match Cesk.Machine.eval (C.App (term, int 2)) with
     | V.VClosure _ -> ()
     | _ -> Alcotest.fail "a partially applied leaf should be a closure")
  | None -> Alcotest.fail "intAdd should resolve"

(* An unknown name declines (None) — the ladder falls through, leaving it unbound
   (stuck only if forced, ADR-0016). *)
let test_unknown_declined () =
  Alcotest.(check bool)
    "unknown declined"
    true
    (Option.is_none (Ffi.resolver "Some.Unknown.thing"))

let () =
  Alcotest.run
    "ffi"
    [ ( "arithmetic"
      , [ Alcotest.test_case "int_add" `Quick test_int_add
        ; Alcotest.test_case "int_mul" `Quick test_int_mul
        ; Alcotest.test_case "int_sub" `Quick test_int_sub
        ; Alcotest.test_case "int_div" `Quick test_int_div
        ; Alcotest.test_case "int_div_zero" `Quick test_int_div_zero
        ; Alcotest.test_case "int_mod" `Quick test_int_mod
        ; Alcotest.test_case "int_degree" `Quick test_int_degree
        ] )
    ; ( "equality"
      , [ Alcotest.test_case "eq_int" `Quick test_eq_int
        ; Alcotest.test_case "eq_string" `Quick test_eq_string
        ; Alcotest.test_case "eq_char" `Quick test_eq_char
        ; Alcotest.test_case "eq_bool" `Quick test_eq_bool
        ] )
    ; ( "boolean"
      , [ Alcotest.test_case "conj" `Quick test_bool_conj
        ; Alcotest.test_case "disj" `Quick test_bool_disj
        ; Alcotest.test_case "not" `Quick test_bool_not
        ] )
    ; "semigroup", [ Alcotest.test_case "concat_string" `Quick test_concat_string ]
    ; ( "ladder"
      , [ Alcotest.test_case "unit_constant" `Quick test_unit_constant
        ; Alcotest.test_case "partial_application" `Quick test_partial_application
        ; Alcotest.test_case "unknown_declined" `Quick test_unknown_declined
        ] )
    ]
