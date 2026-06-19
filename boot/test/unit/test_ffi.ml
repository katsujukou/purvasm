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
  | Some term ->
    Cesk.Machine.eval ~host:Ffi.host (List.fold_left (fun f a -> C.App (f, a)) term args)

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

(* --- structural / higher-order foreigns as guest terms (ADR-0020) --------- *)

let arr xs = C.Array (List.map int xs)

(* arrayMap maps a guest closure over the array via the first-order builders; the
   callback is an ordinary App (no native re-entrancy). *)
let test_array_map () =
  let inc = C.Lam ("x", C.Prim (C.AddInt, [ C.Var "x"; int 1 ])) in
  match apply "Data.Functor.arrayMap" [ inc; arr [ 1; 2; 3 ] ] with
  | V.VArray a ->
    Alcotest.(check (list int)) "map (+1)" [ 2; 3; 4 ] (List.map as_int (Array.to_list a))
  | _ -> Alcotest.fail "arrayMap should return a VArray"

let test_array_map_empty () =
  match apply "Data.Functor.arrayMap" [ C.Lam ("x", C.Var "x"); arr [] ] with
  | V.VArray a -> Alcotest.(check int) "map over []" 0 (Array.length a)
  | _ -> Alcotest.fail "expected VArray"

let eqf = C.Lam ("a", C.Lam ("b", C.Prim (C.EqInt, [ C.Var "a"; C.Var "b" ])))

let test_eq_array_true () =
  Alcotest.(check bool)
    "eqArray [1,2] [1,2]"
    true
    (as_bool (apply "Data.Eq.eqArrayImpl" [ eqf; arr [ 1; 2 ]; arr [ 1; 2 ] ]))

let test_eq_array_false () =
  Alcotest.(check bool)
    "eqArray [1,2] [1,3]"
    false
    (as_bool (apply "Data.Eq.eqArrayImpl" [ eqf; arr [ 1; 2 ]; arr [ 1; 3 ] ]))

let test_eq_array_difflen () =
  Alcotest.(check bool)
    "eqArray [1] [1,2]"
    false
    (as_bool (apply "Data.Eq.eqArrayImpl" [ eqf; arr [ 1 ]; arr [ 1; 2 ] ]))

(* --- scalar Ord comparisons (structural, ADR-0020) ------------------------ *)

(* `ord<T>Impl LT EQ GT x y` returns the matching `Ordering` value; we read back
   its constructor tag. *)
let ordering key x y =
  match apply key [ C.Ctor ("LT", 0); C.Ctor ("EQ", 0); C.Ctor ("GT", 0); x; y ] with
  | V.VData { tag; _ } -> tag
  | _ -> Alcotest.fail "expected an Ordering VData"

let test_ord_int () =
  Alcotest.(check string) "1 vs 2" "LT" (ordering "Data.Ord.ordIntImpl" (int 1) (int 2));
  Alcotest.(check string) "2 vs 2" "EQ" (ordering "Data.Ord.ordIntImpl" (int 2) (int 2));
  Alcotest.(check string) "3 vs 2" "GT" (ordering "Data.Ord.ordIntImpl" (int 3) (int 2))

let test_ord_string () =
  Alcotest.(check string)
    "a vs b"
    "LT"
    (ordering "Data.Ord.ordStringImpl" (str "a") (str "b"));
  Alcotest.(check string)
    "b vs b"
    "EQ"
    (ordering "Data.Ord.ordStringImpl" (str "b") (str "b"))

(* Char reuses the int comparison (Char is Int). *)
let test_ord_char () =
  Alcotest.(check string)
    "'a' vs 'b'"
    "LT"
    (ordering "Data.Ord.ordCharImpl" (int 97) (int 98))

(* Boolean order is false < true. *)
let test_ord_boolean () =
  Alcotest.(check string)
    "false vs true"
    "LT"
    (ordering "Data.Ord.ordBooleanImpl" (bool false) (bool true));
  Alcotest.(check string)
    "true vs true"
    "EQ"
    (ordering "Data.Ord.ordBooleanImpl" (bool true) (bool true));
  Alcotest.(check string)
    "true vs false"
    "GT"
    (ordering "Data.Ord.ordBooleanImpl" (bool true) (bool false))

(* --- native rung: opaque host-provided foreign functions (ADR-0022) -------- *)

let show_int n = as_string (apply "Data.Show.showIntImpl" [ int n ])
let show_num f = as_string (apply "Data.Show.showNumberImpl" [ C.Lit (C.LNumber f) ])
let show_chr cp = as_string (apply "Data.Show.showCharImpl" [ int cp ])
let show_str s = as_string (apply "Data.Show.showStringImpl" [ str s ])

let test_show_int () =
  Alcotest.(check string) "show 42" "42" (show_int 42);
  Alcotest.(check string) "show 0" "0" (show_int 0);
  Alcotest.(check string) "show (-5)" "-5" (show_int (-5))

(* Integral numbers get the `.0` suffix; fractional ones print their shortest
   round-tripping form (ADR-0022). *)
let test_show_number () =
  Alcotest.(check string) "show 3.0" "3.0" (show_num 3.0);
  Alcotest.(check string) "show 10.0" "10.0" (show_num 10.0);
  Alcotest.(check string) "show 3.14" "3.14" (show_num 3.14);
  Alcotest.(check string) "show (-0.5)" "-0.5" (show_num (-0.5));
  Alcotest.(check string) "show 0.1" "0.1" (show_num 0.1)

(* Char is its code point; control characters and the quote/backslash escape. *)
let test_show_char () =
  Alcotest.(check string) "show 'a'" "'a'" (show_chr 97);
  Alcotest.(check string) "show '\\n'" "'\\n'" (show_chr 10);
  Alcotest.(check string) "show '\\t'" "'\\t'" (show_chr 9);
  Alcotest.(check string) "show '\\''" "'\\''" (show_chr 39);
  Alcotest.(check string) "show '\\\\'" "'\\\\'" (show_chr 92);
  Alcotest.(check string) "show DEL" "'\\127'" (show_chr 127)

(* Strings are double-quoted; quote/backslash/control characters escape, and a
   numeric escape before a digit gets a `\&` gap. *)
let test_show_string () =
  Alcotest.(check string) "show \"hi\"" "\"hi\"" (show_str "hi");
  Alcotest.(check string) "show newline" "\"a\\nb\"" (show_str "a\nb");
  Alcotest.(check string) "show quote" "\"x\\\"y\"" (show_str "x\"y");
  Alcotest.(check string) "show backslash" "\"a\\\\b\"" (show_str "a\\b");
  Alcotest.(check string) "show gap" "\"\\1\\&2\"" (show_str "\0012")

(* The native name is bound exactly when the host registry implements it. *)
let test_native_declines_unknown () =
  Alcotest.(check bool)
    "unimplemented native declined"
    true
    (Option.is_none (Ffi.host "Data.Show.showArrayImpl"))

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
    ; ( "ord"
      , [ Alcotest.test_case "ord_int" `Quick test_ord_int
        ; Alcotest.test_case "ord_string" `Quick test_ord_string
        ; Alcotest.test_case "ord_char" `Quick test_ord_char
        ; Alcotest.test_case "ord_boolean" `Quick test_ord_boolean
        ] )
    ; ( "native"
      , [ Alcotest.test_case "show_int" `Quick test_show_int
        ; Alcotest.test_case "show_number" `Quick test_show_number
        ; Alcotest.test_case "show_char" `Quick test_show_char
        ; Alcotest.test_case "show_string" `Quick test_show_string
        ; Alcotest.test_case "native_declines_unknown" `Quick test_native_declines_unknown
        ] )
    ; ( "structural"
      , [ Alcotest.test_case "array_map" `Quick test_array_map
        ; Alcotest.test_case "array_map_empty" `Quick test_array_map_empty
        ; Alcotest.test_case "eq_array_true" `Quick test_eq_array_true
        ; Alcotest.test_case "eq_array_false" `Quick test_eq_array_false
        ; Alcotest.test_case "eq_array_difflen" `Quick test_eq_array_difflen
        ] )
    ]
