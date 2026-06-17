open Cesk.Ast

(* Helpers ------------------------------------------------------------------ *)

let num n = Lit (LInt n)
let add_int a b = Prim (AddInt, [ a; b ])
let sub_int a b = Prim (SubInt, [ a; b ])
let mul_int a b = Prim (MulInt, [ a; b ])
let lt_int a b = Prim (LtInt, [ a; b ])
let lt_string a b = Prim (LtString, [ a; b ])
let eq_int a b = Prim (EqInt, [ a; b ])
let eq_string a b = Prim (EqString, [ a; b ])
let str s = Lit (LString s)
let append a b = Prim (Append, [ a; b ])
let numf f = Lit (LNumber f)
let add_num a b = Prim (AddNumber, [ a; b ])
let sub_num a b = Prim (SubNumber, [ a; b ])
let mul_num a b = Prim (MulNumber, [ a; b ])
let div_num a b = Prim (DivNumber, [ a; b ])
let eq_num a b = Prim (EqNumber, [ a; b ])
let lt_num a b = Prim (LtNumber, [ a; b ])

let eval_int (term : term) : int =
  match Cesk.Machine.eval term with
  | Cesk.Value.VInt n -> n
  | _ -> Alcotest.fail "expected an integer result"

let eval_bool (term : term) : bool =
  match Cesk.Machine.eval term with
  | Cesk.Value.VBool b -> b
  | _ -> Alcotest.fail "expected a boolean result"

let eval_string (term : term) : string =
  match Cesk.Machine.eval term with
  | Cesk.Value.VString s -> s
  | _ -> Alcotest.fail "expected a string result"

let eval_number (term : term) : float =
  match Cesk.Machine.eval term with
  | Cesk.Value.VNumber f -> f
  | _ -> Alcotest.fail "expected a number result"

(* Evaluation must get stuck (raise Machine_error), regardless of the message. *)
let assert_stuck (term : term) : unit =
  match Cesk.Machine.eval term with
  | exception Cesk.Errors.Machine_error _ -> ()
  | _ -> Alcotest.fail "expected the machine to get stuck, but it produced a value"

(* Successful evaluation ---------------------------------------------------- *)

let test_literal () = Alcotest.(check int) "literal" 42 (eval_int (num 42))

let test_arith () =
  Alcotest.(check int) "(2+3)*2" 10 (eval_int (mul_int (add_int (num 2) (num 3)) (num 2)))

let test_sub () = Alcotest.(check int) "10-3" 7 (eval_int (sub_int (num 10) (num 3)))

let test_let () =
  Alcotest.(check int)
    "let x=2 in x+1"
    3
    (eval_int (Let ("x", num 2, add_int (Var "x") (num 1))))

let test_lambda () =
  Alcotest.(check int)
    "(\\x->x+1) 4"
    5
    (eval_int (App (Lam ("x", add_int (Var "x") (num 1)), num 4)))

(* Free variable resolves where the lambda was defined, not where applied. *)
let test_closure_capture () =
  Alcotest.(check int)
    "capture"
    15
    (eval_int (Let ("a", num 10, App (Lam ("x", add_int (Var "x") (Var "a")), num 5))))

let test_higher_order () =
  Alcotest.(check int)
    "twice"
    4
    (eval_int
       (App
          ( Lam ("f", App (Var "f", App (Var "f", num 1)))
          , Lam ("n", mul_int (Var "n") (num 2)) )))

(* Inner binding shadows the outer one. *)
let test_shadowing () =
  Alcotest.(check int) "shadow" 2 (eval_int (Let ("x", num 1, Let ("x", num 2, Var "x"))))

let test_if_true () =
  Alcotest.(check int)
    "if true"
    10
    (eval_int (If (lt_int (num 1) (num 2), num 10, num 20)))

let test_if_false () =
  Alcotest.(check int)
    "if false"
    20
    (eval_int (If (lt_int (num 2) (num 1), num 10, num 20)))

let test_eq_true () =
  Alcotest.(check bool) "3==3" true (eval_bool (eq_int (num 3) (num 3)))

let test_eq_false () =
  Alcotest.(check bool) "3==4" false (eval_bool (eq_int (num 3) (num 4)))

(* Recursion ---------------------------------------------------------------- *)

(* fact = \n -> if n < 1 then 1 else n * fact (n-1) *)
let fact body =
  Letrec
    ( [ ( "fact"
        , Lam
            ( "n"
            , If
                ( lt_int (Var "n") (num 1)
                , num 1
                , mul_int (Var "n") (App (Var "fact", sub_int (Var "n") (num 1))) ) ) )
      ]
    , body )

let test_letrec_fact () =
  Alcotest.(check int) "fact 5" 120 (eval_int (fact (App (Var "fact", num 5))))

(* Base case taken immediately: the recursive call is never reached. *)
let test_letrec_base_case () =
  Alcotest.(check int) "fact 0" 1 (eval_int (fact (App (Var "fact", num 0))))

(* sum = \n -> if n < 1 then 0 else n + sum (n-1) *)
let test_letrec_sum () =
  Alcotest.(check int)
    "sum 5"
    15
    (eval_int
       (Letrec
          ( [ ( "sum"
              , Lam
                  ( "n"
                  , If
                      ( lt_int (Var "n") (num 1)
                      , num 0
                      , add_int (Var "n") (App (Var "sum", sub_int (Var "n") (num 1))) )
                  ) )
            ]
          , App (Var "sum", num 5) )))

(* A letrec whose binding never calls itself still works (non-recursive use). *)
let test_letrec_nonrecursive () =
  Alcotest.(check int)
    "letrec f = \\x -> x+1 in f 10"
    11
    (eval_int
       (Letrec ([ "f", Lam ("x", add_int (Var "x") (num 1)) ], App (Var "f", num 10))))

(* even/odd: a mutually recursive group. Each calls the other; both must be in
   scope in both right-hand sides, which a single-binding letrec cannot do. *)
let even_odd body =
  Letrec
    ( [ ( "even"
        , Lam
            ( "n"
            , If
                ( eq_int (Var "n") (num 0)
                , Lit (LBool true)
                , App (Var "odd", sub_int (Var "n") (num 1)) ) ) )
      ; ( "odd"
        , Lam
            ( "n"
            , If
                ( eq_int (Var "n") (num 0)
                , Lit (LBool false)
                , App (Var "even", sub_int (Var "n") (num 1)) ) ) )
      ]
    , body )

let test_letrec_mutual_even () =
  Alcotest.(check bool) "even 10" true (eval_bool (even_odd (App (Var "even", num 10))))

let test_letrec_mutual_odd () =
  Alcotest.(check bool) "odd 7" true (eval_bool (even_odd (App (Var "odd", num 7))))

(* The first binding refers forward to the second; both are λ, so reserving both
   addresses before evaluation is what makes the forward reference resolve. *)
let test_letrec_forward_reference () =
  Alcotest.(check int)
    "f forwards to g"
    6
    (eval_int
       (Letrec
          ( [ "f", Lam ("x", App (Var "g", Var "x"))
            ; "g", Lam ("x", add_int (Var "x") (num 1))
            ]
          , App (Var "f", num 5) )))

(* An empty group just evaluates its body. *)
let test_letrec_empty () =
  Alcotest.(check int) "empty group" 42 (eval_int (Letrec ([], num 42)))

(* Strings ------------------------------------------------------------------ *)

let test_string_literal () =
  Alcotest.(check string) "literal" "hello" (eval_string (str "hello"))

let test_string_append () =
  Alcotest.(check string)
    "foo<>bar"
    "foobar"
    (eval_string (append (str "foo") (str "bar")))

(* Appending the empty string is identity — a small boundary check. *)
let test_string_append_empty () =
  Alcotest.(check string) "\"\"<>x" "x" (eval_string (append (str "") (str "x")))

(* Multi-byte UTF-8 is stored and concatenated as raw bytes, round-tripping. *)
let test_string_utf8 () =
  Alcotest.(check string)
    "utf8 roundtrip"
    "日本語"
    (eval_string (append (str "日本") (str "語")))

let test_string_eq_true () =
  Alcotest.(check bool) "a==a" true (eval_bool (eq_string (str "abc") (str "abc")))

let test_string_eq_false () =
  Alcotest.(check bool) "a==b" false (eval_bool (eq_string (str "abc") (str "abd")))

(* Byte-wise, which for UTF-8 is natural/code-point order. *)
let test_string_lt () =
  Alcotest.(check bool) "abc<abd" true (eval_bool (lt_string (str "abc") (str "abd")))

(* Numbers ------------------------------------------------------------------ *)

let test_number_add () =
  Alcotest.(check (float 1e-9))
    "1.5+2.0"
    3.5
    (eval_number (add_num (numf 1.5) (numf 2.0)))

let test_number_sub () =
  Alcotest.(check (float 1e-9))
    "5.0-1.5"
    3.5
    (eval_number (sub_num (numf 5.0) (numf 1.5)))

let test_number_mul () =
  Alcotest.(check (float 1e-9))
    "2.0*1.5"
    3.0
    (eval_number (mul_num (numf 2.0) (numf 1.5)))

let test_number_div () =
  Alcotest.(check (float 1e-9))
    "10.0/4.0"
    2.5
    (eval_number (div_num (numf 10.0) (numf 4.0)))

(* Division is total: 1.0/0.0 = +inf, no exception. *)
let test_number_div_inf () =
  Alcotest.(check bool)
    "1.0/0.0 = +inf"
    true
    (eval_number (div_num (numf 1.0) (numf 0.0)) = infinity)

(* 0.0/0.0 = nan, again no exception. *)
let test_number_div_nan () =
  Alcotest.(check bool)
    "0.0/0.0 is nan"
    true
    (Float.is_nan (eval_number (div_num (numf 0.0) (numf 0.0))))

let test_number_eq_true () =
  Alcotest.(check bool) "1.5==1.5" true (eval_bool (eq_num (numf 1.5) (numf 1.5)))

let test_number_eq_false () =
  Alcotest.(check bool) "1.5==2.0" false (eval_bool (eq_num (numf 1.5) (numf 2.0)))

(* IEEE: nan is not equal to itself (matches PureScript/JS). The crux of ADR-0008
   — it must NOT use a total-order comparison. *)
let test_number_eq_nan () =
  Alcotest.(check bool)
    "nan==nan is false"
    false
    (eval_bool (eq_num (numf nan) (numf nan)))

let test_number_lt () =
  Alcotest.(check bool) "1.5<2.0" true (eval_bool (lt_num (numf 1.5) (numf 2.0)))

(* IEEE: any ordering comparison involving nan is false. *)
let test_number_lt_nan () =
  Alcotest.(check bool)
    "nan<1.0 is false"
    false
    (eval_bool (lt_num (numf nan) (numf 1.0)))

(* Stuck states ------------------------------------------------------------- *)

let test_unbound () = assert_stuck (Var "nope")
let test_apply_non_function () = assert_stuck (App (num 1, num 2))
let test_prim_type_error () = assert_stuck (add_int (num 1) (Lam ("x", Var "x")))
let test_if_non_bool () = assert_stuck (If (num 0, num 1, num 2))

(* Black-hole: a binding that forces its own value before it is initialized.
   The RHS is not a value form, so it reads the reserved address (⊥) and gets
   stuck — the dynamic stand-in for OCaml's static let-rec restriction. *)
let test_letrec_self_reference () = assert_stuck (Letrec ([ "x", Var "x" ], Var "x"))

let test_letrec_strict_rhs () =
  assert_stuck (Letrec ([ "x", add_int (Var "x") (num 1) ], Var "x"))

(* The mutual version of the same: x = y and y = x, both non-value, so whichever
   is evaluated first reads the other's still-reserved (⊥) address. *)
let test_letrec_mutual_blackhole () =
  assert_stuck (Letrec ([ "x", Var "y"; "y", Var "x" ], Var "x"))

(* Primitives are monomorphic on the value shape: append wants two strings, and
   a mixed-type comparison has no matching rule. *)
let test_append_type_error () = assert_stuck (append (num 1) (num 2))
let test_eq_mixed_type () = assert_stuck (eq_int (num 1) (str "a"))

(* Number arithmetic on Ints has no matching rule (Int and Number are distinct
   primitives — no cross-type arithmetic). *)
let test_number_type_error () = assert_stuck (add_num (num 1) (num 2))

let () =
  Alcotest.run
    "cesk"
    [ ( "eval"
      , [ Alcotest.test_case "literal" `Quick test_literal
        ; Alcotest.test_case "arith" `Quick test_arith
        ; Alcotest.test_case "sub" `Quick test_sub
        ; Alcotest.test_case "let" `Quick test_let
        ; Alcotest.test_case "lambda" `Quick test_lambda
        ; Alcotest.test_case "closure_capture" `Quick test_closure_capture
        ; Alcotest.test_case "higher_order" `Quick test_higher_order
        ; Alcotest.test_case "shadowing" `Quick test_shadowing
        ; Alcotest.test_case "if_true" `Quick test_if_true
        ; Alcotest.test_case "if_false" `Quick test_if_false
        ; Alcotest.test_case "eq_true" `Quick test_eq_true
        ; Alcotest.test_case "eq_false" `Quick test_eq_false
        ; Alcotest.test_case "letrec_fact" `Quick test_letrec_fact
        ; Alcotest.test_case "letrec_base_case" `Quick test_letrec_base_case
        ; Alcotest.test_case "letrec_sum" `Quick test_letrec_sum
        ; Alcotest.test_case "letrec_nonrecursive" `Quick test_letrec_nonrecursive
        ; Alcotest.test_case "letrec_mutual_even" `Quick test_letrec_mutual_even
        ; Alcotest.test_case "letrec_mutual_odd" `Quick test_letrec_mutual_odd
        ; Alcotest.test_case
            "letrec_forward_reference"
            `Quick
            test_letrec_forward_reference
        ; Alcotest.test_case "letrec_empty" `Quick test_letrec_empty
        ; Alcotest.test_case "string_literal" `Quick test_string_literal
        ; Alcotest.test_case "string_append" `Quick test_string_append
        ; Alcotest.test_case "string_append_empty" `Quick test_string_append_empty
        ; Alcotest.test_case "string_utf8" `Quick test_string_utf8
        ; Alcotest.test_case "string_eq_true" `Quick test_string_eq_true
        ; Alcotest.test_case "string_eq_false" `Quick test_string_eq_false
        ; Alcotest.test_case "string_lt" `Quick test_string_lt
        ; Alcotest.test_case "number_add" `Quick test_number_add
        ; Alcotest.test_case "number_sub" `Quick test_number_sub
        ; Alcotest.test_case "number_mul" `Quick test_number_mul
        ; Alcotest.test_case "number_div" `Quick test_number_div
        ; Alcotest.test_case "number_div_inf" `Quick test_number_div_inf
        ; Alcotest.test_case "number_div_nan" `Quick test_number_div_nan
        ; Alcotest.test_case "number_eq_true" `Quick test_number_eq_true
        ; Alcotest.test_case "number_eq_false" `Quick test_number_eq_false
        ; Alcotest.test_case "number_eq_nan" `Quick test_number_eq_nan
        ; Alcotest.test_case "number_lt" `Quick test_number_lt
        ; Alcotest.test_case "number_lt_nan" `Quick test_number_lt_nan
        ] )
    ; ( "stuck"
      , [ Alcotest.test_case "unbound" `Quick test_unbound
        ; Alcotest.test_case "apply_non_function" `Quick test_apply_non_function
        ; Alcotest.test_case "prim_type_error" `Quick test_prim_type_error
        ; Alcotest.test_case "if_non_bool" `Quick test_if_non_bool
        ; Alcotest.test_case "letrec_self_reference" `Quick test_letrec_self_reference
        ; Alcotest.test_case "letrec_strict_rhs" `Quick test_letrec_strict_rhs
        ; Alcotest.test_case "letrec_mutual_blackhole" `Quick test_letrec_mutual_blackhole
        ; Alcotest.test_case "append_type_error" `Quick test_append_type_error
        ; Alcotest.test_case "eq_mixed_type" `Quick test_eq_mixed_type
        ; Alcotest.test_case "number_type_error" `Quick test_number_type_error
        ] )
    ]
