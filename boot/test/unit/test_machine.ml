open Cesk.Ast

(* Helpers ------------------------------------------------------------------ *)

let num n = Lit (LInt n)
let add a b = Prim (Add, [ a; b ])
let sub a b = Prim (Sub, [ a; b ])
let mul a b = Prim (Mul, [ a; b ])
let lt a b = Prim (Lt, [ a; b ])
let eq a b = Prim (Eq, [ a; b ])

let eval_int (term : term) : int =
  match Cesk.Machine.eval term with
  | Cesk.Value.VInt n -> n
  | _ -> Alcotest.fail "expected an integer result"
;;

let eval_bool (term : term) : bool =
  match Cesk.Machine.eval term with
  | Cesk.Value.VBool b -> b
  | _ -> Alcotest.fail "expected a boolean result"
;;

(* Evaluation must get stuck (raise Machine_error), regardless of the message. *)
let assert_stuck (term : term) : unit =
  match Cesk.Machine.eval term with
  | exception Cesk.Errors.Machine_error _ -> ()
  | _ -> Alcotest.fail "expected the machine to get stuck, but it produced a value"
;;

(* Successful evaluation ---------------------------------------------------- *)

let test_literal () = Alcotest.(check int) "literal" 42 (eval_int (num 42))

let test_arith () =
  Alcotest.(check int) "(2+3)*2" 10 (eval_int (mul (add (num 2) (num 3)) (num 2)))
;;

let test_sub () = Alcotest.(check int) "10-3" 7 (eval_int (sub (num 10) (num 3)))

let test_let () =
  Alcotest.(check int)
    "let x=2 in x+1"
    3
    (eval_int (Let ("x", num 2, add (Var "x") (num 1))))
;;

let test_lambda () =
  Alcotest.(check int)
    "(\\x->x+1) 4"
    5
    (eval_int (App (Lam ("x", add (Var "x") (num 1)), num 4)))
;;

(* Free variable resolves where the lambda was defined, not where applied. *)
let test_closure_capture () =
  Alcotest.(check int)
    "capture"
    15
    (eval_int (Let ("a", num 10, App (Lam ("x", add (Var "x") (Var "a")), num 5))))
;;

let test_higher_order () =
  Alcotest.(check int)
    "twice"
    4
    (eval_int
       (App
          ( Lam ("f", App (Var "f", App (Var "f", num 1)))
          , Lam ("n", mul (Var "n") (num 2)) )))
;;

(* Inner binding shadows the outer one. *)
let test_shadowing () =
  Alcotest.(check int) "shadow" 2 (eval_int (Let ("x", num 1, Let ("x", num 2, Var "x"))))
;;

let test_if_true () =
  Alcotest.(check int) "if true" 10 (eval_int (If (lt (num 1) (num 2), num 10, num 20)))
;;

let test_if_false () =
  Alcotest.(check int) "if false" 20 (eval_int (If (lt (num 2) (num 1), num 10, num 20)))
;;

let test_eq_true () = Alcotest.(check bool) "3==3" true (eval_bool (eq (num 3) (num 3)))
let test_eq_false () = Alcotest.(check bool) "3==4" false (eval_bool (eq (num 3) (num 4)))

(* Recursion ---------------------------------------------------------------- *)

(* fact = \n -> if n < 1 then 1 else n * fact (n-1) *)
let fact body =
  Letrec
    ( "fact"
    , Lam
        ( "n"
        , If
            ( lt (Var "n") (num 1)
            , num 1
            , mul (Var "n") (App (Var "fact", sub (Var "n") (num 1))) ) )
    , body )
;;

let test_letrec_fact () =
  Alcotest.(check int) "fact 5" 120 (eval_int (fact (App (Var "fact", num 5))))
;;

(* Base case taken immediately: the recursive call is never reached. *)
let test_letrec_base_case () =
  Alcotest.(check int) "fact 0" 1 (eval_int (fact (App (Var "fact", num 0))))
;;

(* sum = \n -> if n < 1 then 0 else n + sum (n-1) *)
let test_letrec_sum () =
  Alcotest.(check int)
    "sum 5"
    15
    (eval_int
       (Letrec
          ( "sum"
          , Lam
              ( "n"
              , If
                  ( lt (Var "n") (num 1)
                  , num 0
                  , add (Var "n") (App (Var "sum", sub (Var "n") (num 1))) ) )
          , App (Var "sum", num 5) )))
;;

(* A letrec whose binding never calls itself still works (non-recursive use). *)
let test_letrec_nonrecursive () =
  Alcotest.(check int)
    "letrec f = \\x -> x+1 in f 10"
    11
    (eval_int (Letrec ("f", Lam ("x", add (Var "x") (num 1)), App (Var "f", num 10))))
;;

(* Stuck states ------------------------------------------------------------- *)

let test_unbound () = assert_stuck (Var "nope")
let test_apply_non_function () = assert_stuck (App (num 1, num 2))
let test_prim_type_error () = assert_stuck (add (num 1) (Lam ("x", Var "x")))
let test_if_non_bool () = assert_stuck (If (num 0, num 1, num 2))

(* Black-hole: a binding that forces its own value before it is initialized.
   The RHS is not a value form, so it reads the reserved address (⊥) and gets
   stuck — the dynamic stand-in for OCaml's static let-rec restriction. *)
let test_letrec_self_reference () = assert_stuck (Letrec ("x", Var "x", Var "x"))

let test_letrec_strict_rhs () =
  assert_stuck (Letrec ("x", add (Var "x") (num 1), Var "x"))
;;

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
        ] )
    ; ( "stuck"
      , [ Alcotest.test_case "unbound" `Quick test_unbound
        ; Alcotest.test_case "apply_non_function" `Quick test_apply_non_function
        ; Alcotest.test_case "prim_type_error" `Quick test_prim_type_error
        ; Alcotest.test_case "if_non_bool" `Quick test_if_non_bool
        ; Alcotest.test_case "letrec_self_reference" `Quick test_letrec_self_reference
        ; Alcotest.test_case "letrec_strict_rhs" `Quick test_letrec_strict_rhs
        ] )
    ]
;;
