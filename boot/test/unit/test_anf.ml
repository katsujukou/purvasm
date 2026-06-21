(* Unit tests for the lower IR (ADR-0025): [transl] normalises the upper IR to
   ANF and [rev_transl] re-curries it back, so the round-trip
   [eval (rev_transl (transl t))] must agree with [eval t] (the oracle). A few
   structural checks pin the ANF shape (atomic arguments, uncurried spines). *)

open Cesk.Ast
module V = Cesk.Value
module A = Middle_end.Anf
module T = Middle_end.Transl
module S = Middle_end.Passes.Simplify

let eval = Cesk.Machine.eval

(* The round-trip preserves the value: re-currying the normalised form evaluates
   to the same thing as the original term. Compared via the value printer, which
   covers every first-order value shape used here. *)
let roundtrips (msg : string) (t : term) =
  Alcotest.(check string)
    msg
    (V.to_string (eval t))
    (V.to_string (eval (T.rev_transl (T.transl t))))

(* helpers *)
let num n = Lit (LInt n)
let add a b = Prim (AddInt, [ a; b ])
let mul a b = Prim (MulInt, [ a; b ])
let lam2 x y body = Lam (x, Lam (y, body))

(* --- round-trip equivalence across every node kind ------------------------ *)

let test_rt_literal () = roundtrips "literal" (num 42)
let test_rt_nested_prim () = roundtrips "(1+2)*3" (mul (add (num 1) (num 2)) (num 3))

let test_rt_curried_app () =
  roundtrips
    "(\\x y -> x+y) 3 4"
    (App (App (lam2 "x" "y" (add (Var "x") (Var "y")), num 3), num 4))

(* A non-atomic argument must be let-named, then the round-trip still agrees. *)
let test_rt_arg_is_computation () =
  roundtrips
    "(\\z -> z+1) ((\\w -> w*2) 5)"
    (App (Lam ("z", add (Var "z") (num 1)), App (Lam ("w", mul (Var "w") (num 2)), num 5)))

let test_rt_let () = roundtrips "let x=2 in x+1" (Let ("x", num 2, add (Var "x") (num 1)))

let test_rt_letrec_fact () =
  roundtrips
    "fact 5"
    (Letrec
       ( [ ( "fact"
           , Lam
               ( "n"
               , If
                   ( Prim (LtInt, [ Var "n"; num 1 ])
                   , num 1
                   , mul (Var "n") (App (Var "fact", Prim (SubInt, [ Var "n"; num 1 ])))
                   ) ) )
         ]
       , App (Var "fact", num 5) ))

let test_rt_if () = roundtrips "if" (If (Prim (LtInt, [ num 1; num 2 ]), num 10, num 20))
let test_rt_array () = roundtrips "[1,2,3]" (Array [ num 1; num 2; num 3 ])
let test_rt_record () = roundtrips "{a:1,b:2}" (Record [ "a", num 1; "b", num 2 ])
let test_rt_accessor () = roundtrips "{a:5}.a" (Accessor (Record [ "a", num 5 ], "a"))

let test_rt_update () =
  roundtrips "{a:1}{a=9}" (Accessor (Update (Record [ "a", num 1 ], [ "a", num 9 ]), "a"))

let test_rt_ctor () = roundtrips "Just 5" (App (Ctor ("Just", 1), num 5))

let test_rt_case () =
  roundtrips
    "case Just 5 of Just x -> x+1"
    (Case
       ( [ App (Ctor ("Just", 1), num 5) ]
       , [ { binders = [ BCtor ("Just", [ BVar "x" ]) ]
           ; result = Unconditional (add (Var "x") (num 1))
           }
         ; { binders = [ BCtor ("Nothing", []) ]; result = Unconditional (num 0) }
         ] ))

let test_rt_guards () =
  roundtrips
    "guarded case"
    (Case
       ( [ num 3 ]
       , [ { binders = [ BVar "x" ]
           ; result =
               Guarded
                 [ Prim (EqInt, [ Var "x"; num 3 ]), num 1; Lit (LBool true), num 0 ]
           }
         ] ))

(* --- structural: ANF shape ------------------------------------------------ *)

(* A nested application names the inner call and leaves only atoms as arguments;
   the spine `(f (g x))` becomes `let t = g x in f t`. *)
let test_shape_names_nested_app () =
  match T.transl (App (Var "f", App (Var "g", Var "x"))) with
  | A.Let
      (_, A.CApp (A.AVar "g", [ A.AVar "x" ]), A.Ret (A.CApp (A.AVar "f", [ A.AVar _ ])))
    -> ()
  | _ -> Alcotest.fail "expected the inner application to be let-named"

(* Curried source applications collapse to one uncurried spine. *)
let test_shape_uncurried_app () =
  match T.transl (App (App (Var "f", num 1), num 2)) with
  | A.Ret (A.CApp (A.AVar "f", [ A.ALit (LInt 1); A.ALit (LInt 2) ])) -> ()
  | _ -> Alcotest.fail "expected a single 2-argument application"

(* Curried source lambdas collapse to one uncurried lambda. *)
let test_shape_uncurried_lam () =
  match T.transl (lam2 "x" "y" (add (Var "x") (Var "y"))) with
  | A.Ret (A.CLam ([ "x"; "y" ], _)) -> ()
  | _ -> Alcotest.fail "expected a single 2-parameter lambda"

(* --- Simplify (ADR-0028): copy-propagation + small-callee inlining -------- *)

(* A saturated call of a flat local function is inlined to its substituted body. *)
let test_simplify_inline () =
  let t = App (Lam ("x", add (Var "x") (num 1)), num 5) in
  Alcotest.(check string)
    "(\\x -> x+1) 5 preserved"
    (V.to_string (eval t))
    (V.to_string (eval (T.rev_transl (S.run (T.transl t)))));
  match S.run (T.transl t) with
  | A.Let (_, A.CLam _, A.Ret (A.CPrim (AddInt, [ A.ALit (LInt 5); A.ALit (LInt 1) ]))) ->
    ()
  | _ -> Alcotest.fail "expected the call to be inlined to a Prim"

(* A chain of atom-aliases is copy-propagated away. *)
let test_simplify_copyprop () =
  let t = Let ("y", num 7, Let ("z", Var "y", Var "z")) in
  Alcotest.(check string)
    "let z = y = 7 in z"
    (V.to_string (eval t))
    (V.to_string (eval (T.rev_transl (S.run (T.transl t)))));
  match S.run (T.transl t) with
  | A.Ret (A.CAtom (A.ALit (LInt 7))) -> ()
  | _ -> Alcotest.fail "expected the aliases to be propagated to a literal"

let () =
  Alcotest.run
    "anf"
    [ ( "round-trip"
      , [ Alcotest.test_case "literal" `Quick test_rt_literal
        ; Alcotest.test_case "nested_prim" `Quick test_rt_nested_prim
        ; Alcotest.test_case "curried_app" `Quick test_rt_curried_app
        ; Alcotest.test_case "arg_is_computation" `Quick test_rt_arg_is_computation
        ; Alcotest.test_case "let" `Quick test_rt_let
        ; Alcotest.test_case "letrec_fact" `Quick test_rt_letrec_fact
        ; Alcotest.test_case "if" `Quick test_rt_if
        ; Alcotest.test_case "array" `Quick test_rt_array
        ; Alcotest.test_case "record" `Quick test_rt_record
        ; Alcotest.test_case "accessor" `Quick test_rt_accessor
        ; Alcotest.test_case "update" `Quick test_rt_update
        ; Alcotest.test_case "ctor" `Quick test_rt_ctor
        ; Alcotest.test_case "case" `Quick test_rt_case
        ; Alcotest.test_case "guards" `Quick test_rt_guards
        ] )
    ; ( "shape"
      , [ Alcotest.test_case "names_nested_app" `Quick test_shape_names_nested_app
        ; Alcotest.test_case "uncurried_app" `Quick test_shape_uncurried_app
        ; Alcotest.test_case "uncurried_lam" `Quick test_shape_uncurried_lam
        ] )
    ; ( "simplify"
      , [ Alcotest.test_case "inline" `Quick test_simplify_inline
        ; Alcotest.test_case "copyprop" `Quick test_simplify_copyprop
        ] )
    ]
