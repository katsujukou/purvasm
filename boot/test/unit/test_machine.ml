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
let arr es = Array es
let index a i = Prim (IndexArray, [ a; i ])
let length a = Prim (LengthArray, [ a ])
let rcd fields = Record fields
let proj l e = Accessor (e, l)
let upd e ups = Update (e, ups)
let nothing = Ctor ("Nothing", 0)
let just x = App (Ctor ("Just", 1), x)
let nil = Ctor ("Nil", 0)
let cons h t = App (App (Ctor ("Cons", 2), h), t)
let pair a b = App (App (Ctor ("Pair", 2), a), b)
let case scruts alts = Case (scruts, alts)
let alt binders result = { binders; result }

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

(* The tag and field values of a fully-applied constructor result. *)
let eval_data (term : term) : string * Cesk.Value.t list =
  match Cesk.Machine.eval term with
  | Cesk.Value.VData { tag; fields } -> tag, Array.to_list fields
  | _ -> Alcotest.fail "expected a data (VData) result"

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

(* Arrays ------------------------------------------------------------------- *)

let test_array_length () =
  Alcotest.(check int)
    "length [10,20,30]"
    3
    (eval_int (length (arr [ num 10; num 20; num 30 ])))

let test_array_empty_length () =
  Alcotest.(check int) "length []" 0 (eval_int (length (arr [])))

let test_array_index () =
  Alcotest.(check int)
    "[10,20,30]!!1"
    20
    (eval_int (index (arr [ num 10; num 20; num 30 ]) (num 1)))

(* Boundary indices: first and last element. *)
let test_array_index_first () =
  Alcotest.(check int)
    "[10,20,30]!!0"
    10
    (eval_int (index (arr [ num 10; num 20; num 30 ]) (num 0)))

let test_array_index_last () =
  Alcotest.(check int)
    "[10,20,30]!!2"
    30
    (eval_int (index (arr [ num 10; num 20; num 30 ]) (num 2)))

(* Nested arrays: elements are arbitrary values, so indexing composes. *)
let test_array_nested () =
  Alcotest.(check int)
    "[[1,2],[3,4]]!!1!!0"
    3
    (eval_int
       (index
          (index (arr [ arr [ num 1; num 2 ]; arr [ num 3; num 4 ] ]) (num 1))
          (num 0)))

(* Records ------------------------------------------------------------------ *)

let test_record_proj () =
  Alcotest.(check int)
    "{x:1,y:2}.y"
    2
    (eval_int (proj "y" (rcd [ "x", num 1; "y", num 2 ])))

let test_record_proj_first () =
  Alcotest.(check int)
    "{x:1,y:2}.x"
    1
    (eval_int (proj "x" (rcd [ "x", num 1; "y", num 2 ])))

(* Field order in the literal does not matter: projection finds the label. *)
let test_record_field_order () =
  Alcotest.(check int)
    "{y:2,x:1}.x"
    1
    (eval_int (proj "x" (rcd [ "y", num 2; "x", num 1 ])))

let test_record_update () =
  Alcotest.(check int)
    "({x:1,y:2}{x=9}).x"
    9
    (eval_int (proj "x" (upd (rcd [ "x", num 1; "y", num 2 ]) [ "x", num 9 ])))

(* Update overwrites only the named fields; others are carried through. *)
let test_record_update_preserves () =
  Alcotest.(check int)
    "({x:1,y:2}{x=9}).y"
    2
    (eval_int (proj "y" (upd (rcd [ "x", num 1; "y", num 2 ]) [ "x", num 9 ])))

(* Update is immutable: the original record is unchanged. r{x=9}.x is 9 while the
   original r.x is still 1, so their sum is 10. *)
let test_record_update_immutable () =
  Alcotest.(check int)
    "update does not mutate"
    10
    (eval_int
       (Let
          ( "r"
          , rcd [ "x", num 1 ]
          , add_int (proj "x" (upd (Var "r") [ "x", num 9 ])) (proj "x" (Var "r")) )))

(* Records nest; projection composes. *)
let test_record_nested () =
  Alcotest.(check int)
    "{p:{x:3}}.p.x"
    3
    (eval_int (proj "x" (proj "p" (rcd [ "p", rcd [ "x", num 3 ] ]))))

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

(* Indexing is unsafe (ADR-0009): out of bounds (either end) is stuck, and
   indexing/length of a non-array has no matching rule. *)
let test_array_index_oob () = assert_stuck (index (arr [ num 10 ]) (num 5))
let test_array_index_negative () = assert_stuck (index (arr [ num 10 ]) (num (-1)))
let test_array_index_non_array () = assert_stuck (index (num 5) (num 0))
let test_array_length_non_array () = assert_stuck (length (num 5))

(* Projecting a missing label, or projecting/updating a non-record, is stuck
   (well-typed CoreFn never does this). *)
let test_record_missing_field () = assert_stuck (proj "z" (rcd [ "x", num 1 ]))
let test_record_proj_non_record () = assert_stuck (proj "x" (num 5))
let test_record_update_non_record () = assert_stuck (upd (num 5) [ "x", num 1 ])

(* ADTs and pattern matching (ADR-0011) -------------------------------------- *)

(* A nullary constructor evaluates straight to data with no fields. *)
let test_ctor_nullary () =
  let tag, fields = eval_data nothing in
  Alcotest.(check string) "tag" "Nothing" tag;
  Alcotest.(check int) "no fields" 0 (List.length fields)

(* A saturated constructor application builds data carrying its field. *)
let test_ctor_saturated () =
  let tag, fields = eval_data (just (num 5)) in
  Alcotest.(check string) "tag" "Just" tag;
  Alcotest.(check int) "one field" 1 (List.length fields)

(* A binary constructor saturates only after both arguments. *)
let test_ctor_binary_saturated () =
  let tag, fields = eval_data (pair (num 1) (num 2)) in
  Alcotest.(check string) "tag" "Pair" tag;
  Alcotest.(check int) "two fields" 2 (List.length fields)

(* A constructor given fewer arguments than its arity is a partial constructor
   (VCtor) — a first-class value, not data yet. *)
let test_ctor_partial () =
  match Cesk.Machine.eval (App (Ctor ("Pair", 2), num 1)) with
  | Cesk.Value.VCtor { tag; arity; args } ->
    Alcotest.(check string) "tag" "Pair" tag;
    Alcotest.(check int) "arity" 2 arity;
    Alcotest.(check int) "one collected" 1 (List.length args)
  | _ -> Alcotest.fail "expected a partial constructor (VCtor)"

(* A partial constructor survives as a value (here, bound by let) and saturates
   when the remaining argument is applied later. *)
let test_ctor_partial_then_saturated () =
  Alcotest.(check int)
    "first-class partial ctor"
    3
    (eval_int
       (Let
          ( "p"
          , App (Ctor ("Pair", 2), num 1)
          , case
              [ App (Var "p", num 2) ]
              [ alt
                  [ BCtor ("Pair", [ BVar "a"; BVar "b" ]) ]
                  (add_int (Var "a") (Var "b"))
              ] )))

(* case selects the matching constructor arm and binds its field. *)
let test_case_just () =
  Alcotest.(check int)
    "Just 5 -> x+1"
    6
    (eval_int
       (case
          [ just (num 5) ]
          [ alt [ BCtor ("Just", [ BVar "x" ]) ] (add_int (Var "x") (num 1))
          ; alt [ BCtor ("Nothing", []) ] (num 0)
          ]))

(* The other arm of the same case, reached by the nullary constructor. *)
let test_case_nothing () =
  Alcotest.(check int)
    "Nothing -> 0"
    0
    (eval_int
       (case
          [ nothing ]
          [ alt [ BCtor ("Just", [ BVar "x" ]) ] (add_int (Var "x") (num 1))
          ; alt [ BCtor ("Nothing", []) ] (num 0)
          ]))

(* Alternatives are tried in order: the first match wins even if a later one
   would also match. *)
let test_case_first_match_wins () =
  Alcotest.(check int)
    "wildcard first"
    99
    (eval_int
       (case
          [ just (num 5) ]
          [ alt [ BNull ] (num 99); alt [ BCtor ("Just", [ BVar "x" ]) ] (Var "x") ]))

(* A wildcard matches anything and binds nothing. *)
let test_case_wildcard () =
  Alcotest.(check int) "_ -> 1" 1 (eval_int (case [ num 7 ] [ alt [ BNull ] (num 1) ]))

(* A variable binder matches anything and binds the whole value. *)
let test_case_var_binds_whole () =
  Alcotest.(check int)
    "x -> x+1"
    8
    (eval_int (case [ num 7 ] [ alt [ BVar "x" ] (add_int (Var "x") (num 1)) ]))

(* A scalar literal binder matches an equal value. *)
let test_case_int_literal () =
  Alcotest.(check int)
    "0 -> 10"
    10
    (eval_int (case [ num 0 ] [ alt [ BLit (LInt 0) ] (num 10); alt [ BNull ] (num 20) ]))

(* ...and falls through to the next alternative when it differs. *)
let test_case_int_literal_fallthrough () =
  Alcotest.(check int)
    "5 -> _"
    20
    (eval_int (case [ num 5 ] [ alt [ BLit (LInt 0) ] (num 10); alt [ BNull ] (num 20) ]))

let test_case_bool_literal () =
  Alcotest.(check int)
    "true -> 1"
    1
    (eval_int
       (case
          [ Lit (LBool true) ]
          [ alt [ BLit (LBool true) ] (num 1); alt [ BLit (LBool false) ] (num 0) ]))

let test_case_string_literal () =
  Alcotest.(check int)
    "\"b\" -> 2"
    2
    (eval_int
       (case
          [ str "b" ]
          [ alt [ BLit (LString "a") ] (num 1)
          ; alt [ BLit (LString "b") ] (num 2)
          ; alt [ BNull ] (num 0)
          ]))

(* IEEE: a NaN literal pattern never matches a NaN value, so it falls through.
   This is the matcher's link to ADR-0008 (Prim.eval EqNumber semantics). *)
let test_case_nan_literal_never_matches () =
  Alcotest.(check int)
    "nan literal falls through"
    0
    (eval_int
       (case [ numf nan ] [ alt [ BLit (LNumber nan) ] (num 1); alt [ BNull ] (num 0) ]))

(* Nested constructor patterns destructure several levels at once. *)
let test_case_nested_ctor () =
  Alcotest.(check int)
    "Just (Just 7) -> x"
    7
    (eval_int
       (case
          [ just (just (num 7)) ]
          [ alt [ BCtor ("Just", [ BCtor ("Just", [ BVar "x" ]) ]) ] (Var "x") ]))

(* An as-pattern binds the whole value AND its parts: here `m` is rebound and
   re-matched to recover the field, proving both bindings are in scope. *)
let test_case_as_pattern () =
  Alcotest.(check int)
    "m@(Just x)"
    10
    (eval_int
       (case
          [ just (num 5) ]
          [ alt
              [ BNamed ("m", BCtor ("Just", [ BVar "x" ])) ]
              (case
                 [ Var "m" ]
                 [ alt [ BCtor ("Just", [ BVar "y" ]) ] (add_int (Var "x") (Var "y")) ])
          ]))

(* Multiple scrutinees are matched position-wise by one binder each. *)
let test_case_multi_scrutinee () =
  Alcotest.(check int)
    "a, b -> a+b"
    3
    (eval_int
       (case
          [ num 1; num 2 ]
          [ alt [ BVar "a"; BVar "b" ] (add_int (Var "a") (Var "b")) ]))

(* Multi-scrutinee discrimination with fall-through: the first alternative fails
   on the first scrutinee, the second matches on the second. *)
let test_case_multi_scrutinee_fallthrough () =
  Alcotest.(check int)
    "(_,Just y) reached"
    3
    (eval_int
       (case
          [ nothing; just (num 3) ]
          [ alt [ BCtor ("Just", [ BVar "x" ]); BNull ] (Var "x")
          ; alt [ BNull; BCtor ("Just", [ BVar "y" ]) ] (Var "y")
          ; alt [ BNull; BNull ] (num 0)
          ]))

(* A recursive function over a Cons/Nil list: the canonical ADT + case use. *)
let test_case_list_sum () =
  Alcotest.(check int)
    "sum [1,2,3]"
    6
    (eval_int
       (Letrec
          ( [ ( "sum"
              , Lam
                  ( "xs"
                  , case
                      [ Var "xs" ]
                      [ alt [ BCtor ("Nil", []) ] (num 0)
                      ; alt
                          [ BCtor ("Cons", [ BVar "h"; BVar "t" ]) ]
                          (add_int (Var "h") (App (Var "sum", Var "t")))
                      ] ) )
            ]
          , App (Var "sum", cons (num 1) (cons (num 2) (cons (num 3) nil))) )))

(* The degenerate zero-scrutinee case matches an alternative with no binders. *)
let test_case_empty_scrutinee () =
  Alcotest.(check int) "empty case" 42 (eval_int (case [] [ alt [] (num 42) ]))

(* Stuck states for matching --------------------------------------------------*)

(* No alternative matches: a non-exhaustive case is stuck (well-typed CoreFn is
   exhaustive or supplies its own failure arm). *)
let test_case_non_exhaustive () =
  assert_stuck (case [ nothing ] [ alt [ BCtor ("Just", [ BVar "x" ]) ] (Var "x") ])

(* A literal pattern that matches nothing and has no catch-all is also stuck. *)
let test_case_literal_no_match () =
  assert_stuck (case [ num 5 ] [ alt [ BLit (LInt 0) ] (num 1) ])

(* Over-applying a saturated constructor applies data as a function — stuck, via
   the Arg non-function rule. *)
let test_ctor_over_application () = assert_stuck (App (just (num 1), num 2))

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
        ; Alcotest.test_case "array_length" `Quick test_array_length
        ; Alcotest.test_case "array_empty_length" `Quick test_array_empty_length
        ; Alcotest.test_case "array_index" `Quick test_array_index
        ; Alcotest.test_case "array_index_first" `Quick test_array_index_first
        ; Alcotest.test_case "array_index_last" `Quick test_array_index_last
        ; Alcotest.test_case "array_nested" `Quick test_array_nested
        ; Alcotest.test_case "record_proj" `Quick test_record_proj
        ; Alcotest.test_case "record_proj_first" `Quick test_record_proj_first
        ; Alcotest.test_case "record_field_order" `Quick test_record_field_order
        ; Alcotest.test_case "record_update" `Quick test_record_update
        ; Alcotest.test_case "record_update_preserves" `Quick test_record_update_preserves
        ; Alcotest.test_case "record_update_immutable" `Quick test_record_update_immutable
        ; Alcotest.test_case "record_nested" `Quick test_record_nested
        ] )
    ; ( "adt"
      , [ Alcotest.test_case "ctor_nullary" `Quick test_ctor_nullary
        ; Alcotest.test_case "ctor_saturated" `Quick test_ctor_saturated
        ; Alcotest.test_case "ctor_binary_saturated" `Quick test_ctor_binary_saturated
        ; Alcotest.test_case "ctor_partial" `Quick test_ctor_partial
        ; Alcotest.test_case
            "ctor_partial_then_saturated"
            `Quick
            test_ctor_partial_then_saturated
        ; Alcotest.test_case "case_just" `Quick test_case_just
        ; Alcotest.test_case "case_nothing" `Quick test_case_nothing
        ; Alcotest.test_case "case_first_match_wins" `Quick test_case_first_match_wins
        ; Alcotest.test_case "case_wildcard" `Quick test_case_wildcard
        ; Alcotest.test_case "case_var_binds_whole" `Quick test_case_var_binds_whole
        ; Alcotest.test_case "case_int_literal" `Quick test_case_int_literal
        ; Alcotest.test_case
            "case_int_literal_fallthrough"
            `Quick
            test_case_int_literal_fallthrough
        ; Alcotest.test_case "case_bool_literal" `Quick test_case_bool_literal
        ; Alcotest.test_case "case_string_literal" `Quick test_case_string_literal
        ; Alcotest.test_case
            "case_nan_literal_never_matches"
            `Quick
            test_case_nan_literal_never_matches
        ; Alcotest.test_case "case_nested_ctor" `Quick test_case_nested_ctor
        ; Alcotest.test_case "case_as_pattern" `Quick test_case_as_pattern
        ; Alcotest.test_case "case_multi_scrutinee" `Quick test_case_multi_scrutinee
        ; Alcotest.test_case
            "case_multi_scrutinee_fallthrough"
            `Quick
            test_case_multi_scrutinee_fallthrough
        ; Alcotest.test_case "case_list_sum" `Quick test_case_list_sum
        ; Alcotest.test_case "case_empty_scrutinee" `Quick test_case_empty_scrutinee
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
        ; Alcotest.test_case "array_index_oob" `Quick test_array_index_oob
        ; Alcotest.test_case "array_index_negative" `Quick test_array_index_negative
        ; Alcotest.test_case "array_index_non_array" `Quick test_array_index_non_array
        ; Alcotest.test_case "array_length_non_array" `Quick test_array_length_non_array
        ; Alcotest.test_case "record_missing_field" `Quick test_record_missing_field
        ; Alcotest.test_case "record_proj_non_record" `Quick test_record_proj_non_record
        ; Alcotest.test_case
            "record_update_non_record"
            `Quick
            test_record_update_non_record
        ; Alcotest.test_case "case_non_exhaustive" `Quick test_case_non_exhaustive
        ; Alcotest.test_case "case_literal_no_match" `Quick test_case_literal_no_match
        ; Alcotest.test_case "ctor_over_application" `Quick test_ctor_over_application
        ] )
    ]
