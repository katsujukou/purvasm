(* End-to-end suite: run real `corefn.json` fixtures through the whole frontend
   pipeline — decode (ADR-0014) -> lower (ADR-0015) -> evaluate on the CESK
   machine — and assert on the resulting value. The fixtures are compiled by
   `purs` from self-contained PureScript (Prim types plus their own definitions),
   which is the slice the frontend currently supports. *)

module C = Cesk.Ast
module V = Cesk.Value

let fixture : Corefn.Module.t =
  Corefn.Decode.module_of_file "../fixtures/fixture.corefn.json"

(* Run a module at [entry]: lower the whole module around that binding and
   evaluate it. *)
let run (entry : string) : V.t = Cesk.Machine.eval (Lower.module_ fixture ~entry)

(* Run [entry] applied to an argument term (built from core constructors). *)
let run_app (entry : string) (arg : C.term) : V.t =
  Cesk.Machine.eval (C.App (Lower.module_ fixture ~entry, arg))

let as_int = function
  | V.VInt n -> n
  | _ -> Alcotest.fail "expected VInt"

let as_bool = function
  | V.VBool b -> b
  | _ -> Alcotest.fail "expected VBool"

let as_string = function
  | V.VString s -> s
  | _ -> Alcotest.fail "expected VString"

let as_number = function
  | V.VNumber f -> f
  | _ -> Alcotest.fail "expected VNumber"

(* argument builders *)
let int n = C.Lit (C.LInt n)
let just x = C.App (C.Ctor ("Just", 1), x)
let nothing = C.Ctor ("Nothing", 0)

let nat n =
  let rec go k acc = if k = 0 then acc else go (k - 1) (C.App (C.Ctor ("S", 1), acc)) in
  go n (C.Ctor ("Z", 0))

(* --- literal value bindings ---------------------------------------------- *)

let test_int () = Alcotest.(check int) "anInt" 42 (as_int (run "anInt"))
let test_string () = Alcotest.(check string) "aString" "hi" (as_string (run "aString"))

let test_number () =
  Alcotest.(check (float 1e-9)) "aNumber" 3.5 (as_number (run "aNumber"))

let test_bool () = Alcotest.(check bool) "aBool" true (as_bool (run "aBool"))

(* Char evaluates to its Int code point ('x' = 120), ADR-0006. *)
let test_char () = Alcotest.(check int) "aChar" 120 (as_int (run "aChar"))

let test_array () =
  match run "anArray" with
  | V.VArray a -> Alcotest.(check int) "anArray length" 3 (Array.length a)
  | _ -> Alcotest.fail "anArray should be a VArray"

let test_record () =
  match run "aRecord" with
  | V.VRecord _ -> ()
  | _ -> Alcotest.fail "aRecord should be a VRecord"

(* --- constructors -------------------------------------------------------- *)

let test_ctor_nullary () =
  match run "Nothing" with
  | V.VData { tag = "Nothing"; fields } ->
    Alcotest.(check int) "no fields" 0 (Array.length fields)
  | _ -> Alcotest.fail "Nothing should be VData Nothing"

let test_ctor_partial () =
  match run "Just" with
  | V.VCtor { tag = "Just"; arity = 1; _ } -> ()
  | _ -> Alcotest.fail "Just should be a partial VCtor of arity 1"

let test_mk () =
  match run_app "mk" (int 7) with
  | V.VData { tag = "Just"; fields } ->
    Alcotest.(check int) "Just 7" 7 (as_int fields.(0))
  | _ -> Alcotest.fail "mk 7 should be Just 7"

(* --- case: constructor + literal binders + fall-through ------------------- *)

let test_classify_just_zero () =
  Alcotest.(check int)
    "classify (Just 0)"
    100
    (as_int (run_app "classify" (just (int 0))))

let test_classify_just_n () =
  Alcotest.(check int) "classify (Just 5)" 5 (as_int (run_app "classify" (just (int 5))))

let test_classify_nothing () =
  Alcotest.(check int) "classify Nothing" 7 (as_int (run_app "classify" nothing))

(* --- case: array binder, record binder, as-pattern ----------------------- *)

let test_first_of () =
  Alcotest.(check int)
    "firstOf [10,20]"
    10
    (as_int (run_app "firstOf" (C.Array [ int 10; int 20 ])))

let test_via_record () =
  Alcotest.(check int)
    "viaRecord {x:5}"
    5
    (as_int (run_app "viaRecord" (C.Record [ "x", int 5 ])))

let test_dup () =
  match run_app "dup" (just (int 3)) with
  | V.VData { tag = "Just"; fields } ->
    Alcotest.(check int) "dup (Just 3)" 3 (as_int fields.(0))
  | _ -> Alcotest.fail "dup (Just 3) should be Just 3"

(* --- guards -------------------------------------------------------------- *)

let test_pick_true () =
  Alcotest.(check int) "pick true" 1 (as_int (run_app "pick" (C.Lit (C.LBool true))))

let test_pick_false () =
  Alcotest.(check int) "pick false" 0 (as_int (run_app "pick" (C.Lit (C.LBool false))))

(* --- top-level mutual recursion (Rec -> Letrec) -------------------------- *)

let test_is_even_2 () =
  Alcotest.(check bool) "isEven 2" true (as_bool (run_app "isEven" (nat 2)))

let test_is_even_1 () =
  Alcotest.(check bool) "isEven 1" false (as_bool (run_app "isEven" (nat 1)))

let test_is_odd_3 () =
  Alcotest.(check bool) "isOdd 3" true (as_bool (run_app "isOdd" (nat 3)))

(* --- deferred linking: an unresolved external reference is stuck ---------- *)

(* No module binds another module's name yet, so forcing one is stuck (ADR-0015,
   total lowering). Lowered directly to keep the test self-contained. *)
let test_external_unbound () =
  match
    Cesk.Machine.eval (Lower.lower_var (Corefn.Names.Qualified (Some [ "Other" ], "foo")))
  with
  | exception Cesk.Errors.Machine_error _ -> ()
  | _ -> Alcotest.fail "an unresolved external reference should be stuck"

let () =
  Alcotest.run
    "e2e"
    [ ( "literals"
      , [ Alcotest.test_case "int" `Quick test_int
        ; Alcotest.test_case "string" `Quick test_string
        ; Alcotest.test_case "number" `Quick test_number
        ; Alcotest.test_case "bool" `Quick test_bool
        ; Alcotest.test_case "char" `Quick test_char
        ; Alcotest.test_case "array" `Quick test_array
        ; Alcotest.test_case "record" `Quick test_record
        ] )
    ; ( "constructors"
      , [ Alcotest.test_case "ctor_nullary" `Quick test_ctor_nullary
        ; Alcotest.test_case "ctor_partial" `Quick test_ctor_partial
        ; Alcotest.test_case "mk" `Quick test_mk
        ] )
    ; ( "case"
      , [ Alcotest.test_case "classify_just_zero" `Quick test_classify_just_zero
        ; Alcotest.test_case "classify_just_n" `Quick test_classify_just_n
        ; Alcotest.test_case "classify_nothing" `Quick test_classify_nothing
        ; Alcotest.test_case "first_of" `Quick test_first_of
        ; Alcotest.test_case "via_record" `Quick test_via_record
        ; Alcotest.test_case "dup" `Quick test_dup
        ; Alcotest.test_case "pick_true" `Quick test_pick_true
        ; Alcotest.test_case "pick_false" `Quick test_pick_false
        ] )
    ; ( "recursion"
      , [ Alcotest.test_case "is_even_2" `Quick test_is_even_2
        ; Alcotest.test_case "is_even_1" `Quick test_is_even_1
        ; Alcotest.test_case "is_odd_3" `Quick test_is_odd_3
        ] )
    ; "linking", [ Alcotest.test_case "external_unbound" `Quick test_external_unbound ]
    ]
