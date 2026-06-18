(* Unit tests for the pure pieces of `Lower` (ADR-0015): the name-resolution
   helpers. The structural lowering of whole programs is exercised end-to-end by
   the E2E suite (decode -> lower -> eval), so it is not re-tested here. *)

module C = Cesk.Ast
module N = Corefn.Names

(* The qualified key joins the module segments and the ident with dots. *)
let test_qualified_key () =
  Alcotest.(check string)
    "qualified key"
    "Data.Maybe.fromMaybe"
    (Lower.qualified_key [ "Data"; "Maybe" ] "fromMaybe")

(* A local (unqualified) reference lowers to a bare-keyed Var. *)
let test_lower_var_local () =
  match Lower.lower_var (N.Qualified (None, "x")) with
  | C.Var "x" -> ()
  | _ -> Alcotest.fail "local var should lower to Var \"x\""

(* A module-qualified reference lowers to a qualified-keyed Var, a distinct key
   shape from any local, so it cannot be captured by a same-named local. *)
let test_lower_var_qualified () =
  match Lower.lower_var (N.Qualified (Some [ "Data"; "Maybe" ], "fromMaybe")) with
  | C.Var "Data.Maybe.fromMaybe" -> ()
  | _ -> Alcotest.fail "qualified var should lower to Var \"Data.Maybe.fromMaybe\""

let () =
  Alcotest.run
    "lower"
    [ ( "names"
      , [ Alcotest.test_case "qualified_key" `Quick test_qualified_key
        ; Alcotest.test_case "lower_var_local" `Quick test_lower_var_local
        ; Alcotest.test_case "lower_var_qualified" `Quick test_lower_var_qualified
        ] )
    ]
