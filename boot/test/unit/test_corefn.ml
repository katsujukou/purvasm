(* End-to-end decode test for the CoreFn ingestion (ADR-0014): decode a real
   `corefn.json` produced by `purs` and check the AST it yields. The fixture
   (`fixtures/fixture.corefn.json`) is generated from a module that exercises
   every node kind the decoder supports, so this both pins specific decodes and
   proves each arm runs on genuine compiler output. *)

open Corefn

let m : Module.t = Decode.module_of_file "../fixtures/fixture.corefn.json"

(* Find the expression bound to a top-level identifier (NonRec or within Rec). *)
let find (name : string) : Expr.expr =
  let rec go = function
    | [] -> Alcotest.failf "decl not found: %s" name
    | Expr.NonRec (_, id, e) :: rest -> if String.equal id name then e else go rest
    | Expr.Rec rbs :: rest ->
      (match
         List.find_opt (fun (rb : Expr.rec_binding) -> String.equal rb.ident name) rbs
       with
       | Some rb -> rb.expr
       | None -> go rest)
  in
  go m.decls

(* --- module-level -------------------------------------------------------- *)

let test_module_name () =
  Alcotest.(check (list string)) "module name" [ "Fixture" ] m.name

let test_has_decls () =
  Alcotest.(check bool) "some decls decoded" true (List.length m.decls > 0)

(* --- literals ------------------------------------------------------------ *)

let test_lit_int () =
  match find "anInt" with
  | Expr.Literal (_, Literal.LitInt 42) -> ()
  | _ -> Alcotest.fail "anInt should be LitInt 42"

let test_lit_number () =
  match find "aNumber" with
  | Expr.Literal (_, Literal.LitNumber f) -> Alcotest.(check (float 1e-9)) "aNumber" 3.5 f
  | _ -> Alcotest.fail "aNumber should be LitNumber"

let test_lit_string () =
  match find "aString" with
  | Expr.Literal (_, Literal.LitString "hi") -> ()
  | _ -> Alcotest.fail "aString should be LitString \"hi\""

(* 'x' is code point 120; CharLiteral decodes to a code point (ADR-0006). *)
let test_lit_char () =
  match find "aChar" with
  | Expr.Literal (_, Literal.LitChar 120) -> ()
  | _ -> Alcotest.fail "aChar should be LitChar 120"

let test_lit_bool () =
  match find "aBool" with
  | Expr.Literal (_, Literal.LitBoolean true) -> ()
  | _ -> Alcotest.fail "aBool should be LitBoolean true"

let test_lit_array () =
  match find "anArray" with
  | Expr.Literal (_, Literal.LitArray xs) ->
    Alcotest.(check int) "array length" 3 (List.length xs)
  | _ -> Alcotest.fail "anArray should be LitArray"

let test_lit_object () =
  match find "aRecord" with
  | Expr.Literal (_, Literal.LitObject kvs) ->
    Alcotest.(check (list string))
      "record labels"
      [ "x"; "y" ]
      (List.sort String.compare (List.map fst kvs))
  | _ -> Alcotest.fail "aRecord should be LitObject"

(* --- constructors -------------------------------------------------------- *)

let test_ctor_nullary () =
  match find "Nothing" with
  | Expr.Constructor (_, "Maybe", "Nothing", []) -> ()
  | _ -> Alcotest.fail "Nothing should be a nullary Constructor of Maybe"

let test_ctor_unary () =
  match find "Just" with
  | Expr.Constructor (_, "Maybe", "Just", fields) ->
    Alcotest.(check int) "Just arity" 1 (List.length fields)
  | _ -> Alcotest.fail "Just should be a unary Constructor of Maybe"

(* A constructor used as a first-class value: `mk = Just`, a qualified Var. *)
let test_var_qualified () =
  match find "mk" with
  | Expr.Var (_, Names.Qualified (Some [ "Fixture" ], "Just")) -> ()
  | _ -> Alcotest.fail "mk should be Var (Qualified (Some [Fixture]) Just)"

(* --- accessor / object update -------------------------------------------- *)

let test_accessor () =
  match find "getX" with
  | Expr.Abs (_, _, Expr.Accessor (_, "x", _)) -> ()
  | _ -> Alcotest.fail "getX body should be an Accessor of field x"

(* --- node-kind coverage via a walk over the whole module ----------------- *)

let saw_constructor_binder = ref false
let saw_literal_binder = ref false
let saw_named_binder = ref false
let saw_array_binder = ref false
let saw_record_binder = ref false
let saw_guarded_alt = ref false
let saw_object_update = ref false

let rec walk_expr (e : Expr.expr) : unit =
  let walk_lit = function
    | Literal.LitArray xs -> List.iter walk_expr xs
    | Literal.LitObject kvs -> List.iter (fun (_, x) -> walk_expr x) kvs
    | _ -> ()
  in
  match e with
  | Expr.Literal (_, lit) -> walk_lit lit
  | Expr.Constructor _ | Expr.Var _ -> ()
  | Expr.Accessor (_, _, r) -> walk_expr r
  | Expr.ObjectUpdate (_, r, _, ups) ->
    saw_object_update := true;
    walk_expr r;
    List.iter (fun (_, x) -> walk_expr x) ups
  | Expr.Abs (_, _, b) -> walk_expr b
  | Expr.App (_, f, x) ->
    walk_expr f;
    walk_expr x
  | Expr.Case (_, scruts, alts) ->
    List.iter walk_expr scruts;
    List.iter walk_alt alts
  | Expr.Let (_, binds, body) ->
    List.iter walk_bind binds;
    walk_expr body

and walk_alt (a : Expr.case_alternative) : unit =
  List.iter walk_binder a.binders;
  match a.result with
  | Either.Left gs ->
    saw_guarded_alt := true;
    List.iter
      (fun (g : Expr.guard) ->
         walk_expr g.guard;
         walk_expr g.expression)
      gs
  | Either.Right e -> walk_expr e

and walk_bind = function
  | Expr.NonRec (_, _, e) -> walk_expr e
  | Expr.Rec rbs -> List.iter (fun (rb : Expr.rec_binding) -> walk_expr rb.expr) rbs

and walk_binder (b : Expr.binder) : unit =
  let walk_lit = function
    | Literal.LitArray xs -> List.iter walk_binder xs
    | Literal.LitObject kvs -> List.iter (fun (_, x) -> walk_binder x) kvs
    | _ -> ()
  in
  match b with
  | Expr.NullBinder _ | Expr.VarBinder _ -> ()
  | Expr.LiteralBinder (_, lit) ->
    saw_literal_binder := true;
    (match lit with
     | Literal.LitArray _ -> saw_array_binder := true
     | Literal.LitObject _ -> saw_record_binder := true
     | _ -> ());
    walk_lit lit
  | Expr.ConstructorBinder (_, _, _, subs) ->
    saw_constructor_binder := true;
    List.iter walk_binder subs
  | Expr.NamedBinder (_, _, inner) ->
    saw_named_binder := true;
    walk_binder inner

let () = List.iter walk_bind m.decls

let test_saw_constructor_binder () =
  Alcotest.(check bool) "ConstructorBinder decoded" true !saw_constructor_binder

let test_saw_literal_binder () =
  Alcotest.(check bool) "LiteralBinder decoded" true !saw_literal_binder

let test_saw_named_binder () =
  Alcotest.(check bool) "NamedBinder decoded" true !saw_named_binder

let test_saw_array_binder () =
  Alcotest.(check bool) "array LiteralBinder decoded" true !saw_array_binder

let test_saw_record_binder () =
  Alcotest.(check bool) "record LiteralBinder decoded" true !saw_record_binder

let test_saw_guarded_alt () =
  Alcotest.(check bool) "guarded alternative decoded" true !saw_guarded_alt

let test_saw_object_update () =
  Alcotest.(check bool) "ObjectUpdate decoded" true !saw_object_update

(* --- failure ------------------------------------------------------------- *)

(* A structurally invalid module (missing required fields) fails loudly. *)
let test_decode_error () =
  match Decode.module_of_string "{}" with
  | exception Decode.Decode_error _ -> ()
  | _ -> Alcotest.fail "expected Decode_error on an empty object"

let () =
  Alcotest.run
    "corefn"
    [ ( "module"
      , [ Alcotest.test_case "module_name" `Quick test_module_name
        ; Alcotest.test_case "has_decls" `Quick test_has_decls
        ] )
    ; ( "literals"
      , [ Alcotest.test_case "lit_int" `Quick test_lit_int
        ; Alcotest.test_case "lit_number" `Quick test_lit_number
        ; Alcotest.test_case "lit_string" `Quick test_lit_string
        ; Alcotest.test_case "lit_char" `Quick test_lit_char
        ; Alcotest.test_case "lit_bool" `Quick test_lit_bool
        ; Alcotest.test_case "lit_array" `Quick test_lit_array
        ; Alcotest.test_case "lit_object" `Quick test_lit_object
        ] )
    ; ( "nodes"
      , [ Alcotest.test_case "ctor_nullary" `Quick test_ctor_nullary
        ; Alcotest.test_case "ctor_unary" `Quick test_ctor_unary
        ; Alcotest.test_case "var_qualified" `Quick test_var_qualified
        ; Alcotest.test_case "accessor" `Quick test_accessor
        ; Alcotest.test_case "constructor_binder" `Quick test_saw_constructor_binder
        ; Alcotest.test_case "literal_binder" `Quick test_saw_literal_binder
        ; Alcotest.test_case "named_binder" `Quick test_saw_named_binder
        ; Alcotest.test_case "array_binder" `Quick test_saw_array_binder
        ; Alcotest.test_case "record_binder" `Quick test_saw_record_binder
        ; Alcotest.test_case "guarded_alt" `Quick test_saw_guarded_alt
        ; Alcotest.test_case "object_update" `Quick test_saw_object_update
        ] )
    ; "failure", [ Alcotest.test_case "decode_error" `Quick test_decode_error ]
    ]
