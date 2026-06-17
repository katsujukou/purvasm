open Cesk
open Cesk.Ast

(* Smart constructors keep the sample programs readable. There is no parser yet,
   so programs are built directly as core terms. *)
let num n = Lit (LInt n)
let add_int a b = Prim (AddInt, [ a; b ])
let sub_int a b = Prim (SubInt, [ a; b ])
let mul_int a b = Prim (MulInt, [ a; b ])
let lt_int a b = Prim (LtInt, [ a; b ])
let lt_string a b = Prim (LtString, [ a; b ])
let eq_int a b = Prim (EqInt, [ a; b ])
let str s = Lit (LString s)
let append a b = Prim (Append, [ a; b ])
let numf f = Lit (LNumber f)
let add_num a b = Prim (AddNumber, [ a; b ])
let div_num a b = Prim (DivNumber, [ a; b ])

let samples : (string * term) list =
  [ "arith: (2+3)*2", mul_int (add_int (num 2) (num 3)) (num 2)
  ; "let: let x=2 in x+1", Let ("x", num 2, add_int (Var "x") (num 1))
  ; "lambda: (\\x -> x+1) 4", App (Lam ("x", add_int (Var "x") (num 1)), num 4)
  ; ( "closure: let a=10 in (\\x -> x+a) 5"
    , Let ("a", num 10, App (Lam ("x", add_int (Var "x") (Var "a")), num 5)) )
  ; ( "higher-order: (\\f -> f (f 1)) (\\n -> n*2)"
    , App
        ( Lam ("f", App (Var "f", App (Var "f", num 1)))
        , Lam ("n", mul_int (Var "n") (num 2)) ) )
  ; "if: if 1<2 then 10 else 20", If (lt_int (num 1) (num 2), num 10, num 20)
  ; ( "letrec: fact 5"
    , Letrec
        ( [ ( "fact"
            , Lam
                ( "n"
                , If
                    ( lt_int (Var "n") (num 1)
                    , num 1
                    , mul_int (Var "n") (App (Var "fact", sub_int (Var "n") (num 1))) ) )
            )
          ]
        , App (Var "fact", num 5) ) )
  ; ( "mutual: even 10"
    , Letrec
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
        , App (Var "even", num 10) ) )
  ; "string: \"foo\" <> \"bar\"", append (str "foo") (str "bar")
  ; ( "string: if \"a\" < \"b\" then \"yes\" else \"no\""
    , If (lt_string (str "a") (str "b"), str "yes", str "no") )
  ; "number: 1.5 + 2.0", add_num (numf 1.5) (numf 2.0)
  ; "number: 10.0 / 4.0", div_num (numf 10.0) (numf 4.0)
  ]

let run_all (trace : bool) : unit =
  List.iter
    (fun (name, term) ->
       if trace then Stdlib.print_endline ("=== " ^ name ^ " ===");
       let v = Machine.eval ~trace term in
       Stdlib.Printf.printf "%-44s => %s\n" name (Value.to_string v))
    samples

let trace_arg =
  let open Cmdliner in
  Arg.(value & flag & info [ "t"; "trace" ] ~doc:"Print every CESK transition.")

let cmd =
  let open Cmdliner in
  let doc = "Run sample programs on the CESK machine." in
  Cmd.v (Cmd.info "cesk" ~version:"0.1.0" ~doc) Term.(const run_all $ trace_arg)

let () = Stdlib.exit (Cmdliner.Cmd.eval cmd)
