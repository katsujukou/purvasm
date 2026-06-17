open Cesk
open Cesk.Ast

(* Smart constructors keep the sample programs readable. There is no parser yet,
   so programs are built directly as core terms. *)
let num n = Lit (LInt n)
let add a b = Prim (Add, [ a; b ])
let sub a b = Prim (Sub, [ a; b ])
let mul a b = Prim (Mul, [ a; b ])
let lt a b = Prim (Lt, [ a; b ])

let samples : (string * term) list =
  [ "arith: (2+3)*2", mul (add (num 2) (num 3)) (num 2)
  ; "let: let x=2 in x+1", Let ("x", num 2, add (Var "x") (num 1))
  ; "lambda: (\\x -> x+1) 4", App (Lam ("x", add (Var "x") (num 1)), num 4)
  ; ( "closure: let a=10 in (\\x -> x+a) 5"
    , Let ("a", num 10, App (Lam ("x", add (Var "x") (Var "a")), num 5)) )
  ; ( "higher-order: (\\f -> f (f 1)) (\\n -> n*2)"
    , App
        (Lam ("f", App (Var "f", App (Var "f", num 1))), Lam ("n", mul (Var "n") (num 2)))
    )
  ; "if: if 1<2 then 10 else 20", If (lt (num 1) (num 2), num 10, num 20)
  ; ( "letrec: fact 5"
    , Letrec
        ( "fact"
        , Lam
            ( "n"
            , If
                ( lt (Var "n") (num 1)
                , num 1
                , mul (Var "n") (App (Var "fact", sub (Var "n") (num 1))) ) )
        , App (Var "fact", num 5) ) )
  ]
;;

let run_all (trace : bool) : unit =
  List.iter
    (fun (name, term) ->
       if trace then Stdlib.print_endline ("=== " ^ name ^ " ===");
       let v = Machine.eval ~trace term in
       Stdlib.Printf.printf "%-44s => %s\n" name (Value.to_string v))
    samples
;;

let trace_arg =
  let open Cmdliner in
  Arg.(value & flag & info [ "t"; "trace" ] ~doc:"Print every CESK transition.")
;;

let cmd =
  let open Cmdliner in
  let doc = "Run sample programs on the CESK machine." in
  Cmd.v (Cmd.info "cesk" ~version:"0.1.0" ~doc) Term.(const run_all $ trace_arg)
;;

let () = Stdlib.exit (Cmdliner.Cmd.eval cmd)
