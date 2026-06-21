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

(* --- cross-module linking (ADR-0016) ------------------------------------- *)

(* A multi-module program fixture: Main imports Lib, plus one foreign import. *)
let app_outdir = "../fixtures/app"

let program (entry : string) : C.term =
  Link.link_program ~outdir:app_outdir ~entry_module:[ "Main" ] ~entry ()

let run_program (entry : string) : V.t = Cesk.Machine.eval (program entry)

(* `Main.result = unbox (Box 42)` — a cross-module reference (Main -> Lib) that
   resolves and runs after linking. *)
let test_link_result () =
  Alcotest.(check int) "Main.result" 42 (as_int (run_program "result"))

(* `Main.mapped = unbox (mapBox identity (Box 42))` — a guest closure passed into
   a function defined in another module. *)
let test_link_higher_order () =
  Alcotest.(check int) "Main.mapped" 42 (as_int (run_program "mapped"))

(* load resolves the entry's transitive import closure (Main + Lib); Prim has no
   corefn.json and is skipped. *)
let test_link_loads_closure () =
  Alcotest.(check int)
    "loaded module count"
    2
    (List.length (Link.load ~outdir:app_outdir ~entry_module:[ "Main" ]))

(* `usesForeign` is a function, so the program links and loads; applying it forces
   the foreign `native`, which the empty resolver leaves unbound -> stuck (the
   ADR-0016 FFI seam). *)
let test_link_foreign_unbound () =
  match Cesk.Machine.eval (C.App (program "usesForeign", int 0)) with
  | exception Cesk.Errors.Machine_error _ -> ()
  | _ -> Alcotest.fail "forcing an unresolved foreign should be stuck"

(* Lowering a bare cross-module reference is also stuck when unbound. *)
let test_external_unbound () =
  match
    Cesk.Machine.eval (Lower.lower_var (Corefn.Names.Qualified (Some [ "Other" ], "foo")))
  with
  | exception Cesk.Errors.Machine_error _ -> ()
  | _ -> Alcotest.fail "an unresolved external reference should be stuck"

(* --- module-graph shapes (ADR-0016 loading + topo order) ----------------- *)

let run_at ~(outdir : string) ~(entry_module : string list) (entry : string) : V.t =
  Cesk.Machine.eval (Link.link_program ~outdir ~entry_module ~entry ())

let closure_size ~(outdir : string) ~(entry_module : string list) : int =
  List.length (Link.load ~outdir ~entry_module)

(* Transitive: TransA -> TransB -> TransC; the value threads through all three. *)
let test_transitive () =
  Alcotest.(check int)
    "TransA.a = TransC.c via TransB"
    3
    (as_int (run_at ~outdir:"../fixtures/trans" ~entry_module:[ "TransA" ] "a"))

let test_transitive_closure () =
  Alcotest.(check int)
    "transitive closure size"
    3
    (closure_size ~outdir:"../fixtures/trans" ~entry_module:[ "TransA" ])

(* Diamond: DiaA -> {DiaB, DiaC} -> DiaD. Both paths reach the one DiaD, which is
   loaded and bound once; `both = Two b c` forces both. *)
let test_diamond () =
  match run_at ~outdir:"../fixtures/diamond" ~entry_module:[ "DiaA" ] "both" with
  | V.VData { tag = "Two"; fields } ->
    Alcotest.(check (list int))
      "Two 7 7"
      [ 7; 7 ]
      (List.map as_int (Array.to_list fields))
  | _ -> Alcotest.fail "DiaA.both should be Two 7 7"

let test_diamond_closure () =
  Alcotest.(check int)
    "diamond closure size (DiaD once)"
    4
    (closure_size ~outdir:"../fixtures/diamond" ~entry_module:[ "DiaA" ])

(* Re-exports: Main imports `origin` via Reexport (which re-exports Origin); the
   reference is to the canonical Origin.origin, so it resolves after linking even
   though Reexport itself contributes no bindings. *)
let test_reexport () =
  Alcotest.(check int)
    "re-exported origin resolves to canonical Origin"
    5
    (as_int (run_at ~outdir:"../fixtures/reexport" ~entry_module:[ "Main" ] "result"))

(* --- real Prelude through the FFI provider ladder + newtype dicts --------- *)

(* A program compiled against the real `prelude` package. Its arithmetic / Eq /
   HeytingAlgebra type-class dictionaries are newtype-wrapped records (erased by
   lowering, ADR-0018) whose foreign leaves resolve through the intrinsic rung of
   the FFI ladder (ADR-0017). *)
let run_prelude (entry : string) : V.t =
  Cesk.Machine.eval
    ~host:Ffi.host
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/prelude"
       ~entry_module:[ "Main" ]
       ~entry
       ())

(* answer = add (mul 2 3) (sub 5 4) = 7 — Semiring + Ring dictionaries. *)
let test_prelude_answer () =
  Alcotest.(check int) "answer" 7 (as_int (run_prelude "answer"))

(* quotient = div 17 5 = 3 — EuclideanRing dictionary (incl. degree). *)
let test_prelude_div () =
  Alcotest.(check int) "quotient" 3 (as_int (run_prelude "quotient"))

(* isEq = eq answer 7 = true — Eq Int dictionary. *)
let test_prelude_eq () = Alcotest.(check bool) "isEq" true (as_bool (run_prelude "isEq"))

(* both = conj isEq (not false) = true — HeytingAlgebra Boolean dictionary. *)
let test_prelude_bool () =
  Alcotest.(check bool) "both" true (as_bool (run_prelude "both"))

(* doubled = map (add 1) [1,2,3] = [2,3,4] — the Functor Array instance via the
   structural `arrayMap` (ADR-0020), the mapper a real guest closure. Its closure
   drags in Data.Show/Data.Ord, whose unused instance CAFs are dropped by
   reachability DCE (ADR-0021) rather than forcing their unimplemented leaves. *)
let test_prelude_map () =
  match run_prelude "doubled" with
  | V.VArray a ->
    Alcotest.(check (list int)) "doubled" [ 2; 3; 4 ] (List.map as_int (Array.to_list a))
  | _ -> Alcotest.fail "doubled should be a VArray"

(* show on scalars goes through the native foreign rung (ADR-0022): the `Show`
   dictionary's leaf resolves to an opaque `Foreign` reference whose host
   implementation runs on the evaluated value. DCE links only the reached leaf. *)
let test_prelude_show_int () =
  Alcotest.(check string) "show 42" "42" (as_string (run_prelude "shownInt"))

(* showNumberImpl's "x.0" rule for integral numbers. *)
let test_prelude_show_number () =
  Alcotest.(check string) "show 3.0" "3.0" (as_string (run_prelude "shownNum"))

(* showCharImpl single-quotes (Char is its code point). *)
let test_prelude_show_char () =
  Alcotest.(check string) "show 'a'" "'a'" (as_string (run_prelude "shownChar"))

(* showStringImpl double-quotes and escapes control characters. *)
let test_prelude_show_string () =
  Alcotest.(check string)
    "show \"hi\\n\""
    "\"hi\\n\""
    (as_string (run_prelude "shownStr"))

(* `Control.Lazy.fix f = go where go = defer \_ -> f go` — `go` is a recursive
   *value* whose right-hand side is a general application (not a syntactic lambda
   or constructor). It links and runs only if `letrec` evaluates an arbitrary RHS
   while the self-reference (captured under the `defer` thunk) is not forced. Here
   `sumTo = fix \rec n -> if n == 0 then 0 else n + rec (n-1)`, `sumTo 5 = 15`. *)
let test_lazy_fix () =
  Alcotest.(check int)
    "sumTo 5 via fix"
    15
    (as_int
       (Cesk.Machine.eval
          ~host:Ffi.host
          (Link.link_program
             ~resolver:Ffi.resolver
             ~outdir:"../fixtures/lazy"
             ~entry_module:[ "Main" ]
             ~entry:"result"
             ())))

(* A recursive *value* whose right-hand side is a constructor application, not a
   lambda: `fibAnd = Fib "fib" \n -> … case fibAnd of Fib _ f -> f (n-k) …`. The
   self-reference is reached by pattern-matching the recursive value back open
   inside the closure, so it is only forced after `letrec` has backpatched it.
   `fib 10 = 55`. Exercises Ord (`n < 2`) through the scalar `ordIntImpl` leaf. *)
let test_recursive_value_fib () =
  Alcotest.(check int)
    "fib 10"
    55
    (as_int
       (Cesk.Machine.eval
          ~host:Ffi.host
          (C.App
             ( Link.link_program
                 ~resolver:Ffi.resolver
                 ~outdir:"../fixtures/fib"
                 ~entry_module:[ "FibAnd" ]
                 ~entry:"fib"
                 ()
             , int 10 ))))

(* --- Effect runtime (ADR-0023), unblocked by by-need recursion (ADR-0024) - *)

(* Run a program's `main :: Effect a` through the full pipeline with the FFI
   resolver and host registry. The closure pulls `Effect`'s mutually recursive
   instance dictionaries, which only link because recursive bindings are by-need. *)
let run_effect_program ~(outdir : string) ~(entry_module : string list) : V.t =
  Cesk.Machine.run_effect
    ~host:Ffi.host
    (Link.link_program ~resolver:Ffi.resolver ~outdir ~entry_module ~entry:"main" ())

(* `main = do { r <- Ref.new 0; _ <- modify (_+1) r; _ <- modify (_+10) r; read r }`
   = 11. Exercises the Effect monad (bindE), Effect.Ref as a one-cell mutable
   array, and the run-to-completion driver — all observed as a returned value. *)
let test_effect_ref () =
  Alcotest.(check int)
    "ref counter"
    11
    (as_int
       (run_effect_program ~outdir:"../fixtures/effect_ref" ~entry_module:[ "RefMain" ]))

(* Capture writes to stdout (fd 1) while running [f]. *)
let with_captured_stdout (f : unit -> unit) : string =
  flush stdout;
  let tmp = Filename.temp_file "purvasm" ".out" in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let saved = Unix.dup Unix.stdout in
  Unix.dup2 fd Unix.stdout;
  Unix.close fd;
  let restore () =
    flush stdout;
    Unix.dup2 saved Unix.stdout;
    Unix.close saved
  in
  (try f () with
   | e ->
     restore ();
     raise e);
  restore ();
  let ic = open_in_bin tmp in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove tmp;
  s

(* `main = do { log "Hello"; log "World" }` — the one genuinely effectful leaf
   (`Effect.Console.log`) performs real stdout writes, in order, when each Effect
   thunk is forced. *)
let test_effect_console () =
  let out =
    with_captured_stdout (fun () ->
      ignore
        (run_effect_program
           ~outdir:"../fixtures/effect_console"
           ~entry_module:[ "ConsoleMain" ]))
  in
  Alcotest.(check string) "console output" "Hello\nWorld\n" out

(* --- newtype erasure, including through as-patterns (ADR-0018) ------------ *)

(* `newtype Box = Box Int`; at runtime a Box *is* its Int (the wrapper is erased),
   so a Box argument is built as the bare int. *)
let run_nt (entry : string) (arg : C.term) : V.t =
  Cesk.Machine.eval
    (C.App
       ( Link.link_program ~outdir:"../fixtures/newtype" ~entry_module:[ "Main" ] ~entry ()
       , arg ))

(* A plain newtype match `case b of Box n -> n`. *)
let test_newtype_match () =
  Alcotest.(check int) "unInner (Box 7)" 7 (as_int (run_nt "unInner" (int 7)))

(* The crux: `w@(Box _)` — the as-binding `w` must survive newtype erasure (it is
   re-matched through the erased newtype to extract the inner value). *)
let test_newtype_as_pattern () =
  Alcotest.(check int) "asInner (Box 7)" 7 (as_int (run_nt "asInner" (int 7)))

(* The as-bound whole returned directly. *)
let test_newtype_as_whole () =
  Alcotest.(check int) "asWhole (Box 7)" 7 (as_int (run_nt "asWhole" (int 7)))

(* --- lower IR round-trip against the oracle (ADR-0025) -------------------- *)

(* The differential check: a real linked program, normalised to ANF and re-curried
   back, evaluates to the same value on the oracle as the original. This validates
   `transl`/`rev_transl` over the whole pipeline (dictionaries, foreign leaves,
   `case`, by-need recursion). Later optimiser passes reuse exactly this harness. *)
let prelude_term (entry : string) : C.term =
  Link.link_program
    ~resolver:Ffi.resolver
    ~outdir:"../fixtures/prelude"
    ~entry_module:[ "Main" ]
    ~entry
    ()

let same_after_roundtrip (label : string) (t : C.term) =
  let direct = Cesk.Machine.eval ~host:Ffi.host t in
  let via_anf =
    Cesk.Machine.eval
      ~host:Ffi.host
      (Middle_end.Transl.rev_transl (Middle_end.Transl.transl t))
  in
  Alcotest.(check string) label (V.to_string direct) (V.to_string via_anf)

let test_rt_fixture () =
  same_after_roundtrip "fixture.anInt" (Lower.module_ fixture ~entry:"anInt")

let test_rt_app () = same_after_roundtrip "app.result" (program "result")

let test_rt_prelude_answer () =
  same_after_roundtrip "prelude.answer" (prelude_term "answer")

let test_rt_prelude_doubled () =
  same_after_roundtrip "prelude.doubled" (prelude_term "doubled")

let test_rt_prelude_show () =
  same_after_roundtrip "prelude.shownStr" (prelude_term "shownStr")

let test_rt_newtype () =
  same_after_roundtrip
    "newtype.asInner 7"
    (C.App
       ( Link.link_program
           ~outdir:"../fixtures/newtype"
           ~entry_module:[ "Main" ]
           ~entry:"asInner"
           ()
       , int 7 ))

let test_rt_fib () =
  same_after_roundtrip
    "fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

(* Effect programs round-trip too: same returned value, and same effect order. *)
let test_rt_effect_ref () =
  let t =
    Link.link_program
      ~resolver:Ffi.resolver
      ~outdir:"../fixtures/effect_ref"
      ~entry_module:[ "RefMain" ]
      ~entry:"main"
      ()
  in
  Alcotest.(check int)
    "effect ref round-trips"
    (as_int (Cesk.Machine.run_effect ~host:Ffi.host t))
    (as_int
       (Cesk.Machine.run_effect
          ~host:Ffi.host
          (Middle_end.Transl.rev_transl (Middle_end.Transl.transl t))))

let test_rt_effect_console () =
  let t =
    Link.link_program
      ~resolver:Ffi.resolver
      ~outdir:"../fixtures/effect_console"
      ~entry_module:[ "ConsoleMain" ]
      ~entry:"main"
      ()
  in
  let direct =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  let via_anf =
    with_captured_stdout (fun () ->
      ignore
        (Cesk.Machine.run_effect
           ~host:Ffi.host
           (Middle_end.Transl.rev_transl (Middle_end.Transl.transl t))))
  in
  Alcotest.(check string) "effect console round-trips (same stdout)" direct via_anf

(* --- DictElim (ADR-0027) preserves semantics ----------------------------- *)

(* The pass is sound iff round-tripping through it agrees with the oracle. *)
let dictelim (t : C.term) : C.term =
  Middle_end.Transl.rev_transl
    (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl t))

let same_after_dictelim (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host (dictelim t)))

let test_de_prelude_answer () = same_after_dictelim "answer" (prelude_term "answer")
let test_de_prelude_doubled () = same_after_dictelim "doubled" (prelude_term "doubled")
let test_de_prelude_show () = same_after_dictelim "shownStr" (prelude_term "shownStr")

let test_de_fib () =
  same_after_dictelim
    "fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

(* Effect's mutually recursive dicts have computed fields; DictElim must leave
   them alone and stay correct. *)
let test_de_effect_ref () =
  let t =
    Link.link_program
      ~resolver:Ffi.resolver
      ~outdir:"../fixtures/effect_ref"
      ~entry_module:[ "RefMain" ]
      ~entry:"main"
      ()
  in
  Alcotest.(check int)
    "effect ref via dictelim"
    (as_int (Cesk.Machine.run_effect ~host:Ffi.host t))
    (as_int (Cesk.Machine.run_effect ~host:Ffi.host (dictelim t)))

(* --- DictElim + Simplify (ADR-0028) preserves semantics ------------------ *)

(* The full optimiser pipeline so far; sound iff it agrees with the oracle. *)
let opt (t : C.term) : C.term =
  Middle_end.Transl.rev_transl
    (Middle_end.Passes.Simplify.run
       (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl t)))

let same_after_opt (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host (opt t)))

let test_opt_prelude_answer () = same_after_opt "answer" (prelude_term "answer")
let test_opt_prelude_doubled () = same_after_opt "doubled" (prelude_term "doubled")
let test_opt_prelude_show () = same_after_opt "shownStr" (prelude_term "shownStr")
let test_opt_app () = same_after_opt "app.result" (program "result")

let test_opt_newtype () =
  same_after_opt
    "newtype.asInner 7"
    (C.App
       ( Link.link_program
           ~outdir:"../fixtures/newtype"
           ~entry_module:[ "Main" ]
           ~entry:"asInner"
           ()
       , int 7 ))

let test_opt_fib () =
  same_after_opt
    "fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

let test_opt_effect_ref () =
  let t =
    Link.link_program
      ~resolver:Ffi.resolver
      ~outdir:"../fixtures/effect_ref"
      ~entry_module:[ "RefMain" ]
      ~entry:"main"
      ()
  in
  Alcotest.(check int)
    "effect ref via opt"
    (as_int (Cesk.Machine.run_effect ~host:Ffi.host t))
    (as_int (Cesk.Machine.run_effect ~host:Ffi.host (opt t)))

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
    ; ( "linking"
      , [ Alcotest.test_case "link_result" `Quick test_link_result
        ; Alcotest.test_case "link_higher_order" `Quick test_link_higher_order
        ; Alcotest.test_case "link_loads_closure" `Quick test_link_loads_closure
        ; Alcotest.test_case "link_foreign_unbound" `Quick test_link_foreign_unbound
        ; Alcotest.test_case "external_unbound" `Quick test_external_unbound
        ] )
    ; ( "newtype"
      , [ Alcotest.test_case "match" `Quick test_newtype_match
        ; Alcotest.test_case "as_pattern" `Quick test_newtype_as_pattern
        ; Alcotest.test_case "as_whole" `Quick test_newtype_as_whole
        ] )
    ; ( "recursive-value"
      , [ Alcotest.test_case "lazy_fix" `Quick test_lazy_fix
        ; Alcotest.test_case "fib_and" `Quick test_recursive_value_fib
        ] )
    ; ( "effect"
      , [ Alcotest.test_case "ref" `Quick test_effect_ref
        ; Alcotest.test_case "console" `Quick test_effect_console
        ] )
    ; ( "lower-ir"
      , [ Alcotest.test_case "fixture" `Quick test_rt_fixture
        ; Alcotest.test_case "app" `Quick test_rt_app
        ; Alcotest.test_case "prelude_answer" `Quick test_rt_prelude_answer
        ; Alcotest.test_case "prelude_doubled" `Quick test_rt_prelude_doubled
        ; Alcotest.test_case "prelude_show" `Quick test_rt_prelude_show
        ; Alcotest.test_case "newtype" `Quick test_rt_newtype
        ; Alcotest.test_case "fib" `Quick test_rt_fib
        ; Alcotest.test_case "effect_ref" `Quick test_rt_effect_ref
        ; Alcotest.test_case "effect_console" `Quick test_rt_effect_console
        ] )
    ; ( "dictelim"
      , [ Alcotest.test_case "prelude_answer" `Quick test_de_prelude_answer
        ; Alcotest.test_case "prelude_doubled" `Quick test_de_prelude_doubled
        ; Alcotest.test_case "prelude_show" `Quick test_de_prelude_show
        ; Alcotest.test_case "fib" `Quick test_de_fib
        ; Alcotest.test_case "effect_ref" `Quick test_de_effect_ref
        ] )
    ; ( "opt"
      , [ Alcotest.test_case "prelude_answer" `Quick test_opt_prelude_answer
        ; Alcotest.test_case "prelude_doubled" `Quick test_opt_prelude_doubled
        ; Alcotest.test_case "prelude_show" `Quick test_opt_prelude_show
        ; Alcotest.test_case "app" `Quick test_opt_app
        ; Alcotest.test_case "newtype" `Quick test_opt_newtype
        ; Alcotest.test_case "fib" `Quick test_opt_fib
        ; Alcotest.test_case "effect_ref" `Quick test_opt_effect_ref
        ] )
    ; ( "prelude"
      , [ Alcotest.test_case "answer" `Quick test_prelude_answer
        ; Alcotest.test_case "div" `Quick test_prelude_div
        ; Alcotest.test_case "eq" `Quick test_prelude_eq
        ; Alcotest.test_case "bool" `Quick test_prelude_bool
        ; Alcotest.test_case "map" `Quick test_prelude_map
        ; Alcotest.test_case "show_int" `Quick test_prelude_show_int
        ; Alcotest.test_case "show_number" `Quick test_prelude_show_number
        ; Alcotest.test_case "show_char" `Quick test_prelude_show_char
        ; Alcotest.test_case "show_string" `Quick test_prelude_show_string
        ] )
    ; ( "module-graph"
      , [ Alcotest.test_case "transitive" `Quick test_transitive
        ; Alcotest.test_case "transitive_closure" `Quick test_transitive_closure
        ; Alcotest.test_case "diamond" `Quick test_diamond
        ; Alcotest.test_case "diamond_closure" `Quick test_diamond_closure
        ; Alcotest.test_case "reexport" `Quick test_reexport
        ] )
    ]
