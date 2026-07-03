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
    (List.length (Link.load ~outdir:app_outdir ~entry_module:[ "Main" ] ()))

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
  List.length (Link.load ~outdir ~entry_module ())

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

(* The full optimiser pipeline so far; sound iff it agrees with the oracle. DBE
   (ADR-0034) runs last, consuming the effect analysis fed by [Ffi]'s leaf bits. *)
let opt (t : C.term) : C.term =
  Middle_end.Transl.rev_transl
    (Middle_end.Passes.Dbe.run
       ~effectful_leaf:Ffi.effectful
       ~foreign_arity:Ffi.foreign_arity
       (Middle_end.Passes.Simplify.run
          (Middle_end.Passes.Dict_elim.run (Middle_end.Transl.transl t))))

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

(* Structural node counts — the basis for "effectiveness" tests: a behaviour-only
   differential test can verify an optimiser is *sound* but not that it *fired* (a pass
   that silently became a no-op still passes every differential). Each pass below is
   asserted to move the metric it uniquely owns. Returns (lets, accessors, prims,
   apps). *)
let node_counts (e : Middle_end.Anf.expr) : int * int * int * int =
  let open Middle_end.Anf in
  let l = ref 0
  and acc = ref 0
  and pr = ref 0
  and ap = ref 0 in
  let rec ex = function
    | Ret c -> cx c
    | Let (_, c, b) ->
      incr l;
      cx c;
      ex b
    | LetRec (g, b) ->
      List.iter
        (fun (_, r) ->
           incr l;
           ex r)
        g;
      ex b
  and cx = function
    | CAccessor _ -> incr acc
    | CPrim (_, _) -> incr pr
    | CApp (_, _) -> incr ap
    | CLam (_, b) -> ex b
    | CIf (_, t, e) ->
      ex t;
      ex e
    | CCase (_, alts) ->
      List.iter
        (fun (al : alt) ->
           match al.result with
           | Uncond e -> ex e
           | Guarded gs ->
             List.iter
               (fun (g, e) ->
                  ex g;
                  ex e)
               gs)
        alts
    | _ -> ()
  in
  ex e;
  !l, !acc, !pr, !ap

let n_lets e =
  let l, _, _, _ = node_counts e in
  l

let n_apps e =
  let _, _, _, a = node_counts e in
  a

let transl = Middle_end.Transl.transl
let dictelim t = Middle_end.Passes.Dict_elim.run (transl t)
let simplify t = Middle_end.Passes.Simplify.run (dictelim t)
let opt_pre (t : C.term) = simplify t

let opt_post (t : C.term) =
  Middle_end.Passes.Dbe.run
    ~effectful_leaf:Ffi.effectful
    ~foreign_arity:Ffi.foreign_arity
    (opt_pre t)

(* The fib program (Ord + Semiring dictionaries) — a rich witness for every pass. *)
let fib10 () =
  C.App
    ( Link.link_program
        ~resolver:Ffi.resolver
        ~outdir:"../fixtures/fib"
        ~entry_module:[ "FibAnd" ]
        ~entry:"fib"
        ()
    , int 10 )

(* DictElim (ADR-0027) collapses statically-known dictionary dispatch, removing the
   dictionary-method *applications* (it leaves the let-count untouched — that is
   Simplify's job below, so the two metrics attribute cleanly). *)
let test_dictelim_fires () =
  let t = fib10 () in
  let before = n_apps (transl t)
  and after = n_apps (dictelim t) in
  Alcotest.(check bool)
    (Printf.sprintf "DictElim removes dispatch apps (%d -> %d)" before after)
    true
    (after < before)

(* Simplify (ADR-0028) copy-propagates, dropping the alias [let]s DictElim leaves; the
   let-count strictly drops (DictElim did not move it). *)
let test_simplify_fires () =
  let t = fib10 () in
  let before = n_lets (dictelim t)
  and after = n_lets (simplify t) in
  Alcotest.(check bool)
    (Printf.sprintf "Simplify drops alias lets (%d -> %d)" before after)
    true
    (after < before)

(* DBE (ADR-0034) drops dead pure bindings. The whole-module lowering keeps a module's
   private bindings (no cross-module reachability DCE here, as in per-module compile),
   so an entry like `classify` leaves dead pure helpers DBE must remove. *)
let dbe_fires (label : string) (t : C.term) =
  let pre = n_lets (opt_pre t)
  and post = n_lets (opt_post t) in
  Alcotest.(check bool)
    (Printf.sprintf "%s: DBE drops dead lets (%d -> %d)" label pre post)
    true
    (post < pre)

let test_dbe_fires_classify () =
  dbe_fires "classify" (Lower.module_ fixture ~entry:"classify")

let test_dbe_fires_anint () = dbe_fires "anInt" (Lower.module_ fixture ~entry:"anInt")

(* --- OCaml native backend (ADR-0036) differential equivalence ------------- *)

(* Compile a term through the OCaml backend to a real native executable and run it:
   ANF (ADR-0025) → OCaml source → `ocamlopt` → run → its printed value. This is the
   full native path — a purs-compiled `corefn.json` fixture, lowered and codegen'd to
   native, must print what the CESK oracle computes. *)
let ocaml_run ?(is_effect = false) (t : C.term) : string =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "purvasm_ml_%x" (Hashtbl.hash t))
  in
  (try Sys.mkdir dir 0o755 with
   | Sys_error _ -> ());
  let src = Ocaml_backend.Codegen_ml.program ~is_effect (Middle_end.Transl.transl t) in
  let oc = open_out (Filename.concat dir "gen.ml") in
  output_string oc src;
  close_out oc;
  if
    Sys.command (Printf.sprintf "cd %s && ocamlfind ocamlopt gen.ml -o gen 2>err" dir)
    <> 0
  then Alcotest.failf "ocamlopt failed; see %s/err and %s/gen.ml" dir dir;
  if Sys.command (Printf.sprintf "%s/gen > %s/out" dir dir) <> 0
  then Alcotest.fail "generated program crashed";
  let ic = open_in (Filename.concat dir "out") in
  let s = In_channel.input_all ic in
  close_in ic;
  s

let same_on_ocaml (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (ocaml_run t)

(* An Effect entry's observable is its effects (stdout); the native runner performs them
   and suppresses the (`Unit`) result, matching `purvm run`. So compare the generated
   program's stdout to the oracle's captured `run_effect` stdout. *)
let same_on_ocaml_effect (label : string) (t : C.term) =
  let out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  Alcotest.(check string) label out (ocaml_run ~is_effect:true t)

(* --- LLVM native backend (ADR-0071/0072) differential equivalence --------- *)

(* Locate the owned runtime `staticlib` (ADR-0071 §1) the LLVM output links against. It is built by
   `cargo` outside dune, so we take it from `$PURVASM_RT_A` or the conventional built path under the
   repo root (found by walking up for `runtime/Cargo.toml`). Absent (runtime not built, or no cargo) →
   the llvm-backend group is skipped with a notice rather than failing the whole suite. *)
let repo_root : string option =
  let rec up d =
    if Sys.file_exists (Filename.concat d "runtime/Cargo.toml")
    then Some d
    else (
      let p = Filename.dirname d in
      if p = d then None else up p)
  in
  up (Sys.getcwd ())

let rt_staticlib : string option =
  let exists p = if Sys.file_exists p then Some p else None in
  match Sys.getenv_opt "PURVASM_RT_A" with
  | Some p when Sys.file_exists p -> Some p
  | _ ->
    Option.bind repo_root (fun root ->
      match exists (Filename.concat root "runtime/target/debug/libpurvasm_rt.a") with
      | Some p -> Some p
      | None -> exists (Filename.concat root "runtime/target/release/libpurvasm_rt.a"))

let has_clang : bool = Sys.command "clang --version >/dev/null 2>&1" = 0
let llvm_available : bool = rt_staticlib <> None && has_clang

(* Compile a term through the LLVM backend to a native executable and run it: ANF (ADR-0025) → LLVM IR
   → `clang` (link the runtime `staticlib`) → run → its output. The **fourth** differential
   implementation (ADR-0064 §7), held to the CESK oracle's value / `Effect`-order. *)
let llvm_run ?(is_effect = false) ?(modular = false) ?heap_words (t : C.term) : string =
  let rt = Option.get rt_staticlib in
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "purvasm_ll_%x" (Hashtbl.hash (t, heap_words, modular)))
  in
  (try Sys.mkdir dir 0o755 with
   | Sys_error _ -> ());
  let anf = Middle_end.Transl.transl t in
  let src =
    if modular
    then Llvm_backend.Codegen_llvm.program_modular ~is_effect ?heap_words anf
    else Llvm_backend.Codegen_llvm.program ~is_effect ?heap_words anf
  in
  (* Quote every path (ADR-0072 §10 harness): a `TMPDIR` with spaces must not break the command line. *)
  let genll = Filename.concat dir "gen.ll" in
  let genexe = Filename.concat dir "gen" in
  let err = Filename.concat dir "err" in
  let out = Filename.concat dir "out" in
  let oc = open_out genll in
  output_string oc src;
  close_out oc;
  if
    Sys.command
      (Printf.sprintf
         "clang -Wl,-dead_strip %s %s -o %s 2>%s"
         (Filename.quote genll)
         (Filename.quote rt)
         (Filename.quote genexe)
         (Filename.quote err))
    <> 0
  then Alcotest.failf "clang failed; see %s and %s" err genll;
  if
    Sys.command (Printf.sprintf "%s > %s" (Filename.quote genexe) (Filename.quote out))
    <> 0
  then Alcotest.fail "generated program crashed";
  let ic = open_in out in
  let s = In_channel.input_all ic in
  close_in ic;
  s

(* Part B2 (ADR-0072 §1/§3): lower a program to **separate module objects + one init/entry object**,
   compile **each independently** to a `.o` (`clang -c`), and link them together with the runtime — the
   real per-module separate-compilation path. Exercises the cross-`.o` symbol ABI: each module's
   `@<mangle>$root` / `@<mangle>$init` are resolved by the system linker, so a non-injective or
   double-defined symbol would fail here rather than in one merged module. *)
(* Compile a program's per-module `.ll`s to independent `.o`s and dead-strip-link them with the runtime,
   returning the executable path (in [dir]). The link passes `-Wl,-dead_strip` (ld64) so unreferenced
   symbols — a pruned dead binding's code/globals — are removed (ADR-0072 §3 tree-shaking). *)
let build_split_exe ?(is_effect = false) ?heap_words ~(dir : string) (t : C.term) : string
  =
  let rt = Option.get rt_staticlib in
  (try Sys.mkdir dir 0o755 with
   | Sys_error _ -> ());
  let out =
    Llvm_backend.Codegen_llvm.program_split
      ~is_effect
      ?heap_words
      (Middle_end.Transl.transl t)
  in
  (* one `.ll` → `.o` (`clang -c`), quoting every path; return the `.o` path *)
  let compile_obj (tag : string) (src : string) : string =
    let ll = Filename.concat dir (tag ^ ".ll") in
    let obj = Filename.concat dir (tag ^ ".o") in
    let err = Filename.concat dir (tag ^ ".err") in
    let oc = open_out ll in
    output_string oc src;
    close_out oc;
    if
      Sys.command
        (Printf.sprintf
           "clang -c %s -o %s 2>%s"
           (Filename.quote ll)
           (Filename.quote obj)
           (Filename.quote err))
      <> 0
    then Alcotest.failf "clang -c failed; see %s and %s" err ll;
    obj
  in
  let objs =
    (* index-based object filenames (module names carry dots/specials); entry object last *)
    List.mapi (fun i (_, src) -> compile_obj (Printf.sprintf "mod_%d" i) src) out.modules
    @ [ compile_obj "entry" out.entry ]
  in
  let genexe = Filename.concat dir "gen" in
  let err = Filename.concat dir "link.err" in
  if
    Sys.command
      (Printf.sprintf
         "clang -Wl,-dead_strip %s %s -o %s 2>%s"
         (String.concat " " (List.map Filename.quote objs))
         (Filename.quote rt)
         (Filename.quote genexe)
         (Filename.quote err))
    <> 0
  then Alcotest.failf "link failed; see %s" err;
  genexe

let llvm_run_split ?(is_effect = false) ?heap_words (t : C.term) : string =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "purvasm_lls_%x" (Hashtbl.hash (t, heap_words)))
  in
  let genexe = build_split_exe ~is_effect ?heap_words ~dir t in
  let outf = Filename.concat dir "out" in
  if
    Sys.command (Printf.sprintf "%s > %s" (Filename.quote genexe) (Filename.quote outf))
    <> 0
  then Alcotest.fail "generated program crashed";
  let ic = open_in outf in
  let s = In_channel.input_all ic in
  close_in ic;
  s

(* Slice 1 (ADR-0072 §10): the pure first-order subset — its value must match the CESK oracle's
   printed form. (ADTs/records/case/effects/letrec are later slices, guarded in the codegen.) *)
let same_on_llvm ?heap_words (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (llvm_run ?heap_words t)

(* An `Effect` entry's observable is its stdout: the native binary performs the effects (`pv_run_effect`
   then drains the sink to `stdout`), matching the oracle's captured `run_effect` output (ADR-0072 §8). *)
let same_on_llvm_effect (label : string) (t : C.term) =
  let out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  Alcotest.(check string) label out (llvm_run ~is_effect:true t)

(* The **per-module** path (ADR-0072 §2/§3, Part B): the same term, but every top-level binding lowered to
   a root-handle global `@M.x$root` + an `@M.x$init`, run through a `pv_init_all`. B1 emits a single
   self-contained module, so this reuses the existing terms to differential-check the globals / init /
   root-handle machinery against the CESK oracle before B2's real multi-`.o` split. *)
let same_on_llvm_modular ?heap_words (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (llvm_run ~modular:true ?heap_words t)

let same_on_llvm_modular_effect (label : string) (t : C.term) =
  let out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  Alcotest.(check string) label out (llvm_run ~is_effect:true ~modular:true t)

(* The **separate-compilation** path (ADR-0072 §1/§3, Part B2): each module compiled to its own `.o`, all
   linked with the runtime. Same differential contract against the CESK oracle. *)
let same_on_llvm_split ?heap_words (label : string) (t : C.term) =
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (llvm_run_split ?heap_words t)

let same_on_llvm_split_effect (label : string) (t : C.term) =
  let out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  Alcotest.(check string) label out (llvm_run_split ~is_effect:true t)

let test_llvm_arith () =
  same_on_llvm
    "(1+2)*3"
    (C.Prim (C.MulInt, [ C.Prim (C.AddInt, [ int 1; int 2 ]); int 3 ]))

let test_llvm_app () =
  same_on_llvm
    "app"
    (C.App (C.Lam ("x", C.Prim (C.AddInt, [ C.Var "x"; int 1 ])), int 41))

let test_llvm_if () =
  same_on_llvm "if" (C.If (C.Prim (C.LtInt, [ int 1; int 2 ]), int 10, int 20))

(* `f` is live across `g`'s closure allocation (a safepoint): exercises root-on-create (ADR-0072 §6). *)
let test_llvm_let () =
  same_on_llvm
    "let"
    (C.Let
       ( "f"
       , C.Lam ("x", C.Prim (C.AddInt, [ C.Var "x"; int 1 ]))
       , C.Let
           ( "g"
           , C.Lam ("y", C.Prim (C.MulInt, [ C.Var "y"; int 2 ]))
           , C.Prim (C.AddInt, [ C.App (C.Var "f", int 10); C.App (C.Var "g", int 20) ])
           ) ))

(* `f`'s body `g x` is a tail call → the pv_tailcall trampoline path + a captured `g` (ADR-0071 §4). *)
let test_llvm_tailapp () =
  same_on_llvm
    "tailapp"
    (C.Let
       ( "g"
       , C.Lam ("y", C.Prim (C.AddInt, [ C.Var "y"; int 1 ]))
       , C.Let ("f", C.Lam ("x", C.App (C.Var "g", C.Var "x")), C.App (C.Var "f", int 41))
       ))

(* Slice 1 (ADR-0036): the pure first-order subset, uniform calling convention. A real
   purs-compiled fixture (`prelude_answer`) plus controlled terms covering each node
   kind. (Value-recursive `let rec` → `lazy`, record binders, foreigns, and Effect are
   later slices — see the ADR's named follow-ups.) *)
let test_ml_prelude_answer () = same_on_ocaml "answer" (prelude_term "answer")

let test_ml_arith () =
  same_on_ocaml
    "(1+2)*3"
    (C.Prim (C.MulInt, [ C.Prim (C.AddInt, [ int 1; int 2 ]); int 3 ]))

let test_ml_app () =
  same_on_ocaml
    "app"
    (C.App (C.Lam ("x", C.Prim (C.AddInt, [ C.Var "x"; int 1 ])), int 41))

let test_ml_if () =
  same_on_ocaml "if" (C.If (C.Prim (C.LtInt, [ int 1; int 2 ]), int 10, int 20))

let test_ml_ctor () = same_on_ocaml "ctor" (C.App (C.Ctor ("Just", 1), int 7))

let classify_term scrut =
  C.Case
    ( [ scrut ]
    , [ { C.binders = [ C.BCtor ("Just", [ C.BVar "x" ]) ]
        ; result = C.Unconditional (C.Prim (C.AddInt, [ C.Var "x"; int 1 ]))
        }
      ; { C.binders = [ C.BCtor ("Nothing", []) ]; result = C.Unconditional (int 0) }
      ] )

let test_ml_case_just () =
  same_on_ocaml "case Just 5" (classify_term (C.App (C.Ctor ("Just", 1), int 5)))

let test_ml_case_nothing () =
  same_on_ocaml "case Nothing" (classify_term (C.Ctor ("Nothing", 0)))

(* --- slice 2: ADT construction + case matching on the LLVM backend --------- *)

let test_llvm_case_just () =
  same_on_llvm "case Just 5" (classify_term (C.App (C.Ctor ("Just", 1), int 5)))

let test_llvm_case_nothing () =
  same_on_llvm "case Nothing" (classify_term (C.Ctor ("Nothing", 0)))

(* --- unsaturated constructors (ADR-0072 §5): a partially-applied ctor is a first-class function that
   accumulates the remaining fields, then builds the ADT. ------------------------------------------------ *)

(* A bare constructor used as a function (`0/1`): `let f = Just in case f 5 of Just x -> x+1` → 6. *)
let test_llvm_unsat_ctor_nullary () =
  same_on_llvm
    "unsat ctor 0/1"
    (C.Let ("f", C.Ctor ("Just", 1), classify_term (C.App (C.Var "f", int 5))))

(* A partially-applied field-carrying ctor (`1/2`) — a PAP that saturates on the next arg:
   `let g = Pair 3 in case g 4 of Pair a b -> a*10 + b` → 34. *)
let test_llvm_unsat_ctor_partial () =
  same_on_llvm
    "unsat ctor 1/2"
    (C.Let
       ( "g"
       , C.App (C.Ctor ("Pair", 2), int 3)
       , C.Case
           ( [ C.App (C.Var "g", int 4) ]
           , [ { C.binders = [ C.BCtor ("Pair", [ C.BVar "a"; C.BVar "b" ]) ]
               ; result =
                   C.Unconditional
                     (C.Prim
                        (C.AddInt, [ C.Prim (C.MulInt, [ C.Var "a"; int 10 ]); C.Var "b" ]))
               }
             ] ) ))

(* A bare constructor passed to a higher-order function (the `map Just` shape): `let ap = \h -> h 7 in
   case ap Just of Just x -> x+1` → 8. *)
let test_llvm_unsat_ctor_higher_order () =
  same_on_llvm
    "unsat ctor as arg"
    (C.Let
       ( "ap"
       , C.Lam ("h", C.App (C.Var "h", int 7))
       , classify_term (C.App (C.Var "ap", C.Ctor ("Just", 1))) ))

(* Through the separate-compilation path: the builder closure + its PAP survive per-`.o` linking. *)
let test_llvm_split_unsat_ctor () =
  same_on_llvm_split
    "split unsat ctor 0/1"
    (C.Let ("f", C.Ctor ("Just", 1), classify_term (C.App (C.Var "f", int 5))))

(* Nested constructor binder: `case Just (Just 3) of Just (Just y) -> y ; _ -> 0` → 3. *)
let test_llvm_case_nested () =
  same_on_llvm
    "nested ctor"
    (C.Case
       ( [ C.App (C.Ctor ("Just", 1), C.App (C.Ctor ("Just", 1), int 3)) ]
       , [ { C.binders = [ C.BCtor ("Just", [ C.BCtor ("Just", [ C.BVar "y" ]) ]) ]
           ; result = C.Unconditional (C.Var "y")
           }
         ; { C.binders = [ C.BNull ]; result = C.Unconditional (int 0) }
         ] ))

(* Guarded alternative with guard fall-through: `case 5 of x | x < 3 -> 100 | true -> 200` → 200. *)
let test_llvm_case_guard () =
  same_on_llvm
    "guarded"
    (C.Case
       ( [ int 5 ]
       , [ { C.binders = [ C.BVar "x" ]
           ; result =
               C.Guarded
                 [ C.Prim (C.LtInt, [ C.Var "x"; int 3 ]), int 100
                 ; C.Lit (C.LBool true), int 200
                 ]
           }
         ] ))

(* As-pattern: `case Just 5 of a@(Just x) -> case a of Just y -> x*10 + y` → 55 (binds the whole `a`). *)
let test_llvm_case_aspat () =
  same_on_llvm
    "as-pattern"
    (C.Case
       ( [ C.App (C.Ctor ("Just", 1), int 5) ]
       , [ { C.binders = [ C.BNamed ("a", C.BCtor ("Just", [ C.BVar "x" ])) ]
           ; result =
               C.Unconditional
                 (C.Case
                    ( [ C.Var "a" ]
                    , [ { C.binders = [ C.BCtor ("Just", [ C.BVar "y" ]) ]
                        ; result =
                            C.Unconditional
                              (C.Prim
                                 ( C.AddInt
                                 , [ C.Prim (C.MulInt, [ C.Var "x"; int 10 ]); C.Var "y" ]
                                 ))
                        }
                      ] ))
           }
         ] ))

(* Multi-field constructor: `case Pair 3 4 of Pair a b -> a*10 + b` → 34 (reads fields 0 and 1). *)
let test_llvm_case_multifield () =
  same_on_llvm
    "multi-field"
    (C.Case
       ( [ C.App (C.App (C.Ctor ("Pair", 2), int 3), int 4) ]
       , [ { C.binders = [ C.BCtor ("Pair", [ C.BVar "a"; C.BVar "b" ]) ]
           ; result =
               C.Unconditional
                 (C.Prim
                    (C.AddInt, [ C.Prim (C.MulInt, [ C.Var "a"; int 10 ]); C.Var "b" ]))
           }
         ] ))

(* Guard false falls through to the *next alternative* (not just the next guard):
   `case Just 5 of Just x | 10 < x -> 1 ; _ -> 2` → 2. *)
let test_llvm_case_guard_fallthrough () =
  same_on_llvm
    "guard-fallthrough"
    (C.Case
       ( [ C.App (C.Ctor ("Just", 1), int 5) ]
       , [ { C.binders = [ C.BCtor ("Just", [ C.BVar "x" ]) ]
           ; result = C.Guarded [ C.Prim (C.LtInt, [ int 10; C.Var "x" ]), int 1 ]
           }
         ; { C.binders = [ C.BNull ]; result = C.Unconditional (int 2) }
         ] ))

(* --- slice 3: static records (literal / accessor / update / record binder) ------------------- *)

(* `let r = { a: 3, b: 4 } in r.a + r.b` → 7 (construction + static accessor). *)
let test_llvm_record_access () =
  same_on_llvm
    "record access"
    (C.Let
       ( "r"
       , C.Record [ "a", int 3; "b", int 4 ]
       , C.Prim (C.AddInt, [ C.Accessor (C.Var "r", "a"); C.Accessor (C.Var "r", "b") ])
       ))

(* Functional update leaves the original intact:
   `let r = { a: 1, b: 2 } in (r { a = 10 }).a + r.a` → 11. *)
let test_llvm_record_update () =
  same_on_llvm
    "record update"
    (C.Let
       ( "r"
       , C.Record [ "a", int 1; "b", int 2 ]
       , C.Let
           ( "r2"
           , C.Update (C.Var "r", [ "a", int 10 ])
           , C.Prim
               (C.AddInt, [ C.Accessor (C.Var "r2", "a"); C.Accessor (C.Var "r", "a") ])
           ) ))

(* Record binder (row-poly subset): `case { x: 5, y: 7 } of { x: a, y: b } -> a*10 + b` → 57. *)
let test_llvm_record_binder () =
  same_on_llvm
    "record binder"
    (C.Case
       ( [ C.Record [ "x", int 5; "y", int 7 ] ]
       , [ { C.binders = [ C.BRecord [ "x", C.BVar "a"; "y", C.BVar "b" ] ]
           ; result =
               C.Unconditional
                 (C.Prim
                    (C.AddInt, [ C.Prim (C.MulInt, [ C.Var "a"; int 10 ]); C.Var "b" ]))
           }
         ] ))

(* --- slice 4: strings, foreign leaves, Effect (run_effect + stdout drain) ------------------- *)

(* `writeLine "hello"` as an `Effect Unit` → the native binary prints "hello\n", matching the oracle.
   Exercises a `String` literal (pv_new_str), a foreign leaf (pv_foreign → the two-level thunk), and the
   effect entry (pv_run_effect + pv_drain_output). *)
let test_llvm_effect_writeline () =
  same_on_llvm_effect
    "writeLine"
    (C.App (C.Foreign "Purvasm.Stdio.writeLineImpl", C.Lit (C.LString "hello")))

(* `Number` via a boxed literal + arithmetic, observed as an `Int`: `toInt (3.0 + 4.0)` → 7. *)
let test_llvm_number () =
  same_on_llvm
    "number"
    (C.Prim
       ( C.NumberToInt
       , [ C.Prim (C.AddNumber, [ C.Lit (C.LNumber 3.0); C.Lit (C.LNumber 4.0) ]) ] ))

(* `String` literal + `Append` + `EqString`, observed as an `Int`:
   `if ("ab" <> "cd") == "abcd" then 1 else 0` → 1. *)
let test_llvm_string () =
  same_on_llvm
    "string"
    (C.If
       ( C.Prim
           ( C.EqString
           , [ C.Prim (C.Append, [ C.Lit (C.LString "ab"); C.Lit (C.LString "cd") ])
             ; C.Lit (C.LString "abcd")
             ] )
       , int 1
       , int 0 ))

(* `Array` literal + index/length: `[10,20,30] !! 1` → 20 ; `length [10,20,30]` → 3. *)
let test_llvm_array_index () =
  same_on_llvm
    "array index"
    (C.Prim (C.IndexArray, [ C.Array [ int 10; int 20; int 30 ]; int 1 ]))

let test_llvm_array_length () =
  same_on_llvm
    "array length"
    (C.Prim (C.LengthArray, [ C.Array [ int 10; int 20; int 30 ] ]))

(* Dynamic (`String`-keyed) record access: `Record.get "a" { a: 5, b: 6 }` → 5. *)
let test_llvm_record_dynamic () =
  same_on_llvm
    "dynamic record get"
    (C.Prim (C.RecordGet, [ C.Lit (C.LString "a"); C.Record [ "a", int 5; "b", int 6 ] ]))

(* A foreign `show` leaf feeding a `writeLine`: `writeLine (showInt 42)` → "42\n". *)
let test_llvm_effect_show () =
  same_on_llvm_effect
    "writeLine (show 42)"
    (C.App
       ( C.Foreign "Purvasm.Stdio.writeLineImpl"
       , C.App (C.Foreign "Data.Show.showIntImpl", int 42) ))

(* In-place array mutation (the `Effect.Ref` core, structural over `NewArray`/`SetArray`/`IndexArray`):
   `(newArray 3){1 := 99}[1]` → 99. *)
let test_llvm_array_set () =
  same_on_llvm
    "array set"
    (C.Prim
       ( C.IndexArray
       , [ C.Prim (C.SetArray, [ C.Prim (C.NewArray, [ int 3 ]); int 1; int 99 ]); int 1 ]
       ))

(* Forced GC (ADR-0072 §10): a tiny heap makes throwaway allocations collect mid-run, so the emitted
   root-on-create rooting must keep a live pointer valid across a real relocating collection. The garbage
   comes from *sequential* calls to `g x = (allocate & discard a string); x` — each call's transient is
   rooted only for its own frame, so it dies on return and is reclaimable. The threaded `String` `keep`
   ("aabb") is `g`'s argument every call, so it must survive: `if keep == "aabb" then 1 else 0` → 1. *)
let test_llvm_forced_gc () =
  let g =
    C.Lam
      ( "x"
      , C.Let
          ( "_j"
          , C.Prim (C.Append, [ C.Lit (C.LString "zzzz"); C.Lit (C.LString "zzzz") ])
          , C.Var "x" ) )
  in
  let names = List.init 40 (fun i -> Printf.sprintf "a%d" (i + 1)) in
  let rec chain prev = function
    | [] ->
      C.If (C.Prim (C.EqString, [ C.Var prev; C.Lit (C.LString "aabb") ]), int 1, int 0)
    | name :: rest -> C.Let (name, C.App (C.Var "g", C.Var prev), chain name rest)
  in
  same_on_llvm
    ~heap_words:96
    "forced gc"
    (C.Let
       ( "g"
       , g
       , C.Let
           ( "keep"
           , C.Prim (C.Append, [ C.Lit (C.LString "aa"); C.Lit (C.LString "bb") ])
           , chain "keep" names ) ))

(* --- slice 5a: recursive groups (letrec → by-need Grec) ------------------------------------- *)

(* Self-recursion: `letrec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib 10` → 55.
   The member is a by-need cell; `apply` auto-forces it at each call (ADR-0070 §3). *)
let test_llvm_fib () =
  let fib =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.LtInt, [ C.Var "n"; int 2 ])
          , C.Var "n"
          , C.Prim
              ( C.AddInt
              , [ C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 1 ]))
                ; C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 2 ]))
                ] ) ) )
  in
  same_on_llvm "fib" (C.Letrec ([ "fib", fib ], C.App (C.Var "fib", int 10)))

(* Mutual recursion (two members referencing each other via the shared env):
   `letrec isEven n = n==0 ? 1 : isOdd (n-1) ; isOdd n = n==0 ? 0 : isEven (n-1) in isEven 10` → 1. *)
let test_llvm_mutual_rec () =
  let branch self other =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.EqInt, [ C.Var "n"; int 0 ])
          , self
          , C.App (C.Var other, C.Prim (C.SubInt, [ C.Var "n"; int 1 ])) ) )
  in
  same_on_llvm
    "mutual"
    (C.Letrec
       ( [ "isEven", branch (int 1) "isOdd"; "isOdd", branch (int 0) "isEven" ]
       , C.App (C.Var "isEven", int 10) ))

(* A recursive *value* member (a dictionary: a record whose method fields reference the dict itself,
   lazily via their closures — a strict self-*value* field would black-hole, ADR-0002). `dict` is a
   by-need cell; each `dict.field` projection forces it via `pv_force_if_byneed`, and the method's body
   re-projects the (memoised) dict. `letrec dict = { call: \x -> x + dict.base, base: 10 } in
   (dict.call) 5` → 15. *)
let test_llvm_recursive_value_dict () =
  same_on_llvm
    "recursive value dict"
    (C.Letrec
       ( [ ( "dict"
           , C.Record
               [ ( "call"
                 , C.Lam
                     ( "x"
                     , C.Prim (C.AddInt, [ C.Var "x"; C.Accessor (C.Var "dict", "base") ])
                     ) )
               ; "base", int 10
               ] )
         ]
       , C.App (C.Accessor (C.Var "dict", "call"), int 5) ))

(* A recursive closure whose captured value must survive a collection *during the recursion*: `go`
   allocates garbage each (tail-)call on a tiny heap, threading a captured `String` `keep`; the shared
   env keeps `keep` rooted across the forced GC. → `if go 40 == "aabb" then 1 else 0` → 1. *)
let test_llvm_forced_gc_recursive () =
  let go =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.EqInt, [ C.Var "n"; int 0 ])
          , C.Var "keep"
          , C.Let
              ( "_j"
              , C.Prim (C.Append, [ C.Lit (C.LString "zzzz"); C.Lit (C.LString "zzzz") ])
              , C.App (C.Var "go", C.Prim (C.SubInt, [ C.Var "n"; int 1 ])) ) ) )
  in
  same_on_llvm
    ~heap_words:96
    "forced gc recursive"
    (C.Let
       ( "keep"
       , C.Prim (C.Append, [ C.Lit (C.LString "aa"); C.Lit (C.LString "bb") ])
       , C.Letrec
           ( [ "go", go ]
           , C.If
               ( C.Prim
                   (C.EqString, [ C.App (C.Var "go", int 40); C.Lit (C.LString "aabb") ])
               , int 1
               , int 0 ) ) ))

(* A by-need cell reaching a Boolean demand site must be forced, not branched on by its pointer bits.
   `letrec b = false in if b then 1 else 0`: `b` is a `Grec` member (a `ByNeed` cell), so a raw `ashr`
   of the cell pointer (nonzero) would wrongly take the `then` branch → 1; forcing yields `false` → 0. *)
let test_llvm_letrec_if_byneed () =
  same_on_llvm
    "letrec if byneed"
    (C.Letrec ([ "b", C.Lit (C.LBool false) ], C.If (C.Var "b", int 1, int 0)))

(* The same demand site inside a `case` guard: `letrec ok = false in case 5 of x | ok -> 111 ; _ -> 222`.
   Unforced, the cell pointer reads as truthy → 111; forced, the guard fails → the wildcard alt → 222. *)
let test_llvm_letrec_guard_byneed () =
  same_on_llvm
    "letrec guard byneed"
    (C.Letrec
       ( [ "ok", C.Lit (C.LBool false) ]
       , C.Case
           ( [ int 5 ]
           , [ { C.binders = [ C.BVar "x" ]; result = C.Guarded [ C.Var "ok", int 111 ] }
             ; { C.binders = [ C.BVar "_w" ]; result = C.Unconditional (int 222) }
             ] ) ))

(* The program's final result is itself a by-need cell: `letrec x = 7 in x`. The entry must force it
   before `pv_print_int`, else the raw cell pointer is printed instead of 7. *)
let test_llvm_letrec_entry_byneed () =
  same_on_llvm "letrec entry byneed" (C.Letrec ([ "x", int 7 ], C.Var "x"))

(* Real compiled-PureScript multi-module programs through the whole-program path. *)
let test_llvm_prelude_answer () = same_on_llvm "prelude answer" (prelude_term "answer")

let test_llvm_prelude_quotient () =
  same_on_llvm "prelude quotient" (prelude_term "quotient")

(* A real `Effect` program (compiled PureScript with `Console.log`): its stdout must match the oracle. *)
let test_llvm_effect_console () =
  same_on_llvm_effect
    "effect_console"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/effect_console"
       ~entry_module:[ "ConsoleMain" ]
       ~entry:"main"
       ())

(* --- Part B1: the per-module lowering (root-handle globals + init + pv_init_all), differential-checked
   against the oracle on the same terms. --------------------------------------------------------------- *)

(* Two top-level keys that a non-injective mangle would collide (`A.B` and `A_B` both → `@A_B$root`),
   producing a duplicate global (a clang redefinition / cross-`.o` symbol clash). The injective mangle
   keeps them distinct: `let A.B = 3 in let A_B = 4 in A.B + A_B` → 7. *)
let test_llvm_mod_mangle_injective () =
  same_on_llvm_modular
    "mod mangle injective"
    (C.Let
       ( "A.B"
       , int 3
       , C.Let ("A_B", int 4, C.Prim (C.AddInt, [ C.Var "A.B"; C.Var "A_B" ])) ))

(* A degenerate no-binding program: `split_spine` yields no globals and an empty `pv_init_all`. *)
let test_llvm_mod_no_globals () =
  same_on_llvm_modular
    "mod no-globals"
    (C.Prim (C.MulInt, [ C.Prim (C.AddInt, [ int 1; int 2 ]); int 3 ]))

(* A top-level `Grec` global (a recursive function member): its cell is a root-handle global, forced at
   each self-call via `apply`. `letrec fib n = … in fib 10` → 55. *)
let test_llvm_mod_fib () =
  let fib =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.LtInt, [ C.Var "n"; int 2 ])
          , C.Var "n"
          , C.Prim
              ( C.AddInt
              , [ C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 1 ]))
                ; C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 2 ]))
                ] ) ) )
  in
  same_on_llvm_modular "mod fib" (C.Letrec ([ "fib", fib ], C.App (C.Var "fib", int 10)))

(* A top-level `Grec` value member (a dictionary) read across an init boundary: its global is a `ByNeed`
   cell forced at the projection. Exercises a value global + a function global in one group. *)
let test_llvm_mod_value_dict () =
  same_on_llvm_modular
    "mod value dict"
    (C.Letrec
       ( [ ( "dict"
           , C.Record
               [ ( "call"
                 , C.Lam
                     ( "x"
                     , C.Prim (C.AddInt, [ C.Var "x"; C.Accessor (C.Var "dict", "base") ])
                     ) )
               ; "base", int 10
               ] )
         ]
       , C.App (C.Accessor (C.Var "dict", "call"), int 5) ))

(* Real compiled-PureScript multi-module programs through the **per-module** path: every top-level binding
   of every linked module becomes a root-handle global, `pv_init_all` runs them in dependency order. The
   strongest B1 check — genuine `Gfun`/`Gcaf`/`Grec` bindings and cross-binding references. *)
let test_llvm_mod_prelude_answer () =
  same_on_llvm_modular "mod prelude answer" (prelude_term "answer")

let test_llvm_mod_prelude_quotient () =
  same_on_llvm_modular "mod prelude quotient" (prelude_term "quotient")

let test_llvm_mod_effect_console () =
  same_on_llvm_modular_effect
    "mod effect_console"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/effect_console"
       ~entry_module:[ "ConsoleMain" ]
       ~entry:"main"
       ())

(* --- Part B2: separate compilation — each module to its own `.o`, linked with the runtime. ----------- *)

(* One module object + the init/entry object: the entry reads `@fib$root` as an `external` global resolved
   at link. `letrec fib …` → 55. *)
let test_llvm_split_fib () =
  let fib =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.LtInt, [ C.Var "n"; int 2 ])
          , C.Var "n"
          , C.Prim
              ( C.AddInt
              , [ C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 1 ]))
                ; C.App (C.Var "fib", C.Prim (C.SubInt, [ C.Var "n"; int 2 ]))
                ] ) ) )
  in
  same_on_llvm_split "split fib" (C.Letrec ([ "fib", fib ], C.App (C.Var "fib", int 10)))

(* The injective-mangle collision term through separate compilation: `A.B` and `A_B` are two `external`
   globals the linker must keep distinct (a non-injective mangle → a duplicate-symbol link error). → 7. *)
let test_llvm_split_mangle_injective () =
  same_on_llvm_split
    "split mangle injective"
    (C.Let
       ( "A.B"
       , int 3
       , C.Let ("A_B", int 4, C.Prim (C.AddInt, [ C.Var "A.B"; C.Var "A_B" ])) ))

(* A **local variable shadowing a bare-key top-level global** (ADR-0072 §2/§4). `go` is a top-level
   binding (a bare key → a global); `f`'s body binds a *local* `go` and references it inside a nested
   lambda `\z -> go z`. The lambda-lift must **capture the local `go`** (not read `@go$root`): excluding it
   because `"go"` is a gkey would make `\z -> go z` call the arity-3 global, returning a partial
   application instead of a value. `f 10 5` = local `go 5` = `5 + 10` = 15 (the global `go` is never
   applied). Regressions here surfaced as a `Pap` reaching a leaf in real `where`-hoisted code. *)
let test_llvm_split_local_shadows_global () =
  same_on_llvm_split
    "split local shadows global"
    (C.Let
       ( "go"
       , C.Lam
           ( "a"
           , C.Lam
               ( "b"
               , C.Lam
                   ( "c"
                   , C.Prim
                       ( C.AddInt
                       , [ C.Prim (C.AddInt, [ C.Var "a"; C.Var "b" ]); C.Var "c" ] ) ) )
           )
       , C.Let
           ( "f"
           , C.Lam
               ( "x"
               , C.Let
                   ( "go"
                   , C.Lam ("y", C.Prim (C.AddInt, [ C.Var "y"; C.Var "x" ]))
                   , C.Lam ("z", C.App (C.Var "go", C.Var "z")) ) )
           , C.App (C.App (C.Var "f", int 10), int 5) ) ))

(* Real compiled-PureScript programs spanning **multiple modules**, each compiled to its own `.o` and
   linked: the genuine separate-compilation exercise (`Main` + `Data.*`/`Prelude` module objects + the
   init/entry object), differential-checked against the oracle. *)
let test_llvm_split_prelude_answer () =
  same_on_llvm_split "split prelude answer" (prelude_term "answer")

let test_llvm_split_prelude_quotient () =
  same_on_llvm_split "split prelude quotient" (prelude_term "quotient")

let test_llvm_split_effect_console () =
  same_on_llvm_split_effect
    "split effect_console"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/effect_console"
       ~entry_module:[ "ConsoleMain" ]
       ~entry:"main"
       ())

(* --- Reachability pruning + system-linker dead-strip (ADR-0072 §3). ---------------------------------- *)

let str_contains ~(needle : string) (hay : string) : bool =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* A binding unreachable from the entry (`dead`) is still emitted in its module object but **pruned from
   `pv_init_all`** (ADR-0072 §3). A pure dead binding is observationally invisible, so this proves the
   pruning *structurally*: the init/entry object initialises `live` but never `dead`. *)
let test_llvm_prune_omits_dead () =
  let t = C.Let ("dead", int 999, C.Let ("live", int 5, C.Var "live")) in
  let out = Llvm_backend.Codegen_llvm.program_split (Middle_end.Transl.transl t) in
  Alcotest.(check bool)
    "live is initialised"
    true
    (str_contains ~needle:"pv_g_live$init" out.entry);
  Alcotest.(check bool)
    "dead is pruned from pv_init_all"
    false
    (str_contains ~needle:"pv_g_dead$init" out.entry)

(* End-to-end correctness with a pruned dead binding present in a module object: `let dead = 999 in let
   live = 5 in live` → 5. The dead binding is init-pruned and dead-stripped; the program still matches the
   oracle (which eagerly evaluates the pure `dead` and discards it). *)
let test_llvm_split_dead_binding () =
  same_on_llvm_split
    "split dead binding"
    (C.Let ("dead", int 999, C.Let ("live", int 5, C.Var "live")))

(* The system linker's dead-strip actually removes the pruned binding's symbol from the binary (ADR-0072
   §3 tree-shaking): after `-Wl,-dead_strip`, `nm` shows no `pv_g_dead*`. Skipped if `nm` is unavailable. *)
let test_llvm_deadstrip_nm () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "purvasm_deadstrip_nm" in
  let t = C.Let ("dead", int 999, C.Let ("live", int 5, C.Var "live")) in
  let exe = build_split_exe ~dir t in
  let syms = Filename.concat dir "syms" in
  if
    Sys.command
      (Printf.sprintf "nm %s > %s 2>/dev/null" (Filename.quote exe) (Filename.quote syms))
    = 0
  then (
    let ic = open_in syms in
    let s = In_channel.input_all ic in
    close_in ic;
    (* `live` is initialised → its symbol survives; `dead` is pruned + dead-stripped → gone. *)
    Alcotest.(check bool) "live symbol kept" true (str_contains ~needle:"pv_g_live" s);
    Alcotest.(check bool)
      "dead symbol stripped"
      false
      (str_contains ~needle:"pv_g_dead" s))
  else Printf.printf "  (nm unavailable — skipping dead-strip symbol check)\n"

let test_ml_recursion () =
  let fact =
    C.Lam
      ( "n"
      , C.If
          ( C.Prim (C.LtInt, [ C.Var "n"; int 1 ])
          , int 1
          , C.Prim
              ( C.MulInt
              , [ C.Var "n"
                ; C.App (C.Var "fact", C.Prim (C.SubInt, [ C.Var "n"; int 1 ]))
                ] ) ) )
  in
  same_on_ocaml "fact 5" (C.Letrec ([ "fact", fact ], C.App (C.Var "fact", int 5)))

let test_ml_record () =
  same_on_ocaml "record.x" (C.Accessor (C.Record [ "x", int 5; "y", int 9 ], "x"))

let test_ml_array () = same_on_ocaml "array" (C.Array [ int 10; int 20; int 30 ])

(* Whole-module fixtures (no DCE) — the spine now includes record binders (`viaRecord`)
   and guards (`pick`), which the CPS cascade matcher handles. *)
let test_ml_anint () = same_on_ocaml "anInt" (Lower.module_ fixture ~entry:"anInt")

let test_ml_classify () =
  same_on_ocaml
    "classify (Just 0)"
    (C.App (Lower.module_ fixture ~entry:"classify", just (int 0)))

let test_ml_firstof () =
  same_on_ocaml
    "firstOf [10,20]"
    (C.App (Lower.module_ fixture ~entry:"firstOf", C.Array [ int 10; int 20 ]))

let test_ml_via_record () =
  same_on_ocaml
    "viaRecord {x:5}"
    (C.App (Lower.module_ fixture ~entry:"viaRecord", C.Record [ "x", int 5 ]))

let test_ml_pick () =
  same_on_ocaml
    "pick true"
    (C.App (Lower.module_ fixture ~entry:"pick", C.Lit (C.LBool true)))

(* number-literal binder (OCaml float patterns are disallowed; the cascade uses `=`) *)
let test_ml_number_binder () =
  same_on_ocaml
    "case 3.5"
    (C.Case
       ( [ C.Lit (C.LNumber 3.5) ]
       , [ { C.binders = [ C.BLit (C.LNumber 3.5) ]; result = C.Unconditional (int 1) }
         ; { C.binders = [ C.BNull ]; result = C.Unconditional (int 0) }
         ] ))

(* A recursive *value* binding (ADR-0024): `FibAnd.fibAnd` is the recursive `Fib` data
   value, so its `let rec` member lowers to OCaml `lazy` and uses force. *)
let test_ml_fib () =
  same_on_ocaml
    "fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

(* Foreign boundary (ADR-0036): the native leaf `Data.Show.showIntImpl` (pure) and
   `Effect.Console.log` (effectful), re-implemented in the Rt prelude over `value`. *)
let test_ml_show () = same_on_ocaml "shownStr" (prelude_term "shownStr")
let test_ml_doubled () = same_on_ocaml "doubled" (prelude_term "doubled")

let test_ml_effect_console () =
  same_on_ocaml_effect
    "console"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/effect_console"
       ~entry_module:[ "ConsoleMain" ]
       ~entry:"main"
       ())

let test_ml_effect_ref () =
  same_on_ocaml_effect
    "effect_ref"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/effect_ref"
       ~entry_module:[ "RefMain" ]
       ~entry:"main"
       ())

(* --- I4: loop combinator × effect body (ADR-0034) ------------------------- *)

(* I4 is the soundness invariant that an effect *body*'s arity must match the
   saturation convention of the loop combinator (`forE`/`foreachE`/…) driving it — a
   mismatch is a *silent wrong result* (purs-wasm's hardest GER spot). GER is deferred,
   but the combinators already run as inlined guest code through eval/apply across every
   backend, and the OCaml backend applies the body as a `VClos` value (the hybrid path),
   so this checks all three agree. *)
let foreign_ref k = Option.get (Ffi.resolver k)

let log_show_body var =
  C.Lam
    ( var
    , C.App
        ( C.Foreign "Purvasm.Stdio.writeLineImpl"
        , C.App (C.Foreign "Data.Show.showIntImpl", C.Var var) ) )

let i4_effect (label : string) (t : C.term) =
  let oracle =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  let vm = with_captured_stdout (fun () -> ignore (Vm.run_effect ~host:Ffi.host t)) in
  Alcotest.(check string) (label ^ ": oracle/vm") oracle vm;
  Alcotest.(check string) (label ^ ": oracle/ocaml") oracle (ocaml_run ~is_effect:true t)

(* forE lo hi (\i -> log (show i)) — the index-arg body, applied then forced each step. *)
let test_i4_fore () =
  i4_effect
    "forE"
    (C.App (C.App (C.App (foreign_ref "Effect.forE", int 0), int 4), log_show_body "i"))

(* foreachE [..] (\x -> log (show x)) — the element-arg body. *)
let test_i4_foreach () =
  i4_effect
    "foreachE"
    (C.App
       ( C.App (foreign_ref "Effect.foreachE", C.Array [ int 10; int 20; int 30 ])
       , log_show_body "x" ))

(* whileE/untilE terminate via a Ref condition (mutable state), so these exercise the
   loop combinators *together with* Ref + bindE sequencing across all backends. *)
let bindE m k = C.App (C.App (foreign_ref "Effect.bindE", m), k)
let pureE x = C.App (foreign_ref "Effect.pureE", x)
let new_ref v = C.App (foreign_ref "Effect.Ref._new", v)
let read_ref r = C.App (foreign_ref "Effect.Ref.read", r)
let write_ref v r = C.App (C.App (foreign_ref "Effect.Ref.write", v), r)

let log_show n =
  C.App
    (C.Foreign "Purvasm.Stdio.writeLineImpl", C.App (C.Foreign "Data.Show.showIntImpl", n))

(* count 0..4, printing each, while the Ref is < 5. *)
let test_i4_while () =
  let prog =
    bindE
      (new_ref (int 0))
      (C.Lam
         ( "ref"
         , C.App
             ( C.App
                 ( foreign_ref "Effect.whileE"
                 , bindE
                     (read_ref (C.Var "ref"))
                     (C.Lam ("v", pureE (C.Prim (C.LtInt, [ C.Var "v"; int 5 ])))) )
             , bindE
                 (read_ref (C.Var "ref"))
                 (C.Lam
                    ( "v"
                    , bindE
                        (log_show (C.Var "v"))
                        (C.Lam
                           ( "_"
                           , write_ref
                               (C.Prim (C.AddInt, [ C.Var "v"; int 1 ]))
                               (C.Var "ref") )) )) ) ))
  in
  i4_effect "whileE" prog

(* run the body until the Ref reaches 3, printing 0,1,2. *)
let test_i4_until () =
  let body =
    bindE
      (read_ref (C.Var "ref"))
      (C.Lam
         ( "v"
         , bindE
             (log_show (C.Var "v"))
             (C.Lam
                ( "_"
                , bindE
                    (write_ref (C.Prim (C.AddInt, [ C.Var "v"; int 1 ])) (C.Var "ref"))
                    (C.Lam
                       ( "_"
                       , pureE
                           (C.Prim
                              ( C.NotBool
                              , [ C.Prim
                                    ( C.LtInt
                                    , [ C.Prim (C.AddInt, [ C.Var "v"; int 1 ]); int 3 ]
                                    )
                                ] )) )) )) ))
  in
  let prog =
    bindE (new_ref (int 0)) (C.Lam ("ref", C.App (foreign_ref "Effect.untilE", body)))
  in
  i4_effect "untilE" prog

(* --- PURVASM bytecode VM (ADR-0030) differential equivalence -------------- *)

(* The VM is sound iff its result equals the oracle's on every pure program. The
   VM translates the term to ANF itself (`Vm.eval`), so this also re-exercises the
   ADR-0025 lowering. Compared by printed form (`Value.to_string`), which the VM
   reproduces byte-identically (ADR-0030). *)
let same_on_vm (label : string) (t : C.term) =
  let oracle = Cesk.Machine.eval ~host:Ffi.host t in
  let vm = Vm.eval ~host:Ffi.host t in
  Alcotest.(check string) label (V.to_string oracle) (Vm.Value.to_string vm)

let test_vm_fixture () = same_on_vm "fixture.anInt" (Lower.module_ fixture ~entry:"anInt")

(* Module-fixture entries applied to an argument, run on the VM vs the oracle. These
   reach the in-module `case`s directly: `classify`/`pick` are guarded (ADR-0013),
   `firstOf`/`viaRecord` exercise array / record binders (ADR-0012). *)
let same_on_vm_app (label : string) (entry : string) (arg : C.term) =
  same_on_vm label (C.App (Lower.module_ fixture ~entry, arg))

let test_vm_classify_just () =
  same_on_vm_app "classify (Just 0)" "classify" (just (int 0))

let test_vm_classify_nothing () = same_on_vm_app "classify Nothing" "classify" nothing
let test_vm_pick_true () = same_on_vm_app "pick true" "pick" (C.Lit (C.LBool true))
let test_vm_pick_false () = same_on_vm_app "pick false" "pick" (C.Lit (C.LBool false))

let test_vm_first_of () =
  same_on_vm_app "firstOf [10,20]" "firstOf" (C.Array [ int 10; int 20 ])

let test_vm_via_record () =
  same_on_vm_app "viaRecord {x:5}" "viaRecord" (C.Record [ "x", int 5 ])

(* Hand-built `case` terms that exercise the decision-tree compiler (ADR-0031)
   directly — literal switches, nested constructors, as-patterns, shared prefixes,
   default edges, array-length switches — each checked against the oracle. *)
let alt bs body = { C.binders = bs; result = C.Unconditional body }
let case scruts alts = C.Case (scruts, alts)
let cons h t = C.App (C.App (C.Ctor ("Cons", 2), h), t)
let nil = C.Ctor ("Nil", 0)

(* Literal switch with a default edge. *)
let test_vm_dt_lit () =
  let prog n =
    case [ int n ] [ alt [ C.BLit (C.LInt 0) ] (int 10); alt [ C.BNull ] (int 20) ]
  in
  same_on_vm "dt lit 0" (prog 0);
  same_on_vm "dt lit 7" (prog 7)

(* Nested constructors, exhaustive (no default). *)
let test_vm_dt_nested () =
  let prog s =
    case
      [ s ]
      [ alt [ C.BCtor ("Just", [ C.BCtor ("Just", [ C.BVar "y" ]) ]) ] (C.Var "y")
      ; alt [ C.BCtor ("Just", [ C.BCtor ("Nothing", []) ]) ] (int (-1))
      ; alt [ C.BCtor ("Nothing", []) ] (int (-2))
      ]
  in
  same_on_vm "dt nested Just(Just 5)" (prog (just (just (int 5))));
  same_on_vm "dt nested Just Nothing" (prog (just nothing));
  same_on_vm "dt nested Nothing" (prog nothing)

(* As-pattern binds the whole occurrence; constructor default edge. *)
let test_vm_dt_aspat () =
  let prog s =
    case
      [ s ]
      [ alt [ C.BNamed ("x", C.BCtor ("Just", [ C.BNull ])) ] (C.Var "x")
      ; alt [ C.BNull ] (int 99)
      ]
  in
  same_on_vm "dt as Just 3" (prog (just (int 3)));
  same_on_vm "dt as Nothing(default)" (prog nothing)

(* Shared `Cons` prefix with a literal nested inside it, plus a default within the
   specialised sub-matrix and an exhaustive outer Nil. *)
let test_vm_dt_shared () =
  let prog s =
    case
      [ s ]
      [ alt [ C.BCtor ("Cons", [ C.BLit (C.LInt 1); C.BNull ]) ] (int 100)
      ; alt [ C.BCtor ("Cons", [ C.BVar "x"; C.BNull ]) ] (C.Var "x")
      ; alt [ C.BCtor ("Nil", []) ] (int 0)
      ]
  in
  same_on_vm "dt shared Cons 1" (prog (cons (int 1) nil));
  same_on_vm "dt shared Cons 2" (prog (cons (int 2) nil));
  same_on_vm "dt shared Nil" (prog nil)

(* Array-length switch with multiple heads and a default. *)
let test_vm_dt_array () =
  let prog xs =
    case
      [ C.Array xs ]
      [ alt [ C.BArray [ C.BVar "a" ] ] (C.Var "a")
      ; alt
          [ C.BArray [ C.BVar "a"; C.BVar "b" ] ]
          (C.Prim (C.AddInt, [ C.Var "a"; C.Var "b" ]))
      ; alt [ C.BNull ] (int 0)
      ]
  in
  same_on_vm "dt array [5]" (prog [ int 5 ]);
  same_on_vm "dt array [2,3]" (prog [ int 2; int 3 ]);
  same_on_vm "dt array [1,2,3](default)" (prog [ int 1; int 2; int 3 ])

(* Guarded alternatives folded into the decision tree (ADR-0031 fast-follow): guard
   chains evaluated at the leaf, all-false falling through to the rows below. *)
let altg bs clauses = { C.binders = bs; result = C.Guarded clauses }
let gt n var = C.Prim (C.LtInt, [ int n; C.Var var ]) (* var > n *)

(* Multi-guard chain in one alternative, then fall-through to a later alternative. *)
let test_vm_dt_guard_chain () =
  let prog n =
    case
      [ int n ]
      [ altg [ C.BVar "x" ] [ gt 10 "x", int 1; gt 5 "x", int 2 ]
      ; alt [ C.BNull ] (int 3)
      ]
  in
  same_on_vm "guard chain 20" (prog 20);
  same_on_vm "guard chain 7" (prog 7);
  same_on_vm "guard chain 3" (prog 3)

(* A guarded row inside a constructor branch whose guard fails falls through to the
   next row of the same branch (the tree's [compile occs rest] under a switch). *)
let test_vm_dt_guard_ctor () =
  let prog s =
    case
      [ s ]
      [ altg [ C.BCtor ("Just", [ C.BVar "x" ]) ] [ gt 0 "x", C.Var "x" ]
      ; alt [ C.BCtor ("Just", [ C.BNull ]) ] (int (-1))
      ; alt [ C.BCtor ("Nothing", []) ] (int (-2))
      ]
  in
  same_on_vm "guard ctor Just 5" (prog (just (int 5)));
  same_on_vm "guard ctor Just -3" (prog (just (int (-3))));
  same_on_vm "guard ctor Nothing" (prog nothing)

(* The naive matcher also handles guards; check it differentially against the oracle. *)
let same_on_vm_naive (label : string) (t : C.term) =
  let oracle = Cesk.Machine.eval ~host:Ffi.host t in
  let v, _ = Vm.eval_anf_counted ~naive:true (Middle_end.Transl.transl t) in
  Alcotest.(check string) label (V.to_string oracle) (Vm.Value.to_string v)

let test_vm_naive_guard () =
  let prog n =
    case [ int n ] [ altg [ C.BVar "x" ] [ gt 10 "x", int 1 ]; alt [ C.BNull ] (int 0) ]
  in
  same_on_vm_naive "naive guard 20" (prog 20);
  same_on_vm_naive "naive guard 5" (prog 5)

(* Native foreign rung (ADR-0032): the pure `Data.Show.*` leaves resolve through the
   host registry on the VM, exactly as on the oracle. *)
let test_vm_show_int () = same_on_vm "shownInt" (prelude_term "shownInt")
let test_vm_show_number () = same_on_vm "shownNum" (prelude_term "shownNum")
let test_vm_show_char () = same_on_vm "shownChar" (prelude_term "shownChar")
let test_vm_show_string () = same_on_vm "shownStr" (prelude_term "shownStr")

(* Effect (ADR-0032): run `main` on the VM via the same thunk-forcing driver as the
   oracle. [Effect.Ref] is observed as a returned value; [Effect.Console.log] as the
   stdout it writes — the latter asserts effect *order* (the soundness contract). *)
let effect_term ~(outdir : string) ~(entry_module : string list) : C.term =
  Link.link_program ~resolver:Ffi.resolver ~outdir ~entry_module ~entry:"main" ()

let test_vm_effect_ref () =
  let t = effect_term ~outdir:"../fixtures/effect_ref" ~entry_module:[ "RefMain" ] in
  Alcotest.(check string)
    "effect ref"
    (V.to_string (Cesk.Machine.run_effect ~host:Ffi.host t))
    (Vm.Value.to_string (Vm.run_effect ~host:Ffi.host t))

let test_vm_effect_console () =
  let t =
    effect_term ~outdir:"../fixtures/effect_console" ~entry_module:[ "ConsoleMain" ]
  in
  let oracle_out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  let vm_out = with_captured_stdout (fun () -> ignore (Vm.run_effect ~host:Ffi.host t)) in
  Alcotest.(check string) "console stdout (order)" oracle_out vm_out

(* --- separate-compilation image (ADR-0033): serialize -> deserialize -> run --- *)

(* A program survives the round-trip through a serialized image iff running the
   reloaded image equals the oracle. Compiles the term to an image, serializes it to
   JSON text and back (exercising the on-disk format), then runs it. *)
let same_on_image (label : string) (t : C.term) =
  let img = Pvm.Image.of_string (Pvm.Image.to_string (Pvm.Image.of_term t)) in
  Alcotest.(check string)
    label
    (V.to_string (Cesk.Machine.eval ~host:Ffi.host t))
    (Vm.Value.to_string (Pvm.Image.run ~host:Ffi.host img))

(* A non-round double must survive the text image bit-for-bit (IEEE-bits encoding),
   checked exactly rather than via the cross-printer string form. *)
let test_image_float_bits () =
  let f = 1.0 /. 3.0 in
  let img =
    Pvm.Image.of_string (Pvm.Image.to_string (Pvm.Image.of_term (C.Lit (C.LNumber f))))
  in
  match Pvm.Image.run ~host:Ffi.host img with
  | Vm.Value.Vnumber g ->
    Alcotest.(check bool)
      "float bits round-trip"
      true
      (Int64.equal (Int64.bits_of_float g) (Int64.bits_of_float f))
  | _ -> Alcotest.fail "expected Vnumber"

let test_image_answer () = same_on_image "image.answer" (prelude_term "answer")
let test_image_doubled () = same_on_image "image.doubled" (prelude_term "doubled")
let test_image_show () = same_on_image "image.shownStr" (prelude_term "shownStr")

let test_image_fib () =
  same_on_image
    "image.fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

(* Effect through an image: the entry is forced by applying `main` to unit (as
   `run_effect` does), and the side effects must occur in order on the reloaded
   image. *)
let test_image_effect_console () =
  let t =
    effect_term ~outdir:"../fixtures/effect_console" ~entry_module:[ "ConsoleMain" ]
  in
  let img =
    Pvm.Image.of_string
      (Pvm.Image.to_string (Pvm.Image.of_term (C.App (t, C.Lit (C.LInt 0)))))
  in
  let oracle_out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  let img_out =
    with_captured_stdout (fun () -> ignore (Pvm.Image.run ~host:Ffi.host img))
  in
  Alcotest.(check string) "image console stdout (order)" oracle_out img_out

(* --- separate compilation (ADR-0033): per-module artifacts -> link -> run ----- *)

(* Compile each module of a program to its own `.pvmo`, round-trip every artifact
   through the on-disk format, link the reachable definitions for the entry, and run
   the resulting image. This exercises true separate compilation (one module at a
   time) and cross-module linking — the heart of ADR-0033. *)
let sepcomp_image ~outdir ~entry_module ~entry ?arg () : Pvm.Image.t =
  let artifacts =
    Link.load ~outdir ~entry_module ()
    |> List.map (fun m ->
      Pvm.Artifact.module_of_string
        (Pvm.Artifact.module_to_string (Pvm.Compile.compile_module m)))
  in
  let entry_key = Lower.qualified_key entry_module entry in
  let main_term =
    match arg with
    | None -> C.Var entry_key
    | Some a -> C.App (C.Var entry_key, a)
  in
  Pvm.Plink.link artifacts ~resolver:Ffi.resolver ~main_term

let oracle_value ~outdir ~entry_module ~entry ?arg () : V.t =
  let base = Link.link_program ~resolver:Ffi.resolver ~outdir ~entry_module ~entry () in
  let t =
    match arg with
    | None -> base
    | Some a -> C.App (base, a)
  in
  Cesk.Machine.eval ~host:Ffi.host t

let same_on_sepcomp label ~outdir ~entry_module ~entry ?arg () =
  Alcotest.(check string)
    label
    (V.to_string (oracle_value ~outdir ~entry_module ~entry ?arg ()))
    (Vm.Value.to_string
       (Pvm.Image.run ~host:Ffi.host (sepcomp_image ~outdir ~entry_module ~entry ?arg ())))

let test_sc_answer () =
  same_on_sepcomp
    "sc.answer"
    ~outdir:"../fixtures/prelude"
    ~entry_module:[ "Main" ]
    ~entry:"answer"
    ()

let test_sc_doubled () =
  same_on_sepcomp
    "sc.doubled"
    ~outdir:"../fixtures/prelude"
    ~entry_module:[ "Main" ]
    ~entry:"doubled"
    ()

let test_sc_show () =
  same_on_sepcomp
    "sc.shownStr"
    ~outdir:"../fixtures/prelude"
    ~entry_module:[ "Main" ]
    ~entry:"shownStr"
    ()

let test_sc_fib () =
  same_on_sepcomp
    "sc.fib 10"
    ~outdir:"../fixtures/fib"
    ~entry_module:[ "FibAnd" ]
    ~entry:"fib"
    ~arg:(int 10)
    ()

(* Cross-module linking: the entry's value is defined in terms of other modules'
   exports, so the image must merge several `.pvmo`s. *)
let test_sc_transitive () =
  same_on_sepcomp
    "sc.trans.a"
    ~outdir:"../fixtures/trans"
    ~entry_module:[ "TransA" ]
    ~entry:"a"
    ()

let test_sc_diamond () =
  same_on_sepcomp
    "sc.diamond.both"
    ~outdir:"../fixtures/diamond"
    ~entry_module:[ "DiaA" ]
    ~entry:"both"
    ()

(* The `.pvmi` interface carries each public export's kind/arity and a hash over that
   surface, and survives serialization (ADR-0033). *)
let test_sc_interface () =
  let modules = Link.load ~outdir:"../fixtures/fib" ~entry_module:[ "FibAnd" ] () in
  let m =
    List.find
      (fun (mm : Corefn.Module.t) -> String.equal (Link.name_key mm.name) "FibAnd")
      modules
  in
  let i = Pvm.Artifact.interface_of (Pvm.Compile.compile_module m) in
  let i2 = Pvm.Artifact.interface_of_string (Pvm.Artifact.interface_to_string i) in
  Alcotest.(check string)
    "interface hash stable through serde"
    i.Pvm.Artifact.hash
    i2.Pvm.Artifact.hash;
  match List.assoc_opt "FibAnd.fib" i.Pvm.Artifact.exports with
  | Some (Pvm.Artifact.Efn 1) -> ()
  | _ -> Alcotest.fail "FibAnd.fib should be exported as a fn/1"

let test_sc_effect_console () =
  let t =
    effect_term ~outdir:"../fixtures/effect_console" ~entry_module:[ "ConsoleMain" ]
  in
  let img =
    sepcomp_image
      ~outdir:"../fixtures/effect_console"
      ~entry_module:[ "ConsoleMain" ]
      ~entry:"main"
      ~arg:(int 0)
      ()
  in
  let oracle_out =
    with_captured_stdout (fun () -> ignore (Cesk.Machine.run_effect ~host:Ffi.host t))
  in
  let img_out =
    with_captured_stdout (fun () -> ignore (Pvm.Image.run ~host:Ffi.host img))
  in
  Alcotest.(check string) "sc console stdout (order)" oracle_out img_out

let test_vm_app () = same_on_vm "app.result" (program "result")
let test_vm_prelude_answer () = same_on_vm "prelude.answer" (prelude_term "answer")
let test_vm_prelude_div () = same_on_vm "prelude.quotient" (prelude_term "quotient")
let test_vm_prelude_eq () = same_on_vm "prelude.isEq" (prelude_term "isEq")
let test_vm_prelude_bool () = same_on_vm "prelude.both" (prelude_term "both")
let test_vm_prelude_map () = same_on_vm "prelude.doubled" (prelude_term "doubled")

let test_vm_newtype () =
  same_on_vm
    "newtype.asInner 7"
    (C.App
       ( Link.link_program
           ~outdir:"../fixtures/newtype"
           ~entry_module:[ "Main" ]
           ~entry:"asInner"
           ()
       , int 7 ))

let test_vm_fib () =
  same_on_vm
    "fib 10"
    (C.App
       ( Link.link_program
           ~resolver:Ffi.resolver
           ~outdir:"../fixtures/fib"
           ~entry_module:[ "FibAnd" ]
           ~entry:"fib"
           ()
       , int 10 ))

let test_vm_lazy_fix () =
  same_on_vm
    "sumTo 5 via fix"
    (Link.link_program
       ~resolver:Ffi.resolver
       ~outdir:"../fixtures/lazy"
       ~entry_module:[ "Main" ]
       ~entry:"result"
       ())

let test_vm_transitive () =
  same_on_vm
    "trans.a"
    (Link.link_program
       ~outdir:"../fixtures/trans"
       ~entry_module:[ "TransA" ]
       ~entry:"a"
       ())

let test_vm_diamond () =
  same_on_vm
    "diamond.both"
    (Link.link_program
       ~outdir:"../fixtures/diamond"
       ~entry_module:[ "DiaA" ]
       ~entry:"both"
       ())

(* The llvm-backend differential runs only when the owned runtime `staticlib` + `clang` are available
   (ADR-0072 §10); otherwise it is skipped with a notice, so `dune test` never fails merely because the
   Rust runtime has not been built in this environment. *)
let llvm_groups =
  if llvm_available
  then
    [ ( "llvm-backend"
      , [ Alcotest.test_case "arith" `Quick test_llvm_arith
        ; Alcotest.test_case "app" `Quick test_llvm_app
        ; Alcotest.test_case "if" `Quick test_llvm_if
        ; Alcotest.test_case "let" `Quick test_llvm_let
        ; Alcotest.test_case "tailapp" `Quick test_llvm_tailapp
        ; Alcotest.test_case "case_just" `Quick test_llvm_case_just
        ; Alcotest.test_case "case_nothing" `Quick test_llvm_case_nothing
        ; Alcotest.test_case "unsat_ctor_nullary" `Quick test_llvm_unsat_ctor_nullary
        ; Alcotest.test_case "unsat_ctor_partial" `Quick test_llvm_unsat_ctor_partial
        ; Alcotest.test_case
            "unsat_ctor_higher_order"
            `Quick
            test_llvm_unsat_ctor_higher_order
        ; Alcotest.test_case "split_unsat_ctor" `Quick test_llvm_split_unsat_ctor
        ; Alcotest.test_case "case_nested" `Quick test_llvm_case_nested
        ; Alcotest.test_case "case_guard" `Quick test_llvm_case_guard
        ; Alcotest.test_case "case_aspat" `Quick test_llvm_case_aspat
        ; Alcotest.test_case "case_multifield" `Quick test_llvm_case_multifield
        ; Alcotest.test_case
            "case_guard_fallthrough"
            `Quick
            test_llvm_case_guard_fallthrough
        ; Alcotest.test_case "record_access" `Quick test_llvm_record_access
        ; Alcotest.test_case "record_update" `Quick test_llvm_record_update
        ; Alcotest.test_case "record_binder" `Quick test_llvm_record_binder
        ; Alcotest.test_case "effect_writeline" `Quick test_llvm_effect_writeline
        ; Alcotest.test_case "number" `Quick test_llvm_number
        ; Alcotest.test_case "string" `Quick test_llvm_string
        ; Alcotest.test_case "array_index" `Quick test_llvm_array_index
        ; Alcotest.test_case "array_length" `Quick test_llvm_array_length
        ; Alcotest.test_case "record_dynamic" `Quick test_llvm_record_dynamic
        ; Alcotest.test_case "effect_show" `Quick test_llvm_effect_show
        ; Alcotest.test_case "array_set" `Quick test_llvm_array_set
        ; Alcotest.test_case "forced_gc" `Quick test_llvm_forced_gc
        ; Alcotest.test_case "fib" `Quick test_llvm_fib
        ; Alcotest.test_case "mutual_rec" `Quick test_llvm_mutual_rec
        ; Alcotest.test_case "recursive_value_dict" `Quick test_llvm_recursive_value_dict
        ; Alcotest.test_case "forced_gc_recursive" `Quick test_llvm_forced_gc_recursive
        ; Alcotest.test_case "letrec_if_byneed" `Quick test_llvm_letrec_if_byneed
        ; Alcotest.test_case "letrec_guard_byneed" `Quick test_llvm_letrec_guard_byneed
        ; Alcotest.test_case "letrec_entry_byneed" `Quick test_llvm_letrec_entry_byneed
        ; Alcotest.test_case "prelude_answer" `Quick test_llvm_prelude_answer
        ; Alcotest.test_case "prelude_quotient" `Quick test_llvm_prelude_quotient
        ; Alcotest.test_case "effect_console" `Quick test_llvm_effect_console
        ; Alcotest.test_case "mod_mangle_injective" `Quick test_llvm_mod_mangle_injective
        ; Alcotest.test_case "mod_no_globals" `Quick test_llvm_mod_no_globals
        ; Alcotest.test_case "mod_fib" `Quick test_llvm_mod_fib
        ; Alcotest.test_case "mod_value_dict" `Quick test_llvm_mod_value_dict
        ; Alcotest.test_case "mod_prelude_answer" `Quick test_llvm_mod_prelude_answer
        ; Alcotest.test_case "mod_prelude_quotient" `Quick test_llvm_mod_prelude_quotient
        ; Alcotest.test_case "mod_effect_console" `Quick test_llvm_mod_effect_console
        ; Alcotest.test_case "split_fib" `Quick test_llvm_split_fib
        ; Alcotest.test_case
            "split_mangle_injective"
            `Quick
            test_llvm_split_mangle_injective
        ; Alcotest.test_case
            "split_local_shadows_global"
            `Quick
            test_llvm_split_local_shadows_global
        ; Alcotest.test_case "split_prelude_answer" `Quick test_llvm_split_prelude_answer
        ; Alcotest.test_case
            "split_prelude_quotient"
            `Quick
            test_llvm_split_prelude_quotient
        ; Alcotest.test_case "split_effect_console" `Quick test_llvm_split_effect_console
        ; Alcotest.test_case "prune_omits_dead" `Quick test_llvm_prune_omits_dead
        ; Alcotest.test_case "split_dead_binding" `Quick test_llvm_split_dead_binding
        ; Alcotest.test_case "deadstrip_nm" `Quick test_llvm_deadstrip_nm
        ] )
    ]
  else (
    Printf.eprintf
      "[e2e] skipping llvm-backend group: %s\n%!"
      (if rt_staticlib = None
       then "runtime staticlib not found (build `runtime/` or set PURVASM_RT_A)"
       else "clang not found");
    [])

let () =
  Alcotest.run
    "e2e"
    ([ ( "literals"
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
         ; Alcotest.test_case "fires" `Quick test_dictelim_fires
         ] )
     ; ( "opt"
       , [ Alcotest.test_case "prelude_answer" `Quick test_opt_prelude_answer
         ; Alcotest.test_case "prelude_doubled" `Quick test_opt_prelude_doubled
         ; Alcotest.test_case "prelude_show" `Quick test_opt_prelude_show
         ; Alcotest.test_case "app" `Quick test_opt_app
         ; Alcotest.test_case "newtype" `Quick test_opt_newtype
         ; Alcotest.test_case "fib" `Quick test_opt_fib
         ; Alcotest.test_case "effect_ref" `Quick test_opt_effect_ref
         ; Alcotest.test_case "simplify_fires" `Quick test_simplify_fires
         ; Alcotest.test_case "dbe_fires_classify" `Quick test_dbe_fires_classify
         ; Alcotest.test_case "dbe_fires_anint" `Quick test_dbe_fires_anint
         ] )
     ; ( "ocaml"
       , [ Alcotest.test_case "prelude_answer" `Quick test_ml_prelude_answer
         ; Alcotest.test_case "arith" `Quick test_ml_arith
         ; Alcotest.test_case "app" `Quick test_ml_app
         ; Alcotest.test_case "if" `Quick test_ml_if
         ; Alcotest.test_case "ctor" `Quick test_ml_ctor
         ; Alcotest.test_case "case_just" `Quick test_ml_case_just
         ; Alcotest.test_case "case_nothing" `Quick test_ml_case_nothing
         ; Alcotest.test_case "recursion" `Quick test_ml_recursion
         ; Alcotest.test_case "record" `Quick test_ml_record
         ; Alcotest.test_case "array" `Quick test_ml_array
         ; Alcotest.test_case "anInt" `Quick test_ml_anint
         ; Alcotest.test_case "classify" `Quick test_ml_classify
         ; Alcotest.test_case "firstOf" `Quick test_ml_firstof
         ; Alcotest.test_case "via_record" `Quick test_ml_via_record
         ; Alcotest.test_case "pick" `Quick test_ml_pick
         ; Alcotest.test_case "number_binder" `Quick test_ml_number_binder
         ; Alcotest.test_case "fib" `Quick test_ml_fib
         ; Alcotest.test_case "show" `Quick test_ml_show
         ; Alcotest.test_case "doubled" `Quick test_ml_doubled
         ; Alcotest.test_case "effect_console" `Quick test_ml_effect_console
         ; Alcotest.test_case "effect_ref" `Quick test_ml_effect_ref
         ] )
     ; ( "i4"
       , [ Alcotest.test_case "forE" `Quick test_i4_fore
         ; Alcotest.test_case "foreachE" `Quick test_i4_foreach
         ; Alcotest.test_case "whileE" `Quick test_i4_while
         ; Alcotest.test_case "untilE" `Quick test_i4_until
         ] )
     ; ( "vm"
       , [ Alcotest.test_case "fixture" `Quick test_vm_fixture
         ; Alcotest.test_case "app" `Quick test_vm_app
         ; Alcotest.test_case "prelude_answer" `Quick test_vm_prelude_answer
         ; Alcotest.test_case "prelude_div" `Quick test_vm_prelude_div
         ; Alcotest.test_case "prelude_eq" `Quick test_vm_prelude_eq
         ; Alcotest.test_case "prelude_bool" `Quick test_vm_prelude_bool
         ; Alcotest.test_case "prelude_map" `Quick test_vm_prelude_map
         ; Alcotest.test_case "newtype" `Quick test_vm_newtype
         ; Alcotest.test_case "fib" `Quick test_vm_fib
         ; Alcotest.test_case "lazy_fix" `Quick test_vm_lazy_fix
         ; Alcotest.test_case "transitive" `Quick test_vm_transitive
         ; Alcotest.test_case "diamond" `Quick test_vm_diamond
         ; Alcotest.test_case "classify_just" `Quick test_vm_classify_just
         ; Alcotest.test_case "classify_nothing" `Quick test_vm_classify_nothing
         ; Alcotest.test_case "pick_true" `Quick test_vm_pick_true
         ; Alcotest.test_case "pick_false" `Quick test_vm_pick_false
         ; Alcotest.test_case "first_of" `Quick test_vm_first_of
         ; Alcotest.test_case "via_record" `Quick test_vm_via_record
         ; Alcotest.test_case "dt_lit" `Quick test_vm_dt_lit
         ; Alcotest.test_case "dt_nested" `Quick test_vm_dt_nested
         ; Alcotest.test_case "dt_aspat" `Quick test_vm_dt_aspat
         ; Alcotest.test_case "dt_shared" `Quick test_vm_dt_shared
         ; Alcotest.test_case "dt_array" `Quick test_vm_dt_array
         ; Alcotest.test_case "dt_guard_chain" `Quick test_vm_dt_guard_chain
         ; Alcotest.test_case "dt_guard_ctor" `Quick test_vm_dt_guard_ctor
         ; Alcotest.test_case "naive_guard" `Quick test_vm_naive_guard
         ; Alcotest.test_case "show_int" `Quick test_vm_show_int
         ; Alcotest.test_case "show_number" `Quick test_vm_show_number
         ; Alcotest.test_case "show_char" `Quick test_vm_show_char
         ; Alcotest.test_case "show_string" `Quick test_vm_show_string
         ; Alcotest.test_case "effect_ref" `Quick test_vm_effect_ref
         ; Alcotest.test_case "effect_console" `Quick test_vm_effect_console
         ] )
     ; ( "image"
       , [ Alcotest.test_case "float_bits" `Quick test_image_float_bits
         ; Alcotest.test_case "answer" `Quick test_image_answer
         ; Alcotest.test_case "doubled" `Quick test_image_doubled
         ; Alcotest.test_case "show" `Quick test_image_show
         ; Alcotest.test_case "fib" `Quick test_image_fib
         ; Alcotest.test_case "effect_console" `Quick test_image_effect_console
         ] )
     ; ( "sepcomp"
       , [ Alcotest.test_case "answer" `Quick test_sc_answer
         ; Alcotest.test_case "doubled" `Quick test_sc_doubled
         ; Alcotest.test_case "show" `Quick test_sc_show
         ; Alcotest.test_case "fib" `Quick test_sc_fib
         ; Alcotest.test_case "transitive" `Quick test_sc_transitive
         ; Alcotest.test_case "diamond" `Quick test_sc_diamond
         ; Alcotest.test_case "interface" `Quick test_sc_interface
         ; Alcotest.test_case "effect_console" `Quick test_sc_effect_console
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
     @ llvm_groups)
