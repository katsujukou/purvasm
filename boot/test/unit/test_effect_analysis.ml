(* Unit tests for the structural effect analysis (ADR-0034). The analysis must
   honour the force/saturation model — chiefly I1, *construction ≠ execution*:
   building an [Effect] (even applying [log] to its string) is pure; the effect fires
   only when the built thunk is forced. The dead-binding predicate [eperf] is what a
   sound DBE consults. *)

open Cesk.Ast
module A = Middle_end.Anf
module E = Middle_end.Passes.Effect_analysis

(* The production leaf classification (only [Effect.Console.log] performs). *)
let ea = E.create ~effectful_leaf:Ffi.effectful ~foreign_arity:Ffi.foreign_arity

let log_app s = A.CApp (A.AForeign "Effect.Console.log", [ A.ALit (LString s) ])

let check_eperf msg expected e = Alcotest.(check bool) msg expected (ea.eperf e)

(* I1: applying [log] to its string only *builds* the `#perform` thunk — pure. *)
let test_log_build_is_pure () =
  check_eperf "let p = log \"x\" in p  (build only)" false
    (A.Let ("p", log_app "x", A.Ret (A.CAtom (A.AVar "p"))))

(* …and the bound value is itself an effect-thunk: forcing it (apply to unit)
   performs. So a *forced* log is may-perform. *)
let test_log_force_performs () =
  check_eperf "let p = log \"x\" in p ()" true
    (A.Let
       ( "p"
       , log_app "x"
       , A.Let ("r", A.CApp (A.AVar "p", [ A.ALit (LInt 0) ]), A.Ret (A.CAtom (A.AVar "r"))) ))

(* Over-application: [(log s) u] is flattened by [collect_app] into one node
   [CApp(log, [s; u])] (two args to the arity-1 leaf). Saturating builds the
   `#perform`, then the extra arg forces it — so this performs. *)
let test_log_over_application () =
  check_eperf "(log \"x\") () as one node" true
    (A.Ret (A.CApp (A.AForeign "Effect.Console.log", [ A.ALit (LString "x"); A.ALit (LInt 0) ])))

(* The mutation primops are may-perform at evaluation (NewArray allocates observable
   identity; SetArray mutates). Reads/arithmetic are pure. *)
let test_newarray_setarray_perform () =
  check_eperf "new[]" true (A.Ret (A.CPrim (NewArray, [ A.ALit (LInt 3); A.ALit (LInt 0) ])));
  check_eperf "set[]" true
    (A.Let
       ( "a"
       , A.CPrim (NewArray, [ A.ALit (LInt 1); A.ALit (LInt 0) ])
       , A.Ret (A.CPrim (SetArray, [ A.AVar "a"; A.ALit (LInt 0); A.ALit (LInt 9) ])) ));
  check_eperf "1+2 is pure" false
    (A.Ret (A.CPrim (AddInt, [ A.ALit (LInt 1); A.ALit (LInt 2) ])))

(* A pure first-order function, called, performs nothing. *)
let test_pure_function () =
  let f = A.CLam ([ "x" ], A.Ret (A.CPrim (AddInt, [ A.AVar "x"; A.AVar "x" ]))) in
  check_eperf "let f = \\x -> x+x in f 3" false
    (A.Let ("f", f, A.Let ("r", A.CApp (A.AVar "f", [ A.ALit (LInt 3) ]), A.Ret (A.CAtom (A.AVar "r")))))

(* Applying an *unknown* value (a parameter) is conservatively may-perform — this is
   exactly what makes [bindE]-shaped combinators (whose body forces a parameter) come
   out as effectful functions, with no name recognition needed. *)
let test_unknown_application_conservative () =
  let combinator = A.CLam ([ "g" ], A.Ret (A.CApp (A.AVar "g", [ A.ALit (LInt 0) ]))) in
  let sums = ea.analyze (A.Let ("k", combinator, A.Ret (A.CAtom (A.AVar "k")))) in
  let s = Middle_end.Passes.Effect_analysis.(M.find "k" sums) in
  Alcotest.(check bool) "saturating \\g -> g () may perform" true s.E.vsat

(* A self-recursive effectful loop must be classified may-perform via the group's
   least fixpoint — not misread as pure (which would let DBE delete its effects). *)
let test_recursive_effect_fixpoint () =
  (* go = \n -> let _ = (log "x") () in go n   (forces the log each step) *)
  let body =
    A.Let
      ( "p"
      , log_app "x"
      , A.Let
          ( "_u"
          , A.CApp (A.AVar "p", [ A.ALit (LInt 0) ])
          , A.Ret (A.CApp (A.AVar "go", [ A.AVar "n" ])) ) )
  in
  let go = ("go", A.Ret (A.CLam ([ "n" ], body))) in
  let sums = ea.analyze (A.LetRec ([ go ], A.Ret (A.CAtom (A.AVar "go")))) in
  let s = Middle_end.Passes.Effect_analysis.(M.find "go" sums) in
  Alcotest.(check bool) "recursive go that forces log is may-perform" true s.E.vsat

(* A multi-argument effectful native leaf (a stand-in for a future
   [runEffectFn2 :: EffectFn2 a b c -> a -> b -> Effect c], arity 3). The arity is
   load-bearing: with arity 3, applying all three only *builds* the `Effect` thunk
   (pure), a 2-arg partial application is also a pure build, and forcing the built
   thunk performs. An arity-1 assumption would mis-place all of these. *)
let test_multiarg_effectful_leaf () =
  let arity k = if String.equal k "RT2" then 3 else Ffi.foreign_arity k in
  let effectful k = String.equal k "RT2" || Ffi.effectful k in
  let a = E.create ~effectful_leaf:effectful ~foreign_arity:arity in
  let rt2 args = A.CApp (A.AForeign "RT2", args) in
  let i n = A.ALit (LInt n) in
  (* RT2 f a b  — full application: builds the Effect thunk, pure *)
  Alcotest.(check bool) "runEffectFn2 f a b builds (pure)" false
    (a.E.eperf (A.Ret (rt2 [ i 0; i 1; i 2 ])));
  (* RT2 f a  — partial application: pure build *)
  Alcotest.(check bool) "runEffectFn2 f a (PAP) is pure" false
    (a.E.eperf (A.Ret (rt2 [ i 0; i 1 ])));
  (* (RT2 f a b) ()  — forcing the built thunk performs *)
  Alcotest.(check bool) "forcing runEffectFn2's result performs" true
    (a.E.eperf
       (A.Let ("t", rt2 [ i 0; i 1; i 2 ], A.Ret (A.CApp (A.AVar "t", [ i 9 ])))))

(* Regression (cross-review): an effect buried ≥2 levels deep in a chain of
   let-returned closures. A 2-level [vsum] cannot represent it, so over-application
   must be conservatively may-perform — else DBE would delete the effect.

     h = \c -> (log "x") c     -- over-applies log: h performs when applied
     g = \b -> let h = … in h  -- returns h
     f = \a -> let g = … in g  -- returns g
     f 0 1 2                    -- runs h ⇒ performs

   Both the flattened call and the staged calls must read may-perform. *)
let test_deep_returned_closure_chain () =
  let h_lam =
    A.CLam ([ "c" ], A.Ret (A.CApp (A.AForeign "Effect.Console.log", [ A.ALit (LString "x"); A.AVar "c" ])))
  in
  let g_lam = A.CLam ([ "b" ], A.Let ("h", h_lam, A.Ret (A.CAtom (A.AVar "h")))) in
  let f_lam = A.CLam ([ "a" ], A.Let ("g", g_lam, A.Ret (A.CAtom (A.AVar "g")))) in
  let i n = A.ALit (LInt n) in
  (* flattened: f 0 1 2 in one node *)
  check_eperf "f 0 1 2 (flattened) performs" true
    (A.Let ("f", f_lam, A.Let ("r", A.CApp (A.AVar "f", [ i 0; i 1; i 2 ]), A.Ret (A.CAtom (A.AVar "r")))));
  (* staged: r1 = f 0; r2 = r1 1; r3 = r2 2 *)
  check_eperf "f 0; · 1; · 2 (staged) performs" true
    (A.Let
       ( "f"
       , f_lam
       , A.Let
           ( "r1"
           , A.CApp (A.AVar "f", [ i 0 ])
           , A.Let
               ( "r2"
               , A.CApp (A.AVar "r1", [ i 1 ])
               , A.Let ("r3", A.CApp (A.AVar "r2", [ i 2 ]), A.Ret (A.CAtom (A.AVar "r3"))) ) ) ))

(* Point-free top-level declarations have no syntactic lambda, yet their arity must be
   recovered from the head's arity (a partial application). [inc = add 1] is arity 1
   and pure; [logIt = log "x"] is the effectful thunk (vsat), forced via application. *)
let test_point_free_arity () =
  let add = A.CLam ([ "x"; "y" ], A.Ret (A.CPrim (AddInt, [ A.AVar "x"; A.AVar "y" ]))) in
  let i n = A.ALit (LInt n) in
  let with_inc tail = A.Let ("add", add, A.Let ("inc", A.CApp (A.AVar "add", [ i 1 ]), tail)) in
  let sums = ea.analyze (with_inc (A.Ret (A.CAtom (A.AVar "inc")))) in
  let inc = E.M.find "inc" sums in
  Alcotest.(check int) "point-free inc = add 1 has arity 1" 1 inc.E.arity;
  Alcotest.(check bool) "point-free inc is pure" false inc.E.vsat;
  check_eperf "inc 5 (exact saturation) is pure" false
    (with_inc (A.Let ("r", A.CApp (A.AVar "inc", [ i 5 ]), A.Ret (A.CAtom (A.AVar "r")))));
  (* point-free effectful: logIt = log "x" is the effect thunk; forcing it performs *)
  let with_log tail =
    A.Let ("logIt", A.CApp (A.AForeign "Effect.Console.log", [ A.ALit (LString "x") ]), tail)
  in
  let lsums = ea.analyze (with_log (A.Ret (A.CAtom (A.AVar "logIt")))) in
  Alcotest.(check bool) "point-free logIt = log \"x\" is an effect thunk" true
    (E.M.find "logIt" lsums).E.vsat;
  check_eperf "forcing point-free logIt performs" true
    (with_log (A.Let ("u", A.CApp (A.AVar "logIt", [ i 0 ]), A.Ret (A.CAtom (A.AVar "u")))))

(* --- dead-binding elimination (the analysis' consumer) -------------------- *)

module D = Middle_end.Passes.Dbe

let dbe = D.run ~effectful_leaf:Ffi.effectful ~foreign_arity:Ffi.foreign_arity
let ret0 = A.Ret (A.CAtom (A.ALit (LInt 0)))
let is_ret0 = function A.Ret (A.CAtom (A.ALit (LInt 0))) -> true | _ -> false
let is_let name = function A.Let (n, _, _) -> String.equal n name | _ -> false
let add12 = A.CPrim (AddInt, [ A.ALit (LInt 1); A.ALit (LInt 2) ])

(* A dead, pure binding is dropped. *)
let test_dbe_drops_dead_pure () =
  Alcotest.(check bool) "drop dead pure let" true (is_ret0 (dbe (A.Let ("x", add12, ret0))))

(* A used binding is kept. *)
let test_dbe_keeps_used () =
  Alcotest.(check bool) "keep used let" true
    (is_let "x" (dbe (A.Let ("x", add12, A.Ret (A.CAtom (A.AVar "x"))))))

(* A dead binding that *performs* is kept (forced log). *)
let test_dbe_keeps_effectful () =
  Alcotest.(check bool) "keep dead effectful let" true
    (is_let "u"
       (dbe
          (A.Let
             ("u", A.CApp (A.AForeign "Effect.Console.log", [ A.ALit (LString "x"); A.ALit (LInt 0) ]), ret0))))

(* I1: a dead binding that only *builds* an Effect (never forces it) is droppable. *)
let test_dbe_drops_effect_build () =
  Alcotest.(check bool) "drop dead Effect build (I1)" true (is_ret0 (dbe (A.Let ("p", log_app "x", ret0))))

let dbe_suite =
  [ Alcotest.test_case "drops dead pure" `Quick test_dbe_drops_dead_pure
  ; Alcotest.test_case "keeps used" `Quick test_dbe_keeps_used
  ; Alcotest.test_case "keeps dead effectful" `Quick test_dbe_keeps_effectful
  ; Alcotest.test_case "drops dead Effect build (I1)" `Quick test_dbe_drops_effect_build
  ]

let suite =
  [ Alcotest.test_case "log build is pure (I1)" `Quick test_log_build_is_pure
  ; Alcotest.test_case "log force performs" `Quick test_log_force_performs
  ; Alcotest.test_case "log over-application performs" `Quick test_log_over_application
  ; Alcotest.test_case "new[]/set[] perform" `Quick test_newarray_setarray_perform
  ; Alcotest.test_case "pure function" `Quick test_pure_function
  ; Alcotest.test_case "unknown application conservative" `Quick test_unknown_application_conservative
  ; Alcotest.test_case "recursive effect fixpoint" `Quick test_recursive_effect_fixpoint
  ; Alcotest.test_case "multi-arg effectful leaf (arity-aware)" `Quick test_multiarg_effectful_leaf
  ; Alcotest.test_case "deep returned-closure chain (>=2 levels)" `Quick test_deep_returned_closure_chain
  ; Alcotest.test_case "point-free arity" `Quick test_point_free_arity
  ]

let () = Alcotest.run "effect_analysis" [ "effect_analysis", suite; "dbe", dbe_suite ]
