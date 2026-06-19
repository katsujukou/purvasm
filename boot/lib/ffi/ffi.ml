(** Foreign-name resolution as an ordered *provider ladder* (ADR-0017): a foreign
    qualified ident is tried against providers in priority order — intrinsic, then
    (later) syscall, then user-defined foreign — the first match winning, else the
    name stays unbound (and is [stuck] only if forced, ADR-0016). This slice fills
    only the intrinsic rung: scalar leaves resolve to eta-expanded primop terms.
    The resolver has the shape [Link] consumes: [qualified key -> Cesk.Ast.term
    option]. Stdlib-only. *)

module C = Cesk.Ast

(** A provider: resolve a foreign qualified key to a binding term, or decline. *)
type provider = string -> C.term option

(** Eta-expand an arity-[n] primop into a curried lambda
    [\$0 -> … -> \$(n-1) -> Prim(op, [$0; …])], so a leaf applied curried or
    partially behaves like any other closure. *)
let eta (op : C.primop) (arity : int) : C.term =
  let names = List.init arity (fun i -> "$" ^ string_of_int i) in
  let body = C.Prim (op, List.map (fun n -> C.Var n) names) in
  List.fold_right (fun n acc -> C.Lam (n, acc)) names body

(* `abs` over existing primops (the prelude `intDegree`'s maxInt cap is irrelevant
   for the values reached here), kept a composite term rather than a new primop. *)
let int_degree : C.term =
  C.Lam
    ( "$0"
    , C.If
        ( C.Prim (C.LtInt, [ C.Var "$0"; C.Lit (C.LInt 0) ])
        , C.Prim (C.SubInt, [ C.Lit (C.LInt 0); C.Var "$0" ])
        , C.Var "$0" ) )

(** The intrinsic table: a foreign leaf's qualified ident → its binding term. Most
    are eta-expanded primops; [intDegree] is a composite term. The idents are
    pinned against the `prelude` package's foreign modules, not guessed. `Char` is
    `Int` (ADR-0006), so the char leaves reuse the int primops. *)
let intrinsics : (string * C.term) list =
  let p key op arity = key, eta op arity in
  [ p "Data.Semiring.intAdd" C.AddInt 2
  ; p "Data.Semiring.intMul" C.MulInt 2
  ; p "Data.Semiring.numAdd" C.AddNumber 2
  ; p "Data.Semiring.numMul" C.MulNumber 2
  ; p "Data.Ring.intSub" C.SubInt 2
  ; p "Data.Ring.numSub" C.SubNumber 2
  ; p "Data.EuclideanRing.intDiv" C.DivInt 2
  ; p "Data.EuclideanRing.intMod" C.ModInt 2
  ; p "Data.EuclideanRing.numDiv" C.DivNumber 2
  ; "Data.EuclideanRing.intDegree", int_degree
  ; p "Data.Eq.eqIntImpl" C.EqInt 2
  ; p "Data.Eq.eqNumberImpl" C.EqNumber 2
  ; p "Data.Eq.eqStringImpl" C.EqString 2
  ; p "Data.Eq.eqCharImpl" C.EqInt 2
  ; p "Data.Eq.eqBooleanImpl" C.EqBool 2
  ; p "Data.HeytingAlgebra.boolConj" C.AndBool 2
  ; p "Data.HeytingAlgebra.boolDisj" C.OrBool 2
  ; p "Data.HeytingAlgebra.boolNot" C.NotBool 1
  ; p "Data.Semigroup.concatString" C.Append 2
    (* A foreign *constant*, not a function. `Unit` is opaque (no constructors,
       no fields, never inspected), so purvasm is free to represent `unit` as it
       likes; we pick the immediate [0] — the cheapest representation and aligned
       with the future VM ABI — rather than JS's `{}`. Forced when a
       `Semiring Unit`-style dictionary's `zero`/`one` fields are built, so it
       must resolve to a value. *)
  ; "Data.Unit.unit", C.Lit (C.LInt 0)
    (* The compiler builtin `Prim.undefined` is the throwaway argument applied to
       a nullary superclass thunk (`(\_ -> superDict) Prim.undefined`) to force
       it; never inspected, so any value works — the immediate [0]. (It is not a
       module `foreign import`, so linking must resolve it as a free reference.) *)
  ; "Prim.undefined", C.Lit (C.LInt 0)
  ]

(** The intrinsic rung as a provider. *)
let intrinsic : provider = fun key -> List.assoc_opt key intrinsics

(* --- structural / higher-order foreigns as guest terms (ADR-0020) --------- *)

(* Build a curried lambda from parameter names and a body. *)
let lams (params : string list) (body : C.term) : C.term =
  List.fold_right (fun p acc -> C.Lam (p, acc)) params body

let v = (fun name -> C.Var name : string -> C.term)
let int_lit n = C.Lit (C.LInt n)

(* `arrayMap f xs` (Data.Functor): build a fresh array of [length xs] and fill
   slot i with [f xs[i]] via a guest recursion — the callback is an ordinary
   `App`, so no native re-entrancy (ADR-0020), over the first-order array builders
   (ADR-0019). *)
let array_map : C.term =
  lams
    [ "f"; "xs" ]
    (C.Let
       ( "n"
       , C.Prim (LengthArray, [ v "xs" ])
       , C.Letrec
           ( [ ( "go"
               , lams
                   [ "out"; "i" ]
                   (C.If
                      ( C.Prim (LtInt, [ v "i"; v "n" ])
                      , C.App
                          ( C.App
                              ( v "go"
                              , C.Prim
                                  ( SetArray
                                  , [ v "out"
                                    ; v "i"
                                    ; C.App (v "f", C.Prim (IndexArray, [ v "xs"; v "i" ]))
                                    ] ) )
                          , C.Prim (AddInt, [ v "i"; int_lit 1 ]) )
                      , v "out" )) )
             ]
           , C.App (C.App (v "go", C.Prim (NewArray, [ v "n" ])), int_lit 0) ) ))

(* `eqArrayImpl eq xs ys` (Data.Eq): equal lengths and every element equal, the
   element test `eq xs[i] ys[i]` an ordinary `App`. Needs no new primop. *)
let eq_array : C.term =
  lams
    [ "eq"; "xs"; "ys" ]
    (C.Let
       ( "n"
       , C.Prim (LengthArray, [ v "xs" ])
       , C.If
           ( C.Prim (EqInt, [ v "n"; C.Prim (LengthArray, [ v "ys" ]) ])
           , C.Letrec
               ( [ ( "go"
                   , C.Lam
                       ( "i"
                       , C.If
                           ( C.Prim (LtInt, [ v "i"; v "n" ])
                           , C.If
                               ( C.App
                                   ( C.App (v "eq", C.Prim (IndexArray, [ v "xs"; v "i" ]))
                                   , C.Prim (IndexArray, [ v "ys"; v "i" ]) )
                               , C.App (v "go", C.Prim (AddInt, [ v "i"; int_lit 1 ]))
                               , C.Lit (C.LBool false) )
                           , C.Lit (C.LBool true) ) ) )
                 ]
               , C.App (v "go", int_lit 0) )
           , C.Lit (C.LBool false) ) ))

(** Structural / higher-order foreigns, as guest terms over the first-order
    primitives. Hand-written `Cesk.Ast` for now; a PureScript surface comes with
    the optimiser (ADR-0020). *)
let structural : (string * C.term) list =
  [ "Data.Functor.arrayMap", array_map; "Data.Eq.eqArrayImpl", eq_array ]

let structural_provider : provider = fun key -> List.assoc_opt key structural

(** The provider ladder, in priority order. The syscall and user-foreign rungs
    are later providers appended here, with linking unchanged. *)
let ladder : provider list = [ intrinsic; structural_provider ]

(** The resolver [Link] consumes: the first provider to bind the key wins;
    otherwise the key is left unbound (ADR-0016). *)
let resolver (key : string) : C.term option = List.find_map (fun p -> p key) ladder
