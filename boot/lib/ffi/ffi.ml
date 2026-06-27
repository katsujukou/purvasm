(** Foreign-name resolution as an ordered *provider ladder* (ADR-0017): a foreign
    qualified ident is tried against providers in priority order — intrinsic
    (eta-expanded primops, ADR-0017), then structural (guest terms over first-order
    primitives, ADR-0020), then native (opaque host functions, ADR-0022) — the
    first match winning, else the name stays unbound (and is [stuck] only if
    forced, ADR-0016). The resolver has the shape [Link] consumes: [qualified key
    -> Cesk.Ast.term option]; the native rung additionally exposes [host], the
    registry the machine consults to run an opaque [Foreign] reference. A
    user-defined foreign rung is a later provider appended here. Stdlib-only. *)

module C = Cesk.Ast
module V = Cesk.Value

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
    (* The `purvasm-base` primitive layer (ADR-0038): `Purvasm.*` foreigns the backend
       recognises as intrinsics — the seam the `ulib` reimplementations build on. *)
  ; p "Purvasm.Int.add" C.AddInt 2
  ; p "Purvasm.Int.sub" C.SubInt 2
  ; p "Purvasm.Int.mul" C.MulInt 2
  ; p "Purvasm.Int.eq" C.EqInt 2
  ; p "Purvasm.Int.lt" C.LtInt 2
  ; p "Purvasm.Int.div" C.DivInt 2
  ; p "Purvasm.Int.mod" C.ModInt 2
  ; p "Purvasm.Int.and" C.AndInt 2
  ; p "Purvasm.Int.or" C.OrInt 2
  ; p "Purvasm.Int.xor" C.XorInt 2
  ; p "Purvasm.Int.shl" C.ShlInt 2
  ; p "Purvasm.Int.shr" C.ShrInt 2
  ; p "Purvasm.Int.zshr" C.ZshrInt 2
  ; p "Purvasm.Int.complement" C.ComplementInt 1
    (* Cross-representation conversions (ADR-0041): `Int`<->`Number` casts that JS never
       needed (there `Int` and `Number` are one value). `fromNumber` is `ToInt32`. *)
  ; p "Purvasm.Int.toNumber" C.IntToNumber 1
  ; p "Purvasm.Int.fromNumber" C.NumberToInt 1
  ; p "Purvasm.Number.add" C.AddNumber 2
  ; p "Purvasm.Number.sub" C.SubNumber 2
  ; p "Purvasm.Number.mul" C.MulNumber 2
  ; p "Purvasm.Number.div" C.DivNumber 2
  ; p "Purvasm.Number.eq" C.EqNumber 2
  ; p "Purvasm.Number.lt" C.LtNumber 2
  ; p "Purvasm.Array.length" C.LengthArray 1
  ; p "Purvasm.Array.unsafeIndex" C.IndexArray 2
  ; p "Purvasm.Array.unsafeNew" C.NewArray 1
  ; p "Purvasm.Array.unsafeSet" C.SetArray 3
  ; p "Purvasm.Boolean.not" C.NotBool 1
    (* `Char` is `Int` (ADR-0006), so the char-code conversions are the identity. *)
  ; "Purvasm.Char.toCodePoint", C.Lam ("$0", C.Var "$0")
  ; "Purvasm.Char.fromCodePoint", C.Lam ("$0", C.Var "$0")
    (* `Record.Unsafe` dynamic record access by runtime label (ADR-0010 record-as-field-map):
       eta-expanded record primops (the label-as-value form of `Accessor`/`Update`). *)
  ; p "Record.Unsafe.unsafeGet" C.RecordGet 2
  ; p "Record.Unsafe.unsafeSet" C.RecordSet 3
  ; p "Record.Unsafe.unsafeHas" C.RecordHas 2
  ; p "Record.Unsafe.unsafeDelete" C.RecordDelete 2
    (* `Record.Builder` over the same record primops: `copyRecord` is the identity (records are
       persistent — no defensive copy needed); insert/delete reuse the record primops.
       `unsafeModify`/`unsafeRename` are structural guest terms (below). *)
  ; "Record.Builder.copyRecord", C.Lam ("$0", C.Var "$0")
  ; p "Record.Builder.unsafeInsert" C.RecordSet 3
  ; p "Record.Builder.unsafeDelete" C.RecordDelete 2
    (* `Data.Number` IEEE constants (ADR-0042): pure compile-time `Number` literals, so they
       resolve at the intrinsic rung (like `Data.Unit.unit`), not as host leaves. *)
  ; "Data.Number.nan", C.Lit (C.LNumber Stdlib.nan)
  ; "Data.Number.infinity", C.Lit (C.LNumber Stdlib.infinity)
    (* `unsafeCoerce` is a representation-preserving cast — at runtime the identity,
       exactly like an erased newtype constructor (ADR-0018). The optimizer
       beta-reduces the resulting [(\x -> x) e] away (ADR-0028). *)
  ; "Unsafe.Coerce.unsafeCoerce", C.Lam ("$0", C.Var "$0")
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

(* `ord<T>Impl lt eq gt x y` (Data.Ord): the comparator returns one of the three
   `Ordering` values passed in (the instance supplies `LT`/`EQ`/`GT`). A composite
   first-order guest term over the comparison primops — no new primop, no closure
   at the boundary. *)
let ord_cmp (lt : C.primop) (eq : C.primop) : C.term =
  lams
    [ "lt"; "eq"; "gt"; "x"; "y" ]
    (C.If
       ( C.Prim (lt, [ v "x"; v "y" ])
       , v "lt"
       , C.If (C.Prim (eq, [ v "x"; v "y" ]), v "eq", v "gt") ))

(* Boolean order is `false < true`: equal -> eq, else the truthy one is greater. *)
let ord_boolean : C.term =
  lams
    [ "lt"; "eq"; "gt"; "x"; "y" ]
    (C.If (C.Prim (EqBool, [ v "x"; v "y" ]), v "eq", C.If (v "x", v "gt", v "lt")))

(* --- Effect monad + combinators as structural guest terms (ADR-0023) ------ *)

(* `Effect a` is the nullary thunk `Unit -> a`; `unit` is the immediate 0, which
   these thunks ignore. Correct sequencing is just strict evaluation of `bindE`. *)
let unit_lit = int_lit 0
let run_eff (e : C.term) : C.term = C.App (e, unit_lit)

(* pureE a = \_ -> a *)
let eff_pure : C.term = lams [ "a"; "$u" ] (v "a")

(* bindE a f = \_ -> f (a unit) unit — force a (its effects), pass the result to
   f, then force the resulting effect. *)
let eff_bind : C.term = lams [ "a"; "f"; "$u" ] (run_eff (C.App (v "f", run_eff (v "a"))))

(* untilE f = \_ -> repeat (f unit) until it yields true. *)
let eff_until : C.term =
  lams
    [ "f"; "$u" ]
    (C.Letrec
       ( [ "go", C.Lam ("$g", C.If (run_eff (v "f"), unit_lit, C.App (v "go", unit_lit))) ]
       , C.App (v "go", unit_lit) ))

(* whileE f a = \_ -> while (f unit) run (a unit). *)
let eff_while : C.term =
  lams
    [ "f"; "a"; "$u" ]
    (C.Letrec
       ( [ ( "go"
           , C.Lam
               ( "$g"
               , C.If
                   ( run_eff (v "f")
                   , C.Let ("$_", run_eff (v "a"), C.App (v "go", unit_lit))
                   , unit_lit ) ) )
         ]
       , C.App (v "go", unit_lit) ))

(* forE lo hi f = \_ -> for i in [lo, hi) run (f i unit). *)
let eff_for : C.term =
  lams
    [ "lo"; "hi"; "f"; "$u" ]
    (C.Letrec
       ( [ ( "go"
           , C.Lam
               ( "i"
               , C.If
                   ( C.Prim (LtInt, [ v "i"; v "hi" ])
                   , C.Let
                       ( "$_"
                       , run_eff (C.App (v "f", v "i"))
                       , C.App (v "go", C.Prim (AddInt, [ v "i"; int_lit 1 ])) )
                   , unit_lit ) ) )
         ]
       , C.App (v "go", v "lo") ))

(* foreachE as f = \_ -> for each element run (f x unit). *)
let eff_foreach : C.term =
  lams
    [ "as"; "f"; "$u" ]
    (C.Let
       ( "n"
       , C.Prim (LengthArray, [ v "as" ])
       , C.Letrec
           ( [ ( "go"
               , C.Lam
                   ( "i"
                   , C.If
                       ( C.Prim (LtInt, [ v "i"; v "n" ])
                       , C.Let
                           ( "$_"
                           , run_eff
                               (C.App (v "f", C.Prim (IndexArray, [ v "as"; v "i" ])))
                           , C.App (v "go", C.Prim (AddInt, [ v "i"; int_lit 1 ])) )
                       , unit_lit ) ) )
             ]
           , C.App (v "go", int_lit 0) ) ))

(* --- Effect.Ref as a one-cell mutable array (ADR-0019, ADR-0023) ---------- *)

(* A `Ref` is opaque (`foreign import data Ref`), so we represent it as a fresh
   one-cell mutable array; the JS `{ value }` record is mutable, which our records
   (ADR-0010) are not. Every Ref op is an `Effect` thunk over the array builders. *)
let ref_new : C.term =
  lams
    [ "val"; "$u" ]
    (C.Prim (SetArray, [ C.Prim (NewArray, [ int_lit 1 ]); int_lit 0; v "val" ]))

let ref_read : C.term = lams [ "ref"; "$u" ] (C.Prim (IndexArray, [ v "ref"; int_lit 0 ]))

let ref_write : C.term =
  lams
    [ "val"; "ref"; "$u" ]
    (C.Let ("$_", C.Prim (SetArray, [ v "ref"; int_lit 0; v "val" ]), unit_lit))

(* modifyImpl f ref = \_ -> let t = f ref[0] in (ref[0] := t.state; t.value). *)
let ref_modify : C.term =
  lams
    [ "f"; "ref"; "$u" ]
    (C.Let
       ( "t"
       , C.App (v "f", C.Prim (IndexArray, [ v "ref"; int_lit 0 ]))
       , C.Let
           ( "$_"
           , C.Prim (SetArray, [ v "ref"; int_lit 0; C.Accessor (v "t", "state") ])
           , C.Accessor (v "t", "value") ) ))

(* --- Control.Monad.ST.Internal: the same thunk model as Effect (ADR-0023) -- *)

(* An `ST r a` is the same nullary thunk `Unit -> a` as `Effect a`, and `STRef` the
   same one-cell mutable array as `Ref`; ST mutation stays local to its region, so
   forcing the thunk (via [run]) escapes to a pure value. Most ops are shared with the
   Effect terms (`pure_`/`bind_`/`while`/`for`/`foreach`/`new`/`read`/`modifyImpl`);
   only these three differ. *)

(* map_ f a = \_ -> f (a unit). *)
let st_map : C.term = lams [ "f"; "a"; "$u" ] (C.App (v "f", run_eff (v "a")))

(* run f = f unit — force the ST thunk, escaping its local mutation to a pure result. *)
let st_run : C.term = lams [ "f" ] (run_eff (v "f"))

(* write val ref = \_ -> (ref[0] := val; val). Unlike `Effect.Ref.write` this returns
   the written value (the JS assignment-expression result), not unit. *)
let st_write : C.term =
  lams
    [ "val"; "ref"; "$u" ]
    (C.Let ("$_", C.Prim (SetArray, [ v "ref"; int_lit 0; v "val" ]), v "val"))

(* --- Control.Monad.ST.Uncurried: curried <-> uncurried ST adapters (ADR-0039) -
   In the all-curried runtime an `STFnN` is just a curried function whose saturated
   application runs the `ST`. `mkSTFnN f = \x0..xN -> run (f x0 .. xN)` forces that
   thunk after applying; `runSTFnN g x0..xN = \_ -> g x0 .. xN` rebuilds it. The
   `STFnN` *types* are phantom (`foreign import data`), so only these values resolve. *)
let st_uncurried : (string * C.term) list =
  let arg i = Printf.sprintf "x%d" i in
  List.concat_map
    (fun n ->
       let xs = List.init n arg in
       let app_args f = List.fold_left (fun acc x -> C.App (acc, v x)) f xs in
       [ ( Printf.sprintf "Control.Monad.ST.Uncurried.mkSTFn%d" n
         , lams ("f" :: xs) (run_eff (app_args (v "f"))) )
       ; ( Printf.sprintf "Control.Monad.ST.Uncurried.runSTFn%d" n
         , lams (("g" :: xs) @ [ "$u" ]) (app_args (v "g")) )
       ])
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

(* --- Data.Function.Uncurried: curried <-> uncurried *pure* function adapters ----
   The pure analogue of `ST.Uncurried` (ADR-0039): boot is all-curried, so an `FnN`
   (N>=2) just *is* a curried function — `mkFnN = identity`, `runFnN = saturated apply`.
   There is no `Fn1`; `Fn0 a` is the `Unit -> a` thunk (`mkFn0`/`runFn0` introduce and
   force it). The `FnN` *types* are phantom, so only these values resolve. *)
let fn_uncurried : (string * C.term) list =
  let arg i = Printf.sprintf "x%d" i in
  let nary n =
    let xs = List.init n arg in
    let app_args f = List.fold_left (fun acc x -> C.App (acc, v x)) f xs in
    [ Printf.sprintf "Data.Function.Uncurried.mkFn%d" n, lams [ "f" ] (v "f")
    ; ( Printf.sprintf "Data.Function.Uncurried.runFn%d" n
      , lams ("f" :: xs) (app_args (v "f")) )
    ]
  in
  [ "Data.Function.Uncurried.mkFn0", lams [ "fn"; "$u" ] (C.App (v "fn", int_lit 0))
  ; "Data.Function.Uncurried.runFn0", lams [ "f" ] (C.App (v "f", int_lit 0))
  ]
  @ List.concat_map nary [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

(* `Record.Builder.unsafeModify l f r = RecordSet l (f (RecordGet l r)) r` — higher-order
   (the modifier `f` is an ordinary `App`, ADR-0020), so a guest term, not a primop. *)

(** Structural foreigns, as guest terms over the first-order primitives:
    higher-order ones (`arrayMap`, `eqArrayImpl`) whose callback is an ordinary
    `App`, composite first-order ones (the scalar `Ord` comparisons), the
    `Effect` monad / `Effect.Ref` thunks (ADR-0023), and the `ST` monad / `STRef`
    (same model). Hand-written `Cesk.Ast` for now; a PureScript surface comes with
    the optimiser (ADR-0020). `Char` is `Int` (ADR-0006), so `ordCharImpl` reuses the
    int term. *)
let record_modify : C.term =
  lams
    [ "l"; "f"; "r" ]
    (C.Prim
       (RecordSet, [ v "l"; C.App (v "f", C.Prim (RecordGet, [ v "l"; v "r" ])); v "r" ]))

(* `Record.Builder.unsafeRename old new r = RecordSet new (RecordGet old r) (RecordDelete old r)`. *)
let record_rename : C.term =
  lams
    [ "o"; "n"; "r" ]
    (C.Prim
       ( RecordSet
       , [ v "n"
         ; C.Prim (RecordGet, [ v "o"; v "r" ])
         ; C.Prim (RecordDelete, [ v "o"; v "r" ])
         ] ))

(* `Data.Number.fromStringImpl` is the higher-order `Fn4 String (Number -> Boolean)
   (forall a. a -> Maybe a) (forall a. Maybe a) (Maybe Number)` foreign behind
   `Data.Number.fromString` (ADR-0046). Like the `Fn*`/`ST` adapters it is a structural guest
   term, not a native leaf (native leaves are first-order, ADR-0022): it parses via the
   first-order `parseFloatImpl` leaf, then applies the caller's `isFinite`/`Just`/`Nothing` —
   so a `NaN`/non-finite parse becomes `Nothing`, exactly as the prelude's
   `runFn4 _ str isFinite Just Nothing`. *)
let number_from_string_impl : C.term =
  lams
    [ "str"; "isFin"; "just"; "nothing" ]
    (C.Let
       ( "n"
       , C.App (C.Foreign "Data.Number.parseFloatImpl", v "str")
       , C.If (C.App (v "isFin", v "n"), C.App (v "just", v "n"), v "nothing") ))

let structural : (string * C.term) list =
  [ "Data.Functor.arrayMap", array_map
  ; "Data.Number.fromStringImpl", number_from_string_impl
  ; "Record.Builder.unsafeModify", record_modify
  ; "Record.Builder.unsafeRename", record_rename
  ; "Data.Eq.eqArrayImpl", eq_array
  ; "Data.Ord.ordIntImpl", ord_cmp LtInt EqInt
  ; "Data.Ord.ordNumberImpl", ord_cmp LtNumber EqNumber
  ; "Data.Ord.ordStringImpl", ord_cmp LtString EqString
  ; "Data.Ord.ordCharImpl", ord_cmp LtInt EqInt
  ; "Data.Ord.ordBooleanImpl", ord_boolean
  ; "Effect.pureE", eff_pure
  ; "Effect.bindE", eff_bind
  ; "Effect.untilE", eff_until
  ; "Effect.whileE", eff_while
  ; "Effect.forE", eff_for
  ; "Effect.foreachE", eff_foreach
  ; "Effect.Ref._new", ref_new
  ; "Effect.Ref.read", ref_read
  ; "Effect.Ref.write", ref_write
  ; "Effect.Ref.modifyImpl", ref_modify
    (* `_unsafePartial f = f unit` (matching the prelude JS `f()`): discharge the
       phantom `Partial` constraint by applying the partial computation to a dummy
       dictionary. `Partial` has no methods, so the dict (the immediate [0]) is
       never inspected. *)
  ; "Partial.Unsafe._unsafePartial", lams [ "f" ] (C.App (v "f", int_lit 0))
    (* `Control.Monad.ST.Internal`: same thunk/cell model as Effect (ADR-0023). *)
  ; "Control.Monad.ST.Internal.map_", st_map
  ; "Control.Monad.ST.Internal.pure_", eff_pure
  ; "Control.Monad.ST.Internal.bind_", eff_bind
  ; "Control.Monad.ST.Internal.run", st_run
  ; "Control.Monad.ST.Internal.while", eff_while
  ; "Control.Monad.ST.Internal.for", eff_for
  ; "Control.Monad.ST.Internal.foreach", eff_foreach
  ; "Control.Monad.ST.Internal.new", ref_new
  ; "Control.Monad.ST.Internal.read", ref_read
  ; "Control.Monad.ST.Internal.write", st_write
  ; "Control.Monad.ST.Internal.modifyImpl", ref_modify
  ]
  @ st_uncurried
  @ fn_uncurried

let structural_provider : provider = fun key -> List.assoc_opt key structural

(* --- native rung: opaque host-provided foreign functions (ADR-0022) -------- *)

(* Encode a Unicode code point as UTF-8 bytes (our `String`/`Char` representation,
   ADR-0006), so a shown character round-trips its bytes. *)
let utf8_of_cp (cp : int) : string =
  let b = Buffer.create 4 in
  if cp < 0x80
  then Buffer.add_char b (Char.chr cp)
  else if cp < 0x800
  then (
    Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000
  then (
    Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    Buffer.add_char b (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))));
  Buffer.contents b

(* The C-style control escapes the prelude's `showCharImpl`/`showStringImpl` use. *)
let control_escape (code : int) : string option =
  match code with
  | 7 -> Some "\\a"
  | 8 -> Some "\\b"
  | 12 -> Some "\\f"
  | 10 -> Some "\\n"
  | 13 -> Some "\\r"
  | 9 -> Some "\\t"
  | 11 -> Some "\\v"
  | _ -> None

(* `showCharImpl`: a single-quoted, escaped character (Char is its code point). *)
let show_char (cp : int) : string =
  if cp < 0x20 || cp = 0x7F
  then (
    match control_escape cp with
    | Some e -> "'" ^ e ^ "'"
    | None -> "'\\" ^ string_of_int cp ^ "'")
  else if cp = Char.code '\'' || cp = Char.code '\\'
  then "'\\" ^ utf8_of_cp cp ^ "'"
  else "'" ^ utf8_of_cp cp ^ "'"

(* `showStringImpl`: a double-quoted, escaped string. Bytes are processed directly
   — every escaped byte is < 0x80, so UTF-8 multi-byte sequences pass through
   untouched. A numeric escape followed by a digit gets a `\&` gap, as in the JS
   FFI, so `"\1" <> "2"` shows as `"\12\&3"`-style without ambiguity. *)
let show_string (s : string) : string =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  let n = String.length s in
  for i = 0 to n - 1 do
    let c = s.[i] in
    let code = Char.code c in
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | _ ->
      (match control_escape code with
       | Some e -> Buffer.add_string buf e
       | None ->
         if code < 0x20 || code = 0x7F
         then (
           Buffer.add_string buf ("\\" ^ string_of_int code);
           if i + 1 < n && s.[i + 1] >= '0' && s.[i + 1] <= '9'
           then Buffer.add_string buf "\\&")
         else Buffer.add_char buf c)
  done;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* `showNumberImpl`: the shortest round-tripping decimal, with the prelude's
   `"x.0"` rule for integral values (JS `n.toString()` then `isNaN(str + ".0")`).
   The host's native float printing is not JS's, so we find the shortest `%g` that
   round-trips and append `.0` only when the result stays numeric — matching the JS
   FFI for finite values (ADR-0022's fidelity note). *)
let show_number (f : float) : string =
  if Float.is_nan f
  then "NaN"
  else if f = Float.infinity
  then "Infinity"
  else if f = Float.neg_infinity
  then "-Infinity"
  else if f = 0.0 (* also catches -0.0, which JS prints as "0" *)
  then "0.0"
  else if Float.is_integer f && Float.abs f < 1e21
  then (* integral in fixed-notation range: print without exponent, add the ".0" *)
    Printf.sprintf "%.0f" f ^ ".0"
  else (
    (* fractional (or beyond the fixed-notation range): the shortest decimal that
       round-trips. `%g` only resorts to an exponent for genuinely extreme values,
       matching JS there; a fractional value never needs the ".0" suffix. *)
    let rec shortest p =
      if p > 17
      then Printf.sprintf "%.17g" f
      else (
        let s = Printf.sprintf "%.*g" p f in
        if float_of_string s = f then s else shortest (p + 1))
    in
    shortest 1)

(* ECMAScript `Math.round` (ADR-0042): round half **toward +inf** — NOT OCaml's
   [Float.round] (half away from zero), which differs on negative halves
   (`Math.round (-2.5) = -2`, not `-3`). `NaN`/`±inf` are preserved, and an input in
   `[-0.5, 0)` (or `-0`) yields `-0`, per ECMAScript. *)

(** The host registry (ADR-0022): the native foreign leaves, each an arity and a
    first-order host implementation over evaluated values. The pure leaves
    (`Data.Show`) are faithful to the `prelude` JS FFI; the one effectful leaf
    (`Effect.Console.log`, ADR-0023) performs real IO when its `Effect` thunk is
    forced. This is the single source of truth for which names are native —
    [native_provider] derives from it. *)
let js_round (f : float) : float =
  if not (Stdlib.Float.is_finite f)
  then f
  else (
    let r = Stdlib.Float.floor (f +. 0.5) in
    if r = 0.0 && Stdlib.Float.sign_bit f then -0.0 else r)

(* Native IO for the purvasm-native CLI interpreter (ADR-0045). Stdlib file ops; the effect
   happens when the returned `Effect` thunk is forced (ADR-0023), as for `Console.log`. *)
let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let write_file (path : string) (contents : string) : unit =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

(* mkdir -p: create [d] and any missing parents (idempotent). *)
let rec mkdir_p (d : string) : unit =
  if d = "" || d = "." || d = "/" || Sys.file_exists d
  then ()
  else (
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o755 with
    | _ -> ())

let host : Cesk.Machine.host =
  let unary (f : V.t -> V.t) =
    ( 1
    , fun args ->
        match args with
        | [ a ] -> f a
        | _ -> Cesk.Errors.stuck "native foreign: wrong arity" )
  in
  (* An effectful leaf returns this `Effect` thunk; forcing it (applying unit) runs [f] (ADR-0023). *)
  let perform name f =
    V.VForeign { name; arity = 1; args = []; call = (fun _ -> f ()) }
  in
  fun key ->
    match key with
    | "Data.Show.showIntImpl" ->
      Some
        (unary (function
           | V.VInt n -> V.VString (string_of_int n)
           | _ -> Cesk.Errors.stuck "showIntImpl: not an Int"))
    | "Data.Show.showNumberImpl" ->
      Some
        (unary (function
           | V.VNumber f -> V.VString (show_number f)
           | _ -> Cesk.Errors.stuck "showNumberImpl: not a Number"))
    | "Data.Show.showCharImpl" ->
      Some
        (unary (function
           | V.VInt cp -> V.VString (show_char cp)
           | _ -> Cesk.Errors.stuck "showCharImpl: not a Char"))
    | "Data.Show.showStringImpl" ->
      Some
        (unary (function
           | V.VString s -> V.VString (show_string s)
           | _ -> Cesk.Errors.stuck "showStringImpl: not a String"))
    (* The one genuinely effectful leaf (ADR-0023): `log s :: Effect Unit` is the
       thunk `\_ -> console.log s`. Applying [log] to the string returns that
       thunk; forcing it (running the effect) performs the real stdout write — so
       the side effect happens at the `Perform`, not at construction. *)
    | "Effect.Console.log" ->
      Some
        (unary (function
           | V.VString s ->
             V.VForeign
               { name = "Effect.Console.log#perform"
               ; arity = 1
               ; args = []
               ; call =
                   (fun _ ->
                     print_string s;
                     print_newline ();
                     flush stdout;
                     V.VInt 0)
               }
           | _ -> Cesk.Errors.stuck "log: not a String"))
    (* `_crashWith msg :: a` is a *pure* partial crash (not an `Effect`): forcing it
       halts with the message, mirroring the prelude JS `throw new Error(msg)`. So it
       crashes when the leaf is saturated, like the pure `Data.Show` leaves — not via a
       deferred `Effect` thunk. *)
    | "Partial._crashWith" ->
      Some
        (unary (function
           | V.VString msg -> Cesk.Errors.stuck ("Partial.crashWith: " ^ msg)
           | _ -> Cesk.Errors.stuck "crashWith: not a String"))
    (* `Data.Number` math family as native leaves (ADR-0042): pure libm/`Math.*` ops,
       JS-faithful (`round` half-toward-+inf). Added on demand — the closure reaches
       these via the pure-PureScript MD5. *)
    | "Data.Number.abs" ->
      Some
        (unary (function
           | V.VNumber f -> V.VNumber (Stdlib.Float.abs f)
           | _ -> Cesk.Errors.stuck "abs: not a Number"))
    | "Data.Number.floor" ->
      Some
        (unary (function
           | V.VNumber f -> V.VNumber (Stdlib.Float.floor f)
           | _ -> Cesk.Errors.stuck "floor: not a Number"))
    | "Data.Number.ceil" ->
      Some
        (unary (function
           | V.VNumber f -> V.VNumber (Stdlib.Float.ceil f)
           | _ -> Cesk.Errors.stuck "ceil: not a Number"))
    | "Data.Number.round" ->
      Some
        (unary (function
           | V.VNumber f -> V.VNumber (js_round f)
           | _ -> Cesk.Errors.stuck "round: not a Number"))
    | "Data.Number.sin" ->
      Some
        (unary (function
           | V.VNumber f -> V.VNumber (Stdlib.Float.sin f)
           | _ -> Cesk.Errors.stuck "sin: not a Number"))
    | "Data.Number.isFinite" ->
      Some
        (unary (function
           | V.VNumber f -> V.VBool (Stdlib.Float.is_finite f)
           | _ -> Cesk.Errors.stuck "isFinite: not a Number"))
    | "Data.Number.isNaN" ->
      Some
        (unary (function
           | V.VNumber f -> V.VBool (Stdlib.Float.is_nan f)
           | _ -> Cesk.Errors.stuck "isNaN: not a Number"))
    (* `Data.Number.parseFloatImpl :: String -> Number` (ADR-0046): the first-order parse engine
       behind the structural `fromStringImpl`. A parse failure yields `NaN`; the guest term turns
       that into `Nothing` via `isFinite`, so this leaf never needs to build a `Maybe`. *)
    | "Data.Number.parseFloatImpl" ->
      Some
        (unary (function
           | V.VString s ->
             V.VNumber
               (try Stdlib.float_of_string s with
                | _ -> Stdlib.Float.nan)
           | _ -> Cesk.Errors.stuck "parseFloatImpl: not a String"))
    (* `purvasm-base` `Purvasm.String` byte primitives (ADR-0038): pure UTF-8 byte ops. In the
       native backend these also live in the `Rt` prelude (a codegen fallback for unbound refs);
       the VM / oracle need them in the host registry to resolve them through the foreign rung. *)
    | "Purvasm.String.byteLength" ->
      Some
        (unary (function
           | V.VString s -> V.VInt (String.length s)
           | _ -> Cesk.Errors.stuck "byteLength: not a String"))
    | "Purvasm.String.byteAt" ->
      Some
        ( 2
        , fun args ->
            match args with
            | [ V.VString s; V.VInt i ] -> V.VInt (Char.code (String.get s i))
            | _ -> Cesk.Errors.stuck "byteAt: ill-typed arguments" )
    | "Purvasm.String.unsafeNew" ->
      Some
        (unary (function
           | V.VInt n -> V.VString (String.make n '\000')
           | _ -> Cesk.Errors.stuck "unsafeNew: not an Int"))
    | "Purvasm.String.unsafeSetByte" ->
      Some
        ( 3
        , fun args ->
            match args with
            | [ V.VString s; V.VInt i; V.VInt b ] ->
              let bs = Bytes.of_string s in
              Bytes.set bs i (Char.chr (b land 0xff));
              V.VString (Bytes.to_string bs)
            | _ -> Cesk.Errors.stuck "unsafeSetByte: ill-typed arguments" )
    (* Native IO leaves for the purvasm-native CLI interpreter (ADR-0045): each returns an
       `Effect` thunk that performs the IO when forced (the `Console.log` shape). *)
    | "Purvasm.CLI.Native.readTextImpl" ->
      Some
        (unary (function
           | V.VString path ->
             perform "readTextImpl#perform" (fun () -> V.VString (read_file path))
           | _ -> Cesk.Errors.stuck "readTextImpl: not a String"))
    | "Purvasm.CLI.Native.existsImpl" ->
      Some
        (unary (function
           | V.VString path ->
             perform "existsImpl#perform" (fun () -> V.VBool (Sys.file_exists path))
           | _ -> Cesk.Errors.stuck "existsImpl: not a String"))
    | "Purvasm.CLI.Native.writeTextImpl" ->
      Some
        ( 2
        , fun args ->
            match args with
            | [ V.VString path; V.VString contents ] ->
              perform "writeTextImpl#perform" (fun () ->
                write_file path contents;
                V.VInt 0)
            | _ -> Cesk.Errors.stuck "writeTextImpl: ill-typed arguments" )
    | "Purvasm.CLI.Native.mkdirRecImpl" ->
      Some
        (unary (function
           | V.VString path ->
             perform "mkdirRecImpl#perform" (fun () ->
               mkdir_p path;
               V.VInt 0)
           | _ -> Cesk.Errors.stuck "mkdirRecImpl: not a String"))
    | "Purvasm.CLI.Native.argvImpl" ->
      (* `argvImpl :: Effect (Array String)` is itself the thunk: forcing it (applying unit)
         reads `Sys.argv` (element 0 is the executable, as on the native target). *)
      Some (1, fun _ -> V.VArray (Array.map (fun s -> V.VString s) Sys.argv))
    | "Effect.Console.error" ->
      Some
        (unary (function
           | V.VString s ->
             perform "Effect.Console.error#perform" (fun () ->
               prerr_string s;
               prerr_newline ();
               flush stderr;
               V.VInt 0)
           | _ -> Cesk.Errors.stuck "error: not a String"))
    | _ -> None

(** Whether applying a native leaf yields an *effectful* value — one whose force (its
    `Effect` thunk run to saturation) performs an observable effect (ADR-0034's leaf
    bits). Only `Effect.Console.log` does real IO; the `Data.Show` leaves are pure.
    This is the source of truth the effect analysis ([Middle_end.Effect_analysis])
    consumes for foreign leaves; structural mutation (`Ref` → `NewArray`/`SetArray`)
    is recognised by the analysis from the primops themselves. *)
let effectful (key : string) : bool =
  List.mem
    key
    [ "Effect.Console.log"
    ; "Effect.Console.error"
    ; "Purvasm.CLI.Native.readTextImpl"
    ; "Purvasm.CLI.Native.existsImpl"
    ; "Purvasm.CLI.Native.writeTextImpl"
    ; "Purvasm.CLI.Native.mkdirRecImpl"
    ; "Purvasm.CLI.Native.argvImpl"
    ]

(** A native leaf's declared arity (the [host] entry's). The effect analysis needs the
    real arity, not an assumed 1, to tell a partial application (a pure build) from a
    saturated call: a multi-argument effectful leaf such as a future [runEffectFnN]
    ([EffectFnN -> ... -> Effect c], arity N+1) returns its `Effect` thunk only when
    fully applied, so assuming arity 1 would mis-place the effect. Unknown keys default
    to 1 (a truly unbound foreign is handled conservatively in the analysis). *)
let foreign_arity (key : string) : int =
  match host key with
  | Some (ar, _) -> ar
  | None -> 1

(** The native rung: a name is bound to an opaque [Foreign] reference exactly when
    the host registry implements it, so the two never drift apart. *)
let native_provider : provider = fun key -> Option.map (fun _ -> C.Foreign key) (host key)

(** The provider ladder, in priority order. The user-foreign rung is a later
    provider appended here, with linking unchanged. *)
let ladder : provider list = [ intrinsic; structural_provider; native_provider ]

(** The resolver [Link] consumes: the first provider to bind the key wins;
    otherwise the key is left unbound (ADR-0016). *)
let resolver (key : string) : C.term option = List.find_map (fun p -> p key) ladder
