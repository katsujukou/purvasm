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
  ]

(** The intrinsic rung as a provider. *)
let intrinsic : provider = fun key -> List.assoc_opt key intrinsics

(** The provider ladder, in priority order. Currently one rung; the syscall and
    user-foreign rungs are later providers appended here, with linking unchanged. *)
let ladder : provider list = [ intrinsic ]

(** The resolver [Link] consumes: the first provider to bind the key wins;
    otherwise the key is left unbound (ADR-0016). *)
let resolver (key : string) : C.term option = List.find_map (fun p -> p key) ladder
