(** Dead-binding elimination (ADR-0034), consuming the structural effect analysis
    ([Effect_analysis]). A [let x = c in body] is dropped when [c] cannot perform an
    effect (the analysis' [eperf]) and [x] is unused in [body]; a recursive group is
    dropped when no member is used and none performs while building.

    This preserves *partial correctness*, not full observational equivalence: a dead,
    pure, but diverging/throwing binding may be removed (ADR-0034's accepted relaxation
    — "hang/crash" may become "complete"). It never drops anything that may perform, so
    the `Effect` order and values of a terminating, non-crashing run are preserved.

    [effectful_leaf]/[foreign_arity] are the native-leaf classification ([Ffi]). *)

let run ~(effectful_leaf : string -> bool) ~(foreign_arity : string -> int) (program : Anf.expr)
  : Anf.expr =
  (Effect_analysis.create ~effectful_leaf ~foreign_arity).dbe program
