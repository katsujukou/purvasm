(** CoreFn names, faithful to PureScript's [Language.PureScript.Names] via the
    verified [CoreFn.purs] reference (ADR-0014). A [Qualified] keeps only the
    optional module name; the binding-site [sourcePos] the JSON carries is
    dropped. *)

type module_name = string list
type ident = string
type proper_name = string
type 'a qualified = Qualified of module_name option * 'a
