(** Per-node CoreFn annotation (ADR-0014). We keep the source span and the
    compiler [meta]; the JSON annotation's comments are intentionally dropped (not
    needed to run a program). [end_] spells CoreFn's [end] (a reserved word). *)

type source_pos =
  { line : int
  ; column : int
  }

type source_span =
  { start : source_pos
  ; end_ : source_pos
  }

type constructor_type =
  | ProductType
  | SumType

type meta =
  | IsConstructor of constructor_type * Names.ident list
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  | IsSyntheticApp

type t =
  { span : source_span
  ; meta : meta option
  }
