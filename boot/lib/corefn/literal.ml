(** CoreFn literals (ADR-0014), polymorphic in the sub-term so the same shapes
    serve both expression literals ([expr literal]) and pattern literals
    ([binder literal]). [LitChar] holds a Unicode code point; lowering folds Char
    into Int (ADR-0006). *)

type 'a t =
  | LitInt of int
  | LitNumber of float
  | LitString of string
  | LitChar of int
  | LitBoolean of bool
  | LitArray of 'a list
  | LitObject of (string * 'a) list
