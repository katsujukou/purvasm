(** CoreFn expressions, binders, and binding groups (ADR-0014), transcribed
    faithfully from the verified [CoreFn.purs] reference. Constructor and field
    names follow the reference; the lowering to [Cesk.Ast] is ADR-0015. *)

type expr =
  | Literal of Ann.t * expr Literal.t
  | Constructor of Ann.t * Names.proper_name * Names.proper_name * Names.ident list
  (** [Constructor ann typeName constructorName fieldNames]; its arity is the
          length of [fieldNames]. *)
  | Accessor of Ann.t * string * expr (** [Accessor ann fieldName record]. *)
  | ObjectUpdate of Ann.t * expr * string list option * (string * expr) list
  (** [ObjectUpdate ann record copyFields updates]; [copyFields] is the
          untouched labels of a closed-record update ([Some]) or absent for an
          open-row one. *)
  | Abs of Ann.t * Names.ident * expr
  | App of Ann.t * expr * expr
  | Var of Ann.t * Names.ident Names.qualified
  | Case of Ann.t * expr list * case_alternative list
  | Let of Ann.t * bind list * expr

and bind =
  | NonRec of Ann.t * Names.ident * expr
  | Rec of rec_binding list

and rec_binding =
  { ann : Ann.t
  ; ident : Names.ident
  ; expr : expr
  }

and guard =
  { guard : expr
  ; expression : expr
  }

(** A case alternative: a row of binders and either guarded results ([Left]) or a
    single unconditional result ([Right]) — maps onto ADR-0013's rhs at lowering. *)
and case_alternative =
  { binders : binder list
  ; result : (guard list, expr) Either.t
  }

and binder =
  | NullBinder of Ann.t
  | LiteralBinder of Ann.t * binder Literal.t
  | VarBinder of Ann.t * Names.ident
  | ConstructorBinder of
      Ann.t
      * Names.proper_name Names.qualified
      * Names.proper_name Names.qualified
      * binder list
  | NamedBinder of Ann.t * Names.ident * binder
