(** A decoded CoreFn module (ADR-0014), the full record from the reference. Some
    fields (path, built_with, exports, re_exports) are retained for fidelity even
    though lowering may not use them yet. *)

type import =
  { ann : Ann.t
  ; module_name : Names.module_name
  }

type t =
  { name : Names.module_name
  ; path : string
  ; built_with : string
  ; imports : import list
  ; exports : Names.ident list
  ; re_exports : (string * Names.ident list) list
  ; foreign_names : Names.ident list
  ; decls : Expr.bind list
  }
