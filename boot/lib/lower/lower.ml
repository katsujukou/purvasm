(** Lower the faithful [Corefn] AST to the machine's [Cesk.Ast] (ADR-0015). Most
    of this is a one-to-one structural map; the decisions live in name resolution
    (a flat key per [Var] origin), the module-to-term shaping (decls folded in
    CoreFn's dependency order so strict initialisation stays well-defined), and
    the type-erased drops (constructor type names/qualifiers, [copyFields],
    [Char] folded to [Int]). Lowering is total: an unresolved external or
    [foreign] reference becomes a [Var] under its qualified key, [stuck] only if
    forced at runtime, for a later linking slice to bind. Deliberately stdlib-only
    (no Base) so the decoded [Either] keeps its [Left]/[Right] spelling. *)

module C = Cesk.Ast
module E = Corefn.Expr
module L = Corefn.Literal
module N = Corefn.Names
module A = Corefn.Ann

(** The flat environment key for a module-qualified name, e.g.
    [Data.Maybe.fromMaybe]. Top-level bindings are bound under this key and
    module-qualified references resolve to it; locals use their bare ident, a
    distinct key shape, so a local never captures a qualified reference. *)
let qualified_key (m : N.module_name) (id : N.ident) : string =
  String.concat "." m ^ "." ^ id

let lower_var (q : N.ident N.qualified) : C.term =
  match q with
  | N.Qualified (None, id) -> C.Var id
  | N.Qualified (Some m, id) -> C.Var (qualified_key m id)

let rec expr (e : E.expr) : C.term =
  match e with
  | E.Literal (_, lit) -> lit_expr lit
  (* A newtype constructor is erased — it is the identity (ADR-0018). PureScript
     emits newtype ctors as identity *declarations*, so a `Constructor` node with
     this meta should not occur; handled defensively. *)
  | E.Constructor ({ A.meta = Some A.IsNewtype; _ }, _, _, _) -> C.Lam ("$x", C.Var "$x")
  (* Tag-only (ADR-0011): the type name and the constructor's module qualifier
     carry no runtime meaning, so only the constructor name and arity survive. *)
  | E.Constructor (_, _type_name, ctor, fields) -> C.Ctor (ctor, List.length fields)
  | E.Accessor (_, fieldName, r) -> C.Accessor (expr r, fieldName)
  (* The copyFields hint is a closed-vs-open optimisation our immutable update
     does not need (ADR-0010). *)
  | E.ObjectUpdate (_, r, _copy, updates) ->
    C.Update (expr r, List.map (fun (l, e) -> l, expr e) updates)
  | E.Abs (_, arg, body) -> C.Lam (arg, expr body)
  | E.App (_, f, x) -> C.App (expr f, expr x)
  | E.Var (_, q) -> lower_var q
  | E.Case (_, scrutinees, alts) ->
    C.Case (List.map expr scrutinees, List.map alternative alts)
  (* An expression-level let binds locals: bare keys. *)
  | E.Let (_, binds, body) -> lower_binds (fun id -> id) binds (expr body)

and lit_expr (lit : E.expr L.t) : C.term =
  match lit with
  | L.LitInt n -> C.Lit (C.LInt n)
  | L.LitNumber f -> C.Lit (C.LNumber f)
  | L.LitString s -> C.Lit (C.LString s)
  (* Char is Int — the code point (ADR-0006). *)
  | L.LitChar cp -> C.Lit (C.LInt cp)
  | L.LitBoolean b -> C.Lit (C.LBool b)
  | L.LitArray xs -> C.Array (List.map expr xs)
  | L.LitObject kvs -> C.Record (List.map (fun (l, e) -> l, expr e) kvs)

and alternative (a : E.case_alternative) : C.alternative =
  { C.binders = List.map binder a.binders
  ; result =
      (match a.result with
       | Either.Right e -> C.Unconditional (expr e)
       | Either.Left guards ->
         C.Guarded
           (List.map (fun (g : E.guard) -> expr g.guard, expr g.expression) guards))
  }

and binder (b : E.binder) : C.binder =
  match b with
  | E.NullBinder _ -> C.BNull
  | E.VarBinder (_, id) -> C.BVar id
  | E.LiteralBinder (_, lit) -> lit_binder lit
  (* A newtype constructor binder is transparent: the wrapper is erased, so it
     matches the value directly through its single inner binder (ADR-0018). This
     is what makes typeclass-dictionary dispatch (dicts are newtypes) work. *)
  | E.ConstructorBinder ({ A.meta = Some A.IsNewtype; _ }, _, _, [ sub ]) -> binder sub
  | E.ConstructorBinder (_, _type_name, N.Qualified (_, ctor), subs) ->
    C.BCtor (ctor, List.map binder subs)
  | E.NamedBinder (_, id, inner) -> C.BNamed (id, binder inner)

and lit_binder (lit : E.binder L.t) : C.binder =
  match lit with
  | L.LitInt n -> C.BLit (C.LInt n)
  | L.LitNumber f -> C.BLit (C.LNumber f)
  | L.LitString s -> C.BLit (C.LString s)
  | L.LitChar cp -> C.BLit (C.LInt cp)
  | L.LitBoolean b -> C.BLit (C.LBool b)
  | L.LitArray subs -> C.BArray (List.map binder subs)
  | L.LitObject kvs -> C.BRecord (List.map (fun (l, b) -> l, binder b) kvs)

(* Fold a binding group around a body, in CoreFn's order, [NonRec] -> [Let] and
   [Rec] -> [Letrec] (ADR-0004/0005). [key] maps a bound ident to its environment
   key (qualified for top-level decls, identity for an expression-level let). *)
and lower_binds (key : N.ident -> string) (binds : E.bind list) (body : C.term) : C.term =
  List.fold_right
    (fun (b : E.bind) acc ->
       match b with
       | E.NonRec (_, id, e) -> C.Let (key id, expr e, acc)
       | E.Rec rbs ->
         C.Letrec
           (List.map (fun (rb : E.rec_binding) -> key rb.ident, expr rb.expr) rbs, acc))
    binds
    body

(** Lower a whole module to a term that evaluates [entry] with all of the
    module's declarations in scope (each bound under its own-module qualified
    key). *)
let module_ (m : Corefn.Module.t) ~(entry : N.ident) : C.term =
  let key id = qualified_key m.name id in
  lower_binds key m.decls (C.Var (key entry))
