(** Decode [corefn.json] (as emitted by [purs compile --codegen corefn]) into the
    faithful [Corefn] AST (ADR-0014). This module is the validating boundary:
    untyped JSON in, a well-typed [Module.t] out, or [Decode_error] on anything
    malformed or outside the supported shape — failing loudly at decode time
    rather than letting a bad node surface later. Deliberately stdlib-only (no
    Base) so [Either] keeps its [Left]/[Right] spelling, matching the reference. *)

exception Decode_error of string

let fail (msg : string) : 'a = raise (Decode_error msg)

(* --- generic helpers over Yojson.Safe.t --------------------------------- *)

let obj : Yojson.Safe.t -> (string * Yojson.Safe.t) list = function
  | `Assoc kvs -> kvs
  | _ -> fail "expected a JSON object"

let arr : Yojson.Safe.t -> Yojson.Safe.t list = function
  | `List xs -> xs
  | _ -> fail "expected a JSON array"

let str : Yojson.Safe.t -> string = function
  | `String s -> s
  | _ -> fail "expected a JSON string"

let int_ : Yojson.Safe.t -> int = function
  | `Int n -> n
  | `Intlit s -> int_of_string s
  | _ -> fail "expected a JSON integer"

let number : Yojson.Safe.t -> float = function
  | `Float f -> f
  | `Int n -> float_of_int n
  | `Intlit s -> float_of_string s
  | _ -> fail "expected a JSON number"

let bool_ : Yojson.Safe.t -> bool = function
  | `Bool b -> b
  | _ -> fail "expected a JSON boolean"

let field (kvs : (string * Yojson.Safe.t) list) (k : string) : Yojson.Safe.t =
  match List.assoc_opt k kvs with
  | Some v -> v
  | None -> fail ("missing field: " ^ k)

(** A field that may be absent or JSON null. *)
let field_opt (kvs : (string * Yojson.Safe.t) list) (k : string) : Yojson.Safe.t option =
  match List.assoc_opt k kvs with
  | None | Some `Null -> None
  | some -> some

(** A [[label, value]] pair as JSON emits for object literals and updates. *)
let labelled (decode : Yojson.Safe.t -> 'a) (j : Yojson.Safe.t) : string * 'a =
  match arr j with
  | [ k; v ] -> str k, decode v
  | _ -> fail "expected a [label, value] pair"

(* --- names --------------------------------------------------------------- *)

let module_name (j : Yojson.Safe.t) : Names.module_name = List.map str (arr j)

(** A [Qualified] from [{ identifier, moduleName? }] (the [sourcePos] is dropped). *)
let qualified (j : Yojson.Safe.t) : Names.ident Names.qualified =
  let kvs = obj j in
  let modname = Option.map module_name (field_opt kvs "moduleName") in
  Names.Qualified (modname, str (field kvs "identifier"))

(* --- annotation ---------------------------------------------------------- *)

let source_pos (j : Yojson.Safe.t) : Ann.source_pos =
  match arr j with
  | [ line; column ] -> { Ann.line = int_ line; column = int_ column }
  | _ -> fail "expected a [line, column] source position"

let source_span (j : Yojson.Safe.t) : Ann.source_span =
  let kvs = obj j in
  { Ann.start = source_pos (field kvs "start"); end_ = source_pos (field kvs "end") }

let constructor_type (j : Yojson.Safe.t) : Ann.constructor_type =
  match str j with
  | "ProductType" -> Ann.ProductType
  | "SumType" -> Ann.SumType
  | s -> fail ("unknown constructorType: " ^ s)

let meta (j : Yojson.Safe.t) : Ann.meta =
  let kvs = obj j in
  match str (field kvs "metaType") with
  | "IsConstructor" ->
    Ann.IsConstructor
      ( constructor_type (field kvs "constructorType")
      , List.map str (arr (field kvs "identifiers")) )
  | "IsNewtype" -> Ann.IsNewtype
  | "IsTypeClassConstructor" -> Ann.IsTypeClassConstructor
  | "IsForeign" -> Ann.IsForeign
  | "IsWhere" -> Ann.IsWhere
  | "IsSyntheticApp" -> Ann.IsSyntheticApp
  | s -> fail ("unknown metaType: " ^ s)

let ann (j : Yojson.Safe.t) : Ann.t =
  let kvs = obj j in
  { Ann.span = source_span (field kvs "sourceSpan")
  ; meta = Option.map meta (field_opt kvs "meta")
  }

(** Read the [annotation] field off a node. *)
let node_ann (kvs : (string * Yojson.Safe.t) list) : Ann.t = ann (field kvs "annotation")

(* --- literals ------------------------------------------------------------ *)

(** The first (sole) Unicode code point of a CoreFn [CharLiteral] string. *)
let code_point (s : string) : int =
  Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))

let literal (sub : Yojson.Safe.t -> 'a) (j : Yojson.Safe.t) : 'a Literal.t =
  let kvs = obj j in
  let value () = field kvs "value" in
  match str (field kvs "literalType") with
  | "IntLiteral" -> Literal.LitInt (int_ (value ()))
  | "NumberLiteral" -> Literal.LitNumber (number (value ()))
  | "StringLiteral" -> Literal.LitString (str (value ()))
  | "CharLiteral" -> Literal.LitChar (code_point (str (value ())))
  | "BooleanLiteral" -> Literal.LitBoolean (bool_ (value ()))
  | "ArrayLiteral" -> Literal.LitArray (List.map sub (arr (value ())))
  | "ObjectLiteral" -> Literal.LitObject (List.map (labelled sub) (arr (value ())))
  | s -> fail ("unknown literalType: " ^ s)

(* --- expressions, binders, binds ----------------------------------------- *)

let rec expr (j : Yojson.Safe.t) : Expr.expr =
  let kvs = obj j in
  let a = node_ann kvs in
  match str (field kvs "type") with
  | "Literal" -> Expr.Literal (a, literal expr (field kvs "value"))
  | "Constructor" ->
    Expr.Constructor
      ( a
      , str (field kvs "typeName")
      , str (field kvs "constructorName")
      , List.map str (arr (field kvs "fieldNames")) )
  | "Accessor" ->
    Expr.Accessor (a, str (field kvs "fieldName"), expr (field kvs "expression"))
  | "ObjectUpdate" ->
    let copy = Option.map (fun c -> List.map str (arr c)) (field_opt kvs "copy") in
    Expr.ObjectUpdate
      ( a
      , expr (field kvs "expression")
      , copy
      , List.map (labelled expr) (arr (field kvs "updates")) )
  | "Abs" -> Expr.Abs (a, str (field kvs "argument"), expr (field kvs "body"))
  | "App" -> Expr.App (a, expr (field kvs "abstraction"), expr (field kvs "argument"))
  | "Var" -> Expr.Var (a, qualified (field kvs "value"))
  | "Case" ->
    Expr.Case
      ( a
      , List.map expr (arr (field kvs "caseExpressions"))
      , List.map case_alternative (arr (field kvs "caseAlternatives")) )
  | "Let" ->
    Expr.Let (a, List.map bind (arr (field kvs "binds")), expr (field kvs "expression"))
  | s -> fail ("unknown expression type: " ^ s)

and bind (j : Yojson.Safe.t) : Expr.bind =
  let kvs = obj j in
  match str (field kvs "bindType") with
  | "NonRec" ->
    Expr.NonRec (node_ann kvs, str (field kvs "identifier"), expr (field kvs "expression"))
  | "Rec" -> Expr.Rec (List.map rec_binding (arr (field kvs "binds")))
  | s -> fail ("unknown bindType: " ^ s)

and rec_binding (j : Yojson.Safe.t) : Expr.rec_binding =
  let kvs = obj j in
  { Expr.ann = node_ann kvs
  ; ident = str (field kvs "identifier")
  ; expr = expr (field kvs "expression")
  }

and case_alternative (j : Yojson.Safe.t) : Expr.case_alternative =
  let kvs = obj j in
  let binders = List.map binder (arr (field kvs "binders")) in
  let result =
    if bool_ (field kvs "isGuarded")
    then Either.Left (List.map guard (arr (field kvs "expressions")))
    else Either.Right (expr (field kvs "expression"))
  in
  { Expr.binders; result }

and guard (j : Yojson.Safe.t) : Expr.guard =
  let kvs = obj j in
  { Expr.guard = expr (field kvs "guard"); expression = expr (field kvs "expression") }

and binder (j : Yojson.Safe.t) : Expr.binder =
  let kvs = obj j in
  let a = node_ann kvs in
  match str (field kvs "binderType") with
  | "NullBinder" -> Expr.NullBinder a
  | "LiteralBinder" -> Expr.LiteralBinder (a, literal binder (field kvs "literal"))
  | "VarBinder" -> Expr.VarBinder (a, str (field kvs "identifier"))
  | "ConstructorBinder" ->
    Expr.ConstructorBinder
      ( a
      , qualified (field kvs "typeName")
      , qualified (field kvs "constructorName")
      , List.map binder (arr (field kvs "binders")) )
  | "NamedBinder" ->
    Expr.NamedBinder (a, str (field kvs "identifier"), binder (field kvs "binder"))
  | s -> fail ("unknown binderType: " ^ s)

(* --- module -------------------------------------------------------------- *)

let import (j : Yojson.Safe.t) : Module.import =
  let kvs = obj j in
  { Module.ann = node_ann kvs; module_name = module_name (field kvs "moduleName") }

let re_exports (j : Yojson.Safe.t) : (string * Names.ident list) list =
  List.map (fun (k, v) -> k, List.map str (arr v)) (obj j)

let module_ (j : Yojson.Safe.t) : Module.t =
  let kvs = obj j in
  { Module.name = module_name (field kvs "moduleName")
  ; path = str (field kvs "modulePath")
  ; built_with = str (field kvs "builtWith")
  ; imports = List.map import (arr (field kvs "imports"))
  ; exports = List.map str (arr (field kvs "exports"))
  ; re_exports = re_exports (field kvs "reExports")
  ; foreign_names = List.map str (arr (field kvs "foreign"))
  ; decls = List.map bind (arr (field kvs "decls"))
  }

let module_of_string (s : string) : Module.t = module_ (Yojson.Safe.from_string s)
let module_of_file (path : string) : Module.t = module_ (Yojson.Safe.from_file path)
