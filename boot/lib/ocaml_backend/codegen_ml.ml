(** ANF → OCaml source codegen (ADR-0036), boot's native backend (ADR-0035).

    Slice 1: the *pure first-order* subset with the **uniform** calling convention
    (option 1 — every function is a [VClos], every application goes through [app]).
    Correctness baseline; the hybrid known-arity direct calls (option 2, the decided
    target) and foreigns/Effect are later slices.

    A whole ANF spine becomes one self-contained `.ml`: a fixed runtime prelude [rt]
    (the [value] type, [app], primitives, [to_string] mirroring VM `Value.to_string`),
    the program as a nested `let … in`, and a runner that prints the entry value. The
    output is held to the oracle/VM differential. *)

module A = Middle_end.Anf
module C = Cesk.Ast
module SS = Set.Make (String)

(* The runtime-support module, emitted verbatim as the program's prelude. Kept in a
   raw string (no escaping). It must not contain the sequence that closes the literal. *)
let rt =
  {rt|
module SMap = Map.Make (String)

type value =
  | VInt of int
  | VNumber of float
  | VBool of bool
  | VString of string
  | VArray of value array
  | VRecord of value SMap.t
  | VData of string * value array
  | VCtor of string * int * value list
  | VClos of (value -> value)

let stuck msg = failwith ("purvasm: " ^ msg)

(* Uniform application (slice 1): a closure rides OCaml's currying; a partial
   constructor accumulates until saturated, then becomes VData. *)
let app f x =
  match f with
  | VClos g -> g x
  | VCtor (tag, arity, args) ->
    let args = args @ [ x ] in
    if List.length args >= arity then VData (tag, Array.of_list args) else VCtor (tag, arity, args)
  | _ -> stuck "apply: not a function"

let as_int = function VInt n -> n | _ -> stuck "expected Int"
let as_num = function VNumber n -> n | _ -> stuck "expected Number"
let as_bool = function VBool b -> b | _ -> stuck "expected Bool"
let as_str = function VString s -> s | _ -> stuck "expected String"

(* Primitives (mirror Cesk.Prim / VM): int div/mod by zero yield 0 (ADR semantics). *)
let p_add_int a b = VInt (as_int a + as_int b)
let p_sub_int a b = VInt (as_int a - as_int b)
let p_mul_int a b = VInt (as_int a * as_int b)
let p_div_int a b = let d = as_int b in VInt (if d = 0 then 0 else as_int a / d)
let p_mod_int a b = let d = as_int b in VInt (if d = 0 then 0 else as_int a mod d)
let p_add_num a b = VNumber (as_num a +. as_num b)
let p_sub_num a b = VNumber (as_num a -. as_num b)
let p_mul_num a b = VNumber (as_num a *. as_num b)
let p_div_num a b = VNumber (as_num a /. as_num b)
let p_eq_int a b = VBool (as_int a = as_int b)
let p_eq_num a b = VBool (as_num a = as_num b)
let p_eq_str a b = VBool (as_str a = as_str b)
let p_eq_bool a b = VBool (as_bool a = as_bool b)
let p_lt_int a b = VBool (as_int a < as_int b)
let p_lt_num a b = VBool (as_num a < as_num b)
let p_lt_str a b = VBool (as_str a < as_str b)
let p_and a b = VBool (as_bool a && as_bool b)
let p_or a b = VBool (as_bool a || as_bool b)
let p_not a = VBool (not (as_bool a))
let p_append a b =
  match a, b with
  | VString x, VString y -> VString (x ^ y)
  | VArray x, VArray y -> VArray (Array.append x y)
  | _ -> stuck "append: ill-typed"
let p_index a i = (match a with VArray ar -> let i = as_int i in
    if i >= 0 && i < Array.length ar then ar.(i) else stuck "index out of bounds" | _ -> stuck "index")
let p_length a = (match a with VArray ar -> VInt (Array.length ar) | _ -> stuck "length")
let p_new_array n fill = let n = as_int n in
    if n < 0 then stuck "newArray: negative length" else VArray (Array.make n fill)
let p_set_array a i v = (match a with VArray ar -> let i = as_int i in
    if i >= 0 && i < Array.length ar then (ar.(i) <- v; a) else stuck "set out of bounds" | _ -> stuck "set")

let accessor v l = match v with VRecord m -> SMap.find l m | _ -> stuck "accessor: not a record"
let update v ups =
  match v with
  | VRecord m -> VRecord (List.fold_left (fun m (l, x) -> SMap.add l x m) m ups)
  | _ -> stuck "update: not a record"

(* Mirrors VM Value.to_string byte-for-byte, so the differential printer is shared. *)
let rec to_string = function
  | VInt n -> string_of_int n
  | VNumber f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VArray a -> "[" ^ String.concat ", " (List.map to_string (Array.to_list a)) ^ "]"
  | VRecord m ->
    "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ ": " ^ to_string v) (SMap.bindings m)) ^ "}"
  | VData (tag, fields) ->
    if Array.length fields = 0 then tag
    else tag ^ "(" ^ String.concat ", " (List.map to_string (Array.to_list fields)) ^ ")"
  | VCtor (tag, arity, _) -> "<ctor " ^ tag ^ "/" ^ string_of_int arity ^ ">"
  | VClos _ -> "<closure>"
|rt}

(* --- name mangling: a PureScript/ANF name → a valid lowercase OCaml ident -------- *)

let mangle (name : string) : string =
  let buf = Buffer.create (String.length name + 2) in
  Buffer.add_string buf "v_";
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> Buffer.add_char buf c
      | '.' -> Buffer.add_string buf "_"
      | _ -> Buffer.add_string buf (Printf.sprintf "_x%02x_" (Char.code c)))
    name;
  Buffer.contents buf

let ocaml_string (s : string) : string = "\"" ^ String.concat "" (List.map (fun c ->
    match c with '"' -> "\\\"" | '\\' -> "\\\\" | '\n' -> "\\n" | c -> String.make 1 c)
    (List.init (String.length s) (String.get s))) ^ "\""

(* --- emit -------------------------------------------------------------------- *)

let lit : C.lit -> string = function
  | C.LInt n -> Printf.sprintf "(VInt (%d))" n
  | C.LNumber f -> Printf.sprintf "(VNumber (%h))" f
  | C.LBool b -> Printf.sprintf "(VBool %b)" b
  | C.LString s -> Printf.sprintf "(VString %s)" (ocaml_string s)

(* [lz] is the set of in-scope names bound *lazily* (value members of a recursive group,
   ADR-0024) — a reference to one must be [Lazy.force]d to a [value]. Names are unique
   (purs's Renamer), so no shadow removal is needed. *)
let atom (lz : SS.t) : A.atom -> string = function
  | A.AVar x -> if SS.mem x lz then Printf.sprintf "(Lazy.force %s)" (mangle x) else mangle x
  | A.ALit l -> lit l
  | A.AForeign k -> failwith ("codegen_ml: foreign not supported in slice 1: " ^ k)

let prim_fn : C.primop -> string = function
  | C.AddInt -> "p_add_int" | C.SubInt -> "p_sub_int" | C.MulInt -> "p_mul_int"
  | C.DivInt -> "p_div_int" | C.ModInt -> "p_mod_int"
  | C.AddNumber -> "p_add_num" | C.SubNumber -> "p_sub_num" | C.MulNumber -> "p_mul_num"
  | C.DivNumber -> "p_div_num"
  | C.EqInt -> "p_eq_int" | C.EqNumber -> "p_eq_num" | C.EqString -> "p_eq_str"
  | C.EqBool -> "p_eq_bool"
  | C.LtInt -> "p_lt_int" | C.LtNumber -> "p_lt_num" | C.LtString -> "p_lt_str"
  | C.AndBool -> "p_and" | C.OrBool -> "p_or" | C.NotBool -> "p_not"
  | C.Append -> "p_append"
  | C.IndexArray -> "p_index" | C.LengthArray -> "p_length"
  | C.NewArray -> "p_new_array" | C.SetArray -> "p_set_array"

let prim (lz : SS.t) (op : C.primop) (args : A.atom list) : string =
  Printf.sprintf "(%s %s)" (prim_fn op)
    (String.concat " " (List.map (fun a -> "(" ^ atom lz a ^ ")") args))

(* A recursive-group member is emitted as a plain `let rec` binding when it is a
   function (the knot ties through the closure), else as a `lazy` cell forced on use
   (ADR-0024). Function = a (let-wrapped) lambda RHS. *)
let rec is_fun (e : A.expr) : bool =
  match e with Ret (CLam _) -> true | Let (_, _, b) -> is_fun b | LetRec (_, b) -> is_fun b | _ -> false

let fresh =
  let c = ref 0 in
  fun (p : string) -> incr c; Printf.sprintf "%s%d" p !c

(* The matcher, emitted in continuation-passing style: code that matches binder [b]
   against the value-expression [scrut], runs [succ] on success and [fail] on failure.
   This handles every binder kind — records via `SMap.find`, number/string literals via
   structural equality — which OCaml patterns alone cannot. (A naive linear cascade; a
   decision-tree lowering is a later perf slice, cf. ADR-0036.) *)
let rec emit_match (b : C.binder) (scrut : string) (succ : string) (fail : string) : string =
  match b with
  | C.BNull -> succ
  | C.BVar x -> Printf.sprintf "(let %s = %s in %s)" (mangle x) scrut succ
  | C.BNamed (x, inner) ->
    Printf.sprintf "(let %s = %s in %s)" (mangle x) scrut (emit_match inner scrut succ fail)
  | C.BLit l -> Printf.sprintf "(if %s = %s then %s else %s)" scrut (lit l) succ fail
  | C.BCtor (tag, subs) ->
    let f = fresh "_f" in
    let rec nest i = function
      | [] -> succ
      | s :: r -> emit_match s (Printf.sprintf "%s.(%d)" f i) (nest (i + 1) r) fail
    in
    Printf.sprintf "(match %s with VData (%s, %s) when Array.length %s = %d -> %s | _ -> %s)" scrut
      (ocaml_string tag) f f (List.length subs) (nest 0 subs) fail
  | C.BArray subs ->
    let f = fresh "_a" in
    let rec nest i = function
      | [] -> succ
      | s :: r -> emit_match s (Printf.sprintf "%s.(%d)" f i) (nest (i + 1) r) fail
    in
    Printf.sprintf "(match %s with VArray %s when Array.length %s = %d -> %s | _ -> %s)" scrut f f
      (List.length subs) (nest 0 subs) fail
  | C.BRecord fields ->
    let m = fresh "_m" in
    let rec nest = function
      | [] -> succ
      | (l, s) :: r -> emit_match s (Printf.sprintf "(SMap.find %s %s)" (ocaml_string l) m) (nest r) fail
    in
    Printf.sprintf "(match %s with VRecord %s -> %s | _ -> %s)" scrut m (nest fields) fail

let rec expr (lz : SS.t) (e : A.expr) : string =
  match e with
  | A.Ret c -> cexpr lz c
  | A.Let (x, c, body) -> Printf.sprintf "(let %s = %s in %s)" (mangle x) (cexpr lz c) (expr lz body)
  | A.LetRec (binds, body) ->
    (* value members become `lazy`; extend [lz] so references force them. *)
    let lz' = List.fold_left (fun s (x, rhs) -> if is_fun rhs then s else SS.add x s) lz binds in
    let one (x, rhs) =
      if is_fun rhs then Printf.sprintf "%s = %s" (mangle x) (expr lz' rhs)
      else Printf.sprintf "%s = lazy %s" (mangle x) (expr lz' rhs)
    in
    Printf.sprintf "(let rec %s in %s)" (String.concat " and " (List.map one binds)) (expr lz' body)

and cexpr (lz : SS.t) (c : A.cexpr) : string =
  let atom = atom lz in
  match c with
  | A.CAtom a -> atom a
  | A.CLam (ps, body) ->
    List.fold_right (fun p acc -> Printf.sprintf "(VClos (fun %s -> %s))" (mangle p) acc) ps (expr lz body)
  | A.CApp (f, args) ->
    List.fold_left (fun acc a -> Printf.sprintf "(app %s (%s))" acc (atom a)) (atom f) args
  | A.CPrim (op, args) -> prim lz op args
  | A.CCtor (tag, arity, args) ->
    if List.length args >= arity
    then Printf.sprintf "(VData (%s, [| %s |]))" (ocaml_string tag) (String.concat "; " (List.map atom args))
    else
      Printf.sprintf "(VCtor (%s, %d, [%s]))" (ocaml_string tag) arity
        (String.concat "; " (List.map atom args))
  | A.CArray xs -> Printf.sprintf "(VArray [| %s |])" (String.concat "; " (List.map atom xs))
  | A.CRecord fs ->
    Printf.sprintf "(VRecord (List.fold_left (fun m (k,v) -> SMap.add k v m) SMap.empty [%s]))"
      (String.concat "; " (List.map (fun (l, a) -> Printf.sprintf "(%s, (%s))" (ocaml_string l) (atom a)) fs))
  | A.CAccessor (a, l) -> Printf.sprintf "(accessor (%s) %s)" (atom a) (ocaml_string l)
  | A.CUpdate (a, ups) ->
    Printf.sprintf "(update (%s) [%s])" (atom a)
      (String.concat "; " (List.map (fun (l, x) -> Printf.sprintf "(%s, (%s))" (ocaml_string l) (atom x)) ups))
  | A.CIf (a, t, e) -> Printf.sprintf "(if as_bool (%s) then %s else %s)" (atom a) (expr lz t) (expr lz e)
  | A.CCase (scruts, alts) -> case lz scruts alts

(* case as a CPS cascade over the alternatives. Scrutinees are bound once; each alt is
   tried in order, its failure jumping to a shared thunk that runs the next alt (so the
   rest is not duplicated). Within an alt, the binders match left-to-right, then the
   body (or guards, falling through on all-false) runs. All binder kinds are handled
   uniformly via [emit_match]. *)
and case (lz : SS.t) (scruts : A.atom list) (alts : A.alt list) : string =
  let svars = List.map (fun _ -> fresh "_s") scruts in
  let lets =
    String.concat "" (List.map2 (fun v s -> Printf.sprintf "let %s = %s in " v (atom lz s)) svars scruts)
  in
  let alt_code (a : A.alt) (fail : string) : string =
    let succ =
      match a.result with
      | A.Uncond e -> expr lz e
      | A.Guarded gs ->
        List.fold_right
          (fun (g, e) acc -> Printf.sprintf "(if as_bool (%s) then %s else %s)" (expr lz g) (expr lz e) acc)
          gs fail
    in
    let rec nest bs vs =
      match bs, vs with
      | [], [] -> succ
      | b :: bs, v :: vs -> emit_match b v (nest bs vs) fail
      | _ -> failwith "codegen_ml: case binder/scrutinee arity mismatch"
    in
    nest a.binders svars
  in
  let rec build = function
    | [] -> "(stuck \"no match\")"
    | a :: rest ->
      let fk = fresh "_fk" in
      Printf.sprintf "(let %s () = %s in %s)" fk (build rest) (alt_code a (fk ^ " ()"))
  in
  Printf.sprintf "(%s%s)" lets (build alts)

(** Emit a whole ANF program as a self-contained OCaml source string that, when run,
    prints the entry value in `Value.to_string` form (pure entries; Effect later). *)
let program (e : A.expr) : string =
  Printf.sprintf "[@@@warning \"-a\"]\n%s\nlet result = %s\nlet () = print_string (to_string result)\n"
    rt (expr SS.empty e)
