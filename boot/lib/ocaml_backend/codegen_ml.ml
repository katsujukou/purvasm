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
module IM = Map.Make (String) (* name → arity, for known-arity functions (hybrid calls) *)

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

(* Primitives (mirror Cesk.Prim / VM): int div/mod by zero yield 0 (ADR semantics).
   PureScript `Int` is signed 32-bit with wrapping, so wrap each int result. *)
let w32 n = Int32.to_int (Int32.of_int n)
let p_add_int a b = VInt (w32 (as_int a + as_int b))
let p_sub_int a b = VInt (w32 (as_int a - as_int b))
let p_mul_int a b = VInt (w32 (as_int a * as_int b))
let p_div_int a b = let d = as_int b in VInt (if d = 0 then 0 else w32 (as_int a / d))
let p_mod_int a b = let d = as_int b in VInt (if d = 0 then 0 else w32 (as_int a mod d))
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
let p_new_array n = let n = as_int n in
    if n < 0 then stuck "newArray: negative length" else VArray (Array.make n (VInt 0))
let p_set_array a i v = (match a with VArray ar -> let i = as_int i in
    if i >= 0 && i < Array.length ar then (ar.(i) <- v; a) else stuck "set out of bounds" | _ -> stuck "set")

(* Native foreign leaves, re-implemented over [value]. The generated program is a
   standalone executable and cannot link Cesk's host registry, so the leaves live here;
   their behaviour must match `Ffi.host` and the differential enforces it. Leaves are
   added on demand (the minimal-FFI policy). [log] returns the `#perform` thunk that
   does the IO when forced (ADR-0023). *)
(* Faithful copies of Cesk's Ffi show formatting (the differential enforces parity). *)
let control_escape = function
  | 7 -> Some "\\a" | 8 -> Some "\\b" | 12 -> Some "\\f" | 10 -> Some "\\n"
  | 13 -> Some "\\r" | 9 -> Some "\\t" | 11 -> Some "\\v" | _ -> None
let show_string_impl s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  let n = String.length s in
  for i = 0 to n - 1 do
    let c = s.[i] in
    let code = Char.code c in
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | _ ->
      (match control_escape code with
       | Some e -> Buffer.add_string buf e
       | None ->
         if code < 0x20 || code = 0x7F
         then (
           Buffer.add_string buf ("\\" ^ string_of_int code);
           if i + 1 < n && s.[i + 1] >= '0' && s.[i + 1] <= '9' then Buffer.add_string buf "\\&")
         else Buffer.add_char buf c)
  done;
  Buffer.add_char buf '"';
  Buffer.contents buf
let show_number_impl f =
  if Float.is_nan f then "NaN"
  else if f = Float.infinity then "Infinity"
  else if f = Float.neg_infinity then "-Infinity"
  else if f = 0.0 then "0.0"
  else if Float.is_integer f && Float.abs f < 1e21 then Printf.sprintf "%.0f" f ^ ".0"
  else (
    let rec shortest p =
      if p > 17 then Printf.sprintf "%.17g" f
      else (let s = Printf.sprintf "%.*g" p f in if float_of_string s = f then s else shortest (p + 1))
    in
    shortest 1)

let foreign = function
  | "Data.Show.showIntImpl" -> VClos (fun v -> VString (string_of_int (as_int v)))
  | "Data.Show.showStringImpl" -> VClos (fun v -> VString (show_string_impl (as_str v)))
  | "Data.Show.showNumberImpl" -> VClos (fun v -> VString (show_number_impl (as_num v)))
  | "Effect.Console.log" ->
    VClos (fun s -> VClos (fun _u -> print_string (as_str s); print_newline (); flush stdout; VInt 0))
  | k -> stuck ("unbound foreign: " ^ k)

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

(* The names a binder binds. *)
let rec binder_vars : C.binder -> string list = function
  | C.BNull | C.BLit _ -> []
  | C.BVar x -> [ x ]
  | C.BNamed (x, i) -> x :: binder_vars i
  | C.BCtor (_, subs) | C.BArray subs -> List.concat_map binder_vars subs
  | C.BRecord fs -> List.concat_map (fun (_, b) -> binder_vars b) fs

(* Every name bound anywhere in the program (top-level, λ, let, letrec, case binders).
   Names are unique (purs's Renamer + qualified top-level keys + ANF's fresh `$a…`), so
   "bound somewhere" = "in scope here" — enough to tell a local/known reference from an
   *unresolved foreign* (a qualified key the resolver never bound), which is then emitted
   as `(foreign k)` → a clear `stuck "unbound foreign: k"` instead of an `ocamlopt`
   "Unbound value". Set once per program in [program]. *)
let bound_names : SS.t ref = ref SS.empty

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
  | A.AVar x ->
    if SS.mem x !bound_names
    then if SS.mem x lz then Printf.sprintf "(Lazy.force %s)" (mangle x) else mangle x
    else Printf.sprintf "(foreign %s)" (ocaml_string x) (* an unresolved foreign import *)
  | A.ALit l -> lit l
  | A.AForeign k -> Printf.sprintf "(foreign %s)" (ocaml_string k)

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

(* Hybrid calling convention (ADR-0036, option 2). A binding whose RHS is *directly* a
   lambda is a known-arity function: it gets a native multi-arg definition [native_name]
   (for direct, uncurried calls) and a [VClos] wrapper under its own name (for use as a
   value). Anything else recursive is a `lazy` cell (ADR-0024). *)
let as_lam : A.expr -> (string list * A.expr) option = function
  | Ret (CLam (ps, b)) -> Some (ps, b)
  | _ -> None

let native_name (x : string) : string = "fn_" ^ mangle x

(* [VClos (fun p0 -> VClos (fun p1 -> … native p0 p1 …))] — the value-form of an
   arity-[n] native function. *)
let recurry (native : string) (n : int) : string =
  let ps = List.init n (fun i -> Printf.sprintf "_p%d" i) in
  let call = Printf.sprintf "(%s %s)" native (String.concat " " ps) in
  List.fold_right (fun p acc -> Printf.sprintf "(VClos (fun %s -> %s))" p acc) ps call

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

(* A binder is "friendly" (compilable to an OCaml pattern, so a case can use OCaml's
   `match` and inherit `ocamlopt`'s decision tree) iff it contains no record binder — a
   record is an `SMap`, not key-matchable in a pattern. A number literal is friendly:
   bound as `VNumber v` with a `when v = f` guard (OCaml forbids only float literal
   patterns, not bindings). *)
let rec friendly (b : C.binder) : bool =
  match b with
  | C.BRecord _ -> false
  | C.BNamed (_, i) -> friendly i
  | C.BCtor (_, subs) | C.BArray subs -> List.for_all friendly subs
  | _ -> true

(* A friendly binder → an OCaml pattern plus the side guard conditions it needs (number
   equalities), combined with `&&` at the arm. *)
let rec ocaml_pat (b : C.binder) : string * string list =
  match b with
  | C.BNull -> "_", []
  | C.BVar x -> mangle x, []
  | C.BNamed (x, inner) ->
    let p, g = ocaml_pat inner in
    Printf.sprintf "(%s as %s)" p (mangle x), g
  | C.BLit (C.LInt n) -> Printf.sprintf "(VInt %d)" n, []
  | C.BLit (C.LBool b) -> Printf.sprintf "(VBool %b)" b, []
  | C.BLit (C.LString s) -> Printf.sprintf "(VString %s)" (ocaml_string s), []
  | C.BLit (C.LNumber f) ->
    let v = fresh "_n" in
    Printf.sprintf "(VNumber %s)" v, [ Printf.sprintf "(%s = %h)" v f ]
  | C.BCtor (tag, subs) ->
    let ps, gs = List.split (List.map ocaml_pat subs) in
    Printf.sprintf "(VData (%s, [| %s |]))" (ocaml_string tag) (String.concat "; " ps), List.concat gs
  | C.BArray subs ->
    let ps, gs = List.split (List.map ocaml_pat subs) in
    Printf.sprintf "(VArray [| %s |])" (String.concat "; " ps), List.concat gs
  | C.BRecord _ -> failwith "ocaml_pat: record binder is not friendly"

(* [ae] maps a known-arity function's name to its arity; [lz] the lazily-bound names.
   The two are disjoint (a lazy member is never a direct lambda) given unique names. *)
let rec expr (ae : int IM.t) (lz : SS.t) (e : A.expr) : string =
  match e with
  | A.Ret c -> cexpr ae lz c
  | A.Let (x, c, body) ->
    (match c with
     | A.CLam (ps, lbody) ->
       (* emit both the native multi-arg fn and the VClos wrapper; the body sees x as
          known-arity. (Non-recursive: [lbody] does not see x.) *)
       let n = List.length ps in
       Printf.sprintf "(let %s %s = %s in let %s = %s in %s)" (native_name x)
         (String.concat " " (List.map mangle ps)) (expr ae lz lbody) (mangle x) (recurry (native_name x) n)
         (expr (IM.add x n ae) lz body)
     | _ -> Printf.sprintf "(let %s = %s in %s)" (mangle x) (cexpr ae lz c) (expr ae lz body))
  | A.LetRec (binds, body) ->
    (* function members → native fn + VClos wrapper (both in the `let rec`); other
       members → `lazy`. Extend [ae]/[lz] accordingly for every RHS and the body. *)
    let ae' = List.fold_left (fun m (x, rhs) -> match as_lam rhs with Some (ps, _) -> IM.add x (List.length ps) m | None -> m) ae binds in
    let lz' = List.fold_left (fun s (x, rhs) -> match as_lam rhs with Some _ -> s | None -> SS.add x s) lz binds in
    let bindings =
      List.concat_map
        (fun (x, rhs) ->
          match as_lam rhs with
          | Some (ps, lbody) ->
            [ Printf.sprintf "%s %s = %s" (native_name x) (String.concat " " (List.map mangle ps)) (expr ae' lz' lbody)
            ; Printf.sprintf "%s = %s" (mangle x) (recurry (native_name x) (List.length ps)) ]
          | None -> [ Printf.sprintf "%s = lazy %s" (mangle x) (expr ae' lz' rhs) ])
        binds
    in
    Printf.sprintf "(let rec %s in %s)" (String.concat " and " bindings) (expr ae' lz' body)

and cexpr (ae : int IM.t) (lz : SS.t) (c : A.cexpr) : string =
  let atom = atom lz in
  let app_chain head args = List.fold_left (fun acc a -> Printf.sprintf "(app %s (%s))" acc (atom a)) head args in
  match c with
  | A.CAtom a -> atom a
  | A.CLam (ps, body) ->
    List.fold_right (fun p acc -> Printf.sprintf "(VClos (fun %s -> %s))" (mangle p) acc) ps (expr ae lz body)
  | A.CApp (A.AVar x, args) when IM.mem x ae && not (SS.mem x lz) ->
    let n = List.length args and ar = IM.find x ae in
    if n = ar
    then (* exact saturation: a direct, uncurried native call *)
      Printf.sprintf "(%s %s)" (native_name x) (String.concat " " (List.map (fun a -> "(" ^ atom a ^ ")") args))
    else if n < ar
    then app_chain (mangle x) args (* partial: via the VClos wrapper *)
    else (
      (* over-application: saturate natively, then apply the rest through [app] *)
      let take = List.filteri (fun i _ -> i < ar) args and drop = List.filteri (fun i _ -> i >= ar) args in
      let sat = Printf.sprintf "(%s %s)" (native_name x) (String.concat " " (List.map (fun a -> "(" ^ atom a ^ ")") take)) in
      app_chain sat drop)
  | A.CApp (f, args) -> app_chain (atom f) args
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
  | A.CIf (a, t, e) -> Printf.sprintf "(if as_bool (%s) then %s else %s)" (atom a) (expr ae lz t) (expr ae lz e)
  | A.CCase (scruts, alts) -> case ae lz scruts alts

(* A case whose every binder is friendly compiles to an OCaml `match` — `ocamlopt` turns
   that into a decision tree (each scrutinee examined once, tags switched), far better
   than re-testing per alternative. A case containing a record binder falls back to the
   CPS cascade ([case_cascade]). *)
and case (ae : int IM.t) (lz : SS.t) (scruts : A.atom list) (alts : A.alt list) : string =
  if List.for_all (fun (a : A.alt) -> List.for_all friendly a.binders) alts
  then case_decision ae lz scruts alts
  else case_cascade ae lz scruts alts

and case_decision (ae : int IM.t) (lz : SS.t) (scruts : A.atom list) (alts : A.alt list) : string =
  let scrut =
    match scruts with [ s ] -> atom lz s | ss -> "(" ^ String.concat ", " (List.map (atom lz) ss) ^ ")"
  in
  let n = List.length scruts in
  let pat_of (a : A.alt) =
    let ps, gs = List.split (List.map ocaml_pat a.binders) in
    (if n = 1 then List.hd ps else "(" ^ String.concat ", " ps ^ ")"), List.concat gs
  in
  let arms (a : A.alt) =
    let pat, pg = pat_of a in
    let whenc (extra : string) =
      match List.filter (fun s -> s <> "") (pg @ [ extra ]) with
      | [] -> ""
      | cs -> " when " ^ String.concat " && " cs
    in
    match a.result with
    | A.Uncond e -> [ Printf.sprintf "  | %s%s -> %s" pat (whenc "") (expr ae lz e) ]
    | A.Guarded gs ->
      List.map
        (fun (g, e) -> Printf.sprintf "  | %s%s -> %s" pat (whenc (Printf.sprintf "as_bool (%s)" (expr ae lz g))) (expr ae lz e))
        gs
  in
  Printf.sprintf "(match %s with\n%s\n  | _ -> stuck \"no match\")" scrut
    (String.concat "\n" (List.concat_map arms alts))

(* The general fallback: a CPS cascade. Scrutinees are bound once; each alt is tried in
   order, its failure jumping to a shared thunk that runs the next alt (so the rest is
   not duplicated). Handles every binder kind (records, etc.) via [emit_match]. *)
and case_cascade (ae : int IM.t) (lz : SS.t) (scruts : A.atom list) (alts : A.alt list) : string =
  let svars = List.map (fun _ -> fresh "_s") scruts in
  let lets =
    String.concat "" (List.map2 (fun v s -> Printf.sprintf "let %s = %s in " v (atom lz s)) svars scruts)
  in
  let alt_code (a : A.alt) (fail : string) : string =
    let succ =
      match a.result with
      | A.Uncond e -> expr ae lz e
      | A.Guarded gs ->
        List.fold_right
          (fun (g, e) acc -> Printf.sprintf "(if as_bool (%s) then %s else %s)" (expr ae lz g) (expr ae lz e) acc)
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

(** Emit a whole ANF program as a self-contained OCaml source string. A *pure* entry is
    printed in `Value.to_string` form; an *Effect* entry (`Unit -> a`, ADR-0023) is
    forced — applied to unit (the `VInt 0` convention, ADR-0032) — performing its
    effects, with the (`Unit`) result suppressed (as `purvm run` does for the bytecode
    image). So an Effect program's stdout is exactly its observable effects. *)
(* Collect every bound name (see [bound_names]). *)
let collect_bound (e : A.expr) : SS.t =
  let acc = ref SS.empty in
  let add x = acc := SS.add x !acc in
  let rec ex = function
    | A.Ret c -> cx c
    | A.Let (x, c, b) -> add x; cx c; ex b
    | A.LetRec (g, b) -> List.iter (fun (x, r) -> add x; ex r) g; ex b
  and cx = function
    | A.CLam (ps, b) -> List.iter add ps; ex b
    | A.CIf (_, t, e) -> ex t; ex e
    | A.CCase (_, alts) ->
      List.iter
        (fun (a : A.alt) ->
          List.iter (fun bd -> List.iter add (binder_vars bd)) a.binders;
          match a.result with
          | A.Uncond e -> ex e
          | A.Guarded gs -> List.iter (fun (g, e) -> ex g; ex e) gs)
        alts
    | A.CAtom _ | A.CApp _ | A.CPrim _ | A.CCtor _ | A.CArray _ | A.CRecord _ | A.CAccessor _ | A.CUpdate _ ->
      ()
  in
  ex e;
  !acc

let program ?(is_effect = false) (e : A.expr) : string =
  bound_names := collect_bound e;
  let runner =
    if is_effect then "ignore (app result (VInt 0))"
    else "print_string (to_string result)"
  in
  Printf.sprintf "[@@@warning \"-a\"]\n%s\nlet result = %s\nlet () = %s\n" rt (expr IM.empty SS.empty e) runner
