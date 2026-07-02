(** ANF → LLVM textual IR (ADR-0072), the native backend that lowers onto the owned Rust runtime
    (ADR-0071). Implemented so far (ADR-0072 §10 slice plan):
    - **Slice 1 — pure first-order**: literals (`Int`/`Boolean`), arithmetic/comparison primops, `let`,
      `if`, uncurried lambdas (lambda-lifted), and application through the eval/`apply` trampoline.
    - **Slice 2 — ADTs + `case`**: saturated constructors (nullary → an immediate tag, field-carrying →
      `pv_new_adt`), and the CPS-cascade matcher over `BNull`/`BVar`/`BNamed`/`BLit`(`Int`/`Bool`)/`BCtor`
      binders, with guarded alternatives.
    - **Slice 3 — static records**: literals (`pv_new_record` over compile-time FNV-1a-64 label ids,
      sorted), the static `Accessor` and functional `Update` (id-keyed `pv_record_get`/`pv_record_set`),
      and the `BRecord` row-poly binder.
    - **Slice 4 — strings, numbers, arrays, leaves, `Effect`**: `String`/`Number` literals
      (`pv_new_str`/`pv_new_number`), `Array` literals, the full primop set (Number/String/Array + the
      dynamic `String`-keyed record ops), `foreign` leaves (`pv_foreign`), and the `Effect` entry
      (`pv_run_effect` + drain to `stdout`). Multi-atom sites are `eval_atoms`-protected (root-on-collect,
      since literals now allocate); a small-heap fixture exercises the rooting under a forced collection.
    - **Slice 5a — recursive groups (`letrec`)**: a group is all-by-need `ByNeed` cells over one shared
      env (ADR-0070 §4), so function members recurse (self/sibling) via the env and `apply` auto-forces a
      by-need callee, while a value member (a dictionary) is a cell forced at each value-dereference site
      by `pv_force_if_byneed` (robust to a cell reaching a projection / `case` / primop through an
      argument or data field, without static tracking). The **Boolean demand sites** (`if`/guard
      conditions) and the **program's final result** force likewise; construction sites
      (`Ctor`/`Record`/`Array` fields, `apply` head/args) keep the raw cell (the knot-tie).
    Unsaturated constructors / per-module cross-module linking / pure-`Number`/`String`/`Bool` entry
    printers land later (a clear [failwith] / `pv_print_int`-only guards each).

    Every guest value is one `i64` tagged word (ADR-0064 §1); the module calls the runtime's `extern "C"`
    surface (`pv_*`, ADR-0071). Rooting is **root-on-create** (ADR-0072 §6, the conservative first cut):
    each function opens a shadow-stack frame, every bound value is `pv_root`ed and read back with `pv_get`,
    and every return pops the frame — so any value trivially survives any safepoint. Minimising this to
    "root only across a safepoint" is a later perf slice. Nested lambdas are hoisted to top-level
    functions with an explicit captured-env array (ADR-0072 §4). *)

module A = Middle_end.Anf
module C = Cesk.Ast
module SS = Set.Make (String)

(* --- immediates (ADR-0064 §1): a scalar is `(payload << 1) | 1`, an i64 constant, no call ---------- *)

let imm (payload : int) : string =
  Int64.to_string (Int64.logor (Int64.shift_left (Int64.of_int payload) 1) 1L)

(* [imm_int] applies the 32-bit wrap (ADR-0006) to the literal before tagging. *)
let imm_int (n : int) : string = imm (Int32.to_int (Int32.of_int n))
let imm_bool (b : bool) : string = imm (if b then 1 else 0)
let imm_unit : string = imm 0

(* --- record label ids (ADR-0069 §2): FNV-1a-64, byte-for-byte with the runtime's `record::fnv1a_64`,
   so a compile-time static label id equals a runtime hash of the same name --------------------------- *)

let fnv1a_64 (s : string) : int64 =
  let h = ref 0xcbf29ce484222325L in
  String.iter
    (fun c ->
       h := Int64.logxor !h (Int64.of_int (Char.code c));
       h := Int64.mul !h 0x100000001b3L)
    s;
  !h

(* The label's id as an LLVM `i64` constant operand. `Int64.to_string` is signed decimal, but LLVM
   interprets a negative `i64` literal as its two's-complement bit pattern, so the u64 the runtime reads
   round-trips exactly. *)
let label_id (label : string) : string = Int64.to_string (fnv1a_64 label)

(* --- free variables of an ANF term (for lambda-lifting, ADR-0072 §4) ------------------------------- *)

let rec binder_vars : C.binder -> SS.t = function
  | C.BNull | C.BLit _ -> SS.empty
  | C.BVar x -> SS.singleton x
  | C.BNamed (x, i) -> SS.add x (binder_vars i)
  | C.BCtor (_, subs) | C.BArray subs ->
    List.fold_left (fun a b -> SS.union a (binder_vars b)) SS.empty subs
  | C.BRecord fs ->
    List.fold_left (fun a (_, b) -> SS.union a (binder_vars b)) SS.empty fs

let fv_atom (bound : SS.t) : A.atom -> SS.t = function
  | A.AVar x -> if SS.mem x bound then SS.empty else SS.singleton x
  | A.ALit _ | A.AForeign _ -> SS.empty

let fv_atoms bound = List.fold_left (fun a x -> SS.union a (fv_atom bound x)) SS.empty

let rec fv_expr (bound : SS.t) (e : A.expr) : SS.t =
  match e with
  | A.Ret c -> fv_cexpr bound c
  | A.Let (x, c, body) -> SS.union (fv_cexpr bound c) (fv_expr (SS.add x bound) body)
  | A.LetRec (binds, body) ->
    let bound = List.fold_left (fun s (x, _) -> SS.add x s) bound binds in
    let rhs =
      List.fold_left (fun a (_, r) -> SS.union a (fv_expr bound r)) SS.empty binds
    in
    SS.union rhs (fv_expr bound body)

and fv_cexpr (bound : SS.t) (c : A.cexpr) : SS.t =
  match c with
  | A.CAtom a -> fv_atom bound a
  | A.CLam (ps, b) -> fv_expr (List.fold_left (fun s p -> SS.add p s) bound ps) b
  | A.CApp (f, args) -> SS.union (fv_atom bound f) (fv_atoms bound args)
  | A.CPrim (_, args) | A.CArray args -> fv_atoms bound args
  | A.CCtor (_, _, args) -> fv_atoms bound args
  | A.CRecord fs -> fv_atoms bound (List.map snd fs)
  | A.CAccessor (a, _) -> fv_atom bound a
  | A.CUpdate (a, fs) -> SS.union (fv_atom bound a) (fv_atoms bound (List.map snd fs))
  | A.CIf (a, t, e) ->
    SS.union (fv_atom bound a) (SS.union (fv_expr bound t) (fv_expr bound e))
  | A.CCase (scruts, alts) ->
    let scr = fv_atoms bound scruts in
    List.fold_left (fun acc (alt : A.alt) -> SS.union acc (fv_alt bound alt)) scr alts

and fv_alt (bound : SS.t) (alt : A.alt) : SS.t =
  let bvs = List.fold_left (fun a b -> SS.union a (binder_vars b)) SS.empty alt.binders in
  let bound = SS.union bound bvs in
  match alt.result with
  | A.Uncond e -> fv_expr bound e
  | A.Guarded gs ->
    List.fold_left
      (fun acc (g, e) -> SS.union acc (SS.union (fv_expr bound g) (fv_expr bound e)))
      SS.empty
      gs

(* --- primop → runtime helper (ADR-0071 §6) --------------------------------------------------------- *)

(* [(symbol, needs_ctx)]: scalar `Int`/`Boolean` ops are **pure** (no ctx, no safepoint); ops that read
   or allocate a boxed value (`Number`/`String`/`Array`/`Record`) take the ctx (ADR-0071 §6). *)
let prim_sym : C.primop -> string * bool = function
  | C.AddInt -> "pv_prim_add_int", false
  | C.SubInt -> "pv_prim_sub_int", false
  | C.MulInt -> "pv_prim_mul_int", false
  | C.DivInt -> "pv_prim_div_int", false
  | C.ModInt -> "pv_prim_mod_int", false
  | C.AndInt -> "pv_prim_and_int", false
  | C.OrInt -> "pv_prim_or_int", false
  | C.XorInt -> "pv_prim_xor_int", false
  | C.ShlInt -> "pv_prim_shl_int", false
  | C.ShrInt -> "pv_prim_shr_int", false
  | C.ZshrInt -> "pv_prim_zshr_int", false
  | C.ComplementInt -> "pv_prim_complement_int", false
  | C.EqInt -> "pv_prim_eq_int", false
  | C.LtInt -> "pv_prim_lt_int", false
  | C.EqBool -> "pv_prim_eq_bool", false
  | C.AndBool -> "pv_prim_and_bool", false
  | C.OrBool -> "pv_prim_or_bool", false
  | C.NotBool -> "pv_prim_not_bool", false
  | C.AddNumber -> "pv_prim_add_number", true
  | C.SubNumber -> "pv_prim_sub_number", true
  | C.MulNumber -> "pv_prim_mul_number", true
  | C.DivNumber -> "pv_prim_div_number", true
  | C.IntToNumber -> "pv_prim_int_to_number", true
  | C.NumberToInt -> "pv_prim_number_to_int", true
  | C.EqNumber -> "pv_prim_eq_number", true
  | C.LtNumber -> "pv_prim_lt_number", true
  | C.EqString -> "pv_prim_eq_string", true
  | C.LtString -> "pv_prim_lt_string", true
  | C.Append -> "pv_prim_append", true
  | C.IndexArray -> "pv_prim_index_array", true
  | C.LengthArray -> "pv_prim_length_array", true
  | C.NewArray -> "pv_prim_new_array", true
  | C.SetArray -> "pv_prim_set_array", true
  | C.RecordGet -> "pv_prim_record_get", true
  | C.RecordSet -> "pv_prim_record_set", true
  | C.RecordHas -> "pv_prim_record_has", true
  | C.RecordDelete -> "pv_prim_record_delete", true

(* --- the emitter ----------------------------------------------------------------------------------- *)

(* A lifted lambda awaiting emission: its global name, params, captured free vars (in a fixed order),
   and body. *)
type lifted =
  { name : string
  ; params : string list
  ; captures : string list
  ; body : A.expr
  }

type ctx =
  { md : Buffer.t (* the whole module *)
  ; globals : Buffer.t (* module-level byte constants (String literals / foreign keys) *)
  ; mutable fn : Buffer.t (* the current function body *)
  ; mutable ssa : int
  ; mutable lbl : int
  ; mutable fns : int (* lifted-function counter *)
  ; mutable strs : int (* string-constant counter *)
  ; mutable pending : lifted list (* lambdas to emit *)
  ; mutable frame : string (* the current function's shadow-stack frame handle operand *)
  ; ctors :
      ( string
        , int )
        Hashtbl.t (* constructor name → tag index (program-wide, first-seen order) *)
  }

(* A constructor's runtime tag index (ADR-0064 §1/§2). The name → index map is program-wide and assigned
   in first-seen order; only *consistency* between construction and matching matters (the index is
   internal — a `case` result is observed, never the tag). Cross-module determinism (a hash of the
   qualified name) is a slice-5 concern; slice 2 is single-module. *)
let ctor_index cx (name : string) : int =
  match Hashtbl.find_opt cx.ctors name with
  | Some i -> i
  | None ->
    let i = Hashtbl.length cx.ctors in
    Hashtbl.add cx.ctors name i;
    i

let fresh cx : string =
  cx.ssa <- cx.ssa + 1;
  Printf.sprintf "%%t%d" cx.ssa

let fresh_label cx (p : string) : string =
  cx.lbl <- cx.lbl + 1;
  Printf.sprintf "%s%d" p cx.lbl

let emit cx fmt =
  Printf.ksprintf
    (fun s ->
       Buffer.add_string cx.fn s;
       Buffer.add_char cx.fn '\n')
    fmt

(* env: a guest variable → the SSA operand holding its **root handle** (root-on-create). A reference
   reloads the current value via [pv_get]. A by-need cell may reach a value-dereference site through any
   binding (a member, a function argument, a data field), so forcing is a *runtime* check at the site
   ([force_atom] → `pv_force_if_byneed`), not a static per-binding flag. *)
type env = (string * string) list

let bind (env : env) (x : string) (handle : string) : env = (x, handle) :: env

(* Read a variable's current value (post-safepoint) through its root handle. Raw: a by-need cell stays a
   cell (it propagates and `apply` auto-forces it in callee position). *)
let read_var cx (env : env) (x : string) : string =
  match List.assoc_opt x env with
  | Some h ->
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_get(ptr %%ctx, i64 %s)" t h;
    t
  | None ->
    failwith (Printf.sprintf "codegen_llvm: unbound variable %s (unresolved foreign?)" x)

(* Emit a module-level byte constant for [s] and return `(ptr-to-first-byte, len)`. Used for `String`
   literals (`pv_new_str`) and foreign keys (`pv_foreign`). The empty string needs no global — a null
   pointer with length 0 is the runtime's empty-buffer convention. *)
let string_constant cx (s : string) : string * int =
  let len = String.length s in
  if len = 0
  then "null", 0
  else (
    cx.strs <- cx.strs + 1;
    let name = Printf.sprintf "@.str.%d" cx.strs in
    let esc = Buffer.create (len + 2) in
    String.iter
      (fun c ->
         let b = Char.code c in
         if b >= 0x20 && b <= 0x7e && c <> '"' && c <> '\\'
         then Buffer.add_char esc c
         else Buffer.add_string esc (Printf.sprintf "\\%02X" b))
      s;
    Buffer.add_string
      cx.globals
      (Printf.sprintf
         "%s = private unnamed_addr constant [%d x i8] c\"%s\"\n"
         name
         len
         (Buffer.contents esc));
    let p = fresh cx in
    emit cx "  %s = getelementptr [%d x i8], ptr %s, i64 0, i64 0" p len name;
    p, len)

let atom cx (env : env) : A.atom -> string = function
  | A.AVar x -> read_var cx env x
  | A.ALit (C.LInt n) -> imm_int n
  | A.ALit (C.LBool b) -> imm_bool b
  | A.ALit (C.LNumber f) ->
    (* Boxed `Number` (ADR-0064 §1): pass the IEEE-754 bit pattern. *)
    let t = fresh cx in
    emit
      cx
      "  %s = call i64 @pv_new_number(ptr %%ctx, i64 %s)"
      t
      (Int64.to_string (Int64.bits_of_float f));
    t
  | A.ALit (C.LString s) ->
    let p, len = string_constant cx s in
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_new_str(ptr %%ctx, ptr %s, i64 %d)" t p len;
    t
  | A.AForeign k ->
    (* An unresolved foreign leaf → the runtime resolver returns it as a closure (ADR-0072 §9). *)
    let p, len = string_constant cx k in
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_foreign(ptr %%ctx, ptr %s, i64 %d)" t p len;
    t

(* An atom evaluated at a **value-dereference** site (ADR-0070 §3): a dictionary read for a method, a
   `case` scrutinee, a primop operand. A variable might hold a by-need cell (a recursive member, or one
   passed through an argument / data field), so it is run through `pv_force_if_byneed` — a runtime check
   that forces a cell to its value and passes anything else through (literals never are cells).
   Construction sites (`Ctor`/`Record`/`Array` fields, `apply` head/args) use the raw [atom], so the cell
   propagates and the knot-tie stores it unforced. *)
let force_atom cx (env : env) (a : A.atom) : string =
  match a with
  | A.AVar _ ->
    let v = atom cx env a in
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_force_if_byneed(ptr %%ctx, i64 %s)" t v;
    t
  | A.ALit _ | A.AForeign _ -> atom cx env a

(* Force an arbitrary produced value iff it is a by-need cell — for a demand site whose value is an
   `expr` result (a guard, the entry's final output), not an `atom` reachable by `force_atom`. *)
let force_value cx (v : string) : string =
  let t = fresh cx in
  emit cx "  %s = call i64 @pv_force_if_byneed(ptr %%ctx, i64 %s)" t v;
  t

(* Root a freshly produced value and return its handle operand (root-on-create). *)
let root cx (v : string) : string =
  let h = fresh cx in
  emit cx "  %s = call i64 @pv_root(ptr %%ctx, i64 %s)" h v;
  h

(* Materialise an i64 arg buffer for a call — an [alloca] holding the operands — returning the pointer
   operand and count. Zero args → a null pointer (the runtime treats len 0 as empty). *)
let arg_buffer cx (operands : string list) : string * int =
  let n = List.length operands in
  if n = 0
  then "null", 0
  else (
    let buf = fresh cx in
    emit cx "  %s = alloca [%d x i64]" buf n;
    List.iteri
      (fun i v ->
         let p = fresh cx in
         emit cx "  %s = getelementptr [%d x i64], ptr %s, i64 0, i64 %d" p n buf i;
         emit cx "  store i64 %s, ptr %s" v p)
      operands;
    let p0 = fresh cx in
    emit cx "  %s = getelementptr [%d x i64], ptr %s, i64 0, i64 0" p0 n buf;
    p0, n)

(* Build a closure value for a lifted lambda: assemble the captured-env array from the current values of
   its free variables, then [pv_make_closure] over the lifted function's address. *)
let make_closure cx (env : env) (l : lifted) : string =
  let env_word =
    match l.captures with
    | [] -> imm_unit (* no-capture closure: the immediate env sentinel (ADR-0064 §2) *)
    | caps ->
      let vals = List.map (fun c -> read_var cx env c) caps in
      let p, n = arg_buffer cx vals in
      let arr = fresh cx in
      emit cx "  %s = call i64 @pv_new_array(ptr %%ctx, ptr %s, i64 %d)" arr p n;
      arr
  in
  let addr = fresh cx in
  emit cx "  %s = ptrtoint ptr @%s to i64" addr l.name;
  let clo = fresh cx in
  emit
    cx
    "  %s = call i64 @pv_make_closure(ptr %%ctx, i64 %s, i32 %d, i64 %s)"
    clo
    addr
    (List.length l.params)
    env_word;
  clo

(* Register a lambda for hoisting and return the [lifted] record (its captures fixed in a stable order). *)
let lift cx (params : string list) (body : A.expr) : lifted =
  cx.fns <- cx.fns + 1;
  let bound = List.fold_left (fun s p -> SS.add p s) SS.empty params in
  let captures = SS.elements (fv_expr bound body) in
  let l = { name = Printf.sprintf "fn_%d" cx.fns; params; captures; body } in
  cx.pending <- l :: cx.pending;
  l

(* Emit the current function's return: pop the shadow-stack frame, then `ret`. *)
let emit_ret cx (v : string) : unit =
  emit cx "  call void @pv_pop_frame(ptr %%ctx, i64 %s)" cx.frame;
  emit cx "  ret i64 %s" v

(* Read the current value of a rooted handle (post-safepoint) — [pv_get] on a bare handle operand. *)
let get_current cx (handle : string) : string =
  let t = fresh cx in
  emit cx "  %s = call i64 @pv_get(ptr %%ctx, i64 %s)" t handle;
  t

(* Evaluate a list of atoms and return their **current** value operands, mutually protected against
   each other's allocation. Once `atom` can allocate (slice 4: `String`/`Number` literals, `foreign`
   closures), collecting several results into registers and then calling — e.g. `CRecord`'s values,
   `CApp`'s `f`+args, `CCtor`'s fields — would let a later atom's GC stale an earlier value. So each atom
   is `pv_root`ed as it is produced (left-to-right), and all are read back with `pv_get` *after* every
   allocation is done. The transient roots live in the enclosing function frame (freed at return);
   bounding them is the deferred rooting minimisation (ADR-0072 §6). *)
let eval_atoms cx ?(force = false) (env : env) (atoms : A.atom list) : string list =
  let one = if force then force_atom else atom in
  let handles = List.fold_left (fun acc a -> root cx (one cx env a) :: acc) [] atoms in
  List.rev_map (get_current cx) handles

(* Emit a `br i1` guarded by [ok]: fall through to a fresh continuation block on true, jump to [fail] on
   false. Returns after emitting the continuation block's label (subsequent code runs on match). *)
let test_or_fail cx (ok : string) ~(fail : string) (tag : string) : unit =
  let cont = fresh_label cx tag in
  emit cx "  br i1 %s, label %%%s, label %%%s" ok cont fail;
  emit cx "%s:" cont

(* Match [binder] against value operand [scrut] (ADR-0072 §5, the CPS cascade). On a mismatch, branch to
   [fail]; on a match, fall through (in a new block) with the env extended by the binder's bindings. No
   guest allocation happens inside a match, so [scrut] stays valid throughout (the body may allocate, but
   its bound values are rooted on capture — root-on-create). Records/arrays are later slices. *)
let rec match_binder cx (env : env) ~(fail : string) (binder : C.binder) (scrut : string)
  : env
  =
  match binder with
  | C.BNull -> env
  | C.BVar x -> bind env x (root cx scrut)
  | C.BNamed (x, inner) ->
    let env = bind env x (root cx scrut) in
    match_binder cx env ~fail inner scrut
  | C.BLit l ->
    let lit =
      match l with
      | C.LInt n -> imm_int n
      | C.LBool b -> imm_bool b
      | C.LNumber _ | C.LString _ ->
        failwith "codegen_llvm: Number/String literal binder not in slice 2"
    in
    let ok = fresh cx in
    emit cx "  %s = icmp eq i64 %s, %s" ok scrut lit;
    test_or_fail cx ok ~fail "lit_ok";
    env
  | C.BCtor (name, subs) ->
    let idx = ctor_index cx name in
    let low = fresh cx in
    emit cx "  %s = and i64 %s, 1" low scrut;
    if subs = []
    then (
      (* Nullary → an immediate whose payload is the tag index (ADR-0064 §1). *)
      let is_imm = fresh cx in
      emit cx "  %s = icmp eq i64 %s, 1" is_imm low;
      test_or_fail cx is_imm ~fail "nl_imm";
      let pay = fresh cx in
      emit cx "  %s = ashr i64 %s, 1" pay scrut;
      let ok = fresh cx in
      emit cx "  %s = icmp eq i64 %s, %d" ok pay idx;
      test_or_fail cx ok ~fail "nl_tag";
      env)
    else (
      (* Field-carrying → a pointer to an `Adt` (tag at raw word 0, field i at value slot 1+i). *)
      let is_ptr = fresh cx in
      emit cx "  %s = icmp eq i64 %s, 0" is_ptr low;
      test_or_fail cx is_ptr ~fail "adt_ptr";
      let tag = fresh cx in
      emit cx "  %s = call i64 @pv_read_raw(ptr %%ctx, i64 %s, i64 0)" tag scrut;
      let ok = fresh cx in
      emit cx "  %s = icmp eq i64 %s, %d" ok tag idx;
      test_or_fail cx ok ~fail "adt_tag";
      List.fold_left
        (fun env (i, sub) ->
           let fld = fresh cx in
           emit
             cx
             "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %s, i64 %d)"
             fld
             scrut
             (1 + i);
           match_binder cx env ~fail sub fld)
        env
        (List.mapi (fun i s -> i, s) subs))
  | C.BRecord fields ->
    (* A row-polymorphic subset match (ADR-0012): the record *has* these labels by typing, so each field
       read succeeds; only the sub-binders can fail. Reads are allocation-free, so [scrut] stays valid. *)
    List.fold_left
      (fun env (label, sub) ->
         let fld = fresh cx in
         emit
           cx
           "  %s = call i64 @pv_record_get(ptr %%ctx, i64 %s, i64 %s)"
           fld
           scrut
           (label_id label);
         match_binder cx env ~fail sub fld)
      env
      fields
  | C.BArray _ -> failwith "codegen_llvm: array binder not in slice 2"

(* Compile [e]; [tail] = it is in the enclosing function's tail position. Returns [Some operand] when
   not tail (the produced value), [None] when tail (a `ret`/tail-call was emitted). *)
let rec expr cx (env : env) ~(tail : bool) (e : A.expr) : string option =
  match e with
  | A.Ret c -> cexpr cx env ~tail c
  | A.Let (x, c, body) ->
    let v = Option.get (cexpr cx env ~tail:false c) in
    let h = root cx v in
    expr cx (bind env x h) ~tail body
  | A.LetRec (binds, body) ->
    (* A recursive group as **all-by-need `ByNeed` cells over one shared env** (ADR-0070 §4, mirroring the
       runtime `build_group`). Each member is a cell; its suspension is a nullary thunk over the shared
       env, whose body is the member's RHS with sibling references resolving to the sibling cells (a
       function member's cell forces to its closure on first use; `apply` auto-forces a by-need callee,
       ADR-0070 §3). Distinguishing function members as back-patched closures (ADR-0059 §1) is a deferred
       optimisation. *)
    let members = List.map fst binds in
    let k = List.length members in
    let member_set = List.fold_left (fun s m -> SS.add m s) SS.empty members in
    (* free vars of the group captured from *outside* it (members excluded) *)
    let outside =
      SS.elements
        (List.fold_left
           (fun acc (_, rhs) -> SS.union acc (fv_expr member_set rhs))
           SS.empty
           binds)
    in
    (* the shared env layout: the k member cells, then the outside captures *)
    let shared_layout = members @ outside in
    (* one suspension per member: `\$u -> rhs_i` over the shared env (registered for hoisting) *)
    let susp_names =
      List.map
        (fun (_, rhs) ->
           cx.fns <- cx.fns + 1;
           let name = Printf.sprintf "susp_%d" cx.fns in
           cx.pending
           <- { name; params = [ "$u" ]; captures = shared_layout; body = rhs }
              :: cx.pending;
           name)
        binds
    in
    (* 1. shared env array = [unit × k] ++ [outside-capture values]; root it. *)
    let elems = List.init k (fun _ -> imm_unit) @ List.map (read_var cx env) outside in
    let env_p, env_n = arg_buffer cx elems in
    let env_arr = fresh cx in
    emit cx "  %s = call i64 @pv_new_array(ptr %%ctx, ptr %s, i64 %d)" env_arr env_p env_n;
    let env_h = root cx env_arr in
    (* 2. placeholder cells; store each into env[i] (reloading env/cell after each allocation). *)
    let cell_hs =
      List.init k (fun i ->
        let cell = fresh cx in
        emit cx "  %s = call i64 @pv_new_byneed_placeholder(ptr %%ctx)" cell;
        let ch = root cx cell in
        let envp = get_current cx env_h in
        let cw = get_current cx ch in
        emit cx "  call void @pv_write_field(ptr %%ctx, i64 %s, i64 %d, i64 %s)" envp i cw;
        ch)
    in
    (* 3. build each suspension closure over the shared env; backpatch it into its cell. *)
    List.iteri
      (fun i name ->
         let envp = get_current cx env_h in
         let addr = fresh cx in
         emit cx "  %s = ptrtoint ptr @%s to i64" addr name;
         let susp = fresh cx in
         emit
           cx
           "  %s = call i64 @pv_make_closure(ptr %%ctx, i64 %s, i32 1, i64 %s)"
           susp
           addr
           envp;
         let cellp = get_current cx (List.nth cell_hs i) in
         emit
           cx
           "  call void @pv_byneed_set_suspension(ptr %%ctx, i64 %s, i64 %s)"
           cellp
           susp)
      susp_names;
    (* 4. bind each member to its cell and compile the body. *)
    let env' = List.fold_left2 bind env members cell_hs in
    expr cx env' ~tail body

and cexpr cx (env : env) ~(tail : bool) (c : A.cexpr) : string option =
  (* Finish a produced value in the current tail context. *)
  let finish v =
    if tail
    then (
      emit_ret cx v;
      None)
    else Some v
  in
  match c with
  | A.CAtom a -> finish (atom cx env a)
  | A.CPrim (op, args) ->
    let sym, needs_ctx = prim_sym op in
    (* A primop consumes its operands' *values* (e.g. `RecordGet` on a by-need dict), so force them. *)
    let ops = eval_atoms ~force:true cx env args in
    let t = fresh cx in
    if needs_ctx
    then
      emit
        cx
        "  %s = call i64 @%s(ptr %%ctx, %s)"
        t
        sym
        (String.concat ", " (List.map (fun o -> "i64 " ^ o) ops))
    else
      emit
        cx
        "  %s = call i64 @%s(%s)"
        t
        sym
        (String.concat ", " (List.map (fun o -> "i64 " ^ o) ops));
    finish t
  | A.CLam (ps, body) -> finish (make_closure cx env (lift cx ps body))
  | A.CApp (f, args) ->
    (* `f` and the args are mutually protected: a `foreign` callee or a `String` arg may allocate. *)
    let all = eval_atoms cx env (f :: args) in
    let fv = List.hd all
    and ops = List.tl all in
    if tail
    then (
      (* Trampoline tail call (ADR-0071 §4): stash the pending tail, pop this frame, return (ignored). *)
      let p, n = arg_buffer cx ops in
      emit cx "  call void @pv_tailcall(ptr %%ctx, i64 %s, ptr %s, i64 %d)" fv p n;
      emit cx "  call void @pv_pop_frame(ptr %%ctx, i64 %s)" cx.frame;
      emit cx "  ret i64 %s" imm_unit;
      None)
    else (
      let p, n = arg_buffer cx ops in
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_apply(ptr %%ctx, i64 %s, ptr %s, i64 %d)" t fv p n;
      Some t)
  | A.CIf (a, t, e) ->
    (* A Boolean demand site: a by-need cell reaching the condition (e.g. `letrec b = false in
       if b …`) must be forced before its payload bit is read (mirroring the VM's `Jump_unless`). *)
    let c = force_atom cx env a in
    (* payload != 0 ⇒ true (ADR-0064 §1). *)
    let p = fresh cx in
    emit cx "  %s = ashr i64 %s, 1" p c;
    let b = fresh cx in
    emit cx "  %s = icmp ne i64 %s, 0" b p;
    let lt = fresh_label cx "then"
    and le = fresh_label cx "else" in
    emit cx "  br i1 %s, label %%%s, label %%%s" b lt le;
    if tail
    then (
      emit cx "%s:" lt;
      ignore (expr cx env ~tail:true t);
      emit cx "%s:" le;
      ignore (expr cx env ~tail:true e);
      None)
    else (
      let lend = fresh_label cx "endif" in
      emit cx "%s:" lt;
      let vt = Option.get (expr cx env ~tail:false t) in
      (* the block a value flows from may differ from [lt] after nested control flow *)
      let bt = fresh_label cx "thenv" in
      emit cx "  br label %%%s" bt;
      emit cx "%s:" bt;
      emit cx "  br label %%%s" lend;
      emit cx "%s:" le;
      let ve = Option.get (expr cx env ~tail:false e) in
      let be = fresh_label cx "elsev" in
      emit cx "  br label %%%s" be;
      emit cx "%s:" be;
      emit cx "  br label %%%s" lend;
      emit cx "%s:" lend;
      let r = fresh cx in
      emit cx "  %s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" r vt bt ve be;
      Some r)
  | A.CCtor (name, arity, args) ->
    let nargs = List.length args in
    if nargs <> arity
    then
      (* A partial (or over-) application is a constructor *function* — a closure that accumulates the
         remaining args, then builds the ADT. Deferred: needs a synthetic constructor closure. *)
      failwith
        (Printf.sprintf
           "codegen_llvm: unsaturated constructor %s (%d/%d) not in slice 2"
           name
           nargs
           arity)
    else if arity = 0
    then finish (imm (ctor_index cx name)) (* nullary → an immediate tag (ADR-0064 §1) *)
    else (
      let idx = ctor_index cx name in
      let ops = eval_atoms cx env args in
      let p, n = arg_buffer cx ops in
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_new_adt(ptr %%ctx, i32 %d, ptr %s, i64 %d)" t idx p n;
      finish t)
  | A.CCase (scruts, alts) ->
    (* The CPS cascade (ADR-0072 §5): try each alt in order; a binder mismatch jumps to the next alt;
       an exhausted tail is `pv_case_fail`. Scrutinees are rooted and re-read per alt (`get_current`). *)
    (* matching dereferences the scrutinee's structure, so force a by-need scrutinee. *)
    let scrut_handles = List.map (fun a -> root cx (force_atom cx env a)) scruts in
    let n = List.length alts in
    let try_labels = List.init n (fun _ -> fresh_label cx "alt") in
    let fail_label = fresh_label cx "nomatch" in
    let merge = if tail then "" else fresh_label cx "casejoin" in
    let results = ref [] in
    (* A matched body's value reaches the phi through a fresh single-predecessor block (the CIf idiom). *)
    let run_body env' e =
      if tail
      then ignore (expr cx env' ~tail:true e)
      else (
        let v = Option.get (expr cx env' ~tail:false e) in
        let vb = fresh_label cx "altv" in
        emit cx "  br label %%%s" vb;
        emit cx "%s:" vb;
        emit cx "  br label %%%s" merge;
        results := (v, vb) :: !results)
    in
    emit cx "  br label %%%s" (List.hd try_labels);
    List.iteri
      (fun i (alt : A.alt) ->
         emit cx "%s:" (List.nth try_labels i);
         let next_fail = if i + 1 < n then List.nth try_labels (i + 1) else fail_label in
         let env' =
           List.fold_left2
             (fun env b h -> match_binder cx env ~fail:next_fail b (get_current cx h))
             env
             alt.binders
             scrut_handles
         in
         match alt.result with
         | A.Uncond e -> run_body env' e
         | A.Guarded gs ->
           (* Test guards in order; the first true runs its body, all-false falls to the next alt. *)
           let rec guards = function
             | [] -> emit cx "  br label %%%s" next_fail
             | (g, body) :: rest ->
               (* A Boolean demand site: force a by-need guard result before reading its bit. *)
               let gv = force_value cx (Option.get (expr cx env' ~tail:false g)) in
               let pay = fresh cx in
               emit cx "  %s = ashr i64 %s, 1" pay gv;
               let bb = fresh cx in
               emit cx "  %s = icmp ne i64 %s, 0" bb pay;
               let yes = fresh_label cx "gyes"
               and no = fresh_label cx "gno" in
               emit cx "  br i1 %s, label %%%s, label %%%s" bb yes no;
               emit cx "%s:" yes;
               run_body env' body;
               emit cx "%s:" no;
               guards rest
           in
           guards gs)
      alts;
    emit cx "%s:" fail_label;
    emit cx "  call void @pv_case_fail()";
    emit cx "  unreachable";
    if tail
    then None
    else (
      emit cx "%s:" merge;
      let r = fresh cx in
      let entries =
        List.rev_map (fun (v, b) -> Printf.sprintf "[ %s, %%%s ]" v b) !results
      in
      emit cx "  %s = phi i64 %s" r (String.concat ", " entries);
      Some r)
  | A.CRecord fields ->
    (* Hash each label, sort the (id, value) pairs by *unsigned* id ascending (the runtime `new_record`
       asserts strictly-ascending u64 ids, ADR-0069 §1), and pass parallel id/value buffers. *)
    let pairs = List.map (fun (l, a) -> fnv1a_64 l, a) fields in
    let pairs = List.sort (fun (i1, _) (i2, _) -> Int64.unsigned_compare i1 i2) pairs in
    let n = List.length pairs in
    if n = 0
    then (
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_new_record(ptr %%ctx, ptr null, ptr null, i64 0)" t;
      finish t)
    else (
      let ids = List.map (fun (i, _) -> Int64.to_string i) pairs in
      (* Values are mutually protected: a later `String`/`Number` field must not stale an earlier one. *)
      let vals = eval_atoms cx env (List.map snd pairs) in
      let idp, _ = arg_buffer cx ids in
      let valp, _ = arg_buffer cx vals in
      let t = fresh cx in
      emit
        cx
        "  %s = call i64 @pv_new_record(ptr %%ctx, ptr %s, ptr %s, i64 %d)"
        t
        idp
        valp
        n;
      finish t)
  | A.CAccessor (a, label) ->
    (* A dictionary projection (ADR-0070 §5): force a by-need record before reading its field. *)
    let r = force_atom cx env a in
    let t = fresh cx in
    emit
      cx
      "  %s = call i64 @pv_record_get(ptr %%ctx, i64 %s, i64 %s)"
      t
      r
      (label_id label);
    finish t
  | A.CUpdate (a, ups) ->
    (* Functional update: fold `record_set` (each returns a new record). The accumulator record is rooted
       across each value's evaluation (a `String`/`Number` value may allocate) and reloaded before the
       set; `pv_record_set` self-roots its record + value across its own allocation. The base record is
       forced (a by-need dict update); the update values stay raw (stored, possibly by-need). *)
    let rh = ref (root cx (force_atom cx env a)) in
    List.iter
      (fun (label, va) ->
         let v = atom cx env va in
         let r = get_current cx !rh in
         let t = fresh cx in
         emit
           cx
           "  %s = call i64 @pv_record_set(ptr %%ctx, i64 %s, i64 %s, i64 %s)"
           t
           r
           (label_id label)
           v;
         rh := root cx t)
      ups;
    finish (get_current cx !rh)
  | A.CArray elems ->
    if elems = []
    then (
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_empty_array()" t;
      finish t)
    else (
      let ops = eval_atoms cx env elems in
      let p, n = arg_buffer cx ops in
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_new_array(ptr %%ctx, ptr %s, i64 %d)" t p n;
      finish t)

(* Emit one lifted function: open a frame, root the params (from the arg buffer) and the captured free
   vars (from the closure env block), then compile the body in tail position. *)
let emit_function cx (l : lifted) : unit =
  cx.fn <- Buffer.create 256;
  cx.ssa <- 0;
  let frame = "%frame" in
  cx.frame <- frame;
  emit cx "  %s = call i64 @pv_frame(ptr %%ctx)" frame;
  (* params: arg i ↦ a rooted slot *)
  let env =
    List.mapi (fun i p -> p, i) l.params
    |> List.fold_left
         (fun env (p, i) ->
            let ptr = fresh cx in
            emit cx "  %s = getelementptr i64, ptr %%args, i64 %d" ptr i;
            let v = fresh cx in
            emit cx "  %s = load i64, ptr %s" v ptr;
            bind env p (root cx v))
         []
  in
  (* captures: closure env block (slot 2) then positional reads *)
  let env =
    if l.captures = []
    then env
    else (
      let envb = fresh cx in
      emit cx "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %%clo, i64 2)" envb;
      List.mapi (fun i c -> c, i) l.captures
      |> List.fold_left
           (fun env (c, i) ->
              let v = fresh cx in
              emit cx "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %s, i64 %d)" v envb i;
              bind env c (root cx v))
           env)
  in
  ignore (expr cx env ~tail:true l.body);
  Buffer.add_string
    cx.md
    (Printf.sprintf
       "define i64 @%s(ptr %%ctx, i64 %%clo, ptr %%args, i64 %%nargs) {\nentry:\n%s}\n\n"
       l.name
       (Buffer.contents cx.fn))

(* Drain the pending-lambda queue (each may enqueue more). *)
let rec emit_pending cx : unit =
  match cx.pending with
  | [] -> ()
  | l :: rest ->
    cx.pending <- rest;
    emit_function cx l;
    emit_pending cx

let declarations : string =
  String.concat
    "\n"
    [ "declare ptr @pv_runtime_new(i64)"
    ; "declare void @pv_runtime_free(ptr)"
    ; "declare i64 @pv_apply(ptr, i64, ptr, i64)"
    ; "declare void @pv_tailcall(ptr, i64, ptr, i64)"
    ; "declare i64 @pv_make_closure(ptr, i64, i32, i64)"
    ; "declare i64 @pv_frame(ptr)"
    ; "declare i64 @pv_root(ptr, i64)"
    ; "declare i64 @pv_get(ptr, i64)"
    ; "declare void @pv_pop_frame(ptr, i64)"
    ; "declare i64 @pv_new_array(ptr, ptr, i64)"
    ; "declare i64 @pv_new_adt(ptr, i32, ptr, i64)"
    ; "declare i64 @pv_new_record(ptr, ptr, ptr, i64)"
    ; "declare i64 @pv_new_str(ptr, ptr, i64)"
    ; "declare i64 @pv_new_number(ptr, i64)"
    ; "declare i64 @pv_foreign(ptr, ptr, i64)"
    ; "declare i64 @pv_record_get(ptr, i64, i64)"
    ; "declare i64 @pv_record_set(ptr, i64, i64, i64)"
    ; "declare i64 @pv_read_field(ptr, i64, i64)"
    ; "declare void @pv_write_field(ptr, i64, i64, i64)"
    ; "declare i64 @pv_read_raw(ptr, i64, i64)"
    ; "declare void @pv_case_fail()"
    ; "declare i64 @pv_run_effect(ptr, i64)"
    ; "declare void @pv_drain_output(ptr)"
    ; "declare void @pv_print_int(i64)"
    ; "declare i64 @pv_prim_add_int(i64, i64)"
    ; "declare i64 @pv_prim_sub_int(i64, i64)"
    ; "declare i64 @pv_prim_mul_int(i64, i64)"
    ; "declare i64 @pv_prim_div_int(i64, i64)"
    ; "declare i64 @pv_prim_mod_int(i64, i64)"
    ; "declare i64 @pv_prim_and_int(i64, i64)"
    ; "declare i64 @pv_prim_or_int(i64, i64)"
    ; "declare i64 @pv_prim_xor_int(i64, i64)"
    ; "declare i64 @pv_prim_shl_int(i64, i64)"
    ; "declare i64 @pv_prim_shr_int(i64, i64)"
    ; "declare i64 @pv_prim_zshr_int(i64, i64)"
    ; "declare i64 @pv_prim_complement_int(i64)"
    ; "declare i64 @pv_prim_eq_int(i64, i64)"
    ; "declare i64 @pv_prim_lt_int(i64, i64)"
    ; "declare i64 @pv_prim_eq_bool(i64, i64)"
    ; "declare i64 @pv_prim_and_bool(i64, i64)"
    ; "declare i64 @pv_prim_or_bool(i64, i64)"
    ; "declare i64 @pv_prim_not_bool(i64)"
    ; "declare i64 @pv_prim_add_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_sub_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_mul_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_div_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_int_to_number(ptr, i64)"
    ; "declare i64 @pv_prim_number_to_int(ptr, i64)"
    ; "declare i64 @pv_prim_eq_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_lt_number(ptr, i64, i64)"
    ; "declare i64 @pv_prim_eq_string(ptr, i64, i64)"
    ; "declare i64 @pv_prim_lt_string(ptr, i64, i64)"
    ; "declare i64 @pv_prim_append(ptr, i64, i64)"
    ; "declare i64 @pv_prim_index_array(ptr, i64, i64)"
    ; "declare i64 @pv_prim_length_array(ptr, i64)"
    ; "declare i64 @pv_prim_new_array(ptr, i64)"
    ; "declare i64 @pv_prim_set_array(ptr, i64, i64, i64)"
    ; "declare i64 @pv_prim_record_get(ptr, i64, i64)"
    ; "declare i64 @pv_prim_record_set(ptr, i64, i64, i64)"
    ; "declare i64 @pv_prim_record_has(ptr, i64, i64)"
    ; "declare i64 @pv_prim_record_delete(ptr, i64, i64)"
    ; "declare i64 @pv_empty_array()"
    ; "declare i64 @pv_new_byneed_placeholder(ptr)"
    ; "declare void @pv_byneed_set_suspension(ptr, i64, i64)"
    ; "declare i64 @pv_force_if_byneed(ptr, i64)"
    ]

(* Default heap size in words per semi-space for the entry runtime — a generous fixed size for v1
   (ADR-0066 §4). Overridable per program (a small heap forces GC, exercising the emitted rooting). *)
let default_heap_words = 1 lsl 20

(** Emit a whole ANF program as a self-contained LLVM IR module string. A **pure** entry (`is_effect =
    false`) computes its `Int` value and prints it (`pv_print_int`, matching the oracle's `to_string`); an
    **`Effect`** entry runs `pv_run_effect` then drains the output sink (ADR-0072 §8). The entry body is
    compiled in non-tail (value) position, then the stub prints/runs, frees the runtime, and returns. *)
let program ?(is_effect = false) ?(heap_words = default_heap_words) (e : A.expr) : string =
  let cx =
    { md = Buffer.create 4096
    ; globals = Buffer.create 256
    ; fn = Buffer.create 256
    ; ssa = 0
    ; lbl = 0
    ; fns = 0
    ; strs = 0
    ; pending = []
    ; frame = "%frame"
    ; ctors = Hashtbl.create 16
    }
  in
  (* the entry stub's own body buffer *)
  cx.frame <- "%frame";
  emit cx "  %%ctx = call ptr @pv_runtime_new(i64 %d)" heap_words;
  emit cx "  %%frame = call i64 @pv_frame(ptr %%ctx)";
  let v = Option.get (expr cx [] ~tail:false e) in
  if is_effect
  then (
    let r = fresh cx in
    emit cx "  %s = call i64 @pv_run_effect(ptr %%ctx, i64 %s)" r v;
    emit cx "  call void @pv_drain_output(ptr %%ctx)")
  else
    (* The program's final result is an `Int` demand site: force a by-need cell (e.g. `letrec x = 7 in
       x`, or a call returning a by-need value) before `pv_print_int` reads its payload. The `Effect`
       path needs no force — `pv_run_effect`→`pv_apply` auto-forces a by-need callee. *)
    emit cx "  call void @pv_print_int(i64 %s)" (force_value cx v);
  emit cx "  call void @pv_pop_frame(ptr %%ctx, i64 %%frame)";
  emit cx "  call void @pv_runtime_free(ptr %%ctx)";
  emit cx "  ret i32 0";
  let entry_body = Buffer.contents cx.fn in
  (* emit the lifted functions the entry (and transitively) queued *)
  emit_pending cx;
  Printf.sprintf
    "; ModuleID = 'purvasm'\n\n%s\n\n%s\n%s\ndefine i32 @main() {\nentry:\n%s}\n"
    declarations
    (Buffer.contents cx.globals)
    (Buffer.contents cx.md)
    entry_body
