(** ANF → LLVM textual IR (ADR-0072), the native backend that lowers onto the owned Rust runtime
    (ADR-0071). Slice 1 (ADR-0072 §10): the **pure first-order** subset — literals (`Int`/`Boolean`),
    arithmetic/comparison primops, `let`, `if`, uncurried lambdas (lambda-lifted), and application
    through the eval/`apply` trampoline. ADTs / `case` / records / `letrec` / by-need / strings and the
    full leaf set land in later slices (a clear [failwith] guards each until then).

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

(* [(symbol, needs_ctx)]; scalar ops are pure (no ctx / no safepoint), heap ops take the ctx. Only the
   slice-1 pure arithmetic/comparison set is wired; the allocating ones arrive with their slices. *)
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
  | op ->
    (* Number/String/Array/Record primops need the ctx + allocation; wired with their slices. *)
    failwith
      (Printf.sprintf
         "codegen_llvm: primop not in slice 1 (%s)"
         (match op with
          | C.AddNumber -> "AddNumber"
          | C.SubNumber -> "SubNumber"
          | C.MulNumber -> "MulNumber"
          | C.DivNumber -> "DivNumber"
          | C.IntToNumber -> "IntToNumber"
          | C.NumberToInt -> "NumberToInt"
          | C.EqNumber -> "EqNumber"
          | C.EqString -> "EqString"
          | C.LtNumber -> "LtNumber"
          | C.LtString -> "LtString"
          | C.Append -> "Append"
          | C.IndexArray -> "IndexArray"
          | C.LengthArray -> "LengthArray"
          | C.NewArray -> "NewArray"
          | C.SetArray -> "SetArray"
          | C.RecordGet -> "RecordGet"
          | C.RecordSet -> "RecordSet"
          | C.RecordHas -> "RecordHas"
          | C.RecordDelete -> "RecordDelete"
          | _ -> "?"))

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
  ; mutable fn : Buffer.t (* the current function body *)
  ; mutable ssa : int
  ; mutable lbl : int
  ; mutable fns : int (* lifted-function counter *)
  ; mutable pending : lifted list (* lambdas to emit *)
  ; mutable frame : string (* the current function's shadow-stack frame handle operand *)
  }

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
   reloads the current value via [pv_get]. *)
type env = (string * string) list

let bind (env : env) (x : string) (handle : string) : env = (x, handle) :: env

(* Read a variable's current value (post-safepoint) through its root handle. *)
let read_var cx (env : env) (x : string) : string =
  match List.assoc_opt x env with
  | Some h ->
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_get(ptr %%ctx, i64 %s)" t h;
    t
  | None ->
    failwith (Printf.sprintf "codegen_llvm: unbound variable %s (unresolved foreign?)" x)

let atom cx (env : env) : A.atom -> string = function
  | A.AVar x -> read_var cx env x
  | A.ALit (C.LInt n) -> imm_int n
  | A.ALit (C.LBool b) -> imm_bool b
  | A.ALit (C.LNumber _) -> failwith "codegen_llvm: Number literal not in slice 1"
  | A.ALit (C.LString _) -> failwith "codegen_llvm: String literal not in slice 1"
  | A.AForeign k -> failwith (Printf.sprintf "codegen_llvm: foreign %s not in slice 1" k)

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

(* Compile [e]; [tail] = it is in the enclosing function's tail position. Returns [Some operand] when
   not tail (the produced value), [None] when tail (a `ret`/tail-call was emitted). *)
let rec expr cx (env : env) ~(tail : bool) (e : A.expr) : string option =
  match e with
  | A.Ret c -> cexpr cx env ~tail c
  | A.Let (x, c, body) ->
    let v = Option.get (cexpr cx env ~tail:false c) in
    let h = root cx v in
    expr cx (bind env x h) ~tail body
  | A.LetRec _ -> failwith "codegen_llvm: letrec not in slice 1"

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
    let ops = List.map (atom cx env) args in
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
    let fv = atom cx env f in
    let ops = List.map (atom cx env) args in
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
    let c = atom cx env a in
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
  | A.CCtor _ -> failwith "codegen_llvm: constructor not in slice 1"
  | A.CArray _ -> failwith "codegen_llvm: array not in slice 1"
  | A.CRecord _ | A.CAccessor _ | A.CUpdate _ ->
    failwith "codegen_llvm: records not in slice 1"
  | A.CCase _ -> failwith "codegen_llvm: case not in slice 1"

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
    ; "declare i64 @pv_read_field(ptr, i64, i64)"
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
    ]

(* Heap size in words per semi-space for the entry runtime. A generous fixed size for v1 (ADR-0066 §4). *)
let heap_words = 1 lsl 20

(** Emit a whole ANF program as a self-contained LLVM IR module string. A **pure** entry (`is_effect =
    false`) computes its `Int` value and prints it (`pv_print_int`, matching the oracle's `to_string`); an
    **`Effect`** entry runs `pv_run_effect` then drains the output sink (ADR-0072 §8). The entry body is
    compiled in non-tail (value) position, then the stub prints/runs, frees the runtime, and returns. *)
let program ?(is_effect = false) (e : A.expr) : string =
  let cx =
    { md = Buffer.create 4096
    ; fn = Buffer.create 256
    ; ssa = 0
    ; lbl = 0
    ; fns = 0
    ; pending = []
    ; frame = "%frame"
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
  else emit cx "  call void @pv_print_int(i64 %s)" v;
  emit cx "  call void @pv_pop_frame(ptr %%ctx, i64 %%frame)";
  emit cx "  call void @pv_runtime_free(ptr %%ctx)";
  emit cx "  ret i32 0";
  let entry_body = Buffer.contents cx.fn in
  (* emit the lifted functions the entry (and transitively) queued *)
  emit_pending cx;
  Printf.sprintf
    "; ModuleID = 'purvasm'\n\n%s\n\n%s\ndefine i32 @main() {\nentry:\n%s}\n"
    declarations
    (Buffer.contents cx.md)
    entry_body
