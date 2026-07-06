(** ANF → LLVM textual IR (ADR-0072), the native backend that lowers onto the owned Rust runtime
    (ADR-0071). Implemented so far (ADR-0072 §10 slice plan):
    - **Slice 1 — pure first-order**: literals (`Int`/`Boolean`), arithmetic/comparison primops, `let`,
      `if`, uncurried lambdas (lambda-lifted), and application through the eval/`apply` trampoline.
    - **Slice 2 — ADTs + `case`**: constructors (nullary → an immediate tag, saturated field-carrying →
      `pv_new_adt`, **unsaturated → a builder closure the runtime under-applies to a PAP**, ADR-0072 §5),
      and `case` lowered from the shared decision tree (`Middle_end.Match_compile`, ADR-0083): a `switch`
      per discriminant (ctor split by representation, then tag; boxed-literal equality chain for
      `Number`/`String`), with guarded alternatives. Occurrences are rooted and reloaded per safepoint.
    - **Slice 3 — static records**: literals (`pv_new_record` over compile-time FNV-1a-64 label ids,
      sorted), the static `Accessor` and functional `Update` (id-keyed `pv_record_get`/`pv_record_set`),
      and the `BRecord` row-poly binder.
    - **Slice 4 — strings, numbers, arrays, leaves, `Effect`**: `String`/`Number` literals
      (`pv_new_str`/`pv_new_number`), `Array` literals, the full primop set (Number/String/Array + the
      dynamic `String`-keyed record ops), `foreign` leaves (link-time `pvf_*` symbols, ADR-0073 §3), and the `Effect` entry
      (`pv_run_effect` + drain to `stdout`). Multi-atom sites are `eval_atoms`-protected (root-on-collect,
      since literals now allocate); a small-heap fixture exercises the rooting under a forced collection.
    - **Slice 5a — recursive groups (`letrec`)**: a group is all-by-need `ByNeed` cells over one shared
      env (ADR-0070 §4), so function members recurse (self/sibling) via the env and `apply` auto-forces a
      by-need callee, while a value member (a dictionary) is a cell forced at each value-dereference site
      by `pv_force_if_byneed` (robust to a cell reaching a projection / `case` / primop through an
      argument or data field, without static tracking). The **Boolean demand sites** (`if`/guard
      conditions) and the **program's final result** force likewise; construction sites
      (`Ctor`/`Record`/`Array` fields, `apply` head/args) keep the raw cell (the knot-tie).
    - **Slice 5b — per-module linking (Part B)**: [program_modular] emits every top-level binding as a
      root-handle global `@<mangle key>$root` (a shadow-stack handle, not a stale raw value under the moving
      GC) with a `@<mangle key>$init` unit run by a `pv_init_all`, and references load the handle +
      `pv_get` (ADR-0072 §2/§3). [program_split] splits this across independent per-module `.o`s + one
      init/entry object linked by the injective-mangle symbol ABI. `pv_init_all` initialises **only the
      bindings reachable from the entry** ([reachable_gdefs]); modules still emit all their bindings, so a
      pruned dead binding's now-unreferenced code/globals are removed by the system linker's dead-strip
      (`-Wl,-dead_strip`). Init order is linked-spine order restricted to the reachable set (a valid
      topological order). A **local variable may share a name with a bare-key top-level global** (a
      `where`-hoisted helper like `go` collides with another definition's local `go`); [read_var] resolves
      the local first (env before `gkeys`), and lambda-lift ([lift]) captures such a shadowed name rather
      than reading `@<mangle>$root` — excluding it as a global would make a nested lambda call the wrong
      binding (a partial application leaking out of a mis-resolved recursive helper).
    Pure-`Number`/`String`/`Bool` entry printers land later (a `pv_print_int`-only guard). A top-level
    **bare-key shadow** — two top-level bindings sharing an unqualified key (both `where`-hoisted to the
    same name), which the flat root-handle-global scheme cannot double-define — is resolved before
    splitting by [uniquify_toplevel] (spine-level alpha-renaming of the shadowing binding).

    Every guest value is one `i64` tagged word (ADR-0064 §1); the module calls the runtime's `extern "C"`
    surface (`pv_*`, ADR-0071). Rooting is **root-on-create** (ADR-0072 §6, the conservative first cut):
    each function opens a shadow-stack frame, every bound value is `pv_root`ed and read back with `pv_get`,
    and every return pops the frame — so any value trivially survives any safepoint. Minimising this to
    "root only across a safepoint" is a later perf slice. Nested lambdas are hoisted to top-level
    functions with an explicit captured-env array (ADR-0072 §4). *)

module A = Middle_end.Anf
module C = Cesk.Ast
module MC = Middle_end.Match_compile
module SS = Set.Make (String)
module SM = Map.Make (String)

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

(* A constructor's runtime tag — a **deterministic** hash of its name (ADR-0064 §1/§2), so a ctor built in
   one independently-compiled module matches the same ctor pattern-matched in another (§2 cross-module
   linking; record labels hash the same way, ADR-0069 §2). Masked to 31 bits so a nullary ctor's immediate
   `(tag << 1) | 1` stays inside the 63-bit payload; only construct/match *consistency* matters (the tag is
   internal, never observed). *)
let ctor_tag (name : string) : int =
  Int64.to_int (Int64.logand (fnv1a_64 name) 0x7fffffffL)

(* --- symbol mangling (ADR-0072 §2): a qualified key → a legal, **injective** LLVM link symbol. This is a
   cross-`.o` ABI (B2 resolves `@<mangle>$root` between objects), so the map must be both a valid LLVM
   identifier and collision-free. An alphabetic prefix `pv_g_` avoids a leading digit (LLVM rejects
   `@1_foo`) and namespaces the symbol away from the runtime's `pv_*`; every non-alphanumeric byte —
   **including `_` itself** — is escaped as `_HH` (hex), so the escape is unambiguous and distinct keys can
   never collide (e.g. `A.B` → `pv_g_A_2eB`, `A_B` → `pv_g_A_5fB`). The code symbol is `@<mangle>`, its
   root-handle global `@<mangle>$root`, its init function `@<mangle>$init` (`$` is a legal LLVM identifier
   char). ------------------------------------------------------------------------------------------------ *)

(* The injective identifier escape (ADR-0072 §2): alphanumerics pass through, every other byte (including
   `_` itself) becomes `_HH`, so distinct keys never collide. Shared by the global mangle (`pv_g_` prefix)
   and the foreign-symbol mangle (`pvf_` prefix, ADR-0073 §3). *)
let escape_ident (key : string) : string =
  let b = Buffer.create (String.length key + 8) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char b c
       | _ -> Buffer.add_string b (Printf.sprintf "_%02x" (Char.code c)))
    key;
  Buffer.contents b

(* A top-level binding's linker symbol base: `pv_g_<escape key>` (ADR-0072 §2). *)
let mangle (key : string) : string = "pv_g_" ^ escape_ident key

(* A native foreign leaf's `AbiCodeFn` linker symbol: `pvf_<escape key>` (ADR-0073 §3). The runtime (and,
   later, a ulib's compiled `.c`) exports the leaf under exactly this name; codegen references it directly,
   so foreign resolution is by the linker, not a runtime string dispatch. *)
let mangle_foreign (key : string) : string = "pvf_" ^ escape_ident key

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

(* --- referenced native foreign keys (ADR-0073 §3) -------------------------------------------------- *)

(* Every native foreign key an expression references. The native build compiles the ulib `.c` implementing
   each reachable key and links it (ADR-0073 §2/§3); a superset (a key in a dead binding) is harmless — the
   dead-strip link drops an unreferenced leaf's object. Keys with no ulib `.c` are left to the linker to
   resolve from the runtime staticlib (a truly unbound key becomes a link error). *)
let rec cf_expr (e : A.expr) : SS.t =
  match e with
  | A.Ret c -> cf_cexpr c
  | A.Let (_, c, body) -> SS.union (cf_cexpr c) (cf_expr body)
  | A.LetRec (binds, body) ->
    List.fold_left (fun a (_, r) -> SS.union a (cf_expr r)) (cf_expr body) binds

and cf_cexpr (c : A.cexpr) : SS.t =
  match c with
  | A.CAtom a -> cf_atom a
  | A.CLam (_, b) -> cf_expr b
  | A.CApp (f, args) ->
    List.fold_left (fun s a -> SS.union s (cf_atom a)) (cf_atom f) args
  | A.CPrim (_, args) | A.CArray args | A.CCtor (_, _, args) -> cf_atoms args
  | A.CRecord fs -> List.fold_left (fun s (_, a) -> SS.union s (cf_atom a)) SS.empty fs
  | A.CUpdate (a0, fs) ->
    List.fold_left (fun s (_, a) -> SS.union s (cf_atom a)) (cf_atom a0) fs
  | A.CAccessor (a, _) -> cf_atom a
  | A.CIf (a, t, e) -> SS.union (cf_atom a) (SS.union (cf_expr t) (cf_expr e))
  | A.CCase (scruts, alts) ->
    List.fold_left (fun s alt -> SS.union s (cf_alt alt)) (cf_atoms scruts) alts

and cf_atom : A.atom -> SS.t = function
  | A.AForeign k -> SS.singleton k
  | A.AVar _ | A.ALit _ -> SS.empty

and cf_atoms xs = List.fold_left (fun s a -> SS.union s (cf_atom a)) SS.empty xs

and cf_alt (alt : A.alt) : SS.t =
  match alt.result with
  | A.Uncond e -> cf_expr e
  | A.Guarded gs ->
    List.fold_left
      (fun s (g, e) -> SS.union s (SS.union (cf_expr g) (cf_expr e)))
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
  | C.RecordUnion -> "pv_prim_record_union", true

(* --- the emitter ----------------------------------------------------------------------------------- *)

(* How a direct call site obtains the callee's env word (ADR-0076 §2). *)
type env_src =
  | SSelf (* the enclosing direct entry's own `%env` (a self-call) *)
  | SSentinel
    (* the no-capture immediate sentinel (top-level `Gfun`, no-capture lambdas) *)
  | SClosureEnv (* read the closure value's env slot (a capturing let-bound lambda) *)
  | SForceCell
(* force the `ByNeed` cell, then read the forced closure's env slot (a `Grec` member) *)

(* A statically-known function a saturated call can enter directly: its direct-entry symbol, its
   arity, and how to obtain the env operand. *)
type fn_info =
  { dsym : string
  ; arity : int
  ; src : env_src
  }

(* A lifted lambda awaiting emission: its global name, params, captured free vars (in a fixed order),
   and body. Every lifted function is emitted as **two entries** (ADR-0076 §1): the `tailcc` direct
   entry `@<name>$d(ctx, env, a0…)` holding the body, and the generic `AbiCodeFn` `@<name>` as a thin
   unpack-and-call wrapper (the closure's `code` word keeps pointing at the wrapper, so every dynamic
   path is unchanged). [self_name] is the source binding this lambda is the RHS of (a recursive-group
   function member), enabling the self-call shortcut (own `$d` + own `%env`, ADR-0076 §2). *)
type lifted =
  { name : string
  ; params : string list
  ; captures : string list
  ; body : lifted_body
  ; self_name : string option
  ; capture_fns : (string * fn_info) list
    (* direct-call info for captures that are known recursive-group function members (the
       mutual-recursion case: a sibling reached through the shared env, ADR-0076 §2) *)
  ; exported : bool
    (* the direct entry `$d` is part of the module's export surface (ADR-0077 §3): external
       linkage, so other modules' objects can call it by symbol. Lifted internals and all
       wrappers stay [internal]. *)
  }

(* [LBody] is an ordinary lambda body. [LClosure lm] is a `Grec` function member's suspension body:
   forcing the member's cell builds `lm`'s closure — pre-lifting `lm` under a stable name is what
   lets call sites target its direct entry (ADR-0076 §2). *)
and lifted_body =
  | LBody of A.expr
  | LClosure of lifted

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
  ; gkeys : SS.t
    (* top-level qualified keys → referenced as `@<mangle>$root` globals (ADR-0072 §2) *)
  ; mutable externs :
      SS.t (* referenced globals not defined here → emitted as `external` decls *)
  ; mutable foreigns : SS.t
    (* referenced native foreign keys → declared as `pvf_<mangle>` externals (ADR-0073 §3) *)
  ; mutable gfns : fn_info SM.t
    (* this module's own top-level function bindings, callable directly when saturated
       (ADR-0076 §2 — each per-module [cx] registers only its own gdefs; checked first) *)
  ; xfns : fn_info SM.t
    (* the program's cross-module export surface (ADR-0077 §2): exported top-level function
       key → direct-entry info, exactly what the dependency's `.pmi` publishes (`Efn` →
       [SSentinel], `Erecfn` → [SForceCell]) — spine-derived today, file-read tomorrow *)
  ; mutable xdecls : int SM.t
    (* referenced cross-module direct entries (`$d` symbol → arity) → emitted as
       per-signature `declare tailcc` externs (ADR-0077 §3) *)
  ; mutable self_ctx : (string * string option * string * fn_info) option
    (* the binding whose lambda is being emitted: (name, its entry-time capture handle if the
       name is captured — [None] when it resolves as a global —, the rooted handle of this
       activation's `%env` word — reloaded at self-calls, since the raw `%env` SSA value is
       stale after any safepoint —, its own direct-entry info) *)
  ; mutable in_direct : bool
    (* emitting a `tailcc` direct entry (so `%env` exists and `musttail` is legal) *)
  ; inline_abi : bool
    (* release mode (ADR-0079): emit the rooting/trampoline fast paths as inline IR against
       the `pv_ctx_header`; [false] under `--debug` — every operation stays an entry call
       (the pre-0079 IR), pairing with the generation-checking debug runtime *)
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

(* --- the ctx-header ABI fast paths (ADR-0079 §3) ---------------------------------------------------

   In release mode ([inline_abi]) the five rooting/trampoline operations are plain IR against the
   `pv_ctx_header` at offset 0 of `%ctx`: the shadow stack is `roots_base[0..roots_len]`, a handle
   is the bare slot index, a frame mark is a length, `pending_tail` is the settle flag. The offsets
   and version below are LOCKSTEP with `include/purvasm.h` (and the runtime's `CtxHeader` /
   `purvasm-sys`'s mirror, each pinned by compile-time layout assertions). Slow paths — growth, an
   actually-stashed tail — call the unchanged entries. Under `--debug` every operation is an entry
   call (the pre-0079 IR): the debug runtime packs generations into handles and keeps `root_gens`
   bookkeeping the inline paths would not update (ADR-0079 §2), and the profile-gated
   `pv_ctx_abi_v<N>` link stamp turns a wrong pairing into a link error. *)

let ctx_header_version = 1
let off_roots_len = 8
let off_roots_cap = 16
let off_pending_tail = 24
(* `roots_base` sits at offset 0: its load reads `ptr %ctx` directly. *)

(* A header field's address, computed at the use site — the base moves on growth, so nothing is
   cached across calls; folding the reloads between safepoints is the optimiser's job (§3). *)
let header_field cx (off : int) : string =
  let a = fresh cx in
  emit cx "  %s = getelementptr i8, ptr %%ctx, i64 %d" a off;
  a

let abi_frame_open cx : string =
  if not cx.inline_abi
  then (
    let m = fresh cx in
    emit cx "  %s = call i64 @pv_frame(ptr %%ctx)" m;
    m)
  else (
    let m = fresh cx in
    emit cx "  %s = load i64, ptr %s" m (header_field cx off_roots_len);
    m)

let abi_pop_frame cx (mark : string) : unit =
  if not cx.inline_abi
  then emit cx "  call void @pv_pop_frame(ptr %%ctx, i64 %s)" mark
  else emit cx "  store i64 %s, ptr %s" mark (header_field cx off_roots_len)

let abi_get cx (handle : string) : string =
  if not cx.inline_abi
  then (
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_get(ptr %%ctx, i64 %s)" t handle;
    t)
  else (
    let base = fresh cx in
    emit cx "  %s = load ptr, ptr %%ctx" base;
    let slot = fresh cx in
    emit cx "  %s = getelementptr i64, ptr %s, i64 %s" slot base handle;
    let t = fresh cx in
    emit cx "  %s = load i64, ptr %s" t slot;
    t)

(* The in-capacity store is the fast path; `len == cap` falls to the entry, which grows (the one
   action that moves `roots_base`) and returns the same bare-index handle the fast path computes. *)
let abi_root cx (v : string) : string =
  if not cx.inline_abi
  then (
    let h = fresh cx in
    emit cx "  %s = call i64 @pv_root(ptr %%ctx, i64 %s)" h v;
    h)
  else (
    let chk = fresh_label cx "rchk" in
    let fast = fresh_label cx "rfast" in
    let slow = fresh_label cx "rslow" in
    let done_ = fresh_label cx "rdone" in
    emit cx "  br label %%%s" chk;
    emit cx "%s:" chk;
    let lenp = header_field cx off_roots_len in
    let len = fresh cx in
    emit cx "  %s = load i64, ptr %s" len lenp;
    let cap = fresh cx in
    emit cx "  %s = load i64, ptr %s" cap (header_field cx off_roots_cap);
    let full = fresh cx in
    emit cx "  %s = icmp eq i64 %s, %s" full len cap;
    emit cx "  br i1 %s, label %%%s, label %%%s" full slow fast;
    emit cx "%s:" fast;
    let base = fresh cx in
    emit cx "  %s = load ptr, ptr %%ctx" base;
    let slot = fresh cx in
    emit cx "  %s = getelementptr i64, ptr %s, i64 %s" slot base len;
    emit cx "  store i64 %s, ptr %s" v slot;
    let len1 = fresh cx in
    emit cx "  %s = add i64 %s, 1" len1 len;
    emit cx "  store i64 %s, ptr %s" len1 lenp;
    emit cx "  br label %%%s" done_;
    emit cx "%s:" slow;
    let hs = fresh cx in
    emit cx "  %s = call i64 @pv_root(ptr %%ctx, i64 %s)" hs v;
    emit cx "  br label %%%s" done_;
    emit cx "%s:" done_;
    let h = fresh cx in
    emit cx "  %s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" h len fast hs slow;
    h)

let abi_settle cx (r : string) : string =
  if not cx.inline_abi
  then (
    let t = fresh cx in
    emit cx "  %s = call i64 @pv_settle(ptr %%ctx, i64 %s)" t r;
    t)
  else (
    let chk = fresh_label cx "schk" in
    let slow = fresh_label cx "sslow" in
    let done_ = fresh_label cx "sdone" in
    emit cx "  br label %%%s" chk;
    emit cx "%s:" chk;
    let pf = fresh cx in
    emit cx "  %s = load i64, ptr %s" pf (header_field cx off_pending_tail);
    let has = fresh cx in
    emit cx "  %s = icmp ne i64 %s, 0" has pf;
    emit cx "  br i1 %s, label %%%s, label %%%s" has slow done_;
    emit cx "%s:" slow;
    let rs = fresh cx in
    emit cx "  %s = call i64 @pv_settle(ptr %%ctx, i64 %s)" rs r;
    emit cx "  br label %%%s" done_;
    emit cx "%s:" done_;
    let t = fresh cx in
    emit cx "  %s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" t r chk rs slow;
    t)

(* env: a guest variable → the SSA operand holding its **root handle** (root-on-create), plus the
   static function info when the binding is a known lambda (ADR-0076 §2 — a direct-call candidate).
   A reference reloads the current value via [pv_get]. A by-need cell may reach a value-dereference
   site through any binding (a member, a function argument, a data field), so forcing is a *runtime*
   check at the site ([force_atom] → `pv_force_if_byneed`), not a static per-binding flag. *)
type env_entry =
  { handle : string
  ; known_fn : fn_info option
  }

type env = (string * env_entry) list

let bind (env : env) (x : string) (handle : string) : env =
  (x, { handle; known_fn = None }) :: env

(* Bind a variable that is statically a known lambda (its saturated calls may go direct). *)
let bind_fn (env : env) (x : string) (handle : string) (fn : fn_info) : env =
  (x, { handle; known_fn = Some fn }) :: env

(* Read a variable's current value (post-safepoint). A **local** reloads its value through its root handle
   ([pv_get]); a **top-level global** (a qualified key in [gkeys], ADR-0072 §2) loads its persistent
   root-handle global `@<mangle>$root` and `pv_get`s that — relocation-correct, since a static global is
   not itself a GC root (§3). Raw: a by-need cell stays a cell (it propagates and `apply` auto-forces it in
   callee position). Locals use bare keys and globals qualified keys ([lower.ml]), so they never collide;
   the local lookup is tried first regardless. *)
let read_var cx (env : env) (x : string) : string =
  match List.assoc_opt x env with
  | Some { handle; _ } -> abi_get cx handle
  | None when SS.mem x cx.gkeys ->
    cx.externs <- SS.add x cx.externs;
    let handle = fresh cx in
    emit cx "  %s = load i64, ptr @%s$root" handle (mangle x);
    abi_get cx handle
  | None ->
    failwith (Printf.sprintf "codegen_llvm: unbound variable %s (unresolved foreign?)" x)

(* Emit a module-level byte constant for [s] and return `(ptr-to-first-byte, len)`. Used for `String`
   literals (`pv_new_str`). The empty string needs no global — a null pointer with length 0 is the
   runtime's empty-buffer convention. *)
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
    (* A native foreign leaf resolves by **link-time symbol** (ADR-0073 §3): reference its `AbiCodeFn`
       `@pvf_<mangle key>` and wrap it in a no-capture closure of the leaf's arity (from the `Ffi`
       registry — boot's metadata source, ADR-0073 §3). Its arity is fixed here at the reference. *)
    cx.foreigns <- SS.add k cx.foreigns;
    let addr = fresh cx in
    emit cx "  %s = ptrtoint ptr @%s to i64" addr (mangle_foreign k);
    let clo = fresh cx in
    emit
      cx
      "  %s = call i64 @pv_make_closure(ptr %%ctx, i64 %s, i32 %d, i64 %s)"
      clo
      addr
      (Ffi.foreign_arity k)
      imm_unit;
    clo

(* An atom evaluated at a **value-dereference** site (ADR-0070 §3): a dictionary read for a method, a
   `case` scrutinee, a primop operand. A variable might hold a by-need cell (a recursive member, or one
   passed through an argument / data field), so it is run through `pv_force_if_byneed` — a runtime check
   that forces a cell to its value and passes anything else through (literals never are cells).
   Construction sites (`Ctor`/`Record`/`Array` fields, `apply` head/args) use the raw [atom], so the cell
   propagates and the knot-tie stores it unforced. *)
(* Force an arbitrary value iff it is a by-need cell, with the **immediate fast path inline**
   (ADR-0072 §6): an immediate word (LSB = 1 — the tag bit, the very encoding [imm_int]
   emits) can never be a cell, so the extern crossing (and its guard frame) is paid only for
   pointer words. The slow path is byte-for-byte the previous behaviour and remains the only
   safepoint, so callers' conservative safepoint accounting (e.g. [eval_atoms] under
   [~force]) is unchanged. The explicit entry block gives the phi its predecessor label. *)
let force_value cx (v : string) : string =
  let chk = fresh_label cx "fchk" in
  let slow = fresh_label cx "fslow" in
  let done_ = fresh_label cx "fdone" in
  emit cx "  br label %%%s" chk;
  emit cx "%s:" chk;
  let bit = fresh cx in
  emit cx "  %s = and i64 %s, 1" bit v;
  let imm = fresh cx in
  emit cx "  %s = icmp ne i64 %s, 0" imm bit;
  emit cx "  br i1 %s, label %%%s, label %%%s" imm done_ slow;
  emit cx "%s:" slow;
  let forced = fresh cx in
  emit cx "  %s = call i64 @pv_force_if_byneed(ptr %%ctx, i64 %s)" forced v;
  emit cx "  br label %%%s" done_;
  emit cx "%s:" done_;
  let r = fresh cx in
  emit cx "  %s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" r v chk forced slow;
  r

let force_atom cx (env : env) (a : A.atom) : string =
  match a with
  | A.AVar _ -> force_value cx (atom cx env a)
  | A.ALit _ | A.AForeign _ -> atom cx env a

(* Root a freshly produced value and return its handle operand (root-on-create). *)
let root cx (v : string) : string = abi_root cx v

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

(* Register a lambda for hoisting and return the [lifted] record (its captures fixed in a stable order).
   [env] is the enclosing local scope at the `CLam` site. *)
let lift cx (env : env) (params : string list) (body : A.expr) : lifted =
  cx.fns <- cx.fns + 1;
  let bound = List.fold_left (fun s p -> SS.add p s) SS.empty params in
  (* A top-level global is read via its `@<mangle>$root` handle at the reference (ADR-0072 §2), never
     captured — subtract it from the free-var set. But a **global whose name is shadowed by an enclosing
     local** (a bare-key top-level binding colliding with a local variable, e.g. a `where`-hoisted `go`
     against another definition's local `go`) must still be **captured**: it is lexically the local, and
     [read_var] resolves the local from the closure env. So exclude only the globals *not* shadowed by a
     local in scope — else the lifted body would read the wrong `@<mangle>$root`. *)
  let local_names = List.fold_left (fun s (x, _) -> SS.add x s) SS.empty env in
  let globals_unshadowed = SS.diff cx.gkeys local_names in
  let captures = SS.elements (SS.diff (fv_expr bound body) globals_unshadowed) in
  let l =
    { name = Printf.sprintf "fn_%d" cx.fns
    ; params
    ; captures
    ; body = LBody body
    ; self_name = None
    ; capture_fns = []
    ; exported = false
    }
  in
  cx.pending <- l :: cx.pending;
  l

(* Emit the current function's return: pop the shadow-stack frame, then `ret`. *)
let emit_ret cx (v : string) : unit =
  abi_pop_frame cx cx.frame;
  emit cx "  ret i64 %s" v

(* Read the current value of a rooted handle (post-safepoint) — [pv_get] on a bare handle operand. *)
let get_current cx (handle : string) : string = abi_get cx handle

(* Evaluate a list of atoms and return their **current** value operands, mutually protected against
   each other's safepoints. Collecting several results into registers and then calling — e.g.
   `CRecord`'s values, `CApp`'s `f`+args, `CCtor`'s fields — would let a later atom's GC stale an
   earlier value, so a value produced before a later safepoint is rooted and re-read after every
   evaluation is done (root-on-create). The rooting is *need-driven* (ADR-0072 §6), from the two
   static facts this list exposes:

   - which evaluations can reach a safepoint at all — a `String`/`Number` literal or a `foreign`
     closure allocates, and a *forced* variable may run a suspension (arbitrary guest code); an
     unforced variable read and an immediate literal cannot;
   - an `Int`/`Boolean` literal is an immediate operand — it is not a heap word, so it is never
     rooted (and [force_atom] already passes literals through unforced).

   So an atom is rooted+reloaded only when a *later* atom in the list can safepoint; the common
   all-vars/immediates list (a primop's or a direct call's operands) emits no `pv_root`/`pv_get`
   at all. The words handed back stay valid up to the consuming call — a runtime entry roots its
   own operands before it allocates (ADR-0071 §5), the same contract the previous
   always-root-then-reload form relied on. *)
let eval_atoms cx ?(force = false) (env : env) (atoms : A.atom list) : string list =
  let can_safepoint : A.atom -> bool = function
    | A.ALit (C.LInt _ | C.LBool _) -> false
    | A.ALit (C.LNumber _ | C.LString _) | A.AForeign _ -> true
    | A.AVar _ ->
      force (* forcing may run a suspension; a plain read cannot move the heap *)
  in
  let is_immediate : A.atom -> bool = function
    | A.ALit (C.LInt _ | C.LBool _) -> true
    | _ -> false
  in
  let one = if force then force_atom else atom in
  let rec go = function
    | [] -> []
    | a :: rest ->
      let later_safepoint = List.exists can_safepoint rest in
      let slot =
        if is_immediate a
        then `Raw (one cx env a)
        else (
          let v = one cx env a in
          if later_safepoint then `Rooted (root cx v) else `Raw v)
      in
      slot :: go rest
  in
  go atoms
  |> List.map (function
    | `Raw v -> v
    | `Rooted h -> get_current cx h)

(* Inline the trivially-transcribable scalar primops (ADR-0072 §7) instead of an extern
   `pv_prim_*` call per operation: exactly the ops whose runtime semantics (prim.rs) are a
   handful of instructions on immediate words — 32-bit wrapping arithmetic/bit-ops, the
   JS-masked shifts, and payload compares. `div`/`mod` (the Euclidean/zero-divisor rules,
   ADR-0041) and every ctx-taking op stay runtime helpers. The IR mirrors the runtime's
   reads *exactly*: the tagged encoding is `(payload << 1) | 1` (word.rs), an `Int` is read
   as the payload's low 32 bits (`ii` = `as_int`), a `Boolean` as `payload != 0`
   (`as_bool`) — so a hypothetical non-canonical word behaves identically on both paths.
   Returns [None] for every other op (the caller falls back to the call). *)
let inline_prim cx (op : C.primop) (ops : string list) : string option =
  let payload w =
    let p = fresh cx in
    emit cx "  %s = ashr i64 %s, 1" p w;
    p
  in
  let to_i32 w =
    let v = fresh cx in
    emit cx "  %s = trunc i64 %s to i32" v (payload w);
    v
  in
  let of_i32 v =
    let s = fresh cx in
    emit cx "  %s = sext i32 %s to i64" s v;
    let sh = fresh cx in
    emit cx "  %s = shl i64 %s, 1" sh s;
    let t = fresh cx in
    emit cx "  %s = or i64 %s, 1" t sh;
    t
  in
  let of_i1 b =
    let z = fresh cx in
    emit cx "  %s = zext i1 %s to i64" z b;
    let sh = fresh cx in
    emit cx "  %s = shl i64 %s, 1" sh z;
    let t = fresh cx in
    emit cx "  %s = or i64 %s, 1" t sh;
    t
  in
  let truthy w =
    let b = fresh cx in
    emit cx "  %s = icmp ne i64 %s, 0" b (payload w);
    b
  in
  let bin32 mnem a b =
    let va = to_i32 a in
    let vb = to_i32 b in
    let r = fresh cx in
    emit cx "  %s = %s i32 %s, %s" r mnem va vb;
    Some (of_i32 r)
  in
  (* the shift count is masked `& 31` (prim.rs `wrapping_shl`/`shr` on a masked count),
     which also keeps the LLVM shift amount in range (an unmasked >=32 would be poison) *)
  let shift32 mnem a b =
    let va = to_i32 a in
    let vb = to_i32 b in
    let m = fresh cx in
    emit cx "  %s = and i32 %s, 31" m vb;
    let r = fresh cx in
    emit cx "  %s = %s i32 %s, %s" r mnem va m;
    Some (of_i32 r)
  in
  let cmp32 cond a b =
    let va = to_i32 a in
    let vb = to_i32 b in
    let c = fresh cx in
    emit cx "  %s = icmp %s i32 %s, %s" c cond va vb;
    Some (of_i1 c)
  in
  match op, ops with
  | C.AddInt, [ a; b ] -> bin32 "add" a b
  | C.SubInt, [ a; b ] -> bin32 "sub" a b
  | C.MulInt, [ a; b ] -> bin32 "mul" a b
  | C.AndInt, [ a; b ] -> bin32 "and" a b
  | C.OrInt, [ a; b ] -> bin32 "or" a b
  | C.XorInt, [ a; b ] -> bin32 "xor" a b
  | C.ShlInt, [ a; b ] -> shift32 "shl" a b
  | C.ShrInt, [ a; b ] -> shift32 "ashr" a b
  | C.ZshrInt, [ a; b ] -> shift32 "lshr" a b
  | C.ComplementInt, [ a ] ->
    let va = to_i32 a in
    let r = fresh cx in
    emit cx "  %s = xor i32 %s, -1" r va;
    Some (of_i32 r)
  | C.EqInt, [ a; b ] -> cmp32 "eq" a b
  | C.LtInt, [ a; b ] -> cmp32 "slt" a b
  | C.EqBool, [ a; b ] ->
    let ba = truthy a in
    let bb = truthy b in
    let e = fresh cx in
    emit cx "  %s = icmp eq i1 %s, %s" e ba bb;
    Some (of_i1 e)
  | C.AndBool, [ a; b ] ->
    let ba = truthy a in
    let bb = truthy b in
    let r = fresh cx in
    emit cx "  %s = and i1 %s, %s" r ba bb;
    Some (of_i1 r)
  | C.OrBool, [ a; b ] ->
    let ba = truthy a in
    let bb = truthy b in
    let r = fresh cx in
    emit cx "  %s = or i1 %s, %s" r ba bb;
    Some (of_i1 r)
  | C.NotBool, [ a ] ->
    let ba = truthy a in
    let r = fresh cx in
    emit cx "  %s = xor i1 %s, true" r ba;
    Some (of_i1 r)
  | _ -> None

(* Emit a `br i1` guarded by [ok]: fall through to a fresh continuation block on true, jump to [fail] on
   false. Returns after emitting the continuation block's label (subsequent code runs on match). *)
let test_or_fail cx (ok : string) ~(fail : string) (tag : string) : unit =
  let cont = fresh_label cx tag in
  emit cx "  br i1 %s, label %%%s, label %%%s" ok cont fail;
  emit cx "%s:" cont

(* The boxed (`Number`/`String`) literals a binder compares against. Their allocation cannot happen
   *inside* the match (the no-allocation invariant below — a mid-match safepoint would invalidate the
   raw field/element operands nested binders hold), so the alt entry pre-allocates and roots one value
   per distinct literal and the matcher only *compares* against the rooted handle. Keyed by structural
   [compare] (not `=`), so a pathological NaN literal cannot fail its own lookup. *)
let rec binder_boxed_lits (b : C.binder) : C.lit list =
  match b with
  | C.BLit ((C.LNumber _ | C.LString _) as l) -> [ l ]
  | C.BLit _ | C.BNull | C.BVar _ -> []
  | C.BNamed (_, inner) -> binder_boxed_lits inner
  | C.BCtor (_, subs) | C.BArray subs -> List.concat_map binder_boxed_lits subs
  | C.BRecord fields -> List.concat_map (fun (_, s) -> binder_boxed_lits s) fields

(* Build a recursive group as **all-by-need `ByNeed` cells over one shared env** (ADR-0070 §4, mirroring
   the runtime `build_group`) and return [env] extended with each member bound to its (rooted) cell handle.
   Each member is a cell; its suspension is a nullary thunk over the shared env, whose body is the member's
   RHS with sibling references resolving to the sibling cells (a function member's cell forces to its
   closure on first use; `apply` auto-forces a by-need callee, ADR-0070 §3). Distinguishing function
   members as back-patched closures (ADR-0059 §1) is a deferred optimisation. Shared by the in-function
   [A.LetRec] and the top-level `Grec` init unit (ADR-0072 §3). *)
let build_grec
      ?(named = fun (_ : string) -> None)
      cx
      (env : env)
      (binds : (string * A.expr) list)
  : env
  =
  let members = List.map fst binds in
  let k = List.length members in
  let member_set = List.fold_left (fun s m -> SS.add m s) SS.empty members in
  (* Free vars of the group captured from *outside* it: members excluded, and so are
     top-level globals not shadowed by an enclosing local (ADR-0072 §6) — a global is read
     via its `@…$root` handle at use time, so copying it into the shared env only costs an
     env slot AND hides the binding from [direct_target] (an env-read callee has no
     [known_fn], so a saturated call to a known top-level function would fall back to the
     generic `pv_apply`). Init order is unchanged: the group's init runs after its spine
     dependencies', exactly when the capture would have been taken. *)
  let enclosing_locals = List.fold_left (fun s (x, _) -> SS.add x s) SS.empty env in
  let readable_globals = SS.diff cx.gkeys enclosing_locals in
  let outside =
    SS.elements
      (SS.diff
         (List.fold_left
            (fun acc (_, rhs) -> SS.union acc (fv_expr member_set rhs))
            SS.empty
            binds)
         readable_globals)
  in
  (* the shared env layout: the k member cells, then the outside captures *)
  let shared_layout = members @ outside in
  (* Pre-lift each **function** member under a stable name (ADR-0076 §2): its suspension then just
     builds that closure when forced ([LClosure]), so call sites — self, siblings, and (for a
     top-level group) other bindings via [cx.gfns] — can enter the member's direct entry. The
     capture set is computed exactly as [lift] would at the CLam site inside the suspension, whose
     scope is [shared_layout] (a shared name shadows an equally-named global, so it is captured). *)
  let shared_names = SS.of_list shared_layout in
  let member_fns =
    List.filter_map
      (fun (m, rhs) ->
         match rhs with
         | A.Ret (A.CLam (ps, b)) ->
           let name, top =
             match named m with
             | Some n -> n, true
             | None ->
               cx.fns <- cx.fns + 1;
               Printf.sprintf "recfn_%d" cx.fns, false
           in
           let bound = List.fold_left (fun s p -> SS.add p s) SS.empty ps in
           let globals_unshadowed = SS.diff cx.gkeys shared_names in
           let captures = SS.elements (SS.diff (fv_expr bound b) globals_unshadowed) in
           let info = { dsym = name ^ "$d"; arity = List.length ps; src = SForceCell } in
           Some
             ( m
             , { name
               ; params = ps
               ; captures
               ; body = LBody b
               ; self_name = Some m
               ; capture_fns =
                   []
                   (* an exported `Erecfn` member's `$d` is the cross-module force-cell
                    target (ADR-0077 §2/§3) *)
               ; exported = top && SM.mem m cx.xfns
               }
             , info )
         | _ -> None)
      binds
  in
  (* every member lambda may reach its siblings through its captures — give each the group's info *)
  let group_infos = List.map (fun (m, _, info) -> m, info) member_fns in
  let member_fns =
    List.map
      (fun (m, lm, info) -> m, { lm with capture_fns = group_infos }, info)
      member_fns
  in
  List.iter (fun (_, lm, _) -> cx.pending <- lm :: cx.pending) member_fns;
  (* one suspension per member over the shared env: a function member's suspension builds its
     pre-lifted closure; any other member's suspension evaluates its RHS as before *)
  let susp_names =
    List.map
      (fun (m, rhs) ->
         cx.fns <- cx.fns + 1;
         let name = Printf.sprintf "susp_%d" cx.fns in
         let body =
           match List.find_opt (fun (m', _, _) -> m' = m) member_fns with
           | Some (_, lm, _) -> LClosure lm
           | None -> LBody rhs
         in
         cx.pending
         <- { name
            ; params = [ "$u" ]
            ; captures = shared_layout
            ; body
            ; self_name = None
            ; capture_fns = []
            ; exported = false
            }
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
  (* 4. bind each member to its cell — function members carry their direct-call info. *)
  List.fold_left2
    (fun env m ch ->
       match List.find_opt (fun (m', _, _) -> m' = m) member_fns with
       | Some (_, _, info) -> bind_fn env m ch info
       | None -> bind env m ch)
    env
    members
    cell_hs

(* Resolve a saturated call's statically-known direct target (ADR-0076 §2, extended
   cross-module by ADR-0077 §2): a self-call, a let-bound lambda / recursive-group member, a
   same-module top-level function, or another module's *exported* function ([cx.xfns] — the
   dependency's `.pmi` export surface). [None] falls back to the generic `pv_apply` /
   trampoline path. Resolution mirrors [read_var]'s order — local scope first, then the
   enclosing self binding, then this module's own globals, then the export surface — so a
   local rebinding of a function's name is never mistaken for the function, and same-module
   facts win over the interface. A cross-module hit is recorded in [cx.xdecls] for its
   per-signature `declare tailcc` extern (ADR-0077 §3). *)
let direct_target cx (env : env) (f : A.atom) (nargs : int) : fn_info option =
  match f with
  | A.AVar x ->
    (match List.assoc_opt x env with
     | Some entry ->
       (match cx.self_ctx with
        | Some (nm, Some h0, _, info)
          when nm = x && entry.handle = h0 && info.arity = nargs -> Some info
        | _ ->
          (match entry.known_fn with
           | Some info when info.arity = nargs -> Some info
           | _ -> None))
     | None when SS.mem x cx.gkeys ->
       (match cx.self_ctx with
        | Some (nm, None, _, info) when nm = x && info.arity = nargs -> Some info
        | _ ->
          (match SM.find_opt x cx.gfns with
           | Some info when info.arity = nargs -> Some info
           | Some _ ->
             None (* own-module fact says unsaturated: never fall to the surface *)
           | None ->
             (match SM.find_opt x cx.xfns with
              | Some info when info.arity = nargs ->
                cx.xdecls <- SM.add info.dsym info.arity cx.xdecls;
                Some info
              | _ -> None)))
     | None -> None)
  | A.ALit _ | A.AForeign _ -> None

(* Compile [e]; [tail] = it is in the enclosing function's tail position. Returns [Some operand] when
   not tail (the produced value), [None] when tail (a `ret`/tail-call was emitted). *)
let rec expr cx (env : env) ~(tail : bool) (e : A.expr) : string option =
  match e with
  | A.Ret c -> cexpr cx env ~tail c
  | A.Let (x, A.CLam (ps, lbody), body) ->
    (* A let-bound lambda is a direct-call candidate (ADR-0076 §2): keep its lifted identity on
       the binding so saturated calls skip the generic dispatch. Non-recursive, so no self. *)
    let l = lift cx env ps lbody in
    let v = make_closure cx env l in
    let h = root cx v in
    let info =
      { dsym = l.name ^ "$d"
      ; arity = List.length ps
      ; src = (if l.captures = [] then SSentinel else SClosureEnv)
      }
    in
    expr cx (bind_fn env x h info) ~tail body
  | A.Let (x, c, body) ->
    let v = Option.get (cexpr cx env ~tail:false c) in
    let h = root cx v in
    expr cx (bind env x h) ~tail body
  | A.LetRec (binds, body) -> expr cx (build_grec cx env binds) ~tail body

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
    (* A primop consumes its operands' *values* (e.g. `RecordGet` on a by-need dict), so force them. *)
    let ops = eval_atoms ~force:true cx env args in
    (match inline_prim cx op ops with
     | Some t -> finish t
     | None ->
       let sym, needs_ctx = prim_sym op in
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
       finish t)
  | A.CLam (ps, body) -> finish (make_closure cx env (lift cx env ps body))
  | A.CApp (f, args) ->
    (match direct_target cx env f (List.length args) with
     | Some info ->
       (* Direct known-arity call (ADR-0076 §2/§3): skip the generic dispatch entirely. The env
          word is derived per the callee's shape; a cell force is a safepoint (the suspension may
          run guest code), so the argument values are re-read from their roots after it. *)
       let env_op, ops =
         match info.src with
         | SSelf ->
           let env_h =
             match cx.self_ctx with
             | Some (_, _, h, _) -> h
             | None -> failwith "codegen_llvm: self-call outside a self context"
           in
           let ops = eval_atoms cx env args in
           get_current cx env_h, ops
         | SSentinel -> imm_unit, eval_atoms cx env args
         | SClosureEnv ->
           let all = eval_atoms cx env (f :: args) in
           let e = fresh cx in
           emit
             cx
             "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %s, i64 2)"
             e
             (List.hd all);
           e, List.tl all
         | SForceCell ->
           let fh = root cx (atom cx env f) in
           let arg_hs = List.map (fun a -> root cx (atom cx env a)) args in
           let forced = fresh cx in
           emit
             cx
             "  %s = call i64 @pv_force_if_byneed(ptr %%ctx, i64 %s)"
             forced
             (get_current cx fh);
           let e = fresh cx in
           emit cx "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %s, i64 2)" e forced;
           e, List.map (get_current cx) arg_hs
       in
       let arglist = String.concat "" (List.map (fun o -> ", i64 " ^ o) ops) in
       if tail && cx.in_direct
       then (
         (* musttail (ADR-0076 §3): pop this frame first — the callee opens its own — with every
            operand (env word included) computed before the pop; no safepoint in between. *)
         abi_pop_frame cx cx.frame;
         let r = fresh cx in
         emit
           cx
           "  %s = musttail call tailcc i64 @%s(ptr %%ctx, i64 %s%s)"
           r
           info.dsym
           env_op
           arglist;
         emit cx "  ret i64 %s" r;
         None)
       else (
         let r = fresh cx in
         emit
           cx
           "  %s = call tailcc i64 @%s(ptr %%ctx, i64 %s%s)"
           r
           info.dsym
           env_op
           arglist;
         (* Settle (ADR-0076 §3): the callee's body may have stashed a generic tail bounce
            (`pv_tailcall`) that no enclosing `pv_apply` loop will take on this direct path —
            run it to a real value; a real value passes through. *)
         let r' = abi_settle cx r in
         if tail
         then (
           (* tail position inside a non-`tailcc` context (an init fn / entry stub): an ordinary
              call + return — correct, just without the guaranteed jump. *)
           emit_ret cx r';
           None)
         else Some r')
     | None ->
       (* `f` and the args are mutually protected: a `foreign` callee or a `String` arg may allocate. *)
       let all = eval_atoms cx env (f :: args) in
       let fv = List.hd all
       and ops = List.tl all in
       if tail
       then (
         (* Trampoline tail call (ADR-0071 §4): stash the pending tail, pop this frame, return
            (ignored). *)
         let p, n = arg_buffer cx ops in
         emit cx "  call void @pv_tailcall(ptr %%ctx, i64 %s, ptr %s, i64 %d)" fv p n;
         abi_pop_frame cx cx.frame;
         emit cx "  ret i64 %s" imm_unit;
         None)
       else (
         let p, n = arg_buffer cx ops in
         let t = fresh cx in
         emit cx "  %s = call i64 @pv_apply(ptr %%ctx, i64 %s, ptr %s, i64 %d)" t fv p n;
         Some t))
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
    if nargs > arity
    then
      (* A saturated constructor is a value; applying it further is a type error transl never emits. *)
      failwith
        (Printf.sprintf
           "codegen_llvm: over-applied constructor %s (%d/%d)"
           name
           nargs
           arity)
    else if nargs < arity
    then (
      (* An **unsaturated** constructor is a first-class function that accumulates the remaining fields,
         then builds the ADT (ADR-0072 §5). Synthesise a **builder closure** `\$c0 … $c{arity-1} ->
         Ctor(name, $c0, …)` (a saturated `CCtor` over fresh params, so it lowers to `pv_new_adt`) and
         apply the fields supplied so far: the runtime under-applies an arity-`arity` closure to `nargs`
         args to a PAP ([apply]), which saturates to the ADT once the rest arrive. `nargs = 0` (a bare
         constructor used as a function, e.g. `map Just`) is just the builder. *)
      let params = List.init arity (fun i -> Printf.sprintf "$ctorarg%d" i) in
      let body = A.Ret (A.CCtor (name, arity, List.map (fun p -> A.AVar p) params)) in
      let builder = make_closure cx env (lift cx env params body) in
      if nargs = 0
      then finish builder
      else (
        (* Root the builder across the args' allocation, then apply the supplied fields (raw — a
           construction site keeps a by-need field a cell, per the knot-tie). *)
        let bh = root cx builder in
        let ops = eval_atoms cx env args in
        let p, n = arg_buffer cx ops in
        let bv = get_current cx bh in
        let t = fresh cx in
        emit cx "  %s = call i64 @pv_apply(ptr %%ctx, i64 %s, ptr %s, i64 %d)" t bv p n;
        finish t))
    else if arity = 0
    then finish (imm (ctor_tag name)) (* nullary → an immediate tag (ADR-0064 §1) *)
    else (
      let idx = ctor_tag name in
      let ops = eval_atoms cx env args in
      let p, n = arg_buffer cx ops in
      let t = fresh cx in
      emit cx "  %s = call i64 @pv_new_adt(ptr %%ctx, i32 %d, ptr %s, i64 %d)" t idx p n;
      finish t)
  | A.CCase (scruts, alts) ->
    (* The shared Maranget decision tree ([Middle_end.Match_compile], ADR-0083), lowered to LLVM.
       Occurrences — scrutinees and every extracted sub-value — are **rooted** and re-read via
       [get_current]: unlike the old per-alt cascade (which re-derived sub-values per alt from the
       rooted scrutinees), the tree *shares* sub-occurrences across rows, and a guarded row's
       fall-through reuses them after its guard body may have safepointed — so each must survive a
       safepoint (root-reload, ADR-0064). Rooting is allocation-free, so it may sit between a raw
       read and its use. Boxed `Number`/`String` literals are pre-allocated and rooted once at case
       entry ([binder_boxed_lits]) so the tree walk itself never allocates. *)
    let scrut_binds, dt = MC.compile scruts alts in
    (* Root each scrutinee (forced — matching dereferences its structure). *)
    let occ_env =
      List.map (fun (occ, a) -> occ, root cx (force_atom cx env a)) scrut_binds
    in
    (* Hoist + root every boxed literal any arm compares against (NaN-safe key: [compare]). *)
    let lit_env =
      List.concat_map
        (fun (alt : A.alt) -> List.concat_map binder_boxed_lits alt.binders)
        alts
      |> List.sort_uniq compare
      |> List.map (fun l -> l, root cx (atom cx env (A.ALit l)))
    in
    let boxed_handle l = snd (List.find (fun (k, _) -> compare k l = 0) lit_env) in
    let cur occ_env occ = get_current cx (List.assoc occ occ_env) in
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
    (* Read a sub-value raw (allocation-free), then root it — extending [occ_env]. *)
    let extract occ_env parent (sub_occ, pr) =
      let raw = fresh cx in
      (match pr with
       | MC.Pfield j ->
         emit
           cx
           "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %s, i64 %d)"
           raw
           (cur occ_env parent)
           (1 + j)
       | MC.Pelem j ->
         emit
           cx
           "  %s = call i64 @pv_prim_index_array(ptr %%ctx, i64 %s, i64 %s)"
           raw
           (cur occ_env parent)
           (imm_int j)
       | MC.Precord l ->
         emit
           cx
           "  %s = call i64 @pv_record_get(ptr %%ctx, i64 %s, i64 %s)"
           raw
           (cur occ_env parent)
           (label_id l));
      (sub_occ, root cx raw) :: occ_env
    in
    let bind_leaf occ_env binds =
      List.fold_left (fun env (v, occ) -> bind env v (List.assoc occ occ_env)) env binds
    in
    let switch_arms arms case_const =
      let default_lbl = fresh_label cx "swdef" in
      let arm_lbls = List.map (fun _ -> fresh_label cx "swarm") arms in
      let cases =
        List.map2
          (fun a l -> Printf.sprintf "%s, label %%%s" (case_const a) l)
          arms
          arm_lbls
      in
      default_lbl, arm_lbls, cases
    in
    let rec lower occ_env (dt : MC.dtree) : unit =
      match dt with
      | MC.Dfail _ -> emit cx "  br label %%%s" fail_label
      | MC.Dleaf (binds, e) -> run_body (bind_leaf occ_env binds) e
      | MC.Dguard (binds, clauses, ft) ->
        let env' = bind_leaf occ_env binds in
        let rec guards = function
          | [] -> lower occ_env ft
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
        guards clauses
      | MC.Dswitch_ctor (occ, arms, default) ->
        (* Dispatch by *representation first*, then by tag within each — mirroring the old
           per-binder cascade's kind guard (ADR-0064 §1): a nullary ctor is an immediate carrying
           its tag in the payload, a field-carrying ctor a pointer with its tag at raw word 0.
           Keeping the two apart means a field arm never reads a field out of an immediate, and a
           `ctor_tag` hash collision between a nullary and a field ctor stays disambiguated by kind
           — parity with `codegen_ml` (which matches the real string tag + arity). *)
        let scrut = cur occ_env occ in
        let low = fresh cx in
        emit cx "  %s = and i64 %s, 1" low scrut;
        let is_imm = fresh cx in
        emit cx "  %s = icmp eq i64 %s, 1" is_imm low;
        let imm_blk = fresh_label cx "ctimm"
        and ptr_blk = fresh_label cx "ctptr" in
        emit cx "  br i1 %s, label %%%s, label %%%s" is_imm imm_blk ptr_blk;
        let default_lbl = fresh_label cx "ctdef" in
        let arm_lbls =
          List.map (fun (tag, arm) -> tag, arm, fresh_label cx "ctarm") arms
        in
        let cases_for keep =
          arm_lbls
          |> List.filter (fun (_, (arm : MC.arm), _) -> keep arm.MC.extracts)
          |> List.map (fun (tag, _, l) ->
            Printf.sprintf "i64 %d, label %%%s" (ctor_tag tag) l)
          |> String.concat " "
        in
        (* immediates → the nullary (no-extract) arms, keyed by payload tag *)
        emit cx "%s:" imm_blk;
        let itag = fresh cx in
        emit cx "  %s = ashr i64 %s, 1" itag scrut;
        emit
          cx
          "  switch i64 %s, label %%%s [ %s ]"
          itag
          default_lbl
          (cases_for (fun e -> e = []));
        (* pointers → the field-carrying arms, keyed by the tag at raw word 0 *)
        emit cx "%s:" ptr_blk;
        let ptag = fresh cx in
        emit cx "  %s = call i64 @pv_read_raw(ptr %%ctx, i64 %s, i64 0)" ptag scrut;
        emit
          cx
          "  switch i64 %s, label %%%s [ %s ]"
          ptag
          default_lbl
          (cases_for (fun e -> e <> []));
        List.iter
          (fun (_, (arm : MC.arm), l) ->
             emit cx "%s:" l;
             let occ_env' =
               List.fold_left (fun oe ex -> extract oe occ ex) occ_env arm.MC.extracts
             in
             lower occ_env' arm.MC.sub)
          arm_lbls;
        emit cx "%s:" default_lbl;
        lower occ_env default
      | MC.Dswitch_lit (occ, arms, default) ->
        (match arms with
         | [] -> lower occ_env default
         | ((C.LInt _ | C.LBool _), _) :: _ ->
           (* Immediate literals → a direct LLVM switch on the tagged word. *)
           let scrut = cur occ_env occ in
           let imm_of = function
             | C.LInt n -> imm_int n
             | C.LBool b -> imm_bool b
             | _ -> assert false
           in
           let default_lbl, arm_lbls, cases =
             switch_arms arms (fun (l, _) -> Printf.sprintf "i64 %s" (imm_of l))
           in
           emit
             cx
             "  switch i64 %s, label %%%s [ %s ]"
             scrut
             default_lbl
             (String.concat " " cases);
           List.iter2
             (fun (_, sub) l ->
                emit cx "%s:" l;
                lower occ_env sub)
             arms
             arm_lbls;
           emit cx "%s:" default_lbl;
           lower occ_env default
         | ((C.LNumber _ | C.LString _), _) :: _ ->
           (* Boxed literals cannot be `switch`ed — an equality chain against the hoisted rooted
              handles, via the runtime `Eq` primop (the tested source of truth for IEEE / bytes). *)
           let rec chain = function
             | [] -> lower occ_env default
             | (l, sub) :: rest ->
               let prim =
                 match l with
                 | C.LNumber _ -> "pv_prim_eq_number"
                 | _ -> "pv_prim_eq_string"
               in
               let eq = fresh cx in
               emit
                 cx
                 "  %s = call i64 @%s(ptr %%ctx, i64 %s, i64 %s)"
                 eq
                 prim
                 (cur occ_env occ)
                 (get_current cx (boxed_handle l));
               let pay = fresh cx in
               emit cx "  %s = ashr i64 %s, 1" pay eq;
               let ok = fresh cx in
               emit cx "  %s = icmp ne i64 %s, 0" ok pay;
               let arm_lbl = fresh_label cx "ltarm"
               and next = fresh_label cx "ltnext" in
               emit cx "  br i1 %s, label %%%s, label %%%s" ok arm_lbl next;
               emit cx "%s:" arm_lbl;
               lower occ_env sub;
               emit cx "%s:" next;
               chain rest
           in
           chain arms)
      | MC.Dswitch_len (occ, arms, default) ->
        let len = fresh cx in
        emit
          cx
          "  %s = call i64 @pv_prim_length_array(ptr %%ctx, i64 %s)"
          len
          (cur occ_env occ);
        let default_lbl, arm_lbls, cases =
          switch_arms arms (fun (n, _) -> Printf.sprintf "i64 %s" (imm_int n))
        in
        emit
          cx
          "  switch i64 %s, label %%%s [ %s ]"
          len
          default_lbl
          (String.concat " " cases);
        List.iter2
          (fun (_, (arm : MC.arm)) l ->
             emit cx "%s:" l;
             let occ_env' =
               List.fold_left (fun oe ex -> extract oe occ ex) occ_env arm.MC.extracts
             in
             lower occ_env' arm.MC.sub)
          arms
          arm_lbls;
        emit cx "%s:" default_lbl;
        lower occ_env default
      | MC.Dexpand_record (occ, extracts, sub) ->
        let occ_env' = List.fold_left (fun oe ex -> extract oe occ ex) occ_env extracts in
        lower occ_env' sub
    in
    lower occ_env dt;
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
  let arity = List.length l.params in
  (* --- the direct entry `@<name>$d` (ADR-0076 §1): body under `tailcc`, args as parameters ------- *)
  cx.fn <- Buffer.create 256;
  cx.ssa <- 0;
  let frame = abi_frame_open cx in
  cx.frame <- frame;
  (* params: parameter i ↦ a rooted slot *)
  let env =
    List.mapi (fun i p -> p, i) l.params
    |> List.fold_left
         (fun env (p, i) -> bind env p (root cx (Printf.sprintf "%%p%d" i)))
         []
  in
  (* captures: positional reads from the env word `%env` (the shared/captured array) *)
  let env, self_handle =
    if l.captures = []
    then env, None
    else (
      let env, sh =
        List.mapi (fun i c -> c, i) l.captures
        |> List.fold_left
             (fun (env, sh) (c, i) ->
                let v = fresh cx in
                emit cx "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %%env, i64 %d)" v i;
                let h = root cx v in
                let sh = if Some c = l.self_name then Some h else sh in
                let env =
                  match List.assoc_opt c l.capture_fns with
                  | Some info -> bind_fn env c h info
                  | None -> bind env c h
                in
                env, sh)
             (env, None)
      in
      env, sh)
  in
  (* the self-call shortcut (ADR-0076 §2): while this body runs, a saturated call to [self_name]
     re-enters this very function with this very `%env` — record the entry-time handle so a local
     rebinding of the name is not mistaken for self. *)
  let saved_self = cx.self_ctx
  and saved_direct = cx.in_direct in
  (match l.self_name with
   | Some nm ->
     (* Root the env word itself: a self-call re-supplies it, and the raw `%env` SSA value is
        stale after the first safepoint in the body (the env array moves with the heap). *)
     let env_h = root cx "%env" in
     cx.self_ctx
     <- Some (nm, self_handle, env_h, { dsym = l.name ^ "$d"; arity; src = SSelf })
   | None -> cx.self_ctx <- None);
  cx.in_direct <- true;
  (match l.body with
   | LBody e -> ignore (expr cx env ~tail:true e)
   | LClosure lm ->
     (* a `Grec` function member's suspension: forcing builds the pre-lifted member closure *)
     let clo = make_closure cx env lm in
     emit_ret cx clo);
  cx.self_ctx <- saved_self;
  cx.in_direct <- saved_direct;
  (* Linkage: a lifted internal (`fn_*`/`susp_*`/local `recfn_*`) is referenced only by address
     within its own module, so `internal` keeps it from clashing with an equally-named lifted
     function in another per-module `.o` (ADR-0072 §1). An **exported top-level** binding's `$d`
     is external (ADR-0077 §3): it is the cross-module direct-call target, and the injective
     mangle already guarantees cross-`.o` uniqueness. `tailcc` guarantees the `musttail` TCO on
     direct→direct tail edges (ADR-0076 §3; prototype matching is not required for `tailcc`). *)
  let dparams =
    String.concat "" (List.init arity (fun i -> Printf.sprintf ", i64 %%p%d" i))
  in
  Buffer.add_string
    cx.md
    (Printf.sprintf
       "define %stailcc i64 @%s$d(ptr %%ctx, i64 %%env%s) {\nentry:\n%s}\n\n"
       (if l.exported then "" else "internal ")
       l.name
       dparams
       (Buffer.contents cx.fn));
  (* --- the generic wrapper `@<name>` (the `AbiCodeFn` the closure's code word points at) ---------- *)
  cx.fn <- Buffer.create 256;
  cx.ssa <- 0;
  let envw =
    if l.captures = []
    then imm_unit
    else (
      let e = fresh cx in
      emit cx "  %s = call i64 @pv_read_field(ptr %%ctx, i64 %%clo, i64 2)" e;
      e)
  in
  let args =
    List.init arity (fun i ->
      let ptr = fresh cx in
      emit cx "  %s = getelementptr i64, ptr %%args, i64 %d" ptr i;
      let v = fresh cx in
      emit cx "  %s = load i64, ptr %s" v ptr;
      v)
  in
  let r = fresh cx in
  emit
    cx
    "  %s = call tailcc i64 @%s$d(ptr %%ctx, i64 %s%s)"
    r
    l.name
    envw
    (String.concat "" (List.map (fun a -> ", i64 " ^ a) args));
  emit cx "  ret i64 %s" r;
  Buffer.add_string
    cx.md
    (Printf.sprintf
       "define internal i64 @%s(ptr %%ctx, i64 %%clo, ptr %%args, i64 %%nargs) {\n\
        entry:\n\
        %s}\n\n"
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
    ; "declare void @pv_abi_check(i32)"
    ; "declare void @pv_runtime_free(ptr)"
    ; "declare i64 @pv_apply(ptr, i64, ptr, i64)"
    ; "declare void @pv_tailcall(ptr, i64, ptr, i64)"
    ; "declare i64 @pv_settle(ptr, i64)"
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
    ; "declare i64 @pv_prim_record_union(ptr, i64, i64)"
    ; "declare i64 @pv_empty_array()"
    ; "declare i64 @pv_new_byneed_placeholder(ptr)"
    ; "declare void @pv_byneed_set_suspension(ptr, i64, i64)"
    ; "declare i64 @pv_force_if_byneed(ptr, i64)"
    ]

(* Default heap size in words per semi-space for the entry runtime — a generous fixed size for v1
   (ADR-0066 §4). Overridable per program (a small heap forces GC, exercising the emitted rooting). *)
let default_heap_words = 1 lsl 20

(* A fresh codegen context. [gkeys] is the set of top-level qualified keys that resolve to root-handle
   globals (empty on the whole-program path, which nests every binding as an in-`main` SSA value). *)

(** Emit a whole ANF program as a self-contained LLVM IR module string. A **pure** entry (`is_effect =
    false`) computes its `Int` value and prints it (`pv_print_int`, matching the oracle's `to_string`); an
    **`Effect`** entry runs `pv_run_effect` then drains the output sink (ADR-0072 §8). The entry body is
    compiled in non-tail (value) position, then the stub prints/runs, frees the runtime, and returns. *)
let make_cx ?(gkeys = SS.empty) ?(xfns = SM.empty) ?(inline_abi = true) () : ctx =
  { md = Buffer.create 4096
  ; globals = Buffer.create 256
  ; fn = Buffer.create 256
  ; ssa = 0
  ; lbl = 0
  ; fns = 0
  ; strs = 0
  ; pending = []
  ; frame = "%frame"
  ; gkeys
  ; externs = SS.empty
  ; foreigns = SS.empty
  ; gfns = SM.empty
  ; xfns
  ; xdecls = SM.empty
  ; self_ctx = None
  ; in_direct = false
  ; inline_abi
  }

(* The per-object link-time ABI stamp (ADR-0079 §1): an inline-emitting object carries a kept-alive
   reference to `pv_ctx_abi_v<N>`, which only the matching-version RELEASE runtime defines — so a
   stale object, or an inline object paired with the debug staticlib, fails at link with an
   undefined versioned symbol. `llvm.used` keeps the reference through dead-strip. Entry-call
   (`--debug`) objects carry nothing and link against either profile. *)
let abi_stamp cx : string =
  if not cx.inline_abi
  then ""
  else
    Printf.sprintf
      "@pv_ctx_abi_v%d = external global i8\n\
       @pv_abi_stamp = internal constant ptr @pv_ctx_abi_v%d\n\
       @llvm.used = appending global [1 x ptr] [ptr @pv_abi_stamp], section \
       \"llvm.metadata\"\n"
      ctx_header_version
      ctx_header_version

(* `declare` lines for the native foreign `AbiCodeFn` symbols this object references (ADR-0073 §3). Each has
   the leaf ABI `i64 (ptr ctx, i64 clo, ptr args, i64 nargs)`; the symbol is resolved at link from the
   runtime (or, later, a ulib's compiled `.c`). A `declare` of a symbol referenced nowhere in the object is
   inert, so emitting these per-object is safe. *)
let foreign_decls cx : string =
  SS.elements cx.foreigns
  |> List.map (fun k ->
    Printf.sprintf "declare i64 @%s(ptr, i64, ptr, i64)" (mangle_foreign k))
  |> String.concat "\n"

(* `declare tailcc` lines for the cross-module direct entries this object calls (ADR-0077 §3):
   one per referenced `$d` symbol, with the interface-exact signature (the callee's arity). A
   stale declare is an ABI break — guarding it is the `.pmi` hash cascade's job (ADR-0077 §4). *)
let xfn_decls cx : string =
  SM.bindings cx.xdecls
  |> List.map (fun (dsym, arity) ->
    Printf.sprintf
      "declare tailcc i64 @%s(ptr, i64%s)"
      dsym
      (String.concat "" (List.init arity (fun _ -> ", i64"))))
  |> String.concat "\n"

let program
      ?(is_effect = false)
      ?(heap_words = default_heap_words)
      ?(debug = false)
      (e : A.expr)
  : string
  =
  let cx = make_cx ~inline_abi:(not debug) () in
  (* the entry stub's own body buffer *)
  emit cx "  %%ctx = call ptr @pv_runtime_new(i64 %d)" heap_words;
  (* the run-time half of the ADR-0079 §1 version net (the link-time half is the per-object
     `pv_ctx_abi_v<N>` stamp) *)
  emit cx "  call void @pv_abi_check(i32 %d)" ctx_header_version;
  let frame0 = abi_frame_open cx in
  cx.frame <- frame0;
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
  abi_pop_frame cx frame0;
  emit cx "  call void @pv_runtime_free(ptr %%ctx)";
  emit cx "  ret i32 0";
  let entry_body = Buffer.contents cx.fn in
  (* emit the lifted functions the entry (and transitively) queued *)
  emit_pending cx;
  Printf.sprintf
    "; ModuleID = 'purvasm'\n\n%s\n%s\n%s\n\n%s\n%s\ndefine i32 @main() {\nentry:\n%s}\n"
    declarations
    (abi_stamp cx)
    (foreign_decls cx)
    (Buffer.contents cx.globals)
    (Buffer.contents cx.md)
    entry_body

(* ==================================================================================================== *)
(* Per-module lowering (ADR-0072 §2/§3): top-level bindings as **root-handle globals**, each with an init
   function, so modules compile to independent `.o` files linked by symbol. B1 emits one module's `.ll`
   (its globals + init functions + code); the whole-program `Grec`/by-need machinery above is reused. *)
(* ==================================================================================================== *)

(** A dependency export's call-relevant fact as its `.pmi` publishes it (ADR-0077 §2): a
    non-recursive function of an arity ([Cfn] = `Efn`, the sentinel-env direct entry) or a
    recursive-group function member of an arity ([Crecfn] = `Erecfn`, the force-cell direct
    entry). Value exports (`Ecaf`/`Erec`) carry no call fact. Mirrors
    [Pvm.Artifact.export_kind] without depending on it — the driver translates. *)
type call_fact =
  | Cfn of int
  | Crecfn of int

(** A classified top-level binding (ADR-0072 §3), the `gdef` rule at the ANF level: a syntactic lambda is a
    [Gfun] (a closed global closure); any other non-recursive value is a strict [Gcaf]; a recursive group is
    a [Grec] built by-need (ADR-0070 §4). *)
type gdef =
  | Gfun of string * string list * A.expr (* key, params, body *)
  | Gcaf of string * A.expr (* key, strict value *)
  | Grec of (string * A.expr) list (* recursive-group members: keys + bodies *)

(* Classify a non-recursive binding: a `Ret (CLam …)` is a function, anything else a strict CAF. *)
let classify_nonrec (key : string) (e : A.expr) : gdef =
  match e with
  | A.Ret (A.CLam (ps, b)) -> Gfun (key, ps, b)
  | _ -> Gcaf (key, e)

(* The root-handle-global keys a gdef defines. *)
let gdef_keys : gdef -> string list = function
  | Gfun (k, _, _) | Gcaf (k, _) -> [ k ]
  | Grec ms -> List.map fst ms

(* The init symbol a gdef exposes for `pv_init_all` (a group is named after its first member). *)
let gdef_init_key : gdef -> string = function
  | Gfun (k, _, _) | Gcaf (k, _) -> k
  | Grec ms -> fst (List.hd ms)

(* --- top-level key uniquification (ADR-0072 §2) ---------------------------------------------------- *)

(* Capture-avoiding rename of *free* variable occurrences per [ren] (a name → replacement map). A local
   binder shadowing a renamed name removes it from [ren] within that scope, so only the intended free
   references are rewritten. Used to disambiguate two top-level bindings that share a bare key. *)
let ren_atom (ren : string SM.t) : A.atom -> A.atom = function
  | A.AVar x as a ->
    (match SM.find_opt x ren with
     | Some x' -> A.AVar x'
     | None -> a)
  | (A.ALit _ | A.AForeign _) as a -> a

let rec ren_expr (ren : string SM.t) (e : A.expr) : A.expr =
  match e with
  | A.Ret c -> A.Ret (ren_cexpr ren c)
  | A.Let (x, c, body) -> A.Let (x, ren_cexpr ren c, ren_expr (SM.remove x ren) body)
  | A.LetRec (binds, body) ->
    let ren' = List.fold_left (fun r (x, _) -> SM.remove x r) ren binds in
    A.LetRec (List.map (fun (x, rhs) -> x, ren_expr ren' rhs) binds, ren_expr ren' body)

and ren_cexpr (ren : string SM.t) (c : A.cexpr) : A.cexpr =
  match c with
  | A.CAtom a -> A.CAtom (ren_atom ren a)
  | A.CLam (ps, b) ->
    A.CLam (ps, ren_expr (List.fold_left (fun r p -> SM.remove p r) ren ps) b)
  | A.CApp (f, args) -> A.CApp (ren_atom ren f, List.map (ren_atom ren) args)
  | A.CPrim (op, args) -> A.CPrim (op, List.map (ren_atom ren) args)
  | A.CArray args -> A.CArray (List.map (ren_atom ren) args)
  | A.CCtor (n, ar, args) -> A.CCtor (n, ar, List.map (ren_atom ren) args)
  | A.CRecord fs -> A.CRecord (List.map (fun (l, a) -> l, ren_atom ren a) fs)
  | A.CAccessor (a, l) -> A.CAccessor (ren_atom ren a, l)
  | A.CUpdate (a, fs) ->
    A.CUpdate (ren_atom ren a, List.map (fun (l, x) -> l, ren_atom ren x) fs)
  | A.CIf (a, t, e) -> A.CIf (ren_atom ren a, ren_expr ren t, ren_expr ren e)
  | A.CCase (scruts, alts) ->
    A.CCase (List.map (ren_atom ren) scruts, List.map (ren_alt ren) alts)

and ren_alt (ren : string SM.t) (alt : A.alt) : A.alt =
  let bvs = List.fold_left (fun a b -> SS.union a (binder_vars b)) SS.empty alt.binders in
  let ren' = SS.fold SM.remove bvs ren in
  let result =
    match alt.result with
    | A.Uncond e -> A.Uncond (ren_expr ren' e)
    | A.Guarded gs ->
      A.Guarded (List.map (fun (g, e) -> ren_expr ren' g, ren_expr ren' e) gs)
  in
  { alt with result }

(** Make every top-level binding key **unique** (ADR-0072 §2). The flat root-handle-global scheme cannot
    represent two top-level bindings that share a bare key — e.g. two `where`-hoisted helpers both named
    `alg` lexically shadow in the linked spine but would double-define `@<mangle>$root`. The whole-program
    path handles this via SSA shadowing; the per-module path needs distinct keys. So walk the spine and, on
    a re-bound key, rename the *shadowing* (later) binding and capture-avoidingly rewrite its references in
    scope (the rest of the spine, and its own RHS for a recursive group). References before the shadow keep
    the earlier binding. A no-op when no key repeats. *)
let uniquify_toplevel (e : A.expr) : A.expr =
  let counter = ref 0 in
  let fresh base =
    incr counter;
    Printf.sprintf "%s$sh%d" base !counter
  in
  let rec walk (ren : string SM.t) (seen : SS.t) (e : A.expr) : A.expr =
    match e with
    | A.Let (k, c, rest) ->
      (* the RHS is in the enclosing scope (before this binding shadows), so it uses the current [ren] *)
      let c = ren_cexpr ren c in
      if SS.mem k seen
      then (
        let k' = fresh k in
        A.Let (k', c, walk (SM.add k k' ren) seen rest))
      else A.Let (k, c, walk ren (SS.add k seen) rest)
    | A.LetRec (binds, rest) ->
      (* a recursive group: rename any member key already seen, then rewrite the members' RHS *and* the
         rest with the group's renames (members reference each other by key) *)
      let seen', ren', renamed =
        List.fold_left
          (fun (seen, ren, acc) (k, rhs) ->
             if SS.mem k seen
             then (
               let k' = fresh k in
               seen, SM.add k k' ren, (k', rhs) :: acc)
             else SS.add k seen, ren, (k, rhs) :: acc)
          (seen, ren, [])
          binds
      in
      let binds' = List.rev_map (fun (k, rhs) -> k, ren_expr ren' rhs) renamed in
      A.LetRec (binds', walk ren' seen' rest)
    | main -> ren_expr ren main
  in
  walk SM.empty SS.empty e

(** Split a linked top-level spine into its global definitions (in spine = dependency order) and the entry
    expression (the first non-binding node = `main`). Mirrors `Vm.Codegen.program`; B1's single-module
    stand-in until B2 drives real per-module ANF. *)
let split_spine (e : A.expr) : gdef list * A.expr =
  let rec walk acc = function
    | A.Let (k, c, rest) -> walk (classify_nonrec k (A.Ret c) :: acc) rest
    | A.LetRec (binds, rest) -> walk (Grec binds :: acc) rest
    | main -> List.rev acc, main
  in
  walk [] e

(* The top-level keys a gdef references (in its bodies), for reachability. `∩ gkeys` keeps only global
   references (params/locals are bare/fresh, never in [gkeys]); its own keys are excluded. *)
let gdef_refs ~(gkeys : SS.t) (g : gdef) : SS.t =
  let bodies =
    match g with
    | Gfun (_, _, b) | Gcaf (_, b) -> [ b ]
    | Grec ms -> List.map snd ms
  in
  let fvs = List.fold_left (fun a b -> SS.union a (fv_expr SS.empty b)) SS.empty bodies in
  SS.diff (SS.inter fvs gkeys) (SS.of_list (gdef_keys g))

(** The gdefs **reachable** from the entry (ADR-0072 §3), in the original spine order — the transitive
    closure of the entry's global references over each binding's [gdef_refs]. `pv_init_all` initialises
    only these (no dead init); the system linker's dead-strip removes the unreferenced rest. A `Grec` group
    is reached as a unit (any member pulls the whole atomic init). Spine order restricted to the reachable
    set is still a valid topological order ([Link] spine-orders the dependency DAG), so init ordering stays
    correct without rebuilding the edge graph — the explicit edge-topo is only needed by the future
    artifact-input driver (§3 pin 3). *)
let reachable_gdefs ~(gkeys : SS.t) (entry : A.expr) (gdefs : gdef list) : gdef list =
  let by_key : (string, gdef) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun g -> List.iter (fun k -> Hashtbl.replace by_key k g) (gdef_keys g)) gdefs;
  let reached : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec visit k =
    match Hashtbl.find_opt by_key k with
    | None -> () (* not a top-level binding here (a foreign leaf, or an entry-only key) *)
    | Some g ->
      let ik = gdef_init_key g in
      if not (Hashtbl.mem reached ik)
      then (
        Hashtbl.add reached ik ();
        SS.iter visit (gdef_refs ~gkeys g))
  in
  SS.iter visit (SS.inter (fv_expr SS.empty entry) gkeys);
  List.filter (fun g -> Hashtbl.mem reached (gdef_init_key g)) gdefs

(* Root value word [v] into the top-level global `@<mangle key>$root` and store the handle. The caller must
   already have popped any transient frame, so the root lands in the persistent init region of the shadow
   stack (never popped) and the global's handle stays relocation-valid for the program's lifetime. *)
let store_root_global cx (key : string) (v : string) : unit =
  let h = abi_root cx v in
  emit cx "  store i64 %s, ptr @%s$root" h (mangle key)

(* Wrap an init-body emitter in `define void @<mangle name>$init(ptr %ctx)`, using a fresh function buffer
   (like [emit_function]) and flushing to the module buffer. *)
let emit_init_fn cx ~(name : string) (emit_body : unit -> unit) : unit =
  cx.fn <- Buffer.create 256;
  cx.ssa <- 0;
  emit_body ();
  emit cx "  ret void";
  Buffer.add_string
    cx.md
    (Printf.sprintf
       "define void @%s$init(ptr %%ctx) {\nentry:\n%s}\n\n"
       (mangle name)
       (Buffer.contents cx.fn))

(* Emit one gdef's root-handle global(s) init function. Each opens a **transient** frame for its
   allocating computation, pops it, then roots the final value(s) permanently into their `@…$root`
   globals (so intermediates are not leaked; only the CAF value stays rooted — ADR-0072 §3). *)
let emit_gdef cx : gdef -> unit = function
  | Gfun (key, ps, body) ->
    (* the code symbol (a closed top-level function, no captures) is hoisted like any lambda;
       an exported one's `$d` is the cross-module sentinel-env direct target (ADR-0077 §2/§3) *)
    cx.pending
    <- { name = mangle key
       ; params = ps
       ; captures = []
       ; body = LBody body
       ; self_name = None
       ; capture_fns = []
       ; exported = SM.mem key cx.xfns
       }
       :: cx.pending;
    emit_init_fn cx ~name:key (fun () ->
      let addr = fresh cx in
      emit cx "  %s = ptrtoint ptr @%s to i64" addr (mangle key);
      let clo = fresh cx in
      emit
        cx
        "  %s = call i64 @pv_make_closure(ptr %%ctx, i64 %s, i32 %d, i64 %s)"
        clo
        addr
        (List.length ps)
        imm_unit;
      store_root_global cx key clo)
  | Gcaf (key, e) ->
    emit_init_fn cx ~name:key (fun () ->
      let frame = abi_frame_open cx in
      cx.frame <- frame;
      let v = Option.get (expr cx [] ~tail:false e) in
      abi_pop_frame cx frame;
      store_root_global cx key v)
  | Grec binds ->
    emit_init_fn
      cx
      ~name:(fst (List.hd binds))
      (fun () ->
         let frame = abi_frame_open cx in
         cx.frame <- frame;
         (* stable member code symbols, so [cx.gfns]'s pre-registered `$d` names line up *)
         let env' = build_grec ~named:(fun m -> Some (mangle m)) cx [] binds in
         (* read each member's current cell value *before* popping the transient roots *)
         let vals = List.map (fun (m, _) -> m, read_var cx env' m) binds in
         abi_pop_frame cx frame;
         List.iter (fun (m, v) -> store_root_global cx m v) vals)

(* Emit each gdef's root-handle global definition(s) (handle 0 = an invalid sentinel; init overwrites
   before any read), then its init function. Writes to [cx.md]; queues code functions into [cx.pending]. *)

(** Emit a program as **per-module-style** LLVM IR (ADR-0072 §2/§3): each top-level binding becomes a
    root-handle global `@<mangle key>$root` + an `@<mangle key>$init` function; a `pv_init_all` runs the inits in dependency
    (spine) order into a never-popped shadow-stack region; the entry stub calls it, then evaluates the
    entry (reading globals via `pv_get`). B1 emits a single self-contained module (all globals defined
    locally); B2 splits this across `.o` files with a link-synthesised `pv_init_all`. *)
let emit_gdefs cx (gdefs : gdef list) : unit =
  (* Register this compilation unit's OWN function bindings for direct calls (ADR-0076 §2) —
     never a neighbour's: each per-module [cx] sees only its own gdefs here, and these
     same-module facts win before the export surface. What crosses the module boundary is
     only [cx.xfns] — the dependency's `.pmi` call facts (ADR-0077 §2, superseding ADR-0072
     §2's no-metadata pin). The names are deterministic (`mangle key`), matching
     [emit_gdef]/[build_grec ~named]'s emissions. *)
  List.iter
    (fun g ->
       match g with
       | Gfun (k, ps, _) ->
         cx.gfns
         <- SM.add
              k
              { dsym = mangle k ^ "$d"; arity = List.length ps; src = SSentinel }
              cx.gfns
       | Grec ms ->
         List.iter
           (fun (m, rhs) ->
              match rhs with
              | A.Ret (A.CLam (ps, _)) ->
                cx.gfns
                <- SM.add
                     m
                     { dsym = mangle m ^ "$d"; arity = List.length ps; src = SForceCell }
                     cx.gfns
              | _ -> ())
           ms
       | Gcaf _ -> ())
    gdefs;
  List.iter
    (fun g ->
       List.iter
         (fun k ->
            Buffer.add_string
              cx.md
              (Printf.sprintf "@%s$root = global i64 0\n" (mangle k)))
         (gdef_keys g))
    gdefs;
  if gdefs <> [] then Buffer.add_char cx.md '\n';
  List.iter (emit_gdef cx) gdefs

(* Emit `pv_init_all`: call each given binding's init in [gdefs] order (the dependency / spine order).
   Callers pass the **reachable** subset ([reachable_gdefs]), so this emits calls only for what is
   initialised. The inits push their permanent roots onto the shadow stack; nothing pops them, so the
   globals stay live and relocation-valid for the program's lifetime (ADR-0072 §3). *)
let emit_init_all cx (gdefs : gdef list) : unit =
  let calls =
    List.map
      (fun g ->
         Printf.sprintf "  call void @%s$init(ptr %%ctx)" (mangle (gdef_init_key g)))
      gdefs
  in
  Buffer.add_string
    cx.md
    (Printf.sprintf
       "define void @pv_init_all(ptr %%ctx) {\nentry:\n%s\n  ret void\n}\n\n"
       (String.concat "\n" calls))

(* Emit the `@main` entry stub into [cx.fn] — `pv_runtime_new` → `pv_init_all` → evaluate the entry (in
   value position, reading globals via `pv_get`) → print/run → free — and return its body text. *)
let emit_entry_stub cx ~(is_effect : bool) ~(heap_words : int) (entry : A.expr) : string =
  cx.fn <- Buffer.create 256;
  cx.ssa <- 0;
  emit cx "  %%ctx = call ptr @pv_runtime_new(i64 %d)" heap_words;
  (* the run-time half of the ADR-0079 §1 version net, checked before any init runs *)
  emit cx "  call void @pv_abi_check(i32 %d)" ctx_header_version;
  emit cx "  call void @pv_init_all(ptr %%ctx)";
  let frame0 = abi_frame_open cx in
  cx.frame <- frame0;
  let v = Option.get (expr cx [] ~tail:false entry) in
  if is_effect
  then (
    let r = fresh cx in
    emit cx "  %s = call i64 @pv_run_effect(ptr %%ctx, i64 %s)" r v;
    emit cx "  call void @pv_drain_output(ptr %%ctx)")
  else emit cx "  call void @pv_print_int(i64 %s)" (force_value cx v);
  abi_pop_frame cx frame0;
  emit cx "  call void @pv_runtime_free(ptr %%ctx)";
  emit cx "  ret i32 0";
  Buffer.contents cx.fn

(* `external` decls for referenced globals not defined in this object. *)
let extern_global_decls cx ~(defined : SS.t) : string =
  SS.diff cx.externs defined
  |> SS.elements
  |> List.map (fun k -> Printf.sprintf "@%s$root = external global i64" (mangle k))
  |> String.concat "\n"

(** Emit a program as a **single self-contained** per-module-style module (ADR-0072 §2/§3): every top-level
    binding as a root-handle global `@<mangle key>$root` + `@<mangle key>$init`, one `pv_init_all`, one
    `@main`. B1's single-object form (all globals defined locally, so no `external`); [program_split] is the
    multi-object form. *)
let program_modular
      ?(is_effect = false)
      ?(heap_words = default_heap_words)
      ?(debug = false)
      (e : A.expr)
  : string
  =
  let gdefs, entry = split_spine (uniquify_toplevel e) in
  let gkeys = SS.of_list (List.concat_map gdef_keys gdefs) in
  let cx = make_cx ~gkeys ~inline_abi:(not debug) () in
  (* every binding is emitted, but only the reachable ones are initialised (dead init is pruned; the
     dead-strip link removes their now-unreferenced code/globals — ADR-0072 §3). *)
  emit_gdefs cx gdefs;
  emit_init_all cx (reachable_gdefs ~gkeys entry gdefs);
  let entry_body = emit_entry_stub cx ~is_effect ~heap_words entry in
  emit_pending cx;
  Printf.sprintf
    "; ModuleID = 'purvasm'\n\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\n\
     %s\n\
     %s\n\
     define i32 @main() {\n\
     entry:\n\
     %s}\n"
    declarations
    (abi_stamp cx)
    (extern_global_decls cx ~defined:gkeys)
    (foreign_decls cx)
    (Buffer.contents cx.globals)
    (Buffer.contents cx.md)
    entry_body

(* ==================================================================================================== *)
(* B2 — real per-module `.o` split (ADR-0072 §1/§3): each module emits an independent object (its own
   root-handle globals + init functions + internal code), and a synthesised init/entry object carries
   `pv_init_all` (calling each **reachable** binding's init in dependency order) + `@main`. The system
   linker resolves the `@<mangle>$root` / `@<mangle>$init` symbols across objects — the cross-`.o` ABI the
   injective mangle guarantees — and dead-strips the symbols of bindings a module emits but the entry does
   not reach ([reachable_gdefs], ADR-0072 §3). *)
(* ==================================================================================================== *)

(* The module a qualified key belongs to: everything before its last `.` component (`qualified_key` =
   "Module.Path.ident", so the id is the final component). A key without a `.` is its own module. *)
let module_of_key (k : string) : string =
  match String.rindex_opt k '.' with
  | Some i -> String.sub k 0 i
  | None -> k

(** Emit one module object's `.ll`: its own root-handle global definitions + init functions + internal
    code, plus `external` decls for referenced globals owned by *other* modules and `declare tailcc`
    externs for the cross-module direct entries it calls (ADR-0077 §3). No `pv_init_all`, no
    `@main` — those live in the init/entry object ([entry_ll]). *)
let module_ll
      ~(gkeys : SS.t)
      ~(xfns : fn_info SM.t)
      ~(inline_abi : bool)
      ~(defined : SS.t)
      (gdefs : gdef list)
  : string
  =
  let cx = make_cx ~gkeys ~xfns ~inline_abi () in
  emit_gdefs cx gdefs;
  emit_pending cx;
  Printf.sprintf
    "; ModuleID = 'purvasm.module'\n\n%s\n%s\n%s\n%s\n%s\n\n%s\n%s"
    declarations
    (abi_stamp cx)
    (extern_global_decls cx ~defined)
    (foreign_decls cx)
    (xfn_decls cx)
    (Buffer.contents cx.globals)
    (Buffer.contents cx.md)

(** Emit the **init/entry object**: `pv_init_all` (calling each **reachable** binding's init in dependency order) + the
    `@main` stub. Every binding's init function and every referenced root-handle global is `external`
    (defined in a module object). *)
let entry_ll
      ?(is_effect = false)
      ?(heap_words = default_heap_words)
      ~(gkeys : SS.t)
      ~(xfns : fn_info SM.t)
      ~(inline_abi : bool)
      (gdefs : gdef list)
      (entry : A.expr)
  : string
  =
  let cx = make_cx ~gkeys ~xfns ~inline_abi () in
  (* initialise only the bindings reachable from the entry (ADR-0072 §3); the unreachable module objects'
     code/globals are dropped by the dead-strip link. *)
  let reach = reachable_gdefs ~gkeys entry gdefs in
  emit_init_all cx reach;
  let entry_body = emit_entry_stub cx ~is_effect ~heap_words entry in
  emit_pending cx;
  (* the init functions this object calls are defined in the module objects *)
  let init_decls =
    List.map
      (fun g -> Printf.sprintf "declare void @%s$init(ptr)" (mangle (gdef_init_key g)))
      reach
    |> String.concat "\n"
  in
  Printf.sprintf
    "; ModuleID = 'purvasm.init'\n\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\n\
     %s\n\
     %s\n\
     define i32 @main() {\n\
     entry:\n\
     %s}\n"
    declarations
    (abi_stamp cx)
    init_decls
    (extern_global_decls cx ~defined:SS.empty)
    (foreign_decls cx)
    (xfn_decls cx)
    (Buffer.contents cx.globals)
    (Buffer.contents cx.md)
    entry_body

type split_output =
  { modules : (string * string) list (* (module name, its `.ll`) *)
  ; entry : string (* the init/entry object `.ll` *)
  ; foreigns : SS.t (* native foreign keys the program references (ADR-0073 §3) *)
  }

(** Lower a linked program to **separate module objects + one init/entry object** (ADR-0072 §1/§3). Top-level
    bindings are partitioned by their owning module (via [module_of_key]); each partition is an independent
    `.ll`. `pv_init_all` runs the **reachable** bindings ([reachable_gdefs]) in linked-spine order restricted
    to that set — a valid topological order ([Link] already spine-orders the DAG); modules still emit their
    unreachable bindings, whose symbols the dead-strip link removes.

    [surface] maps key → [call_fact], exactly as each dependency's `.pmi` publishes it
    (ADR-0077 §2 — the interface is the contract; the driver derives it from the same code
    that writes the `.pmi`, and the future artifact-driven driver reads the files). A key
    becomes direct-callable only where the natively-lowered shape **agrees** with the
    published fact (same kind, same arity): the optimised native ANF may reshape a binding
    the bytecode path did not, and on a disagreement the boundary stays generic rather than
    baking a wrong ABI. The exported-`$d` emission keys off the same intersection ([xfns]),
    so every emitted `declare` resolves at link. An empty [surface] disables cross-module
    direct calls (the pre-0077 behaviour). *)
let program_split
      ?(is_effect = false)
      ?(heap_words = default_heap_words)
      ?(surface = SM.empty)
      ?(debug = false)
      (e : A.expr)
  : split_output
  =
  let inline_abi = not debug in
  let gdefs, entry = split_spine (uniquify_toplevel e) in
  let gkeys = SS.of_list (List.concat_map gdef_keys gdefs) in
  let xfns =
    List.fold_left
      (fun acc g ->
         match g with
         | Gfun (k, ps, _) ->
           (match SM.find_opt k surface with
            | Some (Cfn n) when n = List.length ps ->
              SM.add k { dsym = mangle k ^ "$d"; arity = n; src = SSentinel } acc
            | _ -> acc)
         | Gcaf _ -> acc
         | Grec ms ->
           List.fold_left
             (fun acc (m, rhs) ->
                match rhs, SM.find_opt m surface with
                | A.Ret (A.CLam (ps, _)), Some (Crecfn n) when n = List.length ps ->
                  SM.add m { dsym = mangle m ^ "$d"; arity = n; src = SForceCell } acc
                | _ -> acc)
             acc
             ms)
      SM.empty
      gdefs
  in
  (* partition gdefs by module, preserving first-appearance module order and per-module spine order *)
  let order = ref [] in
  let tbl : (string, gdef list ref) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun g ->
       let m = module_of_key (gdef_init_key g) in
       let bucket =
         match Hashtbl.find_opt tbl m with
         | Some r -> r
         | None ->
           let r = ref [] in
           Hashtbl.add tbl m r;
           order := m :: !order;
           r
       in
       bucket := g :: !bucket)
    gdefs;
  let modules =
    List.rev !order
    |> List.map (fun m ->
      let gs = List.rev !(Hashtbl.find tbl m) in
      let defined = SS.of_list (List.concat_map gdef_keys gs) in
      m, module_ll ~gkeys ~xfns ~inline_abi ~defined gs)
  in
  { modules
  ; entry = entry_ll ~is_effect ~heap_words ~gkeys ~xfns ~inline_abi gdefs entry
  ; foreigns = cf_expr e
  }

(** The native foreign keys a [split_output] references, as a sorted list — the native build resolves each
    to a ulib `.c` (compiled and linked) or leaves it to the linker (ADR-0073 §3). *)
let foreign_keys_of (o : split_output) : string list = SS.elements o.foreigns
