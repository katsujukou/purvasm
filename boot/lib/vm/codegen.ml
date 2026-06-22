(** ANF → PURVASM bytecode (ADR-0030 slice 1), by postorder emit (ADR-0003): a
    subexpression's operands are pushed first, so it leaves exactly one value on the
    operand stack. Control flow (`if`/`case`) compiles to *relative* jumps within a
    chunk; calls in tail position become [Tail_call] (TCE), elsewhere [Call].

    The top-level spine (the linked program is one big let-/letrec-spine, ADR-0016)
    is split into global definitions — a function becomes a [Gfun] closure, any other
    binding a [Gcaf] thunk run once at start-up in dependency order (ADR-0030's
    eager construction) — and a [main] chunk for the entry expression. *)

module A = Middle_end.Anf
module C = Cesk.Ast
module B = Bytecode

(** A top-level binding: a function (installed as a global closure, so it sees the
    whole global table — recursion included) or a CAF (evaluated once at start-up). *)
type gdef =
  | Gfun of string list * B.chunk
  | Gcaf of B.chunk

let len = List.length

let gen_atom : A.atom -> B.instr = function
  | A.AVar x -> B.Load x
  | A.ALit (C.LInt n) -> B.Push_int n
  | A.ALit (C.LNumber f) -> B.Push_number f
  | A.ALit (C.LBool b) -> B.Push_bool b
  | A.ALit (C.LString s) -> B.Push_string s
  (* A native foreign is out of slice 1 (pure core); emit a load that is only stuck
     if actually forced — pure programs never reach it (ADR-0030 scope). *)
  | A.AForeign s -> B.Load s

let gen_atoms atoms = List.map gen_atom atoms

(* [tail] is threaded so that a call in tail position becomes [Tail_call] and a
   tail computation ends with [Return]; control constructs propagate it to their
   branches. *)
let rec gen_expr (tail : bool) (e : A.expr) : B.instr list =
  match e with
  | A.Ret c -> gen_cexpr tail c
  | A.Let (x, c, rest) -> gen_cexpr false c @ (B.Bind x :: gen_expr tail rest)
  | A.LetRec (binds, rest) ->
    B.Make_rec (List.map (fun (name, def) -> (name, fn_chunk def)) binds)
    :: gen_expr tail rest

and fn_chunk (body : A.expr) : B.chunk = Array.of_list (gen_expr true body)

and gen_cexpr (tail : bool) (c : A.cexpr) : B.instr list =
  match c with
  | A.CApp (h, args) ->
    (gen_atom h :: gen_atoms args)
    @ [ (if tail then B.Tail_call (len args) else B.Call (len args)) ]
  | A.CIf (a, t, e) ->
    let tc = gen_expr tail t and ec = gen_expr tail e in
    if tail
    then (* each branch ends with Return/Tail_call; skip [then] when false *)
      gen_atom a :: B.Jump_unless (len tc) :: (tc @ ec)
    else
      (* each branch leaves a value; [then] jumps over [else] to the join point *)
      gen_atom a :: B.Jump_unless (len tc + 1) :: (tc @ (B.Jump (len ec) :: ec))
  | A.CCase (scruts, alts) -> gen_case tail scruts alts
  | (A.CAtom _ | A.CPrim _ | A.CCtor _ | A.CArray _ | A.CRecord _ | A.CAccessor _
    | A.CUpdate _ | A.CLam _) as v -> gen_value v @ if tail then [ B.Return ] else []

(* The non-control computations: each leaves exactly one value on the stack. *)
and gen_value (c : A.cexpr) : B.instr list =
  match c with
  | A.CAtom a -> [ gen_atom a ]
  | A.CPrim (op, args) -> gen_atoms args @ [ B.Prim (op, len args) ]
  | A.CCtor (tag, arity, args) -> gen_atoms args @ [ B.Ctor (tag, arity, len args) ]
  | A.CArray atoms -> gen_atoms atoms @ [ B.Array (len atoms) ]
  | A.CRecord fields ->
    List.map (fun (_, a) -> gen_atom a) fields @ [ B.Record (List.map fst fields) ]
  | A.CAccessor (a, label) -> [ gen_atom a; B.Get_field label ]
  | A.CUpdate (a, ups) ->
    (gen_atom a :: List.map (fun (_, x) -> gen_atom x) ups)
    @ [ B.Update (List.map fst ups) ]
  | A.CLam (ps, b) -> [ B.Closure (ps, fn_chunk b) ]
  | A.CApp _ | A.CIf _ | A.CCase _ -> assert false (* handled in gen_cexpr *)

and gen_case (tail : bool) (scruts : A.atom list) (alts : A.alt list) : B.instr list =
  if List.for_all (fun (a : A.alt) -> match a.A.result with A.Uncond _ -> true | _ -> false) alts
  then gen_case_simple tail scruts alts
  else gen_case_guarded tail scruts alts

(* An unconditional `case`: one host-side [Match] selects and binds, then jumps to
   the chosen body. The benchmarks take this fast path. *)
and gen_case_simple (tail : bool) (scruts : A.atom list) (alts : A.alt list)
  : B.instr list
  =
  let n = len scruts in
  let bodies =
    List.map
      (fun (alt : A.alt) ->
        match alt.A.result with
        | A.Uncond e -> gen_expr tail e
        | A.Guarded _ -> assert false)
      alts
  in
  (* Targets are relative to the position right after [Match] (the bodies area). In
     non-tail position each body leaves a value then jumps to the join point; in
     tail position each body ends with Return/Tail_call (no jump needed). *)
  let body_len body = len body + if tail then 0 else 1 in
  let total = List.fold_left (fun acc b -> acc + body_len b) 0 bodies in
  let _, alts_rev, code_rev =
    List.fold_left2
      (fun (off, alts_acc, code_acc) (alt : A.alt) body ->
        let alt' = { B.binders = alt.A.binders; target = off } in
        let after = off + body_len body in
        let code = if tail then body else body @ [ B.Jump (total - after) ] in
        (after, alt' :: alts_acc, code :: code_acc))
      (0, [], []) alts bodies
  in
  let match_alts = Array.of_list (List.rev alts_rev) in
  let bodies_code = List.concat (List.rev code_rev) in
  gen_atoms scruts @ (B.Match (n, match_alts) :: bodies_code)

(* A `case` with at least one guarded alternative (ADR-0013). Each alternative is a
   [Test] (peek + match binders, jump to the next alternative on failure) followed by
   its guard chain; a guard that is false tries the next guard, and the last guard
   falling through goes to the next alternative — so scrutinees stay on the stack
   across alternatives and are dropped only when a body is finally chosen. Emitted
   through a small backpatching assembler (placeholder offsets resolved to relative
   jumps at the end); spliced guard/body chunks keep their own internal jumps. *)
and gen_case_guarded (tail : bool) (scruts : A.atom list) (alts : A.alt list)
  : B.instr list
  =
  let n = len scruts in
  let buf = ref [] (* reversed *) and pos = ref 0 in
  let labels : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let fixups = ref [] (* (position, target-label) for Jump / Jump_unless / Test *) in
  let emit instr =
    buf := instr :: !buf;
    incr pos
  in
  let emit_list = List.iter emit in
  let mark lbl = Hashtbl.replace labels lbl !pos in
  let to_lbl lbl instr =
    fixups := (!pos, lbl) :: !fixups;
    emit instr
  in
  emit_list (gen_atoms scruts);
  let finish_body e =
    emit (B.Drop n);
    emit_list (gen_expr tail e);
    if not tail then to_lbl "end" (B.Jump 0)
  in
  List.iteri
    (fun i (alt : A.alt) ->
      mark (Printf.sprintf "alt%d" i);
      let next = Printf.sprintf "alt%d" (i + 1) in
      to_lbl next (B.Test (n, alt.A.binders, 0));
      match alt.A.result with
      | A.Uncond e -> finish_body e
      | A.Guarded clauses ->
        let nc = List.length clauses in
        List.iteri
          (fun j (guard, body) ->
            mark (Printf.sprintf "alt%d_g%d" i j);
            emit_list (gen_expr false guard);
            let on_false =
              if j = nc - 1 then next else Printf.sprintf "alt%d_g%d" i (j + 1)
            in
            to_lbl on_false (B.Jump_unless 0);
            finish_body body)
          clauses)
    alts;
  mark (Printf.sprintf "alt%d" (List.length alts));
  emit (B.Fail "case: no matching alternative");
  mark "end";
  let arr = Array.of_list (List.rev !buf) in
  List.iter
    (fun (p, lbl) ->
      let rel = Hashtbl.find labels lbl - (p + 1) in
      arr.(p)
      <- (match arr.(p) with
          | B.Jump _ -> B.Jump rel
          | B.Jump_unless _ -> B.Jump_unless rel
          | B.Test (nn, bs, _) -> B.Test (nn, bs, rel)
          | other -> other))
    !fixups;
  Array.to_list arr

(** Split the top-level spine into global definitions (in dependency order) and the
    [main] chunk. Top-level functions (including recursive groups) become [Gfun]s;
    other bindings become [Gcaf]s; the first non-binding expression is [main]. *)
let program (e : A.expr) : (string * gdef) list * B.chunk =
  let rec walk acc = function
    | A.Let (k, A.CLam (ps, b), rest) -> walk ((k, Gfun (ps, fn_chunk b)) :: acc) rest
    | A.Let (k, c, rest) ->
      walk ((k, Gcaf (Array.of_list (gen_cexpr true c))) :: acc) rest
    | A.LetRec (binds, rest) ->
      (* A top-level recursive group is closed by the global table: a function finds
         itself (and its peers) there at call time, and a recursive *value* (e.g.
         `fibAnd = Fib … \n -> … fibAnd …`) is a CAF whose inner closure resolves the
         group through the table once the CAF result is installed. *)
      let defs =
        List.map
          (fun (name, def) ->
            match def with
            | A.Ret (A.CLam (ps, b)) -> (name, Gfun (ps, fn_chunk b))
            | _ -> (name, Gcaf (Array.of_list (gen_expr true def))))
          binds
      in
      walk (List.rev_append defs acc) rest
    | main -> (List.rev acc, Array.of_list (gen_expr true main))
  in
  walk [] e
