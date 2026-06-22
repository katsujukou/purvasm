(** ANF → PURVASM bytecode (ADR-0030 slice 1), by postorder emit (ADR-0003): a
    subexpression's operands are pushed first, so it leaves exactly one value on the
    operand stack. An `if` compiles to *relative* jumps within a chunk; calls in tail
    position become [Tail_call] (TCE), elsewhere [Call]; a `case` is delegated to
    [Match_compile] (ADR-0031), supplied with this module's atom/expr compilers.

    The top-level spine (the linked program is one big let-/letrec-spine, ADR-0016)
    is split into global definitions — a function becomes a [Gfun] closure, any other
    binding a [Gcaf] thunk run once at start-up in dependency order (ADR-0030's
    eager construction) — and a [main] chunk for the entry expression. *)

module A = Middle_end.Anf
module C = Cesk.Ast
module B = Bytecode

(** A top-level binding: a function (a global closure, seeing the whole table —
    recursion included); a non-recursive CAF ([Gcaf], built strictly once at start-up,
    in spine order); or a recursive-group CAF ([Grec], built by-need so a cyclic
    instance-dictionary group can close, mirroring the oracle's ADR-0024). *)
type gdef =
  | Gfun of string list * B.chunk
  | Gcaf of B.chunk
  | Grec of B.chunk

let len = List.length

let gen_atom : A.atom -> B.instr = function
  | A.AVar x -> B.Load x
  | A.ALit (C.LInt n) -> B.Push_int n
  | A.ALit (C.LNumber f) -> B.Push_number f
  | A.ALit (C.LBool b) -> B.Push_bool b
  | A.ALit (C.LString s) -> B.Push_string s
  (* A native foreign leaf (ADR-0032): materialised from the host registry at runtime;
     stuck if the program reaches one the host does not provide. *)
  | A.AForeign s -> B.Foreign_ref s

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
  Match_compile.compile ~atom:gen_atom ~body:gen_expr ~tail scruts alts

(** Split the top-level spine into global definitions (in dependency order) and the
    [main] chunk. A function becomes a [Gfun]; a non-recursive `let` value a strict
    [Gcaf]; a recursive group's non-function member a by-need [Grec] (so a cyclic
    instance-dictionary group can close, ADR-0024/0032); the first non-binding
    expression is [main]. *)
let program (e : A.expr) : (string * gdef) list * B.chunk =
  let rec walk acc = function
    | A.Let (k, A.CLam (ps, b), rest) -> walk ((k, Gfun (ps, fn_chunk b)) :: acc) rest
    | A.Let (k, c, rest) ->
      walk ((k, Gcaf (Array.of_list (gen_cexpr true c))) :: acc) rest
    | A.LetRec (binds, rest) ->
      (* A recursive group: a function member is closed by the global table (it finds
         its peers there at call time); a value member (e.g. an `Effect` instance
         dictionary, or `fibAnd = Fib … \n -> … fibAnd …`) is built by-need so a
         strict reference within the cycle resolves once a sibling is forced. *)
      let defs =
        List.map
          (fun (name, def) ->
            match def with
            | A.Ret (A.CLam (ps, b)) -> (name, Gfun (ps, fn_chunk b))
            | _ -> (name, Grec (Array.of_list (gen_expr true def))))
          binds
      in
      walk (List.rev_append defs acc) rest
    | main -> (List.rev acc, Array.of_list (gen_expr true main))
  in
  walk [] e
