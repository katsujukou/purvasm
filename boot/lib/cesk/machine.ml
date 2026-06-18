open Base

(* The CESK machine itself: a small-step transition relation over states of the
   form (focus, store, kont). `Eval` is the "evaluate this term" mode; `Return`
   is the "hand this value to the continuation" mode. Every transition is total
   and explicit — no host recursion hides in the evaluation of subterms — which
   is what will let us later pause and resume a computation at any point. *)

type focus =
  | Eval of Ast.term * Env.t
  | Return of Value.t

type state =
  { focus : focus
  ; store : Store.t
  ; kont : Cont.t
  }

type result =
  | Step of state
  | Done of Value.t

let inject (term : Ast.term) : state =
  { focus = Eval (term, Env.empty); store = Store.empty; kont = Cont.Halt }

(* Allocate each pattern binding in the store and extend the environment with it
   — the same name -> address -> value indirection every binder uses (ADR-0011). *)
let bind_all (bindings : (string * Value.t) list) (store : Store.t) (env : Env.t)
  : Store.t * Env.t
  =
  List.fold bindings ~init:(store, env) ~f:(fun (st, e) (name, value) ->
    let addr, st' = Store.alloc st value in
    st', Env.extend e name addr)

(* Scan alternatives top to bottom for the first whose binders match the
   scrutinee values, then take its right-hand side (ADR-0011/0013). An
   unconditional arm runs immediately; a guarded arm binds its patterns and
   evaluates the first guard under a `Guard_test` frame, which on failure falls
   back here with the remaining alternatives. Matching is a pure structural walk
   (`Pmatch`); only the chosen result/guard re-enters the machine. No matching
   alternative is a stuck state — well-typed CoreFn is exhaustive. *)
let rec dispatch_case
          (values : Value.t list)
          (alts : Ast.alternative list)
          (store : Store.t)
          (env : Env.t)
          (kont : Cont.t)
  : result
  =
  match alts with
  | [] -> Errors.stuck "no matching case alternative"
  | { Ast.binders; result } :: rest ->
    (match Pmatch.match_binders binders values [] with
     | None -> dispatch_case values rest store env kont
     | Some bindings ->
       (match result with
        | Ast.Unconditional body ->
          let store', env' = bind_all bindings store env in
          Step { focus = Eval (body, env'); store = store'; kont }
        (* An empty guard list never holds; fall through (defensive — CoreFn's
           guard list is non-empty). The speculative bindings are not allocated. *)
        | Ast.Guarded [] -> dispatch_case values rest store env kont
        | Ast.Guarded ((g, e) :: more) ->
          let store', env' = bind_all bindings store env in
          Step
            { focus = Eval (g, env')
            ; store = store'
            ; kont =
                Cont.Guard_test
                  { on_true = e
                  ; rest_guards = more
                  ; alt_env = env'
                  ; rest_alts = rest
                  ; scrutinees = values
                  ; case_env = env
                  ; rest = kont
                  }
            }))

let step (s : state) : result =
  match s.focus with
  | Eval (term, env) ->
    (match term with
     | Ast.Lit (Ast.LInt n) -> Step { s with focus = Return (Value.VInt n) }
     | Ast.Lit (Ast.LNumber f) -> Step { s with focus = Return (Value.VNumber f) }
     | Ast.Lit (Ast.LBool b) -> Step { s with focus = Return (Value.VBool b) }
     | Ast.Lit (Ast.LString str) -> Step { s with focus = Return (Value.VString str) }
     | Ast.Var x -> Step { s with focus = Return (Store.find s.store (Env.lookup env x)) }
     | Ast.Lam (param, body) ->
       Step { s with focus = Return (Value.VClosure { param; body; env }) }
     | Ast.App (f, a) ->
       Step { s with focus = Eval (f, env); kont = Cont.Fun (a, env, s.kont) }
     | Ast.Let (x, e1, e2) ->
       Step { s with focus = Eval (e1, env); kont = Cont.Let_body (x, e2, env, s.kont) }
     | Ast.Letrec (binds, body) ->
       (* Reserve an address for every binding up front so all names are in
          scope in every right-hand side (mutual recursion), then evaluate the
          right-hand sides left to right, backpatching each (Cont.Letrec_bind). *)
       let store', targets =
         List.fold_map binds ~init:s.store ~f:(fun st (x, rhs) ->
           let addr, st' = Store.reserve st in
           st', (x, addr, rhs))
       in
       let env' =
         List.fold targets ~init:env ~f:(fun e (x, addr, _) -> Env.extend e x addr)
       in
       (match targets with
        | [] -> Step { s with focus = Eval (body, env') }
        | (_, addr1, e1) :: rest ->
          let pending = List.map rest ~f:(fun (_, addr, rhs) -> addr, rhs) in
          Step
            { focus = Eval (e1, env')
            ; store = store'
            ; kont = Cont.Letrec_bind (addr1, pending, body, env', s.kont)
            })
     | Ast.If (c, t, e) ->
       Step { s with focus = Eval (c, env); kont = Cont.If_branch (t, e, env, s.kont) }
     | Ast.Prim (op, args) ->
       (match args with
        | [] -> Step { s with focus = Return (Prim.eval op []) }
        | a :: rest ->
          Step
            { s with
              focus = Eval (a, env)
            ; kont = Cont.Prim_args (op, [], rest, env, s.kont)
            })
     | Ast.Array elems ->
       (match elems with
        | [] -> Step { s with focus = Return (Value.VArray [||]) }
        | e1 :: rest ->
          Step
            { s with
              focus = Eval (e1, env)
            ; kont = Cont.Array_elems ([], rest, env, s.kont)
            })
     | Ast.Record fields ->
       (match fields with
        | [] -> Step { s with focus = Return (Value.VRecord (Map.empty (module String))) }
        | (l, e) :: rest ->
          Step
            { s with
              focus = Eval (e, env)
            ; kont = Cont.Record_fields (Map.empty (module String), l, rest, env, s.kont)
            })
     | Ast.Accessor (e, l) ->
       Step { s with focus = Eval (e, env); kont = Cont.Project (l, s.kont) }
     | Ast.Update (e, ups) ->
       Step { s with focus = Eval (e, env); kont = Cont.Update_rec (ups, env, s.kont) }
     | Ast.Ctor (tag, arity) ->
       (* A nullary constructor is data right away; otherwise it is a curried
          constructor function that the Arg rule fills in field by field. *)
       if arity = 0
       then Step { s with focus = Return (Value.VData { tag; fields = [||] }) }
       else Step { s with focus = Return (Value.VCtor { tag; arity; args = [] }) }
     | Ast.Case (scruts, alts) ->
       (match scruts with
        | [] -> dispatch_case [] alts s.store env s.kont
        | e1 :: rest ->
          Step
            { s with
              focus = Eval (e1, env)
            ; kont = Cont.Case_scrut ([], rest, alts, env, s.kont)
            }))
  | Return v ->
    (match s.kont with
     | Cont.Halt -> Done v
     | Cont.Fun (a, env, k) ->
       Step { s with focus = Eval (a, env); kont = Cont.Arg (v, k) }
     | Cont.Arg (vfun, k) ->
       (match vfun with
        | Value.VClosure { param; body; env = closure_env } ->
          let addr, store' = Store.alloc s.store v in
          let env' = Env.extend closure_env param addr in
          Step { focus = Eval (body, env'); store = store'; kont = k }
        | Value.VCtor { tag; arity; args } ->
          (* Collect one more field. Saturating the arity turns the partial
             constructor into data. Like array/record literals, this allocates
             nothing in the store — the field value lives inside the data. *)
          let args = v :: args in
          if List.length args = arity
          then
            Step
              { s with
                focus =
                  Return (Value.VData { tag; fields = Array.of_list (List.rev args) })
              ; kont = k
              }
          else Step { s with focus = Return (Value.VCtor { tag; arity; args }); kont = k }
        | _ -> Errors.stuck "application of a non-function")
     | Cont.Let_body (x, e2, env, k) ->
       let addr, store' = Store.alloc s.store v in
       let env' = Env.extend env x addr in
       Step { focus = Eval (e2, env'); store = store'; kont = k }
     | Cont.Letrec_bind (addr, pending, body, env, k) ->
       (* Backpatch the address just computed; a closure built earlier in the
          group already points here, so this write is what makes the recursion
          observable. Then move to the next right-hand side, or the body. *)
       let store' = Store.set s.store addr v in
       (match pending with
        | [] -> Step { focus = Eval (body, env); store = store'; kont = k }
        | (addr', e) :: rest ->
          Step
            { focus = Eval (e, env)
            ; store = store'
            ; kont = Cont.Letrec_bind (addr', rest, body, env, k)
            })
     | Cont.If_branch (t, e, env, k) ->
       (match v with
        | Value.VBool b ->
          Step { focus = Eval ((if b then t else e), env); store = s.store; kont = k }
        | _ -> Errors.stuck "if-condition is not a boolean")
     | Cont.Prim_args (op, done_, remaining, env, k) ->
       let done_ = v :: done_ in
       (match remaining with
        | [] -> Step { s with focus = Return (Prim.eval op (List.rev done_)); kont = k }
        | a :: rest ->
          Step
            { s with
              focus = Eval (a, env)
            ; kont = Cont.Prim_args (op, done_, rest, env, k)
            })
     | Cont.Array_elems (done_, remaining, env, k) ->
       let done_ = v :: done_ in
       (match remaining with
        | [] ->
          Step
            { s with
              focus = Return (Value.VArray (Array.of_list (List.rev done_)))
            ; kont = k
            }
        | e :: rest ->
          Step
            { s with
              focus = Eval (e, env)
            ; kont = Cont.Array_elems (done_, rest, env, k)
            })
     | Cont.Record_fields (m, l, remaining, env, k) ->
       let m = Map.set m ~key:l ~data:v in
       (match remaining with
        | [] -> Step { s with focus = Return (Value.VRecord m); kont = k }
        | (l2, e2) :: rest ->
          Step
            { s with
              focus = Eval (e2, env)
            ; kont = Cont.Record_fields (m, l2, rest, env, k)
            })
     | Cont.Project (l, k) ->
       (match v with
        | Value.VRecord m ->
          (match Map.find m l with
           | Some value -> Step { s with focus = Return value; kont = k }
           | None -> Errors.stuck ("record has no field: " ^ l))
        | _ -> Errors.stuck "projection of a non-record")
     | Cont.Update_rec (ups, env, k) ->
       (match v with
        | Value.VRecord m ->
          (match ups with
           | [] -> Step { s with focus = Return (Value.VRecord m); kont = k }
           | (l, e) :: rest ->
             Step
               { s with
                 focus = Eval (e, env)
               ; kont = Cont.Record_fields (m, l, rest, env, k)
               })
        | _ -> Errors.stuck "record update of a non-record")
     | Cont.Case_scrut (done_, remaining, alts, env, k) ->
       let done_ = v :: done_ in
       (match remaining with
        | [] -> dispatch_case (List.rev done_) alts s.store env k
        | e :: rest ->
          Step
            { s with
              focus = Eval (e, env)
            ; kont = Cont.Case_scrut (done_, rest, alts, env, k)
            })
     | Cont.Guard_test
         { on_true; rest_guards; alt_env; rest_alts; scrutinees; case_env; rest } ->
       (match v with
        | Value.VBool true -> Step { s with focus = Eval (on_true, alt_env); kont = rest }
        | Value.VBool false ->
          (match rest_guards with
           | (g2, e2) :: more ->
             Step
               { s with
                 focus = Eval (g2, alt_env)
               ; kont =
                   Cont.Guard_test
                     { on_true = e2
                     ; rest_guards = more
                     ; alt_env
                     ; rest_alts
                     ; scrutinees
                     ; case_env
                     ; rest
                     }
               }
           (* All guards of this alternative failed: fall through to the next
              alternative, re-matching it against the same scrutinee values. *)
           | [] -> dispatch_case scrutinees rest_alts s.store case_env rest)
        | _ -> Errors.stuck "case guard is not a boolean"))

let state_to_string (s : state) : string =
  let focus =
    match s.focus with
    | Eval (t, _) -> "eval " ^ Ast.to_string t
    | Return v -> "ret  " ^ Value.to_string v
  in
  Stdlib.Printf.sprintf
    "[ %-28s | k:%-4s | store:%d ]"
    focus
    (Cont.frame_name s.kont)
    (Store.size s.store)

let rec run ?(trace = false) (s : state) : Value.t =
  if trace then Stdlib.print_endline (state_to_string s);
  match step s with
  | Done v -> v
  | Step s' -> run ~trace s'

let eval ?(trace = false) (term : Ast.term) : Value.t = run ~trace (inject term)
