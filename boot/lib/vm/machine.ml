(** The PURVASM stack interpreter (ADR-0030 slice 1): an explicit state machine over
    a heap-allocated operand stack and frame stack — never the OCaml call stack for a
    guest call — so deep guest recursion is stack-safe, and a tail call reuses the
    current frame (TCE). Calls follow the uncurried eval/apply protocol (ADR-0025):
    a saturated application enters a frame, an under-application builds a partial
    ([Vpap]/[Vctor]), and a constructor saturates to [Vdata].

    Free top-level names resolve through a mutable global table, which also closes
    recursion for top-level functions (a function finds itself there); local
    recursion is closed by [Make_rec]'s shared environment (ADR-0030). *)

module B = Bytecode
module V = Value
module C = Cesk.Ast
module SMap = Value.SMap

exception Vm_error of string

let stuck msg = raise (Vm_error msg)

(* Count of instructions dispatched — the deterministic VM cost metric (ADR-0030),
   for the benchmark harness. Kept out of the value-level result; reset by the
   measurement entry point ([Vm.eval_counted]). *)
let executed = ref 0

(* A call frame: the running chunk, the instruction pointer, and the environment of
   locals (parameters, `let`s, captured names) held in a ref. [Bind] repoints the ref
   to an extended map, so a closure that snapshotted the ref earlier is unaffected
   (immutable-environment semantics, matching the oracle). A [share] frame is the
   exception used for recursive-group construction: there a closure captures the ref
   *itself*, so the later knot-tying backpatch becomes visible to it. *)
type code_frame =
  { chunk : B.chunk
  ; mutable ip : int
  ; env : V.t SMap.t ref
  ; share : bool
  }

(* The frame stack also carries an over-application continuation: [Apply_more rest]
   applies [rest] to the value returned by the frame above it (eval/apply's
   "apply-the-rest", ADR-0025/0030). *)
type frame =
  | Code of code_frame
  | Apply_more of V.t list

(* --- Primitives: the same operations as the oracle's [Cesk.Prim], over VM values. *)
let eval_prim (op : C.primop) (args : V.t list) : V.t =
  match op, args with
  | C.AddInt, [ V.Vint a; V.Vint b ] -> V.Vint (a + b)
  | C.SubInt, [ V.Vint a; V.Vint b ] -> V.Vint (a - b)
  | C.MulInt, [ V.Vint a; V.Vint b ] -> V.Vint (a * b)
  | C.DivInt, [ V.Vint a; V.Vint b ] -> V.Vint (if b = 0 then 0 else a / b)
  | C.ModInt, [ V.Vint a; V.Vint b ] -> V.Vint (if b = 0 then 0 else a mod b)
  | C.AddNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a +. b)
  | C.SubNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a -. b)
  | C.MulNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a *. b)
  | C.DivNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a /. b)
  | C.EqInt, [ V.Vint a; V.Vint b ] -> V.Vbool (a = b)
  | C.EqString, [ V.Vstring a; V.Vstring b ] -> V.Vbool (String.equal a b)
  | C.EqNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vbool (a = b)
  | C.EqBool, [ V.Vbool a; V.Vbool b ] -> V.Vbool (Bool.equal a b)
  | C.LtInt, [ V.Vint a; V.Vint b ] -> V.Vbool (a < b)
  | C.LtString, [ V.Vstring a; V.Vstring b ] -> V.Vbool (String.compare a b < 0)
  | C.LtNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vbool (a < b)
  | C.AndBool, [ V.Vbool a; V.Vbool b ] -> V.Vbool (a && b)
  | C.OrBool, [ V.Vbool a; V.Vbool b ] -> V.Vbool (a || b)
  | C.NotBool, [ V.Vbool a ] -> V.Vbool (not a)
  | C.Append, [ V.Vstring a; V.Vstring b ] -> V.Vstring (a ^ b)
  | C.IndexArray, [ V.Varray a; V.Vint i ] ->
    if i >= 0 && i < Array.length a
    then a.(i)
    else stuck ("array index out of bounds: " ^ string_of_int i)
  | C.LengthArray, [ V.Varray a ] -> V.Vint (Array.length a)
  | C.NewArray, [ V.Vint n ] ->
    if n < 0
    then stuck ("array allocation with negative length: " ^ string_of_int n)
    else V.Varray (Array.make n (V.Vint 0))
  | C.SetArray, [ V.Varray a; V.Vint i; v ] ->
    if i >= 0 && i < Array.length a
    then (
      a.(i) <- v;
      V.Varray a)
    else stuck ("array set out of bounds: " ^ string_of_int i)
  | _ -> stuck ("primop " ^ C.primop_to_string op ^ ": ill-typed arguments")

(* --- Pattern matching: the oracle's [Cesk.Pmatch], over VM values. *)
let rec match_binder (b : C.binder) (v : V.t) acc : (string * V.t) list option =
  match b, v with
  | C.BNull, _ -> Some acc
  | C.BVar x, _ -> Some ((x, v) :: acc)
  | C.BNamed (x, inner), _ -> match_binder inner v ((x, v) :: acc)
  | C.BLit (C.LInt n), V.Vint m -> if n = m then Some acc else None
  | C.BLit (C.LBool b'), V.Vbool m -> if Bool.equal b' m then Some acc else None
  | C.BLit (C.LString s), V.Vstring m -> if String.equal s m then Some acc else None
  | C.BLit (C.LNumber f), V.Vnumber m -> if f = m then Some acc else None
  | C.BCtor (tag, subs), V.Vdata (vtag, fields) ->
    if String.equal tag vtag
    then
      if List.length subs = Array.length fields
      then match_binders subs (Array.to_list fields) acc
      else stuck ("constructor pattern arity mismatch: " ^ tag)
    else None
  | C.BArray subs, V.Varray arr ->
    if List.length subs = Array.length arr
    then match_binders subs (Array.to_list arr) acc
    else None
  | C.BRecord fields, V.Vrecord m -> match_fields fields m acc
  | (C.BLit _ | C.BCtor _ | C.BArray _ | C.BRecord _), _ ->
    stuck "pattern matched against a value of the wrong shape"

and match_binders binders values acc =
  match binders, values with
  | [], [] -> Some acc
  | b :: bs, v :: vs ->
    (match match_binder b v acc with
     | Some acc' -> match_binders bs vs acc'
     | None -> None)
  | _ -> stuck "pattern arity mismatch"

and match_fields fields m acc =
  match fields with
  | [] -> Some acc
  | (label, p) :: rest ->
    (match SMap.find_opt label m with
     | Some value ->
       (match match_binder p value acc with
        | Some acc' -> match_fields rest m acc'
        | None -> None)
     | None -> stuck ("record pattern: missing label " ^ label))

(** Take/drop helpers for the eval/apply over-application split. *)
let rec take n = function
  | _ when n = 0 -> []
  | x :: xs -> x :: take (n - 1) xs
  | [] -> []

let rec drop n l = if n = 0 then l else match l with _ :: xs -> drop (n - 1) xs | [] -> []

(** Run a frame stack to completion on a fresh operand stack, resolving free names
    through [globals]; returns the lone value left on the operand stack. Reentrant:
    [make_rec] calls it again to build each member of a recursive group. *)
let rec run (globals : (string, V.t) Hashtbl.t) (frames0 : frame list) : V.t =
  let stack = ref [] in
  let push v = stack := v :: !stack in
  let pop () =
    match !stack with
    | v :: r ->
      stack := r;
      v
    | [] -> stuck "operand stack underflow"
  in
  (* Pop n values, returned in original push order [a1; …; an]. *)
  let popn n =
    let rec go k acc = if k = 0 then acc else go (k - 1) (pop () :: acc) in
    go n []
  in
  let frames = ref frames0 in
  let lookup envref x =
    match SMap.find_opt x !envref with
    | Some v -> v
    | None ->
      (match Hashtbl.find_opt globals x with
       | Some v -> v
       | None -> stuck ("unbound variable: " ^ x))
  in
  let do_return () = frames := List.tl !frames in
  (* A computed value (partial application / saturated constructor) is pushed; in
     tail position it is the frame's result, so return. *)
  let produce ~tail v =
    push v;
    if tail then do_return ()
  in
  let enter ~tail (c : V.closure) args =
    let base = List.fold_left2 (fun e p a -> SMap.add p a e) !(c.V.env) c.V.params args in
    let fr = Code { chunk = c.V.body; ip = 0; env = ref base; share = false } in
    if tail then frames := fr :: List.tl !frames else frames := fr :: !frames
  in
  let rec apply_closure ~tail (c : V.closure) args =
    let np = List.length c.V.params and na = List.length args in
    if na = np
    then enter ~tail c args
    else if na < np
    then produce ~tail (V.Vpap (c, args))
    else (
      (* Over-application: saturate with the first [np] args (entering a frame), then
         apply the rest to the result via an [Apply_more] continuation. In tail
         position the current frame is abandoned first (TCE). *)
      let first = take np args and rest = drop np args in
      if tail then frames := List.tl !frames;
      frames := Apply_more rest :: !frames;
      let base =
        List.fold_left2 (fun e p a -> SMap.add p a e) !(c.V.env) c.V.params first
      in
      frames := Code { chunk = c.V.body; ip = 0; env = ref base; share = false } :: !frames)
  and do_call ~tail (f : V.t) args =
    match f with
    | V.Vclosure c -> apply_closure ~tail c args
    | V.Vpap (c, got) -> apply_closure ~tail c (got @ args)
    | V.Vctor (tag, arity, got) ->
      let all = got @ args in
      let na = List.length all in
      if na = arity
      then produce ~tail (V.Vdata (tag, Array.of_list all))
      else if na < arity
      then produce ~tail (V.Vctor (tag, arity, all))
      else stuck ("constructor " ^ tag ^ " over-applied")
    | V.Vint _ | V.Vnumber _ | V.Vbool _ | V.Vstring _ | V.Varray _ | V.Vrecord _
    | V.Vdata _ -> stuck "application of a non-function"
  in
  let make_rec (fr : code_frame) (members : (string * B.chunk) list) =
    (* Knot-tying (ADR-0030): build each member in a [share] frame over one ref [r]
       (so the members' closures capture [r]), then backpatch [r] — and the enclosing
       frame — with the whole group. The self/mutual references sit under those
       closures, so they resolve correctly once [r] holds the group. *)
    let r = ref !(fr.env) in
    let values =
      List.map
        (fun (name, chunk) ->
          (name, run globals [ Code { chunk; ip = 0; env = r; share = true } ]))
        members
    in
    let grp = List.fold_left (fun e (n, v) -> SMap.add n v e) !r values in
    r := grp;
    fr.env := grp
  in
  let do_match (fr : code_frame) nscrut (alts : B.matchalt array) =
    let scruts = popn nscrut in
    let rec try_alts i =
      if i >= Array.length alts
      then stuck "case: no matching alternative"
      else (
        let alt = alts.(i) in
        match match_binders alt.B.binders scruts [] with
        | Some bindings ->
          fr.env := List.fold_left (fun e (x, v) -> SMap.add x v e) !(fr.env) bindings;
          fr.ip <- fr.ip + alt.B.target
        | None -> try_alts (i + 1))
    in
    try_alts 0
  in
  let rec loop () =
    match !frames with
    | [] -> pop () (* the program's result is the lone value on the stack *)
    | Apply_more rest :: _ ->
      let v = pop () in
      frames := List.tl !frames;
      do_call ~tail:false v rest;
      loop ()
    | Code fr :: _ ->
      if fr.ip >= Array.length fr.chunk
      then stuck "instruction pointer past end of chunk"
      else (
        let instr = fr.chunk.(fr.ip) in
        fr.ip <- fr.ip + 1;
        incr executed;
        match instr with
        | B.Push_int n ->
          push (V.Vint n);
          loop ()
        | B.Push_number f ->
          push (V.Vnumber f);
          loop ()
        | B.Push_bool b ->
          push (V.Vbool b);
          loop ()
        | B.Push_string s ->
          push (V.Vstring s);
          loop ()
        | B.Load x ->
          push (lookup fr.env x);
          loop ()
        | B.Bind x ->
          let v = pop () in
          fr.env := SMap.add x v !(fr.env);
          loop ()
        | B.Closure (ps, body) ->
          let env = if fr.share then fr.env else ref !(fr.env) in
          push (V.Vclosure { V.params = ps; body; env });
          loop ()
        | B.Make_rec members ->
          make_rec fr members;
          loop ()
        | B.Ctor (tag, arity, k) ->
          let args = popn k in
          push
            (if k = arity
             then V.Vdata (tag, Array.of_list args)
             else V.Vctor (tag, arity, args));
          loop ()
        | B.Record labels ->
          let vs = popn (List.length labels) in
          let m =
            List.fold_left2 (fun acc l v -> SMap.add l v acc) SMap.empty labels vs
          in
          push (V.Vrecord m);
          loop ()
        | B.Array k ->
          let vs = popn k in
          push (V.Varray (Array.of_list vs));
          loop ()
        | B.Get_field label ->
          (match pop () with
           | V.Vrecord m ->
             (match SMap.find_opt label m with
              | Some v -> push v
              | None -> stuck ("accessor: missing label " ^ label))
           | _ -> stuck "accessor: not a record");
          loop ()
        | B.Update labels ->
          let vs = popn (List.length labels) in
          (match pop () with
           | V.Vrecord m ->
             let m' = List.fold_left2 (fun acc l v -> SMap.add l v acc) m labels vs in
             push (V.Vrecord m')
           | _ -> stuck "update: not a record");
          loop ()
        | B.Prim (op, k) ->
          let args = popn k in
          push (eval_prim op args);
          loop ()
        | B.Call k ->
          let args = popn k in
          let f = pop () in
          do_call ~tail:false f args;
          loop ()
        | B.Tail_call k ->
          let args = popn k in
          let f = pop () in
          do_call ~tail:true f args;
          loop ()
        | B.Return ->
          do_return ();
          loop ()
        | B.Jump rel ->
          fr.ip <- fr.ip + rel;
          loop ()
        | B.Jump_unless rel ->
          (match pop () with
           | V.Vbool false -> fr.ip <- fr.ip + rel
           | V.Vbool true -> ()
           | _ -> stuck "if: non-boolean condition");
          loop ()
        | B.Match (nscrut, alts) ->
          do_match fr nscrut alts;
          loop ()
        | B.Test (nscrut, binders, fail_rel) ->
          (* Peek (do not pop) the scrutinees so the next alternative can re-match. *)
          let scruts = List.rev (take nscrut !stack) in
          (match match_binders binders scruts [] with
           | Some bindings ->
             fr.env
             := List.fold_left (fun e (x, v) -> SMap.add x v e) !(fr.env) bindings
           | None -> fr.ip <- fr.ip + fail_rel);
          loop ()
        | B.Drop k ->
          for _ = 1 to k do
            ignore (pop ())
          done;
          loop ()
        | B.Fail msg -> stuck msg)
  in
  loop ()

(** Run a single chunk with the given initial environment (the program [main] or a
    CAF), on a fresh stack. *)
let run_chunk (globals : (string, V.t) Hashtbl.t) (chunk : B.chunk) (env0 : V.t SMap.t)
  : V.t
  =
  run globals [ Code { chunk; ip = 0; env = ref env0; share = false } ]
