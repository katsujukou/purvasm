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

(* Force through forwarding cells (top-level knot-tying, ADR-0030/0032) at every point
   a value's shape is inspected; an unfilled cell is a black hole. Values are only
   *stored* (record/array/data fields, env) without forcing, so a cyclic group builds. *)
let rec force (v : Value.t) : Value.t =
  match v with
  | Value.Vindirect cell ->
    (match !cell with
     | Value.Built v' -> force v'
     | Value.Unbuilt build ->
       cell := Value.Building;
       let v' = build () in
       cell := Value.Built v';
       force v'
     | Value.Building ->
       stuck "black hole: a recursive value was forced while being built")
  | _ -> v

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
(* PureScript `Int` is signed 32-bit with wrapping; div/mod are Euclidean (see [Cesk.Prim]). *)
let w32 (n : int) : int = Int32.to_int (Int32.of_int n)

let emod a b =
  if b = 0
  then 0
  else (
    let m = abs b in
    let r = a mod m in
    if r < 0 then r + m else r)

let ediv a b = if b = 0 then 0 else (a - emod a b) / b

(* ECMAScript `ToInt32` (JS `n | 0`), total — see [Cesk.Prim.to_int32] (ADR-0041). *)
let to_int32 (f : float) : int =
  if not (Stdlib.Float.is_finite f)
  then 0
  else w32 (Stdlib.int_of_float (Stdlib.Float.rem (Stdlib.Float.trunc f) 4294967296.0))

let eval_prim (op : C.primop) (args : V.t list) : V.t =
  match op, args with
  | C.AddInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a + b))
  | C.SubInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a - b))
  | C.MulInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a * b))
  | C.DivInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (ediv a b))
  | C.ModInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (emod a b))
  (* Bitwise ops on the signed 32-bit `Int` (see [Cesk.Prim]): shift counts mod 32, `zshr`
     is the logical (zero-fill) right shift re-wrapped to signed 32 via [w32]. *)
  | C.AndInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a land b))
  | C.OrInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a lor b))
  | C.XorInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a lxor b))
  | C.ShlInt, [ V.Vint a; V.Vint b ] -> V.Vint (w32 (a lsl (b land 31)))
  | C.ShrInt, [ V.Vint a; V.Vint b ] -> V.Vint (a asr (b land 31))
  | C.ZshrInt, [ V.Vint a; V.Vint b ] ->
    V.Vint (w32 ((a land 0xFFFFFFFF) lsr (b land 31)))
  | C.ComplementInt, [ V.Vint a ] -> V.Vint (w32 (lnot a))
  | C.AddNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a +. b)
  | C.SubNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a -. b)
  | C.MulNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a *. b)
  | C.DivNumber, [ V.Vnumber a; V.Vnumber b ] -> V.Vnumber (a /. b)
  (* Cross-representation conversions (ADR-0041), mirroring [Cesk.Prim]. *)
  | C.IntToNumber, [ V.Vint a ] -> V.Vnumber (float_of_int a)
  | C.NumberToInt, [ V.Vnumber f ] -> V.Vint (to_int32 f)
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
  (* Dynamic record access by a runtime label (ADR-0010), mirroring [Cesk.Prim]. *)
  | C.RecordGet, [ V.Vstring label; V.Vrecord m ] ->
    (match SMap.find_opt label m with
     | Some v -> v
     | None -> stuck ("record field absent: " ^ label))
  | C.RecordSet, [ V.Vstring label; v; V.Vrecord m ] -> V.Vrecord (SMap.add label v m)
  | C.RecordHas, [ V.Vstring label; V.Vrecord m ] -> V.Vbool (SMap.mem label m)
  | C.RecordDelete, [ V.Vstring label; V.Vrecord m ] -> V.Vrecord (SMap.remove label m)
  | _ -> stuck ("primop " ^ C.primop_to_string op ^ ": ill-typed arguments")

(** Take/drop helpers for the eval/apply over-application split. *)
let rec take n = function
  | _ when n = 0 -> []
  | x :: xs -> x :: take (n - 1) xs
  | [] -> []

let rec drop n l =
  if n = 0
  then l
  else (
    match l with
    | _ :: xs -> drop (n - 1) xs
    | [] -> [])

(* Scalar-literal comparison for [Switch_lit] (ADR-0031): [lit_eq] is value-level
   equality (same kind and value) used to select a case; [lit_kind_eq] tells "wrong
   value, same kind" (→ default edge) from "wrong kind" (→ stuck), matching the
   oracle's [Pmatch] on `BLit`. *)
let lit_eq (l : C.lit) (v : V.t) : bool =
  match l, v with
  | C.LInt n, V.Vint m -> n = m
  | C.LBool b, V.Vbool m -> Bool.equal b m
  | C.LString s, V.Vstring m -> String.equal s m
  | C.LNumber f, V.Vnumber m -> f = m
  | _ -> false

let lit_kind_eq (l : C.lit) (v : V.t) : bool =
  match l, v with
  | C.LInt _, V.Vint _
  | C.LBool _, V.Vbool _
  | C.LString _, V.Vstring _
  | C.LNumber _, V.Vnumber _ -> true
  | _ -> false

(** Run a frame stack to completion on a fresh operand stack, resolving free names
    through [globals]; returns the lone value left on the operand stack. Reentrant:
    [make_rec] calls it again to build each member of a recursive group. *)
let rec run
          (globals : (string, V.t) Hashtbl.t)
          ~(foreigns : string -> V.t option)
          (frames0 : frame list)
  : V.t
  =
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
       | None ->
         (* Native-foreign fall-through (ADR-0032/0033): under separate compilation a
            module references a native leaf by plain name (no resolver ran), so a
            global miss is resolved through the host registry before giving up. The
            materialised foreign is cached in the global table, so a leaf referenced in
            a hot loop is allocated once, not per reference. *)
         (match foreigns x with
          | Some v ->
            Hashtbl.replace globals x v;
            v
          | None -> stuck ("unbound variable: " ^ x)))
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
    let np = List.length c.V.params
    and na = List.length args in
    if na = np
    then enter ~tail c args
    else if na < np
    then produce ~tail (V.Vpap (c, args))
    else (
      (* Over-application: saturate with the first [np] args (entering a frame), then
         apply the rest to the result via an [Apply_more] continuation. In tail
         position the current frame is abandoned first (TCE). *)
      let first = take np args
      and rest = drop np args in
      if tail then frames := List.tl !frames;
      frames := Apply_more rest :: !frames;
      let base =
        List.fold_left2 (fun e p a -> SMap.add p a e) !(c.V.env) c.V.params first
      in
      frames
      := Code { chunk = c.V.body; ip = 0; env = ref base; share = false } :: !frames)
  and do_call ~tail (f : V.t) args =
    match force f with
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
    | V.Vforeign (name, arity, got, call) ->
      let all = got @ args in
      let na = List.length all in
      if na = arity
      then produce ~tail (call all) (* saturated: run the host fn (an effect, if any) *)
      else if na < arity
      then produce ~tail (V.Vforeign (name, arity, all, call))
      else (
        (* Over-application: saturate, then apply the rest to the result. The host
           call is synchronous, so no return frame is needed. *)
        let v = call (take arity all) in
        do_call ~tail v (drop arity all))
    | V.Vint _
    | V.Vnumber _
    | V.Vbool _
    | V.Vstring _
    | V.Varray _
    | V.Vrecord _
    | V.Vdata _
    | V.Vindirect _ -> stuck "application of a non-function"
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
           name, run globals ~foreigns [ Code { chunk; ip = 0; env = r; share = true } ])
        members
    in
    let grp = List.fold_left (fun e (n, v) -> SMap.add n v e) !r values in
    r := grp;
    fr.env := grp
  in
  let rec loop () =
    match !frames with
    | [] -> force (pop ()) (* the program's result is the lone value on the stack *)
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
        | B.Foreign_ref key ->
          (match foreigns key with
           | Some v -> push v
           | None -> stuck ("unbound native foreign: " ^ key));
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
          (match force (pop ()) with
           | V.Vrecord m ->
             (match SMap.find_opt label m with
              | Some v -> push v
              | None -> stuck ("accessor: missing label " ^ label))
           | _ -> stuck "accessor: not a record");
          loop ()
        | B.Proj i ->
          (match force (pop ()) with
           | V.Vdata (_, fields) -> push fields.(i)
           | _ -> stuck "projection: not a data value");
          loop ()
        | B.Proj_arr i ->
          (match force (pop ()) with
           | V.Varray a -> push a.(i)
           | _ -> stuck "array projection: not an array");
          loop ()
        | B.Update labels ->
          let vs = popn (List.length labels) in
          (match force (pop ()) with
           | V.Vrecord m ->
             let m' = List.fold_left2 (fun acc l v -> SMap.add l v acc) m labels vs in
             push (V.Vrecord m')
           | _ -> stuck "update: not a record");
          loop ()
        | B.Prim (op, k) ->
          let args = List.map force (popn k) in
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
          (match force (pop ()) with
           | V.Vbool false -> fr.ip <- fr.ip + rel
           | V.Vbool true -> ()
           | _ -> stuck "if: non-boolean condition");
          loop ()
        | B.Switch_ctor (cases, default) ->
          (match force (pop ()) with
           | V.Vdata (tag, _) ->
             fr.ip
             <- (fr.ip
                 +
                 match List.assoc_opt tag cases with
                 | Some rel -> rel
                 | None -> default)
           | _ -> stuck "switch on a non-data value");
          loop ()
        | B.Switch_lit (cases, default) ->
          let v = force (pop ()) in
          (match cases with
           | (l, _) :: _ when not (lit_kind_eq l v) ->
             stuck "literal switch on a wrong-kind value"
           | _ ->
             fr.ip
             <- (fr.ip
                 +
                 match List.find_opt (fun (l, _) -> lit_eq l v) cases with
                 | Some (_, rel) -> rel
                 | None -> default));
          loop ()
        | B.Switch_len (cases, default) ->
          (match force (pop ()) with
           | V.Varray a ->
             let n = Array.length a in
             fr.ip
             <- (fr.ip
                 +
                 match List.assoc_opt n cases with
                 | Some rel -> rel
                 | None -> default)
           | _ -> stuck "array-length switch on a non-array value");
          loop ()
        | B.Fail msg -> stuck msg)
  in
  loop ()

(** Run a single chunk with the given initial environment (the program [main] or a
    CAF), on a fresh stack. [foreigns] resolves native leaves (ADR-0032); default is
    none, for the pure core. *)
let run_chunk
      ?(foreigns = fun _ -> None)
      (globals : (string, V.t) Hashtbl.t)
      (chunk : B.chunk)
      (env0 : V.t SMap.t)
  : V.t
  =
  run globals ~foreigns [ Code { chunk; ip = 0; env = ref env0; share = false } ]
