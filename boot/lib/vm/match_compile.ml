(** Lower a `case` to PURVASM bytecode (ADR-0031, ADR-0083). The Maranget decision tree
    is now built, once and backend-agnostically, by [Middle_end.Match_compile]; this
    module is the *bytecode* lowering of that tree ([lower_dtree]) plus the
    per-alternative re-testing baseline ([compile_naive], selected by
    [use_naive_matching]) the tree is measured against.

    The tree's occurrences are bound to fresh locals, so the operand stack is empty at
    every branch boundary (ADR-0003); its labels are back-patched to *relative* offsets
    by [resolve]. The compilers are parameterised by [atom] (compile a scrutinee atom)
    and [body] (compile an alternative's right-hand side, tail-aware), supplied by
    [Codegen]. *)

module A = Middle_end.Anf
module C = Cesk.Ast
module B = Bytecode
module MC = Middle_end.Match_compile

(* A pseudo-instruction for the assembler: a final instruction, or a jump/switch
   carrying *label ids* (resolved to relative offsets by [resolve]), or a position
   marker. A spliced body's own (self-relative) jumps ride along inside [Pinstr]
   untouched. *)
type pseudo =
  | Pinstr of B.instr
  | Pjump of int
  | Pjump_unless of
      int (* pop a bool; jump to the label when false (guard fall-through) *)
  | Pswitch_ctor of (string * int) list * int
  | Pswitch_lit of (C.lit * int) list * int
  | Pswitch_len of (int * int) list * int
  | Plabel of int

(* Two passes: record each label's instruction index, then emit the final array with
   every jump/switch target turned into an offset relative to the next instruction
   (matching the VM's [ip := ip + rel] after it has stepped past the instruction). *)
let resolve (pseudos : pseudo list) : B.instr list =
  let labelpos : (int, int) Hashtbl.t = Hashtbl.create 16 in
  let pos = ref 0 in
  List.iter
    (function
      | Plabel l -> Hashtbl.replace labelpos l !pos
      | _ -> incr pos)
    pseudos;
  let p = ref 0 in
  List.filter_map
    (fun ps ->
       match ps with
       | Plabel _ -> None
       | _ ->
         let self = !p in
         incr p;
         let rel l = Hashtbl.find labelpos l - (self + 1) in
         Some
           (match ps with
            | Pinstr i -> i
            | Pjump l -> B.Jump (rel l)
            | Pjump_unless l -> B.Jump_unless (rel l)
            | Pswitch_ctor (cs, d) ->
              B.Switch_ctor (List.map (fun (t, l) -> t, rel l) cs, rel d)
            | Pswitch_lit (cs, d) ->
              B.Switch_lit (List.map (fun (x, l) -> x, rel l) cs, rel d)
            | Pswitch_len (cs, d) ->
              B.Switch_len (List.map (fun (x, l) -> x, rel l) cs, rel d)
            | Plabel _ -> assert false))
    pseudos

(* Matching strategy for unconditional `case` (ADR-0031). Default is the decision tree;
   the benchmark harness flips this to the naive explicit matcher ([compile_naive]) to
   measure the tree's win in VM-instruction terms. Both are correct (differentially
   checked); they differ only in the bytecode emitted. *)
let use_naive_matching = ref false

let proj_instr : MC.proj -> B.instr = function
  | MC.Pfield j -> B.Proj j
  | MC.Pelem j -> B.Proj_arr j
  | MC.Precord l -> B.Get_field l

(* Lower a [Middle_end.Match_compile] decision tree to bytecode. The tree already fixes
   the occurrence names (ADR-0083 byte-identity contract); this walk only turns switches
   into [Pswitch_*] with back-patched labels and threads [atom]/[body]. *)
let lower_dtree
      ~atom
      ~body
      (tail : bool)
      ((scrut_binds, dt) : (string * A.atom) list * MC.dtree)
  : B.instr list
  =
  let buf = ref [] in
  let emit p = buf := p :: !buf in
  let lbl_ctr = ref 0 in
  let fresh_lbl () =
    incr lbl_ctr;
    !lbl_ctr
  in
  let end_lbl = fresh_lbl () in
  (* Extract sub-occurrence [name] = the [proj] of occurrence [occ], balanced on the
     operand stack. *)
  let extract occ pr name =
    emit (Pinstr (B.Load occ));
    emit (Pinstr (proj_instr pr));
    emit (Pinstr (B.Bind name))
  in
  (* Emit a fully-matched right-hand side body. An unconditional body commits (the rows
     below are dead: no fall-through). *)
  let emit_body e =
    List.iter (fun i -> emit (Pinstr i)) (body tail e);
    if not tail then emit (Pjump end_lbl)
  in
  let emit_binds binds =
    List.iter
      (fun (name, occ) ->
         emit (Pinstr (B.Load occ));
         emit (Pinstr (B.Bind name)))
      binds
  in
  let rec lower : MC.dtree -> unit = function
    | MC.Dfail msg -> emit (Pinstr (B.Fail msg))
    | MC.Dleaf (binds, e) ->
      emit_binds binds;
      emit_body e
    | MC.Dguard (binds, clauses, fallthrough) ->
      emit_binds binds;
      (* Guards top to bottom (ADR-0013); the first true runs its body, all-false falls
         through to the rows below (recompiled against the same occurrences). *)
      List.iter
        (fun (guard, gbody) ->
           List.iter (fun i -> emit (Pinstr i)) (body false guard);
           let gnext = fresh_lbl () in
           emit (Pjump_unless gnext);
           emit_body gbody;
           emit (Plabel gnext))
        clauses;
      lower fallthrough
    | MC.Dswitch_ctor (occ_c, arms, default) ->
      let default_lbl = fresh_lbl () in
      let arm_lbls = List.map (fun (tag, _) -> tag, fresh_lbl ()) arms in
      emit (Pinstr (B.Load occ_c));
      emit
        (Pswitch_ctor
           (List.map2 (fun (tag, _) (_, l) -> tag, l) arms arm_lbls, default_lbl));
      List.iter2
        (fun (_, (arm : MC.arm)) (_, l) ->
           emit (Plabel l);
           List.iter (fun (o, pr) -> extract occ_c pr o) arm.MC.extracts;
           lower arm.MC.sub)
        arms
        arm_lbls;
      emit (Plabel default_lbl);
      lower default
    | MC.Dswitch_lit (occ_c, arms, default) ->
      let default_lbl = fresh_lbl () in
      let arm_lbls = List.map (fun (l, _) -> l, fresh_lbl ()) arms in
      emit (Pinstr (B.Load occ_c));
      emit
        (Pswitch_lit (List.map2 (fun (l, _) (_, lb) -> l, lb) arms arm_lbls, default_lbl));
      List.iter2
        (fun (_, sub) (_, lb) ->
           emit (Plabel lb);
           lower sub)
        arms
        arm_lbls;
      emit (Plabel default_lbl);
      lower default
    | MC.Dswitch_len (occ_c, arms, default) ->
      let default_lbl = fresh_lbl () in
      let arm_lbls = List.map (fun (len, _) -> len, fresh_lbl ()) arms in
      emit (Pinstr (B.Load occ_c));
      emit
        (Pswitch_len (List.map2 (fun (len, _) (_, l) -> len, l) arms arm_lbls, default_lbl));
      List.iter2
        (fun (_, (arm : MC.arm)) (_, l) ->
           emit (Plabel l);
           List.iter (fun (o, pr) -> extract occ_c pr o) arm.MC.extracts;
           lower arm.MC.sub)
        arms
        arm_lbls;
      emit (Plabel default_lbl);
      lower default
    | MC.Dexpand_record (occ_c, extracts, sub) ->
      List.iter (fun (o, pr) -> extract occ_c pr o) extracts;
      lower sub
  in
  List.iter
    (fun (o, a) ->
       emit (Pinstr (atom a));
       emit (Pinstr (B.Bind o)))
    scrut_binds;
  lower dt;
  emit (Plabel end_lbl);
  resolve (List.rev !buf)

(* The naive explicit matcher (ADR-0031 measurement baseline): the same bytecode
   primitives as the decision tree, but each alternative re-tests the scrutinees
   independently (no sharing across alternatives), jumping to the next alternative on
   any failure. Semantically identical (differentially checked). *)
let compile_naive ~atom ~body (tail : bool) (scruts : A.atom list) (alts : A.alt list)
  : B.instr list
  =
  let buf = ref [] in
  let emit p = buf := p :: !buf in
  let lbl_ctr = ref 0 in
  let fresh_lbl () =
    incr lbl_ctr;
    !lbl_ctr
  in
  let occ_ctr = ref 0 in
  let fresh_occ () =
    incr occ_ctr;
    "$nv" ^ string_of_int !occ_ctr
  in
  let end_lbl = fresh_lbl () in
  (* Test [binder] against occurrence [o]; on any mismatch jump [fail], else bind and
     fall through. *)
  let rec test o binder fail =
    let extract proj sub =
      let so = fresh_occ () in
      emit (Pinstr (B.Load o));
      emit (Pinstr proj);
      emit (Pinstr (B.Bind so));
      test so sub fail
    in
    match binder with
    | C.BNull -> ()
    | C.BVar x ->
      emit (Pinstr (B.Load o));
      emit (Pinstr (B.Bind x))
    | C.BNamed (x, inner) ->
      emit (Pinstr (B.Load o));
      emit (Pinstr (B.Bind x));
      test o inner fail
    | C.BLit l ->
      let cont = fresh_lbl () in
      emit (Pinstr (B.Load o));
      emit (Pswitch_lit ([ l, cont ], fail));
      emit (Plabel cont)
    | C.BCtor (tag, subs) ->
      let cont = fresh_lbl () in
      emit (Pinstr (B.Load o));
      emit (Pswitch_ctor ([ tag, cont ], fail));
      emit (Plabel cont);
      List.iteri (fun j sub -> extract (B.Proj j) sub) subs
    | C.BArray subs ->
      let cont = fresh_lbl () in
      emit (Pinstr (B.Load o));
      emit (Pswitch_len ([ List.length subs, cont ], fail));
      emit (Plabel cont);
      List.iteri (fun j sub -> extract (B.Proj_arr j) sub) subs
    | C.BRecord fields -> List.iter (fun (l, sub) -> extract (B.Get_field l) sub) fields
  in
  let occ0 =
    List.map
      (fun a ->
         let o = fresh_occ () in
         emit (Pinstr (atom a));
         emit (Pinstr (B.Bind o));
         o)
      scruts
  in
  let emit_body e =
    List.iter (fun i -> emit (Pinstr i)) (body tail e);
    if not tail then emit (Pjump end_lbl)
  in
  List.iter
    (fun (alt : A.alt) ->
       let next = fresh_lbl () in
       List.iter2 (fun o b -> test o b next) occ0 alt.A.binders;
       (match alt.A.result with
        | A.Uncond e -> emit_body e
        | A.Guarded clauses ->
          (* Guards top to bottom (ADR-0013); all false falls through to [next]. *)
          List.iter
            (fun (guard, gbody) ->
               List.iter (fun i -> emit (Pinstr i)) (body false guard);
               let gnext = fresh_lbl () in
               emit (Pjump_unless gnext);
               emit_body gbody;
               emit (Plabel gnext))
            clauses);
       emit (Plabel next))
    alts;
  emit (Pinstr (B.Fail "case: no matching alternative"));
  emit (Plabel end_lbl);
  resolve (List.rev !buf)

(** Compile a `case` to bytecode. [atom] compiles a scrutinee atom and [body] compiles
    an alternative's right-hand side (tail-aware); both are supplied by [Codegen]. The
    decision tree ([Middle_end.Match_compile], lowered by [lower_dtree]) handles
    unconditional and guarded alternatives alike (ADR-0013 guard sequencing preserved);
    [use_naive_matching] selects the per-alternative baseline instead. *)
let compile ~atom ~body ~tail (scruts : A.atom list) (alts : A.alt list) : B.instr list =
  if !use_naive_matching
  then compile_naive ~atom ~body tail scruts alts
  else lower_dtree ~atom ~body tail (MC.compile scruts alts)
