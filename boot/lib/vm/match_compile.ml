(** Pattern-match compilation (ADR-0031): lower a `case` to PURVASM bytecode. This is
    the one closed responsibility of turning ANF alternatives into explicit control
    flow — kept out of the surrounding ANF→bytecode walk ([Codegen]).

    A `case` compiles to a Maranget decision tree of discriminant switches
    ([compile_tree]), handling unconditional and guarded alternatives alike: a guard
    chain (ADR-0013) is evaluated at the matched leaf, and if every guard is false
    control falls through to the rows below — recompiled against the same occurrences,
    which keeps a guarded row's successors reachable. [compile_naive] is the
    per-alternative re-testing baseline the tree is measured against;
    [use_naive_matching] selects it.

    The compilers are parameterised by [atom] (compile a scrutinee atom) and [body]
    (compile an alternative's right-hand side, tail-aware), supplied by [Codegen], so
    this module does not depend on the rest of the lowering. Occurrences — paths into
    the scrutinees — are bound to fresh locals, so the operand stack is empty at every
    branch boundary (ADR-0003). *)

module A = Middle_end.Anf
module C = Cesk.Ast
module B = Bytecode

(* A binder's "head", with the variable names it binds at this occurrence peeled off
   (`BVar`/`BNamed`). [Cwild] imposes no test (matches anything); the others impose a
   discriminant test on the occurrence. *)
type core =
  | Cwild
  | Clit of C.lit
  | Cctor of string * C.binder list
  | Carr of C.binder list
  | Crec of (string * C.binder) list

let rec peel : C.binder -> string list * core = function
  | C.BNull -> [], Cwild
  | C.BVar x -> [ x ], Cwild
  | C.BNamed (x, inner) ->
    let ns, c = peel inner in
    x :: ns, c
  | C.BLit l -> [], Clit l
  | C.BCtor (tag, subs) -> [], Cctor (tag, subs)
  | C.BArray subs -> [], Carr subs
  | C.BRecord fields -> [], Crec fields

(* One clause of the pattern matrix: the binders still to test (aligned to the
   current occurrences), the variable→occurrence bindings collected so far on the way
   down, and the right-hand side to run when this row is selected (unconditional, or a
   guard chain — ADR-0013). *)
type dt_row =
  { pats : C.binder list
  ; binds : (string * string) list
  ; rhs : A.rhs
  }

(* Splice [news] in for the element at index [c]; remove the element at index [c]. *)
let replace_col xs c news =
  List.concat (List.mapi (fun i x -> if i = c then news else [ x ]) xs)

let remove_col xs c = List.filteri (fun i _ -> i <> c) xs

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

(* Matching strategy for unconditional `case` (ADR-0031). Default is the decision
   tree; the benchmark harness flips this to the naive explicit matcher
   ([compile_naive]) to measure the tree's win in VM-instruction terms. Both are
   correct (differentially checked); they differ only in the bytecode emitted. *)
let use_naive_matching = ref false

(* An unconditional `case` (ADR-0031) as a Maranget decision tree of explicit
   bytecode tests, assembled via a label-backpatching pass. *)
let compile_tree ~atom ~body (tail : bool) (scruts : A.atom list) (alts : A.alt list)
  : B.instr list
  =
  let buf =
    ref []
    (* pseudo-instructions, reversed *)
  in
  let emit p = buf := p :: !buf in
  let lbl_ctr = ref 0 in
  let fresh_lbl () =
    incr lbl_ctr;
    !lbl_ctr
  in
  let occ_ctr = ref 0 in
  let fresh_occ () =
    incr occ_ctr;
    "$dt" ^ string_of_int !occ_ctr
  in
  let end_lbl = fresh_lbl () in
  (* Extract sub-occurrence [name] = the [proj] of occurrence [occ] (a constructor
     field / array element / record field), all balanced on the operand stack. *)
  let extract occ proj name =
    emit (Pinstr (B.Load occ));
    emit (Pinstr proj);
    emit (Pinstr (B.Bind name))
  in
  let binds_of names occ = List.map (fun n -> n, occ) names in
  let first_refutable cores =
    let rec go i = function
      | [] -> assert false (* a non-leaf row has a refutable column *)
      | c :: cs -> if c = Cwild then go (i + 1) cs else i
    in
    go 0 cores
  in
  (* Emit a fully-matched right-hand side. An unconditional body commits (the rows
     below are dead). A guard chain (ADR-0013) is evaluated top to bottom: the first
     true guard runs its body; if every guard is false, control falls through to
     [on_fail] — which, in the tree, recompiles the rows below this one against the
     same occurrences (keeping a guarded row's successors reachable). *)
  let emit_body e =
    List.iter (fun i -> emit (Pinstr i)) (body tail e);
    if not tail then emit (Pjump end_lbl)
  in
  let emit_rhs rhs ~on_fail =
    match rhs with
    | A.Uncond e -> emit_body e
    | A.Guarded clauses ->
      List.iter
        (fun (guard, gbody) ->
           List.iter (fun i -> emit (Pinstr i)) (body false guard);
           let gnext = fresh_lbl () in
           emit (Pjump_unless gnext);
           emit_body gbody;
           emit (Plabel gnext))
        clauses;
      on_fail ()
  in
  let rec compile occs rows =
    match rows with
    | [] -> emit (Pinstr (B.Fail "case: no matching alternative"))
    | row0 :: rest ->
      let cores0 = List.map (fun p -> snd (peel p)) row0.pats in
      if List.for_all (fun c -> c = Cwild) cores0
      then (
        (* Leaf: row0's binders all match; bind, then run its right-hand side. *)
        let leaf_binds =
          row0.binds
          @ List.concat
              (List.map2 (fun occ p -> binds_of (fst (peel p)) occ) occs row0.pats)
        in
        List.iter
          (fun (name, occ) ->
             emit (Pinstr (B.Load occ));
             emit (Pinstr (B.Bind name)))
          leaf_binds;
        emit_rhs row0.rhs ~on_fail:(fun () -> compile occs rest))
      else (
        let c = first_refutable cores0 in
        let occ_c = List.nth occs c in
        match List.nth cores0 c with
        | Cwild -> assert false
        | Cctor _ -> compile_ctor occs rows c occ_c
        | Clit _ -> compile_lit occs rows c occ_c
        | Carr _ -> compile_arr occs rows c occ_c
        | Crec _ -> compile_rec occs rows c occ_c)
  (* Switch on a constructor tag. Each head specialises the matrix with the
     constructor's fields as new columns; the default keeps the wildcard rows. *)
  and compile_ctor occs rows c occ_c =
    let heads =
      List.fold_left
        (fun acc row ->
           match snd (peel (List.nth row.pats c)) with
           | Cctor (tag, subs) when not (List.mem_assoc tag acc) ->
             acc @ [ tag, List.length subs ]
           | _ -> acc)
        []
        rows
    in
    let default_rows =
      List.filter_map
        (fun row ->
           let names, core = peel (List.nth row.pats c) in
           match core with
           | Cwild ->
             Some
               { row with
                 pats = remove_col row.pats c
               ; binds = row.binds @ binds_of names occ_c
               }
           | _ -> None)
        rows
    in
    let default_lbl = fresh_lbl () in
    let head_lbls = List.map (fun (tag, _) -> tag, fresh_lbl ()) heads in
    emit (Pinstr (B.Load occ_c));
    emit (Pswitch_ctor (head_lbls, default_lbl));
    List.iter
      (fun (tag, arity) ->
         emit (Plabel (List.assoc tag head_lbls));
         let sub_occs = List.init arity (fun _ -> fresh_occ ()) in
         List.iteri (fun j o -> extract occ_c (B.Proj j) o) sub_occs;
         let rows' =
           List.filter_map
             (fun row ->
                let names, core = peel (List.nth row.pats c) in
                let subpats =
                  match core with
                  | Cctor (t, subs) when String.equal t tag -> Some subs
                  | Cwild -> Some (List.init arity (fun _ -> C.BNull))
                  | _ -> None
                in
                Option.map
                  (fun subs ->
                     { pats = replace_col row.pats c subs
                     ; binds = row.binds @ binds_of names occ_c
                     ; rhs = row.rhs
                     })
                  subpats)
             rows
         in
         compile (replace_col occs c sub_occs) rows')
      heads;
    emit (Plabel default_lbl);
    compile (remove_col occs c) default_rows
  (* Switch on a scalar literal; literal heads have no sub-columns. *)
  and compile_lit occs rows c occ_c =
    let heads =
      List.fold_left
        (fun acc row ->
           match snd (peel (List.nth row.pats c)) with
           | Clit l when not (List.mem l acc) -> acc @ [ l ]
           | _ -> acc)
        []
        rows
    in
    let occs' = remove_col occs c in
    let select_rows keep =
      List.filter_map
        (fun row ->
           let names, core = peel (List.nth row.pats c) in
           if keep core
           then
             Some
               { row with
                 pats = remove_col row.pats c
               ; binds = row.binds @ binds_of names occ_c
               }
           else None)
        rows
    in
    let default_lbl = fresh_lbl () in
    let head_lbls = List.map (fun l -> l, fresh_lbl ()) heads in
    emit (Pinstr (B.Load occ_c));
    emit (Pswitch_lit (head_lbls, default_lbl));
    List.iter
      (fun l ->
         emit (Plabel (List.assoc l head_lbls));
         compile occs' (select_rows (fun core -> core = Clit l || core = Cwild)))
      heads;
    emit (Plabel default_lbl);
    compile occs' (select_rows (fun core -> core = Cwild))
  (* Switch on a `Varray`'s length (ADR-0012); each length head exposes its elements
     as new columns. *)
  and compile_arr occs rows c occ_c =
    let heads =
      List.fold_left
        (fun acc row ->
           match snd (peel (List.nth row.pats c)) with
           | Carr subs when not (List.mem (List.length subs) acc) ->
             acc @ [ List.length subs ]
           | _ -> acc)
        []
        rows
    in
    let default_rows =
      List.filter_map
        (fun row ->
           let names, core = peel (List.nth row.pats c) in
           match core with
           | Cwild ->
             Some
               { row with
                 pats = remove_col row.pats c
               ; binds = row.binds @ binds_of names occ_c
               }
           | _ -> None)
        rows
    in
    let default_lbl = fresh_lbl () in
    let head_lbls = List.map (fun len -> len, fresh_lbl ()) heads in
    emit (Pinstr (B.Load occ_c));
    emit (Pswitch_len (head_lbls, default_lbl));
    List.iter
      (fun arity ->
         emit (Plabel (List.assoc arity head_lbls));
         let sub_occs = List.init arity (fun _ -> fresh_occ ()) in
         List.iteri (fun j o -> extract occ_c (B.Proj_arr j) o) sub_occs;
         let rows' =
           List.filter_map
             (fun row ->
                let names, core = peel (List.nth row.pats c) in
                let subpats =
                  match core with
                  | Carr subs when List.length subs = arity -> Some subs
                  | Cwild -> Some (List.init arity (fun _ -> C.BNull))
                  | _ -> None
                in
                Option.map
                  (fun subs ->
                     { pats = replace_col row.pats c subs
                     ; binds = row.binds @ binds_of names occ_c
                     ; rhs = row.rhs
                     })
                  subpats)
             rows
         in
         compile (replace_col occs c sub_occs) rows')
      heads;
    emit (Plabel default_lbl);
    compile (remove_col occs c) default_rows
  (* A record pattern imposes no discriminant (a missing label is type-impossible,
     ADR-0012); expand the union of named labels as new columns and continue — no
     switch. *)
  and compile_rec occs rows c occ_c =
    let labels =
      List.fold_left
        (fun acc row ->
           match snd (peel (List.nth row.pats c)) with
           | Crec fields ->
             List.fold_left
               (fun acc (l, _) -> if List.mem l acc then acc else acc @ [ l ])
               acc
               fields
           | _ -> acc)
        []
        rows
    in
    let sub_occs = List.map (fun _ -> fresh_occ ()) labels in
    List.iter2 (fun l o -> extract occ_c (B.Get_field l) o) labels sub_occs;
    let rows' =
      List.map
        (fun row ->
           let names, core = peel (List.nth row.pats c) in
           let subpats =
             match core with
             | Crec fields ->
               List.map
                 (fun l ->
                    match List.assoc_opt l fields with
                    | Some b -> b
                    | None -> C.BNull)
                 labels
             | Cwild -> List.map (fun _ -> C.BNull) labels
             | _ -> assert false (* a record column holds only records / wildcards *)
           in
           { pats = replace_col row.pats c subpats
           ; binds = row.binds @ binds_of names occ_c
           ; rhs = row.rhs
           })
        rows
    in
    compile (replace_col occs c sub_occs) rows'
  in
  (* Bind each scrutinee to an occurrence local, then compile the matrix. *)
  let occ0 =
    List.map
      (fun a ->
         let o = fresh_occ () in
         emit (Pinstr (atom a));
         emit (Pinstr (B.Bind o));
         o)
      scruts
  in
  let rows0 =
    List.map
      (fun (alt : A.alt) -> { pats = alt.A.binders; binds = []; rhs = alt.A.result })
      alts
  in
  compile occ0 rows0;
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

(** Compile a `case` to bytecode. [atom] compiles a scrutinee atom and [body]
    compiles an alternative's right-hand side (tail-aware); both are supplied by
    [Codegen]. The decision tree handles unconditional and guarded alternatives alike
    (ADR-0013 guard sequencing preserved); [use_naive_matching] selects the
    per-alternative baseline instead. *)
let compile ~atom ~body ~tail (scruts : A.atom list) (alts : A.alt list) : B.instr list =
  (if !use_naive_matching then compile_naive else compile_tree)
    ~atom
    ~body
    tail
    scruts
    alts
