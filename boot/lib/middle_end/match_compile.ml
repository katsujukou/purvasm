(** Pattern-match compilation (ADR-0031, ADR-0083): turn a `case`'s scrutinees and
    alternatives into an explicit Maranget decision tree ([dtree]) over the scrutinee
    occurrences. This is the ONE shared, backend-agnostic matcher (ADR-0083) — every
    backend lowers the same tree rather than embedding its own matcher: the bytecode
    ([Vm.Codegen] via [Match_compile] lowering), OCaml-source ([Codegen_ml]) and LLVM
    ([Codegen_llvm]) backends all consume [compile]'s output.

    A guard chain (ADR-0013) is evaluated at the matched leaf ([Dguard]); if every
    guard is false, control falls through to the rows below — recompiled against the
    same occurrences (the [Dguard] fall-through subtree), which keeps a guarded row's
    successors reachable. Occurrences — paths into the scrutinees — are fresh local
    names ($dt<n>); each backend binds them (the scrutinee atoms at the root, a
    projection at each extraction) as it lowers the tree. The fresh-occurrence order is
    the byte-identity contract for the bytecode backend (ADR-0083): it is fixed here,
    once, and shared. *)

module A = Anf
module C = Cesk.Ast

(** A projection from a parent occurrence to a sub-occurrence: a constructor field, an
    array element, or a record label. *)
type proj =
  | Pfield of int
  | Pelem of int
  | Precord of string

(** An explicit decision tree over the scrutinee occurrences. A backend lowers each
    node to its own discriminant (a bytecode `switch`, an LLVM `switch`/equality chain,
    an OCaml `match`); the tree itself is discriminant-agnostic. Leaf right-hand sides
    carry raw ANF [expr]s ([Dleaf]/[Dguard]), lowered by the backend's own expression
    compiler. *)
type dtree =
  (* No alternative matched (or every guard fell through): a stuck program. *)
  | Dfail of string
  (* A fully-matched row: bind its variables ([(var, occ)]), then run the body. *)
  | Dleaf of (string * string) list * A.expr
  (* A fully-matched guarded row (ADR-0013): bind, then try each guard/body clause top
     to bottom; if every guard is false, fall through to the subtree (the rows below,
     recompiled against the same occurrences). *)
  | Dguard of (string * string) list * (A.expr * A.expr) list * dtree
  (* Switch on a constructor tag at the occurrence; each arm extracts the constructor's
     fields as sub-occurrences before its subtree, and the default keeps the wildcard
     rows. *)
  | Dswitch_ctor of string * (string * arm) list * dtree
  (* Switch on a scalar literal (Int/Bool/Number/String alike — ADR-0083); literal
     heads have no sub-columns. *)
  | Dswitch_lit of string * (C.lit * dtree) list * dtree
  (* Switch on a `Varray`'s length (ADR-0012); each arm extracts the elements. *)
  | Dswitch_len of string * (int * arm) list * dtree
  (* A record pattern imposes no discriminant (a missing label is type-impossible,
     ADR-0012): extract the union of named labels as sub-occurrences and continue — no
     switch. *)
  | Dexpand_record of string * (string * proj) list * dtree

(** A switch arm: the sub-occurrence extractions [(sub_occ, proj-of-parent)] to perform
    on entry, then the subtree. *)
and arm =
  { extracts : (string * proj) list
  ; sub : dtree
  }

(* A binder's "head", with the variable names it binds at this occurrence peeled off
   (`BVar`/`BNamed`). [Cwild] imposes no test; the others impose a discriminant test. *)
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

(* One clause of the pattern matrix: the binders still to test (aligned to the current
   occurrences), the variable→occurrence bindings collected so far on the way down, and
   the right-hand side to run when this row is selected. *)
type dt_row =
  { pats : C.binder list
  ; binds : (string * string) list
  ; rhs : A.rhs
  }

(* Splice [news] in for the element at index [c]; remove the element at index [c]. *)
let replace_col xs c news =
  List.concat (List.mapi (fun i x -> if i = c then news else [ x ]) xs)

let remove_col xs c = List.filteri (fun i _ -> i <> c) xs
let binds_of names occ = List.map (fun n -> n, occ) names

let first_refutable cores =
  let rec go i = function
    | [] -> assert false (* a non-leaf row has a refutable column *)
    | c :: cs -> if c = Cwild then go (i + 1) cs else i
  in
  go 0 cores

(** Compile a `case`'s scrutinees and alternatives to a decision tree. Returns the
    scrutinee-occurrence bindings [(occ, atom)] to establish at the root and the tree
    itself. The fresh-occurrence generation order (scrutinees first, then per-head
    depth-first) is fixed here and must be preserved by every port for bytecode
    byte-identity (ADR-0083). *)
let compile (scruts : A.atom list) (alts : A.alt list) : (string * A.atom) list * dtree =
  let occ_ctr = ref 0 in
  let fresh_occ () =
    incr occ_ctr;
    "$dt" ^ string_of_int !occ_ctr
  in
  let rec go occs rows : dtree =
    match rows with
    | [] -> Dfail "case: no matching alternative"
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
        match row0.rhs with
        | A.Uncond e -> Dleaf (leaf_binds, e)
        | A.Guarded clauses -> Dguard (leaf_binds, clauses, go occs rest))
      else (
        let c = first_refutable cores0 in
        let occ_c = List.nth occs c in
        match List.nth cores0 c with
        | Cwild -> assert false
        | Cctor _ -> go_ctor occs rows c occ_c
        | Clit _ -> go_lit occs rows c occ_c
        | Carr _ -> go_arr occs rows c occ_c
        | Crec _ -> go_rec occs rows c occ_c)
  (* Switch on a constructor tag. Each head specialises the matrix with the
     constructor's fields as new columns; the default keeps the wildcard rows. *)
  and go_ctor occs rows c occ_c =
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
    let arms =
      List.map
        (fun (tag, arity) ->
           let sub_occs = List.init arity (fun _ -> fresh_occ ()) in
           let extracts = List.mapi (fun j o -> o, Pfield j) sub_occs in
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
           let sub = go (replace_col occs c sub_occs) rows' in
           tag, { extracts; sub })
        heads
    in
    let default = go (remove_col occs c) default_rows in
    Dswitch_ctor (occ_c, arms, default)
  (* Switch on a scalar literal; literal heads have no sub-columns. *)
  and go_lit occs rows c occ_c =
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
    let arms =
      List.map
        (fun l ->
           let sub = go occs' (select_rows (fun core -> core = Clit l || core = Cwild)) in
           l, sub)
        heads
    in
    let default = go occs' (select_rows (fun core -> core = Cwild)) in
    Dswitch_lit (occ_c, arms, default)
  (* Switch on a `Varray`'s length (ADR-0012); each length head exposes its elements as
     new columns. *)
  and go_arr occs rows c occ_c =
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
    let arms =
      List.map
        (fun arity ->
           let sub_occs = List.init arity (fun _ -> fresh_occ ()) in
           let extracts = List.mapi (fun j o -> o, Pelem j) sub_occs in
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
           let sub = go (replace_col occs c sub_occs) rows' in
           arity, { extracts; sub })
        heads
    in
    let default = go (remove_col occs c) default_rows in
    Dswitch_len (occ_c, arms, default)
  (* A record pattern imposes no discriminant; expand the union of named labels as new
     columns and continue — no switch. *)
  and go_rec occs rows c occ_c =
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
    let extracts = List.map2 (fun l o -> o, Precord l) labels sub_occs in
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
    let sub = go (replace_col occs c sub_occs) rows' in
    Dexpand_record (occ_c, extracts, sub)
  in
  (* Bind each scrutinee to an occurrence local, then compile the matrix. *)
  let occ0 = List.map (fun a -> fresh_occ (), a) scruts in
  let rows0 =
    List.map
      (fun (alt : A.alt) -> { pats = alt.A.binders; binds = []; rhs = alt.A.result })
      alts
  in
  let dt = go (List.map fst occ0) rows0 in
  occ0, dt
