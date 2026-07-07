(** PURVASM image artifacts (ADR-0033): the serialized, runnable form of a linked
    program — global definitions ([Vm.Codegen.gdef], bytecode) plus a [main] chunk —
    with an explicit, versioned JSON encoding (not `Marshal`). This slice serializes a
    whole linked program ([of_term]); per-module `.pvmo`/`.pvmi` artifacts reuse the
    same `gdef`/bytecode encoding.

    Bytecode carries no binders (the decision-tree compiler, ADR-0031, lowered them to
    switches), so the encoding covers only instructions, primops, literals, and the
    [gdef] kinds. *)

module B = Vm.Bytecode
module C = Cesk.Ast
module G = Vm.Codegen
module J = Yojson.Safe

(* Artifact-compatibility version, stamped into every `.pvm`/`.pvmo`/`.pvmi` and
   checked on load. Bump it on ANY change to the JSON encoding *or* the emitted
   bytecode (codegen), so a toolchain update invalidates stale artifacts — the
   incremental builder reuses an object only if it still loads at this version. *)
let format_version = 3

exception Bad_image of string

let err msg = raise (Bad_image msg)

(* --- primitive operations (bijective tag table) --------------------------- *)

let prim_tags : (C.primop * string) list =
  [ C.AddInt, "AddInt"
  ; C.SubInt, "SubInt"
  ; C.MulInt, "MulInt"
  ; C.DivInt, "DivInt"
  ; C.ModInt, "ModInt"
  ; C.AndInt, "AndInt"
  ; C.OrInt, "OrInt"
  ; C.XorInt, "XorInt"
  ; C.ShlInt, "ShlInt"
  ; C.ShrInt, "ShrInt"
  ; C.ZshrInt, "ZshrInt"
  ; C.ComplementInt, "ComplementInt"
  ; C.AddNumber, "AddNumber"
  ; C.SubNumber, "SubNumber"
  ; C.MulNumber, "MulNumber"
  ; C.DivNumber, "DivNumber"
  ; C.IntToNumber, "IntToNumber"
  ; C.NumberToInt, "NumberToInt"
  ; C.EqInt, "EqInt"
  ; C.EqString, "EqString"
  ; C.EqNumber, "EqNumber"
  ; C.EqBool, "EqBool"
  ; C.LtInt, "LtInt"
  ; C.LtString, "LtString"
  ; C.LtNumber, "LtNumber"
  ; C.AndBool, "AndBool"
  ; C.OrBool, "OrBool"
  ; C.NotBool, "NotBool"
  ; C.Append, "Append"
  ; C.IndexArray, "IndexArray"
  ; C.LengthArray, "LengthArray"
  ; C.NewArray, "NewArray"
  ; C.SetArray, "SetArray"
  ; C.RecordGet, "RecordGet"
  ; C.RecordSet, "RecordSet"
  ; C.RecordHas, "RecordHas"
  ; C.RecordDelete, "RecordDelete"
  ; C.RecordUnion, "RecordUnion"
  ]

let prim_to_json (op : C.primop) : J.t = `String (List.assoc op prim_tags)

let prim_of_json : J.t -> C.primop = function
  | `String s ->
    (match List.find_opt (fun (_, t) -> String.equal t s) prim_tags with
     | Some (op, _) -> op
     | None -> err ("unknown primop: " ^ s))
  | _ -> err "primop: expected string"

(* --- floats: store the exact IEEE 754 bits (as a string, since the 64-bit pattern
   does not fit yojson's OCaml-int), so a Number round-trips bit-for-bit through the
   text format; tolerate the old `Float`/`Int` forms on read. *)
let float_to_json (f : float) : J.t = `String (Int64.to_string (Int64.bits_of_float f))

let float_of_json : J.t -> float = function
  | `String s -> Int64.float_of_bits (Int64.of_string s)
  | `Float f -> f
  | `Int n -> float_of_int n
  | _ -> err "float: expected IEEE-bits string"

(* --- literals ------------------------------------------------------------- *)

let lit_to_json : C.lit -> J.t = function
  | C.LInt n -> `List [ `String "i"; `Int n ]
  | C.LNumber f -> `List [ `String "n"; float_to_json f ]
  | C.LBool b -> `List [ `String "b"; `Bool b ]
  | C.LString s -> `List [ `String "s"; `String s ]

let lit_of_json : J.t -> C.lit = function
  | `List [ `String "i"; `Int n ] -> C.LInt n
  | `List [ `String "n"; f ] -> C.LNumber (float_of_json f)
  | `List [ `String "b"; `Bool b ] -> C.LBool b
  | `List [ `String "s"; `String s ] -> C.LString s
  | _ -> err "malformed literal"

(* --- small helpers -------------------------------------------------------- *)

let strs ss = `List (List.map (fun s -> `String s) ss)

let strs_of = function
  | `List xs ->
    List.map
      (function
        | `String s -> s
        | _ -> err "expected string")
      xs
  | _ -> err "expected string list"

let int_of = function
  | `Int n -> n
  | _ -> err "expected int"

let str_of = function
  | `String s -> s
  | _ -> err "expected string"

(* --- instructions --------------------------------------------------------- *)

let rec instr_to_json (i : B.instr) : J.t =
  let t tag rest = `List (`String tag :: rest) in
  match i with
  | B.Push_int n -> t "pi" [ `Int n ]
  | B.Push_number f -> t "pn" [ float_to_json f ]
  | B.Push_bool b -> t "pb" [ `Bool b ]
  | B.Push_string s -> t "ps" [ `String s ]
  | B.Load s -> t "ld" [ `String s ]
  | B.Foreign_ref s -> t "fr" [ `String s ]
  | B.Bind s -> t "bd" [ `String s ]
  | B.Closure (ps, body) -> t "cl" [ strs ps; chunk_to_json body ]
  | B.Make_rec ms ->
    t "mr" [ `List (List.map (fun (n, c) -> `List [ `String n; chunk_to_json c ]) ms) ]
  | B.Ctor (tag, arity, n) -> t "ct" [ `String tag; `Int arity; `Int n ]
  | B.Record ls -> t "rc" [ strs ls ]
  | B.Array n -> t "arr" [ `Int n ]
  | B.Get_field l -> t "gf" [ `String l ]
  | B.Proj i -> t "pj" [ `Int i ]
  | B.Proj_arr i -> t "pa" [ `Int i ]
  | B.Update ls -> t "up" [ strs ls ]
  | B.Prim (op, n) -> t "pm" [ prim_to_json op; `Int n ]
  | B.Call n -> t "ca" [ `Int n ]
  | B.Tail_call n -> t "tc" [ `Int n ]
  | B.Return -> t "rt" []
  | B.Jump r -> t "jp" [ `Int r ]
  | B.Jump_unless r -> t "ju" [ `Int r ]
  | B.Switch_ctor (cs, d) -> t "sc" [ ctor_cases cs; `Int d ]
  | B.Switch_lit (cs, d) -> t "sl" [ lit_cases cs; `Int d ]
  | B.Switch_len (cs, d) -> t "sn" [ int_cases cs; `Int d ]
  | B.Fail m -> t "fl" [ `String m ]

and ctor_cases cs = `List (List.map (fun (tag, r) -> `List [ `String tag; `Int r ]) cs)
and int_cases cs = `List (List.map (fun (k, r) -> `List [ `Int k; `Int r ]) cs)
and lit_cases cs = `List (List.map (fun (l, r) -> `List [ lit_to_json l; `Int r ]) cs)
and chunk_to_json (c : B.chunk) : J.t = `List (Array.to_list (Array.map instr_to_json c))

let rec instr_of_json : J.t -> B.instr = function
  | `List (`String tag :: rest) ->
    (match tag, rest with
     | "pi", [ n ] -> B.Push_int (int_of n)
     | "pn", [ f ] -> B.Push_number (float_of_json f)
     | "pb", [ `Bool b ] -> B.Push_bool b
     | "ps", [ s ] -> B.Push_string (str_of s)
     | "ld", [ s ] -> B.Load (str_of s)
     | "fr", [ s ] -> B.Foreign_ref (str_of s)
     | "bd", [ s ] -> B.Bind (str_of s)
     | "cl", [ ps; body ] -> B.Closure (strs_of ps, chunk_of_json body)
     | "mr", [ `List ms ] ->
       B.Make_rec
         (List.map
            (function
              | `List [ `String n; c ] -> n, chunk_of_json c
              | _ -> err "malformed Make_rec member")
            ms)
     | "ct", [ tag; arity; n ] -> B.Ctor (str_of tag, int_of arity, int_of n)
     | "rc", [ ls ] -> B.Record (strs_of ls)
     | "arr", [ n ] -> B.Array (int_of n)
     | "gf", [ l ] -> B.Get_field (str_of l)
     | "pj", [ i ] -> B.Proj (int_of i)
     | "pa", [ i ] -> B.Proj_arr (int_of i)
     | "up", [ ls ] -> B.Update (strs_of ls)
     | "pm", [ op; n ] -> B.Prim (prim_of_json op, int_of n)
     | "ca", [ n ] -> B.Call (int_of n)
     | "tc", [ n ] -> B.Tail_call (int_of n)
     | "rt", [] -> B.Return
     | "jp", [ r ] -> B.Jump (int_of r)
     | "ju", [ r ] -> B.Jump_unless (int_of r)
     | "sc", [ cs; d ] -> B.Switch_ctor (ctor_cases_of cs, int_of d)
     | "sl", [ cs; d ] -> B.Switch_lit (lit_cases_of cs, int_of d)
     | "sn", [ cs; d ] -> B.Switch_len (int_cases_of cs, int_of d)
     | "fl", [ m ] -> B.Fail (str_of m)
     | _ -> err ("malformed instruction: " ^ tag))
  | _ -> err "instruction: expected tagged array"

and ctor_cases_of = function
  | `List xs ->
    List.map
      (function
        | `List [ `String tag; r ] -> tag, int_of r
        | _ -> err "bad ctor case")
      xs
  | _ -> err "expected ctor cases"

and int_cases_of = function
  | `List xs ->
    List.map
      (function
        | `List [ k; r ] -> int_of k, int_of r
        | _ -> err "bad case")
      xs
  | _ -> err "expected int cases"

and lit_cases_of = function
  | `List xs ->
    List.map
      (function
        | `List [ l; r ] -> lit_of_json l, int_of r
        | _ -> err "bad lit case")
      xs
  | _ -> err "expected lit cases"

and chunk_of_json : J.t -> B.chunk = function
  | `List xs -> Array.of_list (List.map instr_of_json xs)
  | _ -> err "chunk: expected array"

(* --- global definitions --------------------------------------------------- *)

let gdef_to_json : G.gdef -> J.t = function
  | G.Gfun (ps, c) -> `List [ `String "fn"; strs ps; chunk_to_json c ]
  | G.Gcaf c -> `List [ `String "caf"; chunk_to_json c ]
  | G.Grec c -> `List [ `String "rec"; chunk_to_json c ]

let gdef_of_json : J.t -> G.gdef = function
  | `List [ `String "fn"; ps; c ] -> G.Gfun (strs_of ps, chunk_of_json c)
  | `List [ `String "caf"; c ] -> G.Gcaf (chunk_of_json c)
  | `List [ `String "rec"; c ] -> G.Grec (chunk_of_json c)
  | _ -> err "malformed gdef"

(* --- the image ------------------------------------------------------------ *)

(** A linked, runnable program: its global definitions (in dependency order), the
    [main] chunk that runs the entry, and whether the entry is an `Effect` (so the
    runner performs it and suppresses the `Unit` result rather than printing it). *)
type t =
  { gdefs : (string * G.gdef) list
  ; main : B.chunk
  ; is_effect : bool (* entry is an Effect: perform it, suppress the Unit result *)
  }

let to_json (img : t) : J.t =
  `Assoc
    [ "version", `Int format_version
    ; ( "gdefs"
      , `List (List.map (fun (n, g) -> `List [ `String n; gdef_to_json g ]) img.gdefs) )
    ; "main", chunk_to_json img.main
    ; "effect", `Bool img.is_effect
    ]

let of_json : J.t -> t = function
  | `Assoc fields ->
    let get k =
      match List.assoc_opt k fields with
      | Some v -> v
      | None -> err ("missing field: " ^ k)
    in
    (match get "version" with
     | `Int v when v = format_version -> ()
     | `Int v ->
       err (Printf.sprintf "image format version %d, expected %d" v format_version)
     | _ -> err "version: expected int");
    let gdefs =
      match get "gdefs" with
      | `List xs ->
        List.map
          (function
            | `List [ `String n; g ] -> n, gdef_of_json g
            | _ -> err "malformed gdef entry")
          xs
      | _ -> err "gdefs: expected array"
    in
    let is_effect =
      match List.assoc_opt "effect" fields with
      | Some (`Bool b) -> b
      | _ -> false
    in
    { gdefs; main = chunk_of_json (get "main"); is_effect }
  | _ -> err "image: expected object"

(* --- text/file round-trip ------------------------------------------------- *)

let to_string (img : t) : string = J.to_string (to_json img)
let of_string (s : string) : t = of_json (J.from_string s)

(* Binary mode: the payload is JSON text but we want byte-exact I/O (no newline
   translation) across platforms. *)
let write_file (path : string) (img : t) : unit =
  let oc = open_out_bin path in
  output_string oc (to_string img);
  close_out oc

let read_file (path : string) : t =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  of_string s

(* --- build / run ---------------------------------------------------------- *)

(** Compile a linked [Cesk.Ast.term] (e.g. from [Link.link_program]) into an image:
    lower to ANF (ADR-0025) and split the spine into global definitions + [main]
    (ADR-0030). *)
let of_term ?(is_effect = false) (t : C.term) : t =
  let gdefs, main = G.program (Middle_end.Transl.transl t) in
  { gdefs; main; is_effect }

(** Run an image to a value, resolving native foreigns through [host]. *)
let run ?(host = Cesk.Machine.no_host) (img : t) : Vm.Value.t =
  Vm.run_image ~host img.gdefs img.main

(** Run an image and also return the deterministic instruction count (the [--count]
    runner behind the ADR-0075 VM optimiser-measurement leg). *)
let run_counted ?(host = Cesk.Machine.no_host) (img : t) : Vm.Value.t * int =
  Vm.run_image_counted ~host img.gdefs img.main
