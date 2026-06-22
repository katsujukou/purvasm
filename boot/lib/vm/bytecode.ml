(** PURVASM bytecode (ADR-0003, ADR-0030 slice 1): a stack machine with an operand
    stack and an explicit frame/return stack. A [chunk] is a function body's flat
    instruction array; control flow inside a chunk is by absolute jump; calls and
    returns move between chunks via the frame stack (see [Machine]).

    Names rather than slots index the environment in this slice (ADR-0030 leaves the
    slot/closure-conversion refinement to later); the operand-stack height is still
    statically known and empty at statement boundaries, per ADR-0003. *)

type instr =
  | Push_int of int
  | Push_number of float
  | Push_bool of bool
  | Push_string of string
  (* A variable: resolved against the frame environment, then the global table. *)
  | Load of string
  (* A native foreign leaf (ADR-0022/0032): materialise a [Vforeign] by looking the
     name up in the host registry passed to the VM. Stuck if the name is not native. *)
  | Foreign_ref of string
  (* Pop the top value and bind it to a name in the frame environment (a `let`). *)
  | Bind of string
  (* params, body — capture the current frame environment into a closure value. *)
  | Closure of string list * chunk
  (* A local recursive group (name, value-computing chunk). Members are evaluated
     sharing one environment ref that is backpatched with the group once built
     (knot-tying, ADR-0030): each member's closures capture that ref, so a member's
     self/mutual references — always under a lambda for an eagerly-constructible
     cycle — resolve after construction. Subsumes the all-lambda case. *)
  | Make_rec of (string * chunk) list
  (* tag, arity, nargs — pop nargs and build saturated `Vdata` or partial `Vctor`. *)
  | Ctor of string * int * int
  | Record of string list
  | Array of int
  | Get_field of string
  (* Pop a `Vdata` / `Varray` and push its i-th field / element. Used by a decision
     tree to extract a known-present sub-occurrence (ADR-0031); the index is in range
     by construction (the tag/length switch above it established the shape). *)
  | Proj of int
  | Proj_arr of int
  | Update of string list
  | Prim of Cesk.Ast.primop * int
  (* Pop n args then the function; apply (eval/apply). Non-tail pushes a frame. *)
  | Call of int
  | Tail_call of int
  | Return
  | Jump of int
  | Jump_unless of int
  (* Decision-tree dispatch (ADR-0031). Each pops the inspected occurrence value and
     branches by its discriminant to a matching case (relative offset) or, failing
     that, the default (relative). A value of the wrong *kind* (e.g. a non-data value
     under [Switch_ctor]) is stuck (type-impossible, as in the oracle's matcher); a
     well-typed discriminant that no case names takes the default edge (a value-level
     non-match). [Switch_lit] branches on a scalar literal, [Switch_len] on a
     `Varray`'s length (ADR-0012: a different length is a value-level non-match). *)
  | Switch_ctor of (string * int) list * int
  | Switch_lit of (Cesk.Ast.lit * int) list * int
  | Switch_len of (int * int) list * int
  (* No alternative matched (or every guard fell through): a stuck program. *)
  | Fail of string

and chunk = instr array
