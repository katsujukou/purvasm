(** PURVASM bytecode (ADR-0003, ADR-0030 slice 1): a stack machine with an operand
    stack and an explicit frame/return stack. A [chunk] is a function body's flat
    instruction array; control flow inside a chunk is by absolute jump; calls and
    returns move between chunks via the frame stack (see [Machine]).

    Names rather than slots index the environment in this slice (ADR-0030 leaves the
    slot/closure-conversion refinement to later); the operand-stack height is still
    statically known and empty at statement boundaries, per ADR-0003. *)

(* A `case` alternative: the per-scrutinee binders to match (the naive matcher of
   ADR-0011/0012/0013, run host-side in slice 1 — decision-tree compilation is the
   deferred VM-stage pass), and the chunk index to jump to when they all match. *)
type matchalt =
  { binders : Cesk.Ast.binder list
  ; target : int
  }

type instr =
  | Push_int of int
  | Push_number of float
  | Push_bool of bool
  | Push_string of string
  (* A variable: resolved against the frame environment, then the global table. *)
  | Load of string
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
  | Update of string list
  | Prim of Cesk.Ast.primop * int
  (* Pop n args then the function; apply (eval/apply). Non-tail pushes a frame. *)
  | Call of int
  | Tail_call of int
  | Return
  | Jump of int
  | Jump_unless of int
  (* Case dispatch for a fully unconditional `case` (no guards): nscrut scrutinee
     values are on top of the operand stack; try the alternatives in order, bind on
     match, pop the scrutinees, and jump to the target. A non-match is stuck. *)
  | Match of int * matchalt array
  (* One alternative of a *guarded* `case` (ADR-0013): peek (do not pop) the nscrut
     scrutinees, match the binders; on success bind them and fall through to the
     guard/body code; on failure jump (relative) to the next alternative. The
     scrutinees stay on the stack so the next alternative can re-match. *)
  | Test of int * Cesk.Ast.binder list * int
  (* Discard the top n operands — used to drop a guarded `case`'s scrutinees once an
     alternative's body is finally chosen. *)
  | Drop of int
  (* No alternative matched (or every guard fell through): a stuck program. *)
  | Fail of string

and chunk = instr array
