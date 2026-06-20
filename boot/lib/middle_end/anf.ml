(** The lower IR (ADR-0025): A-normal form over the same patterns and primitives
    as the CESK upper IR (ADR-0002). Every argument is an *atom* and every compound
    subexpression is [let]-named, so evaluation order is explicit; functions and
    calls are *uncurried* (eval/apply: a call carries all its arguments at once) —
    the substrate the optimiser and a future stack-machine codegen need.

    This module is only the IR *form*. The bridges to the oracle — [transl]
    ([Cesk.Ast] → ANF) and [rev_transl] (ANF → [Cesk.Ast]) — live in [Transl], and
    optimiser passes are validated by the round-trip through them. *)

(** A trivially-valued operand — no evaluation step. Patterns
    ([Cesk.Ast.binder]), literals, and primitives are shared with the upper IR
    rather than re-declared. *)
type atom =
  | AVar of string
  | ALit of Cesk.Ast.lit
  (* An opaque host-foreign reference (ADR-0022), a value like any other atom. *)
  | AForeign of string

(** A computation — a single step that produces a value. Its operands are atoms;
    its sub-*expressions* ([CIf]/[CCase] branches, [CLam] body) are full [expr]s. *)
type cexpr =
  | CAtom of atom
  (* An uncurried lambda: all parameters at once (arity = [List.length params]). *)
  | CLam of string list * expr
  (* eval/apply application: the function atom applied to all arguments at once. *)
  | CApp of atom * atom list
  | CPrim of Cesk.Ast.primop * atom list
  (* A constructor (tag, arity) applied to 0..arity argument atoms. *)
  | CCtor of string * int * atom list
  | CArray of atom list
  | CRecord of (string * atom) list
  | CAccessor of atom * string
  | CUpdate of atom * (string * atom) list
  | CIf of atom * expr * expr
  | CCase of atom list * alt list

(** A let-sequence ending in a tail computation. [Let] binds a (non-recursive)
    computation; [LetRec] a recursive group (each right-hand side a full [expr],
    since its internal bindings may reference the group and cannot be hoisted). *)
and expr =
  | Ret of cexpr
  | Let of string * cexpr * expr
  | LetRec of (string * expr) list * expr

and alt =
  { binders : Cesk.Ast.binder list
  ; result : rhs
  }

and rhs =
  | Uncond of expr
  | Guarded of (expr * expr) list
