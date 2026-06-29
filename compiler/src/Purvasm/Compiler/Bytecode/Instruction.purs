module Purvasm.Compiler.Bytecode.Instruction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.Primitive (PrimOp)

data Instruction
  = PushInt Int
  | PushNumber Number
  | PushBool Boolean
  | PushString String
  -- A variable: resolved against the frame environment, then the global table. 
  | Load String
  -- A native foreign leaf (ADR-0022/0032): materialise a [Vforeign] by looking the
  -- name up in the host registry passed to the VM. Stuck if the name is not native.
  | ForeignRef String
  -- Pop the top value and bind it to a name in the frame environment (a `let`).
  | Bind String
  -- params, body — capture the current frame environment into a closure value.
  | Closure (Array String) CodeBlock
  -- A local recursive group (name, value-computing chunk). Members are evaluated
  -- sharing one environment ref that is backpatched with the group once built
  -- (knot-tying, ADR-0030): each member's closures capture that ref, so a member's
  -- self/mutual references — always under a lambda for an eagerly-constructible
  -- cycle — resolve after construction. Subsumes the all-lambda case.
  | MakeRec (Array (String /\ CodeBlock))
  -- tag, arity, nargs — pop nargs and build saturated `Vdata` or partial `Vctor`.
  | Ctor String Int Int
  | Record (Array String)
  | Array Int
  | GetField String
  -- Pop a `Vdata` / `Varray` and push its i-th field / element. Used by a decision
  -- tree to extract a known-present sub-occurrence (ADR-0031); the index is in range
  -- by construction (the tag/length switch above it established the shape).
  | Proj Int
  | Proj_arr Int
  | Update (Array String)
  | Prim PrimOp Int
  -- Pop n args then the function; apply (eval/apply). Non-tail pushes a frame.
  | Call Int
  | TailCall Int
  | Return
  | Jump Int
  | JumpUnless Int
  -- Decision-tree dispatch (ADR-0031). Each pops the inspected occurrence value and
  -- branches by its discriminant to a matching case (relative offset) or, failing
  -- that, the default (relative). A value the wrong *kind* (e.g. a non-data value
  -- under [Switch_ctor]) is stuck (type-impossible, as in the oracle's matcher); a
  -- well-typed discriminant that no case names takes the default edge (a value-level
  -- non-match). [Switch_lit] branches on a scalar literal, [Switch_len] on a
  -- `Varray`'s length (ADR-0012: a different length is a value-level non-match).
  | SwitchCtor (Array (String /\ Int)) Int
  | SwitchLit (Array (Literal /\ Int)) Int
  | SwitchLen (Array (Int /\ Int)) Int
  -- No alternative matched (or every guard fell through): a stuck program.
  | Fail String

type CodeBlock = Array Instruction

derive instance Eq Instruction
derive instance Generic Instruction _
instance Show Instruction where
  show i = genericShow i