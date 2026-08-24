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
  -- Decision-tree dispatch (ADR-0031/0083), **tree-shaped** (ADR-0110 §4(b)): each pops the
  -- inspected occurrence value and runs the arm whose discriminant matches, else the default. An arm
  -- is a nested block that yields the `case`'s value, and control resumes in the enclosing block —
  -- which is what the linear form's single end-join label was for.
  --
  -- This is the shape `MiddleEnd.MatchCompile` already produced and the lowering used to throw away:
  -- a consumer of an image should read the decision tree, not rebuild it from offsets. The tree
  -- cannot duplicate code — `DTree` is a pure tree, each node emitted once — so the two forms carry
  -- the same instructions in the same order.
  --
  -- A value the wrong *kind* (e.g. a non-data value under `SwitchCtor`) is stuck (type-impossible,
  -- as in the oracle's matcher); a well-typed discriminant that no arm names takes the default edge
  -- (a value-level non-match). `SwitchLit` branches on a scalar literal, `SwitchLen` on a `Varray`'s
  -- length (ADR-0012: a different length is a value-level non-match).
  | SwitchCtor (Array (String /\ CodeBlock)) CodeBlock
  | SwitchLit (Array (Literal /\ CodeBlock)) CodeBlock
  | SwitchLen (Array (Int /\ CodeBlock)) CodeBlock
  -- A fully-matched but guarded row (ADR-0013): try each clause in order; if every guard is false,
  -- fall through to the block below — the remaining rows of the decision tree.
  | Guarded (Array GuardClause) CodeBlock
  -- No alternative matched (or every guard fell through): a stuck program.
  | Fail String
  -- ── The linearised `case` forms ────────────────────────────────────────────────────────────────
  --
  -- Produced by `Linearise.linearise` on the way out to a format that predates the tree shape: the
  -- `.pmo` and boot's version-3 image, whose reader knows offsets and nothing else. Never produced by
  -- the lowering, and never read back — the compiler's own IR is the tree above.
  | SwitchCtorRel (Array (String /\ Int)) Int
  | SwitchLitRel (Array (Literal /\ Int)) Int
  | SwitchLenRel (Array (Int /\ Int)) Int

type CodeBlock = Array Instruction

-- | One clause of a guard chain (ADR-0013): run `guard`, and if it leaves `true`, run `rhs` as the
-- | `case`'s result. Both are blocks, so the chain nests like everything else in the tree form.
type GuardClause = { guard :: CodeBlock, rhs :: CodeBlock }

derive instance Eq Instruction
derive instance Generic Instruction _
instance Show Instruction where
  show i = genericShow i