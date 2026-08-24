-- | The owned VM's bytecode vocabulary ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §4).
-- |
-- | This is the existing `.pvm` instruction set, *evolved* rather than replaced: same stack machine,
-- | same name-keyed environments, same decision-tree dispatch. Two things differ, and both are forced
-- | by decisions the record makes rather than by taste:
-- |
-- |   * `ForeignRef` carries the leaf's **physical arity** (§4(a)). The VM must know when a leaf
-- |     saturates and, unlike boot, has no compiled-in registry to read it from; asking the loaded
-- |     provider would duplicate a fact the PureScript type already states.
-- |   * `case` dispatch keeps its **tree** shape (§4(b)): a switch's arms and default hold nested
-- |     `CodeBlock`s instead of back-patched relative offsets, so a consumer reads the decision tree
-- |     the compiler already built (`MiddleEnd.MatchCompile`) instead of reconstructing it. An arm
-- |     yields the `case`'s value and control resumes in the enclosing block, which is what the
-- |     linear form's single end-join label was for.
-- |
-- | `Jump`/`JumpUnless` are deliberately untouched (§4(b)'s scope discipline): they lower `if`, not
-- | `case`, and structuring them is a separate question.
-- |
-- | The vocabulary is duplicated from `Purvasm.Compiler.Bytecode.Instruction` rather than shared, for
-- | now: the VM consumes the format and must not drag the compiler into its binary. Whether producer
-- | and consumer should share one definition — extracted into a package both depend on — is decided
-- | with the image reader, when a drift between them could first go unnoticed.
module Purvasm.VM.Instruction
  ( CodeBlock
  , GuardClause
  , Instruction(..)
  , Literal(..)
  , PrimOp(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))

-- | A scalar literal, as a `SwitchLit` discriminant or a pushed constant.
data Literal
  = LInt Int
  | LBool Boolean
  | LNumber Number
  | LString String

derive instance eqLiteral :: Eq Literal
derive instance genericLiteral :: Generic Literal _
instance showLiteral :: Show Literal where
  show = genericShow

-- | The primitive operations, monomorphic by construction (ADR-0007). Bitwise `Int` ops follow the
-- | `Data.Int.Bits` semantics the rest of the toolchain implements: results re-wrapped to signed 32
-- | bits, shift counts masked `& 31`, `ZshrInt` the zero-fill right shift.
data PrimOp
  = AddInt
  | SubInt
  | MulInt
  | DivInt
  | ModInt
  | AndInt
  | OrInt
  | XorInt
  | ShlInt
  | ShrInt
  | ZshrInt
  | ComplementInt
  | AddNumber
  | SubNumber
  | MulNumber
  | DivNumber
  | IntToNumber
  | NumberToInt
  | EqInt
  | EqString
  | EqNumber
  | EqBool
  | LtInt
  | LtString
  | LtNumber
  | AndBool
  | OrBool
  | NotBool
  | Append
  | IndexArray
  | LengthArray
  | NewArray
  | SetArray
  | RecordGet
  | RecordSet
  | RecordHas
  | RecordDelete
  | RecordUnion

derive instance eqPrimOp :: Eq PrimOp
derive instance genericPrimOp :: Generic PrimOp _
instance showPrimOp :: Show PrimOp where
  show = genericShow

-- | One clause of a guard chain (ADR-0013): run `guard`, and if it is true, run `rhs` as the `case`'s
-- | result. Both are blocks so the chain nests like everything else in the tree form.
type GuardClause = { guard :: CodeBlock, rhs :: CodeBlock }

data Instruction
  = PushInt Int
  | PushNumber Number
  | PushBool Boolean
  | PushString String
  -- A variable: resolved against the frame environment, then the global table.
  | Load String
  -- A native foreign leaf: `key` and its physical closure arity (§4(a)). Resolution is the foreign
  -- frontier's (ADR-0111), which is why the key survives into the image rather than being bound here.
  | ForeignRef String Int
  -- Pop the top value and bind it to a name in the frame environment (a `let`).
  | Bind String
  -- params, body — capture the current frame environment into a closure value.
  | Closure (Array String) CodeBlock
  -- A local recursive group (name, value-computing chunk), knot-tied through one shared environment
  -- ref that is backpatched once the members are built (ADR-0030).
  | MakeRec (Array (String /\ CodeBlock))
  -- tag, arity, nargs — pop nargs and build a saturated or partial constructor value.
  | Ctor String Int Int
  | Record (Array String)
  | Array Int
  | GetField String
  -- Pop a data/array value and push its i-th field / element. The index is in range by construction:
  -- the tag or length test above it established the shape (ADR-0031).
  | Proj Int
  | ProjArray Int
  | Update (Array String)
  | Prim PrimOp Int
  -- Pop n args then the function; apply (eval/apply). Non-tail pushes a frame.
  | Call Int
  | TailCall Int
  | Return
  | Jump Int
  | JumpUnless Int
  -- Decision-tree dispatch (ADR-0031/0083), tree-shaped (§4(b)): pop the inspected occurrence and run
  -- the arm whose discriminant matches, else the default. The arm's value is the `case`'s value.
  -- A value of the wrong *kind* is stuck (type-impossible); a well-typed discriminant that no arm
  -- names takes the default (a value-level non-match).
  | SwitchCtor (Array (String /\ CodeBlock)) CodeBlock
  | SwitchLit (Array (Literal /\ CodeBlock)) CodeBlock
  | SwitchLen (Array (Int /\ CodeBlock)) CodeBlock
  -- A fully-matched but guarded row: try each clause in order; if every guard is false, fall through
  -- to the block below (the remaining rows of the decision tree).
  | Guarded (Array GuardClause) CodeBlock
  -- No alternative matched (or every guard fell through): a stuck program.
  | Fail String
  -- ── The linearised `case` forms, read from a pre-§4(b) image ──────────────────────────────────
  --
  -- Today's `.pvm` lowers a decision tree to switches over **relative offsets** into a flat block,
  -- and the format change that replaces them (§4(b)) is the LAST step of ADR-0110's slice 2 — after
  -- the owned VM has taken over the optimiser measurement field, since that changeover needs boot
  -- and the owned VM to agree on instruction counts first. Until then the reader meets offsets, and
  -- the alternative to executing them is delinearising on the way in: rebuilding the tree the
  -- producer already had, which §4(b) calls the wrong direction and which would be thrown away at
  -- the same moment these are.
  --
  -- They are therefore deliberately temporary, and deliberately dumb: an arm is a jump, exactly as
  -- in boot's VM, so the two agree about what a step is while both are running the same corpus.
  | SwitchCtorRel (Array (String /\ Int)) Int
  | SwitchLitRel (Array (Literal /\ Int)) Int
  | SwitchLenRel (Array (Int /\ Int)) Int

derive instance eqInstruction :: Eq Instruction
derive instance genericInstruction :: Generic Instruction _

-- | Structural, for a reader's diagnostics and for tests that compare a decode against the vocabulary
-- | it should have produced. `CodeBlock` nests, so this is recursive by construction.
instance showInstruction :: Show Instruction where
  show i = genericShow i

type CodeBlock = Array Instruction
