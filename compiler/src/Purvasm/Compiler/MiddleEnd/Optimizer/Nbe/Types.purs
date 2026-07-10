-- | The NbE semantic domain (ADR-0089 §2): HOAS values (`SLam` holds a host function, so β is
-- | meta-application), neutrals-as-computations (`SComp`, pinned in place when sequenced), residual
-- | sequencing points (`SLet`/`SLetRec`), and the `SRef` spine-deferral carrier — a reference to a
-- | module sibling or intrinsic foreign that **accumulates its applied arguments** while **deferring
-- | the body's unfolding behind `Lazy`** (unfolded only when the spine settles and the gate approves;
-- | `Lazy` sharing guarantees a multi-reference body is evaluated at most once — a pinned ADR-0082
-- | defence, §ref 3 of sidenote 0012).
-- |
-- | The value/computation split carries the ADR-0089 §5 effect-soundness rule structurally: values
-- | (literals, lambdas, constructors, records, arrays, references) may move/inline subject to gates;
-- | a `Comp` sequenced by a `Let` is emitted **in place** by `Quote` — never reordered past another
-- | computation. `pinnedPrim` marks the primops that are computations even on known operands
-- | (mutation / allocation identity / mutable-read ordering); every other primop is a pure value
-- | operation.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types
  ( Sem(..)
  , Comp(..)
  , NAlt
  , NRhs(..)
  , Ref
  , RefTarget(..)
  , ExternEntry
  , NbeEnv
  , EvalEnv
  , binderVarsOrdered
  , pinnedPrim
  ) where

import Prelude

import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.MiddleEnd.ANF (Atom)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | A semantic value. `SVar` is an opaque reference (a residual local minted by `Quote`, or an
-- | unknown global key — globals keep their names, locals are always quote-fresh). `SLet`
-- | sequences a computation (or a shared value) before its continuation; the continuation receives
-- | the binder's value — `Quote` passes a fresh `SVar`.
data Sem
  = SLit Literal
  | SLam (Array String) (Array Sem -> Sem)
  | SCtor String Int (Array Sem)
  | SArr (Array Sem)
  | SRec (Array { prop :: String, val :: Sem })
  | SVar String
  | SForeign String
  | SRef Ref
  | SComp Comp
  | SLet (Maybe String) Sem (Sem -> Sem)
  | SLetRec (Array { hint :: String, rhsF :: Array Sem -> Sem }) (Array Sem -> Sem)

-- | A deferred reference: the original atom spelling (reified verbatim if never unfolded), the
-- | target's unfold facts, and the argument spine accumulated so far. Spine elements are
-- | atom-derived (ANF operands are atoms), so carrying them per-reference cannot duplicate large
-- | structure.
type Ref = { atom :: Atom, target :: RefTarget, spine :: Array Sem }

data RefTarget
  = TIntrinsic { op :: PrimOp, arity :: Int }
  | TExtern ExternEntry

-- | A module sibling published for gate-site-A unfolding (ADR-0089 §4): its gate facts (computed
-- | from the binding's post-DictElim body each `optimizeModule` round) and its lazily-evaluated
-- | semantic value. `arity` is `Just` for a `Ret (CLam …)` body (unfold at saturation), `Nothing`
-- | for a value body (an alias / data CAF — unfold on demand, size-gated).
type ExternEntry =
  { arity :: Maybe Int
  , size :: Int
  , cxLeqDeref :: Boolean
  , closed :: Boolean
  , value :: Lazy Sem
  }

-- | A stuck computation. Sequenced computations are pinned (ADR-0089 §5); `NAcc`/`NUpd` and the
-- | non-`pinnedPrim` `NPrim`s are *pure* computations — safe to sink to their (analysis-gated) use
-- | sites — while `NApp` (a call that may perform when forced), the pinned prims, and the branching
-- | forms (whose arms may contain calls) always stay where the input sequenced them.
data Comp
  = NApp Sem (Array Sem)
  | NPrim PrimOp (Array Sem)
  | NAcc Sem String
  | NUpd Sem (Array { prop :: String, val :: Sem })
  | NIf Sem Sem Sem
  | NCase (Array Sem) (Array NAlt)

-- | A neutral `case` alternative: the binder shape (variable names are placeholders `Quote`
-- | renames), the bound variable names in `binderVarsOrdered` order, and the right-hand side as a
-- | host function of the bound values.
type NAlt = { shape :: Array Binder, vars :: Array String, result :: NRhs }

data NRhs
  = NUncond (Array Sem -> Sem)
  | NGuarded (Array { guard :: Array Sem -> Sem, rhs :: Array Sem -> Sem })

-- | The module facts `Eval` consults (ADR-0089 §1: module siblings + the compiler-global intrinsic
-- | table; slice 2 adds dependency-published bodies).
type NbeEnv =
  { externs :: Map String ExternEntry
  , intrinsic :: String -> Maybe { op :: PrimOp, arity :: Int }
  }

-- | The evaluation environment: local bindings, the current round's gate-B inline marks (binder
-- | names the previous round's analysis approved for inlining), and the module facts.
type EvalEnv =
  { locals :: Map String Sem
  , marks :: Set String
  , nbe :: NbeEnv
  }

-- | The variables a binder introduces, **in traversal order** — the order contract between `Eval`
-- | (which builds an `NAlt`'s closure over them) and `Quote` (which applies it to fresh variables
-- | and renames the shape in the same order).
binderVarsOrdered :: Binder -> Array String
binderVarsOrdered = case _ of
  BNull -> []
  BLit _ -> []
  BVar x -> [ x ]
  BNamed x b -> [ x ] <> binderVarsOrdered b
  BCtor _ bs -> bs >>= binderVarsOrdered
  BArray bs -> bs >>= binderVarsOrdered
  BRecord fs -> fs >>= \f -> binderVarsOrdered f.binder

-- | Primops that are computations even on known operands: `NewArray` (allocation identity —
-- | duplicating it forks the array), `SetArray` (in-place mutation under the ST build discipline),
-- | `IndexArray` (a read that must not move across a `SetArray` of the same buffer). Everything
-- | else is a pure value operation (records are immutable copy-on-update; `LengthArray` is fixed
-- | for a fixed-length array even mid-build).
pinnedPrim :: PrimOp -> Boolean
pinnedPrim = case _ of
  NewArray -> true
  SetArray -> true
  IndexArray -> true
  _ -> false
