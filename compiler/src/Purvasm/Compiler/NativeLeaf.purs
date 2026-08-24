-- | **Native-leaf recognition**, shared by every backend that has to lower one
-- | ([ADR-0090](../../../../docs/design-decisions/0090-foreign-signature-reconstruction.md)'s FSR as
-- | the single source of truth, [ADR-0110](../../../../docs/design-decisions/0110-owned-vm-purescript-native.md)
-- | §4(a)).
-- |
-- | A *native leaf* is a foreign key the compiler does not resolve itself: not a primop or
-- | `unsafeCoerce` intrinsic, not a literal builtin, not a structural higher-order guest term — the
-- | genuine host-provided leaves (a `ulib` `.c` like `Data.Show.showNumberImpl`, a runtime `pvf_` like
-- | `Purvasm.String.byteLength`). Both backends need the *same* three answers about them: which keys
-- | they are, what physical arity each one's closure has, and how a reference to one is spelled in the
-- | IR. They lived in the LLVM driver while it was the only backend that lowered a leaf; the bytecode
-- | backend now needs them too, and a second derivation over the same FSR would be a drift source —
-- | two backends disagreeing about which keys are leaves is exactly the class of bug that hides until
-- | a program runs.
module Purvasm.Compiler.NativeLeaf
  ( leafClosureArity
  , nativeLeafArities
  , resolveNativeForeigns
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Purvasm.Compiler.Ffi (resolver)
import Purvasm.Compiler.ForeignSig (ForeignShape)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), Expr, mapAtoms)

-- | The **leaf closure arity** — the arity of the no-capture closure a backend builds for a native leaf
-- | reference, matching boot's `Ffi.foreign_arity`. This is **not** the raw FSR arrow count
-- | (`ForeignShape.arity`): a native `Effect`/`ST` leaf is a **thunk**, so a *nullary* one — an
-- | `Effect a` with no preceding arrow (`Purvasm.System.Process.argvImpl :: Effect (Array String)`, FSR
-- | `arity 0, retVsat`) — *is* the effect action, and its closure takes the unit-run: arity 1. A leaf with
-- | ≥ 1 data arg returns a *fresh* effect thunk when saturated (`leaf_write_line` builds a `\$u -> …`
-- | closure), so its closure arity stays the data-arg count. Building a nullary `Effect` leaf at arity 0
-- | makes `run`'s unit application **over-apply** an already-fired leaf onto its own result — e.g. `argv
-- | unit` applies `unit` to the returned `Array`, a "not callable (kind Array)" fault / heap corruption.
leafClosureArity :: ForeignShape -> Int
leafClosureArity s = if s.retVsat then max s.arity 1 else s.arity

-- | The **native leaves** among a module's (accumulated own ∪ deps) foreign shapes (ADR-0090): the
-- | foreign keys the compiler does NOT itself resolve (`resolver k = Nothing`), mapped to their
-- | **physical closure arity** — `leafClosureArity` of the FSR shape, *not* the raw semantic
-- | `shape.arity` (which differs for a nullary `Effect` leaf; see `leafClosureArity`). An intrinsic or
-- | structural foreign is materialised as a definition instead — a synthesised gdef in the native
-- | backend, a link-time runtime group in the bytecode one — so it is excluded here.
nativeLeafArities :: Map String ForeignShape -> Map String Int
nativeLeafArities = Map.mapMaybeWithKey \k s -> if isNothing (resolver k) then Just (leafClosureArity s) else Nothing

-- | Resolve a **native leaf** free reference from `AtomVar` to `AtomForeign`, the IR spelling that says
-- | "this is resolved by the host, not by a definition in this program". Runs on whole decl bodies
-- | before classification, so a class method whose impl *is* a native leaf (e.g. `Show Number`'s
-- | `showNumberImpl`) is carried into the instance dictionary as the foreign reference — under dynamic
-- | dispatch the dictionary itself must hold a callable value, not an unbound `AtomVar`.
-- |
-- | Backend-neutral, and required in both: the native backend emits a `@pvf_<key>` symbol plus a
-- | no-capture closure, the bytecode backend a `ForeignRef` the VM resolves through its provider
-- | ladder. Skipping it leaves the reference an ordinary global `Load` of a name nothing defines —
-- | which boot's VM silently rescues from its compiled-in registry (`Machine.lookup`'s native-foreign
-- | fall-through) and an owned VM cannot.
resolveNativeForeigns :: Map String Int -> Expr -> Expr
resolveNativeForeigns leaves = mapAtoms case _ of
  AtomVar k | Map.member k leaves -> AtomForeign k
  a -> a
