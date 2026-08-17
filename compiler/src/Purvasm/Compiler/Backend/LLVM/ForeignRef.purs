-- | ADR-0109 §1.1: ONE value per referenced native leaf, so every party spells its symbol the same
-- | way.
-- |
-- | The reference sites are not all in one place — `Emit.atom` materialises an `AtomForeign` in VALUE
-- | position without ever consulting the call classifier, `directTarget` resolves it in CALL
-- | position, `Program.foreignDecls` emits its `declare`, the hoisted-closure init builds its cell,
-- | and the census reports it. Before this module each of those re-derived the symbol from the key
-- | (`mangleForeign` had two call sites and the `$fclo` cell name would have added a third), so
-- | "derived once" was a convention no type enforced.
-- |
-- | [`ForeignRef`] is therefore opaque and carries every derived spelling. Its only *safe* producer
-- | is `Monad.foreignRef`, which also REGISTERS the reference — obtaining one and declaring one are
-- | the same act, so a reference cannot be emitted without its `declare` and its cell.
-- | [`unsafeForeignRef`] exists because the constructor cannot be shared across modules any other
-- | way; per the project's `unsafeXX` rule it is not part of the API a lowering should reach for,
-- | and `tools/seam-audit.sh` pins its single call site.
module Purvasm.Compiler.Backend.LLVM.ForeignRef
  ( ForeignRef
  , unsafeForeignRef
  , refKey
  , refSym
  , refCell
  , refArity
  , ForeignClosureMode(..)
  , foreignClosureEnvVar
  , parseForeignClosureMode
  , ForeignCallMode(..)
  , foreignCallEnvVar
  , parseForeignCallMode
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Purvasm.Compiler.Backend.LLVM.Mangle (mangleForeign)

-- | How a foreign reference obtains its leaf closure (ADR-0109 slice A, and its §5.2 counterfactual).
-- |
-- | A closed TYPE rather than a Boolean: the two modes are different lowerings with different
-- | safepoint behaviour, and `true`/`false` at a dozen call sites says nothing about which is which.
-- | The SAME value is handed to the emitter and to the activation plan — ADR-0107's rule, and here it
-- | is load-bearing rather than tidy: `PerUse` allocates, so a plan still believing `Hoisted` would
-- | UNDER-ROOT the operands around it and the counterfactual leg would be measuring a broken program.
data ForeignClosureMode
  -- | The measurement counterfactual: build the leaf's closure at every reference, as the emitter
  -- | did before slice A. A safepoint, no cells, no init.
  = PerUse
  -- | The shipped path: read the hoisted cell the entry object built once (§2.2-amended).
  | Hoisted

derive instance eqForeignClosureMode :: Eq ForeignClosureMode

instance showForeignClosureMode :: Show ForeignClosureMode where
  show = case _ of
    PerUse -> "per-use"
    Hoisted -> "hoisted"

-- | How a SATURATED native-leaf call is emitted (ADR-0109 slice B, and its §5.2 counterfactual).
-- | A closed type for the same reason [`ForeignClosureMode`] is one: the two are different lowerings,
-- | and the same value must reach every party that depends on which was chosen.
-- | THREE stages, because ADR-0109 pins slice B and slice C as separate checkpoints with separately
-- | pinned endpoints. A two-state mode cannot express the middle one, and a run that flips both forms
-- | at once nets the two slices together: slice B's contract is that `pv_tailcall_writes` does NOT
-- | move, and that is unobservable if the tail form changed in the same build.
data ForeignCallMode
  -- | The counterfactual for slice B: the generic `pv_apply`/`pv_tailcall` dispatch, as before this
  -- | ADR. The call is still CLASSIFIED as an eligible leaf — it records `foreign-deferred`, not the
  -- | `callee-foreign` residue — so no counter changes meaning between the legs.
  = ViaApply
  -- | SLICE B: the apply form is direct; the TAIL form stays deferred, so the trampoline counters are
  -- | untouched and slice B's mechanical endpoint stays checkable (it is also the `--paired tail`
  -- | pair's BEFORE leg).
  | DirectApplyOnly
  -- | SLICE C (the current default): both forms direct.
  | DirectApplyAndTail

derive instance eqForeignCallMode :: Eq ForeignCallMode

instance showForeignCallMode :: Show ForeignCallMode where
  show = case _ of
    ViaApply -> "via-apply"
    DirectApplyOnly -> "direct-apply-only"
    DirectApplyAndTail -> "direct-apply-and-tail"

foreignCallEnvVar :: String
foreignCallEnvVar = "PURVASM_FOREIGN_CALL"

-- | Parse slice B's knob, once, at the CLI edge. Fail-closed, exactly as [`parseForeignClosureMode`].
parseForeignCallMode :: Maybe String -> Either String ForeignCallMode
parseForeignCallMode = case _ of
  -- SLICE C is the default since 2026-08-17, on its own measured endpoint (§5.1: the tail transfer
  -- is exact and the apply axis is invariant) plus the correctness gates re-run on this tree.
  Nothing -> Right DirectApplyAndTail
  Just "direct-apply-only" -> Right DirectApplyOnly
  Just "direct-apply-and-tail" -> Right DirectApplyAndTail
  Just "via-apply" -> Right ViaApply
  Just other -> Left
    (foreignCallEnvVar <> ": expected absent, \"via-apply\", \"direct-apply-only\" or \"direct-apply-and-tail\", got " <> show other)

-- | The knob's environment variable — harness-owned, absent in every ordinary and correctness build.
foreignClosureEnvVar :: String
foreignClosureEnvVar = "PURVASM_FOREIGN_CLOSURE"

-- | Parse the knob, ONCE, at the CLI edge. Absent is the shipped mode; anything unrecognised is an
-- | ERROR, never a default — a typo silently meaning "hoisted" would report the A/B's two legs as
-- | one, which is the failure mode the whole paired protocol exists to avoid.
parseForeignClosureMode :: Maybe String -> Either String ForeignClosureMode
parseForeignClosureMode = case _ of
  Nothing -> Right Hoisted
  Just "hoisted" -> Right Hoisted
  Just "per-use" -> Right PerUse
  Just other -> Left (foreignClosureEnvVar <> ": expected absent, \"hoisted\" or \"per-use\", got " <> show other)

-- | A referenced native leaf: its key, the derived spellings, and its PHYSICAL closure arity
-- | (ADR-0090 `leafClosureArity` — not the raw FSR arrow count).
newtype ForeignRef = ForeignRef
  { key :: String
  , sym :: String
  , cell :: String
  , arity :: Int
  }

-- Equality/order by KEY: the derived fields are functions of it, so two refs to the same leaf are
-- the same reference no matter which site minted them (the `Map` in `Ctx.foreigns` relies on this).
derive newtype instance eqForeignRef :: Eq ForeignRef
derive newtype instance ordForeignRef :: Ord ForeignRef

instance showForeignRef :: Show ForeignRef where
  show (ForeignRef r) = "ForeignRef " <> r.key

-- | Mint a reference. UNSAFE only in the project's naming sense: nothing here can fail, but a caller
-- | that mints one WITHOUT registering it (the arity check and the `Ctx.foreigns` insert that
-- | `Monad.foreignRef` performs) would produce a reference the object never declares — a link error
-- | at best. Call `Monad.foreignRef`.
unsafeForeignRef :: String -> Int -> ForeignRef
unsafeForeignRef key arity =
  let
    sym = mangleForeign key
  in
    ForeignRef
      { key
      , sym
      -- The hoisted leaf-closure cell (ADR-0109 §2.2-amended): ONE per key program-wide, DEFINED
      -- in the entry object and `external` everywhere else. Suffixed rather than separately mangled
      -- — the leaf symbol is already injective, so `$fclo` over it is injective too, and the two
      -- names stay visibly related in the emitted `.ll`.
      , cell: sym <> "$fclo"
      , arity
      }

-- | The qualified foreign key (`M.f`).
refKey :: ForeignRef -> String
refKey (ForeignRef r) = r.key

-- | The leaf's `AbiCodeFn` linker symbol, bare (`pvf_…`); emitters add the `@`.
refSym :: ForeignRef -> String
refSym (ForeignRef r) = r.sym

-- | The program-wide hoisted-closure cell, bare (`pvf_…$fclo`); emitters add the `@`. Defined by
-- | the entry object (ADR-0109 §2.2-amended), declared `external` by every reader.
refCell :: ForeignRef -> String
refCell (ForeignRef r) = r.cell

-- | The leaf's physical closure arity.
refArity :: ForeignRef -> Int
refArity (ForeignRef r) = r.arity
