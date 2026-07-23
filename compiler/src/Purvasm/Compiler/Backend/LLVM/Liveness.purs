-- | The ADR-0105 liveness analysis and `RootPlan` (§1/§2): for one activation's ANF body, decide
-- | which ANF-level definitions (params, captures, `Let`/case-arm binders, the self-recursion
-- | `%env` word) have a live range **crossing at least one safepoint** — only those need
-- | activation root slots — and whether any reachable lowering recipe **may root** internally
-- | (the lowering-local tier; each recipe's declaration here is derived from its emission code in
-- | `Emit`, and the recipe-consistency unit tests hold the two together).
-- |
-- | A **safepoint** is an emitted operation that may allocate or run guest code (ADR-0064 §4).
-- | The per-node transfer functions (`cexprCanSafepoint` etc.) summarise each ANF node's whole
-- | lowering *sequence* — including operand forcing (`pv_force_if_byneed`'s slow path runs a
-- | thunk, so a forced non-immediate operand is a potential safepoint) and boxed-literal /
-- | foreign-closure materialisation inside `atom` — not just the node's "main" call (§1).
-- |
-- | The crossing criterion implements the §1 use-at-call boundary: a value consumed as an
-- | *operand* of a safepoint operation, with no use reachable after it, does NOT cross — the
-- | backward pass unions, at each safepoint, the names live *after* the whole node (its operands
-- | are consumed by it; protecting operand values across the node's internal safepoints is the
-- | lowering recipes' declared, lowering-local concern).
-- |
-- | Closures are opaque (§2): a `CLam`/`LetRec` body is walked only for its free variables (the
-- | captures, consumed at construction) — its own crossings belong to the lifted function's
-- | activation, analysed when that function is emitted.
-- |
-- | Stack-safety shape (§2a): ONE backward pass; the `Let`/`LetRec` spine is collected with a
-- | pure `tailRec` loop and folded in reverse with stack-safe `Array` folds, so recursion depth
-- | is control-flow nesting only, never spine length. No per-definition re-walks.
-- |
-- | The plan is a pure function of the ANF and the activation config — it takes NO ABI-profile
-- | input, which is the §4 release/debug `RootPlan`-equality contract by construction.
module Purvasm.Compiler.Backend.LLVM.Liveness
  ( ActivationConfig
  , ActivationPlan
  , envPseudo
  , activationPlan
  , needsFrame
  , atomCanSafepoint
  , forcedAtomCanSafepoint
  , primOpSafepoint
  , cexprCanSafepoint
  , cexprMayRootLocally
  , operandsMayRoot
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Purvasm.Compiler.Backend.LLVM.Mangle (sortRecordFields)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | One activation's rootable-name context: its parameters and captures (rooted today at the
-- | prologue), and — for a self-recursive function — the binding name whose calls read the
-- | `%env` word (`Emit`'s `selfCtx`).
type ActivationConfig =
  { params :: Array String
  , captures :: Array String
  , selfName :: Maybe String
  }

-- | The activation tier of the ADR-0105 §2 `RootPlan`, plus the lowering-local tier's
-- | may-root summary for the whole body.
type ActivationPlan =
  { crossing :: Set String
  -- ^ tracked definitions (params/captures/`Let`/case-arm binders; `envPseudo` for the self
  -- `%env` word) whose live range crosses ≥ 1 safepoint — the activation roots.
  , loweringMayRoot :: Boolean
  -- ^ some reachable lowering recipe declares may-root (the lowering-local tier can produce
  -- root sites even when `crossing` is empty).
  , anySafepoint :: Boolean
  -- ^ the body can reach ≥ 1 safepoint at all (no safepoint ⇒ nothing can cross).
  }

-- | The pseudo-name standing for the self-recursion `%env` word in `crossing` (not a legal ANF
-- | identifier, so it cannot collide).
envPseudo :: String
envPseudo = "%env"

-- | §2 frame decision: a frame is needed iff EITHER tier can produce a root site. An activation
-- | with `needsFrame = false` opens no frame and pops none; emitting a transient root inside it
-- | is a structural error (`rootLocal` requires the frame token).
needsFrame :: ActivationPlan -> Boolean
needsFrame p = not (Set.isEmpty p.crossing) || p.loweringMayRoot

-- --- §1 transfer functions (per-atom / per-op / per-node safepoint summaries) -------------------

-- | `atom` (unforced) materialisation: a boxed literal allocates (`pv_new_number`/`pv_new_str`)
-- | and a foreign reference builds its leaf closure (`pv_make_closure`); a var is a plain
-- | slot/global reload and scalar literals are immediates.
atomCanSafepoint :: Atom -> Boolean
atomCanSafepoint = case _ of
  AtomVar _ -> false
  AtomForeign _ -> true
  AtomLit (LInt _) -> false
  AtomLit (LBool _) -> false
  AtomLit (LNumber _) -> true
  AtomLit (LString _) -> true

-- | A *forced* operand (`forceValue`): the slow path (`pv_force_if_byneed`) may run a thunk, so
-- | any non-immediate forced operand is a potential safepoint on top of its materialisation.
forcedAtomCanSafepoint :: Atom -> Boolean
forcedAtomCanSafepoint = case _ of
  AtomLit (LInt _) -> false
  AtomLit (LBool _) -> false
  -- every non-immediate: the force slow path alone may run a thunk (a var may hold a by-need
  -- cell), and boxed literals / foreigns additionally allocate at materialisation.
  _ -> true

-- | The §1 classification of the primop's OWN runtime operation (operand forcing is accounted
-- | separately by `cexprCanSafepoint` — `CPrim` forces its operands). Anything allocating (or
-- | boxing) is a safepoint; in-place/read-only/register-only ops are not.
primOpSafepoint :: PrimOp -> Boolean
primOpSafepoint = case _ of
  -- boxed-Number arithmetic / widening box the result
  AddNumber -> true
  SubNumber -> true
  MulNumber -> true
  DivNumber -> true
  IntToNumber -> true
  -- string append and array construction allocate
  Append -> true
  NewArray -> true
  -- functional record updates allocate
  RecordSet -> true
  RecordDelete -> true
  RecordUnion -> true
  -- in-place store (ADR-0052 linear array-builder contract)
  SetArray -> false
  -- register-only scalar ops and comparisons; read-only accessors
  AddInt -> false
  SubInt -> false
  MulInt -> false
  DivInt -> false
  ModInt -> false
  AndInt -> false
  OrInt -> false
  XorInt -> false
  ShlInt -> false
  ShrInt -> false
  ZshrInt -> false
  ComplementInt -> false
  NumberToInt -> false
  EqInt -> false
  EqString -> false
  EqNumber -> false
  EqBool -> false
  LtInt -> false
  LtString -> false
  LtNumber -> false
  AndBool -> false
  OrBool -> false
  NotBool -> false
  IndexArray -> false
  LengthArray -> false
  RecordGet -> false
  RecordHas -> false

-- | Whether a node's whole lowering sequence can emit ≥ 1 safepoint operation (the §1
-- | `maySafepoint` transfer). Sub-`Expr`s (branches, guard bodies) are NOT included — the
-- | backward pass recurses into them itself; this summarises only the node's own operations.
cexprCanSafepoint :: CExpr -> Boolean
cexprCanSafepoint = case _ of
  CAtom a -> atomCanSafepoint a
  -- calls run guest code (force + call + settle)
  CApp _ _ -> true
  CPerform _ -> true
  -- closure construction allocates
  CLam _ _ -> true
  -- CPrim forces every operand, then runs the op
  CPrim op args -> Array.any forcedAtomCanSafepoint args || primOpSafepoint op
  -- saturated non-nullary ctor allocates; nullary is an immediate tag; unsaturated builds a
  -- builder closure (and `pv_apply`s it when partially applied)
  CCtor _ arity args
    | arity == 0 && Array.null args -> false
    | otherwise -> true
  CArray elems -> not (Array.null elems)
  -- even the empty record allocates
  CRecord _ -> true
  -- the projection forces a by-need record first; the read itself is read-only
  CAccessor a _ -> forcedAtomCanSafepoint a
  CUpdate _ _ -> true
  -- the condition force; branch bodies recursed separately
  CIf a _ _ -> forcedAtomCanSafepoint a
  -- scrutinee forces + dtree machinery (conservative: the dtree is lowering-tier fallback)
  CCase _ _ -> true

-- | Whether `evalAtoms`-style operand evaluation roots intermediates for THIS operand list: an
-- | operand is rooted iff it is non-immediate and some LATER operand can safepoint (the
-- | suffix-scan `Emit.evalAtoms` implements; `force` mirrors its force flag).
operandsMayRoot :: Boolean -> Array Atom -> Boolean
operandsMayRoot force atoms =
  -- ONE reverse fold (the same shape as `Emit.evalAtoms`' precomputed suffix scan): at each
  -- element, `later` is "some LATER operand can safepoint" — a per-index `Array.drop`/`any`
  -- re-scan would be quadratic in the operand count (§2a).
  (Array.foldr step { later: false, out: false } atoms).out
  where
  canSp = if force then forcedAtomCanSafepoint else atomCanSafepoint

  isImm = case _ of
    AtomLit (LInt _) -> true
    AtomLit (LBool _) -> true
    _ -> false

  step a st =
    { later: st.later || canSp a
    , out: st.out || (not (isImm a) && st.later)
    }

-- | The lowering-local tier's per-recipe may-root declaration (§2): does THIS node's lowering
-- | root temporaries internally? Derived from the `Emit` recipes; the recipe-consistency tests
-- | assert the ⇒ direction (roots emitted beyond the prologue ⇒ declared here). Sub-`Expr`s are
-- | not included (the pass ORs over the whole body itself).
cexprMayRootLocally :: CExpr -> Boolean
cexprMayRootLocally = case _ of
  CAtom _ -> false
  -- args may be rooted by evalAtoms; an SForceCell/SClosureEnv target roots its callee/operands
  CApp _ _ -> true
  CPerform _ -> true
  -- makeClosure evaluates captures (evalAtoms-shaped); conservative when any exist
  CLam _ _ -> true
  CPrim _ args -> operandsMayRoot true args
  CCtor _ arity args
    -- unsaturated with supplied fields roots the builder across their evaluation
    | Array.length args < arity && not (Array.null args) -> true
    | otherwise -> operandsMayRoot false args
  CArray elems -> operandsMayRoot false elems
  -- the emitter sorts fields by unsigned label id BEFORE evalAtoms (ADR-0069 §1), so the
  -- rooting suffix-scan must run over the SAME canonical order — source order can invert which
  -- operand precedes a later allocating one.
  CRecord fields -> operandsMayRoot false (map snd (sortRecordFields (map (\f -> Tuple f.prop f.val) fields)))
  CAccessor _ _ -> false
  -- the accumulator is rooted across every step
  CUpdate _ _ -> true
  CIf _ _ _ -> false
  -- the dtree roots occurrences/scrutinees (lowering-tier fallback, ADR-0083)
  CCase _ _ -> true

-- --- the backward pass --------------------------------------------------------------------------

type Res =
  { live :: Set String
  , crossing :: Set String
  , mayRoot :: Boolean
  , anySafepoint :: Boolean
  }

-- | Compute the activation plan for one body (§2): ONE backward pass unioning, at every
-- | safepoint, the names live after it; the result is intersected with the activation's tracked
-- | names (free names not bound locally are globals — they reload through permanent init-region
-- | handles, not activation roots).
activationPlan :: ActivationConfig -> Expr -> ActivationPlan
activationPlan cfg body =
  let
    r = goExpr cfg body Set.empty

    tracked = Set.fromFoldable cfg.params
      <> Set.fromFoldable cfg.captures
      <> boundNames body
      <>
        ( case cfg.selfName of
            Just _ -> Set.singleton envPseudo
            Nothing -> Set.empty
        )
  in
    { crossing: Set.intersection r.crossing tracked
    , loweringMayRoot: r.mayRoot
    , anySafepoint: r.anySafepoint
    }

emptyRes :: Res
emptyRes = { live: Set.empty, crossing: Set.empty, mayRoot: false, anySafepoint: false }

mergeRes :: Res -> Res -> Res
mergeRes a b =
  { live: a.live <> b.live
  , crossing: a.crossing <> b.crossing
  , mayRoot: a.mayRoot || b.mayRoot
  , anySafepoint: a.anySafepoint || b.anySafepoint
  }

-- | One spine step, collected iteratively so processing order can be reversed without recursion.
data Step'
  = SLet String CExpr
  | SLetRec (Array { var :: String, rhs :: Expr })

-- | Walk one `Expr` backward under continuation-live set `k`. The `Let`/`LetRec` spine is
-- | collected with a pure `tailRec` loop, then folded tail-first (`Array.foldl` over the
-- | reversed spine) — recursion depth is control nesting only (§2a).
goExpr :: ActivationConfig -> Expr -> Set String -> Res
goExpr cfg e0 k =
  let
    spine = tailRec
      ( \st -> case st.e of
          Let x c rest -> Loop { e: rest, acc: SLet x c : st.acc }
          LetRec binds rest -> Loop { e: rest, acc: SLetRec binds : st.acc }
          Ret c -> Done { steps: st.acc, final: c }
      )
      { e: e0, acc: Nil }

    -- tail-first: the final computation under `k`, then each step outward (innermost first —
    -- `spine.steps` is already innermost-first from the collection).
    r0 = goC cfg spine.final k
  in
    Array.foldl (stepBackward cfg) r0 (Array.fromFoldable spine.steps)

stepBackward :: ActivationConfig -> Res -> Step' -> Res
stepBackward cfg after = case _ of
  SLet x c ->
    let
      kAfter = Set.delete x after.live
      rc = goC cfg c kAfter
    in
      { live: rc.live
      , crossing: after.crossing <> rc.crossing
      , mayRoot: after.mayRoot || rc.mayRoot
      , anySafepoint: after.anySafepoint || rc.anySafepoint
      }
  SLetRec binds ->
    let
      binders = Set.fromFoldable (map _.var binds)
      kAfter = Set.difference after.live binders
      -- captures = the group bodies' free names minus the group (closure opacity: walked for
      -- fv only; the members' own crossings belong to their lifted activations)
      captures = Set.difference
        (Array.foldl (\acc b -> acc <> (goExpr cfg b.rhs Set.empty).live) Set.empty binds)
        binders
    in
      -- buildGrec allocates the shared env + cells: a safepoint sequence with `kAfter` live
      -- across it, and a may-root recipe (placeholder/env roots).
      { live: kAfter <> captures
      , crossing: after.crossing <> kAfter
      , mayRoot: true
      , anySafepoint: true
      }

-- | The names an `Atom` reads (a var; plus the self `%env` pseudo when it references the
-- | self-recursive binding — a self reference reads the env word).
atomUses :: ActivationConfig -> Atom -> Set String
atomUses cfg = case _ of
  AtomVar x
    | cfg.selfName == Just x -> Set.fromFoldable [ x, envPseudo ]
    | otherwise -> Set.singleton x
  _ -> Set.empty

usesOf :: ActivationConfig -> Array Atom -> Set String
usesOf cfg = Array.foldl (\acc a -> acc <> atomUses cfg a) Set.empty

-- | One computation under continuation-live set `k`. At every node that can safepoint, `k` —
-- | the names live AFTER the node; its own operands are consumed by it (§1 use-at-call
-- | boundary) — joins the crossing set.
goC :: ActivationConfig -> CExpr -> Set String -> Res
goC cfg c k = case c of
  CIf a t e ->
    let
      rt = goExpr cfg t k
      re = goExpr cfg e k
      branchLive = rt.live <> re.live
      condSp = forcedAtomCanSafepoint a
    in
      { live: atomUses cfg a <> branchLive
      -- the condition force precedes both branches: everything live at a branch entry is live
      -- across it.
      , crossing: rt.crossing <> re.crossing <> (if condSp then branchLive else Set.empty)
      , mayRoot: rt.mayRoot || re.mayRoot
      , anySafepoint: condSp || rt.anySafepoint || re.anySafepoint
      }
  CCase scruts alts ->
    let
      rAlts = Array.foldl (\acc alt -> mergeRes acc (goAlt cfg alt k)) emptyRes alts
    in
      -- scrutinee forces + the dtree's own machinery precede/interleave the arm bodies:
      -- conservative — everything live at any arm entry crosses (the dtree is the §3
      -- conservative-fallback tier).
      { live: usesOf cfg scruts <> rAlts.live
      , crossing: rAlts.crossing <> rAlts.live
      , mayRoot: true
      , anySafepoint: true
      }
  CLam ps body ->
    let
      -- closure opacity: the body is walked for free names only (the captures, consumed at
      -- `pv_make_closure`); its crossings belong to the lifted function's own plan.
      captures = Set.difference (goExpr cfg body Set.empty).live (Set.fromFoldable ps)
    in
      { live: k <> captures
      , crossing: k
      , mayRoot: cexprMayRootLocally c
      , anySafepoint: true
      }
  _ ->
    let
      sp = cexprCanSafepoint c
    in
      { live: k <> cexprUses cfg c
      , crossing: if sp then k else Set.empty
      , mayRoot: cexprMayRootLocally c
      , anySafepoint: sp
      }

goAlt :: ActivationConfig -> Alt -> Set String -> Res
goAlt cfg alt k =
  let
    bound = Set.fromFoldable (Array.concatMap binderNames alt.binders)

    scoped r = r { live = Set.difference r.live bound }
  in
    case alt.result of
      Uncond e -> scoped (goExpr cfg e k)
      -- Guard clauses are SEQUENTIAL: a false guard falls through to the next clause, so a
      -- later clause's live-in is part of an earlier guard's continuation — a name used only
      -- after a failing guard still crosses that guard's safepoints. On top of the guard
      -- expression's own safepoints, the emitter FORCES the guard's value before testing it
      -- (the dtree guard-fallthrough): an independent potential safepoint sitting between the
      -- guard and both continuations, exempt only when the guard's tail is already an
      -- immediate literal. Folded tail-first; the last clause's fall-through continuation is
      -- `k` (beyond-the-alt fallthrough is the CCase level's conservatism).
      Guarded gs -> scoped
        ( Array.foldl
            ( \after g ->
                let
                  rr = goExpr cfg g.rhs k
                  contAfterGuard = rr.live <> after.live
                  forceSp = guardResultForced g.guard
                  rg = goExpr cfg g.guard contAfterGuard
                in
                  { live: rg.live
                  , crossing: after.crossing <> rr.crossing <> rg.crossing
                      <> (if forceSp then contAfterGuard else Set.empty)
                  , mayRoot: after.mayRoot || rr.mayRoot || rg.mayRoot
                  , anySafepoint: after.anySafepoint || rr.anySafepoint || rg.anySafepoint || forceSp
                  }
            )
            (emptyRes { live = k })
            (Array.reverse gs)
        )

-- | Whether the post-guard `forceValue` is a potential safepoint: the force's slow path may run
-- | a thunk unless the guard's tail computation is already an immediate literal (the value the
-- | force receives). A spine-iterative walk to the tail (§2a).
guardResultForced :: Expr -> Boolean
guardResultForced = tailRec case _ of
  Let _ _ rest -> Loop rest
  LetRec _ rest -> Loop rest
  Ret c -> Done case c of
    CAtom (AtomLit (LInt _)) -> false
    CAtom (AtomLit (LBool _)) -> false
    _ -> true

-- | Operand uses of the non-branching nodes (branching nodes are handled directly in `goC`).
cexprUses :: ActivationConfig -> CExpr -> Set String
cexprUses cfg = case _ of
  CAtom a -> atomUses cfg a
  CApp f args -> atomUses cfg f <> usesOf cfg args
  CPerform t -> atomUses cfg t
  CPrim _ args -> usesOf cfg args
  CCtor _ _ args -> usesOf cfg args
  CArray elems -> usesOf cfg elems
  CRecord fields -> usesOf cfg (map _.val fields)
  CAccessor a _ -> atomUses cfg a
  CUpdate a ups -> atomUses cfg a <> usesOf cfg (map _.val ups)
  CLam _ _ -> Set.empty
  CIf _ _ _ -> Set.empty
  CCase _ _ -> Set.empty

binderNames :: Binder -> Array String
binderNames = case _ of
  BNull -> []
  BVar x -> [ x ]
  BLit _ -> []
  BCtor _ bs -> Array.concatMap binderNames bs
  BArray bs -> Array.concatMap binderNames bs
  BRecord fs -> Array.concatMap (binderNames <<< _.binder) fs
  BNamed x b -> Array.cons x (binderNames b)

-- | Every name bound inside the body (`Let`/`LetRec` binders, lambda params of NESTED bodies
-- | excluded — those are the lifted functions' concerns; case-arm binders included): the
-- | activation-tier tracked set beyond params/captures. Collected iteratively on the spine,
-- | recursing only through control nesting.
boundNames :: Expr -> Set String
boundNames = goB
  where
  goB e0 =
    let
      spine = tailRec
        ( \st -> case st.e of
            Let x c rest -> Loop { e: rest, acc: (Set.singleton x <> goBC c) <> st.acc }
            LetRec binds rest -> Loop { e: rest, acc: Set.fromFoldable (map _.var binds) <> st.acc }
            Ret c -> Done (goBC c <> st.acc)
        )
        { e: e0, acc: Set.empty }
    in
      spine

  goBC = case _ of
    CIf _ t e -> goB t <> goB e
    CCase _ alts -> Array.foldl
      ( \acc alt ->
          acc
            <> Set.fromFoldable (Array.concatMap binderNames alt.binders)
            <> case alt.result of
              Uncond e -> goB e
              Guarded gs -> Array.foldl (\a g -> a <> goB g.guard <> goB g.rhs) Set.empty gs
      )
      Set.empty
      alts
    -- closure opacity: a nested lambda body's binders are the lifted function's
    _ -> Set.empty
