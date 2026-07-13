-- | The NbE evaluator (ADR-0089 §2/§5): ANF → the semantic domain. A redex fires **only when its
-- | operands are known** — β is HOAS meta-application, `case`/`if` fold only on a decidable
-- | scrutinee, primops fold only on literals (with **VM-exact** semantics), projections fold only
-- | on known records — and everything else becomes a neutral. Sequencing is structural: a `Let`
-- | whose right-hand side is a computation produces an `SLet` the quoter emits **in place**, so
-- | neutral calls are never reordered, duplicated, or dropped (ADR-0089 §5).
-- |
-- | Inline decisions here are the *eager* subset only: trivial values (literals, variables,
-- | references) and the previous round's gate-B `marks`. Everything else is shared via `SLet` and
-- | judged by the analysis after `quote` (the discovery/application split of sidenote 0012 §2).
-- | Gate site A (extern unfolding) is judged here, at spine saturation, from the callee's
-- | pre-computed facts (`ExternEntry`).
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Eval
  ( evalExpr
  , evalC
  , foldPrim
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber) as Int
import Data.Int.Bits (complement, shl, shr, xor, zshr, (.&.), (.|.))
import Data.Lazy (force)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote (quote)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (ArgUse, Comp(..), EvalEnv, NRhs(..), RefTarget(..), Sem(..), binderVarsOrdered)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | Float a value's inner sequencing outward: the continuation receives the settled value, and any
-- | `SLet`/`SLetRec` the sub-evaluation produced stays outside (order preserved).
seqSem :: Sem -> (Sem -> Sem) -> Sem
seqSem s f = case s of
  SLet h rhs k -> SLet h rhs (\v -> seqSem (k v) f)
  SLetRec bs k -> SLetRec bs (\vs -> seqSem (k vs) f)
  v -> f v

bindLocal :: EvalEnv -> String -> Sem -> EvalEnv
bindLocal env x v = env { locals = Map.insert x v env.locals }

bindLocals :: EvalEnv -> Array String -> Array Sem -> EvalEnv
bindLocals env xs vs = env { locals = Array.foldl (\m (Tuple x v) -> Map.insert x v m) env.locals (Array.zip xs vs) }

-- | Values cheap enough to inline eagerly, without waiting for the gate: atoms and references.
-- | An `SRef`'s spine elements are atom-derived, so duplicating the reference duplicates only a
-- | (re)construction of the application chain, never large structure.
eager :: Sem -> Boolean
eager = case _ of
  SLit _ -> true
  SVar _ -> true
  SForeign _ -> true
  SRef _ -> true
  _ -> false

evalExpr :: EvalEnv -> Expr -> Sem
evalExpr env = case _ of
  Ret c -> evalC env c
  -- A defer-marked binder (saturated grouped application, single projection use — ADR-0089
  -- parameterized-instance extension) binds the *deferred saturated ref* instead of a pinned
  -- computation: the marked projection then sees the ref and judges the fold. Single-use (checked
  -- at discovery) is what makes this sharing-safe.
  Let x (CApp h args) rest
    | Set.member x env.defers
    , Just d <- deferredOf env h args -> evalExpr (bindLocal env x d) rest
  Let x c rest ->
    seqSem (evalC env c) \v ->
      if eager v || Set.member x env.marks then evalExpr (bindLocal env x v) rest
      else SLet (Just x) v (\s -> evalExpr (bindLocal env x s) rest)
  LetRec binds body ->
    let
      names = map _.var binds
    in
      SLetRec
        (binds <#> \b -> { hint: b.var, rhsF: \vs -> evalExpr (bindLocals env names vs) b.rhs })
        (\vs -> evalExpr (bindLocals env names vs) body)

evalC :: EvalEnv -> CExpr -> Sem
evalC env = case _ of
  CAtom a -> evalAtom env a
  CLam ps body -> SLam ps (\args -> evalExpr (bindLocals env ps args) body)
  CApp h args -> applySem (evalAtom env h) (map (evalAtom env) args)
  CPrim op args -> evalPrim op (map (evalAtom env) args)
  CCtor t n args -> SCtor t n (map (evalAtom env) args)
  CArray args -> SArr (map (evalAtom env) args)
  CRecord fs -> SRec (fs <#> \f -> { prop: f.prop, val: evalAtom env f.val })
  CAccessor a f -> evalAcc (evalAtom env a) f
  CUpdate a us -> evalUpd (evalAtom env a) (us <#> \u -> { prop: u.prop, val: evalAtom env u.val })
  CIf c t e -> case evalAtom env c of
    SLit (LBool b) -> evalExpr env (if b then t else e)
    s -> SComp (NIf s (evalExpr env t) (evalExpr env e))
  CCase scruts alts -> evalCase env (map (evalAtom env) scruts) alts
  CPerform a -> performSem (evalAtom env a)

-- | Run an `Effect`/`ST` thunk (GER, ADR-0099). β fires **only** on a known unit-lambda
-- | (`perform (\$u -> body) → body`, in place); a stuck computation is sequenced first (mirroring
-- | `applySem`'s computation-head handling), and everything else — an unresolved reference, an
-- | opaque global — stays a pinned `NPerform` marker so the head-based purity analysis never loses
-- | which thunk gets performed (ADR-0099 §2). Deliberately **not** routed through `applySem`: a
-- | performed `SRef` must not accumulate a unit into its spine and unfold through the gate.
performSem :: Sem -> Sem
performSem = case _ of
  SLam ps body | Array.length ps == 1 -> body [ SLit (LInt 0) ]
  SComp c -> SLet Nothing (SComp c) performSem
  s -> SComp (NPerform s)

evalAtom :: EvalEnv -> Atom -> Sem
evalAtom env = case _ of
  AtomLit l -> SLit l
  a@(AtomForeign k) -> refOf env a k
  a@(AtomVar x) -> case Map.lookup x env.locals of
    Just v -> v
    Nothing -> refOf env a x

-- | A non-local name: a module sibling / dependency candidate (gate-A target), an intrinsic
-- | foreign, a structural-rung guest term (also gate-A-target shaped), or an opaque global.
-- |
-- | A **constant-class** extern (a nullary-constructor CAF like `Data.Ordering.LT`, or a literal
-- | builtin) is forced eagerly to its value: reifying it per use site duplicates nothing real (a
-- | nullary constructor is an unboxed immediate, ADR-0064), and the syntactic value is what lets
-- | the slice-3 distribution decide leaves that reference these CAFs.
refOf :: EvalEnv -> Atom -> String -> Sem
refOf env atom k = case Map.lookup k env.nbe.externs of
  Just entry -> constOr entry (SRef { atom, target: TExtern entry, spine: [] })
  Nothing -> case env.nbe.intrinsic k of
    Just ia -> SRef { atom, target: TIntrinsic ia, spine: [] }
    Nothing -> case env.nbe.structural k of
      Just entry -> constOr entry (SRef { atom, target: TExtern entry, spine: [] })
      Nothing -> case atom of
        AtomForeign _ -> SForeign k
        _ -> SVar k
  where
  constOr entry fallback
    | isNothing entry.arity && entry.size <= 2 = case force entry.value of
        v@(SCtor _ 0 []) -> v
        v@(SLit _) -> v
        _ -> fallback
    | otherwise = fallback

-- | Application. β fires on a known lambda; an `SRef` accumulates its spine and is judged at
-- | saturation (gate site A); an application *of* a computation sequences it first; everything
-- | else is a neutral call.
applySem :: Sem -> Array Sem -> Sem
applySem f0 args0
  | Array.null args0 = f0
  | otherwise = seqSem f0 \f -> case f of
      SLam ps body ->
        let
          n = Array.length ps
          m = Array.length args0
        in
          if m == n then body args0
          else if m < n then SLam (Array.drop m ps) (\rest -> body (args0 <> rest))
          else seqSem (body (Array.take n args0)) \r -> applySem r (Array.drop n args0)
      SRef r ->
        let
          spine' = r.spine <> args0
          nargs = Array.length spine'
          neutral = SComp (NApp (SRef r) args0)
        in
          case r.target of
            TIntrinsic { op, arity } ->
              if nargs == arity then evalPrim op spine'
              else if nargs < arity then SRef r { spine = spine' }
              -- over-applied intrinsic: ill-typed; leave the whole application neutral.
              else neutral
            TExtern entry -> case entry.arity of
              Just a ->
                if nargs < a then SRef r { spine = spine' }
                -- a grouped builder never unfolds at bare saturation (its fold site is the
                -- deferred-ref projection trigger; unfolding here would residualize the whole
                -- record with its stopped sibling calls).
                else if not (Set.isEmpty entry.group) then neutral
                else if gateA entry || knownArgTier entry (Array.take a spine') then
                  seqSem (applySem (force entry.value) (Array.take a spine')) \res ->
                    applySem res (Array.drop a spine')
                else neutral
              -- a value-body extern (alias / data CAF) applied: force it if small, else neutral.
              -- A grouped alias applied as a function is ill-typed; keep it neutral.
              Nothing ->
                if entry.size < forceCallBound && Set.isEmpty entry.group then
                  applySem (force entry.value) spine'
                else neutral
      SCtor t n as ->
        let
          need = n - Array.length as
        in
          if Array.length args0 <= need then SCtor t n (as <> args0)
          -- over-applying a constructor is ill-typed; keep it neutral.
          else SComp (NApp f args0)
      SComp c -> SLet Nothing (SComp c) (\v -> applySem v args0)
      -- SVar/SForeign/SLit/SArr/SRec heads: an unknown (or ill-typed) call — a pinned neutral.
      _ -> SComp (NApp f args0)

-- | Gate site A (ADR-0089 §4, reference clauses at saturation): unfold a lambda extern iff it is
-- | small, or closed (no free names at all) and moderately small.
gateA :: forall r. { size :: Int, closed :: Boolean | r } -> Boolean
gateA e = e.size < 16 || (e.closed && e.size < 64)

-- | The scrutinised-known-arg 64-tier (ADR-0089 self-compile extension) — the surgical channel
-- | for 16–63-node dictionary builders after the 64-tier's return to strict closedness. Fires at
-- | saturation iff **no parameter is ever applied in head position** (universal — an existential
-- | check would admit a mixed combinator: a projected config record alongside an applied lambda
-- | parameter; the reference survives its existential only because its clauses stop at 16) and
-- | some argument is a **known value shape** (through the `known` peek — a dictionary CAF
-- | arrives as an `SRef`; the peek is decision-only, substitution still uses the atom-derived
-- | spine element) whose parameter is projected somewhere. Lambda arguments never qualify.
knownArgTier :: forall r. { size :: Int, argUses :: Array ArgUse | r } -> Array Sem -> Boolean
knownArgTier e args =
  e.size < 64
    && not (Array.null e.argUses)
    && Array.all (\u -> u.appliedHead == 0) e.argUses
    && Array.any identity (Array.zipWith qualifying e.argUses args)
  where
  qualifying u a = u.projected > 0 && knownValueShape (known a)

  knownValueShape = case _ of
    SRec _ -> true
    SCtor _ n as -> Array.length as == n
    SLit _ -> true
    _ -> false

-- | The size bound for forcing a value-body extern **as a whole** (an alias applied as a callee):
-- | the entire value survives reification, so this stays the reference's
-- | `shouldInlineExternReference` bound.
forceCallBound :: Int
forceCallBound = 16

-- | The size bound for peeking at a value-body extern under a **projection or match**: only the
-- | projected field / matched substructure survives reification (the rest of the forced value is
-- | discarded, never quoted), so the bound is the publish bound — the peek's residual is governed
-- | by gate B and the size/time gate, not by the container's size.
peekBound :: Int
peekBound = 64

-- | Peek through a bare small extern reference when a *value* is demanded (projection, match).
-- | Grouped entries are excluded: their forced value is (or contains) a deferred saturated ref,
-- | whose only legal consumer is the `groupedProject` trigger — forcing one here would let the
-- | neutral fallback reify the application inline, changing the term on refusal.
known :: Sem -> Sem
known = case _ of
  s@(SRef r)
    | Array.null r.spine -> case r.target of
        TExtern e | isNothing e.arity, e.size < peekBound, Set.isEmpty e.group -> known (force e.value)
        _ -> s
  s -> s

-- | The deferred saturated ref (ADR-0089 parameterized-instance extension): a defer-marked
-- | binder's rhs `builder(dicts…)` as a first-class carrier, judged at its (single) projection.
deferredOf :: EvalEnv -> Atom -> Array Atom -> Maybe Sem
deferredOf env h args = case evalAtom env h of
  SRef r
    | Array.null r.spine
    , TExtern e <- r.target
    , not (Set.isEmpty e.group)
    , Just a <- e.arity
    , Array.length args == a -> Just (SRef r { spine = map (evalAtom env) args })
  _ -> Nothing

-- | The grouped fold trigger: force the (group-stopped) builder value, apply the spine, select
-- | the projected field, and **commit only if the selected residual is group-free** — a selected
-- | field that still references a group sibling folds to a term containing a fresh group call,
-- | which would ratchet one level per driver round (the ADR's termination pin). Refusal returns
-- | `Nothing` and the caller's neutral path reifies the *original* reference unchanged.
-- |
-- | `settle` substitutes let-bound values into the record (the builder body's field lambdas are
-- | `SLet`-shared): sharing loss is bounded by the entry's published size, and the unselected
-- | fields — the superclass thunks carrying the stopped sibling calls — never reify at all.
groupedProject :: Sem -> String -> Maybe Sem
groupedProject s0 field = case s0 of
  SRef r
    | TExtern e <- r.target
    , not (Set.isEmpty e.group) -> case e.arity of
        Just a
          | Array.length r.spine == a ->
              case settle (applySem (force e.value) r.spine) of
                Just (SRec fs)
                  | Just f <- Array.find (\g -> g.prop == field) fs
                  , groupFree e.group f.val -> Just f.val
                _ -> Nothing
        -- the CAF-split alias (a published saturated application): its value is the deferred
        -- ref itself — recurse into the saturated case.
        Nothing
          | Array.null r.spine
          , e.size < peekBound -> case force e.value of
              s'@(SRef _) -> groupedProject s' field
              _ -> Nothing
        _ -> Nothing
  _ -> Nothing
  where
  -- Substitute only *values* through the body's `SLet` chain: a computation rhs must refuse the
  -- commit — the quoter substitutes a fresh variable there, but `settle` passes the rhs itself,
  -- and `applySem`'s computation-head wrap (`SLet Nothing (SComp c) (\v -> applySem v args)`)
  -- would then regenerate the identical `SLet` forever (the Control.Monad.State.Trans
  -- `monoidStateT` hang: a projected thunk applied over an *unknown* dict parameter). With known
  -- dict arguments — the shape the extension targets — those chain links fold to values first.
  settle = case _ of
    SLet _ v k -> case v of
      SComp _ -> Nothing
      _ -> settle (k v)
    v@(SRec _) -> Just v
    _ -> Nothing

  groupFree grp v =
    let
      e = quote v
    in
      Set.isEmpty (Set.intersection grp (Set.union (fvExpr Set.empty e) (cfExpr e)))

evalPrim :: PrimOp -> Array Sem -> Sem
evalPrim op args = case traverse litOf args of
  Just lits | Just l <- foldPrim op lits -> SLit l
  _ -> SComp (NPrim op args)
  where
  litOf = case _ of
    SLit l -> Just l
    _ -> Nothing

evalAcc :: Sem -> String -> Sem
evalAcc s0 field = seqSem s0 \s -> case groupedProject s field of
  Just v -> v
  Nothing -> evalAccKnown s field

evalAccKnown :: Sem -> String -> Sem
evalAccKnown s field = case known s of
  SRec fs -> case Array.find (\f -> f.prop == field) fs of
    Just f -> f.val
    -- a missing field is type-impossible; keep the projection neutral (stuck preserved).
    Nothing -> SComp (NAcc s field)
  v -> SComp (NAcc v field)

evalUpd :: Sem -> Array { prop :: String, val :: Sem } -> Sem
evalUpd s0 ups = seqSem s0 \s -> case known s of
  SRec fs
    -- fold only when every updated field exists (guaranteed by typing; anything else stays put).
    | Array.all (\u -> Array.any (\f -> f.prop == u.prop) fs) ups ->
        SRec
          ( fs <#> \f -> case Array.find (\u -> u.prop == f.prop) ups of
              Just u -> { prop: f.prop, val: u.val }
              Nothing -> f
          )
  v -> SComp (NUpd v ups)

-- --- case folding (ADR-0089 §5: fold only on a decidable row, Uncond target) --------------------

data MatchRes
  = MYes (Array (Tuple String Sem))
  | MNo
  | MUnknown

evalCase :: EvalEnv -> Array Sem -> Array Alt -> Sem
evalCase env scruts alts = go 0
  where
  -- Walk the alternatives: a decidably-non-matching row is skipped; the first decidable match
  -- with an unconditional rhs folds; a guarded or undecidable row keeps the whole case neutral
  -- (guard order is observable, ADR-0013 — v1 does not attempt guard-chain reduction). Before
  -- building the neutral, the two accepted boolean-case rules (ADR-0089 Addendum extension) get a
  -- cheap shape check.
  go i = case Array.index alts i of
    Nothing -> neutral -- no alternative can match: a stuck program, preserved as the residual case
    Just alt -> case matchRow scruts alt.binders of
      MNo -> go (i + 1)
      MYes binds -> case alt.result of
        Uncond e -> evalExpr (Array.foldl (\en (Tuple x v) -> bindLocal en x v) env binds) e
        Guarded _ -> neutral
      MUnknown -> neutral

  neutral = case booleanCaseRule of
    Just v -> v
    Nothing -> SComp (NCase scruts (map naltOf alts))

  -- The ADR-0089 Addendum boolean-case rules, checked only once folding failed:
  --
  --   * boolean case-eta — `case b of <true-lit> -> x; <irrefutable> -> y` with boolean-literal
  --     results is the identity (`x == tb, y == not tb` → `b`) or the negation (`NotBool(b)`).
  --     Well-typed corefn guarantees the scrutinee is Boolean (the same assumption the ordinary
  --     folding makes); the alternatives bind nothing, so nothing is dropped or moved.
  --   * constant-arm collapse — every alternative returns the *same* literal directly, no row
  --     introduces a binding, and an irrefutable trailing row guarantees some arm matches (a
  --     potentially-stuck match must be preserved): the case is that literal. The scrutinees are
  --     atoms, so dropping the dispatch drops no computation.
  booleanCaseRule = case scruts, alts of
    [ s ],
    [ { binders: [ BLit (LBool tb) ], result: Uncond (Ret (CAtom (AtomLit (LBool r1)))) }
    , { binders: [ irr ], result: Uncond (Ret (CAtom (AtomLit (LBool r2)))) }
    ]
      | irrefutable irr ->
          if r1 == tb && r2 == not tb then Just s
          else if r1 == not tb && r2 == tb then Just (evalPrim NotBool [ s ])
          else constArm
    _, _ -> constArm

  constArm = case Array.uncons alts of
    Just { head }
      | Just l <- litRhs head
      , Array.all (\a -> litRhs a == Just l && Array.null (Array.concatMap binderVarsOrdered a.binders)) alts
      , Just lastAlt <- Array.last alts
      , Array.all irrefutable lastAlt.binders -> Just (SLit l)
    _ -> Nothing

  litRhs alt = case alt.result of
    Uncond (Ret (CAtom (AtomLit l))) -> Just l
    _ -> Nothing

  irrefutable = case _ of
    BNull -> true
    BVar _ -> true
    _ -> false

  naltOf alt =
    let
      vars = Array.concatMap binderVarsOrdered alt.binders
      withVars vs = bindLocals env vars vs
    in
      { shape: alt.binders
      , vars
      , result: case alt.result of
          Uncond e -> NUncond (\vs -> evalExpr (withVars vs) e)
          Guarded gs -> NGuarded
            ( gs <#> \g ->
                { guard: \vs -> evalExpr (withVars vs) g.guard
                , rhs: \vs -> evalExpr (withVars vs) g.rhs
                }
            )
      }

matchRow :: Array Sem -> Array Binder -> MatchRes
matchRow scruts binders = combine (Array.zipWith matchBinder scruts binders)

matchBinder :: Sem -> Binder -> MatchRes
matchBinder s0 = case _ of
  BNull -> MYes []
  BVar x -> MYes [ Tuple x s0 ]
  BNamed x b -> case matchBinder s0 b of
    MYes bs -> MYes ([ Tuple x s0 ] <> bs)
    r -> r
  BLit l -> case known s0 of
    SLit l' -> if l == l' then MYes [] else MNo
    _ -> MUnknown
  BCtor tag subs -> case known s0 of
    SCtor tag' arity args
      -- only a *saturated* constructor value is a datum (a partial one is a closure).
      | Array.length args == arity ->
          if tag == tag' then combine (Array.zipWith matchBinder args subs) else MNo
    _ -> MUnknown
  BArray subs -> case known s0 of
    SArr els ->
      if Array.length els == Array.length subs then combine (Array.zipWith matchBinder els subs)
      else MNo
    _ -> MUnknown
  BRecord fields -> case known s0 of
    SRec fs -> combine
      ( fields <#> \fb -> case Array.find (\f -> f.prop == fb.prop) fs of
          Just f -> matchBinder f.val fb.binder
          -- type-impossible (row says the field exists); stay undecidable, keep the case.
          Nothing -> MUnknown
      )
    _ -> MUnknown

-- | A row matches iff every column matches; one decidable non-match decides the row.
combine :: Array MatchRes -> MatchRes
combine = Array.foldl step (MYes [])
  where
  step MNo _ = MNo
  step _ MNo = MNo
  step MUnknown _ = MUnknown
  step _ MUnknown = MUnknown
  step (MYes a) (MYes b) = MYes (a <> b)

-- --- VM-exact constant folding (ADR-0089 §5) -----------------------------------------------------

-- | Fold a primop on literal operands, with exactly the boot-VM semantics (`machine.ml eval_prim`):
-- | 32-bit-wrapping `Int` arithmetic, the registry's Euclidean `div`/`mod` with division-by-zero
-- | → 0 (this compiler's own `Prelude` `div`/`mod` *are* those registry implementations), JS
-- | `Data.Int.Bits` bitwise semantics, IEEE `Number` (NaN-correct comparisons). Anything doubtful
-- | is left unfolded — notably `LtString` (the VM compares UTF-8 bytes = code points; the JS host's
-- | `<` compares UTF-16 code units, which disagree above the BMP) and `NumberToInt` (`ToInt32`).
foldPrim :: PrimOp -> Array Literal -> Maybe Literal
foldPrim op args = case op, args of
  AddInt, [ LInt a, LInt b ] -> Just (LInt (a + b))
  SubInt, [ LInt a, LInt b ] -> Just (LInt (a - b))
  MulInt, [ LInt a, LInt b ] -> Just (LInt (a * b))
  DivInt, [ LInt a, LInt b ] -> Just (LInt (div a b))
  ModInt, [ LInt a, LInt b ] -> Just (LInt (mod a b))
  AndInt, [ LInt a, LInt b ] -> Just (LInt (a .&. b))
  OrInt, [ LInt a, LInt b ] -> Just (LInt (a .|. b))
  XorInt, [ LInt a, LInt b ] -> Just (LInt (xor a b))
  ShlInt, [ LInt a, LInt b ] -> Just (LInt (shl a b))
  ShrInt, [ LInt a, LInt b ] -> Just (LInt (shr a b))
  ZshrInt, [ LInt a, LInt b ] -> Just (LInt (zshr a b))
  ComplementInt, [ LInt a ] -> Just (LInt (complement a))
  EqInt, [ LInt a, LInt b ] -> Just (LBool (a == b))
  LtInt, [ LInt a, LInt b ] -> Just (LBool (a < b))
  AddNumber, [ LNumber a, LNumber b ] -> Just (LNumber (a + b))
  SubNumber, [ LNumber a, LNumber b ] -> Just (LNumber (a - b))
  MulNumber, [ LNumber a, LNumber b ] -> Just (LNumber (a * b))
  DivNumber, [ LNumber a, LNumber b ] -> Just (LNumber (a / b))
  EqNumber, [ LNumber a, LNumber b ] -> Just (LBool (a == b))
  LtNumber, [ LNumber a, LNumber b ] -> Just (LBool (a < b))
  IntToNumber, [ LInt a ] -> Just (LNumber (Int.toNumber a))
  EqString, [ LString a, LString b ] -> Just (LBool (a == b))
  Append, [ LString a, LString b ] -> Just (LString (a <> b))
  EqBool, [ LBool a, LBool b ] -> Just (LBool (a == b))
  AndBool, [ LBool a, LBool b ] -> Just (LBool (a && b))
  OrBool, [ LBool a, LBool b ] -> Just (LBool (a || b))
  NotBool, [ LBool a ] -> Just (LBool (not a))
  _, _ -> Nothing
