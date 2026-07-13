-- | Reify a semantic value back to ANF (ADR-0089 §2). The fresh-name discipline is pinned:
-- | `quote` **α-renames every binder it reifies** from the reserved `$q<n>` supply — a namespace
-- | disjoint from every other name producer in the pipeline (source identifiers cannot start with
-- | `$`; `purs` synthesises `$__unused`/`…$Dict`; `Normalize` mints `$a<n>`; `MatchCompile` mints
-- | `$dt<n>`) — so no binder from the input, an unfolded sibling body, or an upstream generator
-- | survives, and collisions are impossible by namespace disjointness, not by counter luck. Free
-- | names (top-level/foreign keys) are reified verbatim.
-- |
-- | Reconstruction is ANF-disciplined like `Normalize`: a compound value in operand position is
-- | `let`-bound at its use site; an `SLet`'s computation is emitted **in place** (the ADR-0089 §5
-- | pinning — this is the only point that linearises sequencing, and it never reorders).
-- |
-- | Stack-safety note: like `Normalize` and `ANF.FreeVars`, the walk is ordinary structural
-- | recursion (depth = `let`-spine length); the iterative-spine hardening is deferred until real
-- | modules exercise it.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Quote
  ( quote
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (Comp(..), NRhs(..), Sem(..))

type Q = State Int

freshQ :: Q String
freshQ = do
  modify_ (_ + 1)
  n <- get
  pure ("$q" <> show n)

-- | Reify a binding body's semantic value to ANF, with a per-binding deterministic supply.
quote :: Sem -> Expr
quote s = evalState (quoteExpr s) 0

quoteExpr :: Sem -> Q Expr
quoteExpr s = quoteC s (pure <<< Ret)

-- | Reify into a computation position, CPS-building the enclosing `let` spine (the `Normalize`
-- | pattern). `SLet`/`SLetRec` emit their binding here — in the order evaluation produced them.
quoteC :: Sem -> (CExpr -> Q Expr) -> Q Expr
quoteC s k = case s of
  SLet _ rhs kont -> quoteC rhs \c -> do
    x <- freshQ
    rest <- quoteC (kont (SVar x)) k
    pure (Let x c rest)
  SLetRec bs kont -> do
    names <- for bs (const freshQ)
    let vars = map SVar names
    rhss <- for bs \b -> quoteExpr (b.rhsF vars)
    rest <- quoteC (kont vars) k
    pure (LetRec (Array.zipWith (\v r -> { var: v, rhs: r }) names rhss) rest)
  SLit l -> k (CAtom (AtomLit l))
  SVar x -> k (CAtom (AtomVar x))
  SForeign f -> k (CAtom (AtomForeign f))
  SLam hints f -> do
    ps <- for hints (const freshQ)
    body <- quoteExpr (f (map SVar ps))
    k (CLam ps body)
  SCtor t n args -> quoteAtoms args \as -> k (CCtor t n as)
  SArr args -> quoteAtoms args \as -> k (CArray as)
  SRec fs -> quoteAtoms (map _.val fs) \as ->
    k (CRecord (Array.zipWith (\f a -> { prop: f.prop, val: a }) fs as))
  SRef r -> case r.spine of
    [] -> k (CAtom r.atom)
    sp -> quoteAtoms sp \as -> k (CApp r.atom as)
  SComp c -> case c of
    NApp h args -> quoteAtom h \ha -> quoteAtoms args \as -> k (CApp ha as)
    NPrim op args -> quoteAtoms args \as -> k (CPrim op as)
    NAcc v f -> quoteAtom v \a -> k (CAccessor a f)
    NPerform v -> quoteAtom v \a -> k (CPerform a)
    NUpd v ups -> quoteAtom v \a ->
      quoteAtoms (map _.val ups) \as ->
        k (CUpdate a (Array.zipWith (\u v' -> { prop: u.prop, val: v' }) ups as))
    NIf cond t e -> quoteAtom cond \ca -> do
      t' <- quoteExpr t
      e' <- quoteExpr e
      k (CIf ca t' e')
    NCase scruts alts -> quoteAtoms scruts \sas -> do
      alts' <- for alts \alt -> do
        fresh <- for alt.vars (const freshQ)
        let
          renames = Map.fromFoldable (Array.zip alt.vars fresh)
          vars = map SVar fresh
        result <- case alt.result of
          NUncond f -> Uncond <$> quoteExpr (f vars)
          NGuarded gs -> Guarded <$> for gs \g -> do
            guard <- quoteExpr (g.guard vars)
            rhs <- quoteExpr (g.rhs vars)
            pure { guard, rhs }
        pure { binders: map (renameBinder renames) alt.shape, result }
      k (CCase sas alts')

-- | Reify into an atom position: compound values are `let`-bound at the use site.
quoteAtom :: Sem -> (Atom -> Q Expr) -> Q Expr
quoteAtom s k = case s of
  SLit l -> k (AtomLit l)
  SVar x -> k (AtomVar x)
  SForeign f -> k (AtomForeign f)
  SRef r | Array.null r.spine -> k r.atom
  compound -> quoteC compound \c -> do
    x <- freshQ
    rest <- k (AtomVar x)
    pure (Let x c rest)

quoteAtoms :: Array Sem -> (Array Atom -> Q Expr) -> Q Expr
quoteAtoms ss k = go 0 []
  where
  go i acc = case Array.index ss i of
    Nothing -> k acc
    Just s -> quoteAtom s \a -> go (i + 1) (Array.snoc acc a)

-- | Rename a binder shape's variables with the alternative's fresh-name map (total on
-- | `binderVarsOrdered`, so the fallback branch is unreachable for well-formed alternatives).
renameBinder :: Map String String -> Binder -> Binder
renameBinder m = case _ of
  BNull -> BNull
  BLit l -> BLit l
  BVar x -> BVar (rn x)
  BNamed x b -> BNamed (rn x) (renameBinder m b)
  BCtor t bs -> BCtor t (map (renameBinder m) bs)
  BArray bs -> BArray (map (renameBinder m) bs)
  BRecord fs -> BRecord (fs <#> \f -> { prop: f.prop, binder: renameBinder m f.binder })
  where
  rn x = fromMaybe x (Map.lookup x m)
