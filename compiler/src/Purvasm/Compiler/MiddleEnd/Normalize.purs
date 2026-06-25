-- | `normalize`: the CESK AST (upper IR) → ANF (lower IR) bridge (ADR-0025/0037).
-- | A-normalises (every argument an atom, every compound subexpression `let`-named so
-- | evaluation order is explicit) and uncurries (a `\a -> \b -> …` spine becomes one
-- | `CLam [a, b] …`; an `((f a) b) c` spine becomes one `CApp f [a, b, c]`).
-- |
-- | Ported from boot's `Middle_end.Transl.transl`: a CPS walk where `k` builds the
-- | surrounding `let`-sequence, `normAtom` additionally `let`-binds a non-atomic result
-- | so it can sit in argument position, and a `State Int` supplies fresh `$a…` names.
-- | The boot differential is up to α-renaming, so the fresh-name *order* need not match.
module Purvasm.Compiler.MiddleEnd.Normalize where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.CESK.AST (Alternative, Term(..))
import Purvasm.Compiler.CESK.AST (Rhs(..)) as Cesk
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))

type N = State Int

fresh :: N String
fresh = do
  modify_ (_ + 1)
  n <- get
  pure ("$a" <> show n)

-- | Collect a curried lambda spine `\a -> \b -> body` into `{ params: [a, b], body }`.
collectLam :: Term -> { params :: Array String, body :: Term }
collectLam = go []
  where
  go acc (TmLam p body) = go (Array.snoc acc p) body
  go acc t = { params: acc, body: t }

-- | Collect an application spine `((f a) b) c` into `{ head: f, args: [a, b, c] }`.
collectApp :: Term -> { head :: Term, args :: Array Term }
collectApp t0 = go t0 []
  where
  go (TmApp f a) args = go f (Array.cons a args)
  go t args = { head: t, args }

normalize :: Term -> Expr
normalize term = evalState (anfTail term) 0
  where
  -- | Normalise `t`, handing its computation to `k` (which builds the enclosing lets).
  normCe :: Term -> (CExpr -> N Expr) -> N Expr
  normCe t k = case t of
    TmLit l -> k (CAtom (AtomLit l))
    TmVar x -> k (CAtom (AtomVar x))
    TmForeign s -> k (CAtom (AtomForeign s))
    TmCtor tag arity -> k (CCtor tag arity [])
    TmLam _ _ ->
      let { params, body } = collectLam t
      in anfTail body >>= \b -> k (CLam params b)
    TmApp _ _ ->
      let { head, args } = collectApp t
      in normAtoms args \argAtoms -> case head of
           -- a saturated/partial constructor application stays a constructor node
           TmCtor tag arity -> k (CCtor tag arity argAtoms)
           _ -> normAtom head \h -> k (CApp h argAtoms)
    TmPrim op args -> normAtoms args \atoms -> k (CPrim op atoms)
    TmArray es -> normAtoms es \atoms -> k (CArray atoms)
    TmRecord fields ->
      normAtoms (map _.term fields) \atoms ->
        k (CRecord (Array.zipWith (\f a -> { prop: f.prop, val: a }) fields atoms))
    TmAccessor e label -> normAtom e \a -> k (CAccessor a label)
    TmUpdate e ups ->
      normAtom e \a ->
        normAtoms (map _.term ups) \atoms ->
          k (CUpdate a (Array.zipWith (\u v -> { prop: u.prop, val: v }) ups atoms))
    TmIf c t1 t2 ->
      -- boot builds `CIf (ca, anf_tail t1, anf_tail t2)`; OCaml evaluates a constructor's
      -- arguments right-to-left, so the *else* branch's fresh names are allocated before
      -- the *then* branch's. Match that order for byte-identical `$a…` numbering.
      normAtom c \ca -> do
        e2 <- anfTail t2
        e1 <- anfTail t1
        k (CIf ca e1 e2)
    TmLet x e1 e2 -> normCe e1 \c1 -> Let x c1 <$> normCe e2 k
    TmLetrec binds body -> do
      binds' <- traverse (\(x /\ rhs) -> anfTail rhs <#> \r -> { var: x, rhs: r }) binds
      LetRec binds' <$> normCe body k
    TmCase scruts alts ->
      normAtoms scruts \satoms -> do
        alts' <- traverse anfAlt alts
        k (CCase satoms alts')

  -- | Normalise `t` to an atom, `let`-binding it to a fresh name if it is compound.
  normAtom :: Term -> (Atom -> N Expr) -> N Expr
  normAtom t k =
    normCe t case _ of
      CAtom a -> k a
      ce -> do
        x <- fresh
        rest <- k (AtomVar x)
        pure (Let x ce rest)

  normAtoms :: Array Term -> (Array Atom -> N Expr) -> N Expr
  normAtoms ts k = case Array.uncons ts of
    Nothing -> k []
    Just { head: t, tail: rest } ->
      normAtom t \a -> normAtoms rest \atoms -> k (Array.cons a atoms)

  anfTail :: Term -> N Expr
  anfTail t = normCe t (Ret >>> pure)

  anfAlt :: Alternative -> N Alt
  anfAlt a = do
    result <- case a.result of
      Cesk.Unconditional e -> Uncond <$> anfTail e
      -- boot builds the tuple `(anf_tail g, anf_tail e)`; OCaml's right-to-left evaluation
      -- allocates the rhs's fresh names before the guard's. Match that for byte-identity.
      Cesk.Guarded gs ->
        Guarded <$> traverse
          (\g -> do
            rhs <- anfTail g.rhs
            guard <- anfTail g.guard
            pure { guard, rhs })
          gs
    pure { binders: a.binders, result }
