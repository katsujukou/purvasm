-- | Fold-guaranteed case-of-case distribution (ADR-0089 Addendum 2026-07-11, slice 3): rewrite
-- |
-- | ```
-- | let r = <CCase/CIf tree> in case r of oalts
-- | ```
-- |
-- | by pushing the outer `case` **recursively through the tree to its leaves** — but **only** when
-- | every leaf's result value decidably matches `oalts` (the fold guarantee: exactly one
-- | alternative survives per leaf), and one of two tiers licenses the copying (ADR-0089 Addendum +
-- | its accepted 2026-07-11 extension):
-- |
-- |   * **atom tier** — every outer right-hand side is a bare atom (`Ret (CAtom …)`): each
-- |     surviving copy is a single node, so the rewrite is a strict shrink with no threshold.
-- |   * **bounded tier** — non-atom RHSs are allowed when every alternative is *leaf-independent*
-- |     (no binder variable occurs free in its RHS — folding then never substitutes the leaf value
-- |     into the copy, so the budget counts the whole cost) and the total copied size
-- |     (Σ over leaves of the *selected* alternative's RHS size) fits `dupBudget`.
-- |
-- | A leaf whose match is undecidable, a guarded alternative (tree or outer, ADR-0013 order), a
-- | multi-scrutinee outer case, any other use of `r`, or a non-atom-RHS case failing the bounded
-- | tier's conditions blocks the rewrite.
-- |
-- | The pass is **syntactic**, run on the quoted (uniquely-`$q`-named) term inside `nbeBinding`'s
-- | loop: at each qualifying leaf it re-shares the leaf value under the *same* binder `r`
-- | (branch-disjoint scopes, so reuse is capture-free) and re-seats the outer case —
-- | `let r = <leaf value> in case r of oalts` — which the **next evaluation round folds** via the
-- | ordinary case-of-known machinery (the marks pattern: discovery here, application by `Eval`).
-- | Decidability here deliberately mirrors `Eval.matchBinder`; if the two ever disagree the placed
-- | case simply stays unfolded — a lost optimisation, never a semantic change.
-- |
-- | Intermediate computations on an arm's path (`let`s before the leaf) are preserved in place:
-- | only the *continuation* moves, so every computation still runs exactly on the paths it ran on.
module Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Distribute
  ( distributeCases
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvAlt, fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Analysis (sizeExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Nbe.Types (binderVarsOrdered)

-- | Apply the distribution wherever it fires, bottom-up over the whole term.
distributeCases :: Expr -> Expr
distributeCases = goExpr
  where
  goExpr :: Expr -> Expr
  goExpr = case _ of
    Ret c -> Ret (goC c)
    Let r c rest ->
      let
        rest' = goExpr rest
        c' = goC c
      in
        case rest' of
          Ret (CCase [ AtomVar r' ] oalts)
            | r' == r
            , isBranch c'
            , not (Set.member r (altsFree oalts)) ->
                -- two tiers (ADR-0089 Addendum + its accepted extension):
                --   * atom tier — every outer RHS a bare atom: each surviving copy is one node, so
                --     the rewrite is a strict shrink (the fold-guarantee alone bounds surviving
                --     alternative *count*, not size).
                --   * bounded tier — non-atom RHSs allowed when every alternative is
                --     **leaf-independent** (no binder variable occurs in its RHS — a
                --     binder-consuming row substitutes the leaf value into the copy, duplication
                --     the budget below cannot count) and the total copied size fits the budget.
                case push r oalts (Ret c') of
                  Just pushed
                    | Array.all atomRhs oalts -> pushed.expr
                    | Array.all leafIndependent oalts && pushed.copied <= dupBudget -> pushed.expr
                  _ -> Let r c' rest'
          _ -> Let r c' rest'
    LetRec bs rest -> LetRec (bs <#> \b -> b { rhs = goExpr b.rhs }) (goExpr rest)

  goC :: CExpr -> CExpr
  goC = case _ of
    CLam ps b -> CLam ps (goExpr b)
    CIf a t e -> CIf a (goExpr t) (goExpr e)
    CCase ss alts -> CCase ss (map goAlt alts)
    c -> c

  goAlt alt = alt
    { result = case alt.result of
        Uncond e -> Uncond (goExpr e)
        Guarded gs -> Guarded (gs <#> \g -> { guard: goExpr g.guard, rhs: goExpr g.rhs })
    }

  isBranch = case _ of
    CCase _ _ -> true
    CIf _ _ _ -> true
    _ -> false

  atomRhs alt = case alt.result of
    Uncond (Ret (CAtom _)) -> true
    _ -> false

  -- No binder variable of the row occurs free in its RHS (`binderVars ∩ fv(rhs) = ∅`): folding
  -- then never substitutes the leaf value into the copied RHS, so `copied` is the whole cost.
  leafIndependent alt = case alt.result of
    Uncond e ->
      let
        bound = Array.concatMap binderVarsOrdered alt.binders
        free = fvExpr Set.empty e
      in
        not (Array.any (\v -> Set.member v free) bound)
    Guarded _ -> false

  altsFree = foldl (\s a -> Set.union s (fvAlt Set.empty a)) Set.empty

-- | The bounded tier's duplication budget (the accepted extension): the sum over leaves of the
-- | *selected* alternative's quoted-ANF size. Starts at 16; raising it (at most to 32) requires a
-- | measured justification.
dupBudget :: Int
dupBudget = 16

-- | Push `case r of oalts` through the branch tree to its leaves, returning the rewritten tree
-- | **and the total copied size** (Σ over leaves of the selected alternative's RHS size, for the
-- | bounded tier's budget); `Nothing` blocks the whole rewrite (an undecidable leaf, a guarded
-- | tree alternative, or a tree shape we do not descend).
push :: String -> Array Alt -> Expr -> Maybe { expr :: Expr, copied :: Int }
push r oalts = go
  where
  go :: Expr -> Maybe { expr :: Expr, copied :: Int }
  go = case _ of
    -- intermediate bindings stay on their path; only the continuation is rewritten.
    Let x c rest -> (\p -> p { expr = Let x c p.expr }) <$> go rest
    LetRec bs rest -> (\p -> p { expr = LetRec bs p.expr }) <$> go rest
    Ret c -> case c of
      CIf a t e -> do
        t' <- go t
        e' <- go e
        Just { expr: Ret (CIf a t'.expr e'.expr), copied: t'.copied + e'.copied }
      CCase ss ialts -> do
        alts' <- traverse goAlt ialts
        Just
          { expr: Ret (CCase ss (map _.alt alts'))
          , copied: foldl (\n a -> n + a.copied) 0 alts'
          }
      leaf | Just sz <- leafDecides leaf ->
        Just { expr: Let r leaf (Ret (CCase [ AtomVar r ] oalts)), copied: sz }
      _ -> Nothing

  goAlt alt = case alt.result of
    Uncond e -> (\p -> { alt: alt { result = Uncond p.expr }, copied: p.copied }) <$> go e
    Guarded _ -> Nothing

  -- Will `case <leaf> of oalts` decidably fold — and onto which alternative? Mirrors
  -- `Eval.matchBinder`/`evalCase.go` syntactically: skip decidably-non-matching rows, land on a
  -- decidable match with an unconditional rhs (returning that rhs's size, the leaf's copy cost).
  -- Anything else (undecidable row, guarded target, no matching row — a stuck program we must
  -- preserve as-is) declines.
  leafDecides leaf = decide 0
    where
    decide i = case Array.index oalts i of
      Nothing -> Nothing
      Just alt -> case matchRow leaf alt.binders of
        No -> decide (i + 1)
        Yes -> case alt.result of
          Uncond e -> Just (sizeExpr e)
          Guarded _ -> Nothing
        Unknown -> Nothing

  matchRow leaf = case _ of
    [ b ] -> matchC leaf b
    _ -> Unknown

data M = Yes | No | Unknown

andM :: M -> M -> M
andM No _ = No
andM _ No = No
andM Unknown _ = Unknown
andM _ Unknown = Unknown
andM Yes Yes = Yes

-- | Static match of a leaf *value* `CExpr` against a binder (the syntactic mirror of
-- | `Eval.matchBinder`; conservative — `Unknown` wherever evaluation could know more).
matchC :: CExpr -> Binder -> M
matchC c = case _ of
  BNull -> Yes
  BVar _ -> Yes
  BNamed _ b -> matchC c b
  BLit l -> case c of
    CAtom (AtomLit l') -> if l == l' then Yes else No
    _ -> Unknown
  BCtor tag subs -> case c of
    CCtor tag' arity args
      | Array.length args == arity ->
          if tag == tag' then Array.foldl andM Yes (Array.zipWith matchAtom args subs) else No
    _ -> Unknown
  BArray subs -> case c of
    CArray els ->
      if Array.length els == Array.length subs then Array.foldl andM Yes (Array.zipWith matchAtom els subs)
      else No
    _ -> Unknown
  BRecord fields -> case c of
    CRecord fs -> Array.foldl andM Yes
      ( fields <#> \fb -> case Array.find (\f -> f.prop == fb.prop) fs of
          Just f -> matchAtom f.val fb.binder
          Nothing -> Unknown
      )
    _ -> Unknown

matchAtom :: Atom -> Binder -> M
matchAtom a = case _ of
  BNull -> Yes
  BVar _ -> Yes
  BNamed _ b -> matchAtom a b
  BLit l -> case a of
    AtomLit l' -> if l == l' then Yes else No
    _ -> Unknown
  -- a structured binder against a bare atom: only a literal could decide, and it cannot match.
  _ -> case a of
    AtomLit _ -> No
    _ -> Unknown
