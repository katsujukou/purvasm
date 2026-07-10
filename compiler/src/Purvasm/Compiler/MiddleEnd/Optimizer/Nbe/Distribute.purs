-- | Fold-guaranteed case-of-case distribution (ADR-0089 Addendum 2026-07-11, slice 3): rewrite
-- |
-- | ```
-- | let r = <CCase/CIf tree> in case r of oalts
-- | ```
-- |
-- | by pushing the outer `case` **recursively through the tree to its leaves** — but **only** when
-- | every leaf's result value decidably matches `oalts` **and every outer right-hand side is a
-- | bare atom** (`Ret (CAtom …)`). The two conditions together are what make the rewrite a strict
-- | shrink: leaf-decidability guarantees exactly one alternative survives per leaf, and the
-- | atom-RHS bound makes each surviving copy a single node (a larger RHS would still duplicate
-- | once per leaf — the ADR-0089 Addendum's Correction; a size-budgeted relaxation is a separate
-- | Proposed extension). No size threshold, no residual duplication, and ANF's missing join points
-- | never bite. A leaf whose match is undecidable, a non-atom outer RHS, a guarded alternative
-- | (tree or outer, ADR-0013 order), a multi-scrutinee outer case, or any other use of `r` blocks
-- | the rewrite.
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (traverse)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvAlt)

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
            -- every outer right-hand side must be a bare atom: the fold-guarantee bounds how many
            -- alternatives survive per leaf (one), not their size — a larger (or binder-consuming)
            -- RHS would still duplicate once per leaf. Atom RHSs make each surviving copy one
            -- node, so the rewrite is a strict shrink in fact, not just in alternative count.
            , Array.all atomRhs oalts
            , not (Set.member r (altsFree oalts)) ->
                fromMaybe (Let r c' rest') (push r oalts (Ret c'))
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

  altsFree = foldl (\s a -> Set.union s (fvAlt Set.empty a)) Set.empty

-- | Push `case r of oalts` through the branch tree to its leaves; `Nothing` blocks the whole
-- | rewrite (an undecidable leaf, a guarded tree alternative, or a tree shape we do not descend).
push :: String -> Array Alt -> Expr -> Maybe Expr
push r oalts = go
  where
  go :: Expr -> Maybe Expr
  go = case _ of
    -- intermediate bindings stay on their path; only the continuation is rewritten.
    Let x c rest -> Let x c <$> go rest
    LetRec bs rest -> LetRec bs <$> go rest
    Ret c -> case c of
      CIf a t e -> Ret <$> (CIf a <$> go t <*> go e)
      CCase ss ialts -> Ret <<< CCase ss <$> traverse goAlt ialts
      leaf | leafDecides leaf -> Just (Let r leaf (Ret (CCase [ AtomVar r ] oalts)))
      _ -> Nothing

  goAlt alt = case alt.result of
    Uncond e -> (\e' -> alt { result = Uncond e' }) <$> go e
    Guarded _ -> Nothing

  -- Will `case <leaf> of oalts` decidably fold? Mirrors `Eval.matchBinder`/`evalCase.go`
  -- syntactically: skip decidably-non-matching rows, land on a decidable match with an
  -- unconditional rhs. Anything else (undecidable row, guarded target, no matching row — a stuck
  -- program we must preserve as-is) declines.
  leafDecides leaf = decide 0
    where
    decide i = case Array.index oalts i of
      Nothing -> false
      Just alt -> case matchRow leaf alt.binders of
        No -> decide (i + 1)
        Yes -> uncondAlt alt
        Unknown -> false

  uncondAlt alt = case alt.result of
    Uncond _ -> true
    Guarded _ -> false

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
