-- | Free variables of a CESK term (ADR-0021): the qualified keys a binding references but
-- | does not bind locally — its cross-binding dependencies, used for link-time
-- | reachability DCE. A `Foreign` leaf is opaque (no free variables). Ported from boot's
-- | `Link.free_vars`.
module Purvasm.Compiler.CESK.FreeVars (binderVars, freeVars, freeVarsSet) where

import Prelude

import Data.Array (concatMap)
import Data.Foldable (foldl)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.CESK.AST (Rhs(..), Term(..))

-- | Every variable a binder binds (recursively): `BVar`/`BNamed`, through nested patterns.
binderVars :: Binder -> Array String
binderVars = case _ of
  BNull -> []
  BVar x -> [ x ]
  BLit _ -> []
  BCtor _ subs -> concatMap binderVars subs
  BArray subs -> concatMap binderVars subs
  BRecord fs -> concatMap (binderVars <<< _.binder) fs
  BNamed x inner -> [ x ] <> binderVars inner

freeVarsSet :: Term -> Set String
freeVarsSet = fv Set.empty Set.empty
  where
  fv :: Set String -> Set String -> Term -> Set String
  fv bound acc = case _ of
    TmVar x -> if Set.member x bound then acc else Set.insert x acc
    TmLit _ -> acc
    TmCtor _ _ -> acc
    TmForeign _ -> acc
    TmLam p body -> fv (Set.insert p bound) acc body
    TmApp f a -> fv bound (fv bound acc f) a
    TmLet x e body -> fv (Set.insert x bound) (fv bound acc e) body
    TmLetrec binds body ->
      let
        bound' = foldl (\s (n /\ _) -> Set.insert n s) bound binds
        acc' = foldl (\a (_ /\ e) -> fv bound' a e) acc binds
      in
        fv bound' acc' body
    TmIf c t1 t2 -> fv bound (fv bound (fv bound acc c) t1) t2
    TmPrim _ args -> foldl (fv bound) acc args
    TmArray es -> foldl (fv bound) acc es
    TmRecord fs -> foldl (\a r -> fv bound a r.term) acc fs
    TmAccessor e _ -> fv bound acc e
    TmUpdate e ups -> foldl (\a r -> fv bound a r.term) (fv bound acc e) ups
    TmCase scruts alts ->
      let
        acc' = foldl (fv bound) acc scruts
      in
        foldl
          ( \a alt ->
              let
                bound' = foldl (flip Set.insert) bound (concatMap binderVars alt.binders)
              in case alt.result of
                Unconditional e -> fv bound' a e
                Guarded gs -> foldl (\aa g -> fv bound' (fv bound' aa g.guard) g.rhs) a gs
          )
          acc'
          alts

-- | The free variables as a sorted array (boot's `SSet.elements`).
freeVars :: Term -> Array String
freeVars = Set.toUnfoldable <<< freeVarsSet
