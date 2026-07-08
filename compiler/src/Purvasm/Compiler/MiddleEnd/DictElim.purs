-- | DictElim (ADR-0027, ADR-0082 §1): collapse statically-known type-class dispatch — **required
-- | lowering**, not optimisation (it runs under `--no-opt` too; without it dictionary access is
-- | wrong-shape and stuck, [[purescript-dictionaries-are-newtypes]]). A faithful transcription of
-- | boot's `dict_elim.ml`.
-- |
-- | A method call is `accessor dict args`, where the `accessor` is `\d -> case d of v -> v.φ` (the
-- | newtype dict binder erased, ADR-0018) and an instance `dict` is the newtype-identity-wrapped record
-- | `(\x -> x) { φ: impl, … }`. When both are statically known, the saturated call rewrites to
-- | `impl args` — an atom swap, no substitution.
-- |
-- | For B2 separate compilation (ADR-0085) the pass is pure ANF over one module's top-level bindings,
-- | supplied as `(key, CExpr)` spine pairs; the dictionary machinery of *imported* modules arrives as a
-- | `DictMachinery` from the build's in-memory env (so an imported accessor/instance resolves the same as
-- | a local one). `Gdef` extraction and env threading are the driver's job — this module never sees a
-- | `Gdef`.
module Purvasm.Compiler.MiddleEnd.DictElim
  ( DictMachinery
  , emptyMachinery
  , mergeMachinery
  , machineryOf
  , dictElimExpr
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))

-- | A module's dictionary machinery: its method **accessors** (binding key → the field `φ` it projects)
-- | and its instance **dictionaries** (binding key → the map `{ φ → impl atom }` of its members).
-- | Serializable plain data (ADR-0085 §4): the first field of the co-owned summary record.
type DictMachinery =
  { accessors :: Map String String
  , instances :: Map String (Map String Atom)
  }

emptyMachinery :: DictMachinery
emptyMachinery = { accessors: Map.empty, instances: Map.empty }

-- | Union two machineries (a module's own over the imported env; keys are qualified so disjoint).
mergeMachinery :: DictMachinery -> DictMachinery -> DictMachinery
mergeMachinery a b =
  { accessors: Map.union a.accessors b.accessors
  , instances: Map.union a.instances b.instances
  }

-- | The newtype dict wrapper lowers to the identity `\x -> x`; see through it.
isIdentityLam :: CExpr -> Boolean
isIdentityLam = case _ of
  CLam [ x ] (Ret (CAtom (AtomVar y))) -> x == y
  _ -> false

-- | Recognise a method accessor `\d -> case d of v -> v.φ` and return `φ`.
accessorField :: CExpr -> Maybe String
accessorField = case _ of
  CLam [ p ] (Ret (CCase [ AtomVar s ] [ alt ])) ->
    case alt.binders, alt.result of
      [ BVar v ], Uncond (Ret (CAccessor (AtomVar w) field))
        | p == s && v == w -> Just field
      _, _ -> Nothing
  _ -> Nothing

-- | Resolve a spine name to the record it denotes, seeing through aliases and the identity (newtype)
-- | wrapper. Returns the record's `{ field → impl atom }`.
resolveRecord :: Map String CExpr -> Set String -> String -> Maybe (Map String Atom)
resolveRecord tbl seen name
  | Set.member name seen = Nothing
  | otherwise = case Map.lookup name tbl of
      Just (CRecord fields) -> Just (Map.fromFoldable (map (\f -> Tuple f.prop f.val) fields))
      Just (CAtom (AtomVar y)) -> resolveRecord tbl (Set.insert name seen) y
      Just (CApp (AtomVar f) [ AtomVar a ])
        | maybe false isIdentityLam (Map.lookup f tbl) -> resolveRecord tbl (Set.insert name seen) a
      _ -> Nothing

-- | Classify a module's top-level spine bindings (given as `(key, defining CExpr)` pairs) into its
-- | dictionary machinery: accessors first, then instances (a name that is not an accessor and resolves
-- | to a record).
machineryOf :: Array (Tuple String CExpr) -> DictMachinery
machineryOf pairs =
  let
    tbl = Map.fromFoldable pairs
    accessors = Map.fromFoldable
      (Array.mapMaybe (\(Tuple name c) -> Tuple name <$> accessorField c) pairs)
    instances = Map.fromFoldable
      ( Array.mapMaybe
          ( \(Tuple name _) ->
              if Map.member name accessors then Nothing
              else Tuple name <$> resolveRecord tbl Set.empty name
          )
          pairs
      )
  in
    { accessors, instances }

-- | An impl is safe to lift to a call site iff it is in scope everywhere: a literal or foreign, or a
-- | reference to a top-level binding (a program-wide global key).
liftable :: Set String -> Atom -> Boolean
liftable gkeys = case _ of
  AtomLit _ -> true
  AtomForeign _ -> true
  AtomVar k -> Set.member k gkeys

-- | Rewrite one binding body: collapse every saturated `accessor dict …` whose accessor and dict are
-- | known (in `machinery`) and whose impl is liftable, to `impl …`. `gkeys` is the program-wide global
-- | key set (for the liftable check). Recurses into `CLam`/`CIf`/`CCase` bodies.
dictElimExpr :: Set String -> DictMachinery -> Expr -> Expr
dictElimExpr gkeys machinery = rwExpr
  where
  rwExpr :: Expr -> Expr
  rwExpr = case _ of
    Ret c -> Ret (rwCexpr c)
    Let x c rest -> Let x (rwCexpr c) (rwExpr rest)
    LetRec binds rest -> LetRec (map (\b -> b { rhs = rwExpr b.rhs }) binds) (rwExpr rest)

  rwCexpr :: CExpr -> CExpr
  rwCexpr c = case c of
    CApp (AtomVar acc) args -> case Array.uncons args of
      Just { head: AtomVar d, tail: rest } -> fromMaybe c (dispatch acc d rest)
      _ -> c
    CLam ps body -> CLam ps (rwExpr body)
    CIf a t e -> CIf a (rwExpr t) (rwExpr e)
    CCase ats alts -> CCase ats (map rwAlt alts)
    _ -> c

  rwAlt alt = alt
    { result = case alt.result of
        Uncond e -> Uncond (rwExpr e)
        Guarded gs -> Guarded (map (\g -> g { guard = rwExpr g.guard, rhs = rwExpr g.rhs }) gs)
    }

  dispatch :: String -> String -> Array Atom -> Maybe CExpr
  dispatch acc d rest = do
    field <- Map.lookup acc machinery.accessors
    fields <- Map.lookup d machinery.instances
    impl <- Map.lookup field fields
    if liftable gkeys impl then Just (if Array.null rest then CAtom impl else CApp impl rest)
    else Nothing
