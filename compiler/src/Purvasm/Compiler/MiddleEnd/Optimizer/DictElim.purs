-- | DictElim (ADR-0027): collapse statically-known type-class dispatch — **an optimisation** in the
-- | backend-neutral optimiser (ADR-0086 §3), not a codegen concern. A method call `accessor dict args`
-- | rewrites to `impl args` when both the accessor and the instance dictionary are statically known.
-- |
-- | It runs in **two** places (ADR-0086 §3): in `preOptimizeModule` as a *boot-parity bridge / compatibility
-- | shim* — the LLVM native backend cannot dispatch a runtime newtype dictionary, so under `--no-opt` this
-- | pass is what keeps the emitted `.ll` byte-identical to boot's `dict_elim.ml` output (the shim is
-- | retired at wall-3 self-host) — and inside `optimizeModule`'s fixpoint as an ordinary correctness-first
-- | optimisation. The two share this transcription of boot's `dict_elim.ml`; should their directional
-- | constraints ever diverge, ADR-0086 §3 permits splitting them into two definitions.
-- |
-- | A method call is `accessor dict args`, where the `accessor` is `\d -> case d of v -> v.φ` (the
-- | newtype dict binder erased, ADR-0018) and an instance `dict` is the newtype-identity-wrapped record
-- | `(\x -> x) { φ: impl, … }`. When both are statically known, the saturated call rewrites to
-- | `impl args` — an atom swap, no substitution.
-- |
-- | For B2 separate compilation (ADR-0085) the pass is pure ANF over one module's top-level bindings,
-- | supplied as `(key, CExpr)` spine pairs; the dictionary machinery of *imported* modules arrives as a
-- | `DictMachinery` from the build's in-memory env (so an imported accessor/instance resolves the same as
-- | a local one). `Gdef` extraction and env threading are the optimiser seam's job — this module never
-- | sees a `Gdef`.
module Purvasm.Compiler.MiddleEnd.Optimizer.DictElim
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
import Data.Maybe (Maybe(..), fromMaybe)
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

-- | A binding body split into its outer non-recursive `let`-chain (`locals`) and the tail computation
-- | it returns (`tail`, `Nothing` if the body ends in a `LetRec`, which never denotes a record).
-- |
-- | boot's `collect_spine` walks one **whole-program** `let`-spine (B1), where ANF normalisation has
-- | already lifted an instance's inner record binding (`$a = { φ: impl }`) to a *top-level* spine entry —
-- | so `resolve_record` finds it by name. Under B2 (ADR-0085) each binding is normalised **independently**,
-- | so that `$a` stays a **local** `let` inside the instance body (`let $a = { φ: impl } in $Dict $a`) and
-- | is invisible to a cross-binding table. So we resolve a record against the binding's *own* locals as
-- | well as the module's other top-level bindings.
type Stripped = { locals :: Map String CExpr, tail :: Maybe CExpr }

stripLets :: Expr -> Stripped
stripLets = go Map.empty
  where
  go acc = case _ of
    Let x c rest -> go (Map.insert x c acc) rest
    Ret c -> { locals: acc, tail: Just c }
    LetRec _ _ -> { locals: acc, tail: Nothing }

-- | Classify a module's top-level spine bindings (given as `(key, defining Expr)` pairs) into its
-- | dictionary machinery: accessors first, then instances (a non-accessor whose body resolves to a
-- | record). An instance's record is resolved by seeing through the identity (newtype `$Dict`) wrapper
-- | and aliases, following names into either the binding's **own local `let`s** or the module's other
-- | top-level bindings.
machineryOf :: Array (Tuple String Expr) -> DictMachinery
machineryOf pairs =
  let
    byKey :: Map String Stripped
    byKey = Map.fromFoldable (map (\(Tuple k e) -> Tuple k (stripLets e)) pairs)

    entries :: Array (Tuple String Stripped)
    entries = Map.toUnfoldable byKey

    -- Is `f` a top-level identity (newtype `$Dict`) wrapper?
    isIdentityTop f = case Map.lookup f byKey of
      Just { tail: Just c } -> isIdentityLam c
      _ -> false

    accessors = Map.fromFoldable
      ( Array.mapMaybe
          ( \(Tuple k s) -> case s.tail of
              Just c -> Tuple k <$> accessorField c
              Nothing -> Nothing
          )
          entries
      )

    -- Resolve a top-level key to its record (in that binding's own local scope).
    resolveTop seen key =
      if Set.member key seen then Nothing
      else case Map.lookup key byKey of
        Just { locals, tail: Just c } -> resolveTail (Set.insert key seen) locals c
        _ -> Nothing

    -- Resolve a tail computation to a record, given the enclosing body's `locals`.
    resolveTail seen locals = case _ of
      CRecord fields -> Just (Map.fromFoldable (map (\f -> Tuple f.prop f.val) fields))
      CAtom (AtomVar y) -> resolveRef seen locals y
      CApp (AtomVar f) [ AtomVar a ] | isIdentityTop f -> resolveRef seen locals a
      _ -> Nothing

    -- Resolve a name that is either a local `let` in `locals` or a top-level binding.
    resolveRef seen locals name =
      if Set.member name seen then Nothing
      else case Map.lookup name locals of
        Just c -> resolveTail (Set.insert name seen) locals c
        Nothing -> resolveTop seen name

    instances = Map.fromFoldable
      ( Array.mapMaybe
          ( \(Tuple k s) ->
              if Map.member k accessors then Nothing
              else case s.tail of
                Just c -> Tuple k <$> resolveTail Set.empty s.locals c
                Nothing -> Nothing
          )
          entries
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
