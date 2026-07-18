-- | DictElim (ADR-0027): collapse statically-known type-class dispatch — **an optimisation** in the
-- | backend-neutral optimiser (ADR-0086 §3), not a codegen concern. A method call `accessor dict args`
-- | rewrites to `impl args` when both the accessor and the instance dictionary are statically known.
-- |
-- | It runs in **one** place: inside `optimizeModule`'s fixpoint as an ordinary correctness-first
-- | optimisation pass (`--opt`, both backends). Under `--no-opt` no backend collapses dispatch —
-- | dictionaries stay dynamically dispatched on native and VM alike (ADR-0104 §3 retired the LLVM
-- | backend's former boot-parity bridge, this pass's other historical call site). The transcription of
-- | boot's `dict_elim.ml` keeps its `ForeignLift` policy parameter (the split ADR-0086 §3 sanctions):
-- | the optimiser lifts intrinsic foreign keys (`intrinsicLift` — safe because the NbE inliner's
-- | intrinsic saturation runs next).
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
  , ForeignLift
  , emptyMachinery
  , mergeMachinery
  , machineryOf
  , intrinsicLift
  , dictElimExpr
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))

-- | A module's dictionary machinery: its method **accessors** (binding key → the field `φ` it projects),
-- | its instance **dictionaries** (binding key → the map `{ φ → impl atom }` of its members), and its
-- | **identity wrappers** (the newtype `$Dict` constructors, which lower to `\x -> x`). Identities must
-- | ride the machinery because an instance is routinely declared in a *different* module from its class
-- | (`Effect`'s `Bind` instance wraps `Control.Bind.Bind$Dict`) — recognising that instance's record
-- | requires seeing through an *imported* wrapper, which the local binding table alone cannot name.
-- | Serializable plain data (ADR-0085 §4): the first field of the co-owned summary record.
type DictMachinery =
  { accessors :: Map String String
  , instances :: Map String (Map String Atom)
  , identities :: Set String
  -- | Instance-dict keys the GER effect-family validator (ADR-0099 Slice 3, sidenote 0014) admitted
  -- | as belonging to the `Effect` `Monad`-hierarchy group. `machineryOf` leaves this empty (it sees
  -- | flattened members, not Rec groups); `localFactsOf` fills it from the raw `AnfModule` decls, and
  -- | `mergeMachinery` unions dependencies' sets so a consumer sees the imported `Effect` family.
  , effectFamily :: Set String
  }

emptyMachinery :: DictMachinery
emptyMachinery = { accessors: Map.empty, instances: Map.empty, identities: Set.empty, effectFamily: Set.empty }

-- | Union two machineries (a module's own over the imported env; keys are qualified so disjoint).
mergeMachinery :: DictMachinery -> DictMachinery -> DictMachinery
mergeMachinery a b =
  { accessors: Map.union a.accessors b.accessors
  , instances: Map.union a.instances b.instances
  , identities: Set.union a.identities b.identities
  , effectFamily: Set.union a.effectFamily b.effectFamily
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
-- | top-level bindings. `imported` supplies the dependencies' machinery: an instance of an *imported*
-- | class wraps an imported `$Dict` (`Effect.bindEffect` = `Control.Bind.Bind$Dict { … }`), so wrapper
-- | recognition must consult the imported `identities` as well as the module's own bindings.
machineryOf :: DictMachinery -> Array (Tuple String Expr) -> DictMachinery
machineryOf imported pairs =
  let
    byKey :: Map String Stripped
    byKey = Map.fromFoldable (map (\(Tuple k e) -> Tuple k (stripLets e)) pairs)

    entries :: Array (Tuple String Stripped)
    entries = Map.toUnfoldable byKey

    identities = Set.fromFoldable
      ( Array.mapMaybe
          ( \(Tuple k s) -> case s.tail of
              Just c | isIdentityLam c -> Just k
              _ -> Nothing
          )
          entries
      )

    -- Is `f` an identity (newtype `$Dict`) wrapper — of this module, or of a dependency?
    isIdentityTop f = Set.member f identities || Set.member f imported.identities

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
    -- `effectFamily` is a module-**own** fact filled by `localFactsOf` (it needs the Rec-group
    -- structure this function's flattened `pairs` input has lost); empty here, unioned with
    -- dependencies' at `mergeMachinery`.
    { accessors, instances, identities, effectFamily: Set.empty }

-- | The lift-policy axis (the split ADR-0086 §3 explicitly sanctions, realised as a parameter rather
-- | than a baked-in choice): may a *foreign-key* `AtomVar` impl (not in gkeys) be lifted to a call
-- | site? Kept parametric even though only `intrinsicLift` remains in production (the LLVM bridge's
-- | `noForeignLift` went with the bridge, ADR-0104 §3) — the policy is a call-site property, not a
-- | pass property.
type ForeignLift = String -> Boolean

-- | The **optimiser** policy: lift **intrinsic** foreign keys. Safe there — and only there — because
-- | the NbE inliner's intrinsic saturation runs right after in the same `optimizeModule` pass,
-- | collapsing a lifted saturated `Purvasm.Int.add` call to its primop (an unsaturated one stays an `AtomVar` the
-- | VM's link resolver materialises; the same class as the instance-record field references the raw
-- | ANF already carries). The intrinsic arm is the B2 (pre-link, per-module) analogue of what boot's
-- | whole-program `dict_elim.ml` gets from running *after* link (the resolver has materialised these
-- | as spine bindings, so `Hashtbl.mem tbl` accepts them); without it every `ulib` instance impl
-- | (`{ add: Purvasm.Int.add, … }`) is unliftable and `DictElim` silently does nothing on overlaid
-- | programs. Deliberately **intrinsic-only, not the whole resolver ladder** (`isJust <<< intrinsicPrim`):
-- | a *structural* foreign (`Effect.bindE`) is GER-owned (ADR-0099 — `Impurify` lowers its dispatch to a
-- | `CPerform`) and is materialised as a guest-term gdef by both backends (native `synthForeignGdefs`, VM
-- | link resolver), so folding it into the dispatch *here* — bypassing that lowering — would be wrong; the
-- | dispatch is left standing for GER.
intrinsicLift :: ForeignLift
intrinsicLift = isJust <<< intrinsicPrim

-- | An impl is safe to lift to a call site iff it is in scope everywhere **and lowerable by the
-- | consumer of this pass's output**: a literal or (resolved) foreign, a reference to a top-level
-- | binding (a program-wide global key), or a foreign-key reference the caller's `ForeignLift` policy
-- | accepts (see `intrinsicLift`).
liftable :: ForeignLift -> Set String -> Atom -> Boolean
liftable foreignLift gkeys = case _ of
  AtomLit _ -> true
  AtomForeign _ -> true
  AtomVar k -> Set.member k gkeys || foreignLift k

-- | Rewrite one binding body: collapse every saturated `accessor dict …` whose accessor and dict are
-- | known (in `machinery`) and whose impl is liftable, to `impl …`. `gkeys` is the program-wide global
-- | key set and `foreignLift` the call site's lift policy (both feed the liftable check). Recurses
-- | into `CLam`/`CIf`/`CCase` bodies.
dictElimExpr :: ForeignLift -> Set String -> DictMachinery -> Expr -> Expr
dictElimExpr foreignLift gkeys machinery = rwExpr
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
    if liftable foreignLift gkeys impl then Just (if Array.null rest then CAtom impl else CApp impl rest)
    else Nothing
