-- | Pattern-match compilation (ADR-0031, ADR-0083): turn a `case`'s scrutinees and
-- | alternatives into an explicit Maranget decision tree (`DTree`) over the scrutinee
-- | occurrences. This is the ONE shared, backend-agnostic matcher (ADR-0083): the
-- | bytecode backend (`Bytecode.Lower.Match`) lowers the same tree rather than embedding
-- | its own matcher, mirroring boot's `Middle_end.Match_compile`.
-- |
-- | The first refutable column is switched on, the matrix specialised per head and a
-- | default, so a tag/length/literal is tested at most once across all alternatives. A
-- | guard chain (ADR-0013) is evaluated at the matched leaf (`Dguard`); if every guard is
-- | false, control falls through to the rows below — recompiled against the same
-- | occurrences (the `Dguard` fall-through subtree). Occurrences — paths into the
-- | scrutinees — are fresh local names (`$dt<n>`); the backend binds them (the scrutinee
-- | atoms at the root, a projection at each extraction). The fresh-occurrence order is the
-- | byte-identity contract for the bytecode backend (ADR-0083) and is fixed here.
module Purvasm.Compiler.MiddleEnd.MatchCompile
  ( Proj(..)
  , DTree(..)
  , Arm
  , ScrutBind
  , compile
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Foldable (all, any)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.Literal (Literal)
import Purvasm.Compiler.MiddleEnd.ANF (Alt, Atom, Expr, Rhs(..))

-- | A projection from a parent occurrence to a sub-occurrence: a constructor field, an
-- | array element, or a record label.
data Proj
  = Pfield Int
  | Pelem Int
  | Precord String

derive instance Eq Proj

instance Show Proj where
  show = case _ of
    Pfield j -> "(Pfield " <> show j <> ")"
    Pelem j -> "(Pelem " <> show j <> ")"
    Precord l -> "(Precord " <> show l <> ")"

-- | An explicit decision tree over the scrutinee occurrences. A backend lowers each node
-- | to its own discriminant; the tree itself is discriminant-agnostic. Leaf right-hand
-- | sides carry raw ANF `Expr`s (`Dleaf`/`Dguard`), lowered by the backend.
data DTree
  -- No alternative matched (or every guard fell through): a stuck program.
  = Dfail String
  -- A fully-matched row: bind its variables (`(var /\ occ)`), then run the body.
  | Dleaf (Array (String /\ String)) Expr
  -- A fully-matched guarded row (ADR-0013): bind, then try each guard/body clause; if
  -- every guard is false, fall through to the subtree (the rows below).
  | Dguard (Array (String /\ String)) (Array { guard :: Expr, rhs :: Expr }) DTree
  -- Switch on a constructor tag; each arm extracts the constructor's fields before its
  -- subtree, and the default keeps the wildcard rows.
  | DswitchCtor String (Array (String /\ Arm)) DTree
  -- Switch on a scalar literal (Int/Bool/Number/String alike — ADR-0083).
  | DswitchLit String (Array (Literal /\ DTree)) DTree
  -- Switch on a `Varray`'s length (ADR-0012); each arm extracts the elements.
  | DswitchLen String (Array (Int /\ Arm)) DTree
  -- A record pattern imposes no discriminant (ADR-0012): extract the union of named
  -- labels as sub-occurrences and continue — no switch.
  | DexpandRecord String (Array (String /\ Proj)) DTree

-- | A switch arm: the sub-occurrence extractions `(sub_occ /\ proj-of-parent)` to perform
-- | on entry, then the subtree.
type Arm = { extracts :: Array (String /\ Proj), sub :: DTree }

-- | A scrutinee-occurrence binding to establish at the root: `(occ /\ atom)`.
type ScrutBind = String /\ Atom

-- --- pattern matrix ------------------------------------------------------------------

-- | A binder's "head", with the variable names it binds at this occurrence peeled off
-- | (`BVar`/`BNamed`). `Cwild` imposes no test; the others impose a discriminant.
data Core
  = Cwild
  | Clit Literal
  | Cctor String (Array Binder)
  | Carr (Array Binder)
  | Crec (Array { prop :: String, binder :: Binder })

derive instance Eq Core

peel :: Binder -> { names :: Array String, core :: Core }
peel = case _ of
  BNull -> { names: [], core: Cwild }
  BVar x -> { names: [ x ], core: Cwild }
  BNamed x inner -> let p = peel inner in p { names = Array.cons x p.names }
  BLit l -> { names: [], core: Clit l }
  BCtor tag subs -> { names: [], core: Cctor tag subs }
  BArray subs -> { names: [], core: Carr subs }
  BRecord fields -> { names: [], core: Crec fields }

-- | One clause of the pattern matrix: binders still to test (aligned to the current
-- | occurrences), variable→occurrence bindings collected on the way down, and the rhs.
type DtRow = { pats :: Array Binder, binds :: Array (String /\ String), rhs :: Rhs }

bindsOf :: Array String -> String -> Array (String /\ String)
bindsOf names occ = names <#> \n -> n /\ occ

-- | Splice `news` in for the element at index `c`; remove the element at index `c`.
replaceCol :: forall a. Array a -> Int -> Array a -> Array a
replaceCol xs c news = Array.concat (Array.mapWithIndex (\i x -> if i == c then news else [ x ]) xs)

removeCol :: forall a. Array a -> Int -> Array a
removeCol xs c = fromMaybe xs (Array.deleteAt c xs)

idx :: forall a. Array a -> Int -> a
idx xs i = unsafePartial (Array.unsafeIndex xs i)

firstRefutable :: Array Core -> Int
firstRefutable cores = fromMaybe 0 (Array.findIndex (_ /= Cwild) cores)

-- --- the tree builder ----------------------------------------------------------------

-- | The fresh-occurrence counter. Kept separate from any backend label counter (as in
-- | boot), so the occurrence names — the only part of a `case` that reaches the emitted
-- | bytecode — are fixed here regardless of how a backend numbers its labels.
type M = State Int

freshOcc :: M String
freshOcc = do
  modify_ (_ + 1)
  n <- get
  pure ("$dt" <> show n)

-- | Compile a `case`'s scrutinees and alternatives to a decision tree. Returns the
-- | scrutinee-occurrence bindings to establish at the root and the tree. The
-- | fresh-occurrence order (scrutinees first, then per-head depth-first) is the
-- | byte-identity contract (ADR-0083) and mirrors boot exactly.
compile :: Array Atom -> Array Alt -> { scrutBinds :: Array ScrutBind, tree :: DTree }
compile scruts alts = evalState build 0
  where
  build :: M { scrutBinds :: Array ScrutBind, tree :: DTree }
  build = do
    scrutBinds <- for scruts \a -> do
      o <- freshOcc
      pure (o /\ a)
    tree <- go (map fst scrutBinds)
      (alts <#> \alt -> { pats: alt.binders, binds: [], rhs: alt.result })
    pure { scrutBinds, tree }

  go :: Array String -> Array DtRow -> M DTree
  go occs rows = case Array.uncons rows of
    Nothing -> pure (Dfail "case: no matching alternative")
    Just { head: row0 } ->
      let
        cores0 = map (peel >>> _.core) row0.pats
      in
        if all (eq Cwild) cores0 then
          let
            leafBinds = row0.binds
              <> Array.concat (Array.zipWith (\occ p -> bindsOf (peel p).names occ) occs row0.pats)
          in
            case row0.rhs of
              Uncond e -> pure (Dleaf leafBinds e)
              Guarded clauses -> do
                ft <- go occs (Array.drop 1 rows)
                pure (Dguard leafBinds clauses ft)
        else
          let
            c = firstRefutable cores0
            occ_c = idx occs c
          in
            case idx cores0 c of
              Cctor _ _ -> goCtor occs rows c occ_c
              Clit _ -> goLit occs rows c occ_c
              Carr _ -> goArr occs rows c occ_c
              Crec _ -> goRec occs rows c occ_c
              Cwild -> unsafeCrashWith "compile: refutable column is wild"

  -- switch on a constructor tag; each head specialises the matrix with its fields
  goCtor :: Array String -> Array DtRow -> Int -> String -> M DTree
  goCtor occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Cctor tag subs | not (any (\(t /\ _) -> t == tag) acc) -> Array.snoc acc (tag /\ Array.length subs)
            _ -> acc
        )
        []
        rows
      defaultRows = wildRows rows c occ_c
    arms <- for heads \(tag /\ arity) -> do
      subOccs <- replicateA arity freshOcc
      let
        extracts = Array.mapWithIndex (\j o -> o /\ Pfield j) subOccs
        rows' = Array.mapMaybe
          ( \row ->
              let
                p = peel (idx row.pats c)
                binds' = row.binds <> bindsOf p.names occ_c
              in
                case p.core of
                  Cctor t subs | t == tag -> Just { pats: replaceCol row.pats c subs, binds: binds', rhs: row.rhs }
                  Cwild -> Just { pats: replaceCol row.pats c (Array.replicate arity BNull), binds: binds', rhs: row.rhs }
                  _ -> Nothing
          )
          rows
      sub <- go (replaceCol occs c subOccs) rows'
      pure (tag /\ { extracts, sub })
    default <- go (removeCol occs c) defaultRows
    pure (DswitchCtor occ_c arms default)

  -- switch on a scalar literal; literal heads have no sub-columns
  goLit :: Array String -> Array DtRow -> Int -> String -> M DTree
  goLit occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Clit l | not (any (eq l) acc) -> Array.snoc acc l
            _ -> acc
        )
        []
        rows
      occs' = removeCol occs c
      selectRows keep = Array.mapMaybe
        ( \row ->
            let
              p = peel (idx row.pats c)
            in
              if keep p.core then Just { pats: removeCol row.pats c, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
              else Nothing
        )
        rows
    arms <- for heads \l -> do
      sub <- go occs' (selectRows \core -> core == Clit l || core == Cwild)
      pure (l /\ sub)
    default <- go occs' (selectRows (eq Cwild))
    pure (DswitchLit occ_c arms default)

  -- switch on a `Varray`'s length (ADR-0012); each length head exposes its elements
  goArr :: Array String -> Array DtRow -> Int -> String -> M DTree
  goArr occs rows c occ_c = do
    let
      heads = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Carr subs | not (any (eq (Array.length subs)) acc) -> Array.snoc acc (Array.length subs)
            _ -> acc
        )
        []
        rows
      defaultRows = wildRows rows c occ_c
    arms <- for heads \arity -> do
      subOccs <- replicateA arity freshOcc
      let
        extracts = Array.mapWithIndex (\j o -> o /\ Pelem j) subOccs
        rows' = Array.mapMaybe
          ( \row ->
              let
                p = peel (idx row.pats c)
                binds' = row.binds <> bindsOf p.names occ_c
              in
                case p.core of
                  Carr subs | Array.length subs == arity -> Just { pats: replaceCol row.pats c subs, binds: binds', rhs: row.rhs }
                  Cwild -> Just { pats: replaceCol row.pats c (Array.replicate arity BNull), binds: binds', rhs: row.rhs }
                  _ -> Nothing
          )
          rows
      sub <- go (replaceCol occs c subOccs) rows'
      pure (arity /\ { extracts, sub })
    default <- go (removeCol occs c) defaultRows
    pure (DswitchLen occ_c arms default)

  -- a record pattern imposes no discriminant; expand the union of named labels as columns
  goRec :: Array String -> Array DtRow -> Int -> String -> M DTree
  goRec occs rows c occ_c = do
    let
      labels = Array.foldl
        ( \acc row -> case (peel (idx row.pats c)).core of
            Crec fields -> Array.foldl (\a f -> if any (eq f.prop) a then a else Array.snoc a f.prop) acc fields
            _ -> acc
        )
        []
        rows
    subOccs <- replicateA (Array.length labels) freshOcc
    let
      extracts = Array.zipWith (\l o -> o /\ Precord l) labels subOccs
      rows' = rows <#> \row ->
        let
          p = peel (idx row.pats c)
          subpats = case p.core of
            Crec fields -> labels <#> \l -> maybe BNull _.binder (Array.find (\f -> f.prop == l) fields)
            Cwild -> labels <#> const BNull
            _ -> unsafeCrashWith "goRec: non-record column"
        in
          { pats: replaceCol row.pats c subpats, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
    sub <- go (replaceCol occs c subOccs) rows'
    pure (DexpandRecord occ_c extracts sub)

  -- rows whose column `c` is a wildcard: keep them for the default edge, column removed
  wildRows :: Array DtRow -> Int -> String -> Array DtRow
  wildRows rows c occ_c = Array.mapMaybe
    ( \row ->
        let
          p = peel (idx row.pats c)
        in
          case p.core of
            Cwild -> Just { pats: removeCol row.pats c, binds: row.binds <> bindsOf p.names occ_c, rhs: row.rhs }
            _ -> Nothing
    )
    rows
