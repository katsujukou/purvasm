-- | The bytecode linker (ADR-0033): merge per-module artifacts into a runnable `Image`,
-- | keeping only the definitions reachable from the entry (reachability DCE, ADR-0021).
-- | Globals are name-keyed, so linking is a name-graph merge — no relocation. Ported from
-- | boot's `Pvm.Plink`.
-- |
-- | The `Resolver` supplies a binding term for a resolvable foreign (the FFI ladder): a
-- | *guest-term* result is compiled here into a shared runtime definition; a *native*
-- | result (`TmForeign`) is left out and resolved at run through the host.
module Purvasm.Compiler.Link
  ( Resolver
  , link
  , topo
  ) where

import Prelude

import Control.Monad.State (State, execState, get, gets, modify_)
import Data.Array (concatMap, filter, fromFoldable, head)
import Data.Foldable (foldl, for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.Bytecode.Artifact (Group, ModuleArtifact)
import Purvasm.Compiler.Bytecode.Codegen (Gdef, gdefOfExpr, program)
import Purvasm.Compiler.Bytecode.Image (Image)
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.FreeVars (freeVars)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

type Resolver = String -> Maybe Term

headKey :: Group -> String
headKey g = fromMaybe "" (head g.keys)

-- | Order artifacts so a dependency precedes its dependents (DFS over imports), then a
-- | module's own decls keep their order — a valid build order for strict CAFs.
topo :: Array ModuleArtifact -> Array ModuleArtifact
topo arts = fromFoldable (List.reverse (snd (foldl go (Set.empty /\ Nil) arts)))
  where
  byName = Map.fromFoldable (map (\a -> a.name /\ a) arts)

  -- post-order accumulated in reverse (cons is O(1), ADR-0049), reversed once above.
  go acc@(seen /\ ord) a
    | Set.member a.name seen = acc
    | otherwise =
        let
          step acc2 imp = case Map.lookup imp byName of
            Just d -> go acc2 d
            Nothing -> acc2
          seen1 /\ ord1 = foldl step (Set.insert a.name seen /\ ord) a.imports
        in
          seen1 /\ (a : ord1)

type LinkState =
  { seen :: Set String
  , reached :: Set String
  , runtime :: Map String Group
  }

link :: Array ModuleArtifact -> Resolver -> Term -> Image
link artifacts resolver mainTerm =
  { gdefs: runtimeMembers <> moduleMembers <> mainImg.gdefs
  , main: mainImg.main
  , isEffect: false -- the caller sets this per the entry's run mode
  }
  where
  moduleGroups = concatMap _.groups (topo artifacts)
  defs = Map.fromFoldable (concatMap (\g -> map (\k -> k /\ g) g.keys) moduleGroups)
  mainImg = program (normalize mainTerm)

  -- A native leaf (`TmForeign`) contributes no definition; a guest term does.
  structural k = case resolver k of
    Just (TmForeign _) -> Nothing
    Nothing -> Nothing
    Just t -> Just t

  final = execState (for_ (freeVars mainTerm) visitKey)
    { seen: Set.empty, reached: Set.empty, runtime: Map.empty }

  -- runtime (structural) defs first — sorted by key (Map order) — then the reached
  -- module defs in dependency order.
  runtimeMembers =
    concatMap _.members (map snd (Map.toUnfoldable final.runtime :: Array (String /\ Group)))
  moduleMembers =
    concatMap _.members (filter (\g -> Set.member (headKey g) final.reached) moduleGroups)

  resolveGroup :: String -> State LinkState (Maybe Group)
  resolveGroup k = case Map.lookup k defs of
    Just g -> pure (Just g)
    Nothing -> do
      rt <- gets _.runtime
      case Map.lookup k rt of
        Just g -> pure (Just g)
        Nothing -> case structural k of
          Nothing -> pure Nothing
          Just t -> do
            let
              g =
                { keys: [ k ]
                , deps: freeVars t
                , members: [ k /\ gdefOfExpr false (normalize t) ] :: Array (String /\ Gdef)
                }
            modify_ \s -> s { runtime = Map.insert k g s.runtime }
            pure (Just g)

  visitKey :: String -> State LinkState Unit
  visitKey k = do
    st <- get
    unless (Set.member k st.seen) do
      modify_ \s -> s { seen = Set.insert k s.seen }
      resolveGroup k >>= case _ of
        Nothing -> pure unit
        Just g -> do
          let gid = headKey g
          reached <- gets _.reached
          unless (Set.member gid reached) do
            modify_ \s -> s
              { reached = Set.insert gid s.reached
              , seen = foldl (flip Set.insert) s.seen g.keys
              }
            for_ g.deps visitKey
