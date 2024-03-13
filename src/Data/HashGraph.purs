module Data.HashGraph where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet (HashSet)
import Data.HashSet as HS
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.List as L
import Data.List.NonEmpty (fold1)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

newtype HashGraph a = HashGraph (HashMap a (HashSet a))

instance Show a => Show (HashGraph a) where
  show (HashGraph g) = "(HashGraph " <> show g <> ")"

empty :: forall a. HashGraph a
empty = HashGraph $ HM.empty

-- | Build graph with no edges 
discrete :: forall a. Hashable a => Array a -> HashGraph a
discrete vtcs = HashGraph (HM.fromArrayBy identity (const HS.empty) vtcs)

-- | List all vertices in given graph
vertices :: forall a. HashGraph a -> Array a
vertices (HashGraph graph) = HM.keys graph

addVertex :: forall a. Hashable a => a -> HashGraph a -> HashGraph a
addVertex a (HashGraph g) = HashGraph (HM.insert a (HS.empty) g)

addEdge :: forall a. Hashable a => a -> a -> HashGraph a -> HashGraph a
addEdge from to g@(HashGraph graph)
  | not (HM.member to graph) = g
  | otherwise = case HM.lookup from graph of
      Nothing -> g
      Just neighbors -> HashGraph $ HM.insert from (HS.insert to neighbors) graph

deleteVertex :: forall a. Hashable a => a -> HashGraph a -> HashGraph a
deleteVertex a (HashGraph graph) = HashGraph <<< map (HS.delete a) $ HM.delete a graph

dropEdgeBy :: forall a. Hashable a => (a -> a -> Boolean) -> HashGraph a -> HashGraph a
dropEdgeBy f (HashGraph g) = HashGraph $
  mapWithIndex (\vtx adjs -> HS.filter (not <<< (f vtx)) adjs) g

terminalVertices :: forall a. Hashable a => HashGraph a -> HashSet a
terminalVertices (HashGraph graph) = HS.fromArray
  $ HM.keys
  $ HM.filter HS.isEmpty graph

isEmpty :: forall a. HashGraph a -> Boolean
isEmpty (HashGraph graph) = HM.isEmpty graph

squashCycle1 :: forall a. Semigroup a => Show a => Hashable a => HashGraph a -> HashGraph a
squashCycle1 g@(HashGraph graph) = case NEL.fromList =<< detectCycle g of
  Nothing -> g
  Just cycle
    | NEL.length cycle == 1 -> g
    | otherwise ->
        let
          cycleNodes = HS.fromFoldable cycle
          squash = HS.map (\vtx -> if vtx `HS.member` cycleNodes then squashedNode else vtx)
          squashedNode = fold1 cycle
          squashedAdjs = graph
            # HM.filterKeys (_ `HS.member` cycleNodes)
            # HM.values
            # Array.fold
            # squash
          graph' = graph
            # HM.filterKeys (not <<< (_ `HS.member` cycleNodes))
            # HM.mapMaybeWithKey
                ( \vtx adjs ->
                    if vtx `HS.member` cycleNodes then Nothing
                    else Just (squash adjs)
                )
            # HM.insert squashedNode squashedAdjs
        in
          HashGraph graph'

-- | Filter vertices by applying predicate which accepts vertex and its adjacents.
-- | Edges 
filter :: forall a. Hashable a => (a -> HashSet a -> Boolean) -> HashGraph a -> HashGraph a
filter f (HashGraph graph) =
  let
    graph' = HM.filterWithKey f graph
    livingNodes = HS.fromArray $ HM.keys graph'
  in
    graph'
      <#> (HS.filter (_ `HS.member` livingNodes))
      # HashGraph

detectCycle :: forall a. Hashable a => HashGraph a -> Maybe (List a)
detectCycle (HashGraph graph) = loop HS.empty (HM.keys graph)
  where
  loop visited = Array.uncons >>> case _ of
    Nothing -> Nothing
    Just { head, tail }
      | head `HS.member` visited -> loop visited tail
      | otherwise ->
          case dfs { recStack: L.Nil, finished: HS.empty, seen: HS.empty } head of
            Left cycle -> Just cycle
            Right { finished } -> loop (HS.union finished visited) tail

  dfs :: _ -> a -> Either (List a) _
  dfs state v
    | v `HS.member` state.finished = Right state
    -- Visiting already seen vertex means the path is cyclic! 
    | v `HS.member` state.seen =
        Left $ L.Cons v $ L.takeWhile (_ /= v) state.recStack
    | otherwise =
        -- Entering vertex `v`
        case HM.lookup v graph of
          Nothing -> unsafeCrashWith "detectCycle: Impossible!"
          Just neighbors ->
            -- Mark vertex `v` as seen.
            let
              state' = state
                { recStack = v `L.Cons` state.recStack
                , seen = HS.insert v state.seen
                }
            in
              -- Start recursion for each neighbor.
              case foldM dfs state' neighbors of
                Left cycle -> Left cycle
                -- Mark vertex `v` as finised.
                Right finState -> Right $ finState
                  { finished = HS.insert v finState.finished
                  , recStack = L.drop 1 finState.recStack
                  }

