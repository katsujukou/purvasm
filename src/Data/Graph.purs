module Data.Graph
  ( Graph
  , addEdge
  , addVertex
  , addVertexWithOutgoingEdges
  , addVertices
  , deleteEdge
  , deleteVertex
  , empty
  , insert
  , member
  , singleton
  , size
  , successors
  , topsort
  , unwrap
  , vertices
  ) where

import Prelude

import Control.Monad.ST.Internal as ST
import Control.Monad.ST.Internal as STRef
import Data.Array.ST as STArray
import Data.Foldable (foldl, for_, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Safe.Coerce (coerce)

newtype Graph a = Graph (Map a (Set a))

instance Show a => Show (Graph a) where
  show (Graph g) = "(Graph " <> show g <> ")"

unwrap :: forall a. Graph a -> Map a (Set a)
unwrap = coerce

empty :: forall a. Graph a
empty = Graph (Map.empty)

singleton :: forall a. a -> Graph a
singleton a = Graph (Map.singleton a Set.empty)

vertices :: forall a. Graph a -> Set a
vertices = unwrap >>> Map.keys

successors :: forall a. Ord a => a -> Graph a -> Set a
successors a = unwrap >>> Map.lookup a >>> maybe Set.empty identity

member :: forall a. Ord a => a -> Graph a -> Boolean
member vtx (Graph g) = Map.member vtx g

size :: forall a. Graph a -> Int
size = unwrap >>> Map.size

insert :: forall a. Ord a => a -> { incoming :: Set a, outgoing :: Set a } -> Graph a -> Graph a
insert vtx { incoming, outgoing } (Graph g) = Graph do
  let
    outgoing' = Set.intersection outgoing (Set.insert vtx (Map.keys g))
    newGraph =
      if Map.member vtx g then Map.update (Just <<< Set.union outgoing') vtx g
      else Map.insert vtx outgoing' g
  ST.run do
    out <- STRef.new newGraph
    for_ incoming \fromVtx ->
      STRef.modify (Map.update (Just <<< Set.insert vtx) fromVtx) out
    STRef.read out

addVertex :: forall a. Ord a => a -> Graph a -> Graph a
addVertex a = insert a { incoming: Set.empty, outgoing: Set.empty }

addVertices :: forall a. Ord a => Set a -> Graph a -> Graph a
addVertices vtcs = flip (foldl (\g vtx -> if vtx `member` g then g else addVertex vtx g)) vtcs

addVertexWithOutgoingEdges :: forall a. Ord a => a -> Set a -> Graph a -> Graph a
addVertexWithOutgoingEdges from tos (Graph g)
  | from `Map.member` g = Graph $ Map.update (Just <<< Set.union tos) from g
  | otherwise = Graph (Map.insert from tos g)

addEdge :: forall a. Ord a => a -> a -> Graph a -> Graph a
addEdge from to = insert from { incoming: Set.empty, outgoing: Set.singleton to }

deleteVertex :: forall a. Ord a => a -> Graph a -> Graph a
deleteVertex a = coerce <<< Map.delete a <<< map (Set.delete a) <<< coerce

deleteEdge :: forall a. Ord a => a -> a -> Graph a -> Graph a
deleteEdge from to = coerce <<< Map.update (Just <<< Set.delete to) from <<< coerce

topsort :: forall a. Ord a => Show a => Graph a -> Array (Set a)
topsort input =
  ST.run do
    graphRef <- STRef.new input
    sorted <- STArray.new
    continue <- STRef.new true
    leafNodes <- STRef.new Set.empty

    ST.while (STRef.read continue) do
      STRef.read graphRef >>= unwrap >>> traverseWithIndex_ \vtx succNodes -> do
        when (Set.isEmpty succNodes) do
          _ <- STRef.modify (Set.insert vtx) leafNodes
          -- _ <- STArray.push vtx sorted
          pure unit

      whenM (STRef.read leafNodes <#> Set.isEmpty) do
        STRef.write false continue $> unit

      STRef.read leafNodes >>= \leaves -> do
        _ <- STArray.push leaves sorted
        leaves # traverse_ \vtx -> do
          STRef.modify (deleteVertex vtx) graphRef

      void $ STRef.write Set.empty leafNodes

    STArray.freeze sorted