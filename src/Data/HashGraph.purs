module Data.HashGraph
  ( HashGraph
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
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), maybe)
import Safe.Coerce (coerce)

newtype HashGraph a = Graph (HashMap a (HashSet a))

instance Show a => Show (HashGraph a) where
  show (Graph g) = "(Graph " <> show g <> ")"

unwrap :: forall a. HashGraph a -> HashMap a (HashSet a)
unwrap = coerce

empty :: forall a. HashGraph a
empty = Graph (HashMap.empty)

singleton :: forall a. Hashable a => a -> HashGraph a
singleton a = Graph (HashMap.singleton a HashSet.empty)

vertices :: forall a. HashGraph a -> Array a
vertices = unwrap >>> HashMap.keys

successors :: forall a. Hashable a => a -> HashGraph a -> HashSet a
successors a = unwrap >>> HashMap.lookup a >>> maybe HashSet.empty identity

member :: forall a. Hashable a => a -> HashGraph a -> Boolean
member vtx (Graph g) = HashMap.member vtx g

size :: forall a. HashGraph a -> Int
size = unwrap >>> HashMap.size

insert :: forall a. Hashable a => a -> { incoming :: Array a, outgoing :: Array a } -> HashGraph a -> HashGraph a
insert vtx { incoming, outgoing } (Graph g) = Graph do
  let
    outgoing' = HashSet.intersection (HashSet.fromArray outgoing) (HashSet.insert vtx (HashSet.fromArray $ HashMap.keys g))
    newGraph =
      if HashMap.member vtx g then HashMap.update (Just <<< HashSet.union outgoing') vtx g
      else HashMap.insert vtx outgoing' g
  ST.run do
    out <- STRef.new newGraph
    for_ incoming \fromVtx ->
      STRef.modify (HashMap.update (Just <<< HashSet.insert vtx) fromVtx) out
    STRef.read out

addVertex :: forall a. Hashable a => a -> HashGraph a -> HashGraph a
addVertex a = insert a { incoming: [], outgoing: [] }

addVertices :: forall a. Hashable a => Array a -> HashGraph a -> HashGraph a
addVertices vtcs = flip (foldl (\g vtx -> if vtx `member` g then g else addVertex vtx g)) vtcs

addVertexWithOutgoingEdges :: forall a. Hashable a => a -> Array a -> HashGraph a -> HashGraph a
addVertexWithOutgoingEdges from tos (Graph g)
  | from `HashMap.member` g = Graph $ HashMap.update (Just <<< HashSet.union (HashSet.fromArray tos)) from g
  | otherwise = Graph (HashMap.insert from (HashSet.fromArray tos) g)

addEdge :: forall a. Hashable a => a -> a -> HashGraph a -> HashGraph a
addEdge from to = insert from { incoming: [], outgoing: [ to ] }

deleteVertex :: forall a. Hashable a => a -> HashGraph a -> HashGraph a
deleteVertex a = coerce <<< HashMap.delete a <<< map (HashSet.delete a) <<< coerce

deleteEdge :: forall a. Hashable a => a -> a -> HashGraph a -> HashGraph a
deleteEdge from to = coerce <<< HashMap.update (Just <<< HashSet.delete to) from <<< coerce

topsort :: forall a. Hashable a => Show a => HashGraph a -> Array (Array a)
topsort input =
  ST.run do
    graphRef <- STRef.new input
    sorted <- STArray.new
    continue <- STRef.new true
    leafNodes <- STRef.new HashSet.empty

    ST.while (STRef.read continue) do
      STRef.read graphRef >>= unwrap >>> traverseWithIndex_ \vtx succNodes -> do
        when (HashSet.isEmpty succNodes) do
          _ <- STRef.modify (HashSet.insert vtx) leafNodes
          -- _ <- STArray.push vtx sorted
          pure unit

      whenM (STRef.read leafNodes <#> HashSet.isEmpty) do
        STRef.write false continue $> unit

      STRef.read leafNodes >>= \leaves -> do
        when (not $ HashSet.isEmpty leaves) do
          void $ STArray.push (HashSet.toArray leaves) sorted
        leaves # traverse_ \vtx -> do
          STRef.modify (deleteVertex vtx) graphRef

      void $ STRef.write HashSet.empty leafNodes

    STArray.freeze sorted