module Purvasm.DependencyGraph where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Set (Set)
import Data.Set as Set

newtype DirectedGraph a = DirectedGraph (Map a (Set a))

derive instance Newtype (DirectedGraph a) _
instance Show a => Show (DirectedGraph a) where
  show (DirectedGraph g) = "(DirectedGraph " <> show g <> ")"

empty :: forall a. DirectedGraph a
empty = DirectedGraph (Map.empty)

singleton :: forall a. a -> DirectedGraph a
singleton = DirectedGraph <<< flip Map.singleton Set.empty

addVertex :: forall a. Ord a => a -> DirectedGraph a -> DirectedGraph a
addVertex a = wrapIso DirectedGraph (Map.insert a (Set.empty))

addEdge :: forall a. a -> a -> DirectedGraph a -> Maybe (DirectedGraph a)
addEdge from to (DirectedGraph g) = case Map.lookup from g, Map.lookup to g of
  Just vtxFrom, Just _ ->
    Map.update (Set.insert b >>> Just) from