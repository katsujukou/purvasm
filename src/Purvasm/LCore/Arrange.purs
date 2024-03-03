module Purvasm.LCore.Arrange where

import Prelude

import Data.Array (elem, filter, fold)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet (HashSet, member)
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.LCore.Syntax (LCore(..), Program(..))
import Purvasm.Primitives (Primitive(..))
import Purvasm.Types (Global(..), Ident)

type Graph a = HashMap a (HashSet a)

-- emptyGraph :: forall a. Graph a
-- emptyGraph = HM.empty

-- addVertex :: forall a. Hashable a => a -> Graph a -> Graph a
-- addVertex a = HM.insert a (S.empty)

-- addEdge :: forall a. Hashable a => a -> a -> Graph a -> Graph a
-- addEdge from to graph
--   | not (HM.member to graph) = graph
--   | otherwise = case HM.lookup from graph of
--       Nothing -> graph
--       Just neighbors -> HM.insert from (S.insert to neighbors) graph

-- graph :: Graph Int
-- graph = emptyGraph
--   # flip (foldr addVertex) (1 .. 12)
--   # addEdge 1 2
--   # addEdge 2 3
--   # addEdge 3 4
--   # addEdge 4 5
--   # addEdge 5 6
--   # addEdge 6 7
--   # addEdge 7 8
--   # addEdge 7 9
--   # addEdge 5 10
--   # addEdge 10 9
--   # addEdge 5 11
--   # addEdge 11 12
--   # addEdge 12 3

detectCycle :: forall a. Hashable a => Graph a -> Maybe (List a)
detectCycle graph = loop S.empty (HM.keys graph)
  where
  loop visited = Array.uncons >>> case _ of
    Nothing -> Nothing
    Just { head, tail }
      | head `S.member` visited -> loop visited tail
      | otherwise ->
          case dfs { recStack: L.Nil, finished: S.empty, seen: S.empty } head of
            Left cycle -> Just cycle
            Right { finished } -> loop (S.union finished visited) tail

  dfs :: _ -> a -> Either (List a) _
  dfs state v
    | v `S.member` state.finished = Right state
    -- Visiting already seen vertex means the path is cyclic! 
    | v `S.member` state.seen =
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
                , seen = S.insert v state.seen
                }
            in
              -- Start recursion for each neighbor.
              case foldM dfs state' neighbors of
                Left cycle -> Left cycle
                -- Mark vertex `v` as finised.
                Right finState -> Right $ finState
                  { finished = S.insert v finState.finished
                  , recStack = L.drop 1 finState.recStack
                  }

arrangeDecls :: Program -> Program
arrangeDecls (Program { name: moduleName, decls, foreigns }) = do
  let
    static = decls # filter staticDecl
    dynamic = decls # filter (\{ name } -> not $ name `elem` (_.name <$> static))
  Program $
    { name: moduleName
    , foreigns
    , static: static
    , decls: sortDecl dynamic
        <#> \ident -> case Array.find (_.name >>> (_ == ident)) dynamic of
          Just it -> it
          Nothing -> unsafeCrashWith "arragenDecls: Impossible!"
    }
  where
  staticDecl { lambda } = case lambda of
    LCConst _ -> true
    LCFunction _ _ -> true
    LCNil -> true
    LCNone -> true
    LCStaticFail -> true
    LCStaticHandle _ _ -> true
    _ -> false

  sortDecl decls' =
    let
      nodes = S.fromArray $ _.name <$> decls'
      depGraph = HM.fromArray $
        decls' <#> \{ name, lambda } -> name /\ (collectRef name nodes lambda)
    in
      go HM.empty [] depGraph
    where
    go :: HashMap Ident (HashSet Ident) -> Array (Array Ident) -> Graph Ident -> Array Ident
    go recMap sorted graph
      | HM.isEmpty graph = Array.fold <<< map (recoverRec recMap) $ fold $ sorted
      | otherwise =
          let
            leafNodes :: Array Ident
            leafNodes = HM.keys $ HM.filter S.isEmpty graph

            restGraph :: Graph Ident
            restGraph = graph
              # HM.filterKeys (not <<< (_ `Array.elem` leafNodes))
              <#> (S.filter (not <<< (_ `Array.elem` leafNodes)))
          in
            case leafNodes, restGraph of
              [], _
                | Just cycle <- detectCycle restGraph
                , Just { head: reprNode, tail: cycleRest } <- L.uncons cycle -> do
                    let
                      cycleNodes = S.fromFoldable cycle
                      shrinkedNode = S.filter (not <<< (_ `S.member` cycleNodes))
                        $ fold
                        $ HM.values
                        $ HM.filterKeys (_ `S.member` cycleNodes) restGraph
                      restGraph' = restGraph
                        # HM.filterKeys (not <<< (_ `S.member` cycleNodes))
                        # HM.insert reprNode shrinkedNode
                        <#> (S.map \vertex -> if vertex `S.member` cycleNodes then reprNode else vertex)
                      recMap' = updateRecMap recMap reprNode cycleNodes
                    go recMap' sorted restGraph'
              _, _ -> go recMap (Array.snoc sorted leafNodes) restGraph
  updateRecMap map node nodes =
    case HM.lookup node map of
      Just recNodes -> HM.insert node (recNodes `S.union` nodes) map
      Nothing ->
        let
          unionNodes = fold $ HM.values $ HM.filterKeys (_ `S.member` nodes) map
          restMap = HM.filterKeys (not <<< (_ `S.member` nodes)) map
        in
          HM.insert node (unionNodes `S.union` nodes) restMap

  recoverRec recMap ident = case HM.lookup ident recMap of
    Nothing -> [ ident ]
    Just idents -> S.toArray idents

  collectRef node nodes = go >>> S.fromFoldable
    where
    go = case _ of
      LCPrim (PGetGlobal gloname) _
        | Global { modname, ident } <- gloname
        , modname == moduleName
        , ident `member` nodes
        -- exclude itself
        , ident /= node -> [ ident ]
      LCPrim _ args -> args >>= go
      LCApply func args -> go func <> (args >>= go)
      LCFunction _ body -> go body
      LCConditional head branches -> go head <> (branches >>= snd >>> go)
      LCSwitch head table -> go head <> (table >>= snd >>> go)
      LCifthenelse cond ifSo notSo -> go cond <> go ifSo <> go notSo
      LClet vars body -> go body <> (vars >>= go)
      LCletrec vars body -> go body <> (vars >>= go)
      LCStaticHandle e1 e2 -> go e1 <> go e2
      _ -> []
