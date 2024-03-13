module Purvasm.LCore.Arrange where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet (HashSet)
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.LCore.Syntax (Module)

arrangeDecls :: Module -> Module
arrangeDecls = identity
-- arrangeDecls (Module { name: moduleName, decls, foreigns }) = 
--   let
--     static = decls # filter staticDecl
--     dynamic = decls # filter (\{ name } -> not $ name `elem` (_.name <$> static))
--   Program $
--     { name: moduleName
--     , foreigns
--     , static: static
--     , decls: sortDecl dynamic
--         <#> \ident -> case Array.find (_.name >>> (_ == ident)) dynamic of
--           Just it -> it
--           Nothing -> unsafeCrashWith "arragenDecls: Impossible!"
--     }
--   where
--   staticDecl { lambda } = case lambda of
--     NCConst _ -> true
--     NCFunction _ _ -> true
--     NCNil -> true
--     NCNone -> true
--     NCStaticFail -> true
--     NCStaticHandle _ _ -> true
--     _ -> false

--   sortDecl decls' =
--     let
--       nodes = S.fromArray $ _.name <$> decls'
--       depGraph = HM.fromArray $
--         decls' <#> \{ name, lambda } -> name /\ (collectRef name nodes lambda)
--     in
--       go HM.empty [] depGraph
--     where
--     go :: HashMap Ident (HashSet Ident) -> Array (Array Ident) -> Graph Ident -> Array Ident
--     go recMap sorted graph
--       | HM.isEmpty graph = Array.fold <<< map (recoverRec recMap) $ fold $ sorted
--       | otherwise =
--           let
--             leafNodes :: Array Ident
--             leafNodes = HM.keys $ HM.filter S.isEmpty graph

--             restGraph :: Graph Ident
--             restGraph = graph
--               # HM.filterKeys (not <<< (_ `Array.elem` leafNodes))
--               <#> (S.filter (not <<< (_ `Array.elem` leafNodes)))
--           in
--             case leafNodes, restGraph of
--               [], _
--                 | Just cycle <- detectCycle restGraph
--                 , Just { head: reprNode, tail: cycleRest } <- L.uncons cycle -> do
--                     let
--                       cycleNodes = S.fromFoldable cycle
--                       shrinkedNode = S.filter (not <<< (_ `S.member` cycleNodes))
--                         $ fold
--                         $ HM.values
--                         $ HM.filterKeys (_ `S.member` cycleNodes) restGraph
--                       restGraph' = restGraph
--                         # HM.filterKeys (not <<< (_ `S.member` cycleNodes))
--                         # HM.insert reprNode shrinkedNode
--                         <#> (S.map \vertex -> if vertex `S.member` cycleNodes then reprNode else vertex)
--                       recMap' = updateRecMap recMap reprNode cycleNodes
--                     go recMap' sorted restGraph'
--               _, _ -> go recMap (Array.snoc sorted leafNodes) restGraph
--   updateRecMap map node nodes =
--     case HM.lookup node map of
--       Just recNodes -> HM.insert node (recNodes `S.union` nodes) map
--       Nothing ->
--         let
--           unionNodes = fold $ HM.values $ HM.filterKeys (_ `S.member` nodes) map
--           restMap = HM.filterKeys (not <<< (_ `S.member` nodes)) map
--         in
--           HM.insert node (unionNodes `S.union` nodes) restMap

--   recoverRec recMap ident = case HM.lookup ident recMap of
--     Nothing -> [ ident ]
--     Just idents -> S.toArray idents

--   collectRef node nodes = go >>> S.fromFoldable
--     where
--     go = case _ of
--       NCPrim (PGetGlobal gloname) _
--         | Global { modname, ident } <- gloname
--         , modname == moduleName
--         , ident `member` nodes
--         -- exclude itself
--         , ident /= node -> [ ident ]
--       NCPrim _ args -> args >>= go
--       NCApply func args -> go func <> (args >>= go)
--       NCFunction _ body -> go body
--       NCConditional head branches -> go head <> (branches >>= snd >>> go)
--       NCSwitch head table -> go head <> (table >>= snd >>> go)
--       NCifthenelse cond ifSo notSo -> go cond <> go ifSo <> go notSo
--       NClet vars body -> go body <> (vars >>= go)
--       NCletrec vars body -> go body <> (vars >>= go)
--       NCStaticHandle e1 e2 -> go e1 <> go e2
--       _ -> []
