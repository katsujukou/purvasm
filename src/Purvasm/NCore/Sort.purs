module Purvasm.NCore.Sort where

import Prelude

import Data.Array (foldMap)
import Data.Array as Arrary
import Data.Array as Array
import Data.Foldable (fold, foldr)
import Data.HashGraph (HashGraph)
import Data.HashGraph as HG
import Data.HashMap as HM
import Data.HashSet as HS
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.NCore.Syntax (Declaration, NCore(..), ToplevelDecl(..))
import Purvasm.Primitives (Primitive(..))
import Purvasm.Types (Global(..), Ident, ModuleName)

sortDecls :: ModuleName -> Array Declaration -> Array ToplevelDecl
sortDecls moduleName decls = topsort [] callGraph
  where
  topsort sorted graph
    | HG.isEmpty graph = sorted
    | otherwise = case HG.terminalVertices graph of
        vertices
          | not (HS.isEmpty vertices) ->
              topsort (addToSorted (HS.toArray vertices) sorted) (HG.filter (\vtx _ -> not (HS.member vtx vertices)) graph)
          | otherwise -> do
              let
                squashedGraph = HG.squashCycle1 graph
                  # HG.dropEdgeBy (\from to -> from == to)
              topsort sorted squashedGraph

  addToSorted :: Array (Array Ident) -> Array ToplevelDecl -> Array ToplevelDecl
  addToSorted idents sorted = Arrary.foldl addLoop sorted idents
    where
    addLoop sorted' idents' = case traverse (\ident -> { name: ident, lambda: _ } <$> HM.lookup ident declsMap) idents' of
      Nothing -> unsafeCrashWith "addToSorted: Impossible!"
      Just decls' -> Array.snoc sorted' (toToplevelDecl decls')

  toToplevelDecl :: Array Declaration -> ToplevelDecl
  toToplevelDecl = case _ of
    [] -> unsafeCrashWith "sortDecls: Empty declaration group is Impossible!"
    [ decl ] -> DeclNonRec decl
    declGrp -> DeclRec declGrp

  callGraph :: HashGraph (Array Ident)
  callGraph =
    let
      buildGraph graph = Array.uncons >>> case _ of
        Nothing -> graph
        Just { head: { name, lambda }, tail: rest } ->
          let
            calleeIdents = collectCallee name lambda
          in
            buildGraph (foldr (HG.addEdge [ name ]) graph calleeIdents) rest
    in
      buildGraph (HG.discrete (Array.singleton <<< _.name <$> decls)) decls

  declsMap = HM.fromArrayBy _.name _.lambda decls

  collectCallee self = go
    where
    go = case _ of
      NCApply f args -> go f <> foldMap go args
      NCConditional cond tbl -> go cond <> foldMap (go <<< snd) tbl
      NCFunction _ body -> go body
      NCifthenelse cond ifSo notSo -> fold [ go cond, go ifSo, go notSo ]
      NClet binds body -> foldMap go binds <> go body
      NCletrec binds body -> foldMap go binds <> go body
      NCStaticHandle e1 e2 -> go e1 <> go e2
      NCSwitch head tbl -> go head <> foldMap (go <<< snd) tbl
      NCPrim prim []
        | PGetGlobal gloname <- prim
        , Global { modname, ident } <- gloname
        , modname == moduleName
        , ident /= self -> [ [ ident ] ]
      _ -> []