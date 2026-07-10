-- | The backend-neutral **ANF module** the optimiser/codegen seam operates on (ADR-0086 §2): a module's
-- | name and its top-level binding groups (`Decl`s), one per CoreFn `Bind`, a recursive group kept
-- | atomic. This is the shared middle-end's product — every backend (VM/LLVM) consumes the *same*
-- | `AnfModule`, so `Gdef` classification (backend-specific) happens **after** the optimiser, in the
-- | backend, never here.
-- |
-- | `declsOfModule` is the ingestion half of the seam (CoreFn → CESK term → normalised ANF), factored out
-- | of the per-backend drivers so `translExpr → normalize` is written once; the neutral `AnfModule` it
-- | yields is what `optimizeModule` (`MiddleEnd.Optimizer`) rewrites under `--opt` — and what the
-- | backends consume as-is under `--no-opt` (the driver is the identity there, ADR-0086 Addendum).
module Purvasm.Compiler.MiddleEnd.Module
  ( Decl
  , AnfModule
  , declsOfModule
  , mapDeclBodies
  , declKeys
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\))
import PureScript.CoreFn.Expr (Bind(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey, translExpr)
import Purvasm.Compiler.MiddleEnd.ANF (Expr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

-- | A top-level binding group: its members as `key = term`, and whether it is a recursive group
-- | (carried from the CoreFn bind site — a singleton self-recursive group is indistinguishable from a
-- | plain one, so rec-membership cannot be re-derived from the members). This is the unit the backend
-- | classifies into a `Gdef` (a recursive `Decl` → one atomic `Grec`).
type Decl =
  { recursive :: Boolean
  , members :: Array (String /\ Expr)
  }

-- | A whole module's ANF: its name plus its `Decl`s in source order.
type AnfModule =
  { name :: String
  , decls :: Array Decl
  }

-- | One CoreFn module → its neutral `AnfModule`: lower each binding's right-hand side (CoreFn → CESK
-- | term → normalised ANF) into a `Decl`, keeping source order and recursive-group atomicity. Keys are
-- | qualified with the module name (`qualifiedKey`), matching every backend's key convention.
declsOfModule :: CF.Module -> AnfModule
declsOfModule m =
  { name: nameKey m.name
  , decls: map declOfBind m.decls
  }
  where
  key = qualifiedKey m.name

  declOfBind = case _ of
    CF.NonRec _ id e ->
      { recursive: false, members: [ Tuple (key id) (normalize (translExpr e)) ] }
    CF.Rec rbs ->
      { recursive: true, members: rbs <#> \rb -> Tuple (key rb.ident) (normalize (translExpr rb.expr)) }

-- | Apply an ANF-body rewrite to each member of a `Decl` (used to run a middle-end pass over a module's
-- | binding bodies).
mapDeclBodies :: (Expr -> Expr) -> Decl -> Decl
mapDeclBodies f d = d { members = map (\(Tuple k e) -> Tuple k (f e)) d.members }

-- | The top-level keys a `Decl` defines.
declKeys :: Decl -> Array String
declKeys d = map fst d.members
