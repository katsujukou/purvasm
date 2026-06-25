-- | Separate compilation (ADR-0033): compile one CoreFn module, in isolation, into a
-- | bytecode `ModuleArtifact` (its `.pmo`; the `.pmi` follows via `Artifact.interfaceOf`).
-- | Cross-module and foreign references are left as qualified names, resolved at link/run.
-- | Each binding becomes a linkable group — its right-hand side lowered (CoreFn → CESK →
-- | ANF → `Gdef`), its free variables recorded as dependencies. Ported from boot's
-- | `Pvm.Compile`.
module Purvasm.Compiler.Compile (compileModule, groupOfBind) where

import Prelude

import Data.Array (concatMap, filter) as Array
import Data.Foldable (foldl)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CoreFn.Expr (Bind(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import PureScript.CoreFn.Names (Ident) as CF
import Purvasm.Compiler.Bytecode.Artifact (Group, ModuleArtifact)
import Purvasm.Compiler.Bytecode.Codegen (gdefOfExpr)
import Purvasm.Compiler.CESK.FreeVars (freeVarsSet)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey, translExpr)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)

-- | One CoreFn binding → a linkable group: lower each right-hand side, record its free
-- | variables as (sorted) dependencies, and classify its ANF into a `Gdef`. A recursive
-- | group is kept atomic.
groupOfBind :: (CF.Ident -> String) -> CF.Bind -> Group
groupOfBind key = case _ of
  CF.NonRec _ id e ->
    let
      t = translExpr e
      k = key id
    in
      { keys: [ k ]
      , deps: Set.toUnfoldable (freeVarsSet t)
      , members: [ k /\ gdefOfExpr false (normalize t) ]
      }
  CF.Rec rbs ->
    let
      pairs = rbs <#> \rb -> key rb.ident /\ translExpr rb.expr
      deps = Set.toUnfoldable (foldl (\acc (_ /\ t) -> Set.union acc (freeVarsSet t)) Set.empty pairs)
    in
      { keys: map (\(k /\ _) -> k) pairs
      , deps
      , members: pairs <#> \(k /\ t) -> k /\ gdefOfExpr true (normalize t)
      }

-- | Compile a module to its `.pmo` artifact. The `exports` are its *public* value exports
-- | (CoreFn `exports`) intersected with the keys it actually defines, so private
-- | compiler-synthesised bindings stay in the object but off the public surface.
compileModule :: CF.Module -> ModuleArtifact
compileModule m =
  let
    key = qualifiedKey m.name
    groups = map (groupOfBind key) m.decls
    defined = Set.fromFoldable (Array.concatMap _.keys groups)
  in
    { name: nameKey m.name
    , imports: map (\i -> nameKey i.moduleName) m.imports
    , exports: Array.filter (\k -> Set.member k defined) (map key m.exports)
    , groups
    }
