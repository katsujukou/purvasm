-- | Separate compilation (ADR-0033): compile one CoreFn module, in isolation, into a
-- | bytecode `ModuleArtifact` (its `.pmo`; the `.pmi` follows via `Artifact.interfaceOf`).
-- | Cross-module and foreign references are left as qualified names, resolved at link/run.
-- | Each binding becomes a linkable group — its right-hand side lowered (CoreFn → CESK →
-- | ANF → `Gdef`), its free variables recorded as dependencies. Ported from boot's
-- | `Pvm.Compile`.
module Purvasm.Compiler.Compile (compileModule, compileModuleWith, groupOfBind) where

import Prelude

import Data.Array (concatMap, filter) as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import PureScript.CoreFn.Expr (Bind(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import PureScript.CoreFn.Names (Ident) as CF
import Purvasm.Compiler.Bytecode.Artifact (Group, ModuleArtifact)
import Purvasm.Compiler.Bytecode.Codegen (gdefOfExpr)
import Purvasm.Compiler.CESK.FreeVars (freeVarsSet)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey, translExpr)
import Purvasm.Compiler.Ffi (intrinsicPrim)
import Purvasm.Compiler.MiddleEnd.ANF (Expr) as Anf
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Purvasm.Compiler.MiddleEnd.Optimizer.Simplify as Simplify

-- | The `--opt` optimiser pipeline over normalised ANF. `false` = `--no-opt` — the identity, so
-- | the emitted `.pmo` stays byte-identical to boot (ADR-0082 §2); `true` = `--opt`. The pipeline
-- | grows as passes land (Dbe, EffectAnalysis, caller-homed specialization, the NbE inliner);
-- | today it is copy-propagation + small-callee inlining (ADR-0028).
optimizeAnf :: Boolean -> Anf.Expr -> Anf.Expr
optimizeAnf false = identity
optimizeAnf true = Simplify.run intrinsicPrim Map.empty

-- | One CoreFn binding → a linkable group: lower each right-hand side, record its free
-- | variables as (sorted) dependencies, and classify its ANF into a `Gdef`. A recursive
-- | group is kept atomic.
groupOfBind :: Boolean -> (CF.Ident -> String) -> CF.Bind -> Group
groupOfBind opt key = case _ of
  CF.NonRec _ id e ->
    let
      t = translExpr e
      k = key id
    in
      { keys: [ k ]
      , deps: Set.toUnfoldable (freeVarsSet t)
      , members: [ k /\ gdefOfExpr false (optimizeAnf opt (normalize t)) ]
      , recursive: false
      }
  CF.Rec rbs ->
    let
      pairs = rbs <#> \rb -> key rb.ident /\ translExpr rb.expr
      deps = Set.toUnfoldable (foldl (\acc (_ /\ t) -> Set.union acc (freeVarsSet t)) Set.empty pairs)
    in
      { keys: map (\(k /\ _) -> k) pairs
      , deps
      , members: pairs <#> \(k /\ t) -> k /\ gdefOfExpr true (optimizeAnf opt (normalize t))
      , recursive: true
      }

-- | Compile a module to its `.pmo` artifact. The `exports` are its *public* value exports
-- | (CoreFn `exports`) intersected with the keys it actually defines, so private
-- | compiler-synthesised bindings stay in the object but off the public surface.
-- | Compile a module with the optimiser gated by `opt` (`true` = `--opt`, `false` = `--no-opt`,
-- | byte-identical to boot).
compileModuleWith :: Boolean -> CF.Module -> ModuleArtifact
compileModuleWith opt m =
  let
    key = qualifiedKey m.name
    groups = map (groupOfBind opt key) m.decls
    defined = Set.fromFoldable (Array.concatMap _.keys groups)
  in
    { name: nameKey m.name
    , imports: map (\i -> nameKey i.moduleName) m.imports
    , exports: Array.filter (\k -> Set.member k defined) (map key m.exports)
    , groups
    }

-- | The `--no-opt` compile (byte-identical to boot):
-- | the export-surface / byte-identity callers and tests use this. 
-- | `--opt` builds call `compileModuleWith true`.
compileModuleNoOpt :: CF.Module -> ModuleArtifact
compileModuleNoOpt = compileModuleWith false

-- | An alias for `compileModuleNoOpt` for keep backwards compatibility.
compileModule :: CF.Module -> ModuleArtifact
compileModule = compileModuleNoOpt