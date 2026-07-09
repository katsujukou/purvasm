-- | The bytecode (VM) backend as an ADR-0087 `Backend` value (`bytecodeBackend`), the ADR-0088 **(a)**
-- | scope: consume the neutral build driver's **seam-output** `AnfModule` (optimised under `--opt`,
-- | normalise-only under `--no-opt`) and emit the current boot-runnable `ModuleArtifact` (`.pmo`). boot's
-- | frozen VM runs *optimised* boot-shape bytecode exactly as it runs un-optimised, so `--opt` VM becomes
-- | the optimiser measurement field with no interpreter change; the byte-identity **gate** to boot is
-- | released (the `.pmo` `deps` now come from the ANF, not the CESK term), but the `.pmi` is unchanged
-- | (ADR-0088 §1) and stays byte-identical to boot.
-- |
-- | Reachability stays with `Link.link` in this scope (ADR-0088 §0/§4), so a decl's `deps` are its full
-- | ANF free references — free **global** vars (`fvExpr`) ∪ referenced **foreign** keys (`cfExpr`) — the
-- | superset `Link.link`'s `visitKey`/`resolveGroup` already tolerates (unknown/foreign keys resolve to no
-- | definition and are skipped). `lowerEntry` is a **placeholder**: the VM entry is the link-time `mainTerm`
-- | the CLI's `Link.link` finalisation supplies (ADR-0087 §4), not a per-module object. The decls-+-init
-- | format and `lowerEntry`-owned reachability (ADR-0088 §2/§3, the **(b)** scope) wait for an owned VM.
module Purvasm.Compiler.Backend.Bytecode
  ( bytecodeBackend
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Purvasm.Compiler (Backend, LoweredModule)
import Purvasm.Compiler.Bytecode.Artifact (Group, ModuleArtifact, interfaceOf)
import Purvasm.Compiler.Bytecode.Codegen (gdefOfExpr)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Module (Decl)

-- | The bytecode backend. `o = ModuleArtifact` (a `.pmo`); the whole-program context is `Unit` — bytecode
-- | is per-module independent, and cross-module reachability is `Link.link`'s at finalisation (ADR-0088 §0).
bytecodeBackend :: Backend Unit ModuleArtifact
bytecodeBackend =
  { context: const unit
  , interfaceOf: \_ lm -> interfaceOf (moduleArtifactOf lm)
  , lowerModule: \_ lm -> moduleArtifactOf lm
  -- The VM entry is the link-time `mainTerm` (ADR-0087 §4); there is no per-module entry object, so this
  -- is an empty placeholder that CLI finalisation ignores in favour of `Link.link` over the modules.
  , lowerEntry: \_ _ -> { name: "entry", imports: [], exports: [], groups: [] }
  }

-- | One seam-output module → its bytecode `.pmo`: classify each `Decl`'s members to bytecode `Gdef`s, and
-- | keep the public export surface (intersected with the keys the module actually defines, so private
-- | compiler-synthesised bindings stay off the surface). Mirrors `Compile.compileModuleWith`, but consuming
-- | the already-normalised/optimised `AnfModule` rather than re-translating from CoreFn.
moduleArtifactOf :: LoweredModule -> ModuleArtifact
moduleArtifactOf lm =
  let
    key = qualifiedKey lm.source.name
    groups = map groupOfDecl lm.module.decls
    defined = Set.fromFoldable (Array.concatMap _.keys groups)
  in
    { name: nameKey lm.source.name
    , imports: map (\i -> nameKey i.moduleName) lm.source.imports
    , exports: Array.filter (\k -> Set.member k defined) (map key lm.source.exports)
    , groups
    }

-- | One seam-output `Decl` → a linkable bytecode `Group`: its keys, its dependencies (the union of its
-- | members' ANF free references, for link reachability), its members as bytecode `Gdef`s, and its
-- | recursive flag (carried from the bind site — a singleton self-recursive group is indistinguishable
-- | from a plain one).
groupOfDecl :: Decl -> Group
groupOfDecl d =
  { keys: map fst d.members
  , deps: Set.toUnfoldable
      (foldl (\acc (Tuple _ e) -> Set.union acc (Set.union (fvExpr Set.empty e) (cfExpr e))) Set.empty d.members)
  , members: map (\(Tuple k e) -> Tuple k (gdefOfExpr d.recursive e)) d.members
  , recursive: d.recursive
  }
