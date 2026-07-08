-- | The `ANF → Pmi` path: compute a module's `.pmi` **directly from its native ANF gdefs** plus its
-- | CoreFn interface (name/imports/exports) — never routing through the `.pmo` (`ANF → Pmo → Pmi` was an
-- | accident of the bytecode path; the `.pmi` is a sibling of the `.pmo`/`.ll` off the ANF). The export
-- | surface (each public export's `ExportKind`) is a
-- | function of the ANF classification the native gdefs already carry, and the hash is a function of that
-- | surface (`Bytecode.Artifact.interfaceFromExports`), so this is byte-identical to `interfaceOf` over the
-- | same source.
-- |
-- | The native `Gdef → ExportKind` mapping mirrors boot's bytecode `kindOfGdef` exactly:
-- | a `Gfun` is `Efn arity`; a `Gcaf` is `Ecaf`; a `Grec` member is `Erecfn arity` when its body is a
-- | lambda (`Ret (CLam …)`) and `Erec` otherwise (a recursive-group *value* member — e.g. a mutually
-- | recursive instance dictionary like `Data.List.Types.applyList`, which boot tags `rec`, not `caf`).
module Purvasm.Compiler.Backend.LLVM.Interface
  ( interfaceOfAnf
  , gdefKindMap
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Bytecode.Artifact (ExportKind(..), Interface, interfaceFromExports)
import Purvasm.Compiler.CESK.Translate (nameKey, qualifiedKey)
import Purvasm.Compiler.MiddleEnd.ANF (CExpr(..), Expr(..))

-- | Each defined key → its `ExportKind`, read off the native gdefs' ANF classification. A `Grec`'s
-- | members are keyed individually; a member whose body is `Ret (CLam ps _)` is a recursive function
-- | (`Erecfn`), any other member body is a recursive-group value (`Erec`).
gdefKindMap :: Array Gdef -> Map String ExportKind
gdefKindMap = foldl over Map.empty
  where
  over acc = case _ of
    Gfun k ps _ -> Map.insert k (Efn (Array.length ps)) acc
    Gcaf k _ -> Map.insert k Ecaf acc
    Grec ms -> foldl overMember acc ms

  overMember acc (Tuple k e) = Map.insert k (memberKind e) acc

  memberKind = case _ of
    Ret (CLam ps _) -> Erecfn (Array.length ps)
    _ -> Erec

-- | A module's `.pmi` computed from its ANF gdefs and CoreFn interface: intersect the public exports
-- | (qualified) with the keys the gdefs actually define, pair each with its `ExportKind`, and hash the
-- | surface. Byte-identical to `interfaceOf (compileModule m)` (both route through `interfaceFromExports`).
interfaceOfAnf :: CF.Module -> Array Gdef -> Interface
interfaceOfAnf m gdefs =
  let
    defs = gdefKindMap gdefs
    exports = Array.mapMaybe (\k -> map (Tuple k) (Map.lookup k defs))
      (map (qualifiedKey m.name) m.exports)
  in
    interfaceFromExports
      { name: nameKey m.name
      , imports: map (\i -> nameKey i.moduleName) m.imports
      , exports
      }
