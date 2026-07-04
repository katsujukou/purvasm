-- | Per-module separate-compilation artifacts (ADR-0033), byte-identical to boot's
-- | `.pvmo`/`.pvmi`:
-- |
-- |   * `.pmo` (`ModuleArtifact`) — the compiled object: binding groups as bytecode
-- |     `Gdef`s, each with the keys it defines and its cross-binding dependencies (for
-- |     link-time reachability DCE).
-- |   * `.pmi` (`Interface`) — the public export surface (sorted, with each export's
-- |     kind/arity) plus an MD5 content `hash`, the unit of recompilation avoidance.
-- |
-- | Reuses `Image`'s `Gdef`/bytecode JSON encoding and `Util.MD5` for the hash.
module Purvasm.Compiler.Bytecode.Artifact where

import Prelude

import Data.Array (concatMap, length, mapMaybe, sortWith)
import Data.Map as Map
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.Bytecode.Codegen (Gdef(..))
import Purvasm.Compiler.Bytecode.Image (Json(..), formatVersion, gdefToJson, stringify, strs)
import Purvasm.Compiler.Util.MD5 (md5Hex)

-- | A linkable binding group: the keys it defines, the keys it references (deps, for
-- | reachability), and its members as bytecode definitions. A recursive group is atomic.
-- | `recursive` is carried from the bind site (ADR-0077): a recursive lambda member is
-- | still a `Gfun`, and a singleton self-recursive group is indistinguishable from a
-- | plain one, so rec-membership cannot be re-derived from the members.
type Group =
  { keys :: Array String
  , deps :: Array String
  , members :: Array (String /\ Gdef)
  , recursive :: Boolean
  }

type ModuleArtifact =
  { name :: String
  , imports :: Array String
  , exports :: Array String
  , groups :: Array Group
  }

-- | An exported value's link/optimisation-relevant shape: a non-recursive function of an
-- | arity, a recursive-group function member of an arity (ADR-0077 — a native caller
-- | enters it through the force-cell path, not with the env sentinel), a strict CAF, or
-- | a recursive-group value.
data ExportKind = Efn Int | Erecfn Int | Ecaf | Erec

type Interface =
  { ifaceName :: String
  , exports :: Array (String /\ ExportKind) -- sorted by key
  , ifaceImports :: Array String
  , hash :: String
  }

kindOfGdef :: Boolean -> Gdef -> ExportKind
kindOfGdef recursive = case _ of
  Gfun ps _ -> if recursive then Erecfn (length ps) else Efn (length ps)
  Gcaf _ -> Ecaf
  Grec _ -> Erec

kindToTag :: ExportKind -> String
kindToTag = case _ of
  Efn n -> "fn" <> show n
  Erecfn n -> "recfn" <> show n
  Ecaf -> "caf"
  Erec -> "rec"

-- | The interface of a compiled module: its *public* exports paired with each one's kind,
-- | sorted by key, and an MD5 hash over `name:kind` lines so a dependent-observable change
-- | (e.g. an exported function's arity) moves the hash while a private edit does not.
interfaceOf :: ModuleArtifact -> Interface
interfaceOf a =
  let
    defs = Map.fromFoldable
      (concatMap (\g -> map (\(k /\ gd) -> k /\ (g.recursive /\ gd)) g.members) a.groups)
    exports = sortWith (\(k /\ _) -> k)
      ( mapMaybe (\k -> map (\(r /\ gd) -> k /\ kindOfGdef r gd) (Map.lookup k defs))
          a.exports
      )
    surface = map (\(k /\ kd) -> k <> ":" <> kindToTag kd) exports
  in
    { ifaceName: a.name
    , exports
    , ifaceImports: a.imports
    , hash: md5Hex (joinWith "\n" surface)
    }

-- --- serialization (.pmo / .pmi) ----------------------------------------------------

groupToJson :: Group -> Json
groupToJson g = JObj
  [ "keys" /\ strs g.keys
  , "deps" /\ strs g.deps
  , "recursive" /\ JBool g.recursive
  , "members" /\ JArr (map (\(n /\ gd) -> JArr [ JStr n, gdefToJson gd ]) g.members)
  ]

moduleToJson :: ModuleArtifact -> Json
moduleToJson a = JObj
  [ "version" /\ JInt formatVersion
  , "name" /\ JStr a.name
  , "imports" /\ strs a.imports
  , "exports" /\ strs a.exports
  , "groups" /\ JArr (map groupToJson a.groups)
  ]

kindToJson :: ExportKind -> Json
kindToJson = case _ of
  Efn n -> JArr [ JStr "fn", JInt n ]
  Erecfn n -> JArr [ JStr "recfn", JInt n ]
  Ecaf -> JStr "caf"
  Erec -> JStr "rec"

interfaceToJson :: Interface -> Json
interfaceToJson i = JObj
  [ "version" /\ JInt formatVersion
  , "name" /\ JStr i.ifaceName
  , "exports" /\ JArr (map (\(k /\ kd) -> JArr [ JStr k, kindToJson kd ]) i.exports)
  , "imports" /\ strs i.ifaceImports
  , "hash" /\ JStr i.hash
  ]

moduleToString :: ModuleArtifact -> String
moduleToString = stringify <<< moduleToJson

interfaceToString :: Interface -> String
interfaceToString = stringify <<< interfaceToJson
