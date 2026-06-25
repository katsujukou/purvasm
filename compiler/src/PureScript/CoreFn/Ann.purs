module PureScript.CoreFn.Ann where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import PureScript.CoreFn.Names (Ident)



-- | A 1-based source position `{ line, column }`.
type SourcePos = { line :: Int, column :: Int }

-- | A source span. Synthetic nodes use `{ start: {0,0}, end: {0,0} }`.
type SourceSpan = { start :: SourcePos, end :: SourcePos }

-- | Whether a data constructor belongs to a single-constructor (product) or
-- | multi-constructor (sum) type.
data ConstructorType = ProductType | SumType

derive instance genericConstructorType :: Generic ConstructorType _
derive instance eqConstructorType :: Eq ConstructorType
instance showConstructorType :: Show ConstructorType where
  show = genericShow

-- | Compiler-attached metadata on a node, guiding code generation.
data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  | IsSyntheticApp

derive instance genericMeta :: Generic Meta _
derive instance eqMeta :: Eq Meta
instance showMeta :: Show Meta where
  show = genericShow

-- | Per-node annotation: where it came from, any compiler metadata, and the
-- | type reconstructed by the retyper.
type Ann = { span :: SourceSpan, meta :: Maybe Meta }

