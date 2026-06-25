-- | CoreFn literals (ADR-0014), polymorphic in the sub-term so the same shapes
-- | serve both expression literals ([expr literal]) and pattern literals
-- | ([binder literal]). [LitChar] holds a Unicode code point; lowering folds Char
-- | into Int (ADR-0006).
module PureScript.CoreFn.Literal where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)

data Literal a
  = LitInt Int
  | LitNumber Number
  | LitString String
  | LitChar Char
  | LitBoolean Boolean
  | LitArray (Array a)
  | LitObject (Array (Tuple String a))

derive instance genericLiteral :: Generic (Literal a) _
derive instance eqLiteral :: Eq a => Eq (Literal a)
instance showLiteral :: Show a => Show (Literal a) where
  show l = genericShow l

