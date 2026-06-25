-- | CoreFn names, faithful to PureScript's [Language.PureScript.Names] via the
-- | verified [CoreFn.purs] reference (ADR-0014). A [Qualified] keeps only the
-- | optional module name; the binding-site [sourcePos] the JSON carries is dropped.
module PureScript.CoreFn.Names where

import Prelude

import Data.Filterable (maybeBool)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)

-- | A module name as its dot-separated parts, e.g. `["Data", "Maybe"]`.
type ModuleName = Array String

toModuleName :: String -> Maybe ModuleName
toModuleName = Str.split (Pattern ".") >>> traverse (maybeBool (Re.test moduleNamePartRegex))
  where
  moduleNamePartRegex = unsafeRegex """[A-Z][a-zA-Z0-9_]*""" unicode

-- | An identifier (value-level name).
type Ident = String

-- | A constructor or type name (`ProperName` in the compiler).
type ProperName = String

-- | A name that is either qualified by the module it came from
-- | (`Just`), or local to the current scope (`Nothing`).
data Qualified a = Qualified (Maybe ModuleName) a

derive instance genericQualified :: Generic (Qualified a) _
derive instance eqQualified :: Eq a => Eq (Qualified a)
instance showQualified :: Show a => Show (Qualified a) where
  show q = genericShow q

