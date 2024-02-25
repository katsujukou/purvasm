module PureScript.CoreFn.Utils where

import Data.String.Regex (Regex)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)

typeClassConstructorRegex :: Regex
typeClassConstructorRegex = unsafeRegex """^([A-Z][a-zA-Z0-9'_]*)\$Dict$""" unicode
