-- | ulib SHADOW of `argonaut-core`'s `Data.Argonaut.Parser` (ADR-0046), targeting argonaut-core
-- | 7.0.0.
-- |
-- | Upstream `jsonParser` is `runFn3 _jsonParser` over a JS `JSON.parse` foreign. Here it is the
-- | backend-agnostic `Json.Core` recursive-descent parser driven by an argonaut `Builder`, which
-- | constructs the `Data.Argonaut.Core` ADT (objects via the `ulib` `Foreign.Object`). No
-- | `foreign import` remains.
module Data.Argonaut.Parser (jsonParser) where

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Either (Either)
import Foreign.Object as Obj
import Json.Core.Parser (parse)
import Json.Core.Types (Builder)

-- | Parse a JSON string, constructing the `Json` value described by the string.
-- | To convert a string into a `Json` string, see `fromString`.
jsonParser :: String -> Either String Json
jsonParser = parse argonautBuilder

argonautBuilder :: Builder Json
argonautBuilder =
  { jnull: jsonNull
  , jboolean: fromBoolean
  , jnumber: fromNumber
  , jstring: fromString
  , jarray: fromArray
  , jobject: \kvs -> fromObject (Obj.fromFoldable kvs)
  }
