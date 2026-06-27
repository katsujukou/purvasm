-- | ulib SHADOW of `argonaut-core`'s `Data.Argonaut.Core` (ADR-0046), targeting argonaut-core
-- | 7.0.0.
-- |
-- | Upstream `Json` is `foreign import data Json` — a raw `JSON.parse` result — with its case
-- | analysis, ordering, constructors and `stringify` all in JS FFI. On this backend `Json` is a
-- | pure-PureScript ADT (kept abstract: the constructors are not exported, so the public surface
-- | is unchanged), the case/`from`/`to` family is ordinary guest code, and `stringify` delegates
-- | to the backend-agnostic `Json.Core` printer. The object case is the `ulib` `Foreign.Object`
-- | (ADR-0044). No `foreign import` remains, so `argonaut-codecs` and its consumers compile over
-- | this representation unchanged.
module Data.Argonaut.Core
  ( Json
  , caseJson
  , caseJsonNull
  , caseJsonBoolean
  , caseJsonNumber
  , caseJsonString
  , caseJsonArray
  , caseJsonObject
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , jsonNull
  , jsonTrue
  , jsonFalse
  , jsonZero
  , jsonEmptyString
  , jsonEmptyArray
  , jsonSingletonArray
  , jsonEmptyObject
  , jsonSingletonObject
  , stringify
  , stringifyWithIndent
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Obj
import Json.Core.Printer (print)
import Json.Core.Types (Eliminator)

-- | The type of JSON data. Abstract, as upstream: a pure-PureScript ADT whose object case is the
-- | `ulib` `Foreign.Object` (ADR-0044) and whose numeric case is a single `Number` (JSON has one
-- | numeric type; `Int` is recovered by the codecs).
data Json
  = JNull
  | JBoolean Boolean
  | JNumber Number
  | JString String
  | JArray (Array Json)
  | JObject (Object Json)

derive instance eqJson :: Eq Json
derive instance ordJson :: Ord Json

-- | Case analysis for `Json` values.
caseJson
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array Json -> a)
  -> (Object Json -> a)
  -> Json
  -> a
caseJson a b c d e f = case _ of
  JNull -> a unit
  JBoolean x -> b x
  JNumber x -> c x
  JString x -> d x
  JArray x -> e x
  JObject x -> f x

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was null, and a default value for all other cases.
caseJsonNull :: forall a. a -> (Unit -> a) -> Json -> a
caseJsonNull d f = caseJson f (const d) (const d) (const d) (const d) (const d)

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Boolean`, and a default value for all other cases.
caseJsonBoolean :: forall a. a -> (Boolean -> a) -> Json -> a
caseJsonBoolean d f = caseJson (const d) f (const d) (const d) (const d) (const d)

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Number`, and a default value for all other cases.
caseJsonNumber :: forall a. a -> (Number -> a) -> Json -> a
caseJsonNumber d f = caseJson (const d) (const d) f (const d) (const d) (const d)

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `String`, and a default value for all other cases.
caseJsonString :: forall a. a -> (String -> a) -> Json -> a
caseJsonString d f = caseJson (const d) (const d) (const d) f (const d) (const d)

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was a `Array Json`, and a default value for all other cases.
caseJsonArray :: forall a. a -> (Array Json -> a) -> Json -> a
caseJsonArray d f = caseJson (const d) (const d) (const d) (const d) f (const d)

-- | A simpler version of `caseJson` which accepts a callback for when the
-- | `Json` argument was an `Object`, and a default value for all other cases.
caseJsonObject :: forall a. a -> (Object Json -> a) -> Json -> a
caseJsonObject d f = caseJson (const d) (const d) (const d) (const d) (const d) f

verbJsonType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Json -> b) -> Json -> b
verbJsonType def f g = g def f

-- Tests

isJsonType :: forall a. (Boolean -> (a -> Boolean) -> Json -> Boolean) -> Json -> Boolean
isJsonType = verbJsonType false (const true)

-- | Check if the provided `Json` is the `null` value
isNull :: Json -> Boolean
isNull = isJsonType caseJsonNull

-- | Check if the provided `Json` is a `Boolean`
isBoolean :: Json -> Boolean
isBoolean = isJsonType caseJsonBoolean

-- | Check if the provided `Json` is a `Number`
isNumber :: Json -> Boolean
isNumber = isJsonType caseJsonNumber

-- | Check if the provided `Json` is a `String`
isString :: Json -> Boolean
isString = isJsonType caseJsonString

-- | Check if the provided `Json` is an `Array`
isArray :: Json -> Boolean
isArray = isJsonType caseJsonArray

-- | Check if the provided `Json` is an `Object`
isObject :: Json -> Boolean
isObject = isJsonType caseJsonObject

-- Decoding

toJsonType
  :: forall a
   . (Maybe a -> (a -> Maybe a) -> Json -> Maybe a)
  -> Json
  -> Maybe a
toJsonType = verbJsonType Nothing Just

-- | Convert `Json` to the `Unit` value if the `Json` is the null value
toNull :: Json -> Maybe Unit
toNull = toJsonType caseJsonNull

-- | Convert `Json` to a `Boolean` value, if the `Json` is a boolean.
toBoolean :: Json -> Maybe Boolean
toBoolean = toJsonType caseJsonBoolean

-- | Convert `Json` to a `Number` value, if the `Json` is a number.
toNumber :: Json -> Maybe Number
toNumber = toJsonType caseJsonNumber

-- | Convert `Json` to a `String` value, if the `Json` is a string. To write a
-- | `Json` value to a JSON string, see `stringify`.
toString :: Json -> Maybe String
toString = toJsonType caseJsonString

-- | Convert `Json` to an `Array` of `Json` values, if the `Json` is an array.
toArray :: Json -> Maybe (Array Json)
toArray = toJsonType caseJsonArray

-- | Convert `Json` to an `Object` of `Json` values, if the `Json` is an object.
toObject :: Json -> Maybe (Object Json)
toObject = toJsonType caseJsonObject

-- Encoding

-- | Construct `Json` from a `Boolean` value
fromBoolean :: Boolean -> Json
fromBoolean = JBoolean

-- | Construct `Json` from a `Number` value
fromNumber :: Number -> Json
fromNumber = JNumber

-- | Construct the `Json` representation of a `String` value.
-- | Note that this function only produces `Json` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Json` - For that
-- | purpose, you'll need to use `jsonParser`.
fromString :: String -> Json
fromString = JString

-- | Construct `Json` from an array of `Json` values
fromArray :: Array Json -> Json
fromArray = JArray

-- | Construct `Json` from an object with `Json` values
fromObject :: Object Json -> Json
fromObject = JObject

-- Defaults

-- | The JSON null value represented as `Json`
jsonNull :: Json
jsonNull = JNull

-- | The true boolean value represented as `Json`
jsonTrue :: Json
jsonTrue = fromBoolean true

-- | The false boolean value represented as `Json`
jsonFalse :: Json
jsonFalse = fromBoolean false

-- | The number zero represented as `Json`
jsonZero :: Json
jsonZero = fromNumber 0.0

-- | An empty string represented as `Json`
jsonEmptyString :: Json
jsonEmptyString = fromString ""

-- | An empty array represented as `Json`
jsonEmptyArray :: Json
jsonEmptyArray = fromArray []

-- | An empty object represented as `Json`
jsonEmptyObject :: Json
jsonEmptyObject = fromObject Obj.empty

-- | Constructs a `Json` array value containing only the provided value
jsonSingletonArray :: Json -> Json
jsonSingletonArray j = fromArray [ j ]

-- | Constructs a `Json` object value containing only the provided key and value
jsonSingletonObject :: String -> Json -> Json
jsonSingletonObject key val = fromObject (Obj.singleton key val)

-- | Converts a `Json` value to a JSON string. To retrieve a string from a `Json`
-- | string value, see `fromString`.
stringify :: Json -> String
stringify = print jsonEliminator

-- | Converts a `Json` value to a JSON string.
-- | The first `Int` argument specifies the amount of white space characters to use as indentation.
-- |
-- | This backend currently renders compactly regardless of the indent (the result is still valid
-- | JSON); pretty-printing is a documented divergence (ADR-0046).
stringifyWithIndent :: Int -> Json -> String
stringifyWithIndent _ = stringify

-- | Case analysis that exposes the object case as a neutral association array, so the shared
-- | `Json.Core` printer never sees the concrete object representation.
jsonEliminator :: Eliminator Json
jsonEliminator cases = case _ of
  JNull -> cases.onNull
  JBoolean b -> cases.onBoolean b
  JNumber n -> cases.onNumber n
  JString s -> cases.onString s
  JArray xs -> cases.onArray xs
  JObject o -> cases.onObject (Obj.toUnfoldable o)
