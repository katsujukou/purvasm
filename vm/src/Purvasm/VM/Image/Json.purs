-- | The JSON tree an image is written as, and the [builder] that parses into it.
-- |
-- | A representation of its own, rather than one borrowed from the compiler: `.pvm` is an
-- | *interchange* artifact, and the VM consumes it without depending on the producer
-- | ([ADR-0110](../../../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §2 — the
-- | compiler must not gain a dependency on the VM, and the VM does not gain one on the compiler
-- | either). `Json.Core.Parser` is representation-agnostic precisely so both sides can keep their own.
-- |
-- | Small on purpose: the interesting work is decoding this tree into instructions, and that lives
-- | next door where it can be read as one piece.
module Purvasm.VM.Image.Json
  ( Json(..)
  , entry
  , parseJson
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Json.Core.Parser as Parser
import Json.Core.Types (Builder)

data Json
  = JNull
  | JBool Boolean
  | JNumber Number
  | JString String
  | JArray (Array Json)
  | JObject (Array (Tuple String Json))

derive instance eqJson :: Eq Json

builder :: Builder Json
builder =
  { jnull: JNull
  , jboolean: JBool
  , jnumber: JNumber
  , jstring: JString
  , jarray: JArray
  , jobject: JObject
  }

parseJson :: String -> Either String Json
parseJson = Parser.parse builder

-- | An object's field, by name.
entry :: String -> Array (Tuple String Json) -> Maybe Json
entry name fields = case Array.find (\(Tuple k _) -> k == name) fields of
  Just (Tuple _ v) -> Just v
  Nothing -> Nothing
