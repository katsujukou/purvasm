-- | Representation-neutral interfaces shared by every JSON front-end (ADR-0046).
-- |
-- | A `Builder` tells the parser how to *construct* a node; an `Eliminator` tells the printer
-- | how to *deconstruct* one. Both decouple the object/array representation from the grammar
-- | walk via the neutral `Array (Tuple String j)` / `Array j`, so the same parser and printer
-- | serve `argonaut-core`'s `Json` (`Foreign.Object` + `Array`) and, later, `purescript-json`'s
-- | `JObject` / `JArray` without duplicating either.
module Json.Core.Types
  ( Builder
  , Eliminator
  , Cases
  ) where

import Data.Tuple (Tuple)

-- | How to build each JSON node from its parsed contents. Object entries are passed as a neutral
-- | association array; the adapter folds them into its own object representation.
type Builder j =
  { jnull :: j
  , jboolean :: Boolean -> j
  , jnumber :: Number -> j
  , jstring :: String -> j
  , jarray :: Array j -> j
  , jobject :: Array (Tuple String j) -> j
  }

-- | Handlers for case analysis of a JSON node, one per variant. The object case receives a
-- | neutral association array so the printer never sees the concrete object type.
type Cases j r =
  { onNull :: r
  , onBoolean :: Boolean -> r
  , onNumber :: Number -> r
  , onString :: String -> r
  , onArray :: Array j -> r
  , onObject :: Array (Tuple String j) -> r
  }

-- | Case analysis for a representation `j`: dispatch a node to the matching handler. This is the
-- | `caseJson`-shaped eliminator the printer folds over (with the object case as entries).
type Eliminator j = forall r. Cases j r -> j -> r
