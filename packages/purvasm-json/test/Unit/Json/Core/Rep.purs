-- | A concrete JSON representation used only by the tests: a plain ADT with a `Builder` and
-- | `Eliminator`, so `Json.Core.parse` / `Json.Core.print` can be exercised against inspectable,
-- | comparable values.
module Test.Unit.Json.Core.Rep
  ( TJ(..)
  , builder
  , eliminator
  ) where

import Prelude

import Data.Tuple (Tuple)
import Json.Core.Types (Builder, Eliminator)

data TJ
  = TNull
  | TBool Boolean
  | TNum Number
  | TStr String
  | TArr (Array TJ)
  | TObj (Array (Tuple String TJ))

derive instance eqTJ :: Eq TJ

instance showTJ :: Show TJ where
  show TNull = "TNull"
  show (TBool b) = "(TBool " <> show b <> ")"
  show (TNum n) = "(TNum " <> show n <> ")"
  show (TStr s) = "(TStr " <> show s <> ")"
  show (TArr xs) = "(TArr " <> show xs <> ")"
  show (TObj kvs) = "(TObj " <> show kvs <> ")"

builder :: Builder TJ
builder =
  { jnull: TNull
  , jboolean: TBool
  , jnumber: TNum
  , jstring: TStr
  , jarray: TArr
  , jobject: TObj
  }

eliminator :: Eliminator TJ
eliminator cases = case _ of
  TNull -> cases.onNull
  TBool b -> cases.onBoolean b
  TNum n -> cases.onNumber n
  TStr s -> cases.onString s
  TArr xs -> cases.onArray xs
  TObj kvs -> cases.onObject kvs
