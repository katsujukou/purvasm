module Purvasm.Record
  ( RecordProp(..)
  , RecordSignature
  , mkRecordSignature
  ) where

import Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype)
import Data.StringHash (stringHash)

newtype RecordProp = RecordProp String

derive instance Eq RecordProp
derive instance Newtype RecordProp _

instance Hashable RecordProp where
  hash (RecordProp p) = stringHash p

instance Ord RecordProp where
  compare = compare `on` hash

instance Show RecordProp where
  show (RecordProp p) = "(RecordProp " <> p <> ")"

newtype RecordSignature = RecordSignature (Array RecordProp)

derive instance Eq RecordSignature
derive instance Ord RecordSignature
derive newtype instance Hashable RecordSignature
instance Show RecordSignature where
  show (RecordSignature sig) = "(RecordSignature " <> show sig <> ")"

mkRecordSignature :: Array String -> RecordSignature
mkRecordSignature = RecordSignature <<< Array.nub <<< Array.sort <<< map RecordProp