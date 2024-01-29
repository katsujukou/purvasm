module Purvasm.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data StructuredConstant
  = SCAtom AtomicConstant
  | SCBlock Tag (Array StructuredConstant)

derive instance Generic StructuredConstant _
instance Show StructuredConstant where
  show sc = genericShow sc

data Tag
  = TArray
  -- | TNumber
  -- | TNumberArray
  | TClosure
  | TString
  | TConstr Int

derive instance Generic Tag _
instance Show Tag where
  show = genericShow

data AtomicConstant
  = ACInt Int
  | ACNumber Number
  | ACBool Boolean
  | ACChar Char
  | ACString String

derive instance Generic AtomicConstant _
instance Show AtomicConstant where
  show = genericShow