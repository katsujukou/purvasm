module Purvasm.Compiler.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Fmt (fmt)
import Node.Path (FilePath)
import Purvasm.Types (ModuleName)

data CompInput
  = InpFilePath FilePath
  | InpModule ModuleName

derive instance Generic CompInput _
instance Show CompInput where
  show = genericShow

data LogVerbosity = Quiet | Normal | Verbose

derive instance Eq LogVerbosity
derive instance Ord LogVerbosity
derive instance Generic LogVerbosity _
instance Show LogVerbosity where
  show = genericShow

type BuildIndex = { stage :: Int, current :: Int, total :: Int }

prettyPrintIndex :: BuildIndex -> String
prettyPrintIndex { stage, current, total } =
  fmt @"[\x1b[1;36m{stage}\x1b[0m/\x1b[1;32m{current}\x1b[0m/{total}]"
    { stage, current, total }