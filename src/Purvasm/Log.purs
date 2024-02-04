module Purvasm.Log where

import Prelude

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

printLogLevel :: LogLevel -> String
printLogLevel = case _ of
  Debug -> "DEBUG"
  Info -> "INFO"
  Warn -> "WARN"
  Error -> "ERROR"