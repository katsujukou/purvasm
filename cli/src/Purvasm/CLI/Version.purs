module Purvasm.CLI.Version where

import Fmt as Fmt
import Spago.Generated.BuildInfo (pursVersion)

foreign import version :: String

versionString :: String
versionString = Fmt.fmt
  @"""purs-wasm {version} (compat with purs={pursVersion})"""
  { version, pursVersion: pursVersion }
