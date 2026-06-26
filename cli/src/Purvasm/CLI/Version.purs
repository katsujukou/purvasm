module Purvasm.CLI.Version where

import Fmt as Fmt
import Purvasm.CLI.Version.Generated (version)
import Spago.Generated.BuildInfo (pursVersion)

versionString :: String
versionString = Fmt.fmt
  @"""purvasm {version} (compat with purs={pursVersion})"""
  { version, pursVersion: pursVersion }
