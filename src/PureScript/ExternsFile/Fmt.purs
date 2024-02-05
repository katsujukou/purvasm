module PureScript.ExternsFile.Fmt where

import Fmt (type (#))
import Fmt as Fmt

type ShowRecordLikeConfig =
  Fmt.DefaultConfig
    # Fmt.SetOpenClose "<" ">"
