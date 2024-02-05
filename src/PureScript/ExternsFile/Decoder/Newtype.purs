module PureScript.ExternsFile.Decoder.Newtype where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.Newtype (class Newtype, wrap)
import PureScript.ExternsFile.Decoder.Class (class Decode, decoder)
import PureScript.ExternsFile.Decoder.Monad (DecodeError(..), Decoder(..), runDecoder)
import PureScript.ExternsFile.Decoder.Utils (asInt, readAt)

newtypeDecoder :: forall a b. Newtype a b => Decode b => Decoder a
newtypeDecoder = Decoder \fgn -> 
  case runFn2 readAt 0 fgn >>= asInt "newtypeDecoder" of
    Left err -> Left err
    Right n
      | n == 0 -> runFn2 readAt 1 fgn >>= runDecoder (decoder@b) <#> wrap
      | otherwise -> Left $ Unexpected "Not a constructor tag of newtype" 