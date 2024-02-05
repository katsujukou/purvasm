module PureScript.ExternsFile.Decoder.Utils where

import Data.Either (Either)
import Data.Function.Uncurried (Fn2)
import Foreign (Foreign)
import PureScript.ExternsFile.Decoder.Monad (DecodeError)


foreign import asInt :: forall a. a -> Foreign -> Either DecodeError Int

foreign import asArray :: Foreign -> Either DecodeError (Array Foreign)

foreign import readAt :: Fn2 Int Foreign (Either DecodeError Foreign)
