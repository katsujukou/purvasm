module PureScript.ExternsFile.Decoder.Monad where

import Prelude

import Control.Alt (class Alt)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)

data DecodeError
  = UnknownConstructorTag Int
  | Unexpected String
  | NotSupported
  | AtIndex Int DecodeError
  | MissingValue

describeError :: DecodeError -> String
describeError _ = "DecodeError (Not Implemented)"

derive instance Generic DecodeError _
instance Show DecodeError where
  show err = genericShow err

newtype Decoder a = Decoder (Foreign -> Either DecodeError a)

runDecoder :: forall a. Decoder a -> Foreign -> Either DecodeError a
runDecoder (Decoder d) = d

instance Functor Decoder where
  map f (Decoder k) = Decoder \fgn -> map f (k fgn)

instance Apply Decoder where
  apply (Decoder k1) (Decoder k2) = Decoder \fgn ->
    case k1 fgn of
      Left err -> Left err
      Right f -> f <$> k2 fgn

instance Applicative Decoder where
  pure a = Decoder \_ -> Right a

instance Bind Decoder where
  bind (Decoder k1) f = Decoder \fgn -> case k1 fgn of
    Left err -> Left err
    Right a -> let Decoder k2 = f a in k2 fgn

instance Monad Decoder

instance Alt Decoder where
  alt (Decoder dec1) (Decoder dec2) = Decoder \fgn ->
    case dec1 fgn of
      Left _ -> dec2 fgn
      Right a -> Right a

fail :: forall a. DecodeError -> Decoder a
fail err = Decoder \_ -> Left err
