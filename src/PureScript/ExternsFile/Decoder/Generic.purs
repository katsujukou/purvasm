module PureScript.ExternsFile.Decoder.Generic where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), to)
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int as PInt
import PureScript.ExternsFile.Decoder.Class (class Decode, decoder)
import PureScript.ExternsFile.Decoder.Monad (DecodeError(..), Decoder(..), runDecoder)
import PureScript.ExternsFile.Decoder.Utils (asInt, readAt)
import Type.Proxy (Proxy(..))

-- class GenericDecoder a where
--   genericDecoder :: Decoder a
genericDecoder :: forall @a rep. Generic a rep => GenericDecoderRep 0 rep => Decoder a
genericDecoder = map to (genericDecoderRep (Proxy @ 0) (Proxy @ rep))

-- instance genericDecoderDefault ::
--   ( Generic a rep
--   , GenericDecoderRep 0 rep
--   ) => 
--   GenericDecoder a
--   where
--   genericDecoder = map to (genericDecoderRep (Proxy@0) (Proxy@rep))

class GenericDecoderRep (n :: Int) rep where
  genericDecoderRep :: Proxy n -> Proxy rep -> Decoder rep

instance 
  ( Reflectable n Int
  , GenericDecoderArguments 1 args
  ) =>
  GenericDecoderRep n (Constructor constr args) where
  genericDecoderRep nproxy _ = Decoder \fgn ->
    let n = reflectType nproxy in
    case runFn2 readAt 0 fgn >>= asInt "genericDecoderRepConstructor" of
      Left err -> Left $ AtIndex 0 err
      Right n'
        | n' == n -> Constructor @constr <$>
            runDecoder (genericDecoderArguments (Proxy@1) (Proxy@args)) fgn
        | otherwise -> Left $ UnknownConstructorTag n'

else instance 
  ( PInt.Add n 1 succ 
  , GenericDecoderRep n inl
  , GenericDecoderRep succ inr
  , Reflectable n Int
  ) =>
  GenericDecoderRep n (Sum inl inr) where
  genericDecoderRep nproxy _ = Decoder \fgn ->
    case runFn2 readAt 0 fgn >>= asInt "GenericDecoderRepSum" of
      Left err -> Left $ AtIndex 0 err
      Right n 
        | n == reflectType nproxy -> Inl <$> runDecoder (genericDecoderRep nproxy (Proxy @ inl)) fgn
        | otherwise -> Inr <$> runDecoder (genericDecoderRep (Proxy @ succ) (Proxy@inr)) fgn


class GenericDecoderArguments (n :: Int) args where
  genericDecoderArguments :: Proxy n -> Proxy args -> Decoder args

instance GenericDecoderArguments n NoArguments where
  genericDecoderArguments _ _ = pure NoArguments

else instance
  ( Decode t 
  , Reflectable n Int
  ) => GenericDecoderArguments n (Argument t) where
  genericDecoderArguments nproxy _ = Decoder \fgn ->
    let n = reflectType nproxy in
    case runFn2 readAt n fgn of
      Left err -> Left $ AtIndex n err
      Right x -> case runDecoder (decoder @t) x of
        Left err' -> Left $ AtIndex n err'
        Right a -> Right $ Argument a

else instance
  ( PInt.Add n 1 succ
  , GenericDecoderArguments n pro1 
  , GenericDecoderArguments succ pro2
  , Reflectable n Int
  ) =>
  GenericDecoderArguments n (Product pro1 pro2) where
  genericDecoderArguments nproxy _ = 
    Product
      <$> genericDecoderArguments nproxy (Proxy @ pro1)
      <*> genericDecoderArguments (Proxy @ succ) (Proxy @ pro2) 
