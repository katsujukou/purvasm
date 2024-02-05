module PureScript.ExternsFile.Decoder.Class where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Function.Uncurried (runFn2)
import Data.Identity (Identity(..))
import Data.Lens (_Left, over)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Flat (T2, T3, mkT2, mkT3, toTuple2)
import Foreign (readBoolean, readInt, readString, renderForeignError)
import PureScript.ExternsFile.Decoder.Monad (DecodeError(..), Decoder(..), runDecoder)
import PureScript.ExternsFile.Decoder.Utils (asArray, asInt, readAt)

class Decode t where
  decoder :: Decoder t 

instance Decode Int where
  decoder = Decoder \fgn ->
    let Identity res = runExceptT (readInt fgn)
    in  res # over _Left (Unexpected <<< foldMap renderForeignError) 

instance Decode String where
  decoder = Decoder \fgn ->
    let Identity res = runExceptT (readString fgn)
    in  res # over _Left (Unexpected <<< foldMap renderForeignError)

instance Decode Boolean where
  decoder = Decoder \fgn ->
    let Identity res = runExceptT (readBoolean fgn)
    in  res # over _Left (Unexpected <<< foldMap renderForeignError) 

instance Decode a => Decode (Maybe a) where
  decoder = Decoder \fgn ->
    case runFn2 readAt 0 fgn of
      Left MissingValue -> Right Nothing
      Left err -> Left err
      Right fgn' -> case runDecoder (decoder@a) fgn' of
        Right a -> pure (Just a)
        Left _ ->  Just <$> runDecoder (decoder@a) fgn

instance Decode a => Decode (Array a) where
  decoder = Decoder \fgn -> 
    case asArray fgn of
      Left err -> Left err
      Right fgns -> ST.run do
        buf <- STArray.thaw fgns 
        successes <- STArray.new
        failure <- STRef.new Nothing 
        continue <- STRef.new true
        ST.while (STRef.read continue) do
          STArray.pop buf >>= case _ of
            Nothing -> STRef.write false continue $> unit
            Just fgn' -> case runDecoder decoder fgn' of 
              Left err -> do
                STRef.write false continue 
                 *> STRef.write (Just err) failure 
                 $> unit
              Right a -> STArray.push a successes $> unit
        STRef.read failure >>= case _ of
          Just err -> pure (Left err)
          Nothing -> Right <$> STArray.freeze successes

instance (Decode a, Decode b) => Decode (Either a b) where
  decoder = Decoder \fgn -> 
    case runFn2 readAt 0 fgn >>= asInt "Either" of
      Left err -> Left (AtIndex 0 err)
      Right tag -> case tag of
        0 -> runFn2 readAt 1 fgn >>= runDecoder (decoder@a) <#> Left
        1 -> runFn2 readAt 1 fgn >>= runDecoder (decoder@b) <#> Right
        _ -> Left $ UnknownConstructorTag tag

instance (Decode a, Decode b) => Decode (Tuple a b) where
  decoder = decoder <#> toTuple2

instance (Decode a, Decode b) => Decode (T2 a b) where
  decoder = Decoder \fgn -> ado 
    a <- runFn2 readAt 0 fgn >>= runDecoder decoder
    b <- runFn2 readAt 1 fgn >>= runDecoder decoder
    in mkT2 a b

instance (Decode a, Decode b, Decode c) => Decode (T3 a b c) where
  decoder = Decoder \fgn -> ado 
    a <- runFn2 readAt 0 fgn >>= runDecoder decoder
    b <- runFn2 readAt 1 fgn >>= runDecoder decoder
    c <- runFn2 readAt 2 fgn >>= runDecoder decoder
    in mkT3 a b c