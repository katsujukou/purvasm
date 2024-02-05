module PureScript.ExternsFile.SourcePos where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Fmt (fmtWith)
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Fmt (ShowRecordLikeConfig)

data SourcePos = SourcePos Int Int -- line, column

derive instance Eq SourcePos
derive instance Ord SourcePos
derive instance Generic SourcePos _
instance showSourcePos :: Show SourcePos where
  show (SourcePos ln col) =
    fmtWith
      @ShowRecordLikeConfig
      @"SourcePos { line = <ln>, column = <col> }"
      { ln, col }

instance decodeSourcePos :: Decode SourcePos where
  decoder = genericDecoder

data SourceSpan = SourceSpan String SourcePos SourcePos

derive instance Eq SourceSpan
derive instance Ord SourceSpan
derive instance Generic SourceSpan _
instance showSourceSpan :: Show SourceSpan where
  show (SourceSpan name start end) =
    fmtWith
      @ShowRecordLikeConfig
      @"SourceSpan { name = <name>, start = <start>, end = <end> }"
      { name, start: show start, end: show end }

instance decodeSourceSpan :: Decode SourceSpan where
  decoder = genericDecoder

type SourceAnn = Tuple SourceSpan (Array Comment)

data Comment
  = LineComment String
  | BlockComment String

derive instance Eq Comment
derive instance Ord Comment
derive instance Generic Comment _
instance Show Comment where
  show = genericShow

instance Decode Comment where
  decoder = genericDecoder
