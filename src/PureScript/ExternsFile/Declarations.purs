module PureScript.ExternsFile.Declarations where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Decoder.Monad (Decoder(..), runDecoder)
import PureScript.ExternsFile.Names (Ident, ModuleName, NameSource, OpName, ProperName)
import PureScript.ExternsFile.SourcePos (SourceSpan)



data ExportSource = ExportSource
  (Maybe ModuleName)
  -- ^imported from
  ModuleName
  -- ^defined in

derive instance Eq ExportSource
derive instance Ord ExportSource
derive instance Generic ExportSource _
instance Show ExportSource where
  show = genericShow

instance Decode ExportSource where
  decoder = genericDecoder

data DeclarationRef
  = TypeClassRef SourceSpan ProperName
  | TypeOpRef SourceSpan OpName
  | TypeRef SourceSpan ProperName (Maybe (Array ProperName))
  | ValueRef SourceSpan Ident
  | ValueOpRef SourceSpan OpName
  | TypeInstanceRef SourceSpan Ident NameSource
  | ModuleRef SourceSpan ModuleName
  | ReExportRef SourceSpan ExportSource DeclarationRef

instance Eq DeclarationRef where
  eq = case _, _ of
    TypeClassRef _ name1, TypeClassRef _ name2 -> name1 == name2
    TypeOpRef _ name1, TypeOpRef _ name2 -> name1 == name2 
    TypeRef _ name1 dctor1, TypeRef _ name2 dctor2 -> name1 == name2 && dctor1 == dctor2
    ValueRef _ name1, ValueRef _ name2 -> name1 == name2
    ValueOpRef _ name1, ValueOpRef _ name2 -> name1 == name2
    TypeInstanceRef _ name1 _, TypeInstanceRef _ name2 _ -> name1 == name2 
    ModuleRef _ name1, ModuleRef _ name2 -> name1 == name2 
    ReExportRef _ mn1 ref1, ReExportRef _ mn2 ref2 -> mn1 == mn2 && ref1 == ref2
    _, _ -> false 

instance Ord DeclarationRef where
  compare = case _, _ of
    TypeClassRef _ name1, TypeClassRef _ name2 -> compare name1 name2
    TypeOpRef _ name1, TypeOpRef _ name2 -> compare name1 name2 
    TypeRef _ name1 dctor1, TypeRef _ name2 dctor2 -> compare name1 name2 <> compare dctor1 dctor2
    ValueRef _ name1, ValueRef _ name2 -> compare name1 name2
    ValueOpRef _ name1, ValueOpRef _ name2 -> compare name1 name2
    TypeInstanceRef _ name1 _, TypeInstanceRef _ name2 _ -> compare name1 name2 
    ModuleRef _ name1, ModuleRef _ name2 -> compare name1 name2 
    ReExportRef _ mn1 ref1, ReExportRef _ mn2 ref2 -> compare mn1 mn2 <> compare ref1 ref2
    ref1, ref2 -> compare (orderOf ref1) (orderOf ref2) 
      where
        orderOf :: DeclarationRef -> Int
        orderOf = case _ of
          TypeClassRef _ _ -> 0
          TypeOpRef _ _ -> 1 
          TypeRef _ _ _ -> 2 
          ValueRef _ _ -> 3
          ValueOpRef _ _ -> 4
          TypeInstanceRef _ _ _ -> 5
          ModuleRef _ _ -> 6
          ReExportRef _ _ _ -> 7

derive instance Generic DeclarationRef _

instance Show DeclarationRef where
  show ref = genericShow ref

instance Decode DeclarationRef where
  decoder = Decoder \fgn -> runDecoder genericDecoder fgn
