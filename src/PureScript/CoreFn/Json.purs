module PureScript.CoreFn.Json
  ( PartialModule(..)
  , JsonDecode
  , decodePartialModule
  , fullModule
  , runJsonDecode
  ) where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Data.Argonaut (Json, JsonDecodeError(..), caseJson)
import Data.Array (intercalate, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Lazy as L
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.TraversableWithIndex (forWithIndex)
import Foreign.Object (Object)
import Foreign.Object as Object
import PureScript.CoreFn (Ann(..), Bind(..), Binder(..), Binding(..), CaseAlternative(..), CaseGuard(..), ConstructorType(..), Expr(..), Guard(..), Ident(..), Import(..), Literal(..), Meta(..), Module(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..))
import Safe.Coerce (coerce)

newtype JsonDecode a = JsonDecode (Json -> L.Lazy (Either JsonDecodeError a))

runJsonDecode :: forall a. JsonDecode a -> Json -> Either JsonDecodeError a
runJsonDecode (JsonDecode decoder) json = L.force $ decoder json

instance Functor JsonDecode where
  map f (JsonDecode k) = JsonDecode \j ->
    L.defer \_ -> map f (L.force $ k j)

instance Apply JsonDecode where
  apply (JsonDecode k1) (JsonDecode k2) = JsonDecode \j ->
    L.defer \_ -> case L.force $ k1 j of
      Left e -> Left e
      Right f -> f <$> L.force (k2 j)

instance Applicative JsonDecode where
  pure a = JsonDecode \_ -> L.defer \_ -> Right a

instance Bind JsonDecode where
  bind (JsonDecode k1) f = JsonDecode \j ->
    L.defer \_ -> case L.force (k1 j) of
      Left e -> Left e
      Right a -> let JsonDecode k2 = f a in L.force (k2 j)

instance Monad JsonDecode

instance Alt JsonDecode where
  alt (JsonDecode k1) (JsonDecode k2) = JsonDecode \j ->
    L.defer \_ -> alt (L.force $ k1 j) (L.force $ k2 j)

read :: JsonDecode Json
read = JsonDecode \json -> L.defer \_ -> Right json

fail :: forall a. JsonDecodeError -> JsonDecode a
fail err = JsonDecode \_ -> L.defer \_ -> Left err

defer :: forall a. (Unit -> JsonDecode a) -> JsonDecode (L.Lazy (Either JsonDecodeError a))
defer l = do
  json <- read
  pure $ L.defer \_ -> runJsonDecode (l unit) json

-- For the purpose of purvasm's compilation pass, decoding of fields
-- other than name and imports should be defered until actual compilation 
newtype PartialModule a = PartialModule
  { name :: ModuleName
  , imports :: Array (Import a)
  , rest ::
      L.Lazy
        ( Either JsonDecodeError
            { decls :: Array (Bind a)
            , foreign :: Array Ident
            -- , reExports :: Array ReExport
            }
        )
  }

fullModule :: forall a. PartialModule a -> Either JsonDecodeError (Module a)
fullModule (PartialModule { name, imports, rest }) = case L.force rest of
  Left e -> Left e
  Right rest'@{ decls } -> pure $ Module
    { name
    , imports
    , decls
    , foreign: rest'.foreign
    }

decodePartialModule :: JsonDecode (PartialModule Ann)
decodePartialModule = decodePartialModule' decodeAnn

decodePartialModule' :: forall a. JsonDecode a -> JsonDecode (PartialModule a)
decodePartialModule' decodeAnn' = do
  name <- decodeFieldWith decodeModuleName "moduleName"
  imports <- decodeFieldWith (decodeArray (decodeImport decodeAnn')) "imports"
  rest <- defer \_ -> do
    decls <- decodeFieldWith (decodeArray (decodeBind (decodeAnn'))) "decls"
    foreign_ <- decodeFieldWith (decodeArray decodeIdent) "foreign"
    pure
      { decls
      , foreign: foreign_
      }
  pure $ PartialModule
    { name
    , imports
    , rest
    }

decodeIdent :: JsonDecode Ident
decodeIdent = coerce <$> decodeString

decodeProperName :: JsonDecode ProperName
decodeProperName = coerce <$> decodeString

decodeModuleName :: JsonDecode ModuleName
decodeModuleName = do
  part <- decodeArray decodeString
  pure (ModuleName <<< intercalate "." $ part)

decodeQualified :: forall a. JsonDecode a -> JsonDecode (Qualified a)
decodeQualified dec = do
  moduleName <- decodeOptionalFieldWith decodeModuleName "moduleName"
  identifier <- decodeFieldWith dec "identifier"
  pure $ Qualified moduleName identifier

decodeConstructorType :: JsonDecode ConstructorType
decodeConstructorType = do
  str <- decodeString
  case str of
    "ProductType" -> pure ProductType
    "SumType" -> pure SumType
    _ -> fail $ TypeMismatch "ConstructorType"

decodeMeta :: JsonDecode Meta
decodeMeta = do
  decodeFieldWith decodeString "metaType" >>= case _ of
    "IsConstructor" -> do
      ct <- decodeFieldWith decodeConstructorType "constructorType"
      is <- decodeFieldWith (decodeArray decodeIdent) "identifiers"
      pure $ IsConstructor ct is
    "IsNewtype" ->
      pure IsNewtype
    "IsTypeClassConstructor" ->
      pure IsTypeClassConstructor
    "IsForeign" ->
      pure IsForeign
    "IsWhere" ->
      pure IsWhere
    "IsSyntheticApp" ->
      pure IsSyntheticApp
    _ ->
      fail $ TypeMismatch "Meta"

decodeAnn :: JsonDecode Ann
decodeAnn = do
  meta <- (decodeFieldWith decodeMaybeMeta "meta")
  pure $ Ann { meta }
  where
  decodeMaybeMeta = (Just <$> decodeMeta)
    <|> (decodeNull <#> const Nothing)

-- decodeImport :: JsonDecode Import
-- decodeImport = do
decodeImport :: forall a. JsonDecode a -> JsonDecode (Import a)
decodeImport decodeAnn' = do
  ann <- decodeFieldWith decodeAnn' "annotation"
  mod <- decodeFieldWith decodeModuleName "moduleName"
  pure $ Import ann mod

decodeBind :: forall a. JsonDecode a -> JsonDecode (Bind a)
decodeBind decAnn = do
  typ <- decodeFieldWith decodeString "bindType"
  case typ of
    "NonRec" -> NonRec <$> decodeBinding decAnn
    "Rec" -> Rec <$> decodeFieldWith (decodeArray (decodeBinding decAnn)) "binds"
    _ -> fail $ TypeMismatch "Bind"

decodeBinding :: forall a. JsonDecode a -> JsonDecode (Binding a)
decodeBinding decAnn = do
  ann <- decodeFieldWith decAnn "annotation"
  ident <- decodeFieldWith decodeIdent "identifier"
  expr <- decodeFieldWith (decodeExpr decAnn) "expression"
  pure $ Binding ann ident expr

decodeExpr :: forall a. JsonDecode a -> JsonDecode (Expr a)
decodeExpr decAnn = do
  ann <- decodeFieldWith decAnn "annotation"
  typ <- decodeFieldWith decodeString "type"
  case typ of
    "Var" ->
      ExprVar ann <$> decodeFieldWith (decodeQualified decodeIdent) "value"
    "Literal" ->
      ExprLit ann <$> decodeFieldWith (decodeLiteral (decodeExpr decAnn)) "value"
    "Constructor" -> do
      tyn <- decodeFieldWith decodeProperName "typeName"
      con <- decodeFieldWith decodeIdent "constructorName"
      is <- decodeFieldWith (decodeArray decodeString) "fieldNames"
      pure $ ExprConstructor ann tyn con is
    "Accessor" -> do
      e <- decodeFieldWith (decodeExpr decAnn) "expression"
      f <- decodeFieldWith decodeString "fieldName"
      pure $ ExprAccessor ann e f
    "ObjectUpdate" -> do
      e <- decodeFieldWith (decodeExpr decAnn) "expression"
      us <- decodeFieldWith (decodeRecord (decodeExpr decAnn)) "updates"
      pure $ ExprUpdate ann e us
    "Abs" -> do
      idn <- decodeFieldWith decodeIdent "argument"
      e <- decodeFieldWith (decodeExpr decAnn) "body"
      pure $ ExprAbs ann idn e
    "App" -> do
      e1 <- decodeFieldWith (decodeExpr decAnn) "abstraction"
      e2 <- decodeFieldWith (decodeExpr decAnn) "argument"
      pure $ ExprApp ann e1 e2
    "Case" -> do
      cs <- decodeFieldWith (decodeArray (decodeExpr decAnn)) "caseExpressions"
      cas <- decodeFieldWith (decodeArray (decodeCaseAlternative decAnn)) "caseAlternatives"
      pure $ ExprCase ann cs cas
    "Let" -> do
      bs <- decodeFieldWith (decodeArray (decodeBind decAnn)) "binds"
      e <- decodeFieldWith (decodeExpr decAnn) "expression"
      pure $ ExprLet ann bs e
    _ ->
      fail $ TypeMismatch "Expr"

decodeCaseAlternative :: forall a. JsonDecode a -> JsonDecode (CaseAlternative a)
decodeCaseAlternative decAnn = do
  binders <- decodeFieldWith (decodeArray (decodeBinder decAnn)) "binders"
  isGuarded <- decodeFieldWith decodeBoolean "isGuarded"
  if isGuarded then do
    es <- decodeFieldWith (decodeArray (decodeGuard decAnn)) "expressions"
    pure $ CaseAlternative binders (Guarded es)
  else do
    e <- decodeFieldWith (decodeExpr decAnn) "expression"
    pure $ CaseAlternative binders (Unconditional e)

decodeGuard :: forall a. JsonDecode a -> JsonDecode (Guard a)
decodeGuard decAnn = do
  guard <- decodeFieldWith (decodeExpr decAnn) "guard"
  expr <- decodeFieldWith (decodeExpr decAnn) "expression"
  pure $ Guard guard expr

decodeBinder :: forall a. JsonDecode a -> JsonDecode (Binder a)
decodeBinder decAnn = do
  ann <- decodeFieldWith decAnn "annotation"
  typ <- decodeFieldWith decodeString "binderType"
  case typ of
    "NullBinder" ->
      pure $ BinderNull ann
    "VarBinder" ->
      BinderVar ann <$> decodeFieldWith decodeIdent "identifier"
    "LiteralBinder" ->
      BinderLit ann <$> decodeFieldWith (decodeLiteral (decodeBinder decAnn)) "literal"
    "ConstructorBinder" -> do
      tyn <- decodeFieldWith (decodeQualified decodeProperName) "typeName"
      ctn <- decodeFieldWith (decodeQualified decodeIdent) "constructorName"
      binders <- decodeFieldWith (decodeArray (decodeBinder decAnn)) "binders"
      pure $ BinderConstructor ann tyn ctn binders
    "NamedBinder" -> do
      ident <- decodeFieldWith decodeIdent "identifier"
      binder <- decodeFieldWith (decodeBinder decAnn) "binder"
      pure $ BinderNamed ann ident binder
    _ ->
      fail $ TypeMismatch "Binder"

decodeLiteral :: forall a. JsonDecode a -> JsonDecode (Literal a)
decodeLiteral dec = do
  typ <- decodeFieldWith decodeString "literalType"
  case typ of
    "IntLiteral" ->
      LitInt <$> decodeFieldWith decodeInt "value"
    "NumberLiteral" ->
      LitNumber <$> decodeFieldWith decodeNumber "value"
    "StringLiteral" ->
      LitString <$> decodeFieldWith decodeString "value"
    "CharLiteral" -> do
      str <- decodeFieldWith decodeString "value"
      case Array.head (SCU.toCharArray str) of
        Just ch
          | SCU.length str == 1 -> pure $ LitChar ch
        _ -> fail $ TypeMismatch "Char"
    "BooleanLiteral" ->
      LitBoolean <$> decodeFieldWith decodeBoolean "value"
    "ArrayLiteral" ->
      LitArray <$> decodeFieldWith (decodeArray dec) "value"
    "ObjectLiteral" ->
      LitRecord <$> decodeFieldWith (decodeRecord dec) "value"
    _ ->
      fail $ TypeMismatch "Literal"

decodeRecord :: forall a. JsonDecode a -> JsonDecode (Array (Prop a))
decodeRecord = decodeArray <<< decodeProp
  where
  decodeProp decoder = do
    assumeLength (TypeMismatch "Tuple") 2
    prop <- decodeArrayElemWith decodeString 0
    value <- decodeArrayElemWith decoder 1
    pure $ Prop prop value

assumeLength :: JsonDecodeError -> Int -> JsonDecode Unit
assumeLength err len = do
  decodeJsonArray <#> Array.length >>= case _ of
    l | l == len -> pure unit
    _ -> fail err

decodeArray :: forall a. JsonDecode a -> JsonDecode (Array a)
decodeArray (JsonDecode decoder) = JsonDecode \json ->
  L.defer \_ -> case runJsonDecode decodeJsonArray json of
    Left e -> Left e
    Right jsons -> forWithIndex jsons \i j -> do
      case L.force (decoder j) of
        Left e -> Left (AtIndex i e)
        Right val -> Right val

-- decodeOptionalFieldWith :: forall a. JsonDecode a -> String -> JsonDecode (Maybe a)
-- decodeOptionalFieldWith decoder prop = do
--   obj <- decodeJsonObject
--   case Object.lookup prop obj of
--     Nothing -> pure Nothing
--     Just a -> case runJsonDecode decoder a of
--       Left e -> fail (AtKey prop e)
--       Right v -> pure (Just v)

decodeArrayElemWith :: forall a. JsonDecode a -> Int -> JsonDecode a
decodeArrayElemWith (JsonDecode k) idx = do
  ary <- decodeJsonArray
  case ary !! idx of
    Nothing -> fail (AtIndex idx MissingValue)
    Just json -> case L.force (k json) of
      Left e -> fail e
      Right a -> pure a

decodeFieldWith :: forall a. JsonDecode a -> String -> JsonDecode a
decodeFieldWith decoder prop = do
  obj <- decodeJsonObject
  case Object.lookup prop obj of
    Nothing -> fail (AtKey prop MissingValue)
    Just a -> case runJsonDecode decoder a of
      Left e -> fail (AtKey prop e)
      Right v -> pure v

decodeOptionalFieldWith :: forall a. JsonDecode a -> String -> JsonDecode (Maybe a)
decodeOptionalFieldWith decoder prop = do
  obj <- decodeJsonObject
  case Object.lookup prop obj of
    Nothing -> pure Nothing
    Just a -> case runJsonDecode decoder a of
      Left e -> fail (AtKey prop e)
      Right v -> pure (Just v)

decodeJsonArray :: JsonDecode (Array Json)
decodeJsonArray = JsonDecode \j -> L.defer \_ -> caseJson err err err err Right err j
  where
  err :: forall a. a -> Either JsonDecodeError (Array Json)
  err _ = Left $ TypeMismatch "Array"

decodeJsonObject :: JsonDecode (Object Json)
decodeJsonObject = JsonDecode \j -> L.defer \_ -> caseJson err err err err err Right j
  where
  err :: forall a. a -> Either JsonDecodeError (Object Json)
  err _ = Left $ TypeMismatch "Object"

decodeString :: JsonDecode String
decodeString = JsonDecode \j -> L.defer \_ -> caseJson err err err Right err err j
  where
  err :: forall a. a -> Either JsonDecodeError String
  err _ = Left $ TypeMismatch "String"

decodeNumber :: JsonDecode Number
decodeNumber = JsonDecode \j -> L.defer \_ -> caseJson err err Right err err err j
  where
  err :: forall a. a -> Either JsonDecodeError Number
  err _ = Left $ TypeMismatch "Number"

decodeBoolean :: JsonDecode Boolean
decodeBoolean = JsonDecode \j -> L.defer \_ -> caseJson err Right err err err err j
  where
  err :: forall a. a -> Either JsonDecodeError Boolean
  err _ = Left $ TypeMismatch "Boolean"

decodeInt :: JsonDecode Int
decodeInt = do
  num <- decodeNumber
  case Int.fromNumber num of
    Nothing ->
      fail (TypeMismatch "Int")
    Just int ->
      pure int

decodeNull :: JsonDecode Unit
decodeNull = JsonDecode \json -> L.defer \_ -> caseJson Right err err err err err json
  where
  err :: forall a. a -> Either JsonDecodeError Unit
  err _ = Left $ TypeMismatch "null"
