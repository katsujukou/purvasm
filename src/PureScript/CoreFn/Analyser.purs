module PureScript.CoreFn.Analyser where

import Prelude

import Data.Array (fold, foldMap, sort)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.Regex as Re
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CoreFn (propValue)
import PureScript.CoreFn as CF
import PureScript.CoreFn.Utils (typeClassConstructorRegex)

collectRecordSignatures :: CF.Expr CF.Ann -> Array (Array String)
collectRecordSignatures = case _ of
  CF.ExprLit _ lit
    | CF.LitArray exps <- lit -> foldMap collectRecordSignatures exps
    | CF.LitRecord props <- lit -> fold
        [ props
            # foldMap (CF.propValue >>> collectRecordSignatures)
        , [ sort $ CF.propKey <$> props ]
        ]
  CF.ExprAbs _ _ exp -> collectRecordSignatures exp
  CF.ExprApp _ e1 e2 -> foldMap collectRecordSignatures [ e1, e2 ]
  CF.ExprAccessor _ exp _ -> collectRecordSignatures exp
  CF.ExprUpdate _ exp updators -> collectRecordSignatures exp <> foldMap (CF.propValue >>> collectRecordSignatures) updators
  CF.ExprLet _ binds exp ->
    let
      recordTypesInBinding (CF.Binding _ _ e) = collectRecordSignatures e
    in
      fold
        [ binds
            <#> case _ of
              CF.NonRec b -> recordTypesInBinding b
              CF.Rec bs -> foldMap recordTypesInBinding bs
            # fold
        , collectRecordSignatures exp
        ]
  CF.ExprCase _ caseHeads caseAlts ->
    let
      recordTypesInCaseAlt (CF.CaseAlternative _ caseGuard) = case caseGuard of
        CF.Unconditional exp -> collectRecordSignatures exp
        CF.Guarded guards ->
          guards # foldMap (\(CF.Guard g e) -> collectRecordSignatures g <> collectRecordSignatures e)
    in
      fold
        [ foldMap collectRecordSignatures caseHeads
        , foldMap recordTypesInCaseAlt caseAlts
        ]
  _ -> []

staticExpr :: CF.Expr CF.Ann -> Boolean
staticExpr = case _ of
  CF.ExprAbs _ _ _ -> true
  CF.ExprLit _ lit -> case lit of
    CF.LitArray elts -> Array.all staticExpr elts
    CF.LitRecord props -> Array.all (CF.propValue >>> staticExpr) props
    _ -> true
  exp@(CF.ExprApp _ _ _)
    | f /\ args <- appSpine [] exp
    , CF.ExprConstructor _ _ _ _ <- f -> Array.all staticExpr args
  _ -> false
  where
  appSpine args = case _ of
    CF.ExprApp _ f arg -> appSpine (Array.cons arg args) f
    exp -> exp /\ args

type TypeclassInstance =
  { typeclass :: CF.Qualified CF.ProperName
  , members :: Array (CF.Ident /\ CF.Expr CF.Ann)
  , constraints :: Array (Maybe (CF.Qualified CF.ProperName))
  }

classify
  :: Array (CF.Bind CF.Ann)
  -> { plain :: Array (Tuple CF.Ident (CF.Expr CF.Ann))
     , constructors :: Array (Tuple CF.Ident (CF.Expr CF.Ann))
     , newtypeConstructors :: Array (Tuple CF.Ident (CF.Expr CF.Ann))
     , typeclassConstructors :: Array (Tuple CF.Ident (CF.Expr CF.Ann))
     , typeclassInstances :: Array (Tuple CF.Ident (CF.Expr CF.Ann))
     }
classify = foldMap (go1 initial)
  where
  initial =
    { plain: []
    , constructors: []
    , newtypeConstructors: []
    , typeclassConstructors: []
    , typeclassInstances: []
    }

  go1 accu = case _ of
    CF.NonRec binding -> go2 accu binding
    CF.Rec bindings -> foldMap (go2 accu) bindings

  go2 accu (CF.Binding _ ident@(CF.Ident id) expr) = case expr of
    CF.ExprConstructor _ _ _ _ -> accu { constructors = Array.cons (ident /\ expr) accu.constructors }
    CF.ExprAbs ann' _ _
      | CF.Ann { meta: Just CF.IsNewtype } <- ann' ->
          if Re.test typeClassConstructorRegex id then
            let
              typclsCtors = Array.cons (ident /\ expr) accu.typeclassConstructors
              plain = Array.cons (ident /\ expr) accu.plain
            in
              accu { typeclassConstructors = typclsCtors, plain = plain }
          else accu { newtypeConstructors = Array.cons (ident /\ expr) accu.newtypeConstructors }
    _
      | Just _ <- typeclassInstanceOfExpr expr ->
          accu { typeclassInstances = Array.cons (ident /\ expr) accu.typeclassInstances }
      | otherwise -> accu { plain = Array.cons (ident /\ expr) accu.plain }

typeclassInstanceOfExpr :: CF.Expr CF.Ann -> Maybe TypeclassInstance
typeclassInstanceOfExpr = go []
  where
  go constraints = case _ of
    CF.ExprApp _ abs arg
      | CF.ExprVar (CF.Ann { meta: Just CF.IsNewtype }) absVar <- abs
      , CF.Qualified (Just moduleName) (CF.Ident typeclassDict) <- absVar
      , Re.test typeClassConstructorRegex typeclassDict
      , CF.ExprLit _ (CF.LitRecord props) <- arg ->
          let
            clsName = Str.replace (Str.Pattern "$Dict") (Str.Replacement "") typeclassDict
          in
            Just
              { typeclass: CF.Qualified (Just moduleName) (CF.ProperName clsName)
              , members: props <#> \(CF.Prop id exp) -> CF.Ident id /\ exp
              , constraints
              }
    CF.ExprAbs _ _ body -> go (Array.cons Nothing constraints) body
    CF.ExprLet _ _ body -> go constraints body
    _ -> Nothing

freeVars :: CF.Expr CF.Ann -> Array CF.Ident
freeVars = case _ of
  CF.ExprVar _ (CF.Qualified Nothing var) -> [ var ]
  CF.ExprAbs _ arg body -> Array.filter (_ /= arg) (freeVars body)
  CF.ExprApp _ func arg -> fold [ freeVars func, freeVars arg ]
  CF.ExprLet _ binds body ->
    let
      boundVars = fold $ binds <#> case _ of
        CF.NonRec (CF.Binding _ id _) -> [ id ]
        CF.Rec bindings -> map CF.bindingIdent bindings
    in
      Array.filter (not <<< (_ `Array.elem` boundVars)) $ freeVars body
  CF.ExprAccessor _ e _ -> freeVars e
  CF.ExprUpdate _ e ups -> fold [ freeVars e, foldMap (freeVars <<< propValue) ups ]
  CF.ExprCase _ caseHeads caseAlts ->
    let
      freeVarsInHeads = foldMap freeVars caseHeads
      freeVarsInGuard (CF.Guard gur exp) = freeVars gur <> freeVars exp
      freeVarsInCaseAlts = join $ caseAlts
        <#> \(CF.CaseAlternative _ act) -> case act of
          CF.Unconditional exp -> freeVars exp
          CF.Guarded gs -> foldMap freeVarsInGuard gs
    in
      freeVarsInHeads <> freeVarsInCaseAlts
  _ -> []
