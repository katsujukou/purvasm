module Purvasm.NCore.Translate where

import Prelude

import Data.Array (catMaybes, mapWithIndex, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.HashMap as HM
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String as Str
import Data.Traversable (class Traversable, for, for_, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (unfoldr)
import Effect.Console (logShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Purvasm.ECore.Syntax as ECF
import Purvasm.Global (GlobalEnv(..), ValueDesc(..), mkGlobalName)
import Purvasm.Global as Global
import Purvasm.NCore.Arrange (arrangeDecls)
import Purvasm.NCore.Env (LocalSymbolTable(..), TranslEnv, VariableDesc(..), extendByNewVar)
import Purvasm.NCore.Foreign (overrideForeign)
import Purvasm.NCore.MatchComp (PatternMatching(..), partitionEither)
import Purvasm.NCore.MatchComp as MatchComp
import Purvasm.NCore.Syntax (NCore(..), Program(..))
import Purvasm.NCore.Typeclass (overrideInstance)
import Purvasm.NCore.Types (FieldPos(..), Occurrunce, Var(..))
import Purvasm.Primitives (Primitive(..))
import Purvasm.Record (mkRecordSignature)
import Purvasm.Types (BlockTag(..), Ident(..), StructuredConstant(..))

newtype TranslM a = TranslM (TranslEnv -> TranslEnv /\ a)

derive instance Functor TranslM
instance Apply TranslM where
  apply (TranslM k1) (TranslM k2) = TranslM \tenv ->
    let
      tenv' /\ f = k1 tenv
      tenv'' /\ a = k2 tenv'
    in
      tenv'' /\ f a

instance Applicative TranslM where
  pure a = TranslM \tenv -> tenv /\ a

instance Bind TranslM where
  bind (TranslM m) f = TranslM \tenv ->
    let
      tenv' /\ a = m tenv
      TranslM k = f a
    in
      k tenv'

instance Monad TranslM

get :: TranslM TranslEnv
get = TranslM \tenv -> tenv /\ tenv

update :: (TranslEnv -> TranslEnv) -> TranslM Unit
update f = TranslM \tenv -> (f tenv) /\ unit

put :: TranslEnv -> TranslM Unit
put tenv = update (const tenv)

eachWithCurrentEnv :: forall t a b. Traversable t => (a -> TranslM b) -> t a -> TranslM (t b)
eachWithCurrentEnv f ta = do
  tenv <- get
  traverse (\a -> put tenv *> f a) ta

putStatic :: Ident -> ECF.Expr ECF.Ann -> TranslM Unit
putStatic ident expr = update (\tenv -> tenv { static = Array.cons (ident /\ expr) tenv.static })

searchVar :: Ident -> TranslM (Maybe (Int /\ VariableDesc /\ Occurrunce))
searchVar ident = get >>= \{ locals } -> go 0 locals
  where
  go i = case _ of
    Tnull -> pure Nothing
    Tenv desc vars tenv
      | Just occur <- HM.lookup ident vars -> pure $ Just $ i /\ desc /\ occur
      | otherwise -> go (i + 1) tenv

freshVar :: TranslM Ident
freshVar = TranslM \tenv ->
  (tenv { fresh = tenv.fresh + 1 }) /\ Ident ("$pvsm_v" <> show tenv.fresh)

runTransl :: forall a. TranslEnv -> TranslM a -> TranslEnv /\ a
runTransl tenv (TranslM k) = k tenv

type Translate a = ECF.Expr ECF.Ann -> TranslM a

translateProgram :: GlobalEnv -> ECF.Module ECF.Ann -> Program
translateProgram genv (ECF.Module ecfModule@{ name: moduleName }) = do
  let
    decls = ecfModule.decls <#> \(ECF.Binding ident expr) ->
      let
        tenv =
          { moduleName: ecfModule.name
          , locals: Tnull
          , globals: genv
          , static: []
          , fresh: 0
          , isToplevel: true
          }
        _ /\ lambda = runTransl tenv (translateExpr ident expr)
      in
        case lambda of
          NCNone -> Nothing
          _ -> Just { name: ident, lambda }

    { left: foreigns, right: foreignDelcs } = partitionEither $
      ecfModule.foreigns <#> \foreign_ ->
        let
          gloname = mkGlobalName moduleName foreign_
        in
          case Global.lookupValue gloname genv of
            Just val
              | ValPrim { prim, arity } <- val.desc ->
                  Right $ { name: foreign_, lambda: mkPrimDecl prim arity }
            _ -> overrideForeign genv gloname
              # maybe (Left foreign_) ({ name: foreign_, lambda: _ } >>> Right)

    unarrangedProgram =
      Program $
        { name: moduleName
        , static: []
        , decls: foreignDelcs <> catMaybes decls
        , foreigns
        }

  arrangeDecls unarrangedProgram
  where
  mkPrimDecl prim = case _ of
    0 -> NCPrim prim []
    arity -> NCFunction arity $
      NCPrim prim (unfoldr unfoldArg arity)
  unfoldArg n
    | n > 0 = Just $ (NCVar VarUnknown (Var (n - 1))) /\ (n - 1)
    | otherwise = Nothing

translateExpr :: Ident -> Translate NCore
translateExpr ident = go
  where
  -- translation for nested subexpression
  goRec exp = update (\tenv -> tenv { isToplevel = false }) >>= \_ -> go exp

  go = case _ of
    ECF.ExprLit _ lit -> NCConst <$> (constantOfLiteral lit)
    ECF.ExprVar _ var -> searchVar var >>= case _ of
      Nothing -> unsafeCrashWith $ "translateExpr: Unknown variable!" <> show var
      Just (i /\ desc /\ occur) ->
        let
          accessorPrim = case _ of
            PosIndex n -> PGetField n
            PosProp p -> PGetRecordField p
        in
          pure $
            L.foldr (\o lambda -> NCPrim (accessorPrim o) [ lambda ]) (NCVar desc $ Var i) occur
    ECF.ExprGlobal _ gname -> do
      { globals } <- get
      case Global.lookupConstructor gname globals of
        Just desc
          | desc.arity == 0 -> pure $ NCPrim (PMakeBlock (TConstr desc.tag)) []
        _ -> pure $ NCPrim (PGetGlobal gname) []
    ECF.ExprArray _ exprs -> NCPrim (PMakeBlock TArray)
      <$> (eachWithCurrentEnv goRec exprs)

    -- Record and Typeclass instances
    ECF.ExprRecord _ props -> do
      { globals: globals@(GlobalEnv { recordTypes }) } <- get
      let
        recordSig = mkRecordSignature (ECF.propKey <$> props)
      case Global.lookupRecordType Nothing recordSig globals of
        Nothing -> unsafePerformEffect do
          logShow $ recordTypes
          throw $ "translateExpr: Unknown record type: " <> "\n" <> show recordTypes
        Just recordId -> NCPrim (PMakeBlock $ TRecord recordId)
          <$> eachWithCurrentEnv goRec (ECF.propValue <$> props)
    -- Typeclass related should be static.
    exp@(ECF.ExprTypeclass _ _) -> putStatic ident exp $> NCNone
    -- Typeclass instance is static iff all members are static
    ECF.ExprTypeclassInstance _ clsName members -> do
      { moduleName, globals } <- get
      case overrideInstance globals clsName (mkGlobalName moduleName ident) of
        Just impl -> pure impl
        _ -> NCPrim (PMakeBlock (TDict clsName))
          <$> eachWithCurrentEnv goRec members
    -- Constructor. If every argument is constant, translate to blok constant. 
    ECF.ExprConstruct _ desc args -> do
      sequence <$> (traverse constantOfExpr args) >>= case _ of
        Just constArgs -> pure $ NCConst $ SCBlock (TConstr desc.tag) constArgs
        _ -> NCPrim (PMakeBlock (TConstr desc.tag))
          <$> eachWithCurrentEnv goRec args

    -- Let and Letrec.
    ECF.ExprLet _ binds body ->
      NClet <$> traverse translLetBind binds <*> (goRec body)
    ECF.ExprLetRec _ binds body -> do
      -- Well... is it possible to recursive binding group has a member 
      -- which should be treated as known variable (other local variable, global, typeclass dict, etc)
      -- I did not come up with such an example X(
      for_ binds \(bindName /\ _) -> do
        update (extendByNewVar bindName VarUnknown)
      NCletrec
        <$> traverse (goRec <<< snd) binds
        <*> goRec body

    ECF.ExprAbs _ params body -> do
      { moduleName, isToplevel } <- get
      constraints <- getConstraints (mkGlobalName moduleName ident)
      forWithIndex_ params \i param -> do
        let
          desc =
            if isToplevel then (constraints !! i) # join # maybe VarUnknown VarTypeclass
            else VarUnknown
        update (extendByNewVar param desc)
      NCFunction (Array.length params) <$> goRec body
      where
      getConstraints globalIdent
        | Ident ident' <- Global.identOfGlobalName globalIdent
        , [ clsInstance, method ] <- Str.split (Str.Pattern "$") ident' = do
            { moduleName, globals } <- get
            case Global.lookupValue (mkGlobalName moduleName $ Ident method) globals of
              Just { constraints, desc: ValTypeclassMember _ } -> pure $ Array.drop 1 constraints
              _ -> pure []
        | otherwise = do
            { globals } <- get
            pure
              $ maybe [] _.constraints
              $ Global.lookupValue globalIdent globals

    ECF.ExprApp _ func args -> do
      spine <- eachWithCurrentEnv goRec $ Array.cons func args
      case Array.uncons spine of
        Just { head, tail } -> pure $ NCApply head tail
        _ -> unsafeCrashWith "translateExpr: Impossible"
    -- Pattern matching compilation
    ECF.ExprCase _ casHeads casAlts -> do
      mbBinds <- for casHeads \exp -> do
        if not $ immediateExpr exp then do
          var <- freshVar
          pure $ Just (var /\ exp)
        else do
          pure Nothing
      if Array.all isNothing mbBinds then translExprCase casHeads casAlts
      else do
        let
          casHeads' = Array.zipWith
            (\exp -> fromMaybe exp <<< map (ECF.ExprVar ECF.emptyAnn <<< fst))
            casHeads
            mbBinds
        goRec $
          ECF.ExprLet ECF.emptyAnn
            (Array.catMaybes mbBinds)
            (ECF.ExprCase ECF.emptyAnn casHeads' casAlts)

    -- Record property access.
    ECF.ExprAccess _ exp prop
      -- | Just sig <- lookupField exp prop -> case offsetOfProp prop sig of
      --     Just ofs -> NCPrim (PGetField ofs) [ go exp ]
      --     Nothing -> unsafeCrashWith "translateExpr: Record property missing!"
      | otherwise ->
          do
            { globals } <- get
            trExp <- goRec exp
            -- Access to typeclass member should be resolved statically
            case typeclassOfNCoreExpr globals trExp of
              Just (_ /\ { members })
                | Just i <- Array.findIndex (fst >>> (_ == Ident prop)) members ->
                    pure $ NCPrim (PGetField i) [ trExp ]
              _ -> pure $ NCPrim (PGetRecordField prop) [ trExp ]
    -- Misc.
    ECF.ExprGetField _ i exp -> NCPrim (PGetField i) <<< Array.singleton <$> goRec exp
    ECF.ExprGetSize _ exp -> NCPrim PBlockSize <<< Array.singleton <$> goRec exp
    ECF.ExprStaticFail _ -> pure NCStaticFail
    ECF.ExprstaticHandle _ e1 e2 -> NCStaticHandle <$> (goRec e1) <*> (goRec e2)
    ECF.ExprNone -> pure NCNone
    ECF.ExprNil _ -> pure NCNil
    ECF.ExprIf _ cond ifSo notSo -> unsafePartial do
      [ trCond, trSo, trNot ] <- eachWithCurrentEnv goRec [ cond, ifSo, notSo ]
      pure $ NCifthenelse trCond trSo trNot
    exp -> unsafeCrashWith $ "Not Implemented!" <> show exp

  translExprCase :: Array (ECF.Expr ECF.Ann) -> Array (ECF.CaseAlternative ECF.Ann) -> _
  translExprCase caseHeads caseAlts =
    let
      pm = PatternMatching
        { pmHeads: caseHeads
        , pmMatrix: mapWithIndex (\i (ECF.CaseAlternative { patterns }) -> { pats: patterns, action: i }) caseAlts
        }
    in
      translDecisionTree $ MatchComp.decomposePatternMatch pm
    where
    translDecisionTree :: MatchComp.DecisionTree -> _ NCore
    translDecisionTree = case _ of
      MatchComp.MatchFail -> pure $ NCStaticFail
      MatchComp.MatchLeaf act
        | Just (ECF.CaseAlternative { action, patterns }) <- caseAlts !! act ->
            extendByPatternVars patterns
              *> goRec action
        | otherwise -> unsafeCrashWith "translDecisionTree: Impossible!"
      MatchComp.Conditional exp tbl -> NCConditional
        <$> goRec exp
        <*> traverse (\(ac /\ subtree) -> (ac /\ _) <$> translDecisionTree subtree) tbl
      MatchComp.JumpThru exp tbl -> NCSwitch
        <$> goRec exp
        <*> traverse (\(tag /\ subtree) -> (tag /\ _) <$> translDecisionTree subtree) tbl
      MatchComp.MatchTry tree1 tree2 -> NCStaticHandle
        <$> translDecisionTree tree1
        <*> translDecisionTree tree2

    extendByPatternVars :: Array ECF.Pattern -> TranslM _
    extendByPatternVars pats = forWithIndex_ pats \i pat ->
      case caseHeads !! i of
        Just casHead -> extendByPattern casHead pat
        Nothing -> unsafeCrashWith "extendByPatternVars: Impossible!"

  extendByPattern :: ECF.Expr ECF.Ann -> ECF.Pattern -> TranslM _
  extendByPattern exp pat = for (occurrenceOfPat L.Nil pat) \(var /\ o) -> do
    update (extendEnvByBind var (Just o) exp)

  translLetBind :: Ident /\ ECF.Expr ECF.Ann -> TranslM NCore
  translLetBind (bindName /\ exp) = case exp of
    _ -> do
      goRec exp
        <* case exp of
          ECF.ExprVar _ id -> update (addNewNameToBoundVar id bindName Nothing)
          _ -> update (extendByNewVar bindName VarUnknown)

occurrenceOfPat :: Occurrunce -> ECF.Pattern -> Array (Ident /\ Occurrunce)
occurrenceOfPat o = case _ of
  ECF.PatVar var -> [ var /\ o ]
  ECF.PatAliase s pat -> [ s /\ o ] <> occurrenceOfPat o pat
  ECF.PatConstruct _ subpats -> occurrenceOfSubPats subpats
  ECF.PatArray subpats -> occurrenceOfSubPats subpats
  ECF.PatRecord subpats -> occurrenceOfRecordSubpats subpats
  _ -> []
  where
  occurrenceOfSubPats subpats = subpats
    # mapWithIndex (\i pat -> occurrenceOfPat (PosIndex i : o) pat)
    # join
  occurrenceOfRecordSubpats subpats = subpats
    <#> (\(ECF.Prop prop pat) -> occurrenceOfPat (PosProp prop : o) pat)
    # join

immediateExpr :: ECF.Expr ECF.Ann -> Boolean
immediateExpr = case _ of
  ECF.ExprLit _ _ -> true
  ECF.ExprVar _ _ -> true
  ECF.ExprGlobal _ _ -> true
  ECF.ExprTypeclass _ _ -> true
  ECF.ExprTypeclassInstance _ _ _ -> true
  _ -> false

extendByFuncParams :: Array Ident -> TranslEnv -> TranslEnv
extendByFuncParams = flip go
  where
  go tenv = Array.uncons >>> case _ of
    Nothing -> tenv
    Just { head, tail } ->
      let
        locals = Tenv VarUnknown (HM.singleton head L.Nil) tenv.locals
      in
        go (tenv { locals = locals }) tail

addNewNameToBoundVar :: Ident -> Ident -> Maybe VariableDesc -> TranslEnv -> TranslEnv
addNewNameToBoundVar var newVar mbDesc tenv = tenv { locals = locals' tenv.locals }
  where
  locals' = case _ of
    Tnull -> unsafeCrashWith "addNewNameToBoundVar: Unbound var!"
    Tenv desc vars tenv'
      | Just occur <- HM.lookup var vars ->
          Tenv
            (fromMaybe desc mbDesc)
            (HM.insert newVar occur vars)
            tenv'
      | otherwise -> Tenv desc vars (locals' tenv')

extendEnvByBind :: Ident -> Maybe Occurrunce -> ECF.Expr ECF.Ann -> TranslEnv -> TranslEnv
extendEnvByBind name occur exp tenv = tenv { locals = extendEnv tenv.locals }
  where
  extendEnv locals =
    case exp of
      ECF.ExprGlobal _ _ -> unsafeCrashWith "Ho!"
      ECF.ExprVar _ var -> bindNewName name var locals
      _ -> Tenv VarUnknown (HM.insert name L.Nil HM.empty) locals

  bindNewName newName to = case _ of
    Tnull -> unsafeCrashWith $ "extendEnvByBinds: Unbound name!" <> show to
    Tenv desc names env
      | Just var <- HM.lookup to names -> Tenv desc (HM.insert newName (fromMaybe var occur) names) env
      | otherwise -> Tenv desc names (bindNewName newName to env)

constantOfExpr :: Translate (Maybe StructuredConstant)
constantOfExpr = case _ of
  ECF.ExprLit _ lit -> Just <$> constantOfLiteral lit
  _ -> pure Nothing

constantOfLiteral :: ECF.Literal -> TranslM StructuredConstant
constantOfLiteral = case _ of
  ECF.LitAtomic ac -> pure $ SCAtom ac
  ECF.LitStruct sl -> case sl of
    ECF.LitArray lits -> SCBlock TArray <$> (traverse constantOfLiteral lits)
    ECF.LitRecord props -> do
      { globals } <- get
      let recordSig = mkRecordSignature (ECF.propKey <$> props)
      case Global.lookupRecordType Nothing recordSig globals of
        Nothing -> unsafeCrashWith "constantOfLiteral: Unknown record type!"
        Just recordId ->
          SCBlock (TRecord recordId) <$> traverse (ECF.propValue >>> constantOfLiteral) props
    ECF.LitConstructor desc args -> SCBlock (TConstr desc.tag) <$> traverse constantOfLiteral args

typeclassOfNCoreExpr :: GlobalEnv -> NCore -> Maybe (Global.GlobalName /\ Global.TypeclassDesc)
typeclassOfNCoreExpr genv = go
  where
  go = case _ of
    NCVar (VarTypeclass clsname) _ -> (clsname /\ _) <$> Global.lookupTypeclass clsname genv
    NCPrim (PGetField i) [ exp ]
      | Just (cls /\ { members }) <- go exp
      , Just (_ /\ Just constraint) <- members !! i ->
          (constraint /\ _) <$> Global.lookupTypeclass constraint genv
    _ -> Nothing
