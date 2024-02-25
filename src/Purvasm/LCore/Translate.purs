module Purvasm.LCore.Translate where

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Array as Array
import Data.FoldableWithIndex (forWithIndex_)
import Data.HashMap as HM
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String as Str
import Data.Traversable (for, for_, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.ECore.SpecialGlobal (glo_Prim_undefined)
import Purvasm.ECore.Syntax as ECF
import Purvasm.Global (GlobalEnv, ValueDesc(..), mkGlobalName)
import Purvasm.Global as Global
import Purvasm.LCore.Env (LocalSymbolTable(..), TranslEnv, VariableDesc(..), extendByNewVar)
import Purvasm.LCore.MatchComp (PatternMatching(..))
import Purvasm.LCore.MatchComp as MatchComp
import Purvasm.LCore.Syntax (LCore(..))
import Purvasm.LCore.Types (Var(..), Occurrunce)
import Purvasm.Primitives (Primitive(..))
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

translateExpr :: Ident -> Translate LCore
translateExpr ident = go
  where
  -- translation for nested subexpression
  goRec exp = update (\tenv -> tenv { isToplevel = false }) >>= \_ -> go exp

  go = case _ of
    ECF.ExprLit _ lit -> LCConst <$> (constantOfLiteral lit)
    ECF.ExprVar _ var -> searchVar var >>= case _ of
      Nothing -> unsafeCrashWith $ "translateExpr: Unknown variable!" <> show var
      Just (i /\ desc /\ occur) -> pure $
        L.foldr (\o lambda -> LCPrim (PGetField o) [ lambda ]) (LCVar desc $ Var i) occur
    ECF.ExprGlobal _ gname -> do
      { globals } <- get
      case Global.lookupConstructor gname globals of
        Just desc
          | desc.arity == 0 -> pure $ LCPrim (PMakeBlock (TConstr desc.tag)) []
        _ -> pure $ LCPrim (PGetGlobal gname) []
    ECF.ExprArray _ exprs -> LCPrim (PMakeBlock TArray) <$> (traverse goRec exprs)

    -- Record and Typeclass instances
    ECF.ExprRecord _ recId _ -> LCPrim (PMakeBlock $ TRecord recId) <$> pure []
    -- Typeclass related should be static.
    exp@(ECF.ExprTypeclass _ _) -> putStatic ident exp $> LCNone
    -- Typeclass instance is static iff all members are static
    exp@(ECF.ExprTypeclassInstance _ _ _) -> putStatic ident exp $> LCNone
    -- Constructor. If every argument is constant, translate to blok constant. 
    ECF.ExprConstruct _ desc args -> do
      sequence <$> (traverse constantOfExpr args) >>= case _ of
        Just constArgs -> pure $ LCConst $ SCBlock (TConstr desc.tag) constArgs
        _ -> LCPrim (PMakeBlock (TConstr desc.tag)) <$> traverse goRec args

    -- Let and Letrec.
    ECF.ExprLet _ binds body ->
      LClet <$> traverse translLetBind binds <*> (goRec body)
    ECF.ExprLetRec _ binds body -> do
      -- Well... is it possible to recursive binding group has a member 
      -- which should be treated as known variable (other local variable, global, typeclass dict, etc)
      -- I did not come up with such an example X(
      for_ binds \(bindName /\ _) -> do
        update (extendByNewVar bindName VarUnknown)
      LCletrec
        <$> traverse (goRec <<< snd) binds
        <*> goRec body

    ECF.ExprAbs _ params body -> do
      { moduleName, isToplevel } <- get
      constraints <- getConstraints (mkGlobalName moduleName ident)
      forWithIndex_ params \i param -> do
        let
          desc =
            if isToplevel then (constraints !! i) # maybe VarUnknown VarTypeclass
            else VarUnknown
        update (extendByNewVar param desc)
      LCFunction (Array.length params) <$> goRec body
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

    ECF.ExprApp _ func args
      | [ ECF.ExprGlobal _ argname ] <- args
      , argname == glo_Prim_undefined -> do
          { globals } <- get
          trFunc <- goRec func
          -- 制約付きtypeclass instanceにおける制約クラスのインスタンスへの
          -- 参照である場合は、undefinedのAppを取り除く
          case typeclassOfLCoreExpr globals trFunc of
            Just _ -> pure trFunc
            _ -> unsafeCrashWith "Unknown Prim.undefiend"
      | otherwise -> LCApply <$> (goRec func) <*> (traverse goRec args)
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
          _ = unsafePerformEffect do
            logShow $ ECF.ExprLet ECF.emptyAnn
              (Array.catMaybes mbBinds)
              (ECF.ExprCase ECF.emptyAnn casHeads' casAlts)
        goRec $
          ECF.ExprLet ECF.emptyAnn
            (Array.catMaybes mbBinds)
            (ECF.ExprCase ECF.emptyAnn casHeads' casAlts)

    -- Record property access.
    ECF.ExprAccess _ exp prop
      -- | Just sig <- lookupField exp prop -> case offsetOfProp prop sig of
      --     Just ofs -> LCPrim (PGetField ofs) [ go exp ]
      --     Nothing -> unsafeCrashWith "translateExpr: Record property missing!"
      | otherwise ->
          do
            { globals } <- get
            trExp <- goRec exp
            -- Access to typeclass member should be resolved statically
            case typeclassOfLCoreExpr globals trExp of
              Just (_ /\ { members })
                | Just i <- Array.findIndex (fst >>> (_ == Ident prop)) members ->
                    pure $ LCPrim (PGetField i) [ trExp ]
              _ -> pure $ LCPrim (PGetRecordField prop) [ trExp ]
    -- Misc.
    ECF.ExprStaticFail _ -> pure LCStaticFail
    ECF.ExprstaticHandle _ e1 e2 -> LCStaticHandle <$> (goRec e1) <*> (goRec e2)
    ECF.ExprNone -> pure LCNone
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
    translDecisionTree :: MatchComp.DecisionTree -> _ LCore
    translDecisionTree = case _ of
      MatchComp.MatchFail -> pure $ LCStaticFail
      MatchComp.MatchLeaf act
        | Just (ECF.CaseAlternative { action, patterns }) <- caseAlts !! act ->
            extendByPatternVars patterns
              *> goRec action
        | otherwise -> unsafeCrashWith "translDecisionTree: Impossible!"
      MatchComp.Conditional exp tbl -> LCConditional
        <$> goRec exp
        <*> traverse (\(ac /\ subtree) -> (ac /\ _) <$> translDecisionTree subtree) tbl
      MatchComp.JumpThru exp tbl -> LCSwitch
        <$> goRec exp
        <*> traverse (\(tag /\ subtree) -> (tag /\ _) <$> translDecisionTree subtree) tbl
      MatchComp.MatchTry tree1 tree2 -> LCStaticHandle
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

  translLetBind :: Ident /\ ECF.Expr ECF.Ann -> TranslM LCore
  translLetBind (bindName /\ exp) = case exp of
    _ -> do
      goRec exp
        <* case exp of
          ECF.ExprVar _ id -> update (addNewNameToBoundVar id bindName Nothing)
          _ -> update (extendByNewVar bindName VarUnknown)

occurrenceOfPat :: Occurrunce -> ECF.Pattern -> Array (Ident /\ Occurrunce)
occurrenceOfPat o = case _ of
  ECF.PatVar var -> [ var /\ o ]
  ECF.PatConstruct _ subpats -> subpats
    # mapWithIndex (\i pat -> occurrenceOfPat (i : o) pat)
    # join
  ECF.PatAliase s pat -> [ s /\ o ] <> occurrenceOfPat o pat
  _ -> []

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
      ECF.ExprGlobal _ boundTo -> unsafeCrashWith "Ho!"
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
    ECF.LitRecord recId props -> SCBlock (TRecord recId) <$> pure [] -- traverse constantOfLiteral props
    ECF.LitConstructor desc args -> SCBlock (TConstr desc.tag) <$> traverse constantOfLiteral args

typeclassOfLCoreExpr :: GlobalEnv -> LCore -> Maybe (Global.GlobalName /\ Global.TypeclassDesc)
typeclassOfLCoreExpr genv = go
  where
  go = case _ of
    LCVar (VarTypeclass clsname) _ -> (clsname /\ _) <$> Global.lookupTypeclass clsname genv
    LCPrim (PGetField i) [ exp ]
      | Just (cls /\ { members }) <- go exp
      , Just (_ /\ Just constraint) <- members !! i ->
          (constraint /\ _) <$> Global.lookupTypeclass constraint genv
    _ -> Nothing
