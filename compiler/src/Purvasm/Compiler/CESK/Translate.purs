module Purvasm.Compiler.CESK.Translate where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import PureScript.CoreFn.Ann (Meta(..)) as CF
import PureScript.CoreFn.Expr (Bind(..), Binder(..), Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Module (Module) as CF
import PureScript.CoreFn.Names (ModuleName, Qualified(..), Ident) as CF
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.CESK.AST (Rhs(..), Term(..))
import Purvasm.Compiler.CESK.AST as Cesk
import Purvasm.Compiler.Literal (Literal(..))

-- | A dotted module name, e.g. `["Data", "Maybe"]` → `"Data.Maybe"` (boot's `name_key`).
nameKey :: CF.ModuleName -> String
nameKey = joinWith "."

qualifiedKey :: CF.ModuleName -> CF.Ident -> String
qualifiedKey mn id = joinWith "." mn <> "." <> id

translLiteral :: CF.Literal CF.Expr -> Cesk.Term
translLiteral = case _ of
  CF.LitInt i -> TmLit $ LInt i
  CF.LitNumber n -> TmLit $ LNumber n
  CF.LitString s -> TmLit $ LString s
  CF.LitChar ch -> TmLit $ LInt $ fromEnum ch
  CF.LitBoolean b -> TmLit $ LBool b
  CF.LitArray xs -> TmArray (translExpr <$> xs)
  CF.LitObject kvs -> TmRecord (kvs <#> \(prop /\ e) -> { prop, term: translExpr e })

translVar :: CF.Qualified CF.Ident -> Cesk.Term
translVar = case _ of
  CF.Qualified Nothing id -> Cesk.TmVar id
  CF.Qualified (Just m) id -> Cesk.TmVar (qualifiedKey m id)

translExpr :: CF.Expr -> Cesk.Term
translExpr = case _ of
  CF.Literal _ lit -> translLiteral lit
  CF.Constructor ann _ ctor args
    -- a newtype constructor is erased - it is the identity (ADR-0018). 
    -- PureScript emits newtype ctor as identity *declatations*, so a `Constructor` node wih
    -- this meta should not occur; handled defensively.
    | { meta: Just CF.IsNewtype } <- ann
    , [ arg ] <- args -> Cesk.TmLam "$x" (TmVar "$x")
    | otherwise -> TmCtor ctor (length args)
  CF.Accessor _ fld r -> TmAccessor (translExpr r) fld
  CF.ObjectUpdate _ r _copy updates -> TmUpdate (translExpr r) (updates <#> \(prop /\ e) -> { prop, term: translExpr e })
  CF.Abs _ arg body -> TmLam arg (translExpr body)
  CF.App _ f x -> TmApp (translExpr f) (translExpr x)
  CF.Var _ q -> translVar q
  CF.Case _ scrutinees alts -> TmCase (translExpr <$> scrutinees) (translAlt <$> alts)
  -- a `let` expression: its bindings are local, so keys stay bare (boot's `id`).
  CF.Let _ binds body -> translBinds identity binds (translExpr body)

  where
  translAlt alt =
    { binders: translBinder <$> alt.binders
    , result:
        case alt.result of
          Right e -> Unconditional (translExpr e)
          Left guards -> Guarded $ guards <#>
            \{ guard, expression } ->
              { guard: translExpr guard
              , rhs: translExpr expression
              }
    }

  translBinder :: CF.Binder -> Binder
  translBinder = case _ of
    CF.NullBinder _ -> BNull
    CF.VarBinder _ id -> BVar id
    CF.LiteralBinder _ lit -> litBinder lit
    -- A constructor binder matches on the *bare* constructor name (boot's `BCtor (ctor,
    -- …)`), to agree with the bare tag a `Constructor` expression lowers to.
    CF.ConstructorBinder ann _ (CF.Qualified _ ctor) subs
      | { meta: Just CF.IsNewtype } <- ann
      , [ sub ] <- subs -> translBinder sub
      | otherwise -> BCtor ctor (translBinder <$> subs)
    CF.NamedBinder _ id inner -> BNamed id (translBinder inner)

    where
    litBinder = case _ of
      CF.LitInt i -> BLit $ LInt i
      CF.LitNumber f -> BLit $ LNumber f
      CF.LitString s -> BLit $ LString s
      CF.LitChar ch -> BLit $ LInt $ fromEnum ch
      CF.LitBoolean b -> BLit $ LBool b
      CF.LitArray subs -> BArray (translBinder <$> subs)
      CF.LitObject kvs -> BRecord (kvs <#> \(prop /\ b) -> { prop, binder: translBinder b })

translBinds :: (CF.Ident -> String) -> Array CF.Bind -> Cesk.Term -> Cesk.Term
translBinds key binds body = foldr
  ( \b acc ->
      case b of
        CF.NonRec _ id e -> TmLet (key id) (translExpr e) acc
        CF.Rec rbs ->
          TmLetrec (rbs <#> \{ ident, expr } -> (key ident /\ translExpr expr)) acc
  )
  body
  binds

translModule :: CF.Ident -> CF.Module -> Cesk.Term
translModule entry m =
  let
    key id = qualifiedKey m.name id
  in
    translBinds key m.decls (TmVar (key entry))