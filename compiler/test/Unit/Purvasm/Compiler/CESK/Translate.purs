-- | Invariants of `translExpr` (CoreFn → CESK AST) that the types cannot enforce:
-- | newtype constructors are *erased* to the identity (ADR-0018); a module-qualified
-- | *value* lowers to its full dot-separated key, but a *constructor* (in an expression
-- | or a binder) keeps its **bare** name — type-directed matching makes that unambiguous,
-- | and it agrees with boot; a `let` expression's bindings are local, so their keys stay
-- | bare too.
module Test.Unit.Purvasm.Compiler.CESK.Translate where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import PureScript.CoreFn.Ann (Ann, Meta(..))
import PureScript.CoreFn.Expr (Bind(..), Binder(..), Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.Binder (Binder(..))
import Purvasm.Compiler.CESK.AST (Rhs(..), Term(..))
import Purvasm.Compiler.CESK.Translate (qualifiedKey, translExpr)
import Purvasm.Compiler.Literal (Literal(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nullAnn :: Ann
nullAnn = { span: { start: z, end: z }, meta: Nothing }
  where
  z = { line: 0, column: 0 }

int :: Int -> CF.Expr
int = CF.Literal nullAnn <<< CF.LitInt

localVar :: String -> CF.Expr
localVar x = CF.Var nullAnn (CF.Qualified Nothing x)

spec :: Spec Unit
spec = describe "Purvasm.Compiler.CESK.Translate" do
  describe "qualifiedKey" do
    it "joins a module name and identifier into a full dot-separated key" do
      qualifiedKey [ "Data", "Maybe" ] "fromMaybe" `shouldEqual` "Data.Maybe.fromMaybe"

    it "distinguishes keys that share a prefix (no run-together collision)" do
      qualifiedKey [ "A", "B" ] "c" `shouldEqual` "A.B.c"

  describe "translExpr" do
    it "erases a single-field newtype constructor to the identity (ADR-0018)" do
      translExpr (CF.Constructor (nullAnn { meta = Just IsNewtype }) "N" "MkN" [ "x" ])
        `shouldEqual` TmLam "$x" (TmVar "$x")

    it "keeps a data constructor as a bare TmCtor carrying its field arity" do
      translExpr (CF.Constructor nullAnn "Maybe" "Just" [ "value" ])
        `shouldEqual` TmCtor "Just" 1

    it "lowers a local (unqualified) variable to its bare name" do
      translExpr (CF.Var nullAnn (CF.Qualified Nothing "x")) `shouldEqual` TmVar "x"

    it "matches a constructor binder on its bare name, not the module-qualified one" do
      translExpr
        ( CF.Case nullAnn [ localVar "s" ]
            [ { binders:
                  [ CF.ConstructorBinder nullAnn (CF.Qualified (Just [ "M" ]) "T") (CF.Qualified (Just [ "M" ]) "Mk")
                      [ CF.VarBinder nullAnn "a" ]
                  ]
              , result: Right (localVar "a")
              }
            ]
        )
        `shouldEqual`
          TmCase [ TmVar "s" ]
            [ { binders: [ BCtor "Mk" [ BVar "a" ] ], result: Unconditional (TmVar "a") } ]

    it "lowers a let expression to a TmLet with a bare local key" do
      translExpr (CF.Let nullAnn [ CF.NonRec nullAnn "x" (int 1) ] (localVar "x"))
        `shouldEqual` TmLet "x" (TmLit (LInt 1)) (TmVar "x")

    it "lowers a recursive let to a TmLetrec keeping bare local keys" do
      translExpr (CF.Let nullAnn [ CF.Rec [ { ann: nullAnn, ident: "f", expr: localVar "f" } ] ] (localVar "f"))
        `shouldEqual` TmLetrec [ "f" /\ TmVar "f" ] (TmVar "f")
