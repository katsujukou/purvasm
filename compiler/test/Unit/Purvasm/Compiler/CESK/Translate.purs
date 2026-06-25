-- | Invariants of `translExpr` (CoreFn → CESK AST) that the types cannot enforce:
-- | newtype constructors are *erased* to the identity (ADR-0018), and a module-qualified
-- | name lowers to its full dot-separated key.
module Test.Unit.Purvasm.Compiler.CESK.Translate where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CoreFn.Ann (Ann, Meta(..))
import PureScript.CoreFn.Expr (Expr(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.CESK.Translate (qualifiedKey, translExpr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nullAnn :: Ann
nullAnn = { span: { start: z, end: z }, meta: Nothing }
  where
  z = { line: 0, column: 0 }

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

    it "keeps a data constructor as a TmCtor carrying its field arity" do
      translExpr (CF.Constructor nullAnn "Maybe" "Just" [ "value" ])
        `shouldEqual` TmCtor "Just" 1

    it "lowers a local (unqualified) variable to its bare name" do
      translExpr (CF.Var nullAnn (CF.Qualified Nothing "x")) `shouldEqual` TmVar "x"
