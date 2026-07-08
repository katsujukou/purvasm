-- | The `ANF → Pmi` path. `gdefKindMap` must classify native gdefs into exactly boot's
-- | bytecode `ExportKind`s (the `Grec` non-lambda member → `Erec`, not `Ecaf`, is the subtle case), and
-- | `interfaceOfAnf` must produce a `.pmi` byte-identical to `interfaceOf . compileModule` over the same
-- | source — verified here on the `Slice1` fixture (the whole-stdlib 607-module sweep is a one-off, not
-- | committed).
module Test.Unit.Purvasm.Compiler.Backend.LLVM.Interface where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Module (Module)
import Purvasm.Compiler.Backend.LLVM.Driver (gdefsOfModule)
import Purvasm.Compiler.Backend.LLVM.Interface (gdefKindMap, interfaceOfAnf)
import Purvasm.Compiler.Backend.LLVM.Types (Gdef(..))
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, kindToTag)
import Purvasm.Compiler.Compile (compileModule)
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

parseModule :: String -> Either String Module
parseModule src = jsonParser src >>= (lmap printJsonDecodeError <<< decodeModule)

-- A body placeholder; only its lambda-vs-not shape matters to the kind map.
body :: Expr
body = Ret (CAtom (AtomVar "x"))

spec :: Spec Unit
spec = describe "Purvasm.Compiler.Backend.LLVM.Interface" do
  describe "gdefKindMap" do
    it "classifies each native gdef to boot's ExportKind (fn/caf/recfn/rec)" do
      let
        gdefs =
          [ Gfun "M.f" [ "a", "b" ] body
          , Gcaf "M.x" body
          , Grec
              [ Tuple "M.g" (Ret (CLam [ "y" ] body)) -- lambda member → recfn
              , Tuple "M.dict" body -- value member → rec (NOT caf)
              ]
          ]
        kinds = gdefKindMap gdefs
        tag k = kindToTag <$> Map.lookup k kinds
      tag "M.f" `shouldEqual` Just "fn2"
      tag "M.x" `shouldEqual` Just "caf"
      tag "M.g" `shouldEqual` Just "recfn1"
      tag "M.dict" `shouldEqual` Just "rec"
      tag "M.absent" `shouldEqual` Nothing

  describe "interfaceOfAnf" do
    it "computes a .pmi byte-identical to interfaceOf . compileModule (no Pmo detour)" do
      src <- liftEffect (readTextFile UTF8 "compiler/test/fixtures/slice1/corefn.json")
      case parseModule src of
        Left err -> fail ("CoreFn decode failed: " <> err)
        Right m ->
          interfaceToString (interfaceOfAnf m (gdefsOfModule m))
            `shouldEqual` interfaceToString (interfaceOf (compileModule m))
