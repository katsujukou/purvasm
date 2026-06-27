module Test.Unit.Purvasm.UlibTools.Stage (spec) where

import Prelude

import Purvasm.UlibTools.Stage (modulePath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Purvasm.UlibTools.Stage" do
    describe "modulePath" do
      it "maps a dotted module name to its slash path" do
        modulePath "Data.Array" `shouldEqual` "Data/Array"
        modulePath "Data.Array.ST" `shouldEqual` "Data/Array/ST"
      it "leaves a single-segment name unchanged" do
        modulePath "Prelude" `shouldEqual` "Prelude"
