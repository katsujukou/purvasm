module Test.Unit.Purvasm.UlibTools.Verify (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Set as Set
import Purvasm.UlibTools.Verify (diffSurface, parseSurface)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Purvasm.UlibTools.Verify" do
    describe "parseSurface" do
      it "collects exports and flattened reExports into a name set" do
        parseSurface """{"exports":["a","b"],"reExports":{"Mod":["c"],"Mod2":["d"]}}"""
          `shouldEqual` Right (Set.fromFoldable [ "a", "b", "c", "d" ])
      it "tolerates a missing reExports field" do
        parseSurface """{"exports":["a"]}""" `shouldEqual` Right (Set.fromFoldable [ "a" ])
      it "fails on a non-object top level" do
        isLeft (parseSurface "[]") `shouldEqual` true
      it "fails on invalid JSON" do
        isLeft (parseSurface "not json") `shouldEqual` true

    describe "diffSurface" do
      it "reports registry exports dropped by the patch as missing" do
        let
          d = diffSurface "M"
            (Set.fromFoldable [ "a", "b", "c" ])
            (Set.fromFoldable [ "a", "c" ])
        d.missing `shouldEqual` [ "b" ]
        d.extra `shouldEqual` []
      it "reports patch-only exports as extra" do
        let
          d = diffSurface "M"
            (Set.fromFoldable [ "a" ])
            (Set.fromFoldable [ "a", "z" ])
        d.missing `shouldEqual` []
        d.extra `shouldEqual` [ "z" ]
      it "is clean when surfaces match" do
        let
          d = diffSurface "M"
            (Set.fromFoldable [ "a", "b" ])
            (Set.fromFoldable [ "b", "a" ])
        d.missing `shouldEqual` []
        d.extra `shouldEqual` []
