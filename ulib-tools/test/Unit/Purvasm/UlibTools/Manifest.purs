module Test.Unit.Purvasm.UlibTools.Manifest (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.UlibTools.Manifest (Resolution(..), classifyDep, findCycle, inRepoClosure, parseDependencies, parsePackageSet, parseSpagoDependencies, stripVersion)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Purvasm.UlibTools.Manifest" do
    describe "parseDependencies" do
      it "reads the dependencies array" do
        parseDependencies """{"dependencies":["purvasm-json","prelude"]}"""
          `shouldEqual` Right [ "purvasm-json", "prelude" ]
      it "treats a missing field as no deps" do
        parseDependencies "{}" `shouldEqual` Right []
      it "fails on a non-array dependencies field" do
        isLeft (parseDependencies """{"dependencies":5}""") `shouldEqual` true
      it "fails on a non-object top level" do
        isLeft (parseDependencies "[]") `shouldEqual` true

    describe "parseSpagoDependencies" do
      it "reads the first (package) dependencies block, not test deps" do
        parseSpagoDependencies pkgYaml `shouldEqual` [ "prelude", "arrays", "numbers" ]

    describe "parsePackageSet" do
      it "extracts package names from the spago ls packages table" do
        Set.toUnfoldable (parsePackageSet lsOutput) `shouldEqual` [ "abc-parser", "argonaut-core" ]

    describe "stripVersion" do
      it "drops a trailing semver" do
        stripVersion "foldable-traversable-6.0.0" `shouldEqual` "foldable-traversable"
        stripVersion "arrays-7.3.0" `shouldEqual` "arrays"
      it "keeps a name with no version" do
        stripVersion "somegitpkg" `shouldEqual` "somegitpkg"

    describe "classifyDep" do
      let sets = { inRepo: Set.fromFoldable [ "purvasm-json" ], resolved: Set.fromFoldable [ "prelude" ], inSet: Set.fromFoldable [ "prelude", "lens" ] }
      it "prefers in-repo, then resolved, then set, else unknown" do
        classifyDep sets "purvasm-json" `shouldEqual` InRepo
        classifyDep sets "prelude" `shouldEqual` Resolved
        classifyDep sets "lens" `shouldEqual` UnresolvedRegistry
        classifyDep sets "nope" `shouldEqual` Unknown

    describe "findCycle" do
      it "finds no cycle in an acyclic graph" do
        findCycle (Map.fromFoldable [ t "a" [ "b" ], t "b" [ "c" ] ]) `shouldEqual` Nothing
      it "detects a direct cycle and names the path" do
        findCycle (Map.fromFoldable [ t "a" [ "b" ], t "b" [ "a" ] ])
          `shouldEqual` Just [ "a", "b", "a" ]

    describe "inRepoClosure" do
      it "collects the transitive in-repo deps, ignoring registry leaves" do
        let
          inRepo = Set.fromFoldable [ "a", "b", "c" ]
          depMap = Map.fromFoldable [ t "a" [ "b", "prelude" ], t "b" [ "c" ] ]
        Set.toUnfoldable (inRepoClosure inRepo depMap [ "a" ])
          `shouldEqual` [ "a", "b", "c" ]
  where
  t k v = Tuple k v

pkgYaml :: String
pkgYaml =
  """package:
  name: purvasm-json
  dependencies:
    - prelude
    - arrays
    - numbers
  test:
    main: Test.Main
    dependencies:
      - spec
      - spec-node
"""

lsOutput :: String
lsOutput =
  """+------------+---------+----------+
| Package    | Version | Location |
+------------+---------+----------+
| abc-parser | 2.1.0   | -        |
| argonaut-core | 7.0.0 | -       |
+------------+---------+----------+
"""
