module Test.Unit.Purvasm.UlibTools.Manifest (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Purvasm.UlibTools.Manifest (Fidelity(..), Resolution(..), classifyDep, findCycle, inRepoClosure, parseBowerDependencies, parseDependencies, parseForeign, parsePackageSet, parseRegistryVersion, parseSpagoDependencies, parseTest, renderForeignManifest, repoSlug, stripVersion)
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

    describe "parseForeign" do
      it "reads the foreign map as key/source pairs" do
        map Map.fromFoldable (parseForeign """{"foreign":{"Data.Show.showNumberImpl":"Data.Show.foreign.c"}}""")
          `shouldEqual` Right (Map.fromFoldable [ Tuple "Data.Show.showNumberImpl" "Data.Show.foreign.c" ])
      it "treats a missing foreign field as no native foreigns" do
        parseForeign """{"dependencies":["prelude"]}""" `shouldEqual` Right []
      it "fails on a non-object foreign field" do
        isLeft (parseForeign """{"foreign":5}""") `shouldEqual` true
      it "fails on a non-string source value" do
        isLeft (parseForeign """{"foreign":{"A.f":42}}""") `shouldEqual` true
      it "fails on a non-object top level" do
        isLeft (parseForeign "[]") `shouldEqual` true

    describe "renderForeignManifest" do
      it "round-trips through parseForeign" do
        let entries = [ Tuple "A.f" "native/p/A.foreign.c", Tuple "B.g" "native/q/B.foreign.c" ]
        map Map.fromFoldable (parseForeign (renderForeignManifest entries))
          `shouldEqual` Right (Map.fromFoldable entries)
      it "renders an empty map as an empty foreign object" do
        parseForeign (renderForeignManifest []) `shouldEqual` Right []

    describe "parseTest" do
      it "treats a missing test block as not-yet-tested" do
        parseTest """{"dependencies":["purvasm-json"]}""" `shouldEqual` Right Nothing
      it "reads a full test block" do
        parseTest fullTest `shouldEqual` Right
          ( Just
              { repo: "https://github.com/purescript/purescript-arrays"
              , ref: "v7.3.0"
              , testMain: "Test.Main"
              , testDeps: [ "assert", "console" ]
              , fidelity: JsFidelity
              , xfail: []
              }
          )
      it "defaults testMain to Test.Main and optional arrays to empty" do
        parseTest """{"test":{"repo":"r","ref":"x","fidelity":"native"}}""" `shouldEqual` Right
          ( Just
              { repo: "r", ref: "x", testMain: "Test.Main", testDeps: [], fidelity: NativeFidelity, xfail: [] }
          )
      it "parses the bespoke fidelity" do
        map (map _.fidelity) (parseTest """{"test":{"repo":"r","ref":"x","fidelity":"bespoke"}}""")
          `shouldEqual` Right (Just BespokeFidelity)
      it "fails on an unknown fidelity" do
        isLeft (parseTest """{"test":{"repo":"r","ref":"x","fidelity":"wasm"}}""") `shouldEqual` true
      it "fails when a required string is missing" do
        isLeft (parseTest """{"test":{"ref":"x","fidelity":"js"}}""") `shouldEqual` true

    describe "parseBowerDependencies" do
      it "reads dependency keys and strips the purescript- prefix" do
        parseBowerDependencies """{"dependencies":{"purescript-prelude":"^6.0.0","purescript-arrays":"^7.0.0"}}"""
          `shouldEqual` Right [ "prelude", "arrays" ]
      it "treats a missing field as no deps" do
        parseBowerDependencies "{}" `shouldEqual` Right []
      it "fails on a non-object dependencies field" do
        isLeft (parseBowerDependencies """{"dependencies":[]}""") `shouldEqual` true

    describe "parseRegistryVersion" do
      it "extracts the registry version from a workspace spago.yaml" do
        parseRegistryVersion "workspace:\n  packageSet:\n    registry: 77.8.0\n  extraPackages: {}\n"
          `shouldEqual` Just "77.8.0"
      it "is Nothing when no registry line is present" do
        parseRegistryVersion "workspace:\n  extraPackages: {}\n" `shouldEqual` Nothing

    describe "repoSlug" do
      it "takes the last path segment without a trailing .git" do
        repoSlug "https://github.com/purescript/purescript-arrays" `shouldEqual` "purescript-arrays"
        repoSlug "https://github.com/purescript/purescript-arrays.git" `shouldEqual` "purescript-arrays"

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

fullTest :: String
fullTest =
  """{
  "test": {
    "repo": "https://github.com/purescript/purescript-arrays",
    "ref": "v7.3.0",
    "testMain": "Test.Main",
    "testDeps": ["assert", "console"],
    "fidelity": "js",
    "xfail": []
  }
}"""

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
