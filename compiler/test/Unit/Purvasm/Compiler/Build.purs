-- | Regression coverage for the ADR-0087 build driver (`Purvasm.Compiler.build`/`loadClosure`), the core
-- | of which is otherwise invisible to the existing CLI/e2e gates. Exercised over a `Writer`-based test
-- | monad that records the effect order, with a stub `Backend` (fake IR = `"IR:" <> name`) and a
-- | table-driven `CompilerAction`. Pins: the root-`Missing` → `EntryMissing`, transitively-imported
-- | `Missing` → skip, and `Failed` → `onLoadFailed` + `Left (LoadFailed …)` contracts of `loadClosure`;
-- | the dependency-order `emitFile` … then `emitEntry` sequence; and the `--opt` fixpoint hook iteration.
module Test.Unit.Purvasm.Compiler.Build where

import Prelude

import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import PureScript.CoreFn.Module (Module) as CF
import Purvasm.Compiler (Backend, BuildError(..), BuildProducts, CompilerAction, CompilerActionHooks, LoadResult(..), Options, build, defaultHooks)
import Purvasm.Compiler.Bytecode.Artifact (interfaceFromExports)
import Purvasm.Compiler.CESK.Translate (nameKey)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- The recording monad: a log of tagged events in effect order.
type M = Writer (Array String)

-- A minimal CoreFn module: a single-segment name and its imports; no decls/exports (an empty `AnfModule`,
-- which is all the driver's load/emit orchestration needs to exercise).
modl :: String -> Array String -> CF.Module
modl name imports =
  { name: [ name ]
  , path: ""
  , builtWith: ""
  , imports: map (\i -> { ann, moduleName: [ i ] }) imports
  , exports: []
  , reExports: Object.empty
  , foreignNames: []
  , decls: []
  }
  where
  ann = { span: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }, meta: Nothing }

loaded :: CF.Module -> LoadResult
loaded m = Loaded { path: "path/" <> nameKey m.name, mod: m }

-- A stub backend: fake per-module IR `"IR:" <> name`, a stub interface, a single entry IR.
testBackend :: Backend Unit String
testBackend =
  { context: const unit
  , interfaceOf: \_ lm -> interfaceFromExports { name: lm.module.name, imports: [], exports: [] }
  , lowerModule: \_ lm -> "IR:" <> lm.module.name
  , lowerEntry: \_ _ -> "IR:entry"
  }

-- The table-driven action: `loadModule` looks a name up (absent ⇒ `Missing`), and every effect appends a
-- tagged event to the log.
mkAction :: Map String LoadResult -> Int -> CompilerAction String M
mkAction table maxIter =
  { workdir: "."
  , maxOptimizeIter: maxIter
  , loadModule: \name -> tell [ "load:" <> name ] $> fromMaybe Missing (Map.lookup name table)
  , emitFile: \art -> tell [ "emitFile:" <> art.backendIR ] $> ("out/" <> art.backendIR <> ".ll")
  , emitEntry: \o -> tell [ "emitEntry:" <> o ] $> "out/entry.ll"
  , hooks: recHooks
  }

recHooks :: CompilerActionHooks M
recHooks = defaultHooks
  { onLoadFailed = \e -> tell [ "onLoadFailed:" <> e.moduleName ]
  , onStartCompile = \p m -> tell [ "start:" <> nameKey m.name <> ":" <> show p.current <> "/" <> show p.total ]
  , onEnterOptimizeIter = \am -> tell [ "enterOpt:" <> am.name ]
  , onContinueOptimizeIter = \n am -> tell [ "contOpt:" <> show n <> ":" <> am.name ]
  , onLeaveOptimizeIter = \am -> tell [ "leaveOpt:" <> am.name ]
  }

baseOptions :: Options
baseOptions = { entryModule: "Main", entryName: "main", isEffect: true, opt: false }

-- Run the driver and split the result from the event log.
run :: Map String LoadResult -> Options -> Int -> Tuple (Either BuildError (BuildProducts String)) (Array String)
run table opts maxIter = runWriter (build testBackend (mkAction table maxIter) opts)

-- Keep only the log events whose tag has the given prefix.
withPrefix :: String -> Array String -> Array String
withPrefix pfx = Array.filter (\s -> isJust (stripPrefix (Pattern pfx) s))

-- A tag for the `BuildError`/success outcome, avoiding an `Eq`/`Show` instance on the public error type.
outcome :: forall o. Either BuildError (BuildProducts o) -> String
outcome = case _ of
  Left (EntryMissing n) -> "EntryMissing:" <> n
  Left (LoadFailed e) -> "LoadFailed:" <> e.moduleName
  Right _ -> "Right"

-- A diamond: Main → {A, B} → C.
diamond :: Map String LoadResult
diamond = Map.fromFoldable
  [ Tuple "Main" (loaded (modl "Main" [ "A", "B" ]))
  , Tuple "A" (loaded (modl "A" [ "C" ]))
  , Tuple "B" (loaded (modl "B" [ "C" ]))
  , Tuple "C" (loaded (modl "C" []))
  ]

spec :: Spec Unit
spec = describe "Purvasm.Compiler.build" do
  describe "loadClosure contracts" do
    it "fails the build when the root entry is Missing (EntryMissing)" do
      let Tuple result _ = run Map.empty baseOptions 1
      outcome result `shouldEqual` "EntryMissing:Main"

    it "skips a transitively-imported Missing module (e.g. Prim) and still succeeds" do
      -- C imports Prim, which is absent from the table ⇒ Missing ⇒ skipped, not an error.
      let
        table = Map.insert "C" (loaded (modl "C" [ "Prim" ])) diamond
        Tuple result log = run table baseOptions 1
      outcome result `shouldEqual` "Right"
      -- Prim is attempted (one load) but never emitted.
      Array.elem "load:Prim" log `shouldEqual` true
      Array.elem "emitFile:IR:Prim" log `shouldEqual` false

    it "halts on a Failed load, firing onLoadFailed then Left (LoadFailed)" do
      let
        table = Map.insert "A" (Failed { moduleName: "A", detail: "boom" }) diamond
        Tuple result log = run table baseOptions 1
      outcome result `shouldEqual` "LoadFailed:A"
      Array.elem "onLoadFailed:A" log `shouldEqual` true
      -- The build short-circuits: nothing is emitted.
      withPrefix "emitFile" log `shouldEqual` []

  describe "orchestration order" do
    it "emits modules in dependency order, then the entry object last" do
      let
        Tuple result log = run diamond baseOptions 1
        emits = Array.filter (\s -> isJust (stripPrefix (Pattern "emitFile") s) || isJust (stripPrefix (Pattern "emitEntry") s)) log
      outcome result `shouldEqual` "Right"
      emits `shouldEqual`
        [ "emitFile:IR:C", "emitFile:IR:A", "emitFile:IR:B", "emitFile:IR:Main", "emitEntry:IR:entry" ]

    it "returns one product per module plus the entry" do
      let Tuple result _ = run diamond baseOptions 1
      case result of
        Right products -> do
          Array.length products.modules `shouldEqual` 4
          products.entry.path `shouldEqual` "out/entry.ll"
        Left _ -> "expected Right" `shouldEqual` "got Left"

  describe "optimiser fixpoint iteration (driver-owned, ADR-0087)" do
    it "converges immediately on an idempotent pass: enter + leave, no changing round (--opt)" do
      -- The seam's only pass (DictElim) is idempotent, so round 1 leaves the module unchanged and the loop
      -- stops on convergence — `onContinueOptimizeIter` (fired only for a *changing* round) never runs, and
      -- the `maxOptimizeIter` cap (5 here) is never reached.
      let
        table = Map.singleton "Main" (loaded (modl "Main" []))
        Tuple _ log = run table (baseOptions { opt = true }) 5
      withPrefix "enterOpt" log `shouldEqual` [ "enterOpt:Main" ]
      withPrefix "contOpt" log `shouldEqual` []
      withPrefix "leaveOpt" log `shouldEqual` [ "leaveOpt:Main" ]

    it "runs no optimiser iteration under --no-opt" do
      let
        table = Map.singleton "Main" (loaded (modl "Main" []))
        Tuple _ log = run table (baseOptions { opt = false }) 2
      withPrefix "enterOpt" log `shouldEqual` []
      withPrefix "contOpt" log `shouldEqual` []
