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
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Expr (Bind(..), Expr(..)) as CFE
import PureScript.CoreFn.Literal (Literal(..)) as CFL
import PureScript.CoreFn.Module (Module) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CFN
import Purvasm.Compiler (Backend, BuildError(..), BuildProducts, CompilerAction, CompilerActionHooks, LoadResult(..), Options, build, defaultHooks)
import Purvasm.Compiler.Bytecode.Artifact (interfaceFromExports)
import Purvasm.Compiler.MiddleEnd.ANF as ANF
import Purvasm.Compiler.MiddleEnd.Module (AnfModule, declsOfModule)
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (fvExpr)
import Purvasm.Compiler.MiddleEnd.Optimizer.Quarantine (RejectionEvent(..))
import Data.Set as Set
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
  -- ADR-0090 stub: the table fixtures declare no foreigns, so the driver never calls this.
  , foreignSigsOf: \_ -> tell [ "foreignSigs" ] $> Right Map.empty
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
  Left (ForeignSigFailed e) -> "ForeignSigFailed:" <> e.moduleName
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
  backstopSpec
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

  describe "foreign shapes reach downstream codegen (ADR-0089 slice 2 × ADR-0090)" do
    it "a candidate-referenced private foreign's shape is visible in the consumer's LoweredModule" do
      -- Dep declares a *private* foreign `privImpl` and an exported wrapper `\x -> privImpl(x)` —
      -- a publishable inline candidate. Inlining it lands the private reference in Main, so Main's
      -- visible foreign shapes must carry `Dep.privImpl` (the exports-only publication cannot).
      let
        ann0 = { span: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }, meta: Nothing }
        wrapExpr = CFE.Abs ann0 "x"
          ( CFE.App ann0
              (CFE.Var ann0 (CFN.Qualified (Just [ "Dep" ]) "privImpl"))
              (CFE.Var ann0 (CFN.Qualified Nothing "x"))
          )
        depMod = (modl "Dep" [])
          { foreignNames = [ "privImpl" ]
          , exports = [ "wrap" ]
          , decls = [ CFE.NonRec ann0 "wrap" wrapExpr ]
          }
        table = Map.fromFoldable
          [ Tuple "Main" (loaded (modl "Main" [ "Dep" ]))
          , Tuple "Dep" (loaded depMod)
          ]
        -- a backend whose per-module IR lists the module's visible foreign-shape keys
        sigBackend = testBackend
          { lowerModule = \_ lm ->
              "SIGS:" <> lm.module.name <> ":"
                <> Array.intercalate "," (Array.fromFoldable (Map.keys lm.foreignSigs))
          }
        action = (mkAction table 3)
          { foreignSigsOf = \m ->
              tell [ "fsr:" <> nameKey m.name ]
                $> Right (Map.singleton "Dep.privImpl" { arity: 1, vsat: false, retVsat: false })
          }
        Tuple result log = runWriter (build sigBackend action (baseOptions { opt = true }))
      outcome result `shouldEqual` "Right"
      -- FSR ran for the foreign-bearing module only…
      withPrefix "fsr:" log `shouldEqual` [ "fsr:Dep" ]
      -- …and the consumer's codegen sees the private foreign's shape.
      withPrefix "emitFile:SIGS:Main" log `shouldEqual` [ "emitFile:SIGS:Main:Dep.privImpl" ]

-- --- optimizer backstop driver contract (ADR-0089 Addendum 2026-07-16) ----------------------------
--
-- The seam-level quarantine specs (Test.Unit….MiddleEnd.Optimizer) drive `optimizeModule` by hand;
-- these fixtures cover the half the *driver* owns: hook dispatch order (`BackstopFired` per recorded
-- rejection, one `BackstopSummary` with exact counts, before `onLeaveOptimizeIter`), the
-- Fired-only counter semantics, per-module quarantine/counter lifetime, and the `maxOptimizeIter`
-- exhaustion path still emitting the post-backstop module.

bsAnn :: Ann
bsAnn = { span: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }, meta: Nothing }

bsInt :: Int -> CFE.Expr
bsInt = CFE.Literal bsAnn <<< CFL.LitInt

bsObj :: Array (Tuple String CFE.Expr) -> CFE.Expr
bsObj = CFE.Literal bsAnn <<< CFL.LitObject

bsLVar :: String -> CFE.Expr
bsLVar x = CFE.Var bsAnn (CFN.Qualified Nothing x)

bsQVar :: String -> String -> CFE.Expr
bsQVar m x = CFE.Var bsAnn (CFN.Qualified (Just [ m ]) x)

-- | The ADR-0089 backstop blow-up shape, as CoreFn (the seam fixtures' ANF shape, one level up):
-- | `dict = {f:1}`; `b = \d -> let w1..w12 = {k:i} in {p: d.f, w1..w12}` (a <64-node live builder,
-- | publishable, param projected); `big` = ten saturated `b dict` calls kept live by a record tail —
-- | each unfold re-materialises ~50 nodes, so the round output trips the ×4-over-256 cap.
bsTripModule :: String -> CF.Module
bsTripModule mn = (modl mn [])
  { exports = [ "dict", "b", "big" ]
  , decls =
      [ CFE.NonRec bsAnn "dict" (bsObj [ Tuple "f" (bsInt 1) ])
      , CFE.NonRec bsAnn "b" builder
      , CFE.NonRec bsAnn "big" bigBody
      ]
  }
  where
  builder = CFE.Abs bsAnn "d"
    ( CFE.Let bsAnn (map wBind (Array.range 1 12))
        ( bsObj
            ( [ Tuple "p" (CFE.Accessor bsAnn "f" (bsLVar "d")) ]
                <> map (\i -> Tuple ("w" <> show i) (bsLVar ("w" <> show i))) (Array.range 1 12)
            )
        )
    )

  wBind i = CFE.NonRec bsAnn ("w" <> show i) (bsObj [ Tuple "k" (bsInt i) ])

  bigBody = CFE.Let bsAnn (map rBind (Array.range 1 10))
    (bsObj (map (\i -> Tuple ("r" <> show i) (bsLVar ("r" <> show i))) (Array.range 1 10)))

  rBind i = CFE.NonRec bsAnn ("r" <> show i) (CFE.App bsAnn (bsQVar mn "b") (bsQVar mn "dict"))

-- | `recHooks` plus the backstop hook, recording each event as a tagged log line.
bsHooks :: CompilerActionHooks M
bsHooks = recHooks
  { onOptimizerBackstop = case _ of
      BackstopFired r -> tell [ "bsFired:" <> r.key ]
      BackstopSummary s ->
        tell [ "bsSummary:" <> show s.rejectionAttempts <> ":" <> show s.distinctBindings ]
  }

runBs
  :: Backend Unit String
  -> Map String LoadResult
  -> Options
  -> Int
  -> Tuple (Either BuildError (BuildProducts String)) (Array String)
runBs backend table opts maxIter =
  runWriter (build backend ((mkAction table maxIter) { hooks = bsHooks }) opts)

-- | Keep only the log lines carrying one of the given tags, in order.
taggedBy :: Array String -> Array String -> Array String
taggedBy pfxs = Array.filter (\s -> Array.any (\p -> isJust (stripPrefix (Pattern p) s)) pfxs)

-- | The `big` member of a module's ANF (the backstop-kept term the assertions compare).
bsBigOf :: String -> AnfModule -> Maybe ANF.Expr
bsBigOf mn m =
  map (\(Tuple _ e) -> e)
    (Array.find (\(Tuple k _) -> k == (mn <> ".big")) (Array.concatMap _.members m.decls))

backstopSpec :: Spec Unit
backstopSpec = describe "optimizer backstop driver contract (ADR-0089 Addendum)" do
  bsSkipConvergeSpec
  bsSummaryFlowsSpec
  it "dispatches Fired per recorded rejection, one Summary with exact counts, before leaveOpt" do
    let
      table = Map.singleton "TQ" (loaded (bsTripModule "TQ"))
      Tuple result log = runBs testBackend table (baseOptions { opt = true, entryModule = "TQ" }) 10
    outcome result `shouldEqual` "Right"
    taggedBy [ "enterOpt", "contOpt", "leaveOpt", "bs" ] log `shouldEqual`
      [ "enterOpt:TQ"
      , "bsFired:TQ.big" -- round 1 trips and records
      , "contOpt:1:TQ" -- round 1 changed the module (sibling normalisation)
      , "bsFired:TQ.big" -- round 2: the sibling candidates α-renamed ⇒ genuine-fact retry trips again
      , "bsSummary:2:1" -- rejectionAttempts=2, distinctBindings=1, before leaveOpt
      , "leaveOpt:TQ"
      ]

  it "resets counters and quarantine per module fixpoint (nothing persists across the fold)" do
    let
      table = Map.fromFoldable
        [ Tuple "Main" (loaded (modl "Main" [ "TQ1", "TQ2" ]))
        , Tuple "TQ1" (loaded (bsTripModule "TQ1"))
        , Tuple "TQ2" (loaded (bsTripModule "TQ2"))
        ]
      Tuple result log = runBs testBackend table (baseOptions { opt = true }) 10
    outcome result `shouldEqual` "Right"
    -- each tripping module gets its own fresh counters — two independent 2:1 summaries…
    withPrefix "bsSummary" log `shouldEqual` [ "bsSummary:2:1", "bsSummary:2:1" ]
    withPrefix "bsFired" log `shouldEqual`
      [ "bsFired:TQ1.big", "bsFired:TQ1.big", "bsFired:TQ2.big", "bsFired:TQ2.big" ]
    -- …and the entry module's own fixpoint sees no backstop event at all.
    let mainSeg = Array.takeWhile (_ /= "leaveOpt:Main") (Array.dropWhile (_ /= "enterOpt:Main") log)
    withPrefix "bs" mainSeg `shouldEqual` []

  it "still emits the post-backstop module when maxOptimizeIter exhausts mid-blow-up" do
    let
      expectedBig = bsBigOf "TQ" (declsOfModule (bsTripModule "TQ"))
      keptBackend = testBackend
        { lowerModule = \_ lm ->
            "IR:" <> lm.module.name <>
              (if bsBigOf "TQ" lm.module == expectedBig then ":bigKept" else ":bigCHANGED")
        }
      table = Map.singleton "TQ" (loaded (bsTripModule "TQ"))
      Tuple result log = runBs keptBackend table (baseOptions { opt = true, entryModule = "TQ" }) 1
    outcome result `shouldEqual` "Right"
    -- one round only: one rejection, summary 1:1, and the kept (round-input) term reaches codegen.
    withPrefix "bsFired" log `shouldEqual` [ "bsFired:TQ.big" ]
    withPrefix "bsSummary" log `shouldEqual` [ "bsSummary:1:1" ]
    Array.elem "emitFile:IR:TQ:bigKept" log `shouldEqual` true

-- | The 3-changing-rounds chain (all outside `big`'s reachable set): `slowX` is a >64-node lambda
-- | whose dead padding (24 pads, keeping it over the 64-node publish bound in round 1) drops in round 1 (published from round 2); `slowY` calls `slowX` with a
-- | large live literal argument, so it stays >64 until the round-2 unfold collapses it (published
-- | from round 3, its extern reference blocking the 16–63 tier before then); `slowZ` calls `slowY`
-- | and can only unfold it in round 3. Net: the module still *changes* in round 3, after the
-- | quarantined `big` has gone quiet — the skip-while-others-converge path.
bsSlowChain :: String -> Array CFE.Bind
bsSlowChain mn =
  [ CFE.NonRec bsAnn "slowX"
      (CFE.Abs bsAnn "a" (CFE.Let bsAnn (map deadBind (Array.range 1 24)) (bsInt 5)))
  , CFE.NonRec bsAnn "slowY"
      (CFE.Abs bsAnn "b" (CFE.App bsAnn (bsQVar mn "slowX") bigLiveArg))
  , CFE.NonRec bsAnn "slowZ"
      (CFE.Abs bsAnn "c" (CFE.App bsAnn (bsQVar mn "slowY") (bsLVar "c")))
  ]
  where
  deadBind i = CFE.NonRec bsAnn ("dx" <> show i) (bsObj [ Tuple "k" (bsInt i) ])
  bigLiveArg = bsObj (map (\i -> Tuple ("g" <> show i) (bsInt i)) (Array.range 1 24))

bsSkipConvergeSpec :: Spec Unit
bsSkipConvergeSpec =
  it "keeps iterating while the quarantine skips: a no-Fired round still continues and converges" do
    let
      mod' = (bsTripModule "TW") { decls = (bsTripModule "TW").decls <> bsSlowChain "TW" }
      table = Map.singleton "TW" (loaded mod')
      Tuple result log = runBs testBackend table (baseOptions { opt = true, entryModule = "TW" }) 10
    outcome result `shouldEqual` "Right"
    taggedBy [ "enterOpt", "contOpt", "leaveOpt", "bs" ] log `shouldEqual`
      [ "enterOpt:TW"
      , "bsFired:TW.big" -- round 1 trips and records
      , "contOpt:1:TW"
      , "bsFired:TW.big" -- round 2: genuine-fact retry (siblings α-renamed); slowY also unfolds slowX
      , "contOpt:2:TW"
      , "contOpt:3:TW" -- round 3: big SKIPS (no Fired) while slowZ unfolds slowY — the fold continues
      , "bsSummary:2:1" -- round 4 converges; the skipped round contributed no attempt
      , "leaveOpt:TW"
      ]

bsSummaryFlowsSpec :: Spec Unit
bsSummaryFlowsSpec =
  it "the exhausted module's post-backstop summary reaches dependents (candidates and effects)" do
    -- maxOptimizeIter = 1: TQ's fixpoint exhausts mid-blow-up, so the summary handed to
    -- `extendSummary` is the round-1, post-backstop one. The dependent TD then proves both of its
    -- channels: TQ.b (a candidate published by that summary) unfolds into TD.use, and TQ.pureBig
    -- (>64, never a candidate — its *effect fact* is the only channel) lets TD.w drop a dead call.
    -- (The inflated term itself can never be observed through the summary: the backstop floor,
    -- 256, sits above the publish bound, 64 — so the observable contract is that the kept-term
    -- summary flows onward, which these two channels pin.)
    let
      tq = (bsTripModule "TQ")
        { decls = (bsTripModule "TQ").decls <>
            [ CFE.NonRec bsAnn "pureBig"
                ( CFE.Abs bsAnn "y"
                    ( CFE.Let bsAnn (map liveBind (Array.range 1 16))
                        ( bsObj
                            ( [ Tuple "y" (bsLVar "y") ]
                                <> map (\i -> Tuple ("lv" <> show i) (bsLVar ("lv" <> show i))) (Array.range 1 16)
                            )
                        )
                    )
                )
            ]
        }
      liveBind i = CFE.NonRec bsAnn ("lv" <> show i) (bsObj [ Tuple "k" (bsInt i) ])
      td = (modl "TD" [ "TQ" ])
        { exports = [ "use", "w" ]
        , decls =
            [ CFE.NonRec bsAnn "use" (CFE.App bsAnn (bsQVar "TQ" "b") (bsQVar "TQ" "dict"))
            , CFE.NonRec bsAnn "w"
                ( CFE.Abs bsAnn "x"
                    ( CFE.Let bsAnn
                        [ CFE.NonRec bsAnn "dead" (CFE.App bsAnn (bsQVar "TQ" "pureBig") (bsLVar "x")) ]
                        (bsLVar "x")
                    )
                )
            ]
        }
      table = Map.fromFoldable [ Tuple "TQ" (loaded tq), Tuple "TD" (loaded td) ]
      memberOf key m = map (\(Tuple _ e) -> e)
        (Array.find (\(Tuple k _) -> k == key) (Array.concatMap _.members m.decls))
      identityLam = ANF.Ret (ANF.CLam [ "$q1" ] (ANF.Ret (ANF.CAtom (ANF.AtomVar "$q1"))))
      tdBackend = testBackend
        { lowerModule = \_ lm ->
            if lm.module.name /= "TD" then "IR:" <> lm.module.name
            else
              "IR:TD:use-unfolded="
                <> show (map (\e -> Set.member "TQ.b" (fvExpr Set.empty e)) (memberOf "TD.use" lm.module) == Just false)
                <> ":w-dropped="
                <> show (memberOf "TD.w" lm.module == Just identityLam)
        }
      Tuple result log = runBs tdBackend table (baseOptions { opt = true, entryModule = "TD" }) 1
    outcome result `shouldEqual` "Right"
    -- TQ exhausted its single round on a rejection…
    withPrefix "bsFired" log `shouldEqual` [ "bsFired:TQ.big" ]
    withPrefix "bsSummary" log `shouldEqual` [ "bsSummary:1:1" ]
    -- …and its post-backstop summary still fed both dependent channels.
    Array.elem "emitFile:IR:TD:use-unfolded=true:w-dropped=true" log `shouldEqual` true
