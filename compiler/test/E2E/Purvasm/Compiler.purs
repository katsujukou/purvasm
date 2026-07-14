-- | End-to-end of the implemented pipeline as a black box: a CoreFn `Expr` in, the
-- | lowered bytecode `CodeBlock` out — `translExpr` (CoreFn → CESK AST) ≫ `normalize`
-- | (CESK AST → ANF) ≫ `lowerExpr` (ANF → bytecode). Asserting observable I/O, not
-- | intermediate structure. (Becomes compile-and-run once a bytecode VM lands; ADR-0037.)
module Test.E2E.Purvasm.Compiler where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, readdir) as FSSync
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..)) as A
import Purvasm.Compiler.MiddleEnd.ANF.FreeVars (cfExpr, fvExpr)
import Purvasm.Compiler.MiddleEnd.Module (declsOfModule)
import Purvasm.Compiler.MiddleEnd.Optimizer (emptyBuildEnv, extendSummary, localFactsOf, optimizeModule)
import Purvasm.Compiler.Primitive (PrimOp(..)) as Po
import PureScript.CoreFn.Module (Module) as CFM
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import PureScript.CoreFn.Ann (Ann)
import PureScript.CoreFn.Decode (decodeModule)
import PureScript.CoreFn.Expr (Expr(..)) as CF
import PureScript.CoreFn.Literal (Literal(..)) as CF
import PureScript.CoreFn.Names (Qualified(..)) as CF
import Purvasm.Compiler.Bytecode.Artifact (interfaceOf, interfaceToString, moduleToString)
import Purvasm.Compiler.Bytecode.Instruction (CodeBlock, Instruction(..))
import Purvasm.Compiler.Bytecode.Lower (lowerExpr)
import Purvasm.Compiler.CESK.Translate (translExpr)
import Purvasm.Compiler.Compile (compileModule)
import Purvasm.Compiler.MiddleEnd.Normalize (normalize)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | A synthetic (zero-span, no-metadata) annotation for hand-built CoreFn nodes.
nullAnn :: Ann
nullAnn = { span: { start: z, end: z }, meta: Nothing }
  where
  z = { line: 0, column: 0 }

local :: String -> CF.Expr
local x = CF.Var nullAnn (CF.Qualified Nothing x)

int :: Int -> CF.Expr
int = CF.Literal nullAnn <<< CF.LitInt

-- | The whole front-to-back lowering of a top-level (tail-position) expression.
compile :: CF.Expr -> CodeBlock
compile = translExpr >>> normalize >>> lowerExpr true

-- boot's `diamond/DiaA/corefn.json` verbatim (real `purs 0.15.16` output).
diaACorefn :: String
diaACorefn = """{"builtWith":"0.15.16","comments":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"end":[4,23],"start":[4,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[4,23],"start":[4,1]}},"constructorName":"Two","fieldNames":["value0","value1"],"type":"Constructor","typeName":"Two"},"identifier":"Two"},{"annotation":{"meta":null,"sourceSpan":{"end":[5,12],"start":[5,1]}},"bindType":"NonRec","expression":{"abstraction":{"abstraction":{"annotation":{"meta":{"constructorType":"ProductType","identifiers":["value0","value1"],"metaType":"IsConstructor"},"sourceSpan":{"end":[6,11],"start":[6,8]}},"type":"Var","value":{"identifier":"Two","moduleName":["DiaA"]}},"annotation":{"meta":null,"sourceSpan":{"end":[6,13],"start":[6,8]}},"argument":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[6,13],"start":[6,12]}},"type":"Var","value":{"identifier":"b","moduleName":["DiaB"]}},"type":"App"},"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[6,8]}},"argument":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[6,15],"start":[6,14]}},"type":"Var","value":{"identifier":"c","moduleName":["DiaC"]}},"type":"App"},"identifier":"both"}],"exports":["Two","both"],"foreign":[],"imports":[{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaA"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaB"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["DiaC"]},{"annotation":{"meta":null,"sourceSpan":{"end":[6,15],"start":[1,1]}},"moduleName":["Prim"]}],"moduleName":["DiaA"],"modulePath":"src/DiaA.purs","reExports":{},"sourceSpan":{"end":[6,15],"start":[1,1]}}"""

refPmoDiaA :: String
refPmoDiaA = """{"version":3,"name":"DiaA","imports":["DiaA","DiaB","DiaC","Prim"],"exports":["DiaA.Two","DiaA.both"],"groups":[{"keys":["DiaA.Two"],"deps":[],"recursive":false,"members":[["DiaA.Two",["caf",[["ct","Two",2,0],["rt"]]]]]},{"keys":["DiaA.both"],"deps":["DiaA.Two","DiaB.b","DiaC.c"],"recursive":false,"members":[["DiaA.both",["caf",[["ld","DiaA.Two"],["ld","DiaB.b"],["ld","DiaC.c"],["tc",2]]]]]}]}"""

refPmiDiaA :: String
refPmiDiaA = """{"version":3,"name":"DiaA","exports":[["DiaA.Two","caf"],["DiaA.both","caf"]],"imports":["DiaA","DiaB","DiaC","Prim"],"hash":"abfec547bb4356605e4c57f967084fce"}"""

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  paritySpec
  describe "pipeline: CoreFn → CESK → ANF → bytecode" do
    it "lowers a curried application to one uncurried tail call" do
      compile (CF.App nullAnn (CF.App nullAnn (local "f") (int 1)) (int 2))
        `shouldEqual` [ Load "f", PushInt 1, PushInt 2, TailCall 2 ]

    it "lowers an identity abstraction to a returning closure" do
      compile (CF.Abs nullAnn "x" (local "x"))
        `shouldEqual` [ Closure [ "x" ] [ Load "x", Return ], Return ]

  describe "separate compilation: corefn.json → .pmo/.pmi (byte-identical to boot)" do
    it "compiles real purs corefn (diamond/DiaA) to the exact .pmo and .pmi bytes" do
      case jsonParser diaACorefn of
        Left e -> fail ("JSON parse failed: " <> e)
        Right json -> case decodeModule json of
          Left e -> fail ("decode failed: " <> show e)
          Right m -> do
            moduleToString (compileModule m) `shouldEqual` refPmoDiaA
            interfaceToString (interfaceOf (compileModule m)) `shouldEqual` refPmiDiaA

-- --- ADR-0094 fold parity: sliced structural keys ride the ulib shadow bodies --------------------

-- | Load a real corefn artifact by the build's own resolution rule (ulib overlay first, then the
-- | workspace output) — the parity harness must fold the artifacts programs actually compile.
loadReal :: String -> Effect (Maybe CFM.Module)
loadReal name = go roots
  where
  roots = do
    base <- [ "", "../" ]
    root <- [ "dist/ulib/", "output/" ]
    pure (base <> root <> name <> "/corefn.json")

  go = Array.uncons >>> case _ of
    Nothing -> pure Nothing
    Just { head, tail } -> FSSync.exists head >>= case _ of
      false -> go tail
      true -> do
        txt <- FSSync.readTextFile UTF8 head
        case jsonParser txt of
          Left _ -> pure Nothing
          Right json -> case decodeModule json of
            Left _ -> pure Nothing
            Right m -> pure (Just m)

-- | Thread the loaded closure through the real optimiser seam in dependency order, then optimize
-- | a probe module against the accumulated env — the whole `--opt` pipeline in miniature.
optimizeProbe :: Array CFM.Module -> Array (Tuple String A.Expr) -> Array (Tuple String A.Expr)
optimizeProbe mods probes =
  let
    env = Array.foldl
      ( \e m ->
          let
            am = declsOfModule m
          in
            extendSummary e (optimizeModule e (localFactsOf e am) am).summary
      )
      emptyBuildEnv
      mods
    probeAm = { name: "Parity.T", decls: map (\(Tuple k e) -> { recursive: false, members: [ Tuple k e ] }) probes }
  in
    Array.concatMap _.members (optimizeModule env (localFactsOf env probeAm) probeAm).module.decls

-- | Does the expression anywhere use the given primop / reference the given name?
usesPrim :: Po.PrimOp -> A.Expr -> Boolean
usesPrim op = goE
  where
  goE = case _ of
    A.Ret c -> goC c
    A.Let _ c rest -> goC c || goE rest
    A.LetRec bs rest -> Array.any (goE <<< _.rhs) bs || goE rest

  goC = case _ of
    A.CPrim o _ -> o == op
    A.CLam _ b -> goE b
    A.CIf _ t e -> goE t || goE e
    A.CCase _ alts -> Array.any goAlt alts
    _ -> false

  goAlt alt = case alt.result of
    A.Uncond e -> goE e
    A.Guarded gs -> Array.any (\g -> goE g.guard || goE g.rhs) gs

-- | Does the expression contain a `CPerform` run-marker (GER, ADR-0099)?
usesPerform :: A.Expr -> Boolean
usesPerform = goE
  where
  goE = case _ of
    A.Ret c -> goC c
    A.Let _ c rest -> goC c || goE rest
    A.LetRec bs rest -> Array.any (goE <<< _.rhs) bs || goE rest

  goC = case _ of
    A.CPerform _ -> true
    A.CLam _ b -> goE b
    A.CIf _ t e -> goE t || goE e
    A.CCase _ alts -> Array.any goAlt alts
    _ -> false

  goAlt alt = case alt.result of
    A.Uncond e -> goE e
    A.Guarded gs -> Array.any (\g -> goE g.guard || goE g.rhs) gs

refsName :: String -> A.Expr -> Boolean
refsName n e = Set.member n (Set.union (fvExpr Set.empty e) (cfExpr e))

paritySpec :: Spec Unit
paritySpec = describe "ADR-0094 fold parity (sliced structural keys ride the ulib shadow bodies)" do
  let
    avar = A.AtomVar
    aint = A.AtomLit <<< LInt
    load names = liftEffect (map Array.catMaybes (traverse loadReal names))
    -- the shadow Ord closure: Ordering (workspace), Eq + Ord (overlay shadows).
    ordClosure = [ "Data.Ordering", "Data.Eq", "Data.Ord" ]

  it "scalar compare/lessThan fold to constants through the real shadow closure (Int)" do
    mods <- load ordClosure
    Array.length mods `shouldEqual` 3
    let
      out = optimizeProbe mods
        [ Tuple "Parity.T.cmp" (A.Ret (A.CApp (avar "Data.Ord.compare") [ avar "Data.Ord.ordInt", aint 1, aint 2 ]))
        , Tuple "Parity.T.lt" (A.Ret (A.CApp (avar "Data.Ord.lessThan") [ avar "Data.Ord.ordInt", aint 1, aint 2 ]))
        ]
    map snd out `shouldEqual`
      [ A.Ret (A.CCtor "LT" 0 [])
      , A.Ret (A.CAtom (A.AtomLit (LBool true)))
      ]

  it "string compare survives as a call to the shadow's recursive byte-loop body (no constant fold)" do
    -- The real shadow implements string compare as a recursive UTF-8 byte loop
    -- (`compareStringImpl`, byte-exact with the VM's order by construction) — not an `LtString`
    -- composition as ADR-0094's sketch assumed from the registry guest term. Recursion never
    -- inlines (ADR-0089 §5), so parity here is body *visibility*: the dispatch collapses to a
    -- direct call into the shadow body.
    mods <- load ordClosure
    let
      str = A.AtomLit <<< LString
      out = optimizeProbe mods
        [ Tuple "Parity.T.s"
            (A.Ret (A.CApp (avar "Data.Ord.compare") [ avar "Data.Ord.ordString", str "a", str "b" ]))
        ]
    Array.any (refsName "Data.Ord.compareStringImpl" <<< snd) out `shouldEqual` true

  it "array eq / functor map shadow bodies are visible and lower as calls" do
    mods <- load [ "Data.Ordering", "Data.Eq", "Data.Ord", "Data.Functor" ]
    let
      out = optimizeProbe mods
        [ Tuple "Parity.T.m"
            ( A.Let "f" (A.CLam [ "x" ] (A.Ret (A.CPrim Po.AddInt [ avar "x", aint 1 ])))
                ( A.Let "a" (A.CArray [ aint 1, aint 2 ])
                    (A.Ret (A.CApp (avar "Data.Functor.arrayMap") [ avar "f", avar "a" ]))
                )
            )
        ]
      outEq = optimizeProbe mods
        [ Tuple "Parity.T.e"
            ( A.Let "f" (A.CLam [ "x", "y" ] (A.Ret (A.CPrim Po.EqInt [ avar "x", avar "y" ])))
                ( A.Let "a" (A.CArray [ aint 1, aint 2 ])
                    (A.Ret (A.CApp (avar "Data.Eq.eqArrayImpl") [ avar "f", avar "a", avar "a" ]))
                )
            )
        ]
      -- correct lowering, two admissible shapes: the shadow body is a non-recursive wrapper over
      -- an internal loop, so the optimiser may inline it (the residual then carries the loop's
      -- array primitives) or keep the call. Either way the behaviour rides the shadow body — a
      -- vanished call with no loop would mean the work disappeared.
      lowered = Array.any (\(Tuple _ e) -> refsName "Data.Functor.arrayMap" e || usesPrim Po.SetArray e) out
      -- same two admissible shapes for the Eq slice key: the shadow `eqArrayImpl` is a
      -- non-recursive wrapper over an internal element loop (the loop's `IndexArray` reads are
      -- the pinned trace it leaves when inlined).
      loweredEq = Array.any (\(Tuple _ e) -> refsName "Data.Eq.eqArrayImpl" e || usesPrim Po.IndexArray e) outEq
    lowered `shouldEqual` true
    loweredEq `shouldEqual` true

  it "the sliced foreign keys are unreachable in the overlay corpus (dead as a provider)" do
    -- Data.Ord's shadow drops the *Impl names entirely; Data.Eq/Data.Functor keep them as
    -- ordinary PS bindings. In neither case may any overlay corefn declare them as FOREIGN.
    entries <- liftEffect do
      base <- FSSync.exists "dist/ulib" >>= if _ then pure "dist/ulib" else pure "../dist/ulib"
      dirs <- FSSync.readdir base
      map Array.concat $ traverse
        ( \d -> do
            let p = base <> "/" <> d <> "/corefn.json"
            FSSync.exists p >>= case _ of
              false -> pure []
              true -> do
                txt <- FSSync.readTextFile UTF8 p
                case lmap show (jsonParser txt) >>= (decodeModule >>> lmap show) of
                  Left _ -> pure []
                  Right m -> pure [ Tuple d m.foreignNames ]
        )
        dirs
    let
      sliced = [ "arrayMap", "eqArrayImpl", "ordIntImpl", "ordNumberImpl", "ordStringImpl", "ordCharImpl", "ordBooleanImpl" ]
      offenders = Array.filter (\(Tuple _ fs) -> Array.any (\f -> Array.elem f sliced) fs) entries
    map fst offenders `shouldEqual` []

  -- ADR-0099 Slice 2 (subsumes the ADR-0098 exposure fixture): a concrete-`Effect` do-block's
  -- `bind`/`pure`/`discard` dispatch devirtualizes through the mutually-recursive `Effect` instance
  -- group and GER lowers it all the way to canonical `CPerform` ANF — the exposed `Effect.bindE` /
  -- `Effect.pureE` foreign is now a transient GER consumes (`early` dispatch recognition for
  -- `bind`/`pure`, `close` for the `discard` two-hop NbE exposes), on the real artifacts.
  it "ADR-0099 Slice 2: Effect do-block dispatch GER-lowers to canonical CPerform" do
    mods <- load [ "Data.Unit", "Type.Proxy", "Data.Functor", "Control.Apply", "Control.Applicative", "Control.Bind", "Control.Monad", "Effect" ]
    let
      out = optimizeProbe mods
        [ Tuple "Parity.T.p" (A.Ret (A.CApp (avar "Control.Applicative.pure") [ avar "Effect.applicativeEffect", avar "x" ]))
        , Tuple "Parity.T.b" (A.Ret (A.CApp (avar "Control.Bind.bind") [ avar "Effect.bindEffect", avar "m", avar "k" ]))
        , Tuple "Parity.T.d"
            (A.Ret (A.CApp (avar "Control.Bind.discard") [ avar "Control.Bind.discardUnit", avar "Effect.bindEffect", avar "a", avar "k" ]))
        ]
      performs key = Array.any (\(Tuple k e) -> k == key && usesPerform e) out
      refsAny names = Array.any (\(Tuple _ e) -> Array.any (\n -> refsName n e) names) out
    -- bind / discard reflect to an explicit run marker (they sequence effects)
    performs "Parity.T.b" `shouldEqual` true
    performs "Parity.T.d" `shouldEqual` true
    -- pure lowers to a pure unit thunk — no run marker (nothing to perform)
    performs "Parity.T.p" `shouldEqual` false
    -- the exposed GER foreign is consumed (no residual structural foreign), and the dict-CAF
    -- projection / accessor is fully devirtualized — nothing of the dispatch surface survives.
    refsAny [ "Effect.pureE", "Effect.bindE" ] `shouldEqual` false
    refsAny [ "Effect.bindEffect", "Effect.applicativeEffect", "Control.Bind.bind", "Control.Applicative.pure", "Control.Bind.discard" ] `shouldEqual` false

  it "ADR-0099 Slice 3: Effect map/void GER-lower to CPerform (no functorEffect dict projection)" do
    mods <- load [ "Data.Unit", "Type.Proxy", "Data.Functor", "Control.Apply", "Control.Applicative", "Control.Bind", "Control.Monad", "Effect" ]
    let
      out = optimizeProbe mods
        -- `map`: recognised at `early` as a dispatch (`Effect.functorEffect` ∈ effectFamily);
        -- `void`: NbE inlines it to a `functorEffect.map` projection, caught at `close`.
        [ Tuple "Parity.T.m" (A.Ret (A.CApp (avar "Data.Functor.map") [ avar "Effect.functorEffect", avar "f", avar "m" ]))
        , Tuple "Parity.T.v" (A.Ret (A.CApp (avar "Data.Functor.void") [ avar "Effect.functorEffect", avar "m" ]))
        ]
      performs key = Array.any (\(Tuple k e) -> k == key && usesPerform e) out
      refsAny names = Array.any (\(Tuple _ e) -> Array.any (\n -> refsName n e) names) out
    -- both reflect the mapped/voided effect to an explicit run marker
    performs "Parity.T.m" `shouldEqual` true
    performs "Parity.T.v" `shouldEqual` true
    -- and the `functorEffect` dict projection / the `Data.Functor.*` dispatch is fully gone
    refsAny [ "Effect.functorEffect", "Data.Functor.map", "Data.Functor.void" ] `shouldEqual` false

  it "ADR-0099 Slice 4: Effect.forE lowers to an inline loop with CPerform (no structural foreign)" do
    mods <- load [ "Data.Unit", "Effect" ]
    let
      out = optimizeProbe mods
        [ Tuple "Loop.T.f" (A.Ret (A.CApp (avar "Effect.forE") [ avar "lo", avar "hi", avar "body" ])) ]
      performs = Array.any (\(Tuple _ e) -> usesPerform e) out
      refsForE = Array.any (\(Tuple _ e) -> refsName "Effect.forE" e) out
    -- forE is lowered in-place (the loop body's `perform (body i)` is an explicit run marker) and the
    -- structural foreign is gone — nothing left for the linker/backend to materialise.
    performs `shouldEqual` true
    refsForE `shouldEqual` false

  it "ADR-0099 Slice 5: ST glue / eliminator / ref combinators lower to ANF (no structural foreign)" do
    mods <- load [ "Data.Unit" ]
    let
      out = optimizeProbe mods
        [ Tuple "STm.T.r" (A.Ret (A.CApp (avar "Control.Monad.ST.Internal.run") [ avar "st" ]))
        , Tuple "STm.T.b" (A.Ret (A.CApp (avar "Control.Monad.ST.Internal.bind_") [ avar "m", avar "k" ]))
        , Tuple "STm.T.nw" (A.Ret (A.CApp (avar "Control.Monad.ST.Internal.new") [ avar "v" ]))
        , Tuple "STm.T.md" (A.Ret (A.CApp (avar "Control.Monad.ST.Internal.modifyImpl") [ avar "f", avar "ref" ]))
        ]
      performs key = Array.any (\(Tuple k e) -> k == key && usesPerform e) out
      usesP key op = Array.any (\(Tuple k e) -> k == key && usesPrim op e) out
      refsAny names = Array.any (\(Tuple _ e) -> Array.any (\n -> refsName n e) names) out
    -- `run` forces the thunk; `bind_` sequences the performs — both carry an explicit run marker.
    performs "STm.T.r" `shouldEqual` true
    performs "STm.T.b" `shouldEqual` true
    -- `new` / `modifyImpl` are the mutation thunk *bodies* (no inner thunk to force → no CPerform),
    -- lowered to the `STRef`-cell array primops; the structural foreign is gone in every case.
    usesP "STm.T.nw" Po.NewArray `shouldEqual` true
    usesP "STm.T.md" Po.SetArray `shouldEqual` true
    refsAny
      [ "Control.Monad.ST.Internal.run"
      , "Control.Monad.ST.Internal.bind_"
      , "Control.Monad.ST.Internal.new"
      , "Control.Monad.ST.Internal.modifyImpl"
      ] `shouldEqual` false
